#include "emit_llvm.hpp"

#include <llvm/ADT/StringMap.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/SubtargetFeature.h>
#include <llvm/TargetParser/Triple.h>

#include <algorithm>
#include <array>
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <limits>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include "ast.hpp"
#include "comptime.hpp"
#include "hir.hpp"
#include "layout.hpp"
#include "lower_mir.hpp"
#include "mir.hpp"
#include "mir_interp.hpp"
#include "target.hpp"

namespace cog {
namespace {

// LLVM backend (early).
//
// This is a direct AST→LLVM IR emitter for the current v0.0.x subset; it is
// intentionally simple and prioritizes debuggability over optimization.
//
// Key ABI notes:
// - Core v0.1 intent: see `spec/layout_abi.md`.
// - Pointers are modeled as ordinary LLVM pointers (opaque `ptr`); we do not
//   attach `noalias` and we assume pointers may alias freely.
// - Slice pointers (`const* [T]` / `mut* [T]`) are lowered as `{ ptr, usize }`.
// - Enums are lowered as `{ tag, payload_bytes }` where `payload_bytes` is an
//   integer array sized/aligned by the layout engine.

static bool is_signed_int(IntKind k) {
    switch (k) {
        case IntKind::I8:
        case IntKind::I16:
        case IntKind::I32:
        case IntKind::I64:
        case IntKind::I128:
        case IntKind::Isize:
            return true;
        case IntKind::U8:
        case IntKind::U16:
        case IntKind::U32:
        case IntKind::U64:
        case IntKind::U128:
        case IntKind::Usize:
            return false;
    }
    return true;
}

static std::uint32_t int_bits(IntKind k, std::uint32_t ptr_bits) {
    switch (k) {
        case IntKind::I8:
        case IntKind::U8:
            return 8;
        case IntKind::I16:
        case IntKind::U16:
            return 16;
        case IntKind::I32:
        case IntKind::U32:
            return 32;
        case IntKind::I64:
        case IntKind::U64:
            return 64;
        case IntKind::I128:
        case IntKind::U128:
            return 128;
        case IntKind::Isize:
        case IntKind::Usize:
            return ptr_bits;
    }
    return 32;
}

static std::uint64_t align_up(std::uint64_t x, std::uint64_t a) {
    if (a == 0) return x;
    std::uint64_t r = x % a;
    if (r == 0) return x;
    return x + (a - r);
}

static std::optional<size_t> parse_decimal_index(std::string_view text) {
    if (text.empty()) return std::nullopt;
    size_t out = 0;
    for (char c : text) {
        if (c < '0' || c > '9') return std::nullopt;
        const size_t digit = static_cast<size_t>(c - '0');
        if (out > (std::numeric_limits<size_t>::max() - digit) / 10)
            return std::nullopt;
        out = out * 10 + digit;
    }
    return out;
}

static std::string sanitize(std::string_view s) {
    std::string out{};
    out.reserve(s.size());
    for (char c : s) {
        if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
            (c >= '0' && c <= '9')) {
            out.push_back(c);
        } else {
            out.push_back('_');
        }
    }
    return out;
}

static bool path_is_ident(const Path* p, std::string_view name) {
    if (!p || p->segments.size() != 1) return false;
    return p->segments[0]->text == name;
}

static bool is_packed(const std::vector<Attr*>& attrs) {
    for (const Attr* a : attrs) {
        if (!a || !a->name || !path_is_ident(a->name, "repr") || !a->arg_path)
            continue;
        if (path_is_ident(a->arg_path, "packed")) return true;
    }
    return false;
}

struct ItemLocator {
    const ResolvedCrate& crate;
    std::unordered_map<const Item*, ModuleId> item_module{};
    std::unordered_map<const ItemFn*, std::string> fn_symbol{};

    // Collects a stable mapping from AST items to their defining module, and
    // precomputes mangled symbols for functions/methods.
    explicit ItemLocator(const ResolvedCrate& crate) : crate(crate) {
        for (ModuleId mid = 0; mid < crate.modules.size(); mid++) {
            for (const Item* item : crate.modules[mid].items) {
                if (!item) continue;
                item_module[item] = mid;
                if (item->kind == AstNodeKind::ItemFn) {
                    auto* fn = static_cast<const ItemFn*>(item);
                    bool no_mangle = false;
                    std::optional<std::string> forced_symbol{};
                    for (const Attr* a : fn->attrs) {
                        if (!a || !a->name) continue;
                        if (path_is_ident(a->name, "extern") ||
                            path_is_ident(a->name, "export"))
                            no_mangle = true;
                        if (path_is_ident(a->name, "extern_name") ||
                            path_is_ident(a->name, "export_name")) {
                            if (a->arg_string) forced_symbol = *a->arg_string;
                            if (a->arg_path &&
                                a->arg_path->segments.size() == 1 &&
                                a->arg_path->segments[0]) {
                                forced_symbol = a->arg_path->segments[0]->text;
                            }
                        }
                    }
                    // insert into map
                    if (fn->decl) {
                        std::string name =
                            forced_symbol
                                ? *forced_symbol
                                : (no_mangle
                                       ? fn->decl->name
                                       : mangle_in_module(
                                             mid, sanitize(fn->decl->name)));
                        fn_symbol.insert({fn, std::move(name)});
                    }
                }
                if (item->kind == AstNodeKind::ItemImplInherent) {
                    auto* impl = static_cast<const ItemImplInherent*>(item);
                    for (const ItemFn* m : impl->methods) {
                        item_module[static_cast<const Item*>(m)] = mid;
                        if (m && m->decl) {
                            fn_symbol.insert(
                                {m,
                                 mangle_in_module(
                                     mid, "inherent" +
                                              mangle_path(impl->type_name) +
                                              "$" + sanitize(m->decl->name))});
                        }
                    }
                }
            }
        }
    }

    static std::string mangle_path(const Path* p) {
        if (!p) return "$<path>";
        std::string out{};
        for (const Ident* seg : p->segments) {
            out += "$";
            out += sanitize(seg ? seg->text : "<seg>");
        }
        return out;
    }

    ModuleId module_of(const Item* item) const {
        auto it = item_module.find(item);
        if (it == item_module.end()) return 0;
        return it->second;
    }

    std::vector<std::string_view> module_path(ModuleId mid) const {
        std::vector<std::string_view> parts{};
        ModuleId cur = mid;
        while (true) {
            const Module& m = crate.modules[cur];
            if (m.parent == cur || m.name == "<crate>") break;
            parts.push_back(m.name);
            if (m.parent == cur) break;
            cur = m.parent;
            if (cur >= crate.modules.size()) break;
        }
        std::reverse(parts.begin(), parts.end());
        return parts;
    }

    std::string mangle_in_module(ModuleId mid, std::string_view leaf) const {
        std::string out = "cog";
        for (auto part : module_path(mid)) {
            out += "$";
            out += sanitize(part);
        }
        out += "$";
        out += std::string(leaf);
        return out;
    }

    std::string symbol_for(const ItemFn* fn) const {
        if (!fn || !fn->decl) return "cog$<fn>";
        auto it = fn_symbol.find(fn);
        if (it != fn_symbol.end()) return it->second;

        for (const Attr* a : fn->attrs) {
            if (!a || !a->name) continue;
            if (path_is_ident(a->name, "extern_name") ||
                path_is_ident(a->name, "export_name")) {
                if (a->arg_string) return *a->arg_string;
                if (a->arg_path && a->arg_path->segments.size() == 1 &&
                    a->arg_path->segments[0])
                    return a->arg_path->segments[0]->text;
            }
        }
        for (const Attr* a : fn->attrs) {
            if (!a || !a->name) continue;
            if (path_is_ident(a->name, "extern") ||
                path_is_ident(a->name, "export"))
                return fn->decl->name;
        }
        return mangle_in_module(module_of(static_cast<const Item*>(fn)),
                                sanitize(fn->decl->name));
    }

    std::string symbol_for_static(const ItemStatic* st) const {
        if (!st) return "cog$<static>";
        return mangle_in_module(module_of(static_cast<const Item*>(st)),
                                "static$" + sanitize(st->name));
    }
};

struct CgValue {
    TypeId type = 0;
    llvm::Value* value = nullptr;  // null means "void" / unit
};

struct LocalSlot {
    TypeId type = 0;
    llvm::AllocaInst* alloca = nullptr;
};

class LlvmBackend {
   public:
    LlvmBackend(Session& session, const ResolvedCrate& crate,
                CheckedCrate& checked, const TargetSpec& target)
        : session_(session),
          crate_(crate),
          checked_(checked),
          target_(target),
          locator_(crate),
          layout_(session, checked.types, checked.struct_info,
                  checked.enum_info, checked.array_lens, target.layout),
          eval_(session, crate, &checked.types, &layout_),
          module_(std::make_unique<llvm::Module>("cog", ctx_)),
          builder_(ctx_) {}

    bool run(const EmitLlvmOptions& opts) {
        // NOTE: target selection and layout must be consistent across:
        // - type checking / layout engine (for field offsets, enum layouts,
        // etc)
        // - the LLVM TargetMachine/DataLayout used here.
        if (!create_target_machine()) return false;

        module_->setTargetTriple(llvm::Triple(target_.triple));
        module_->setDataLayout(target_machine_->createDataLayout());

        build_nominal_types();
        build_function_decls();
        if (!build_hir_and_mir()) return false;
        build_function_bodies();
        if (opts.emit_main_wrapper) emit_main_shim();

        if (session_.has_errors()) return false;
        if (!verify_module()) return false;

        if (!write_outputs(opts)) return false;
        return !session_.has_errors();
    }

   private:
    Session& session_;
    const ResolvedCrate& crate_;
    CheckedCrate& checked_;
    const TargetSpec& target_;
    ItemLocator locator_;

    LayoutEngine layout_;
    ComptimeEvaluator eval_;
    std::optional<HirCrate> hir_{};
    std::optional<MirProgram> mir_{};
    std::unique_ptr<MirInterpreter> mir_eval_{};

    llvm::LLVMContext ctx_{};
    std::unique_ptr<llvm::Module> module_{};
    llvm::IRBuilder<> builder_;
    std::unique_ptr<llvm::TargetMachine> target_machine_{};

    std::unordered_map<const ItemStruct*, llvm::StructType*> struct_types_{};
    std::unordered_map<const ItemEnum*, llvm::StructType*> enum_types_{};
    llvm::StructType* slice_ptr_type_ = nullptr;

    std::unordered_map<const ItemFn*, llvm::Function*> fn_decls_{};
    std::unordered_set<const ItemFn*> emitted_fns_{};

    std::unordered_map<const ItemStatic*, llvm::GlobalVariable*>
        static_globals_{};

    // v0.0.17: residualized (comptime-parameter) function variants.
    // Keyed by a stable, mangled name derived from the original function +
    // comptime args.
    std::unordered_map<std::string, llvm::Function*>
        comptime_specializations_{};

    void error(Span span, std::string message) {
        session_.diags.push_back(Diagnostic{.severity = Severity::Error,
                                            .span = span,
                                            .message = std::move(message)});
    }

    static void ensure_llvm_target_init() {
        static bool done = false;
        if (done) return;
        done = true;
        llvm::InitializeAllTargetInfos();
        llvm::InitializeAllTargets();
        llvm::InitializeAllTargetMCs();
        llvm::InitializeAllAsmParsers();
        llvm::InitializeAllAsmPrinters();
    }

    bool create_target_machine() {
        ensure_llvm_target_init();

        std::string error_message{};
        const llvm::Target* target =
            llvm::TargetRegistry::lookupTarget(target_.triple, error_message);
        if (!target) {
            error(Span{}, "LLVM target lookup failed for `" + target_.triple +
                              "`: " + error_message);
            return false;
        }

        llvm::TargetOptions opts{};
        auto reloc = std::optional<llvm::Reloc::Model>{};
        target_machine_ =
            std::unique_ptr<llvm::TargetMachine>(target->createTargetMachine(
                llvm::Triple(target_.triple), target_.cpu, target_.features,
                opts, reloc));
        if (!target_machine_) {
            error(Span{}, "failed to create LLVM TargetMachine for `" +
                              target_.triple + "`");
            return false;
        }
        return true;
    }

    llvm::PointerType* ptr_ty() { return llvm::PointerType::get(ctx_, 0); }

    std::uint32_t ptr_bits() const { return target_.layout.pointer_bits; }

    llvm::Type* llvm_int_ty(IntKind k) {
        return llvm::IntegerType::get(ctx_, int_bits(k, ptr_bits()));
    }

    llvm::Type* llvm_float_ty(FloatKind k) {
        switch (k) {
            case FloatKind::F32:
                return llvm::Type::getFloatTy(ctx_);
            case FloatKind::F64:
                return llvm::Type::getDoubleTy(ctx_);
        }
        return llvm::Type::getDoubleTy(ctx_);
    }

    llvm::Type* llvm_tag_int_bytes(std::uint64_t bytes) {
        return llvm::IntegerType::get(ctx_, static_cast<unsigned>(bytes * 8));
    }

    llvm::StructType* llvm_slice_ptr_type() {
        if (slice_ptr_type_) return slice_ptr_type_;
        slice_ptr_type_ = llvm::StructType::create(ctx_, "cog.slice_ptr");
        slice_ptr_type_->setBody({ptr_ty(), llvm_int_ty(IntKind::Usize)},
                                 /*isPacked=*/false);
        return slice_ptr_type_;
    }

    llvm::Type* llvm_type(TypeId ty) {
        const TypeData& d = checked_.types.get(ty);
        switch (d.kind) {
            case TypeKind::Error:
                return llvm::Type::getInt8Ty(ctx_);
            case TypeKind::Unit:
                return llvm::Type::getVoidTy(ctx_);
            case TypeKind::Bool:
                return llvm::Type::getInt1Ty(ctx_);
            case TypeKind::Int:
                return llvm_int_ty(d.int_kind);
            case TypeKind::Float:
                return llvm_float_ty(d.float_kind);
            case TypeKind::Never:
                return llvm::Type::getVoidTy(ctx_);
            case TypeKind::Ptr: {
                const TypeData& pd = checked_.types.get(d.pointee);
                if (pd.kind == TypeKind::Slice) return llvm_slice_ptr_type();
                return ptr_ty();
            }
            case TypeKind::Array: {
                llvm::Type* elem_ty = llvm_type(d.elem);
                std::uint64_t len = 0;
                if (d.array_len_value) {
                    len = *d.array_len_value;
                } else if (d.array_len_expr) {
                    if (auto it = checked_.array_lens.find(d.array_len_expr);
                        it != checked_.array_lens.end()) {
                        len = it->second;
                    } else {
                        error(Span{},
                              "array length is not a known comptime constant "
                              "during LLVM lowering");
                    }
                } else {
                    error(Span{},
                          "array length is missing during LLVM lowering");
                }
                return llvm::ArrayType::get(elem_ty, len);
            }
            case TypeKind::Tuple: {
                std::vector<llvm::Type*> elems{};
                elems.reserve(d.tuple_elems.size());
                for (TypeId e : d.tuple_elems) elems.push_back(llvm_type(e));
                return llvm::StructType::get(ctx_, elems, /*isPacked=*/false);
            }
            case TypeKind::Struct:
                return llvm_struct_type(d.struct_def);
            case TypeKind::Enum:
                return llvm_enum_type(d.enum_def);
            default:
                error(Span{}, "unsupported type in LLVM backend: " +
                                  checked_.types.to_string(ty));
                return llvm::Type::getInt8Ty(ctx_);
        }
    }

    llvm::StructType* llvm_struct_type(const ItemStruct* s) {
        if (!s) return nullptr;
        auto it = struct_types_.find(s);
        return it == struct_types_.end() ? nullptr : it->second;
    }

    llvm::StructType* llvm_enum_type(const ItemEnum* e) {
        if (!e) return nullptr;
        auto it = enum_types_.find(e);
        return it == enum_types_.end() ? nullptr : it->second;
    }

    std::string llvm_struct_type_name(const ItemStruct* s) const {
        ModuleId mid = locator_.module_of(static_cast<const Item*>(s));
        std::string name = "struct";
        for (auto part : locator_.module_path(mid)) {
            name += ".";
            name += sanitize(part);
        }
        name += ".";
        name += sanitize(s ? s->name : "<struct>");
        return name;
    }

    std::string llvm_enum_type_name(const ItemEnum* e) const {
        ModuleId mid = locator_.module_of(static_cast<const Item*>(e));
        std::string name = "enum";
        for (auto part : locator_.module_path(mid)) {
            name += ".";
            name += sanitize(part);
        }
        name += ".";
        name += sanitize(e ? e->name : "<enum>");
        return name;
    }

    TypeId type_of(const Expr* e) const {
        if (!e) return checked_.types.error();
        auto it = checked_.expr_types.find(e);
        if (it == checked_.expr_types.end()) return checked_.types.error();
        return it->second;
    }

    void build_nominal_types() {
        for (ModuleId mid = 0; mid < crate_.modules.size(); mid++) {
            for (const Item* item : crate_.modules[mid].items) {
                if (!item) continue;
                if (item->kind == AstNodeKind::ItemStruct) {
                    auto* s = static_cast<const ItemStruct*>(item);
                    struct_types_.insert(
                        {s, llvm::StructType::create(
                                ctx_, llvm_struct_type_name(s))});
                } else if (item->kind == AstNodeKind::ItemEnum) {
                    auto* e = static_cast<const ItemEnum*>(item);
                    enum_types_.insert({e, llvm::StructType::create(
                                               ctx_, llvm_enum_type_name(e))});
                }
            }
        }

        // Fill bodies (after predecl so pointer recursion can work).
        for (const auto& [s, st] : struct_types_) {
            auto info_it = checked_.struct_info.find(s);
            if (info_it == checked_.struct_info.end()) continue;
            std::vector<llvm::Type*> elems{};
            elems.reserve(info_it->second.fields_in_order.size());
            for (const auto& fld : info_it->second.fields_in_order)
                elems.push_back(llvm_type(fld.type));
            st->setBody(elems, is_packed(s->attrs));
        }

        for (const auto& [e, et] : enum_types_) {
            auto enum_layout = layout_.enum_layout(e, Span{});
            if (!enum_layout) continue;

            llvm::Type* tag_ty = llvm_tag_int_bytes(enum_layout->tag_size);
            if (enum_layout->payload_size == 0) {
                et->setBody({tag_ty}, /*isPacked=*/false);
                continue;
            }

            std::uint64_t align =
                std::max<std::uint64_t>(enum_layout->payload_align, 1);
            std::uint64_t words = enum_layout->payload_size / align;
            llvm::Type* payload_elem_ty = llvm_tag_int_bytes(align);
            llvm::Type* payload_array_ty =
                llvm::ArrayType::get(payload_elem_ty, words);
            et->setBody({tag_ty, payload_array_ty}, /*isPacked=*/false);
        }
    }

    llvm::FunctionType* llvm_fn_type(const FnInfo& sig) {
        std::vector<llvm::Type*> params{};
        params.reserve(sig.params.size());
        for (TypeId p : sig.params) params.push_back(llvm_type(p));
        llvm::Type* ret = llvm_type(sig.ret);
        return llvm::FunctionType::get(ret, params,
                                       /*isVarArg=*/sig.is_variadic);
    }

    llvm::FunctionType* llvm_fn_type(TypeId fn_ty) {
        const TypeData& d = checked_.types.get(fn_ty);
        if (d.kind != TypeKind::Fn) return nullptr;
        std::vector<llvm::Type*> params{};
        params.reserve(d.fn_params.size());
        for (TypeId p : d.fn_params) params.push_back(llvm_type(p));
        llvm::Type* ret = llvm_type(d.fn_ret);
        return llvm::FunctionType::get(ret, params, /*isVarArg=*/false);
    }

    void build_function_decls() {
        for (ModuleId mid = 0; mid < crate_.modules.size(); mid++) {
            for (const Item* item : crate_.modules[mid].items) {
                if (!item) continue;
                if (item->kind == AstNodeKind::ItemFn)
                    declare_function(static_cast<const ItemFn*>(item));
                if (item->kind == AstNodeKind::ItemImplInherent) {
                    auto* impl = static_cast<const ItemImplInherent*>(item);
                    for (const ItemFn* m : impl->methods) declare_function(m);
                }
            }
        }
    }

    bool build_hir_and_mir() {
        if (hir_ && mir_ && mir_eval_) return true;
        hir_ = build_hir(crate_, checked_);
        auto program = lower_mir(session_, *hir_);
        if (!program) return false;
        mir_ = std::move(*program);
        mir_eval_ = std::make_unique<MirInterpreter>(session_, *mir_);
        return !session_.has_errors();
    }

    void declare_function(const ItemFn* fn) {
        if (!fn || !fn->decl || !fn->decl->sig) return;
        if (fn_decls_.contains(fn)) return;

        auto sig_it = checked_.fn_info.find(fn);
        if (sig_it == checked_.fn_info.end()) return;
        const FnInfo& sig = sig_it->second;
        if (!sig.comptime_params.empty() &&
            std::any_of(sig.comptime_params.begin(), sig.comptime_params.end(),
                        [](bool b) { return b; })) {
            // Functions with comptime parameters are not first-class values yet
            // (and therefore do not have a stable runtime symbol). They are
            // emitted on demand as residualized `$ct...` variants.
            return;
        }
        llvm::FunctionType* fty = llvm_fn_type(sig);

        const std::string symbol = locator_.symbol_for(fn);
        llvm::Function* f = llvm::Function::Create(
            fty, llvm::GlobalValue::ExternalLinkage, symbol, module_.get());
        fn_decls_.insert({fn, f});
    }

    llvm::Function* llvm_fn(const ItemFn* fn) {
        auto it = fn_decls_.find(fn);
        return it == fn_decls_.end() ? nullptr : it->second;
    }

    static std::optional<std::string> comptime_arg_key(TypeId expected_ty,
                                                       const TypeStore& ts,
                                                       const ComptimeValue& v) {
        const TypeData& td = ts.get(expected_ty);
        switch (v.kind) {
            case ComptimeValue::Kind::Int:
                if (td.kind != TypeKind::Int) return std::nullopt;
                if (v.int_value < 0) return "n" + std::to_string(-v.int_value);
                return "p" + std::to_string(v.int_value);
            case ComptimeValue::Kind::Bool:
                if (td.kind != TypeKind::Bool) return std::nullopt;
                return v.bool_value ? "b1" : "b0";
            default:
                return std::nullopt;
        }
    }

    llvm::FunctionType* llvm_fn_type_residualized(const FnInfo& sig) {
        std::vector<llvm::Type*> params{};
        params.reserve(sig.params.size());
        for (size_t i = 0; i < sig.params.size(); i++) {
            const bool is_ct =
                i < sig.comptime_params.size() && sig.comptime_params[i];
            if (is_ct) continue;
            params.push_back(llvm_type(sig.params[i]));
        }
        llvm::Type* ret = llvm_type(sig.ret);
        return llvm::FunctionType::get(ret, params, /*isVarArg=*/false);
    }

    llvm::Function* get_or_create_comptime_specialization(
        const ItemFn* fn, const FnInfo& sig,
        const std::vector<ComptimeValue>& comptime_args, Span use_site) {
        if (!fn || !fn->decl || !fn->decl->sig) return nullptr;

        // Construct a stable specialization name.
        std::string name = locator_.symbol_for(fn);
        name += "$ct";

        size_t ct_index = 0;
        for (size_t i = 0; i < sig.params.size(); i++) {
            const bool is_ct =
                i < sig.comptime_params.size() && sig.comptime_params[i];
            if (!is_ct) continue;
            if (ct_index >= comptime_args.size()) {
                error(use_site,
                      "internal error: missing comptime arg while naming "
                      "specialization");
                return nullptr;
            }
            auto key = comptime_arg_key(sig.params[i], checked_.types,
                                        comptime_args[ct_index]);
            if (!key) {
                error(use_site,
                      "unsupported comptime argument kind for residualization");
                return nullptr;
            }
            name += "$";
            name += *key;
            ct_index++;
        }

        if (auto it = comptime_specializations_.find(name);
            it != comptime_specializations_.end())
            return it->second;

        llvm::FunctionType* fty = llvm_fn_type_residualized(sig);
        llvm::Function* specialized = llvm::Function::Create(
            fty, llvm::GlobalValue::InternalLinkage, name, module_.get());
        comptime_specializations_.insert({name, specialized});

        // Emit the residualized body immediately.
        if (fn->body && hir_ && mir_) {
            auto dit = hir_->def_ids.find(static_cast<const Item*>(fn));
            if (dit != hir_->def_ids.end()) {
                if (const MirBody* body = mir_->body_for_fn(dit->second)) {
                    llvm::IRBuilder<>::InsertPointGuard guard(builder_);
                    mir_emit_body(specialized, *body, sig, &comptime_args,
                                  use_site);
                }
            }
        }
        return specialized;
    }

    void build_function_bodies() {
        // v0.0.20: runtime codegen is driven by MIR, not the AST.
        if (!hir_ || !mir_) return;
        for (ModuleId mid = 0; mid < crate_.modules.size(); mid++) {
            for (const Item* item : crate_.modules[mid].items) {
                if (!item) continue;
                if (item->kind == AstNodeKind::ItemFn)
                    mir_emit_function(static_cast<const ItemFn*>(item));
                if (item->kind == AstNodeKind::ItemImplInherent) {
                    auto* impl = static_cast<const ItemImplInherent*>(item);
                    for (const ItemFn* m : impl->methods) mir_emit_function(m);
                }
            }
        }
    }

    // ---- MIR → LLVM lowering (v0.0.20) ----

    struct MirFnCtx {
        llvm::Function* fn = nullptr;
        const MirBody* body = nullptr;
        std::vector<llvm::AllocaInst*> locals{};
        std::vector<llvm::BasicBlock*> blocks{};
    };

    struct MirPlaceAddr {
        llvm::Value* ptr = nullptr;  // pointer to current place
        TypeId ty = 0;

        struct EnumPayload {
            TypeId enum_ty = 0;
            std::uint32_t variant_index = 0;
        };
        std::optional<EnumPayload> enum_payload{};
    };

    const ItemFn* fn_def(DefId def) const {
        if (!hir_) return nullptr;
        const HirDef* hd = hir_->def(def);
        if (!hd || hd->kind != HirDefKind::Fn || !hd->ast) return nullptr;
        if (hd->ast->kind != AstNodeKind::ItemFn) return nullptr;
        return static_cast<const ItemFn*>(hd->ast);
    }

    const ItemStatic* static_def(DefId def) const {
        if (!hir_) return nullptr;
        const HirDef* hd = hir_->def(def);
        if (!hd || hd->kind != HirDefKind::Static || !hd->ast) return nullptr;
        if (hd->ast->kind != AstNodeKind::ItemStatic) return nullptr;
        return static_cast<const ItemStatic*>(hd->ast);
    }

    TypeId type_of_static(const ItemStatic* st) const {
        if (!st) return checked_.types.error();
        auto it = checked_.static_types.find(st);
        return it == checked_.static_types.end() ? checked_.types.error()
                                                 : it->second;
    }

    TypeId type_of_const(const ItemConst* c) const {
        if (!c) return checked_.types.error();
        auto it = checked_.const_types.find(c);
        return it == checked_.const_types.end() ? checked_.types.error()
                                                : it->second;
    }

    TypeId type_of_fn_value(const ItemFn* fn) const {
        if (!fn) return checked_.types.error();
        auto it = checked_.fn_info.find(fn);
        if (it == checked_.fn_info.end()) return checked_.types.error();
        const FnInfo& sig = it->second;
        TypeId fn_ty = checked_.types.fn(
            std::vector<TypeId>(sig.params.begin(), sig.params.end()), sig.ret);
        return checked_.types.ptr(Mutability::Const, fn_ty);
    }

    bool has_comptime_params(const FnInfo& sig) const {
        return !sig.comptime_params.empty() &&
               std::any_of(sig.comptime_params.begin(),
                           sig.comptime_params.end(), [](bool b) { return b; });
    }

    TypeId mir_place_type(const MirFnCtx& f, const MirPlace& place) const {
        if (!f.body) return checked_.types.error();
        TypeId ty = checked_.types.error();
        if (std::holds_alternative<MirPlace::Local>(place.base)) {
            MirLocalId id = std::get<MirPlace::Local>(place.base).local;
            if (id < f.body->locals.size())
                ty = f.body->locals[static_cast<size_t>(id)].ty;
        } else if (std::holds_alternative<MirPlace::Static>(place.base)) {
            const ItemStatic* st =
                static_def(std::get<MirPlace::Static>(place.base).def);
            ty = type_of_static(st);
        }

        std::optional<std::pair<TypeId, std::uint32_t>> enum_payload{};
        for (const MirProjection& p : place.projection) {
            if (std::holds_alternative<MirProjection::Downcast>(p.data)) {
                const TypeData& td = checked_.types.get(ty);
                if (td.kind != TypeKind::Enum || !td.enum_def) break;
                enum_payload = {
                    ty,
                    std::get<MirProjection::Downcast>(p.data).variant_index};
                continue;
            }
            if (std::holds_alternative<MirProjection::Deref>(p.data)) {
                const TypeData& td = checked_.types.get(ty);
                if (td.kind != TypeKind::Ptr) break;
                ty = td.pointee;
                enum_payload.reset();
                continue;
            }
            if (std::holds_alternative<MirProjection::Index>(p.data)) {
                const TypeData& td = checked_.types.get(ty);
                if (td.kind == TypeKind::Array) {
                    ty = td.elem;
                    enum_payload.reset();
                    continue;
                }
                if (td.kind == TypeKind::Ptr) {
                    const TypeData& pd = checked_.types.get(td.pointee);
                    if (pd.kind == TypeKind::Slice) {
                        ty = pd.elem;
                        enum_payload.reset();
                        continue;
                    }
                    ty = td.pointee;
                    enum_payload.reset();
                    continue;
                }
                break;
            }
            if (std::holds_alternative<MirProjection::Field>(p.data)) {
                const std::uint32_t index =
                    std::get<MirProjection::Field>(p.data).index;

                if (enum_payload) {
                    const TypeData& td =
                        checked_.types.get(enum_payload->first);
                    if (td.kind != TypeKind::Enum || !td.enum_def) break;
                    auto ei_it = checked_.enum_info.find(td.enum_def);
                    if (ei_it == checked_.enum_info.end()) break;
                    const EnumInfo& ei = ei_it->second;
                    if (enum_payload->second >= ei.variants_in_order.size())
                        break;
                    const std::string& vname =
                        ei.variants_in_order[enum_payload->second];
                    auto vit = ei.variants.find(vname);
                    if (vit == ei.variants.end()) break;
                    const VariantInfo& vi = vit->second;
                    if (index >= vi.payload.size()) break;
                    ty = vi.payload[index];
                    enum_payload.reset();
                    continue;
                }

                const TypeData& td = checked_.types.get(ty);
                if (td.kind == TypeKind::Struct && td.struct_def) {
                    auto si_it = checked_.struct_info.find(td.struct_def);
                    if (si_it == checked_.struct_info.end()) break;
                    if (index >= si_it->second.fields_in_order.size()) break;
                    ty = si_it->second.fields_in_order[index].type;
                    continue;
                }
                if (td.kind == TypeKind::Tuple) {
                    if (index >= td.tuple_elems.size()) break;
                    ty = td.tuple_elems[index];
                    continue;
                }
                break;
            }
        }
        return ty;
    }

    TypeId mir_operand_type(const MirFnCtx& f, const MirOperand& op) const {
        if (std::holds_alternative<MirOperand::Const>(op.data))
            return std::get<MirOperand::Const>(op.data).ty;
        if (std::holds_alternative<MirOperand::Copy>(op.data))
            return mir_place_type(f, std::get<MirOperand::Copy>(op.data).place);
        if (std::holds_alternative<MirOperand::Move>(op.data))
            return mir_place_type(f, std::get<MirOperand::Move>(op.data).place);
        if (std::holds_alternative<MirOperand::Fn>(op.data)) {
            const ItemFn* fn = fn_def(std::get<MirOperand::Fn>(op.data).def);
            return type_of_fn_value(fn);
        }
        if (std::holds_alternative<MirOperand::ConstItem>(op.data)) {
            DefId def = std::get<MirOperand::ConstItem>(op.data).def;
            if (mir_) {
                if (const MirBody* b = mir_->body_for_const(def))
                    return b->ret_ty;
            }
            const HirDef* hd = hir_ ? hir_->def(def) : nullptr;
            if (hd && hd->ast && hd->ast->kind == AstNodeKind::ItemConst)
                return type_of_const(static_cast<const ItemConst*>(hd->ast));
            return checked_.types.error();
        }
        return checked_.types.error();
    }

    CgValue mir_cast_value(const CgValue& from, TypeId to_ty) {
        TypeId from_ty = from.type;
        const TypeData& from_d = checked_.types.get(from_ty);
        const TypeData& to_d = checked_.types.get(to_ty);

        auto is_slice_ptr = [&](const TypeData& d) -> bool {
            if (d.kind != TypeKind::Ptr) return false;
            return checked_.types.get(d.pointee).kind == TypeKind::Slice;
        };
        const bool from_is_slice_ptr = is_slice_ptr(from_d);
        const bool to_is_slice_ptr = is_slice_ptr(to_d);

        llvm::Type* dst_ll_ty = llvm_type(to_ty);
        if (!from.value) return CgValue{.type = to_ty, .value = nullptr};

        if (from_d.kind == TypeKind::Float && to_d.kind == TypeKind::Float) {
            if (from_d.float_kind == to_d.float_kind)
                return CgValue{.type = to_ty, .value = from.value};
            if (from_d.float_kind == FloatKind::F32 &&
                to_d.float_kind == FloatKind::F64) {
                return CgValue{
                    .type = to_ty,
                    .value = builder_.CreateFPExt(from.value, dst_ll_ty)};
            }
            if (from_d.float_kind == FloatKind::F64 &&
                to_d.float_kind == FloatKind::F32) {
                return CgValue{
                    .type = to_ty,
                    .value = builder_.CreateFPTrunc(from.value, dst_ll_ty)};
            }
            return CgValue{.type = to_ty, .value = from.value};
        }
        if (from_d.kind == TypeKind::Int && to_d.kind == TypeKind::Float) {
            bool signed_from = is_signed_int(from_d.int_kind);
            llvm::Value* out =
                signed_from ? builder_.CreateSIToFP(from.value, dst_ll_ty)
                            : builder_.CreateUIToFP(from.value, dst_ll_ty);
            return CgValue{.type = to_ty, .value = out};
        }
        if (from_d.kind == TypeKind::Float && to_d.kind == TypeKind::Int) {
            bool signed_to = is_signed_int(to_d.int_kind);
            llvm::Value* out =
                signed_to ? builder_.CreateFPToSI(from.value, dst_ll_ty)
                          : builder_.CreateFPToUI(from.value, dst_ll_ty);
            return CgValue{.type = to_ty, .value = out};
        }
        if (from_d.kind == TypeKind::Int && to_d.kind == TypeKind::Int) {
            std::uint32_t from_bits = int_bits(from_d.int_kind, ptr_bits());
            std::uint32_t to_bits = int_bits(to_d.int_kind, ptr_bits());
            if (from_bits == to_bits)
                return CgValue{.type = to_ty, .value = from.value};
            bool signed_from = is_signed_int(from_d.int_kind);
            if (from_bits < to_bits) {
                llvm::Value* out =
                    signed_from ? builder_.CreateSExt(from.value, dst_ll_ty)
                                : builder_.CreateZExt(from.value, dst_ll_ty);
                return CgValue{.type = to_ty, .value = out};
            }
            return CgValue{
                .type = to_ty,
                .value = builder_.CreateTrunc(from.value, dst_ll_ty)};
        }

        if (from_d.kind == TypeKind::Int && to_d.kind == TypeKind::Ptr) {
            if (to_is_slice_ptr) {
                llvm::Value* p = builder_.CreateIntToPtr(from.value, ptr_ty());
                llvm::Value* len =
                    llvm::ConstantInt::get(llvm_int_ty(IntKind::Usize), 0);
                llvm::Value* agg = llvm::UndefValue::get(llvm_type(to_ty));
                agg = builder_.CreateInsertValue(agg, p, {0});
                agg = builder_.CreateInsertValue(agg, len, {1});
                return CgValue{.type = to_ty, .value = agg};
            }
            return CgValue{
                .type = to_ty,
                .value = builder_.CreateIntToPtr(from.value, ptr_ty())};
        }

        if (from_d.kind == TypeKind::Ptr && to_d.kind == TypeKind::Int) {
            if (from_is_slice_ptr) {
                llvm::Value* p = builder_.CreateExtractValue(from.value, {0});
                return CgValue{.type = to_ty,
                               .value = builder_.CreatePtrToInt(p, dst_ll_ty)};
            }
            return CgValue{
                .type = to_ty,
                .value = builder_.CreatePtrToInt(from.value, dst_ll_ty)};
        }

        if (from_d.kind == TypeKind::Ptr && to_d.kind == TypeKind::Ptr) {
            if (from_is_slice_ptr && to_is_slice_ptr) {
                return CgValue{.type = to_ty, .value = from.value};
            }
            if (from_is_slice_ptr && !to_is_slice_ptr) {
                llvm::Value* p = builder_.CreateExtractValue(from.value, {0});
                return CgValue{.type = to_ty,
                               .value = builder_.CreateBitCast(p, ptr_ty())};
            }
            if (!from_is_slice_ptr && to_is_slice_ptr) {
                std::uint64_t len_value = 0;
                const TypeData& fp = checked_.types.get(from_d.pointee);
                if (fp.kind == TypeKind::Array) {
                    if (fp.array_len_value) {
                        len_value = *fp.array_len_value;
                    } else if (fp.array_len_expr) {
                        auto it = checked_.array_lens.find(fp.array_len_expr);
                        if (it != checked_.array_lens.end()) {
                            len_value = it->second;
                        } else {
                            error(Span{},
                                  "array length is not a known comptime "
                                  "constant during array-to-slice coercion");
                        }
                    }
                }
                llvm::Value* len = llvm::ConstantInt::get(
                    llvm_int_ty(IntKind::Usize), len_value);
                llvm::Value* agg = llvm::UndefValue::get(llvm_type(to_ty));
                agg = builder_.CreateInsertValue(agg, from.value, {0});
                agg = builder_.CreateInsertValue(agg, len, {1});
                return CgValue{.type = to_ty, .value = agg};
            }

            return CgValue{
                .type = to_ty,
                .value = builder_.CreateBitCast(from.value, ptr_ty())};
        }

        return CgValue{.type = to_ty, .value = from.value};
    }

    CgValue mir_coerce_value(CgValue v, TypeId to_ty, Span use_site) {
        if (checked_.types.equal(v.type, to_ty)) return v;
        if (!checked_.types.can_coerce(v.type, to_ty)) {
            error(use_site, "type mismatch: expected `" +
                                checked_.types.to_string(to_ty) + "`, got `" +
                                checked_.types.to_string(v.type) + "`");
            return CgValue{.type = to_ty, .value = zero_init(llvm_type(to_ty))};
        }
        return mir_cast_value(v, to_ty);
    }

    std::optional<ComptimeValue> mir_eval_comptime_operand(const MirFnCtx& f,
                                                           const MirOperand& op,
                                                           TypeId expected_ty,
                                                           Span use_site) {
        (void)expected_ty;
        if (!mir_eval_) return std::nullopt;
        if (std::holds_alternative<MirOperand::Const>(op.data)) {
            const auto& c = std::get<MirOperand::Const>(op.data);
            return std::visit(
                [&](const auto& v) -> std::optional<ComptimeValue> {
                    using T = std::decay_t<decltype(v)>;
                    if constexpr (std::is_same_v<T, MirConst::Unit>) {
                        return ComptimeValue::unit();
                    } else if constexpr (std::is_same_v<T, MirConst::Bool>) {
                        return ComptimeValue::bool_(v.v);
                    } else if constexpr (std::is_same_v<T, MirConst::Int>) {
                        return ComptimeValue::int_(v.v);
                    } else if constexpr (std::is_same_v<T, MirConst::Float>) {
                        return ComptimeValue::float_(v.v);
                    } else if constexpr (std::is_same_v<T, MirConst::String>) {
                        return ComptimeValue::string(v.bytes);
                    }
                    return std::nullopt;
                },
                c.value.data);
        }
        if (std::holds_alternative<MirOperand::ConstItem>(op.data)) {
            DefId def = std::get<MirOperand::ConstItem>(op.data).def;
            return mir_eval_->eval_const(def);
        }

        // v0.0.20 MVP: support only literal/const-item comptime args at codegen
        // time. More general MIR residualization happens in later milestones.
        error(use_site,
              "unsupported comptime argument form in MIR codegen "
              "(expected a literal or `const` item)");
        return std::nullopt;
    }

    MirPlaceAddr mir_place_addr(MirFnCtx& f, const MirPlace& place) {
        MirPlaceAddr out{};
        out.ty = checked_.types.error();

        if (std::holds_alternative<MirPlace::Local>(place.base)) {
            MirLocalId id = std::get<MirPlace::Local>(place.base).local;
            if (id < f.locals.size() && f.body && id < f.body->locals.size()) {
                out.ptr = f.locals[static_cast<size_t>(id)];
                out.ty = f.body->locals[static_cast<size_t>(id)].ty;
            }
        } else if (std::holds_alternative<MirPlace::Static>(place.base)) {
            DefId def = std::get<MirPlace::Static>(place.base).def;
            const ItemStatic* st = static_def(def);
            llvm::GlobalVariable* g =
                st ? get_or_create_static_global(st) : nullptr;
            out.ptr = g;
            out.ty = type_of_static(st);
        }

        for (const MirProjection& proj : place.projection) {
            if (!out.ptr) break;

            if (std::holds_alternative<MirProjection::Downcast>(proj.data)) {
                const std::uint32_t variant_index =
                    std::get<MirProjection::Downcast>(proj.data).variant_index;
                const TypeData& td = checked_.types.get(out.ty);
                if (td.kind != TypeKind::Enum || !td.enum_def) {
                    error(Span{}, "invalid enum downcast in MIR place");
                    break;
                }
                out.enum_payload = MirPlaceAddr::EnumPayload{
                    .enum_ty = out.ty, .variant_index = variant_index};

                auto el = layout_.enum_layout(td.enum_def, Span{});
                if (!el) break;
                llvm::StructType* ll_enum =
                    llvm::cast<llvm::StructType>(llvm_type(out.ty));
                out.ptr = builder_.CreateStructGEP(ll_enum, out.ptr, 1);
                continue;
            }

            if (std::holds_alternative<MirProjection::Deref>(proj.data)) {
                const TypeData& td = checked_.types.get(out.ty);
                if (td.kind != TypeKind::Ptr) {
                    error(Span{}, "invalid deref projection in MIR place");
                    break;
                }
                llvm::Value* p = emit_load(out.ty, out.ptr);
                out.ptr = p;
                out.ty = td.pointee;
                out.enum_payload.reset();
                continue;
            }

            if (std::holds_alternative<MirProjection::Index>(proj.data)) {
                MirLocalId idx_local =
                    std::get<MirProjection::Index>(proj.data).index_local;
                if (!f.body || idx_local >= f.locals.size() ||
                    idx_local >= f.body->locals.size()) {
                    error(Span{}, "invalid MIR index local");
                    break;
                }
                TypeId idx_ty =
                    f.body->locals[static_cast<size_t>(idx_local)].ty;
                llvm::Value* idx_val =
                    emit_load(idx_ty, f.locals[static_cast<size_t>(idx_local)]);

                const TypeData& td = checked_.types.get(out.ty);
                if (td.kind == TypeKind::Array) {
                    llvm::Value* zero =
                        llvm::ConstantInt::get(llvm_int_ty(IntKind::Usize), 0);
                    out.ptr = builder_.CreateInBoundsGEP(
                        llvm_type(out.ty), out.ptr, {zero, idx_val});
                    out.ty = td.elem;
                    out.enum_payload.reset();
                    continue;
                }
                if (td.kind == TypeKind::Ptr) {
                    const TypeData& pd = checked_.types.get(td.pointee);
                    if (pd.kind == TypeKind::Slice) {
                        llvm::Value* slice_val = emit_load(out.ty, out.ptr);
                        llvm::Value* data_ptr =
                            builder_.CreateExtractValue(slice_val, {0});
                        out.ptr = builder_.CreateInBoundsGEP(
                            llvm_type(pd.elem), data_ptr, {idx_val});
                        out.ty = pd.elem;
                        out.enum_payload.reset();
                        continue;
                    }
                    llvm::Value* base_ptr_val = emit_load(out.ty, out.ptr);
                    out.ptr = builder_.CreateInBoundsGEP(
                        llvm_type(td.pointee), base_ptr_val, {idx_val});
                    out.ty = td.pointee;
                    out.enum_payload.reset();
                    continue;
                }
                error(Span{}, "invalid index projection in MIR place");
                break;
            }

            if (std::holds_alternative<MirProjection::Field>(proj.data)) {
                const std::uint32_t index =
                    std::get<MirProjection::Field>(proj.data).index;

                if (out.enum_payload) {
                    const TypeData& td =
                        checked_.types.get(out.enum_payload->enum_ty);
                    if (td.kind != TypeKind::Enum || !td.enum_def) break;
                    auto ei_it = checked_.enum_info.find(td.enum_def);
                    if (ei_it == checked_.enum_info.end()) break;
                    const EnumInfo& ei = ei_it->second;
                    if (out.enum_payload->variant_index >=
                        ei.variants_in_order.size())
                        break;
                    const std::string& vname =
                        ei.variants_in_order[out.enum_payload->variant_index];
                    auto vit = ei.variants.find(vname);
                    if (vit == ei.variants.end()) break;
                    const VariantInfo& vi = vit->second;
                    if (index >= vi.payload.size()) break;
                    TypeId field_ty = vi.payload[index];

                    std::uint64_t off = 0;
                    for (size_t i = 0; i < index; i++) {
                        TypeId pt = vi.payload[i];
                        auto al = layout_.align_of(pt, Span{});
                        auto sz = layout_.size_of(pt, Span{});
                        if (!al || !sz) break;
                        off = align_up(off, *al);
                        off += *sz;
                    }
                    auto al = layout_.align_of(field_ty, Span{});
                    if (al) off = align_up(off, *al);

                    out.ptr = builder_.CreateInBoundsGEP(
                        llvm::Type::getInt8Ty(ctx_), out.ptr,
                        builder_.getInt64(static_cast<std::uint64_t>(off)));
                    out.ty = field_ty;
                    out.enum_payload.reset();
                    continue;
                }

                const TypeData& td = checked_.types.get(out.ty);
                if (td.kind == TypeKind::Struct && td.struct_def) {
                    auto si_it = checked_.struct_info.find(td.struct_def);
                    if (si_it == checked_.struct_info.end()) break;
                    if (index >= si_it->second.fields_in_order.size()) break;
                    llvm::StructType* ll_struct =
                        llvm::cast<llvm::StructType>(llvm_type(out.ty));
                    out.ptr = builder_.CreateStructGEP(
                        ll_struct, out.ptr, static_cast<unsigned>(index));
                    out.ty = si_it->second.fields_in_order[index].type;
                    continue;
                }
                if (td.kind == TypeKind::Tuple) {
                    if (index >= td.tuple_elems.size()) break;
                    llvm::StructType* ll_tuple =
                        llvm::cast<llvm::StructType>(llvm_type(out.ty));
                    out.ptr = builder_.CreateStructGEP(
                        ll_tuple, out.ptr, static_cast<unsigned>(index));
                    out.ty = td.tuple_elems[index];
                    continue;
                }
                error(Span{}, "invalid field projection in MIR place");
                break;
            }
        }

        return out;
    }

    CgValue mir_emit_operand(MirFnCtx& f, const MirOperand& op) {
        if (std::holds_alternative<MirOperand::Const>(op.data)) {
            const auto& c = std::get<MirOperand::Const>(op.data);
            llvm::Constant* ll = zero_init(llvm_type(c.ty));
            std::visit(
                [&](const auto& v) {
                    using T = std::decay_t<decltype(v)>;
                    if constexpr (std::is_same_v<T, MirConst::Unit>) {
                        ll = zero_init(llvm_type(c.ty));
                    } else if constexpr (std::is_same_v<T, MirConst::Bool>) {
                        ll = llvm::ConstantInt::get(llvm_type(c.ty),
                                                    v.v ? 1 : 0);
                    } else if constexpr (std::is_same_v<T, MirConst::Int>) {
                        ll = llvm::ConstantInt::get(
                            llvm_type(c.ty), static_cast<std::uint64_t>(v.v),
                            /*isSigned=*/true);
                    } else if constexpr (std::is_same_v<T, MirConst::Float>) {
                        ll = llvm::ConstantFP::get(llvm_type(c.ty), v.v);
                    } else if constexpr (std::is_same_v<T, MirConst::String>) {
                        (void)v.is_c_string;
                        ll = llvm_const_value(
                            c.ty, ComptimeValue::string(v.bytes), Span{});
                    }
                },
                c.value.data);
            return CgValue{.type = c.ty, .value = ll};
        }
        if (std::holds_alternative<MirOperand::Copy>(op.data)) {
            const auto& c = std::get<MirOperand::Copy>(op.data);
            TypeId ty = mir_place_type(f, c.place);
            MirPlaceAddr addr = mir_place_addr(f, c.place);
            return CgValue{.type = ty, .value = emit_load(ty, addr.ptr)};
        }
        if (std::holds_alternative<MirOperand::Move>(op.data)) {
            const auto& c = std::get<MirOperand::Move>(op.data);
            TypeId ty = mir_place_type(f, c.place);
            MirPlaceAddr addr = mir_place_addr(f, c.place);
            return CgValue{.type = ty, .value = emit_load(ty, addr.ptr)};
        }
        if (std::holds_alternative<MirOperand::Fn>(op.data)) {
            DefId def = std::get<MirOperand::Fn>(op.data).def;
            const ItemFn* fn = fn_def(def);
            llvm::Function* callee = fn ? llvm_fn(fn) : nullptr;
            if (!callee)
                return CgValue{.type = checked_.types.error(),
                               .value = nullptr};
            return CgValue{.type = type_of_fn_value(fn),
                           .value = builder_.CreateBitCast(callee, ptr_ty())};
        }
        if (std::holds_alternative<MirOperand::ConstItem>(op.data)) {
            DefId def = std::get<MirOperand::ConstItem>(op.data).def;
            const MirBody* b = mir_ ? mir_->body_for_const(def) : nullptr;
            TypeId ty = b ? b->ret_ty : checked_.types.error();
            if (!mir_eval_)
                return CgValue{.type = ty, .value = zero_init(llvm_type(ty))};
            auto v = mir_eval_->eval_const(def);
            if (!v) {
                return CgValue{.type = ty, .value = zero_init(llvm_type(ty))};
            }
            llvm::Constant* c = llvm_const_value(ty, *v, Span{});
            return CgValue{.type = ty, .value = c};
        }
        error(Span{}, "unsupported MIR operand in LLVM lowering");
        return CgValue{.type = checked_.types.error(), .value = nullptr};
    }

    CgValue mir_emit_rvalue(MirFnCtx& f, const MirRvalue& rv, TypeId expected,
                            Span use_site) {
        if (std::holds_alternative<MirRvalue::Use>(rv.data)) {
            CgValue v =
                mir_emit_operand(f, std::get<MirRvalue::Use>(rv.data).op);
            return mir_coerce_value(v, expected, use_site);
        }
        if (std::holds_alternative<MirRvalue::Unary>(rv.data)) {
            const auto& u = std::get<MirRvalue::Unary>(rv.data);
            CgValue ov = mir_emit_operand(f, u.operand);
            TypeId op_ty = ov.type;
            const TypeData& td = checked_.types.get(op_ty);
            switch (u.op) {
                case UnaryOp::Neg:
                    if (td.kind == TypeKind::Int)
                        return CgValue{.type = op_ty,
                                       .value = builder_.CreateNeg(ov.value)};
                    if (td.kind == TypeKind::Float)
                        return CgValue{.type = op_ty,
                                       .value = builder_.CreateFNeg(ov.value)};
                    break;
                case UnaryOp::Not:
                    return CgValue{.type = checked_.types.bool_(),
                                   .value = builder_.CreateNot(ov.value)};
                case UnaryOp::BitNot:
                    return CgValue{.type = op_ty,
                                   .value = builder_.CreateNot(ov.value)};
                case UnaryOp::Deref: {
                    if (td.kind != TypeKind::Ptr) break;
                    TypeId pointee = td.pointee;
                    llvm::Value* v =
                        builder_.CreateLoad(llvm_type(pointee), ov.value);
                    return CgValue{.type = pointee, .value = v};
                }
                case UnaryOp::AddrOf:
                case UnaryOp::AddrOfMut:
                    break;
            }
            error(use_site, "unsupported unary op in MIR codegen");
            return CgValue{.type = expected,
                           .value = zero_init(llvm_type(expected))};
        }
        if (std::holds_alternative<MirRvalue::Binary>(rv.data)) {
            const auto& b = std::get<MirRvalue::Binary>(rv.data);
            CgValue lv = mir_emit_operand(f, b.lhs);
            CgValue rvv = mir_emit_operand(f, b.rhs);
            if (!checked_.types.equal(lv.type, rvv.type) &&
                checked_.types.can_coerce(rvv.type, lv.type)) {
                rvv = mir_cast_value(rvv, lv.type);
            }
            TypeId op_ty = lv.type;
            const TypeData& td = checked_.types.get(op_ty);
            llvm::Value* out = nullptr;

            auto shift_kind = [&]() -> bool {
                return b.op == BinaryOp::Shl || b.op == BinaryOp::Shr;
            };

            if (td.kind == TypeKind::Int || td.kind == TypeKind::Bool ||
                shift_kind()) {
                switch (b.op) {
                    case BinaryOp::Add:
                        out = builder_.CreateAdd(lv.value, rvv.value);
                        break;
                    case BinaryOp::Sub:
                        out = builder_.CreateSub(lv.value, rvv.value);
                        break;
                    case BinaryOp::Mul:
                        out = builder_.CreateMul(lv.value, rvv.value);
                        break;
                    case BinaryOp::Div: {
                        bool signed_int = td.kind == TypeKind::Int &&
                                          is_signed_int(td.int_kind);
                        out = signed_int
                                  ? builder_.CreateSDiv(lv.value, rvv.value)
                                  : builder_.CreateUDiv(lv.value, rvv.value);
                        break;
                    }
                    case BinaryOp::Mod: {
                        bool signed_int = td.kind == TypeKind::Int &&
                                          is_signed_int(td.int_kind);
                        out = signed_int
                                  ? builder_.CreateSRem(lv.value, rvv.value)
                                  : builder_.CreateURem(lv.value, rvv.value);
                        break;
                    }
                    case BinaryOp::BitAnd:
                        out = builder_.CreateAnd(lv.value, rvv.value);
                        break;
                    case BinaryOp::BitOr:
                        out = builder_.CreateOr(lv.value, rvv.value);
                        break;
                    case BinaryOp::BitXor:
                        out = builder_.CreateXor(lv.value, rvv.value);
                        break;
                    case BinaryOp::Shl:
                        out = builder_.CreateShl(lv.value, rvv.value);
                        break;
                    case BinaryOp::Shr: {
                        bool signed_int = td.kind == TypeKind::Int &&
                                          is_signed_int(td.int_kind);
                        out = signed_int
                                  ? builder_.CreateAShr(lv.value, rvv.value)
                                  : builder_.CreateLShr(lv.value, rvv.value);
                        break;
                    }
                    case BinaryOp::Eq:
                        out = builder_.CreateICmpEQ(lv.value, rvv.value);
                        return CgValue{.type = checked_.types.bool_(),
                                       .value = out};
                    case BinaryOp::Ne:
                        out = builder_.CreateICmpNE(lv.value, rvv.value);
                        return CgValue{.type = checked_.types.bool_(),
                                       .value = out};
                    case BinaryOp::Lt:
                        out = (td.kind == TypeKind::Int &&
                               !is_signed_int(td.int_kind))
                                  ? builder_.CreateICmpULT(lv.value, rvv.value)
                                  : builder_.CreateICmpSLT(lv.value, rvv.value);
                        return CgValue{.type = checked_.types.bool_(),
                                       .value = out};
                    case BinaryOp::Le:
                        out = (td.kind == TypeKind::Int &&
                               !is_signed_int(td.int_kind))
                                  ? builder_.CreateICmpULE(lv.value, rvv.value)
                                  : builder_.CreateICmpSLE(lv.value, rvv.value);
                        return CgValue{.type = checked_.types.bool_(),
                                       .value = out};
                    case BinaryOp::Gt:
                        out = (td.kind == TypeKind::Int &&
                               !is_signed_int(td.int_kind))
                                  ? builder_.CreateICmpUGT(lv.value, rvv.value)
                                  : builder_.CreateICmpSGT(lv.value, rvv.value);
                        return CgValue{.type = checked_.types.bool_(),
                                       .value = out};
                    case BinaryOp::Ge:
                        out = (td.kind == TypeKind::Int &&
                               !is_signed_int(td.int_kind))
                                  ? builder_.CreateICmpUGE(lv.value, rvv.value)
                                  : builder_.CreateICmpSGE(lv.value, rvv.value);
                        return CgValue{.type = checked_.types.bool_(),
                                       .value = out};
                    case BinaryOp::And:
                    case BinaryOp::Or:
                        break;
                }
                return CgValue{.type = op_ty, .value = out};
            }

            if (td.kind == TypeKind::Float) {
                switch (b.op) {
                    case BinaryOp::Add:
                        out = builder_.CreateFAdd(lv.value, rvv.value);
                        break;
                    case BinaryOp::Sub:
                        out = builder_.CreateFSub(lv.value, rvv.value);
                        break;
                    case BinaryOp::Mul:
                        out = builder_.CreateFMul(lv.value, rvv.value);
                        break;
                    case BinaryOp::Div:
                        out = builder_.CreateFDiv(lv.value, rvv.value);
                        break;
                    case BinaryOp::Eq:
                        out = builder_.CreateFCmpOEQ(lv.value, rvv.value);
                        return CgValue{.type = checked_.types.bool_(),
                                       .value = out};
                    case BinaryOp::Ne:
                        out = builder_.CreateFCmpONE(lv.value, rvv.value);
                        return CgValue{.type = checked_.types.bool_(),
                                       .value = out};
                    case BinaryOp::Lt:
                        out = builder_.CreateFCmpOLT(lv.value, rvv.value);
                        return CgValue{.type = checked_.types.bool_(),
                                       .value = out};
                    case BinaryOp::Le:
                        out = builder_.CreateFCmpOLE(lv.value, rvv.value);
                        return CgValue{.type = checked_.types.bool_(),
                                       .value = out};
                    case BinaryOp::Gt:
                        out = builder_.CreateFCmpOGT(lv.value, rvv.value);
                        return CgValue{.type = checked_.types.bool_(),
                                       .value = out};
                    case BinaryOp::Ge:
                        out = builder_.CreateFCmpOGE(lv.value, rvv.value);
                        return CgValue{.type = checked_.types.bool_(),
                                       .value = out};
                    default:
                        break;
                }
                return CgValue{.type = op_ty, .value = out};
            }

            if (td.kind == TypeKind::Ptr &&
                (b.op == BinaryOp::Eq || b.op == BinaryOp::Ne)) {
                out = (b.op == BinaryOp::Eq)
                          ? builder_.CreateICmpEQ(lv.value, rvv.value)
                          : builder_.CreateICmpNE(lv.value, rvv.value);
                return CgValue{.type = checked_.types.bool_(), .value = out};
            }

            error(use_site, "unsupported binary op in MIR codegen");
            return CgValue{.type = expected,
                           .value = zero_init(llvm_type(expected))};
        }
        if (std::holds_alternative<MirRvalue::Cast>(rv.data)) {
            const auto& c = std::get<MirRvalue::Cast>(rv.data);
            CgValue v = mir_emit_operand(f, c.operand);
            return mir_cast_value(v, c.to);
        }
        if (std::holds_alternative<MirRvalue::AddrOf>(rv.data)) {
            const auto& a = std::get<MirRvalue::AddrOf>(rv.data);
            MirPlaceAddr addr = mir_place_addr(f, a.place);
            return CgValue{.type = expected,
                           .value = builder_.CreateBitCast(addr.ptr, ptr_ty())};
        }
        if (std::holds_alternative<MirRvalue::EnumTag>(rv.data)) {
            const auto& t = std::get<MirRvalue::EnumTag>(rv.data);
            CgValue v = mir_emit_operand(f, t.operand);
            llvm::Value* tag = builder_.CreateExtractValue(v.value, {0});
            llvm::Value* out =
                builder_.CreateZExtOrTrunc(tag, llvm_int_ty(IntKind::Usize));
            return CgValue{.type = checked_.types.int_(IntKind::Usize),
                           .value = out};
        }
        if (std::holds_alternative<MirRvalue::Aggregate>(rv.data)) {
            const auto& a = std::get<MirRvalue::Aggregate>(rv.data);
            llvm::Value* agg = llvm::UndefValue::get(llvm_type(a.ty));

            if (a.kind == MirRvalue::AggregateKind::Tuple) {
                const TypeData& td = checked_.types.get(a.ty);
                for (size_t i = 0; i < a.elems.size(); i++) {
                    TypeId elem_ty = i < td.tuple_elems.size()
                                         ? td.tuple_elems[i]
                                         : checked_.types.error();
                    CgValue ev = mir_emit_operand(f, a.elems[i]);
                    ev = mir_coerce_value(ev, elem_ty, use_site);
                    agg = builder_.CreateInsertValue(
                        agg, ev.value, {static_cast<unsigned>(i)});
                }
                return CgValue{.type = a.ty, .value = agg};
            }

            if (a.kind == MirRvalue::AggregateKind::Array) {
                const TypeData& td = checked_.types.get(a.ty);
                for (size_t i = 0; i < a.elems.size(); i++) {
                    CgValue ev = mir_emit_operand(f, a.elems[i]);
                    ev = mir_coerce_value(ev, td.elem, use_site);
                    agg = builder_.CreateInsertValue(
                        agg, ev.value, {static_cast<unsigned>(i)});
                }
                return CgValue{.type = a.ty, .value = agg};
            }

            if (a.kind == MirRvalue::AggregateKind::Struct) {
                const TypeData& td = checked_.types.get(a.ty);
                if (td.kind != TypeKind::Struct || !td.struct_def) {
                    error(use_site, "struct aggregate has non-struct type");
                    return CgValue{.type = a.ty, .value = agg};
                }
                auto si_it = checked_.struct_info.find(td.struct_def);
                if (si_it == checked_.struct_info.end()) {
                    error(use_site, "missing struct info during MIR lowering");
                    return CgValue{.type = a.ty, .value = agg};
                }
                for (size_t i = 0; i < a.elems.size() &&
                                   i < si_it->second.fields_in_order.size();
                     i++) {
                    TypeId field_ty = si_it->second.fields_in_order[i].type;
                    CgValue ev = mir_emit_operand(f, a.elems[i]);
                    ev = mir_coerce_value(ev, field_ty, use_site);
                    agg = builder_.CreateInsertValue(
                        agg, ev.value, {static_cast<unsigned>(i)});
                }
                return CgValue{.type = a.ty, .value = agg};
            }

            if (a.kind == MirRvalue::AggregateKind::EnumVariant) {
                const TypeData& td = checked_.types.get(a.ty);
                if (td.kind != TypeKind::Enum || !td.enum_def) {
                    error(use_site, "enum aggregate has non-enum type");
                    return CgValue{.type = a.ty, .value = agg};
                }
                auto el = layout_.enum_layout(td.enum_def, use_site);
                if (!el) return CgValue{.type = a.ty, .value = agg};

                auto ei_it = checked_.enum_info.find(td.enum_def);
                if (ei_it == checked_.enum_info.end())
                    return CgValue{.type = a.ty, .value = agg};
                const EnumInfo& ei = ei_it->second;
                if (a.variant_index >= ei.variants_in_order.size())
                    return CgValue{.type = a.ty, .value = agg};
                const std::string& vname =
                    ei.variants_in_order[a.variant_index];
                auto vit = ei.variants.find(vname);
                if (vit == ei.variants.end())
                    return CgValue{.type = a.ty, .value = agg};
                const VariantInfo& vi = vit->second;

                std::int64_t disc_value =
                    static_cast<std::int64_t>(a.variant_index);
                if (auto di = ei.discriminants.find(vname);
                    di != ei.discriminants.end())
                    disc_value = di->second;

                llvm::StructType* ll_enum =
                    llvm::cast<llvm::StructType>(llvm_type(a.ty));
                llvm::AllocaInst* slot =
                    create_entry_alloca(f.fn, ll_enum, "enum.tmp");
                llvm::Value* tag_ptr =
                    builder_.CreateStructGEP(ll_enum, slot, 0);
                llvm::Type* tag_ty = llvm_tag_int_bytes(el->tag_size);
                llvm::Value* tag_value = llvm::ConstantInt::get(
                    tag_ty, static_cast<std::uint64_t>(disc_value),
                    /*isSigned=*/true);
                builder_.CreateStore(tag_value, tag_ptr);

                if (!vi.payload.empty() && el->payload_size != 0) {
                    llvm::Value* payload_base =
                        builder_.CreateStructGEP(ll_enum, slot, 1);
                    std::uint64_t off = 0;
                    for (size_t i = 0;
                         i < a.elems.size() && i < vi.payload.size(); i++) {
                        TypeId payload_ty = vi.payload[i];
                        auto al = layout_.align_of(payload_ty, use_site);
                        auto sz = layout_.size_of(payload_ty, use_site);
                        if (!al || !sz) break;
                        off = align_up(off, *al);
                        CgValue ev = mir_emit_operand(f, a.elems[i]);
                        ev = mir_coerce_value(ev, payload_ty, use_site);
                        llvm::Value* gep = builder_.CreateInBoundsGEP(
                            llvm::Type::getInt8Ty(ctx_), payload_base,
                            builder_.getInt64(off));
                        llvm::StoreInst* store =
                            builder_.CreateStore(ev.value, gep);
                        store->setAlignment(llvm::Align(*al));
                        off += *sz;
                    }
                }

                llvm::Value* out = builder_.CreateLoad(ll_enum, slot);
                return CgValue{.type = a.ty, .value = out};
            }
        }

        error(use_site, "unsupported MIR rvalue in LLVM lowering");
        return CgValue{.type = expected,
                       .value = zero_init(llvm_type(expected))};
    }

    void mir_emit_stmt(MirFnCtx& f, const MirStatement& st) {
        if (std::holds_alternative<MirStatement::Assign>(st.data)) {
            const auto& a = std::get<MirStatement::Assign>(st.data);
            TypeId dst_ty = mir_place_type(f, a.dst);
            MirPlaceAddr dst_addr = mir_place_addr(f, a.dst);
            CgValue v = mir_emit_rvalue(
                f, a.src, dst_ty, a.dst.projection.empty() ? Span{} : Span{});
            v = mir_coerce_value(v, dst_ty, Span{});
            emit_store(dst_ty, dst_addr.ptr, v.value);
            return;
        }
        if (std::holds_alternative<MirStatement::Assert>(st.data)) {
            const auto& a = std::get<MirStatement::Assert>(st.data);
            CgValue cond = mir_emit_operand(f, a.cond);
            cond = mir_coerce_value(cond, checked_.types.bool_(), a.span);
            llvm::Function* fn = f.fn;
            llvm::BasicBlock* ok_bb =
                llvm::BasicBlock::Create(ctx_, "assert.ok", fn);
            llvm::BasicBlock* fail_bb =
                llvm::BasicBlock::Create(ctx_, "assert.fail", fn);
            builder_.CreateCondBr(cond.value, ok_bb, fail_bb);

            builder_.SetInsertPoint(fail_bb);
            auto trap = llvm::Intrinsic::getOrInsertDeclaration(
                module_.get(), llvm::Intrinsic::trap);
            builder_.CreateCall(trap);
            builder_.CreateUnreachable();

            builder_.SetInsertPoint(ok_bb);
            return;
        }
        error(Span{}, "unsupported MIR statement in LLVM lowering");
    }

    void mir_emit_terminator(MirFnCtx& f, MirBlockId bb,
                             const MirTerminator& term) {
        if (std::holds_alternative<MirTerminator::Return>(term.data)) {
            if (!f.body) {
                builder_.CreateRetVoid();
                return;
            }
            if (f.body->ret_ty == checked_.types.unit() ||
                f.body->ret_ty == checked_.types.never()) {
                builder_.CreateRetVoid();
                return;
            }
            llvm::Value* rv = emit_load(f.body->ret_ty, f.locals[0]);
            builder_.CreateRet(rv);
            return;
        }
        if (std::holds_alternative<MirTerminator::Goto>(term.data)) {
            builder_.CreateBr(
                f.blocks[std::get<MirTerminator::Goto>(term.data).target]);
            return;
        }
        if (std::holds_alternative<MirTerminator::SwitchInt>(term.data)) {
            const auto& sw = std::get<MirTerminator::SwitchInt>(term.data);
            TypeId scrut_ty = mir_operand_type(f, sw.scrut);
            CgValue scrut = mir_emit_operand(f, sw.scrut);
            llvm::BasicBlock* otherwise_bb = f.blocks[sw.otherwise];
            llvm::SwitchInst* si =
                builder_.CreateSwitch(scrut.value, otherwise_bb,
                                      static_cast<unsigned>(sw.cases.size()));
            llvm::Type* ll_scrut_ty = llvm_type(scrut_ty);
            if (!ll_scrut_ty->isIntegerTy()) {
                error(Span{},
                      "internal error: MIR switch scrutinee is not an integer");
                return;
            }
            for (const auto& [v, target] : sw.cases) {
                llvm::ConstantInt* cv = llvm::ConstantInt::get(
                    llvm::cast<llvm::IntegerType>(ll_scrut_ty),
                    static_cast<std::uint64_t>(v));
                si->addCase(cv, f.blocks[target]);
            }
            return;
        }
        if (std::holds_alternative<MirTerminator::Call>(term.data)) {
            const auto& c = std::get<MirTerminator::Call>(term.data);

            // Direct calls (`Fn(def#)`) vs indirect calls (`fp(...)`).
            if (std::holds_alternative<MirOperand::Fn>(c.callee.data)) {
                DefId callee_def = std::get<MirOperand::Fn>(c.callee.data).def;
                const ItemFn* callee_ast = fn_def(callee_def);
                if (!callee_ast) {
                    error(Span{},
                          "missing function def during MIR call lowering");
                    builder_.CreateBr(f.blocks[c.next]);
                    return;
                }
                auto sig_it = checked_.fn_info.find(callee_ast);
                if (sig_it == checked_.fn_info.end()) {
                    builder_.CreateBr(f.blocks[c.next]);
                    return;
                }
                const FnInfo& sig = sig_it->second;

                bool is_extern_c = false;
                for (const Attr* a : callee_ast->attrs) {
                    if (a && a->name && path_is_ident(a->name, "extern"))
                        is_extern_c = true;
                }

                if (has_comptime_params(sig) && sig.is_variadic) {
                    error(Span{},
                          "functions with comptime parameters cannot be "
                          "variadic (internal limitation)");
                    builder_.CreateBr(f.blocks[c.next]);
                    return;
                }

                std::vector<llvm::Value*> args{};
                std::vector<ComptimeValue> ct_args{};
                args.reserve(c.args.size());

                const Span use_site = f.body ? f.body->span : Span{};
                const size_t fixed = sig.params.size();
                for (size_t i = 0; i < c.args.size(); i++) {
                    const bool is_ct = i < sig.comptime_params.size() &&
                                       sig.comptime_params[i];
                    if (is_ct) {
                        auto v = mir_eval_comptime_operand(
                            f, c.args[i], sig.params[i], use_site);
                        if (v) ct_args.push_back(*v);
                        continue;
                    }

                    CgValue av = mir_emit_operand(f, c.args[i]);
                    if (!sig.is_variadic) {
                        if (i < fixed) {
                            av = mir_coerce_value(av, sig.params[i], use_site);
                        }
                        args.push_back(av.value);
                        continue;
                    }

                    if (i < fixed) {
                        av = mir_coerce_value(av, sig.params[i], use_site);
                        args.push_back(av.value);
                        continue;
                    }

                    // Variadic args.
                    args.push_back(is_extern_c
                                       ? promote_c_vararg(av.type, av.value)
                                       : av.value);
                }

                llvm::Function* callee_ll = nullptr;
                if (has_comptime_params(sig)) {
                    callee_ll = get_or_create_comptime_specialization(
                        callee_ast, sig, ct_args, use_site);
                } else {
                    callee_ll = llvm_fn(callee_ast);
                }

                if (!callee_ll) {
                    builder_.CreateBr(f.blocks[c.next]);
                    return;
                }

                llvm::CallInst* call = builder_.CreateCall(
                    callee_ll->getFunctionType(), callee_ll, args);
                if (c.ret) {
                    TypeId ret_ty = mir_place_type(f, *c.ret);
                    if (llvm_type(ret_ty)->isVoidTy()) {
                        builder_.CreateBr(f.blocks[c.next]);
                        return;
                    }
                    MirPlaceAddr ret_addr = mir_place_addr(f, *c.ret);
                    emit_store(ret_ty, ret_addr.ptr, call);
                }
                builder_.CreateBr(f.blocks[c.next]);
                return;
            }

            // Indirect call through a function pointer.
            CgValue callee_val = mir_emit_operand(f, c.callee);
            TypeId callee_ty = callee_val.type;
            const TypeData& cd = checked_.types.get(callee_ty);
            if (cd.kind != TypeKind::Ptr) {
                error(Span{}, "indirect call on non-pointer type");
                builder_.CreateBr(f.blocks[c.next]);
                return;
            }
            llvm::FunctionType* fty = llvm_fn_type(cd.pointee);
            if (!fty) {
                error(Span{}, "indirect call has non-function pointee type");
                builder_.CreateBr(f.blocks[c.next]);
                return;
            }

            std::vector<llvm::Value*> args{};
            args.reserve(c.args.size());
            for (const MirOperand& a : c.args) {
                CgValue av = mir_emit_operand(f, a);
                args.push_back(av.value);
            }

            llvm::CallInst* call =
                builder_.CreateCall(fty, callee_val.value, args);
            if (c.ret) {
                TypeId ret_ty = mir_place_type(f, *c.ret);
                if (!llvm_type(ret_ty)->isVoidTy()) {
                    MirPlaceAddr ret_addr = mir_place_addr(f, *c.ret);
                    emit_store(ret_ty, ret_addr.ptr, call);
                }
            }
            builder_.CreateBr(f.blocks[c.next]);
            return;
        }
        if (std::holds_alternative<MirTerminator::Unreachable>(term.data)) {
            builder_.CreateUnreachable();
            return;
        }

        error(Span{}, "unsupported MIR terminator in LLVM lowering");
        builder_.CreateUnreachable();
    }

    void mir_emit_body(llvm::Function* ll_fn, const MirBody& body,
                       const FnInfo& sig,
                       const std::vector<ComptimeValue>* comptime_args,
                       Span use_site) {
        if (!ll_fn) return;

        // Construct blocks.
        llvm::BasicBlock* entry =
            llvm::BasicBlock::Create(ctx_, "entry", ll_fn);
        MirFnCtx f{};
        f.fn = ll_fn;
        f.body = &body;
        f.blocks.resize(body.blocks.size(), nullptr);
        for (size_t i = 0; i < body.blocks.size(); i++) {
            f.blocks[i] =
                llvm::BasicBlock::Create(ctx_, "bb" + std::to_string(i), ll_fn);
        }

        // Allocate locals.
        builder_.SetInsertPoint(entry);
        f.locals.resize(body.locals.size(), nullptr);
        for (size_t i = 0; i < body.locals.size(); i++) {
            TypeId ty = body.locals[i].ty;
            llvm::Type* ll_ty = llvm_type(ty);
            if (ll_ty->isVoidTy()) continue;
            f.locals[i] =
                create_entry_alloca(ll_fn, ll_ty, body.locals[i].name);
        }

        // Store params.
        size_t ct_index = 0;
        size_t runtime_index = 0;
        for (size_t i = 0; i < sig.params.size(); i++) {
            const bool is_ct =
                i < sig.comptime_params.size() && sig.comptime_params[i];
            const MirLocalId local_id = static_cast<MirLocalId>(1 + i);
            if (local_id >= body.locals.size()) break;

            TypeId param_ty = sig.params[i];
            if (is_ct) {
                if (!comptime_args || ct_index >= comptime_args->size()) {
                    error(use_site, "internal error: missing comptime arg");
                    ct_index++;
                    continue;
                }
                if (f.locals[local_id]) {
                    llvm::Constant* c = llvm_const_value(
                        param_ty, (*comptime_args)[ct_index], use_site);
                    emit_store(param_ty, f.locals[local_id], c);
                }
                ct_index++;
                continue;
            }

            if (runtime_index >= ll_fn->arg_size()) {
                error(use_site, "internal error: missing runtime argument");
                runtime_index++;
                continue;
            }
            if (f.locals[local_id]) {
                llvm::Argument* arg =
                    ll_fn->getArg(static_cast<unsigned>(runtime_index));
                emit_store(param_ty, f.locals[local_id], arg);
            }
            runtime_index++;
        }

        builder_.CreateBr(f.blocks[body.start]);

        // Emit each block.
        for (size_t i = 0; i < body.blocks.size(); i++) {
            builder_.SetInsertPoint(f.blocks[i]);
            const MirBlock& b = body.blocks[i];
            for (const MirStatement& st : b.stmts) mir_emit_stmt(f, st);
            if (!builder_.GetInsertBlock()->getTerminator())
                mir_emit_terminator(f, static_cast<MirBlockId>(i), b.term);
        }
    }

    void mir_emit_function(const ItemFn* fn) {
        if (!fn || !fn->decl || !fn->decl->sig || !fn->body) return;
        if (emitted_fns_.contains(fn)) return;

        auto sig_it = checked_.fn_info.find(fn);
        if (sig_it == checked_.fn_info.end()) return;
        const FnInfo& sig = sig_it->second;
        if (has_comptime_params(sig)) {
            // Emitted on demand as `$ct...` variants.
            return;
        }

        llvm::Function* ll_fn = llvm_fn(fn);
        if (!ll_fn) return;

        auto dit = hir_->def_ids.find(static_cast<const Item*>(fn));
        if (dit == hir_->def_ids.end()) return;
        const MirBody* body = mir_->body_for_fn(dit->second);
        if (!body) return;

        emitted_fns_.insert(fn);
        llvm::IRBuilder<>::InsertPointGuard guard(builder_);
        mir_emit_body(ll_fn, *body, sig, /*comptime_args=*/nullptr, fn->span);
    }

    struct FnCtx {
        ModuleId mid = 0;
        const ItemFn* fn = nullptr;
        llvm::Function* llvm_fn = nullptr;
        std::vector<std::unordered_map<std::string, LocalSlot>> scopes{};
        std::unordered_map<std::string, CgValue> comptime_consts{};
        struct LoopTargets {
            llvm::BasicBlock* break_bb = nullptr;
            llvm::BasicBlock* continue_bb = nullptr;
        };
        std::vector<LoopTargets> loops{};
    };

    void push_scope(FnCtx& f) { f.scopes.emplace_back(); }

    void pop_scope(FnCtx& f) {
        if (!f.scopes.empty()) f.scopes.pop_back();
    }

    std::optional<LocalSlot> lookup_local(const FnCtx& f,
                                          std::string_view name) const {
        for (auto it = f.scopes.rbegin(); it != f.scopes.rend(); ++it) {
            auto found = it->find(std::string(name));
            if (found != it->end()) return found->second;
        }
        return std::nullopt;
    }

    void declare_local(FnCtx& f, std::string name, LocalSlot slot) {
        if (f.scopes.empty()) push_scope(f);
        f.scopes.back().insert({std::move(name), slot});
    }

    void emit_comptime_specialization(
        const ItemFn* fn, llvm::Function* specialized, const FnInfo& sig,
        const std::vector<ComptimeValue>& comptime_args, Span use_site) {
        if (!fn || !fn->decl || !fn->decl->sig || !fn->body) return;

        FnCtx f{};
        f.mid = locator_.module_of(static_cast<const Item*>(fn));
        f.fn = fn;
        f.llvm_fn = specialized;

        llvm::BasicBlock* entry =
            llvm::BasicBlock::Create(ctx_, "entry", specialized);
        builder_.SetInsertPoint(entry);
        push_scope(f);

        size_t ct_index = 0;
        size_t runtime_index = 0;
        size_t param_index = 0;
        for (const Param* p : fn->decl->sig->params) {
            if (!p) continue;
            if (param_index >= sig.params.size()) break;
            TypeId param_ty = sig.params[param_index];
            const bool is_ct = param_index < sig.comptime_params.size() &&
                               sig.comptime_params[param_index];
            if (is_ct) {
                if (ct_index >= comptime_args.size()) {
                    error(use_site,
                          "internal error: missing comptime arg while emitting "
                          "specialization");
                    return;
                }
                llvm::Constant* c = llvm_const_value(
                    param_ty, comptime_args[ct_index], p->span);
                f.comptime_consts.insert(
                    {p->name, CgValue{.type = param_ty, .value = c}});
                ct_index++;
            } else {
                llvm::Type* ll_ty = llvm_type(param_ty);
                llvm::AllocaInst* slot =
                    create_entry_alloca(specialized, ll_ty, p->name);
                builder_.CreateStore(
                    specialized->getArg(static_cast<unsigned>(runtime_index)),
                    slot);
                declare_local(f, p->name,
                              LocalSlot{.type = param_ty, .alloca = slot});
                runtime_index++;
            }
            param_index++;
        }

        CgValue body = emit_block(f, fn->body);

        if (!builder_.GetInsertBlock()->getTerminator()) {
            llvm::Type* ll_ret = llvm_type(sig.ret);
            if (ll_ret->isVoidTy()) {
                builder_.CreateRetVoid();
            } else if (body.value) {
                if (!checked_.types.equal(body.type, sig.ret))
                    body = emit_cast(f, body, sig.ret);
                builder_.CreateRet(body.value);
            } else {
                builder_.CreateRet(zero_init(ll_ret));
            }
        }
        pop_scope(f);
    }

    llvm::Value* promote_c_vararg(TypeId ty, llvm::Value* v) {
        if (!v) return nullptr;
        const TypeData& d = checked_.types.get(ty);
        if (d.kind == TypeKind::Float) {
            if (d.float_kind == FloatKind::F32)
                return builder_.CreateFPExt(v, llvm::Type::getDoubleTy(ctx_));
            return v;
        }
        if (d.kind == TypeKind::Bool) {
            return builder_.CreateZExt(v, llvm::Type::getInt32Ty(ctx_));
        }
        if (d.kind == TypeKind::Int) {
            std::uint32_t bits = int_bits(d.int_kind, ptr_bits());
            if (bits < 32) {
                llvm::Type* i32 = llvm::Type::getInt32Ty(ctx_);
                return is_signed_int(d.int_kind) ? builder_.CreateSExt(v, i32)
                                                 : builder_.CreateZExt(v, i32);
            }
        }
        return v;
    }

    llvm::AllocaInst* create_entry_alloca(llvm::Function* fn, llvm::Type* ty,
                                          std::string_view name) {
        llvm::IRBuilder<> entry_builder(&fn->getEntryBlock(),
                                        fn->getEntryBlock().begin());
        return entry_builder.CreateAlloca(
            ty, nullptr, llvm::StringRef(name.data(), name.size()));
    }

    llvm::Constant* zero_init(llvm::Type* ty) {
        return llvm::Constant::getNullValue(ty);
    }

    llvm::GlobalVariable* get_or_create_static_global(const ItemStatic* st) {
        if (!st) return nullptr;
        if (auto it = static_globals_.find(st); it != static_globals_.end())
            return it->second;

        auto ty_it = checked_.static_types.find(st);
        if (ty_it == checked_.static_types.end()) {
            error(st->span, "internal error: missing type for `static " +
                                st->name + "` in LLVM backend");
            return nullptr;
        }
        TypeId ty = ty_it->second;
        llvm::Type* ll_ty = llvm_type(ty);
        if (ll_ty->isVoidTy()) {
            error(st->span,
                  "`static` of type `()` is not supported in LLVM backend");
            return nullptr;
        }

        llvm::Constant* init = zero_init(ll_ty);
        if (mir_eval_ && hir_) {
            auto dit = hir_->def_ids.find(static_cast<const Item*>(st));
            if (dit != hir_->def_ids.end()) {
                if (auto v = mir_eval_->eval_static(dit->second)) {
                    if (llvm::Constant* c = llvm_const_value(ty, *v, st->span))
                        init = c;
                }
            }
        }

        std::string name = locator_.symbol_for_static(st);
        auto* g = new llvm::GlobalVariable(
            *module_, ll_ty,
            /*isConstant=*/true, llvm::GlobalValue::InternalLinkage, init,
            llvm::StringRef(name.data(), name.size()));

        if (auto al = layout_.align_of(ty, st->span))
            g->setAlignment(llvm::Align(*al));
        static_globals_.insert({st, g});
        return g;
    }

    llvm::Constant* llvm_const_value(TypeId ty, const ComptimeValue& v,
                                     Span use_site) {
        llvm::Type* ll_ty = llvm_type(ty);
        if (ll_ty->isVoidTy()) return nullptr;

        const TypeData& td = checked_.types.get(ty);
        switch (v.kind) {
            case ComptimeValue::Kind::Error:
                return zero_init(ll_ty);
            case ComptimeValue::Kind::Unit:
                return zero_init(ll_ty);
            case ComptimeValue::Kind::Bool:
                if (td.kind != TypeKind::Bool) return zero_init(ll_ty);
                return llvm::ConstantInt::get(llvm::Type::getInt1Ty(ctx_),
                                              v.bool_value ? 1 : 0);
            case ComptimeValue::Kind::Int:
                if (td.kind != TypeKind::Int) return zero_init(ll_ty);
                return llvm::ConstantInt::get(
                    ll_ty, static_cast<std::uint64_t>(v.int_value),
                    /*isSigned=*/true);
            case ComptimeValue::Kind::Float:
                if (td.kind != TypeKind::Float) return zero_init(ll_ty);
                return llvm::ConstantFP::get(ll_ty, v.float_value);
            case ComptimeValue::Kind::Array: {
                if (td.kind != TypeKind::Array) {
                    error(use_site,
                          "array comptime value requires an array type");
                    return zero_init(ll_ty);
                }
                auto* arr_ty = llvm::cast<llvm::ArrayType>(ll_ty);
                std::uint64_t n = arr_ty->getNumElements();
                if (v.array_elems.size() != n) {
                    error(use_site,
                          "array comptime value length does not match expected "
                          "array type");
                }
                std::vector<llvm::Constant*> elems{};
                elems.reserve(static_cast<size_t>(n));
                for (std::uint64_t i = 0; i < n; i++) {
                    if (i < v.array_elems.size()) {
                        elems.push_back(llvm_const_value(
                            td.elem, v.array_elems[i], use_site));
                    } else {
                        elems.push_back(zero_init(llvm_type(td.elem)));
                    }
                }
                return llvm::ConstantArray::get(arr_ty, elems);
            }
            case ComptimeValue::Kind::String: {
                if (td.kind != TypeKind::Ptr) {
                    error(use_site,
                          "string comptime value requires a pointer type");
                    return zero_init(ll_ty);
                }
                const TypeData& pd = checked_.types.get(td.pointee);

                const llvm::StringRef bytes{v.string_value.data(),
                                            v.string_value.size()};

                // `const* u8`: C string pointer (NUL-terminated).
                if (pd.kind == TypeKind::Int && pd.int_kind == IntKind::U8) {
                    llvm::Constant* data = llvm::ConstantDataArray::getString(
                        ctx_, bytes, /*AddNull=*/true);
                    auto* g = new llvm::GlobalVariable(
                        *module_, data->getType(),
                        /*isConstant=*/true, llvm::GlobalValue::PrivateLinkage,
                        data, "cstr");
                    g->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
                    g->setAlignment(llvm::Align(1));

                    llvm::Constant* z =
                        llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx_), 0);
                    std::array<llvm::Constant*, 2> idxs{z, z};
                    llvm::Constant* gep =
                        llvm::ConstantExpr::getInBoundsGetElementPtr(
                            g->getValueType(), g, idxs);
                    return llvm::ConstantExpr::getBitCast(gep, ptr_ty());
                }

                // `const* [u8]`: slice pointer { ptr, len }.
                if (pd.kind == TypeKind::Slice) {
                    const TypeData& ed = checked_.types.get(pd.elem);
                    if (ed.kind != TypeKind::Int ||
                        ed.int_kind != IntKind::U8) {
                        error(use_site,
                              "string comptime value only supports `u8` slices "
                              "in LLVM backend");
                        return zero_init(ll_ty);
                    }

                    llvm::Constant* ptr =
                        llvm::ConstantPointerNull::get(ptr_ty());
                    if (!bytes.empty()) {
                        llvm::Constant* data =
                            llvm::ConstantDataArray::getString(
                                ctx_, bytes, /*AddNull=*/false);
                        auto* g = new llvm::GlobalVariable(
                            *module_, data->getType(),
                            /*isConstant=*/true,
                            llvm::GlobalValue::PrivateLinkage, data, "str");
                        g->setUnnamedAddr(
                            llvm::GlobalValue::UnnamedAddr::Global);
                        g->setAlignment(llvm::Align(1));

                        llvm::Constant* z = llvm::ConstantInt::get(
                            llvm::Type::getInt32Ty(ctx_), 0);
                        std::array<llvm::Constant*, 2> idxs{z, z};
                        llvm::Constant* gep =
                            llvm::ConstantExpr::getInBoundsGetElementPtr(
                                g->getValueType(), g, idxs);
                        ptr = llvm::ConstantExpr::getBitCast(gep, ptr_ty());
                    }
                    llvm::Constant* len = llvm::ConstantInt::get(
                        llvm_int_ty(IntKind::Usize),
                        static_cast<std::uint64_t>(bytes.size()));
                    return llvm::ConstantStruct::get(
                        llvm::cast<llvm::StructType>(ll_ty), {ptr, len});
                }

                error(use_site,
                      "string comptime value requires `const* u8` or `const* "
                      "[u8]`");
                return zero_init(ll_ty);
            }
            case ComptimeValue::Kind::Struct: {
                if (td.kind != TypeKind::Struct || !td.struct_def) {
                    error(use_site,
                          "struct comptime value requires a struct type");
                    return zero_init(ll_ty);
                }
                if (v.struct_def != td.struct_def) {
                    error(use_site,
                          "struct comptime value does not match expected "
                          "struct type");
                    return zero_init(ll_ty);
                }
                auto si_it = checked_.struct_info.find(td.struct_def);
                if (si_it == checked_.struct_info.end()) {
                    error(use_site,
                          "internal error: missing struct layout info during "
                          "constant lowering");
                    return zero_init(ll_ty);
                }
                std::vector<llvm::Constant*> field_consts{};
                field_consts.reserve(si_it->second.fields_in_order.size());
                for (const auto& fld : si_it->second.fields_in_order) {
                    auto it = v.struct_fields.find(fld.name);
                    if (it == v.struct_fields.end()) {
                        error(use_site, "missing field `" + fld.name +
                                            "` in struct comptime value");
                        field_consts.push_back(zero_init(llvm_type(fld.type)));
                        continue;
                    }
                    field_consts.push_back(
                        llvm_const_value(fld.type, it->second, use_site));
                }
                return llvm::ConstantStruct::get(
                    llvm::cast<llvm::StructType>(ll_ty), field_consts);
            }
            default:
                error(use_site,
                      "unsupported comptime value in LLVM constant lowering");
                return zero_init(ll_ty);
        }
    }

    llvm::Value* emit_load(TypeId ty, llvm::Value* ptr) {
        llvm::Type* ll_ty = llvm_type(ty);
        if (ll_ty->isVoidTy()) return nullptr;
        return builder_.CreateLoad(ll_ty, ptr);
    }

    void emit_store(TypeId ty, llvm::Value* ptr, llvm::Value* value) {
        llvm::Type* ll_ty = llvm_type(ty);
        if (ll_ty->isVoidTy()) return;
        (void)builder_.CreateStore(value, ptr);
    }

    std::optional<size_t> struct_field_index(const ItemStruct* def,
                                             std::string_view name) const {
        if (!def) return std::nullopt;
        auto it = checked_.struct_info.find(def);
        if (it == checked_.struct_info.end()) return std::nullopt;
        for (size_t i = 0; i < it->second.fields_in_order.size(); i++) {
            if (it->second.fields_in_order[i].name == name) return i;
        }
        return std::nullopt;
    }

    std::optional<size_t> enum_variant_index(const ItemEnum* def,
                                             std::string_view name) const {
        if (!def) return std::nullopt;
        auto it = checked_.enum_info.find(def);
        if (it == checked_.enum_info.end()) return std::nullopt;
        for (size_t i = 0; i < it->second.variants_in_order.size(); i++) {
            if (it->second.variants_in_order[i] == name) return i;
        }
        return std::nullopt;
    }

    std::optional<std::int64_t> enum_variant_discriminant(
        const ItemEnum* def, std::string_view name) const {
        if (!def) return std::nullopt;
        auto it = checked_.enum_info.find(def);
        if (it == checked_.enum_info.end()) return std::nullopt;
        if (auto di = it->second.discriminants.find(std::string(name));
            di != it->second.discriminants.end())
            return di->second;
        if (auto idx = enum_variant_index(def, name))
            return static_cast<std::int64_t>(*idx);
        return std::nullopt;
    }

    const VariantInfo* enum_variant_info(const ItemEnum* def,
                                         std::string_view name) const {
        if (!def) return nullptr;
        auto it = checked_.enum_info.find(def);
        if (it == checked_.enum_info.end()) return nullptr;
        auto vit = it->second.variants.find(std::string(name));
        if (vit == it->second.variants.end()) return nullptr;
        return &vit->second;
    }

    llvm::Value* emit_enum_payload_base_i8(TypeId enum_ty,
                                           llvm::Value* enum_ptr,
                                           const EnumLayout& el) {
        if (el.payload_size == 0) return nullptr;
        llvm::StructType* ll_enum =
            llvm::cast<llvm::StructType>(llvm_type(enum_ty));
        return builder_.CreateStructGEP(ll_enum, enum_ptr, 1);
    }

    CgValue emit_enum_ctor(FnCtx& f, TypeId enum_ty, const ItemEnum* def,
                           std::string_view variant,
                           const std::vector<Expr*>& args) {
        auto enum_layout = layout_.enum_layout(def, Span{});
        if (!enum_layout)
            return CgValue{.type = enum_ty,
                           .value = llvm::UndefValue::get(llvm_type(enum_ty))};

        auto disc_value = enum_variant_discriminant(def, variant);
        const VariantInfo* variant_info = enum_variant_info(def, variant);
        if (!disc_value || !variant_info)
            return CgValue{.type = enum_ty,
                           .value = llvm::UndefValue::get(llvm_type(enum_ty))};

        llvm::StructType* ll_enum =
            llvm::cast<llvm::StructType>(llvm_type(enum_ty));
        llvm::AllocaInst* slot =
            create_entry_alloca(f.llvm_fn, ll_enum, "enum.tmp");

        llvm::Value* tag_ptr = builder_.CreateStructGEP(ll_enum, slot, 0);
        llvm::Type* tag_ty = llvm_tag_int_bytes(enum_layout->tag_size);
        llvm::Value* tag_value = llvm::ConstantInt::get(
            tag_ty, static_cast<std::uint64_t>(*disc_value), /*isSigned=*/true);
        builder_.CreateStore(tag_value, tag_ptr);

        if (!variant_info->payload.empty() && enum_layout->payload_size != 0) {
            if (args.size() != variant_info->payload.size()) {
                error(Span{}, "enum constructor arity mismatch in codegen");
            } else {
                llvm::Value* payload_base =
                    emit_enum_payload_base_i8(enum_ty, slot, *enum_layout);
                std::uint64_t off = 0;
                for (size_t i = 0; i < args.size(); i++) {
                    TypeId payload_ty = variant_info->payload[i];
                    auto al = layout_.align_of(payload_ty, Span{});
                    auto sz = layout_.size_of(payload_ty, Span{});
                    if (!al || !sz) break;
                    off = align_up(off, *al);

                    CgValue arg_v = emit_expr_coerced(f, args[i], payload_ty);
                    llvm::Value* gep = builder_.CreateInBoundsGEP(
                        llvm::Type::getInt8Ty(ctx_), payload_base,
                        builder_.getInt64(off));
                    llvm::StoreInst* store =
                        builder_.CreateStore(arg_v.value, gep);
                    store->setAlignment(llvm::Align(*al));
                    off += *sz;
                }
            }
        }

        llvm::Value* out = builder_.CreateLoad(ll_enum, slot);
        return CgValue{.type = enum_ty, .value = out};
    }

    llvm::Value* emit_pat_test(FnCtx& f, const Pattern* pat, TypeId scrut_ty,
                               llvm::Value* scrut_ptr) {
        if (!pat) return builder_.getTrue();

        switch (pat->kind) {
            case AstNodeKind::PatWildcard:
            case AstNodeKind::PatBinding:
                return builder_.getTrue();
            case AstNodeKind::PatInt: {
                llvm::Value* scrut_val = emit_load(scrut_ty, scrut_ptr);
                auto* ip = static_cast<const PatInt*>(pat);
                llvm::Value* const_val = llvm::ConstantInt::get(
                    llvm_type(scrut_ty), static_cast<std::uint64_t>(ip->value),
                    /*isSigned=*/true);
                return builder_.CreateICmpEQ(scrut_val, const_val);
            }
            case AstNodeKind::PatBool: {
                llvm::Value* scrut_val = emit_load(scrut_ty, scrut_ptr);
                auto* bp = static_cast<const PatBool*>(pat);
                llvm::Value* const_val = llvm::ConstantInt::get(
                    llvm::Type::getInt1Ty(ctx_), bp->value ? 1 : 0);
                return builder_.CreateICmpEQ(scrut_val, const_val);
            }
            case AstNodeKind::PatOr: {
                auto* o = static_cast<const PatOr*>(pat);
                llvm::Value* lhs =
                    emit_pat_test(f, o->lhs, scrut_ty, scrut_ptr);
                llvm::Value* rhs =
                    emit_pat_test(f, o->rhs, scrut_ty, scrut_ptr);
                return builder_.CreateOr(lhs, rhs);
            }
            case AstNodeKind::PatPath:
            case AstNodeKind::PatVariant: {
                const TypeData& td = checked_.types.get(scrut_ty);
                if (td.kind != TypeKind::Enum || !td.enum_def) {
                    error(pat->span, "enum pattern on non-enum in codegen");
                    return builder_.getFalse();
                }
                const ItemEnum* def = td.enum_def;
                auto enum_layout = layout_.enum_layout(def, Span{});
                if (!enum_layout) return builder_.getFalse();

                const Path* path = nullptr;
                const std::vector<Pattern*>* args = nullptr;
                if (pat->kind == AstNodeKind::PatPath) {
                    path = static_cast<const PatPath*>(pat)->path;
                } else {
                    auto* vp = static_cast<const PatVariant*>(pat);
                    path = vp->path;
                    args = &vp->args;
                }

                if (!path || path->segments.empty()) return builder_.getFalse();
                std::string_view variant_name = path->segments.back()->text;
                auto disc_value = enum_variant_discriminant(def, variant_name);
                const VariantInfo* variant_info =
                    enum_variant_info(def, variant_name);
                if (!disc_value || !variant_info) return builder_.getFalse();

                llvm::StructType* ll_enum =
                    llvm::cast<llvm::StructType>(llvm_type(scrut_ty));
                llvm::Value* tag_ptr =
                    builder_.CreateStructGEP(ll_enum, scrut_ptr, 0);
                llvm::Type* tag_ty = llvm_tag_int_bytes(enum_layout->tag_size);
                llvm::Value* tag_val = builder_.CreateLoad(tag_ty, tag_ptr);
                llvm::Value* tag_ok = builder_.CreateICmpEQ(
                    tag_val,
                    llvm::ConstantInt::get(
                        tag_ty, static_cast<std::uint64_t>(*disc_value),
                        /*isSigned=*/true));

                if (!args || args->empty()) return tag_ok;

                llvm::Value* payload_base = emit_enum_payload_base_i8(
                    scrut_ty, scrut_ptr, *enum_layout);
                if (!payload_base) return builder_.getFalse();

                llvm::Value* cond = tag_ok;
                std::uint64_t off = 0;
                for (size_t i = 0; i < args->size(); i++) {
                    const Pattern* arg_pat = (*args)[i];
                    if (!arg_pat) {
                        auto al =
                            layout_.align_of(variant_info->payload[i], Span{});
                        auto sz =
                            layout_.size_of(variant_info->payload[i], Span{});
                        if (!al || !sz) break;
                        off = align_up(off, *al);
                        off += *sz;
                        continue;
                    }

                    if (arg_pat->kind == AstNodeKind::PatWildcard ||
                        arg_pat->kind == AstNodeKind::PatBinding) {
                        auto al =
                            layout_.align_of(variant_info->payload[i], Span{});
                        auto sz =
                            layout_.size_of(variant_info->payload[i], Span{});
                        if (!al || !sz) break;
                        off = align_up(off, *al);
                        off += *sz;
                        continue;
                    }

                    if (arg_pat->kind != AstNodeKind::PatInt &&
                        arg_pat->kind != AstNodeKind::PatBool) {
                        error(
                            arg_pat->span,
                            "unsupported nested pattern in enum match codegen");
                        return builder_.getFalse();
                    }

                    TypeId payload_ty = variant_info->payload[i];
                    auto al = layout_.align_of(payload_ty, Span{});
                    auto sz = layout_.size_of(payload_ty, Span{});
                    if (!al || !sz) break;
                    off = align_up(off, *al);

                    llvm::Value* gep = builder_.CreateInBoundsGEP(
                        llvm::Type::getInt8Ty(ctx_), payload_base,
                        builder_.getInt64(off));
                    llvm::Value* pv =
                        builder_.CreateLoad(llvm_type(payload_ty), gep);

                    llvm::Value* test = builder_.getFalse();
                    if (arg_pat->kind == AstNodeKind::PatInt) {
                        auto* ip = static_cast<const PatInt*>(arg_pat);
                        llvm::Value* cv = llvm::ConstantInt::get(
                            llvm_type(payload_ty),
                            static_cast<std::uint64_t>(ip->value),
                            /*isSigned=*/true);
                        test = builder_.CreateICmpEQ(pv, cv);
                    } else {
                        auto* bp = static_cast<const PatBool*>(arg_pat);
                        llvm::Value* cv = llvm::ConstantInt::get(
                            llvm::Type::getInt1Ty(ctx_), bp->value ? 1 : 0);
                        test = builder_.CreateICmpEQ(pv, cv);
                    }

                    cond = builder_.CreateAnd(cond, test);
                    off += *sz;
                }

                return cond;
            }
            default:
                error(pat->span, "unsupported pattern in codegen");
                return builder_.getFalse();
        }
    }

    void emit_pat_bindings(FnCtx& f, const Pattern* pat, TypeId scrut_ty,
                           llvm::Value* scrut_ptr) {
        if (!pat) return;
        switch (pat->kind) {
            case AstNodeKind::PatWildcard:
            case AstNodeKind::PatInt:
            case AstNodeKind::PatBool:
            case AstNodeKind::PatPath:
            case AstNodeKind::PatOr:
                return;
            case AstNodeKind::PatBinding: {
                auto* b = static_cast<const PatBinding*>(pat);
                llvm::Type* ll_ty = llvm_type(scrut_ty);
                llvm::AllocaInst* slot =
                    create_entry_alloca(f.llvm_fn, ll_ty, "pat");
                llvm::Value* v = emit_load(scrut_ty, scrut_ptr);
                emit_store(scrut_ty, slot, v);
                declare_local(f, b->name,
                              LocalSlot{.type = scrut_ty, .alloca = slot});
                return;
            }
            case AstNodeKind::PatVariant: {
                auto* vp = static_cast<const PatVariant*>(pat);
                const TypeData& td = checked_.types.get(scrut_ty);
                if (td.kind != TypeKind::Enum || !td.enum_def) return;
                const ItemEnum* def = td.enum_def;

                if (!vp->path || vp->path->segments.empty()) return;
                std::string_view variant_name = vp->path->segments.back()->text;
                const VariantInfo* variant_info =
                    enum_variant_info(def, variant_name);
                if (!variant_info) return;
                if (vp->args.size() != variant_info->payload.size()) return;

                auto enum_layout = layout_.enum_layout(def, Span{});
                if (!enum_layout) return;
                llvm::Value* payload_base = emit_enum_payload_base_i8(
                    scrut_ty, scrut_ptr, *enum_layout);
                if (!payload_base) return;

                std::uint64_t off = 0;
                for (size_t i = 0; i < vp->args.size(); i++) {
                    TypeId payload_ty = variant_info->payload[i];
                    auto al = layout_.align_of(payload_ty, Span{});
                    auto sz = layout_.size_of(payload_ty, Span{});
                    if (!al || !sz) return;
                    off = align_up(off, *al);

                    const Pattern* arg_pat = vp->args[i];
                    if (arg_pat && arg_pat->kind == AstNodeKind::PatBinding) {
                        llvm::Value* gep = builder_.CreateInBoundsGEP(
                            llvm::Type::getInt8Ty(ctx_), payload_base,
                            builder_.getInt64(off));
                        llvm::Value* v =
                            builder_.CreateLoad(llvm_type(payload_ty), gep);

                        auto* b = static_cast<const PatBinding*>(arg_pat);
                        llvm::AllocaInst* slot = create_entry_alloca(
                            f.llvm_fn, llvm_type(payload_ty), b->name);
                        builder_.CreateStore(v, slot);
                        declare_local(
                            f, b->name,
                            LocalSlot{.type = payload_ty, .alloca = slot});
                    } else if (arg_pat &&
                               arg_pat->kind != AstNodeKind::PatWildcard &&
                               arg_pat->kind != AstNodeKind::PatInt &&
                               arg_pat->kind != AstNodeKind::PatBool) {
                        error(arg_pat->span,
                              "unsupported nested binding pattern in enum "
                              "match codegen");
                    }

                    off += *sz;
                }
                return;
            }
            default:
                error(pat->span, "unsupported binding pattern in codegen");
                return;
        }
    }

    std::optional<const ItemFn*> resolve_fn_path(ModuleId mid,
                                                 const Path* path) const {
        if (!path || path->segments.empty()) return std::nullopt;
        if (path->segments.size() == 1) {
            auto it = crate_.modules[mid].values.find(path->segments[0]->text);
            if (it == crate_.modules[mid].values.end()) return std::nullopt;
            if (it->second->kind == AstNodeKind::ItemFn)
                return static_cast<const ItemFn*>(it->second);
            return std::nullopt;
        }
        ModuleId cur = mid;
        for (size_t i = 0; i + 1 < path->segments.size(); i++) {
            auto it =
                crate_.modules[cur].submodules.find(path->segments[i]->text);
            if (it == crate_.modules[cur].submodules.end()) return std::nullopt;
            cur = it->second;
        }
        auto it = crate_.modules[cur].values.find(path->segments.back()->text);
        if (it == crate_.modules[cur].values.end()) return std::nullopt;
        if (it->second->kind == AstNodeKind::ItemFn)
            return static_cast<const ItemFn*>(it->second);
        return std::nullopt;
    }

    const Item* resolve_value_item(ModuleId mid, const Path* path) const {
        if (!path || path->segments.empty()) return nullptr;
        ModuleId cur = mid;
        for (size_t i = 0; i + 1 < path->segments.size(); i++) {
            const Ident* seg = path->segments[i];
            auto it = crate_.modules[cur].submodules.find(seg->text);
            if (it == crate_.modules[cur].submodules.end()) return nullptr;
            cur = it->second;
        }
        const Ident* last = path->segments.back();
        auto it = crate_.modules[cur].values.find(last->text);
        if (it == crate_.modules[cur].values.end()) return nullptr;
        return it->second;
    }

    const Item* resolve_type_item(ModuleId mid, const Path* path) const {
        if (!path || path->segments.empty()) return nullptr;
        ModuleId cur = mid;
        for (size_t i = 0; i + 1 < path->segments.size(); i++) {
            const Ident* seg = path->segments[i];
            auto it = crate_.modules[cur].submodules.find(seg->text);
            if (it == crate_.modules[cur].submodules.end()) return nullptr;
            cur = it->second;
        }
        const Ident* last = path->segments.back();
        auto it = crate_.modules[cur].types.find(last->text);
        if (it == crate_.modules[cur].types.end()) return nullptr;
        return it->second;
    }

    std::optional<std::pair<TypeId, llvm::Value*>> emit_place_ptr(
        FnCtx& f, const Expr* e) {
        if (!e) return std::nullopt;

        switch (e->kind) {
            case AstNodeKind::ExprPath: {
                auto* p = static_cast<const ExprPath*>(e);
                if (!p->path || p->path->segments.empty()) return std::nullopt;

                // Locals (single segment).
                if (p->path->segments.size() == 1) {
                    auto local = lookup_local(f, p->path->segments[0]->text);
                    if (local && local->alloca)
                        return std::pair<TypeId, llvm::Value*>{local->type,
                                                               local->alloca};
                }

                // Statics are addressable.
                if (const Item* item = resolve_value_item(f.mid, p->path)) {
                    if (item->kind == AstNodeKind::ItemStatic) {
                        auto* st = static_cast<const ItemStatic*>(item);
                        auto ty_it = checked_.static_types.find(st);
                        if (ty_it == checked_.static_types.end())
                            return std::nullopt;
                        llvm::GlobalVariable* g =
                            get_or_create_static_global(st);
                        if (!g) return std::nullopt;
                        return std::pair<TypeId, llvm::Value*>{ty_it->second,
                                                               g};
                    }
                }

                return std::nullopt;
            }
            case AstNodeKind::ExprUnary: {
                auto* u = static_cast<const ExprUnary*>(e);
                if (u->op != UnaryOp::Deref) return std::nullopt;
                TypeId ptr_ty_id = type_of(u->expr);
                const TypeData& ptr_td = checked_.types.get(ptr_ty_id);
                if (ptr_td.kind != TypeKind::Ptr) return std::nullopt;
                if (checked_.types.get(ptr_td.pointee).kind == TypeKind::Slice)
                    return std::nullopt;
                CgValue pv = emit_expr(f, u->expr);
                if (!pv.value) return std::nullopt;
                return std::pair<TypeId, llvm::Value*>{ptr_td.pointee,
                                                       pv.value};
            }
            case AstNodeKind::ExprField: {
                auto* fe = static_cast<const ExprField*>(e);
                TypeId base_ty = type_of(fe->base);
                const TypeData& bd = checked_.types.get(base_ty);

                TypeId agg_ty = base_ty;
                llvm::Value* base_ptr = nullptr;

                if (bd.kind == TypeKind::Ptr) {
                    agg_ty = bd.pointee;
                    const TypeData& ad = checked_.types.get(agg_ty);
                    if (ad.kind == TypeKind::Slice) return std::nullopt;
                    if (ad.kind != TypeKind::Struct &&
                        ad.kind != TypeKind::Tuple)
                        return std::nullopt;
                    CgValue bp = emit_expr(f, fe->base);
                    base_ptr = bp.value;
                } else if (bd.kind == TypeKind::Struct ||
                           bd.kind == TypeKind::Tuple) {
                    if (fe->base->kind != AstNodeKind::ExprPath)
                        return std::nullopt;
                    const Path* p =
                        static_cast<const ExprPath*>(fe->base)->path;
                    if (!p || p->segments.size() != 1) return std::nullopt;
                    auto local = lookup_local(f, p->segments[0]->text);
                    if (!local || !local->alloca) return std::nullopt;
                    base_ptr = local->alloca;
                } else {
                    return std::nullopt;
                }

                const TypeData& ad = checked_.types.get(agg_ty);
                if (ad.kind == TypeKind::Struct) {
                    const ItemStruct* sdef = ad.struct_def;
                    auto idx = struct_field_index(sdef, fe->field);
                    if (!idx) return std::nullopt;

                    llvm::StructType* ll_struct =
                        llvm::cast<llvm::StructType>(llvm_type(agg_ty));
                    llvm::Value* field_ptr = builder_.CreateStructGEP(
                        ll_struct, base_ptr, static_cast<unsigned>(*idx));
                    TypeId field_ty = type_of(e);
                    return std::pair<TypeId, llvm::Value*>{field_ty, field_ptr};
                }
                if (ad.kind == TypeKind::Tuple) {
                    auto idx = parse_decimal_index(fe->field);
                    if (!idx || *idx >= ad.tuple_elems.size())
                        return std::nullopt;
                    llvm::StructType* ll_tuple =
                        llvm::cast<llvm::StructType>(llvm_type(agg_ty));
                    llvm::Value* elem_ptr = builder_.CreateStructGEP(
                        ll_tuple, base_ptr, static_cast<unsigned>(*idx));
                    TypeId elem_ty = type_of(e);
                    return std::pair<TypeId, llvm::Value*>{elem_ty, elem_ptr};
                }
                return std::nullopt;
            }
            case AstNodeKind::ExprIndex: {
                auto* ix = static_cast<const ExprIndex*>(e);
                if (!ix || !ix->base) return std::nullopt;

                TypeId base_ty = type_of(ix->base);
                const TypeData& bd = checked_.types.get(base_ty);

                llvm::Value* idx = emit_expr(f, ix->index).value;
                if (!idx) return std::nullopt;

                if (bd.kind == TypeKind::Array) {
                    llvm::Value* base_ptr = nullptr;
                    if (auto base_place = emit_place_ptr(f, ix->base)) {
                        base_ptr = base_place->second;
                    } else {
                        CgValue base_val = emit_expr(f, ix->base);
                        llvm::AllocaInst* tmp = create_entry_alloca(
                            f.llvm_fn, llvm_type(base_ty), "arr.tmp");
                        emit_store(base_ty, tmp, base_val.value);
                        base_ptr = tmp;
                    }

                    llvm::Value* zero =
                        llvm::ConstantInt::get(llvm_int_ty(IntKind::Usize), 0);
                    llvm::Value* gep = builder_.CreateInBoundsGEP(
                        llvm_type(base_ty), base_ptr, {zero, idx});
                    return std::pair<TypeId, llvm::Value*>{bd.elem, gep};
                }

                if (bd.kind == TypeKind::Ptr) {
                    const TypeData& pd = checked_.types.get(bd.pointee);
                    if (pd.kind == TypeKind::Slice) {
                        CgValue base_val = emit_expr(f, ix->base);
                        if (!base_val.value) return std::nullopt;
                        llvm::Value* data_ptr =
                            builder_.CreateExtractValue(base_val.value, {0});
                        llvm::Value* gep = builder_.CreateInBoundsGEP(
                            llvm_type(pd.elem), data_ptr, {idx});
                        return std::pair<TypeId, llvm::Value*>{pd.elem, gep};
                    }
                    CgValue base_ptr_val = emit_expr(f, ix->base);
                    if (!base_ptr_val.value) return std::nullopt;
                    llvm::Value* gep = builder_.CreateInBoundsGEP(
                        llvm_type(bd.pointee), base_ptr_val.value, {idx});
                    return std::pair<TypeId, llvm::Value*>{bd.pointee, gep};
                }

                return std::nullopt;
            }
            default:
                return std::nullopt;
        }
    }

    CgValue emit_cast([[maybe_unused]] FnCtx& f, const CgValue& from,
                      TypeId to_ty) {
        TypeId from_ty = from.type;
        const TypeData& from_d = checked_.types.get(from_ty);
        const TypeData& to_d = checked_.types.get(to_ty);

        auto is_slice_ptr = [&](const TypeData& d) -> bool {
            if (d.kind != TypeKind::Ptr) return false;
            return checked_.types.get(d.pointee).kind == TypeKind::Slice;
        };
        const bool from_is_slice_ptr = is_slice_ptr(from_d);
        const bool to_is_slice_ptr = is_slice_ptr(to_d);

        llvm::Type* dst_ll_ty = llvm_type(to_ty);
        if (from_d.kind == TypeKind::Float && to_d.kind == TypeKind::Float) {
            if (from_d.float_kind == to_d.float_kind)
                return CgValue{.type = to_ty, .value = from.value};
            if (from_d.float_kind == FloatKind::F32 &&
                to_d.float_kind == FloatKind::F64) {
                return CgValue{
                    .type = to_ty,
                    .value = builder_.CreateFPExt(from.value, dst_ll_ty)};
            }
            if (from_d.float_kind == FloatKind::F64 &&
                to_d.float_kind == FloatKind::F32) {
                return CgValue{
                    .type = to_ty,
                    .value = builder_.CreateFPTrunc(from.value, dst_ll_ty)};
            }
            return CgValue{.type = to_ty, .value = from.value};
        }
        if (from_d.kind == TypeKind::Int && to_d.kind == TypeKind::Float) {
            bool signed_from = is_signed_int(from_d.int_kind);
            llvm::Value* out =
                signed_from ? builder_.CreateSIToFP(from.value, dst_ll_ty)
                            : builder_.CreateUIToFP(from.value, dst_ll_ty);
            return CgValue{.type = to_ty, .value = out};
        }
        if (from_d.kind == TypeKind::Float && to_d.kind == TypeKind::Int) {
            bool signed_to = is_signed_int(to_d.int_kind);
            llvm::Value* out =
                signed_to ? builder_.CreateFPToSI(from.value, dst_ll_ty)
                          : builder_.CreateFPToUI(from.value, dst_ll_ty);
            return CgValue{.type = to_ty, .value = out};
        }
        if (from_d.kind == TypeKind::Int && to_d.kind == TypeKind::Int) {
            std::uint32_t from_bits = int_bits(from_d.int_kind, ptr_bits());
            std::uint32_t to_bits = int_bits(to_d.int_kind, ptr_bits());
            if (from_bits == to_bits)
                return CgValue{.type = to_ty, .value = from.value};
            bool signed_from = is_signed_int(from_d.int_kind);
            if (from_bits < to_bits) {
                llvm::Value* out =
                    signed_from ? builder_.CreateSExt(from.value, dst_ll_ty)
                                : builder_.CreateZExt(from.value, dst_ll_ty);
                return CgValue{.type = to_ty, .value = out};
            }
            return CgValue{
                .type = to_ty,
                .value = builder_.CreateTrunc(from.value, dst_ll_ty)};
        }

        if (from_d.kind == TypeKind::Int && to_d.kind == TypeKind::Ptr) {
            if (to_is_slice_ptr) {
                llvm::Value* p = builder_.CreateIntToPtr(from.value, ptr_ty());
                llvm::Value* len =
                    llvm::ConstantInt::get(llvm_int_ty(IntKind::Usize), 0);
                llvm::Value* agg = llvm::UndefValue::get(llvm_type(to_ty));
                agg = builder_.CreateInsertValue(agg, p, {0});
                agg = builder_.CreateInsertValue(agg, len, {1});
                return CgValue{.type = to_ty, .value = agg};
            }
            return CgValue{
                .type = to_ty,
                .value = builder_.CreateIntToPtr(from.value, ptr_ty())};
        }

        if (from_d.kind == TypeKind::Ptr && to_d.kind == TypeKind::Int) {
            if (from_is_slice_ptr) {
                llvm::Value* p = builder_.CreateExtractValue(from.value, {0});
                return CgValue{.type = to_ty,
                               .value = builder_.CreatePtrToInt(p, dst_ll_ty)};
            }
            return CgValue{
                .type = to_ty,
                .value = builder_.CreatePtrToInt(from.value, dst_ll_ty)};
        }

        if (from_d.kind == TypeKind::Ptr && to_d.kind == TypeKind::Ptr) {
            if (from_is_slice_ptr && to_is_slice_ptr) {
                return CgValue{.type = to_ty, .value = from.value};
            }
            if (from_is_slice_ptr && !to_is_slice_ptr) {
                llvm::Value* p = builder_.CreateExtractValue(from.value, {0});
                return CgValue{.type = to_ty,
                               .value = builder_.CreateBitCast(p, ptr_ty())};
            }
            if (!from_is_slice_ptr && to_is_slice_ptr) {
                std::uint64_t len_value = 0;
                const TypeData& fp = checked_.types.get(from_d.pointee);
                if (fp.kind == TypeKind::Array) {
                    if (fp.array_len_value) {
                        len_value = *fp.array_len_value;
                    } else if (fp.array_len_expr) {
                        auto it = checked_.array_lens.find(fp.array_len_expr);
                        if (it != checked_.array_lens.end()) {
                            len_value = it->second;
                        } else {
                            error(Span{},
                                  "array length is not a known comptime "
                                  "constant during array-to-slice coercion");
                        }
                    }
                }
                llvm::Value* len = llvm::ConstantInt::get(
                    llvm_int_ty(IntKind::Usize), len_value);
                llvm::Value* agg = llvm::UndefValue::get(llvm_type(to_ty));
                agg = builder_.CreateInsertValue(agg, from.value, {0});
                agg = builder_.CreateInsertValue(agg, len, {1});
                return CgValue{.type = to_ty, .value = agg};
            }

            // Opaque pointers make this a no-op; keep it explicit for
            // readability in IR dumps.
            return CgValue{
                .type = to_ty,
                .value = builder_.CreateBitCast(from.value, ptr_ty())};
        }

        return CgValue{.type = to_ty, .value = from.value};
    }

    CgValue emit_expr_coerced(FnCtx& f, const Expr* e, TypeId to_ty) {
        CgValue v = emit_expr(f, e);
        if (!checked_.types.equal(v.type, to_ty)) v = emit_cast(f, v, to_ty);
        return v;
    }

    CgValue emit_expr(FnCtx& f, const Expr* e) {
        if (!e) return CgValue{.type = checked_.types.unit(), .value = nullptr};

        TypeId ty = type_of(e);
        const TypeData& td = checked_.types.get(ty);

        switch (e->kind) {
            case AstNodeKind::ExprUnit:
                return CgValue{.type = ty, .value = nullptr};
            case AstNodeKind::ExprBool: {
                auto* b = static_cast<const ExprBool*>(e);
                return CgValue{
                    .type = ty,
                    .value = llvm::ConstantInt::get(llvm::Type::getInt1Ty(ctx_),
                                                    b->value ? 1 : 0)};
            }
            case AstNodeKind::ExprInt: {
                auto* i = static_cast<const ExprInt*>(e);
                llvm::Type* ll_ty = llvm_type(ty);
                return CgValue{.type = ty,
                               .value = llvm::ConstantInt::get(
                                   ll_ty, static_cast<std::uint64_t>(i->value),
                                   /*isSigned=*/true)};
            }
            case AstNodeKind::ExprFloat: {
                auto* fl = static_cast<const ExprFloat*>(e);
                llvm::Type* ll_ty = llvm_type(ty);
                return CgValue{
                    .type = ty,
                    .value = llvm::ConstantFP::get(ll_ty, fl->value)};
            }
            case AstNodeKind::ExprString: {
                auto* s = static_cast<const ExprString*>(e);
                const llvm::StringRef bytes{s->value.data(), s->value.size()};
                if (s->is_c_string) {
                    llvm::Constant* data = llvm::ConstantDataArray::getString(
                        ctx_, bytes, /*AddNull=*/true);
                    auto* g = new llvm::GlobalVariable(
                        *module_, data->getType(),
                        /*isConstant=*/true, llvm::GlobalValue::PrivateLinkage,
                        data, "cstr");
                    g->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
                    g->setAlignment(llvm::Align(1));

                    llvm::Constant* z =
                        llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx_), 0);
                    std::array<llvm::Constant*, 2> idxs{z, z};
                    llvm::Constant* v =
                        llvm::ConstantExpr::getInBoundsGetElementPtr(
                            g->getValueType(), g, idxs);
                    return CgValue{
                        .type = ty,
                        .value = builder_.CreateBitCast(v, ptr_ty())};
                }

                // `"..."` is a `const* [u8]` slice pointer { ptr, len }.
                llvm::Constant* ptr = llvm::ConstantPointerNull::get(ptr_ty());
                if (!bytes.empty()) {
                    llvm::Constant* data = llvm::ConstantDataArray::getString(
                        ctx_, bytes, /*AddNull=*/false);
                    auto* g = new llvm::GlobalVariable(
                        *module_, data->getType(),
                        /*isConstant=*/true, llvm::GlobalValue::PrivateLinkage,
                        data, "str");
                    g->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
                    g->setAlignment(llvm::Align(1));

                    llvm::Constant* z =
                        llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx_), 0);
                    std::array<llvm::Constant*, 2> idxs{z, z};
                    llvm::Constant* v =
                        llvm::ConstantExpr::getInBoundsGetElementPtr(
                            g->getValueType(), g, idxs);
                    ptr = llvm::ConstantExpr::getBitCast(v, ptr_ty());
                }
                llvm::Constant* len = llvm::ConstantInt::get(
                    llvm_int_ty(IntKind::Usize),
                    static_cast<std::uint64_t>(bytes.size()));

                llvm::Value* slice = llvm::UndefValue::get(llvm_type(ty));
                slice = builder_.CreateInsertValue(slice, ptr, {0});
                slice = builder_.CreateInsertValue(slice, len, {1});
                return CgValue{.type = ty, .value = slice};
            }
            case AstNodeKind::ExprStructLit: {
                auto* sl = static_cast<const ExprStructLit*>(e);
                if (td.kind != TypeKind::Struct || !td.struct_def) {
                    error(e->span, "struct literal has non-struct type");
                    return CgValue{
                        .type = ty,
                        .value = llvm::UndefValue::get(llvm_type(ty))};
                }
                auto info_it = checked_.struct_info.find(td.struct_def);
                if (info_it == checked_.struct_info.end())
                    return CgValue{
                        .type = ty,
                        .value = llvm::UndefValue::get(llvm_type(ty))};

                std::unordered_map<std::string_view, const Expr*> init_map{};
                for (const FieldInit* fi : sl->inits) {
                    if (fi) init_map[fi->name] = fi->value;
                }

                llvm::Type* agg_ty = llvm_type(ty);
                llvm::Value* agg = llvm::UndefValue::get(agg_ty);
                for (size_t field_index = 0;
                     field_index < info_it->second.fields_in_order.size();
                     field_index++) {
                    const auto& fld =
                        info_it->second.fields_in_order[field_index];
                    auto init_it = init_map.find(fld.name);
                    if (init_it == init_map.end() || !init_it->second) {
                        error(e->span, "missing field initializer `" +
                                           fld.name + "` in codegen");
                        continue;
                    }
                    CgValue fv =
                        emit_expr_coerced(f, init_it->second, fld.type);
                    agg = builder_.CreateInsertValue(
                        agg, fv.value, {static_cast<unsigned>(field_index)});
                }
                return CgValue{.type = ty, .value = agg};
            }
            case AstNodeKind::ExprTuple: {
                auto* t = static_cast<const ExprTuple*>(e);
                if (td.kind != TypeKind::Tuple) {
                    error(e->span, "tuple expression has non-tuple type");
                    return CgValue{
                        .type = ty,
                        .value = llvm::UndefValue::get(llvm_type(ty))};
                }
                llvm::Type* agg_ty = llvm_type(ty);
                llvm::Value* agg = llvm::UndefValue::get(agg_ty);
                for (size_t i = 0; i < t->elems.size(); i++) {
                    CgValue ev = emit_expr(f, t->elems[i]);
                    agg = builder_.CreateInsertValue(
                        agg, ev.value, {static_cast<unsigned>(i)});
                }
                return CgValue{.type = ty, .value = agg};
            }
            case AstNodeKind::ExprArrayLit: {
                auto* a = static_cast<const ExprArrayLit*>(e);
                if (td.kind != TypeKind::Array) {
                    error(e->span, "array literal has non-array type");
                    return CgValue{
                        .type = ty,
                        .value = llvm::UndefValue::get(llvm_type(ty))};
                }
                llvm::Type* agg_ty = llvm_type(ty);
                llvm::Value* agg = llvm::UndefValue::get(agg_ty);
                for (size_t i = 0; i < a->elems.size(); i++) {
                    CgValue ev = emit_expr_coerced(f, a->elems[i], td.elem);
                    agg = builder_.CreateInsertValue(
                        agg, ev.value, {static_cast<unsigned>(i)});
                }
                return CgValue{.type = ty, .value = agg};
            }
            case AstNodeKind::ExprArrayRepeat: {
                auto* a = static_cast<const ExprArrayRepeat*>(e);
                if (td.kind != TypeKind::Array) {
                    error(e->span, "array repeat has non-array type");
                    return CgValue{
                        .type = ty,
                        .value = llvm::UndefValue::get(llvm_type(ty))};
                }

                // `[x; N]` evaluates `x` once, then repeats the value.
                CgValue elem = emit_expr_coerced(f, a->elem, td.elem);

                std::uint64_t len = 0;
                if (td.array_len_value) {
                    len = *td.array_len_value;
                } else if (td.array_len_expr) {
                    auto it = checked_.array_lens.find(td.array_len_expr);
                    if (it != checked_.array_lens.end()) {
                        len = it->second;
                    } else {
                        error(e->span,
                              "array repeat length is not a known comptime "
                              "constant in codegen");
                    }
                }

                llvm::Type* agg_ty = llvm_type(ty);
                llvm::Value* agg = llvm::UndefValue::get(agg_ty);
                for (std::uint64_t i = 0; i < len; i++) {
                    agg = builder_.CreateInsertValue(
                        agg, elem.value, {static_cast<unsigned>(i)});
                }
                return CgValue{.type = ty, .value = agg};
            }
            case AstNodeKind::ExprPath: {
                auto* p = static_cast<const ExprPath*>(e);
                if (!p->path || p->path->segments.empty())
                    return CgValue{.type = ty,
                                   .value = zero_init(llvm_type(ty))};

                if (p->path->segments.size() == 1) {
                    if (auto local =
                            lookup_local(f, p->path->segments[0]->text)) {
                        llvm::Value* v = emit_load(local->type, local->alloca);
                        return CgValue{.type = local->type, .value = v};
                    }
                    if (auto it =
                            f.comptime_consts.find(p->path->segments[0]->text);
                        it != f.comptime_consts.end()) {
                        return it->second;
                    }
                }

                if (const Item* item = resolve_value_item(f.mid, p->path)) {
                    if (item->kind == AstNodeKind::ItemConst) {
                        auto v = eval_.eval_const(
                            static_cast<const ItemConst*>(item));
                        if (!v)
                            return CgValue{.type = ty,
                                           .value = zero_init(llvm_type(ty))};
                        if (llvm::Constant* c =
                                llvm_const_value(ty, *v, e->span))
                            return CgValue{.type = ty, .value = c};
                        return CgValue{.type = ty,
                                       .value = zero_init(llvm_type(ty))};
                    }
                    if (item->kind == AstNodeKind::ItemStatic) {
                        auto* st = static_cast<const ItemStatic*>(item);
                        llvm::GlobalVariable* g =
                            get_or_create_static_global(st);
                        if (!g)
                            return CgValue{.type = ty,
                                           .value = zero_init(llvm_type(ty))};
                        llvm::Value* v = emit_load(ty, g);
                        return CgValue{.type = ty, .value = v};
                    }
                    if (item->kind == AstNodeKind::ItemFn) {
                        auto* fn = static_cast<const ItemFn*>(item);
                        llvm::Function* callee = llvm_fn(fn);
                        if (!callee)
                            return CgValue{.type = ty,
                                           .value = zero_init(llvm_type(ty))};
                        return CgValue{
                            .type = ty,
                            .value = builder_.CreateBitCast(callee, ptr_ty())};
                    }
                }

                // Enum unit variant value `Enum::Variant`.
                if (td.kind == TypeKind::Enum && td.enum_def &&
                    p->path->segments.size() >= 2) {
                    std::string_view variant_name =
                        p->path->segments.back()->text;
                    if (const VariantInfo* vi =
                            enum_variant_info(td.enum_def, variant_name)) {
                        if (vi->payload.empty()) {
                            std::vector<Expr*> no_args{};
                            return emit_enum_ctor(f, ty, td.enum_def,
                                                  variant_name, no_args);
                        }
                    }
                }
                dump_ast(std::cout, e);
                error(e->span, "unsupported path expression in codegen");
                return CgValue{.type = ty, .value = zero_init(llvm_type(ty))};
            }
            case AstNodeKind::ExprBlock:
                return emit_block(f, static_cast<const ExprBlock*>(e)->block);
            case AstNodeKind::ExprComptime: {
                // `comptime { ... }` always executes at compile time; embed the
                // value as a constant.
                llvm::Type* ll_ty = llvm_type(ty);
                if (ll_ty->isVoidTy()) {
                    (void)eval_.eval_expr(f.mid, e);
                    return CgValue{.type = ty, .value = nullptr};
                }
                if (auto v = eval_.eval_expr(f.mid, e)) {
                    if (llvm::Constant* c = llvm_const_value(ty, *v, e->span))
                        return CgValue{.type = ty, .value = c};
                }
                return CgValue{.type = ty, .value = zero_init(ll_ty)};
            }
            case AstNodeKind::ExprIf: {
                auto* iff = static_cast<const ExprIf*>(e);
                llvm::Value* cond = emit_expr(f, iff->cond).value;

                llvm::Function* fn = builder_.GetInsertBlock()->getParent();
                llvm::BasicBlock* then_bb =
                    llvm::BasicBlock::Create(ctx_, "then", fn);
                llvm::BasicBlock* else_bb =
                    llvm::BasicBlock::Create(ctx_, "else", fn);
                llvm::BasicBlock* end_bb =
                    llvm::BasicBlock::Create(ctx_, "endif", fn);

                llvm::AllocaInst* out_slot = nullptr;
                llvm::Type* out_ty = llvm_type(ty);
                if (!out_ty->isVoidTy())
                    out_slot = create_entry_alloca(fn, out_ty, "if.out");

                builder_.CreateCondBr(cond, then_bb, else_bb);

                builder_.SetInsertPoint(then_bb);
                CgValue then_v = emit_block(f, iff->then_block);
                if (!builder_.GetInsertBlock()->getTerminator()) {
                    if (out_slot) builder_.CreateStore(then_v.value, out_slot);
                    builder_.CreateBr(end_bb);
                }

                builder_.SetInsertPoint(else_bb);
                CgValue else_v{.type = checked_.types.unit(), .value = nullptr};
                if (iff->else_expr) else_v = emit_expr(f, iff->else_expr);
                if (!builder_.GetInsertBlock()->getTerminator()) {
                    if (out_slot) {
                        llvm::Value* stored =
                            else_v.value ? else_v.value : zero_init(out_ty);
                        builder_.CreateStore(stored, out_slot);
                    }
                    builder_.CreateBr(end_bb);
                }

                builder_.SetInsertPoint(end_bb);
                if (!out_slot) return CgValue{.type = ty, .value = nullptr};
                llvm::Value* loaded = builder_.CreateLoad(out_ty, out_slot);
                return CgValue{.type = ty, .value = loaded};
            }
            case AstNodeKind::ExprWhile: {
                auto* wh = static_cast<const ExprWhile*>(e);

                llvm::Function* fn = builder_.GetInsertBlock()->getParent();
                llvm::BasicBlock* cond_bb =
                    llvm::BasicBlock::Create(ctx_, "while.cond", fn);
                llvm::BasicBlock* body_bb =
                    llvm::BasicBlock::Create(ctx_, "while.body", fn);
                llvm::BasicBlock* end_bb =
                    llvm::BasicBlock::Create(ctx_, "while.end", fn);

                builder_.CreateBr(cond_bb);

                builder_.SetInsertPoint(cond_bb);
                llvm::Value* cond = emit_expr(f, wh->cond).value;
                builder_.CreateCondBr(cond, body_bb, end_bb);

                builder_.SetInsertPoint(body_bb);
                f.loops.push_back(FnCtx::LoopTargets{.break_bb = end_bb,
                                                     .continue_bb = cond_bb});
                (void)emit_block(f, wh->body);
                f.loops.pop_back();
                if (!builder_.GetInsertBlock()->getTerminator())
                    builder_.CreateBr(cond_bb);

                builder_.SetInsertPoint(end_bb);
                return CgValue{.type = ty, .value = nullptr};
            }
            case AstNodeKind::ExprLoop: {
                auto* lp = static_cast<const ExprLoop*>(e);

                llvm::Function* fn = builder_.GetInsertBlock()->getParent();
                llvm::BasicBlock* body_bb =
                    llvm::BasicBlock::Create(ctx_, "loop.body", fn);
                llvm::BasicBlock* end_bb =
                    llvm::BasicBlock::Create(ctx_, "loop.end", fn);

                builder_.CreateBr(body_bb);

                builder_.SetInsertPoint(body_bb);
                f.loops.push_back(FnCtx::LoopTargets{.break_bb = end_bb,
                                                     .continue_bb = body_bb});
                (void)emit_block(f, lp->body);
                f.loops.pop_back();
                if (!builder_.GetInsertBlock()->getTerminator())
                    builder_.CreateBr(body_bb);

                builder_.SetInsertPoint(end_bb);
                return CgValue{.type = ty, .value = nullptr};
            }
            case AstNodeKind::ExprMatch: {
                auto* m = static_cast<const ExprMatch*>(e);
                llvm::Function* fn = builder_.GetInsertBlock()->getParent();

                llvm::BasicBlock* end_bb =
                    llvm::BasicBlock::Create(ctx_, "match.end", fn);
                llvm::BasicBlock* nomatch_bb =
                    llvm::BasicBlock::Create(ctx_, "match.nomatch", fn);

                TypeId scrut_ty = type_of(m->scrutinee);
                llvm::Type* scrut_ll_ty = llvm_type(scrut_ty);
                llvm::AllocaInst* scrut_slot =
                    create_entry_alloca(fn, scrut_ll_ty, "match.scrut");
                llvm::Value* scrut_val = emit_expr(f, m->scrutinee).value;
                builder_.CreateStore(scrut_val, scrut_slot);

                llvm::AllocaInst* out_slot = nullptr;
                llvm::Type* out_ty = llvm_type(ty);
                if (!out_ty->isVoidTy())
                    out_slot = create_entry_alloca(fn, out_ty, "match.out");

                llvm::BasicBlock* cond_bb = builder_.GetInsertBlock();

                for (size_t arm_index = 0; arm_index < m->arms.size();
                     arm_index++) {
                    const MatchArm* arm = m->arms[arm_index];
                    if (!arm) continue;

                    llvm::BasicBlock* arm_bb =
                        llvm::BasicBlock::Create(ctx_, "match.arm", fn);
                    llvm::BasicBlock* next_bb =
                        (arm_index + 1 < m->arms.size())
                            ? llvm::BasicBlock::Create(ctx_, "match.next", fn)
                            : nomatch_bb;

                    builder_.SetInsertPoint(cond_bb);
                    llvm::Value* ok =
                        emit_pat_test(f, arm->pat, scrut_ty, scrut_slot);
                    builder_.CreateCondBr(ok, arm_bb, next_bb);

                    builder_.SetInsertPoint(arm_bb);
                    push_scope(f);
                    emit_pat_bindings(f, arm->pat, scrut_ty, scrut_slot);

                    if (arm->guard) {
                        llvm::Value* guard = emit_expr(f, arm->guard).value;
                        llvm::BasicBlock* body_bb =
                            llvm::BasicBlock::Create(ctx_, "match.body", fn);
                        builder_.CreateCondBr(guard, body_bb, next_bb);

                        builder_.SetInsertPoint(body_bb);
                        CgValue rv = emit_expr(f, arm->body);
                        if (!builder_.GetInsertBlock()->getTerminator()) {
                            if (out_slot)
                                builder_.CreateStore(rv.value, out_slot);
                            builder_.CreateBr(end_bb);
                        }
                    } else {
                        CgValue rv = emit_expr(f, arm->body);
                        if (!builder_.GetInsertBlock()->getTerminator()) {
                            if (out_slot)
                                builder_.CreateStore(rv.value, out_slot);
                            builder_.CreateBr(end_bb);
                        }
                    }

                    pop_scope(f);
                    cond_bb = next_bb;
                }

                builder_.SetInsertPoint(nomatch_bb);
                if (!builder_.GetInsertBlock()->getTerminator())
                    builder_.CreateUnreachable();

                builder_.SetInsertPoint(end_bb);
                if (!out_slot) return CgValue{.type = ty, .value = nullptr};
                llvm::Value* loaded = builder_.CreateLoad(out_ty, out_slot);
                return CgValue{.type = ty, .value = loaded};
            }
            case AstNodeKind::ExprField: {
                auto* fe = static_cast<const ExprField*>(e);
                if (auto place = emit_place_ptr(f, e)) {
                    llvm::Value* v = emit_load(place->first, place->second);
                    return CgValue{.type = place->first, .value = v};
                }

                // Rvalue field access (`foo().x` / `(a,b).0`): extract from the
                // aggregate value.
                CgValue base = emit_expr(f, fe->base);
                TypeId base_ty = type_of(fe->base);
                const TypeData& bd = checked_.types.get(base_ty);
                if (bd.kind == TypeKind::Struct && bd.struct_def) {
                    auto idx = struct_field_index(bd.struct_def, fe->field);
                    if (!idx) {
                        error(e->span, "unknown field in codegen");
                        return CgValue{.type = ty,
                                       .value = zero_init(llvm_type(ty))};
                    }
                    llvm::Value* v = builder_.CreateExtractValue(
                        base.value, {static_cast<unsigned>(*idx)});
                    return CgValue{.type = ty, .value = v};
                }
                if (bd.kind == TypeKind::Tuple) {
                    auto idx = parse_decimal_index(fe->field);
                    if (!idx || *idx >= bd.tuple_elems.size()) {
                        error(e->span, "tuple index out of range in codegen");
                        return CgValue{.type = ty,
                                       .value = zero_init(llvm_type(ty))};
                    }
                    llvm::Value* v = builder_.CreateExtractValue(
                        base.value, {static_cast<unsigned>(*idx)});
                    return CgValue{.type = ty, .value = v};
                }

                error(e->span, "unsupported field access in codegen");
                return CgValue{.type = ty, .value = zero_init(llvm_type(ty))};
            }
            case AstNodeKind::ExprIndex: {
                if (auto place = emit_place_ptr(f, e)) {
                    llvm::Value* v = emit_load(place->first, place->second);
                    return CgValue{.type = place->first, .value = v};
                }
                error(e->span, "unsupported indexing in codegen");
                return CgValue{.type = ty, .value = zero_init(llvm_type(ty))};
            }
            case AstNodeKind::ExprAssign: {
                auto* a = static_cast<const ExprAssign*>(e);
                auto place = emit_place_ptr(f, a->lhs);
                if (!place) {
                    error(e->span, "unsupported assignment target in codegen");
                    return CgValue{.type = checked_.types.unit(),
                                   .value = nullptr};
                }
                CgValue rv = emit_expr_coerced(f, a->rhs, place->first);
                emit_store(place->first, place->second, rv.value);
                return CgValue{.type = checked_.types.unit(), .value = nullptr};
            }
            case AstNodeKind::ExprUnary: {
                auto* u = static_cast<const ExprUnary*>(e);
                if (u->op == UnaryOp::AddrOf || u->op == UnaryOp::AddrOfMut) {
                    auto place = emit_place_ptr(f, u->expr);
                    if (!place) {
                        error(e->span,
                              "unsupported address-of operand in codegen");
                        return CgValue{.type = ty,
                                       .value = zero_init(llvm_type(ty))};
                    }
                    return CgValue{.type = ty,
                                   .value = builder_.CreateBitCast(
                                       place->second, ptr_ty())};
                }

                CgValue v = emit_expr(f, u->expr);
                if (!v.value) return v;

                if (u->op == UnaryOp::Neg) {
                    if (checked_.types.get(ty).kind == TypeKind::Float) {
                        return CgValue{.type = ty,
                                       .value = builder_.CreateFNeg(v.value)};
                    }
                    return CgValue{
                        .type = ty,
                        .value = builder_.CreateSub(
                            llvm::ConstantInt::get(llvm_type(ty), 0), v.value)};
                }
                if (u->op == UnaryOp::Not) {
                    return CgValue{
                        .type = ty,
                        .value = builder_.CreateXor(
                            v.value, llvm::ConstantInt::get(
                                         llvm::Type::getInt1Ty(ctx_), 1))};
                }
                if (u->op == UnaryOp::BitNot) {
                    return CgValue{.type = ty,
                                   .value = builder_.CreateNot(v.value)};
                }
                if (u->op == UnaryOp::Deref) {
                    TypeId ptr_ty_id = type_of(u->expr);
                    const TypeData& pd = checked_.types.get(ptr_ty_id);
                    if (pd.kind != TypeKind::Ptr)
                        return CgValue{.type = ty,
                                       .value = zero_init(llvm_type(ty))};
                    if (checked_.types.get(pd.pointee).kind ==
                        TypeKind::Slice) {
                        error(e->span,
                              "deref of a slice pointer is not supported in "
                              "codegen yet");
                        return CgValue{.type = ty,
                                       .value = zero_init(llvm_type(ty))};
                    }
                    llvm::Value* loaded = emit_load(pd.pointee, v.value);
                    return CgValue{.type = pd.pointee, .value = loaded};
                }
                return v;
            }
            case AstNodeKind::ExprBinary: {
                auto* b = static_cast<const ExprBinary*>(e);
                if (!b)
                    return CgValue{.type = ty,
                                   .value = zero_init(llvm_type(ty))};

                if (b->op == BinaryOp::And || b->op == BinaryOp::Or) {
                    // Short-circuit semantics.
                    CgValue lhs = emit_expr(f, b->lhs);

                    llvm::Function* fn = builder_.GetInsertBlock()->getParent();
                    const bool is_and = (b->op == BinaryOp::And);

                    llvm::BasicBlock* lhs_bb = builder_.GetInsertBlock();
                    llvm::BasicBlock* rhs_bb = llvm::BasicBlock::Create(
                        ctx_, is_and ? "and.rhs" : "or.rhs", fn);
                    llvm::BasicBlock* end_bb = llvm::BasicBlock::Create(
                        ctx_, is_and ? "and.end" : "or.end", fn);

                    if (is_and)
                        builder_.CreateCondBr(lhs.value, rhs_bb, end_bb);
                    else
                        builder_.CreateCondBr(lhs.value, end_bb, rhs_bb);

                    builder_.SetInsertPoint(rhs_bb);
                    CgValue rhs = emit_expr(f, b->rhs);
                    llvm::BasicBlock* rhs_end_bb = builder_.GetInsertBlock();
                    const bool rhs_reaches_end =
                        (rhs_end_bb->getTerminator() == nullptr);
                    if (rhs_reaches_end) builder_.CreateBr(end_bb);

                    builder_.SetInsertPoint(end_bb);
                    llvm::PHINode* phi = builder_.CreatePHI(
                        llvm::Type::getInt1Ty(ctx_), rhs_reaches_end ? 2u : 1u,
                        is_and ? "and" : "or");
                    phi->addIncoming(
                        is_and ? builder_.getFalse() : builder_.getTrue(),
                        lhs_bb);
                    if (rhs_reaches_end)
                        phi->addIncoming(rhs.value, rhs_end_bb);
                    return CgValue{.type = ty, .value = phi};
                }

                CgValue lhs = emit_expr(f, b->lhs);
                TypeId lhs_ty = type_of(b->lhs);
                CgValue rhs = emit_expr_coerced(f, b->rhs, lhs_ty);
                const TypeData& lhs_td = checked_.types.get(lhs_ty);

                switch (b->op) {
                    case BinaryOp::Add:
                        if (lhs_td.kind == TypeKind::Float)
                            return CgValue{.type = ty,
                                           .value = builder_.CreateFAdd(
                                               lhs.value, rhs.value)};
                        return CgValue{
                            .type = ty,
                            .value = builder_.CreateAdd(lhs.value, rhs.value)};
                    case BinaryOp::Sub:
                        if (lhs_td.kind == TypeKind::Float)
                            return CgValue{.type = ty,
                                           .value = builder_.CreateFSub(
                                               lhs.value, rhs.value)};
                        return CgValue{
                            .type = ty,
                            .value = builder_.CreateSub(lhs.value, rhs.value)};
                    case BinaryOp::Mul:
                        if (lhs_td.kind == TypeKind::Float)
                            return CgValue{.type = ty,
                                           .value = builder_.CreateFMul(
                                               lhs.value, rhs.value)};
                        return CgValue{
                            .type = ty,
                            .value = builder_.CreateMul(lhs.value, rhs.value)};
                    case BinaryOp::Div:
                        if (lhs_td.kind == TypeKind::Float)
                            return CgValue{.type = ty,
                                           .value = builder_.CreateFDiv(
                                               lhs.value, rhs.value)};
                        if (lhs_td.kind != TypeKind::Int) break;
                        return CgValue{
                            .type = ty,
                            .value =
                                is_signed_int(lhs_td.int_kind)
                                    ? builder_.CreateSDiv(lhs.value, rhs.value)
                                    : builder_.CreateUDiv(lhs.value,
                                                          rhs.value)};
                    case BinaryOp::Mod:
                        if (lhs_td.kind != TypeKind::Int) break;
                        return CgValue{
                            .type = ty,
                            .value =
                                is_signed_int(lhs_td.int_kind)
                                    ? builder_.CreateSRem(lhs.value, rhs.value)
                                    : builder_.CreateURem(lhs.value,
                                                          rhs.value)};
                    case BinaryOp::BitAnd:
                        if (lhs_td.kind != TypeKind::Int) break;
                        return CgValue{
                            .type = ty,
                            .value = builder_.CreateAnd(lhs.value, rhs.value)};
                    case BinaryOp::BitOr:
                        if (lhs_td.kind != TypeKind::Int) break;
                        return CgValue{
                            .type = ty,
                            .value = builder_.CreateOr(lhs.value, rhs.value)};
                    case BinaryOp::BitXor:
                        if (lhs_td.kind != TypeKind::Int) break;
                        return CgValue{
                            .type = ty,
                            .value = builder_.CreateXor(lhs.value, rhs.value)};
                    case BinaryOp::Shl:
                        if (lhs_td.kind != TypeKind::Int) break;
                        return CgValue{
                            .type = ty,
                            .value = builder_.CreateShl(lhs.value, rhs.value)};
                    case BinaryOp::Shr:
                        if (lhs_td.kind != TypeKind::Int) break;
                        return CgValue{
                            .type = ty,
                            .value =
                                is_signed_int(lhs_td.int_kind)
                                    ? builder_.CreateAShr(lhs.value, rhs.value)
                                    : builder_.CreateLShr(lhs.value,
                                                          rhs.value)};
                    case BinaryOp::Eq:
                        if (lhs_td.kind == TypeKind::Float)
                            return CgValue{.type = ty,
                                           .value = builder_.CreateFCmpOEQ(
                                               lhs.value, rhs.value)};
                        return CgValue{.type = ty,
                                       .value = builder_.CreateICmpEQ(
                                           lhs.value, rhs.value)};
                    case BinaryOp::Ne:
                        if (lhs_td.kind == TypeKind::Float)
                            return CgValue{.type = ty,
                                           .value = builder_.CreateFCmpUNE(
                                               lhs.value, rhs.value)};
                        return CgValue{.type = ty,
                                       .value = builder_.CreateICmpNE(
                                           lhs.value, rhs.value)};
                    case BinaryOp::Lt:
                    case BinaryOp::Le:
                    case BinaryOp::Gt:
                    case BinaryOp::Ge: {
                        if (lhs_td.kind == TypeKind::Float) {
                            llvm::CmpInst::Predicate pred =
                                llvm::CmpInst::BAD_FCMP_PREDICATE;
                            if (b->op == BinaryOp::Lt)
                                pred = llvm::CmpInst::FCMP_OLT;
                            if (b->op == BinaryOp::Le)
                                pred = llvm::CmpInst::FCMP_OLE;
                            if (b->op == BinaryOp::Gt)
                                pred = llvm::CmpInst::FCMP_OGT;
                            if (b->op == BinaryOp::Ge)
                                pred = llvm::CmpInst::FCMP_OGE;
                            return CgValue{.type = ty,
                                           .value = builder_.CreateFCmp(
                                               pred, lhs.value, rhs.value)};
                        }
                        if (lhs_td.kind == TypeKind::Int) {
                            bool sign = is_signed_int(lhs_td.int_kind);
                            llvm::CmpInst::Predicate pred =
                                llvm::CmpInst::BAD_ICMP_PREDICATE;
                            if (b->op == BinaryOp::Lt)
                                pred = sign ? llvm::CmpInst::ICMP_SLT
                                            : llvm::CmpInst::ICMP_ULT;
                            if (b->op == BinaryOp::Le)
                                pred = sign ? llvm::CmpInst::ICMP_SLE
                                            : llvm::CmpInst::ICMP_ULE;
                            if (b->op == BinaryOp::Gt)
                                pred = sign ? llvm::CmpInst::ICMP_SGT
                                            : llvm::CmpInst::ICMP_UGT;
                            if (b->op == BinaryOp::Ge)
                                pred = sign ? llvm::CmpInst::ICMP_SGE
                                            : llvm::CmpInst::ICMP_UGE;
                            return CgValue{.type = ty,
                                           .value = builder_.CreateICmp(
                                               pred, lhs.value, rhs.value)};
                        }
                        if (lhs_td.kind == TypeKind::Bool) {
                            llvm::CmpInst::Predicate pred =
                                llvm::CmpInst::BAD_ICMP_PREDICATE;
                            if (b->op == BinaryOp::Lt)
                                pred = llvm::CmpInst::ICMP_ULT;
                            if (b->op == BinaryOp::Le)
                                pred = llvm::CmpInst::ICMP_ULE;
                            if (b->op == BinaryOp::Gt)
                                pred = llvm::CmpInst::ICMP_UGT;
                            if (b->op == BinaryOp::Ge)
                                pred = llvm::CmpInst::ICMP_UGE;
                            return CgValue{.type = ty,
                                           .value = builder_.CreateICmp(
                                               pred, lhs.value, rhs.value)};
                        }
                        break;
                    }
                    default:
                        break;
                }

                error(e->span, "unsupported binary op in codegen");
                return CgValue{.type = ty, .value = zero_init(llvm_type(ty))};
            }
            case AstNodeKind::ExprCast: {
                auto* c = static_cast<const ExprCast*>(e);
                CgValue from = emit_expr(f, c->value);
                return emit_cast(f, from, ty);
            }
            case AstNodeKind::ExprMethodCall: {
                auto* mc = static_cast<const ExprMethodCall*>(e);
                if (!mc->receiver)
                    return CgValue{.type = ty,
                                   .value = zero_init(llvm_type(ty))};

                TypeId recv_ty = type_of(mc->receiver);
                const TypeData& recv_td = checked_.types.get(recv_ty);

                TypeId nominal = recv_ty;
                if (recv_td.kind == TypeKind::Ptr) nominal = recv_td.pointee;
                const TypeData& nominal_td = checked_.types.get(nominal);
                // Inherent dispatch.
                const Item* def = nullptr;
                if (nominal_td.kind == TypeKind::Struct)
                    def = static_cast<const Item*>(nominal_td.struct_def);
                if (nominal_td.kind == TypeKind::Enum)
                    def = static_cast<const Item*>(nominal_td.enum_def);
                if (!def) {
                    error(e->span,
                          "unsupported receiver type in method call codegen");
                    return CgValue{.type = ty,
                                   .value = zero_init(llvm_type(ty))};
                }

                const ItemFn* method = nullptr;
                if (auto it = crate_.inherent_methods.find(def);
                    it != crate_.inherent_methods.end()) {
                    if (auto mi = it->second.find(mc->method);
                        mi != it->second.end())
                        method = mi->second;
                }

                if (!method) {
                    error(e->span, "cannot resolve method in codegen");
                    return CgValue{.type = ty,
                                   .value = zero_init(llvm_type(ty))};
                }

                auto sig_it = checked_.fn_info.find(method);
                if (sig_it == checked_.fn_info.end())
                    return CgValue{.type = ty,
                                   .value = zero_init(llvm_type(ty))};
                const FnInfo& sig = sig_it->second;
                if (sig.params.empty()) {
                    error(e->span, "method has no receiver param");
                    return CgValue{.type = ty,
                                   .value = zero_init(llvm_type(ty))};
                }

                TypeId self_param = sig.params[0];
                const TypeData& sp = checked_.types.get(self_param);

                CgValue recv_arg{.type = checked_.types.error(),
                                 .value = nullptr};
                if (sp.kind == TypeKind::Ptr && recv_td.kind != TypeKind::Ptr) {
                    if (mc->receiver->kind != AstNodeKind::ExprPath) {
                        error(mc->receiver->span,
                              "codegen v0 only supports implicit borrow from "
                              "local paths");
                        return CgValue{.type = ty,
                                       .value = zero_init(llvm_type(ty))};
                    }
                    const Path* p =
                        static_cast<const ExprPath*>(mc->receiver)->path;
                    if (!p || p->segments.size() != 1)
                        return CgValue{.type = ty,
                                       .value = zero_init(llvm_type(ty))};
                    auto local = lookup_local(f, p->segments[0]->text);
                    if (!local || !local->alloca)
                        return CgValue{.type = ty,
                                       .value = zero_init(llvm_type(ty))};
                    recv_arg =
                        CgValue{.type = self_param, .value = local->alloca};
                } else {
                    recv_arg = emit_expr(f, mc->receiver);
                    if (!checked_.types.equal(recv_arg.type, self_param))
                        recv_arg = emit_cast(f, recv_arg, self_param);
                }

                std::vector<llvm::Value*> args{};
                args.reserve(mc->args.size() + 1);
                args.push_back(recv_arg.value);
                if (sig.params.size() != mc->args.size() + 1) {
                    error(e->span, "method call arity mismatch in codegen");
                } else {
                    for (size_t i = 0; i < mc->args.size(); i++) {
                        TypeId expected_ty = sig.params[i + 1];
                        CgValue av =
                            emit_expr_coerced(f, mc->args[i], expected_ty);
                        args.push_back(av.value);
                    }
                }

                llvm::Function* callee = llvm_fn(method);
                if (!callee)
                    return CgValue{.type = ty,
                                   .value = zero_init(llvm_type(ty))};
                llvm::CallInst* call = builder_.CreateCall(
                    callee->getFunctionType(), callee, args);
                if (call->getType()->isVoidTy())
                    return CgValue{.type = checked_.types.unit(),
                                   .value = nullptr};
                return CgValue{.type = sig.ret, .value = call};
            }
            case AstNodeKind::ExprCall: {
                auto* call = static_cast<const ExprCall*>(e);
                const Path* callee_path = nullptr;
                if (call->callee &&
                    call->callee->kind == AstNodeKind::ExprPath) {
                    callee_path =
                        static_cast<const ExprPath*>(call->callee)->path;
                }

                if (callee_path) {
                    if (callee_path->segments.size() == 2 &&
                        callee_path->segments[0]->text == "builtin" &&
                        (callee_path->segments[1]->text == "size_of" ||
                         callee_path->segments[1]->text == "align_of" ||
                         callee_path->segments[1]->text == "type_info")) {
                        if (auto v = eval_.eval_expr(f.mid, e)) {
                            if (llvm::Constant* c =
                                    llvm_const_value(ty, *v, e->span))
                                return CgValue{.type = ty, .value = c};
                        }
                        return CgValue{.type = ty,
                                       .value = zero_init(llvm_type(ty))};
                    }

                    if (auto fn_item = resolve_fn_path(f.mid, callee_path)) {
                        const ItemFn* fn = *fn_item;

                        auto sig_it = checked_.fn_info.find(fn);
                        if (sig_it == checked_.fn_info.end())
                            return CgValue{.type = ty,
                                           .value = zero_init(llvm_type(ty))};
                        const FnInfo& sig = sig_it->second;

                        const bool has_comptime_params =
                            !sig.comptime_params.empty() &&
                            std::any_of(sig.comptime_params.begin(),
                                        sig.comptime_params.end(),
                                        [](bool b) { return b; });
                        if (has_comptime_params && sig.is_variadic) {
                            error(e->span,
                                  "variadic functions with `comptime` params "
                                  "are not supported yet");
                            return CgValue{.type = ty,
                                           .value = zero_init(llvm_type(ty))};
                        }

                        llvm::Function* callee_fn = nullptr;
                        std::vector<llvm::Value*> args{};

                        if (has_comptime_params) {
                            std::vector<ComptimeValue> comptime_args{};
                            comptime_args.reserve(sig.params.size());
                            args.reserve(call->args.size());

                            for (size_t i = 0; i < call->args.size(); i++) {
                                const bool is_ct =
                                    i < sig.comptime_params.size() &&
                                    sig.comptime_params[i];
                                if (is_ct) {
                                    auto v =
                                        eval_.eval_expr(f.mid, call->args[i]);
                                    if (!v) {
                                        error(call->args[i]->span,
                                              "failed to evaluate comptime "
                                              "argument");
                                        return CgValue{
                                            .type = ty,
                                            .value = zero_init(llvm_type(ty))};
                                    }
                                    comptime_args.push_back(*v);
                                } else {
                                    CgValue arg = emit_expr_coerced(
                                        f, call->args[i], sig.params[i]);
                                    args.push_back(arg.value);
                                }
                            }

                            callee_fn = get_or_create_comptime_specialization(
                                fn, sig, comptime_args, e->span);
                            if (!callee_fn)
                                return CgValue{
                                    .type = ty,
                                    .value = zero_init(llvm_type(ty))};
                        } else {
                            callee_fn = llvm_fn(fn);
                            if (!callee_fn)
                                return CgValue{
                                    .type = ty,
                                    .value = zero_init(llvm_type(ty))};

                            args.reserve(call->args.size());
                            for (size_t i = 0; i < call->args.size(); i++) {
                                if (sig.is_variadic && i >= sig.params.size()) {
                                    CgValue arg = emit_expr(f, call->args[i]);
                                    args.push_back(
                                        promote_c_vararg(arg.type, arg.value));
                                } else {
                                    CgValue arg = emit_expr_coerced(
                                        f, call->args[i], sig.params[i]);
                                    args.push_back(arg.value);
                                }
                            }
                        }

                        llvm::CallInst* call_inst = builder_.CreateCall(
                            callee_fn->getFunctionType(), callee_fn, args);
                        if (call_inst->getType()->isVoidTy())
                            return CgValue{.type = checked_.types.unit(),
                                           .value = nullptr};
                        return CgValue{.type = type_of(e), .value = call_inst};
                    }

                    // Tuple-struct constructor `Name(e0, e1, ...)`.
                    if (const Item* ti =
                            resolve_type_item(f.mid, callee_path)) {
                        if (ti->kind == AstNodeKind::ItemStruct &&
                            td.kind == TypeKind::Struct &&
                            td.struct_def == ti) {
                            auto* s = static_cast<const ItemStruct*>(ti);
                            auto si_it = checked_.struct_info.find(s);
                            if (si_it != checked_.struct_info.end()) {
                                const auto& fields =
                                    si_it->second.fields_in_order;
                                bool is_tuple = true;
                                for (size_t i = 0; i < fields.size(); i++) {
                                    if (fields[i].name != std::to_string(i)) {
                                        is_tuple = false;
                                        break;
                                    }
                                }
                                if (is_tuple) {
                                    if (fields.size() != call->args.size()) {
                                        error(e->span,
                                              "tuple struct constructor arity "
                                              "mismatch in codegen");
                                        return CgValue{
                                            .type = ty,
                                            .value = zero_init(llvm_type(ty))};
                                    }
                                    llvm::Value* agg =
                                        llvm::UndefValue::get(llvm_type(ty));
                                    for (size_t i = 0; i < call->args.size();
                                         i++) {
                                        CgValue av = emit_expr_coerced(
                                            f, call->args[i], fields[i].type);
                                        agg = builder_.CreateInsertValue(
                                            agg, av.value,
                                            {static_cast<unsigned>(i)});
                                    }
                                    return CgValue{.type = ty, .value = agg};
                                }
                            }
                        }
                    }

                    // Enum variant constructor `Enum::Variant(...)`.
                    if (td.kind == TypeKind::Enum && td.enum_def &&
                        callee_path->segments.size() >= 2) {
                        std::string_view variant_name =
                            callee_path->segments.back()->text;
                        if (const VariantInfo* vi =
                                enum_variant_info(td.enum_def, variant_name)) {
                            if (vi->payload.size() == call->args.size())
                                return emit_enum_ctor(f, ty, td.enum_def,
                                                      variant_name, call->args);
                        }
                    }
                }

                // Call through a function pointer: `fp(args...)`.
                if (!call->callee) {
                    error(e->span, "missing call target in codegen");
                    return CgValue{.type = ty,
                                   .value = zero_init(llvm_type(ty))};
                }
                TypeId callee_ty = type_of(call->callee);
                const TypeData& cd = checked_.types.get(callee_ty);
                if (cd.kind == TypeKind::Ptr) {
                    const TypeData& pd = checked_.types.get(cd.pointee);
                    if (pd.kind == TypeKind::Fn) {
                        llvm::FunctionType* fty = llvm_fn_type(cd.pointee);
                        if (!fty) {
                            error(e->span,
                                  "internal error: missing function type for "
                                  "call");
                            return CgValue{.type = ty,
                                           .value = zero_init(llvm_type(ty))};
                        }
                        CgValue callee_v = emit_expr(f, call->callee);
                        std::vector<llvm::Value*> args{};
                        args.reserve(call->args.size());
                        if (pd.fn_params.size() != call->args.size()) {
                            error(e->span,
                                  "function pointer call arity mismatch in "
                                  "codegen");
                        } else {
                            for (size_t i = 0; i < call->args.size(); i++) {
                                CgValue av = emit_expr_coerced(f, call->args[i],
                                                               pd.fn_params[i]);
                                args.push_back(av.value);
                            }
                        }
                        llvm::CallInst* call_inst =
                            builder_.CreateCall(fty, callee_v.value, args);
                        if (call_inst->getType()->isVoidTy())
                            return CgValue{.type = checked_.types.unit(),
                                           .value = nullptr};
                        return CgValue{.type = type_of(e), .value = call_inst};
                    }
                }

                error(e->span, "unresolved call in codegen");
                return CgValue{.type = ty, .value = zero_init(llvm_type(ty))};
            }
            default:
                error(e->span, "unsupported expression in codegen");
                return CgValue{.type = ty, .value = zero_init(llvm_type(ty))};
        }
    }

    void emit_stmt(FnCtx& f, const Stmt* s) {
        if (!s) return;
        if (builder_.GetInsertBlock()->getTerminator()) return;

        switch (s->kind) {
            case AstNodeKind::StmtLet: {
                auto* ls = static_cast<const StmtLet*>(s);
                if (!ls->pat || ls->pat->kind != AstNodeKind::PatBinding) {
                    error(
                        s->span,
                        "codegen only supports `let <ident>` patterns for now");
                    return;
                }
                auto* binding = static_cast<const PatBinding*>(ls->pat);
                if (!ls->init) {
                    error(s->span,
                          "codegen requires `let` initializers for now");
                    return;
                }
                TypeId local_ty = type_of(ls->init);
                if (auto it = checked_.binding_types.find(binding);
                    it != checked_.binding_types.end()) {
                    local_ty = it->second;
                }
                llvm::Type* ll_ty = llvm_type(local_ty);
                llvm::AllocaInst* slot =
                    create_entry_alloca(f.llvm_fn, ll_ty, binding->name);
                declare_local(f, binding->name,
                              LocalSlot{.type = local_ty, .alloca = slot});

                CgValue init = emit_expr_coerced(f, ls->init, local_ty);
                emit_store(local_ty, slot, init.value);
                return;
            }
            case AstNodeKind::StmtExpr: {
                (void)emit_expr(f, static_cast<const StmtExpr*>(s)->expr);
                return;
            }
            case AstNodeKind::StmtReturn: {
                auto* r = static_cast<const StmtReturn*>(s);
                TypeId ret_ty = checked_.fn_info.at(f.fn).ret;
                llvm::Type* ll_ret = llvm_type(ret_ty);
                if (ll_ret->isVoidTy()) {
                    builder_.CreateRetVoid();
                    return;
                }
                if (!r->value) {
                    builder_.CreateRet(zero_init(ll_ret));
                    return;
                }
                CgValue v = emit_expr_coerced(f, r->value, ret_ty);
                builder_.CreateRet(v.value);
                return;
            }
            case AstNodeKind::StmtBreak: {
                if (f.loops.empty()) {
                    error(s->span, "`break` outside of a loop (codegen)");
                    builder_.CreateUnreachable();
                    return;
                }
                builder_.CreateBr(f.loops.back().break_bb);
                return;
            }
            case AstNodeKind::StmtContinue: {
                if (f.loops.empty()) {
                    error(s->span, "`continue` outside of a loop (codegen)");
                    builder_.CreateUnreachable();
                    return;
                }
                builder_.CreateBr(f.loops.back().continue_bb);
                return;
            }
            default:
                error(s->span, "unsupported statement in codegen");
                return;
        }
    }

    CgValue emit_block(FnCtx& f, const Block* b) {
        if (!b) return CgValue{.type = checked_.types.unit(), .value = nullptr};
        push_scope(f);
        for (const Stmt* s : b->stmts) emit_stmt(f, s);
        CgValue out{.type = checked_.types.unit(), .value = nullptr};
        if (b->tail && !builder_.GetInsertBlock()->getTerminator())
            out = emit_expr(f, b->tail);
        pop_scope(f);
        return out;
    }

    void emit_function(const ItemFn* fn, ModuleId mid) {
        if (!fn || !fn->decl || !fn->decl->sig || !fn->body) return;
        if (emitted_fns_.contains(fn)) return;
        emitted_fns_.insert(fn);

        llvm::Function* llvm_fn_ptr = llvm_fn(fn);
        if (!llvm_fn_ptr) return;

        auto sig_it = checked_.fn_info.find(fn);
        if (sig_it == checked_.fn_info.end()) return;
        const FnInfo& sig = sig_it->second;

        FnCtx f{};
        f.mid = mid;
        f.fn = fn;
        f.llvm_fn = llvm_fn_ptr;

        llvm::BasicBlock* entry =
            llvm::BasicBlock::Create(ctx_, "entry", llvm_fn_ptr);
        builder_.SetInsertPoint(entry);
        push_scope(f);

        // Parameters are stored into allocas so we can treat them like ordinary
        // locals.
        size_t param_index = 0;
        for (const Param* p : fn->decl->sig->params) {
            if (!p) continue;
            TypeId param_ty = sig.params.at(param_index);
            llvm::Type* ll_ty = llvm_type(param_ty);
            llvm::AllocaInst* slot =
                create_entry_alloca(llvm_fn_ptr, ll_ty, p->name);
            builder_.CreateStore(
                llvm_fn_ptr->getArg(static_cast<unsigned>(param_index)), slot);
            declare_local(f, p->name,
                          LocalSlot{.type = param_ty, .alloca = slot});
            param_index++;
        }

        CgValue body = emit_block(f, fn->body);

        if (!builder_.GetInsertBlock()->getTerminator()) {
            llvm::Type* ll_ret = llvm_type(sig.ret);
            if (ll_ret->isVoidTy()) {
                builder_.CreateRetVoid();
            } else if (body.value) {
                if (!checked_.types.equal(body.type, sig.ret))
                    body = emit_cast(f, body, sig.ret);
                builder_.CreateRet(body.value);
            } else {
                builder_.CreateRet(zero_init(ll_ret));
            }
        }
        pop_scope(f);
    }

    void emit_main_shim() {
        const Module& root = crate_.modules[crate_.root];
        auto it = root.values.find("main");
        if (it == root.values.end() || !it->second) {
            error(Span{},
                  "missing entry point `main` (see `spec/layout_abi.md`)");
            return;
        }
        if (it->second->kind != AstNodeKind::ItemFn) {
            error(it->second->span,
                  "`main` must be a function (see `spec/layout_abi.md`)");
            return;
        }

        auto* main_fn = static_cast<const ItemFn*>(it->second);
        auto sig_it = checked_.fn_info.find(main_fn);
        if (sig_it == checked_.fn_info.end()) {
            error(main_fn->span,
                  "internal error: missing signature for `main`");
            return;
        }
        const FnInfo& sig = sig_it->second;

        bool is_extern = false;
        bool is_export = false;
        for (const Attr* a : main_fn->attrs) {
            if (!a || !a->name) continue;
            if (path_is_ident(a->name, "extern")) is_extern = true;
            if (path_is_ident(a->name, "export")) is_export = true;
        }

        if (is_extern) {
            error(main_fn->span,
                  "executable entry point `main` must not be `extern(C)`");
            return;
        }

        TypeId i32 = checked_.types.int_(IntKind::I32);
        TypeId u8 = checked_.types.int_(IntKind::U8);
        TypeId argv_ty = checked_.types.ptr(
            Mutability::Const, checked_.types.ptr(Mutability::Const, u8));

        auto has_c_main_sig = [&]() -> bool {
            return sig.params.size() == 2 &&
                   checked_.types.equal(sig.params[0], i32) &&
                   checked_.types.equal(sig.params[1], argv_ty) &&
                   checked_.types.equal(sig.ret, i32);
        };

        // 1) Prefer an explicit C ABI entry point:
        //    `fn[export(C)] main(argc: i32, argv: const* const* u8) -> i32 {
        //    ... }`
        if (is_export) {
            if (!has_c_main_sig()) {
                error(main_fn->span,
                      "`fn[export(C)] main` must have signature "
                      "`fn[export(C)] main(argc: i32, argv: const* const* u8) "
                      "-> i32`");
                return;
            }

            if (locator_.symbol_for(main_fn) != "main") {
                error(main_fn->span,
                      "`fn[export(C)] main` must have symbol name `main` "
                      "(remove `export_name(...)` or set it to `\"main\"`)");
                return;
            }
            return;
        }

        // 2) Otherwise, look for a Cog ABI main and synthesize a C ABI shim.
        // Supported Cog ABI mains (v0.1):
        //   a) `fn main() -> ()`
        //   b) `fn main() -> i32`
        //
        // `argc`/`argv` are only available via an explicit exported C ABI
        // entrypoint.
        const bool has_0_params = sig.params.empty();
        if (!has_0_params) {
            if (sig.params.size() == 2 &&
                checked_.types.equal(sig.params[0], i32) &&
                checked_.types.equal(sig.params[1], argv_ty)) {
                error(main_fn->span,
                      "`argc`/`argv` are only available via an explicit C ABI "
                      "entry point; write `fn[export(C)] main(argc: i32, argv: "
                      "const* const* u8) -> i32 { ... }`");
                return;
            }
            error(main_fn->span,
                  "invalid `main` signature (see `spec/layout_abi.md`)");
            return;
        }

        const bool ret_unit =
            checked_.types.equal(sig.ret, checked_.types.unit());
        const bool ret_i32 = checked_.types.equal(sig.ret, i32);
        if (!ret_unit && !ret_i32) {
            error(main_fn->span,
                  "invalid `main` return type (expected `()` or `i32`)");
            return;
        }

        if (module_->getFunction("main")) {
            error(main_fn->span,
                  "multiple definitions of the executable entry point `main`");
            return;
        }

        llvm::FunctionType* wrapper_ty = llvm::FunctionType::get(
            llvm::Type::getInt32Ty(ctx_),
            {llvm::Type::getInt32Ty(ctx_), ptr_ty()}, /*isVarArg=*/false);
        llvm::Function* wrapper = llvm::Function::Create(
            wrapper_ty, llvm::GlobalValue::ExternalLinkage, "main",
            module_.get());

        llvm::BasicBlock* entry =
            llvm::BasicBlock::Create(ctx_, "entry", wrapper);
        llvm::IRBuilder<> b(entry);

        llvm::Function* callee = llvm_fn(main_fn);
        if (!callee) {
            b.CreateRet(
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx_), 1));
            return;
        }

        std::vector<llvm::Value*> args{};

        llvm::CallInst* call =
            b.CreateCall(callee->getFunctionType(), callee, args);
        if (ret_i32) {
            b.CreateRet(call);
        } else {
            b.CreateRet(
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx_), 0));
        }
    }

    bool verify_module() {
        std::string out{};
        llvm::raw_string_ostream os(out);
        if (!llvm::verifyModule(*module_, &os)) return true;
        error(Span{}, "LLVM module verification failed:\n" + os.str());
        return false;
    }

    bool write_ll(const std::filesystem::path& out_ll) {
        std::error_code ec{};
        llvm::raw_fd_ostream out(out_ll.string(), ec, llvm::sys::fs::OF_None);
        if (ec) {
            error(Span{}, "failed to open output file `" + out_ll.string() +
                              "`: " + ec.message());
            return false;
        }
        module_->print(out, nullptr);
        return true;
    }

    bool write_bc(const std::filesystem::path& out_bc) {
        std::error_code ec{};
        llvm::raw_fd_ostream out(out_bc.string(), ec, llvm::sys::fs::OF_None);
        if (ec) {
            error(Span{}, "failed to open output file `" + out_bc.string() +
                              "`: " + ec.message());
            return false;
        }
        llvm::WriteBitcodeToFile(*module_, out);
        return true;
    }

    bool write_obj(const std::filesystem::path& out_obj) {
        std::error_code ec{};
        llvm::raw_fd_ostream out(out_obj.string(), ec, llvm::sys::fs::OF_None);
        if (ec) {
            error(Span{}, "failed to open output file `" + out_obj.string() +
                              "`: " + ec.message());
            return false;
        }

        llvm::legacy::PassManager pm;
        if (target_machine_->addPassesToEmitFile(
                pm, out, nullptr, llvm::CodeGenFileType::ObjectFile)) {
            error(Span{}, "LLVM target does not support object emission");
            return false;
        }
        pm.run(*module_);
        return true;
    }

    bool link_exe(const std::filesystem::path& obj,
                  const std::filesystem::path& out_exe) {
        std::string cmd = "clang ";
        if (!target_.triple.empty()) cmd += "-target " + target_.triple + " ";
        cmd += "\"" + obj.string() + "\" -o \"" + out_exe.string() + "\"";
        int rc = std::system(cmd.c_str());
        if (rc != 0) {
            error(Span{}, "clang failed: " + cmd);
            return false;
        }
        return true;
    }

    bool write_outputs(const EmitLlvmOptions& opts) {
        if (opts.out_ll && !write_ll(*opts.out_ll)) return false;
        if (opts.out_bc && !write_bc(*opts.out_bc)) return false;

        std::optional<std::filesystem::path> obj_path{};
        if (opts.out_obj) obj_path = opts.out_obj;
        if (!obj_path && opts.out_exe)
            obj_path = std::filesystem::path(opts.out_exe->string() + ".o");

        if (obj_path && !write_obj(*obj_path)) return false;
        if (opts.out_exe && obj_path && !link_exe(*obj_path, *opts.out_exe))
            return false;
        return true;
    }
};

}  // namespace

bool emit_llvm(Session& session, const ResolvedCrate& crate,
               CheckedCrate& checked, const EmitLlvmOptions& opts) {
    if (!opts.target) {
        session.diags.push_back(Diagnostic{
            .severity = Severity::Error,
            .span = Span{},
            .message = "internal error: missing target spec for LLVM backend",
        });
        return false;
    }
    LlvmBackend backend(session, crate, checked, *opts.target);
    return backend.run(opts);
}

}  // namespace cog
