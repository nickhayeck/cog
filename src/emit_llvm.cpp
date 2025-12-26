#include "emit_llvm.hpp"

#include <llvm/ADT/StringMap.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
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
#include "layout.hpp"
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
                if (!d.array_len_expr) {
                    error(Span{},
                          "array length is missing during LLVM lowering");
                } else if (auto it = checked_.array_lens.find(d.array_len_expr);
                           it != checked_.array_lens.end()) {
                    len = it->second;
                } else {
                    error(Span{},
                          "array length is not a known comptime constant "
                          "during LLVM lowering");
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

    void declare_function(const ItemFn* fn) {
        if (!fn || !fn->decl || !fn->decl->sig) return;
        if (fn_decls_.contains(fn)) return;

        auto sig_it = checked_.fn_info.find(fn);
        if (sig_it == checked_.fn_info.end()) return;
        llvm::FunctionType* fty = llvm_fn_type(sig_it->second);

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
        if (fn->body) {
            llvm::IRBuilder<>::InsertPointGuard guard(builder_);
            emit_comptime_specialization(fn, specialized, sig, comptime_args,
                                         use_site);
        }
        return specialized;
    }

    void build_function_bodies() {
        for (ModuleId mid = 0; mid < crate_.modules.size(); mid++) {
            for (const Item* item : crate_.modules[mid].items) {
                if (!item) continue;
                if (item->kind == AstNodeKind::ItemFn)
                    emit_function(static_cast<const ItemFn*>(item), mid);
                if (item->kind == AstNodeKind::ItemImplInherent) {
                    auto* impl = static_cast<const ItemImplInherent*>(item);
                    for (const ItemFn* m : impl->methods) emit_function(m, mid);
                }
            }
        }
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
        if (auto v = eval_.eval_static(st)) {
            if (llvm::Constant* c = llvm_const_value(ty, *v, st->span))
                init = c;
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

                    CgValue arg_v = emit_expr(f, args[i]);
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
                if (!p->path || p->path->segments.size() != 1)
                    return std::nullopt;
                auto local = lookup_local(f, p->path->segments[0]->text);
                if (!local || !local->alloca) return std::nullopt;
                return std::pair<TypeId, llvm::Value*>{local->type,
                                                       local->alloca};
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
                llvm::Value* len =
                    llvm::ConstantInt::get(llvm_int_ty(IntKind::Usize), 0);
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
                    CgValue fv = emit_expr(f, init_it->second);
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
            case AstNodeKind::ExprAssign: {
                auto* a = static_cast<const ExprAssign*>(e);
                auto place = emit_place_ptr(f, a->lhs);
                if (!place) {
                    error(e->span, "unsupported assignment target in codegen");
                    return CgValue{.type = checked_.types.unit(),
                                   .value = nullptr};
                }
                CgValue rv = emit_expr(f, a->rhs);
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
                CgValue lhs = emit_expr(f, b->lhs);
                CgValue rhs = emit_expr(f, b->rhs);

                TypeId lhs_ty = type_of(b->lhs);
                const TypeData& lhs_td = checked_.types.get(lhs_ty);

                switch (b->op) {
                    case BinaryOp::Add:
                        return CgValue{
                            .type = ty,
                            .value = builder_.CreateAdd(lhs.value, rhs.value)};
                    case BinaryOp::Sub:
                        return CgValue{
                            .type = ty,
                            .value = builder_.CreateSub(lhs.value, rhs.value)};
                    case BinaryOp::Mul:
                        return CgValue{
                            .type = ty,
                            .value = builder_.CreateMul(lhs.value, rhs.value)};
                    case BinaryOp::Div:
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
                    case BinaryOp::Eq:
                        return CgValue{.type = ty,
                                       .value = builder_.CreateICmpEQ(
                                           lhs.value, rhs.value)};
                    case BinaryOp::Ne:
                        return CgValue{.type = ty,
                                       .value = builder_.CreateICmpNE(
                                           lhs.value, rhs.value)};
                    case BinaryOp::Lt:
                    case BinaryOp::Le:
                    case BinaryOp::Gt:
                    case BinaryOp::Ge: {
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
                    case BinaryOp::And:
                        return CgValue{
                            .type = ty,
                            .value = builder_.CreateAnd(lhs.value, rhs.value)};
                    case BinaryOp::Or:
                        return CgValue{
                            .type = ty,
                            .value = builder_.CreateOr(lhs.value, rhs.value)};
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

                llvm::Value* recv_arg = nullptr;
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
                    recv_arg = local->alloca;
                } else {
                    recv_arg = emit_expr(f, mc->receiver).value;
                }

                std::vector<llvm::Value*> args{};
                args.reserve(mc->args.size() + 1);
                args.push_back(recv_arg);
                for (Expr* arg_expr : mc->args)
                    args.push_back(emit_expr(f, arg_expr).value);

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
                                    CgValue arg = emit_expr(f, call->args[i]);
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
                                CgValue arg = emit_expr(f, call->args[i]);
                                llvm::Value* v = arg.value;
                                if (sig.is_variadic && i >= sig.params.size())
                                    v = promote_c_vararg(arg.type, v);
                                args.push_back(v);
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
                                        CgValue av =
                                            emit_expr(f, call->args[i]);
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
                        for (Expr* arg_expr : call->args)
                            args.push_back(emit_expr(f, arg_expr).value);
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
                llvm::Type* ll_ty = llvm_type(local_ty);
                llvm::AllocaInst* slot =
                    create_entry_alloca(f.llvm_fn, ll_ty, binding->name);
                declare_local(f, binding->name,
                              LocalSlot{.type = local_ty, .alloca = slot});

                CgValue init = emit_expr(f, ls->init);
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
                CgValue v = emit_expr(f, r->value);
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
        TypeId argv_ty =
            checked_.types.ptr(Mutability::Const,
                               checked_.types.ptr(Mutability::Const, u8));

        auto has_c_main_sig = [&]() -> bool {
            return sig.params.size() == 2 &&
                   checked_.types.equal(sig.params[0], i32) &&
                   checked_.types.equal(sig.params[1], argv_ty) &&
                   checked_.types.equal(sig.ret, i32);
        };

        // 1) Prefer an explicit C ABI entry point:
        //    `fn[export(C)] main(argc: i32, argv: const* const* u8) -> i32 { ... }`
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
        //   c) `fn main(i32, const* const* u8) -> ()`
        //   d) `fn main(i32, const* const* u8) -> i32`
        const bool has_0_params = sig.params.empty();
        const bool has_2_params =
            sig.params.size() == 2 && checked_.types.equal(sig.params[0], i32) &&
            checked_.types.equal(sig.params[1], argv_ty);
        if (!has_0_params && !has_2_params) {
            error(main_fn->span,
                  "invalid `main` signature (see `spec/layout_abi.md`)");
            return;
        }

        const bool ret_unit = checked_.types.equal(sig.ret, checked_.types.unit());
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
        if (has_2_params) {
            args.push_back(wrapper->getArg(0));  // argc
            args.push_back(wrapper->getArg(1));  // argv
        }

        llvm::CallInst* call =
            b.CreateCall(callee->getFunctionType(), callee, args);
        if (ret_i32) {
            b.CreateRet(call);
        } else {
            b.CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx_), 0));
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
