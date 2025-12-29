#include "llvm_backend.hpp"

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
#include "hir.hpp"
#include "layout.hpp"
#include "mir.hpp"
#include "mir_interp.hpp"
#include "target.hpp"

namespace cog {
namespace {

// LLVM backend (early).
//
// v0.0.20+: runtime codegen is driven by MIR, lowered to LLVM IR.
//
// The goal is correctness and debuggability first (we rely on LLVM for most
// optimizations).
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

class LlvmBackend {
   public:
    LlvmBackend(Session& session, const HirCrate& hir, const MirProgram& mir,
                const TargetSpec& target, MirInterpreter* mir_eval)
        : session_(session),
          hir_(hir),
          crate_(*hir.crate),
          checked_(*hir.checked),
          mir_(mir),
          target_(target),
          locator_(crate_),
          layout_(session, checked_.types, checked_.struct_info,
                  checked_.enum_info, target.layout),
          module_(std::make_unique<llvm::Module>("cog", ctx_)),
          builder_(ctx_) {
        if (mir_eval) {
            mir_eval_ = mir_eval;
        } else {
            mir_eval_owned_ = std::make_unique<MirInterpreter>(session_, mir_);
            mir_eval_ = mir_eval_owned_.get();
        }
    }

    bool run(const LlvmEmitOptions& opts) {
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
    const HirCrate& hir_;
    const ResolvedCrate& crate_;
    CheckedCrate& checked_;
    const MirProgram& mir_;
    const TargetSpec& target_;
    ItemLocator locator_;

    LayoutEngine layout_;
    MirInterpreter* mir_eval_ = nullptr;
    std::unique_ptr<MirInterpreter> mir_eval_owned_{};

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
        name += "$";
        name += std::to_string(reinterpret_cast<std::uintptr_t>(s));
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
        name += "$";
        name += std::to_string(reinterpret_cast<std::uintptr_t>(e));
        return name;
    }

    TypeId type_of(const Expr* e) const {
        if (!e) return checked_.types.error();
        auto it = checked_.expr_types.find(e);
        if (it == checked_.expr_types.end()) return checked_.types.error();
        return it->second;
    }

    void build_nominal_types() {
        // v0.0.23: nominal types are driven by the checker side tables, not
        // by module item iteration. This includes constructed types created by
        // comptime type construction builtins.
        std::optional<ModuleId> builtin_mid{};
        if (crate_.root < crate_.modules.size()) {
            if (auto it = crate_.modules[crate_.root].submodules.find("builtin");
                it != crate_.modules[crate_.root].submodules.end())
                builtin_mid = it->second;
        }

        auto is_builtin_item = [&](const Item* it) -> bool {
            if (!builtin_mid) return false;
            return locator_.module_of(it) == *builtin_mid;
        };

        for (const auto& [s, _] : checked_.struct_info) {
            if (!s) continue;
            if (is_builtin_item(static_cast<const Item*>(s))) {
                // Builtin descriptor structs are comptime-only; they must not
                // be codegenned.
                continue;
            }
            struct_types_.insert(
                {s,
                 llvm::StructType::create(ctx_, llvm_struct_type_name(s))});
        }

        for (const auto& [e, _] : checked_.enum_info) {
            if (!e) continue;
            if (is_builtin_item(static_cast<const Item*>(e)) &&
                e->name != "TypeKind") {
                // Builtin descriptor enums are comptime-only (except TypeKind,
                // which backs TypeInfo).
                continue;
            }
            enum_types_.insert(
                {e, llvm::StructType::create(ctx_, llvm_enum_type_name(e))});
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
        const FnInfo& sig = sig_it->second;
        if (sig.has_auto && !sig.auto_instantiated) {
            // v0.0.22 MVP: `auto` functions are instantiated on demand.
            return;
        }
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
        if (fn->body) {
            auto dit = hir_.def_ids.find(static_cast<const Item*>(fn));
            if (dit != hir_.def_ids.end()) {
                if (const MirBody* body = mir_.body_for_fn(dit->second)) {
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
        const HirDef* hd = hir_.def(def);
        if (!hd || hd->kind != HirDefKind::Fn || !hd->ast) return nullptr;
        if (hd->ast->kind != AstNodeKind::ItemFn) return nullptr;
        return static_cast<const ItemFn*>(hd->ast);
    }

    const ItemStatic* static_def(DefId def) const {
        const HirDef* hd = hir_.def(def);
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
            if (const MirBody* b = mir_.body_for_const(def)) return b->ret_ty;

            const HirDef* hd = hir_.def(def);
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
                    if (fp.array_len_value)
                        len_value = *fp.array_len_value;
                    else
                        error(Span{},
                              "array length is not a known comptime "
                              "constant during array-to-slice coercion");
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
        (void)f;
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
            const MirBody* b = mir_.body_for_const(def);
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
        (void)bb;
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

        auto dit = hir_.def_ids.find(static_cast<const Item*>(fn));
        if (dit == hir_.def_ids.end()) return;
        const MirBody* body = mir_.body_for_fn(dit->second);
        if (!body) return;

        emitted_fns_.insert(fn);
        llvm::IRBuilder<>::InsertPointGuard guard(builder_);
        mir_emit_body(ll_fn, *body, sig, /*comptime_args=*/nullptr, fn->span);
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
        if (mir_eval_) {
            auto dit = hir_.def_ids.find(static_cast<const Item*>(st));
            if (dit != hir_.def_ids.end())
                if (auto v = mir_eval_->eval_static(dit->second))
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
            case ComptimeValue::Kind::Enum: {
                if (td.kind != TypeKind::Enum || !td.enum_def) {
                    error(use_site, "enum comptime value requires an enum type");
                    return zero_init(ll_ty);
                }
                if (!v.enum_def || v.enum_def != td.enum_def) {
                    error(use_site,
                          "enum comptime value does not match expected enum "
                          "type");
                    return zero_init(ll_ty);
                }
                auto el = layout_.enum_layout(td.enum_def, use_site);
                if (!el) return zero_init(ll_ty);
                auto ei_it = checked_.enum_info.find(td.enum_def);
                if (ei_it == checked_.enum_info.end()) {
                    error(use_site,
                          "internal error: missing enum info during constant "
                          "lowering");
                    return zero_init(ll_ty);
                }
                const EnumInfo& ei = ei_it->second;
                std::optional<std::size_t> idx{};
                for (std::size_t i = 0; i < ei.variants_in_order.size(); i++) {
                    if (ei.variants_in_order[i] == v.enum_variant) {
                        idx = i;
                        break;
                    }
                }
                if (!idx) {
                    error(use_site,
                          "unknown enum variant `" + v.enum_variant +
                              "` in comptime value");
                    return zero_init(ll_ty);
                }
                std::int64_t disc_value = static_cast<std::int64_t>(*idx);
                if (auto di = ei.discriminants.find(v.enum_variant);
                    di != ei.discriminants.end())
                    disc_value = di->second;

                llvm::Type* tag_ty = llvm_tag_int_bytes(el->tag_size);
                llvm::Constant* tag = llvm::ConstantInt::get(
                    tag_ty, static_cast<std::uint64_t>(disc_value),
                    /*isSigned=*/true);

                llvm::StructType* ll_enum =
                    llvm::cast<llvm::StructType>(llvm_type(ty));
                if (el->payload_size == 0 || ll_enum->getNumElements() == 1) {
                    return llvm::ConstantStruct::get(ll_enum, {tag});
                }
                if (!v.enum_payload.empty()) {
                    error(use_site,
                          "payload enums are not supported in LLVM constant "
                          "lowering yet");
                }
                llvm::Constant* payload =
                    zero_init(ll_enum->getElementType(1));
                return llvm::ConstantStruct::get(ll_enum, {tag, payload});
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

    // ---- Legacy AST-based codegen removed (v0.0.20 cleanup) ----

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

    bool write_outputs(const LlvmEmitOptions& opts) {
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

bool llvm_backend_emit(Session& session, const HirCrate& hir,
                       const MirProgram& mir, const TargetSpec& target,
                       const LlvmEmitOptions& opts) {
    LlvmBackend backend(session, hir, mir, target, opts.mir_eval);
    return backend.run(opts);
}

}  // namespace cog
