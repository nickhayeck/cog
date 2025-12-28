#include "check.hpp"

#include <algorithm>
#include <cmath>
#include <functional>
#include <limits>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include "comptime.hpp"
#include "layout.hpp"
#include "sem.hpp"
#include "target.hpp"
#include "types.hpp"

namespace cog {
namespace {

enum class VarState : std::uint8_t { Uninit, Live, Moved };

struct VarInfo {
    TypeId type = 0;
    bool is_mut = false;
    VarState state = VarState::Uninit;
};

struct Env {
    std::vector<std::unordered_map<std::string, VarInfo>> scopes{};
    std::optional<TypeId> fn_ret{};

    Env() { scopes.emplace_back(); }

    void push_scope() { scopes.emplace_back(); }
    void pop_scope() {
        if (scopes.size() > 1) scopes.pop_back();
    }

    VarInfo* lookup(std::string_view name) {
        for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
            auto found = it->find(std::string(name));
            if (found != it->end()) return &found->second;
        }
        return nullptr;
    }

    const VarInfo* lookup(std::string_view name) const {
        for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
            auto found = it->find(std::string(name));
            if (found != it->end()) return &found->second;
        }
        return nullptr;
    }

    void declare(std::string name, VarInfo info) {
        scopes.back().insert({std::move(name), info});
    }
};

static VarState join_state(VarState a, VarState b) {
    if (a == VarState::Live && b == VarState::Live) return VarState::Live;
    if (a == VarState::Uninit || b == VarState::Uninit) return VarState::Uninit;
    return VarState::Moved;
}

static void join_env(Env& into, const Env& other) {
    if (into.scopes.size() != other.scopes.size()) return;
    for (size_t i = 0; i < into.scopes.size(); i++) {
        for (auto& [name, var] : into.scopes[i]) {
            auto it = other.scopes[i].find(name);
            if (it == other.scopes[i].end()) continue;
            var.state = join_state(var.state, it->second.state);
        }
    }
}

static bool is_signed_int_kind(IntKind k) {
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

static std::uint32_t int_bits_for(IntKind k, std::uint32_t ptr_bits) {
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
    return 64;
}

static bool fits_in_int_kind_value(std::int64_t v, IntKind k,
                                   std::uint32_t ptr_bits) {
    std::uint32_t bits = int_bits_for(k, ptr_bits);
    if (bits >= 64) {
        if (!is_signed_int_kind(k)) return v >= 0;
        return true;
    }
    if (is_signed_int_kind(k)) {
        const std::int64_t min = -(std::int64_t(1) << (bits - 1));
        const std::int64_t max = (std::int64_t(1) << (bits - 1)) - 1;
        return v >= min && v <= max;
    }
    if (v < 0) return false;
    const std::uint64_t max = (std::uint64_t(1) << bits) - 1;
    return static_cast<std::uint64_t>(v) <= max;
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

struct ExprResult {
    TypeId type = 0;
    bool diverged = false;
};

struct Place {
    TypeId type = 0;
    bool writable = false;
    std::string root_local{};  // empty means not a local-backed place
};

class Checker {
   public:
    Checker(Session& session, const ResolvedCrate& crate,
            const TargetLayout& target_layout)
        : session_(session), crate_(crate), target_layout_(target_layout) {}

    bool run() {
        typeinfo_struct_ = find_struct_global("TypeInfo");
        if (!typeinfo_struct_)
            error(Span{}, "internal error: missing builtin struct `TypeInfo`");

        predeclare_nominals();
        collect_type_layouts();
        collect_signatures();
        init_comptime_support();
        check_comptime();
        check_bodies();
        return !session_.has_errors();
    }

    CheckedCrate finish() && {
        CheckedCrate out{};
        out.types = std::move(types_);
        out.struct_info = std::move(struct_info_);
        out.enum_info = std::move(enum_info_);
        out.const_types = std::move(const_types_);
        out.static_types = std::move(static_types_);
        out.fn_info = std::move(fn_info_);
        out.binding_types = std::move(binding_types_);
        out.expr_types = std::move(expr_types_);
        out.array_lens = std::move(array_lens_);
        return out;
    }

   private:
    Session& session_;
    const ResolvedCrate& crate_;
    const TargetLayout& target_layout_;
    TypeStore types_;

    std::unordered_map<const ItemStruct*, StructInfo> struct_info_{};
    std::unordered_map<const ItemEnum*, EnumInfo> enum_info_{};
    std::unordered_map<const ItemConst*, TypeId> const_types_{};
    std::unordered_map<const ItemStatic*, TypeId> static_types_{};
    std::unordered_map<const ItemFn*, FnInfo> fn_info_{};
    std::unordered_map<const PatBinding*, TypeId> binding_types_{};
    std::unordered_map<const Expr*, TypeId> expr_types_{};
    std::unordered_map<const Expr*, std::uint64_t> array_lens_{};

    const ItemStruct* typeinfo_struct_ = nullptr;
    std::unique_ptr<LayoutEngine> layout_engine_{};
    std::unique_ptr<ComptimeEvaluator> comptime_eval_{};

    void error(Span span, std::string message) {
        session_.diags.push_back(Diagnostic{.severity = Severity::Error,
                                            .span = span,
                                            .message = std::move(message)});
    }

    const ItemStruct* find_struct_global(std::string_view name) const {
        for (const Module& m : crate_.modules) {
            auto it = m.types.find(std::string(name));
            if (it == m.types.end()) continue;
            if (it->second && it->second->kind == AstNodeKind::ItemStruct)
                return static_cast<const ItemStruct*>(it->second);
        }
        return nullptr;
    }

    TypeId typeinfo_type() {
        if (!typeinfo_struct_) return types_.error();
        return types_.struct_(typeinfo_struct_);
    }

    void init_comptime_support() {
        if (layout_engine_ && comptime_eval_) return;
        layout_engine_ = std::make_unique<LayoutEngine>(
            session_, types_, struct_info_, enum_info_, array_lens_,
            target_layout_);
        comptime_eval_ = std::make_unique<ComptimeEvaluator>(
            session_, crate_, &types_, layout_engine_.get());
    }

    static bool path_is_ident(const Path* p, std::string_view name) {
        if (!p || p->segments.size() != 1 || !p->segments[0]) return false;
        return p->segments[0]->text == name;
    }

    struct TagLookup {
        const Attr* first = nullptr;
        size_t count = 0;
    };

    static TagLookup find_tag(const std::vector<Attr*>& attrs,
                              std::string_view name) {
        TagLookup out{};
        for (const Attr* a : attrs) {
            if (!a || !a->name) continue;
            if (!path_is_ident(a->name, name)) continue;
            out.count++;
            if (!out.first) out.first = a;
        }
        return out;
    }

    void validate_fn_abi_tags(const ItemFn* fn, std::optional<TypeId> self_ty) {
        if (!fn || !fn->decl || !fn->decl->sig) return;

        for (const Param* p : fn->decl->sig->params) {
            if (p && p->is_comptime) {
                const TagLookup ex = find_tag(fn->attrs, "extern");
                const TagLookup exp = find_tag(fn->attrs, "export");
                if (ex.first || exp.first) {
                    error(p->span,
                          "`comptime` parameters are not allowed on "
                          "`extern`/`export` functions in v0.0.x");
                }
                break;
            }
        }

        const TagLookup ex = find_tag(fn->attrs, "extern");
        const TagLookup exp = find_tag(fn->attrs, "export");
        const TagLookup exn = find_tag(fn->attrs, "extern_name");
        const TagLookup expn = find_tag(fn->attrs, "export_name");
        const TagLookup inl = find_tag(fn->attrs, "inline");

        if (ex.count > 1)
            error(ex.first ? ex.first->span : fn->span,
                  "duplicate tag `extern`");
        if (exp.count > 1)
            error(exp.first ? exp.first->span : fn->span,
                  "duplicate tag `export`");
        if (exn.count > 1)
            error(exn.first ? exn.first->span : fn->span,
                  "duplicate tag `extern_name`");
        if (expn.count > 1)
            error(expn.first ? expn.first->span : fn->span,
                  "duplicate tag `export_name`");
        if (inl.count > 1)
            error(inl.first ? inl.first->span : fn->span,
                  "duplicate tag `inline`");

        // Reject unknown fn tags (v0.1 spec: fixed set, may expand later).
        for (const Attr* a : fn->attrs) {
            if (!a || !a->name || a->name->segments.empty() ||
                !a->name->segments[0])
                continue;
            if (a->name->segments.size() != 1) {
                error(a->span, "invalid fn tag (expected a single identifier)");
                continue;
            }
            std::string_view name = a->name->segments[0]->text;
            const bool ok =
                (name == "extern" || name == "extern_name" ||
                 name == "export" || name == "export_name" || name == "inline");
            if (!ok)
                error(a->span, "unknown fn tag `" + std::string(name) + "`");
        }

        const bool is_extern = ex.count > 0;
        const bool is_export = exp.count > 0;
        const bool has_body = fn->body != nullptr;
        const bool is_variadic = fn->decl->sig->is_variadic;

        if (self_ty && (is_extern || is_export || is_variadic)) {
            error(fn->span,
                  "`extern(C)`/`export(C)`/varargs are only supported on free "
                  "functions");
            return;
        }

        if (is_extern && is_export)
            error(fn->span, "a function cannot be both `extern` and `export`");

        if (is_extern && ex.first) {
            if (!ex.first->arg_path) {
                error(ex.first->span,
                      "`extern` requires an ABI argument (e.g. `fn[extern(C)] "
                      "...;`)");
            } else if (!path_is_ident(ex.first->arg_path, "C")) {
                error(
                    ex.first->span,
                    "unsupported extern ABI (only `C` is supported in v0.0.x)");
            }
            if (ex.first->arg_string)
                error(ex.first->span,
                      "`extern(C)` does not accept a string argument");
        }

        if (is_export && exp.first) {
            if (!exp.first->arg_path) {
                error(exp.first->span,
                      "`export` requires an ABI argument (e.g. `fn[export(C)] "
                      "... { ... }`)");
            } else if (!path_is_ident(exp.first->arg_path, "C")) {
                error(
                    exp.first->span,
                    "unsupported export ABI (only `C` is supported in v0.0.x)");
            }
            if (exp.first->arg_string)
                error(exp.first->span,
                      "`export(C)` does not accept a string argument");
        }

        if (exn.count > 0 && !is_extern)
            error(exn.first ? exn.first->span : fn->span,
                  "`extern_name(...)` requires `extern(C)`");
        if (expn.count > 0 && !is_export)
            error(expn.first ? expn.first->span : fn->span,
                  "`export_name(...)` requires `export(C)`");

        if (exn.first) {
            if (!exn.first->arg_path && !exn.first->arg_string) {
                error(exn.first->span, "`extern_name` requires an argument");
            } else if (exn.first->arg_path &&
                       (exn.first->arg_path->segments.size() != 1 ||
                        !exn.first->arg_path->segments[0])) {
                error(exn.first->span,
                      "`extern_name` requires a single identifier or a string "
                      "literal");
            }
        }
        if (expn.first) {
            if (!expn.first->arg_path && !expn.first->arg_string) {
                error(expn.first->span, "`export_name` requires an argument");
            } else if (expn.first->arg_path &&
                       (expn.first->arg_path->segments.size() != 1 ||
                        !expn.first->arg_path->segments[0])) {
                error(expn.first->span,
                      "`export_name` requires a single identifier or a string "
                      "literal");
            }
        }

        // Conflict rules: extern/export groups cannot be combined with other
        // tags.
        if (is_extern) {
            for (const Attr* a : fn->attrs) {
                if (!a || !a->name) continue;
                if (path_is_ident(a->name, "extern") ||
                    path_is_ident(a->name, "extern_name"))
                    continue;
                error(
                    a->span,
                    "`extern(C)` functions may not have other tags in v0.0.x");
            }
        }
        if (is_export) {
            for (const Attr* a : fn->attrs) {
                if (!a || !a->name) continue;
                if (path_is_ident(a->name, "export") ||
                    path_is_ident(a->name, "export_name"))
                    continue;
                error(
                    a->span,
                    "`export(C)` functions may not have other tags in v0.0.x");
            }
        }

        if (!has_body && !is_extern)
            error(fn->span,
                  "function declarations without a body must be marked "
                  "`fn[extern(C)]`");
        if (has_body && is_extern)
            error(fn->span,
                  "`fn[extern(C)]` declarations must not have a body");
        if (!has_body && is_export)
            error(fn->span, "`fn[export(C)]` requires a body");

        if (is_variadic) {
            if (!is_extern)
                error(fn->span,
                      "variadics (`...`) are only supported for "
                      "`fn[extern(C)]` declarations");
            if (has_body)
                error(fn->span,
                      "variadic functions must not have a body (extern-only)");
            if (is_export)
                error(fn->span, "`fn[export(C)]` cannot be variadic in v0.0.x");
            if (fn->decl->sig->params.empty())
                error(fn->span,
                      "variadic extern functions must have at least one fixed "
                      "parameter");
        }
    }

    void predeclare_nominals() {
        for (const Module& m : crate_.modules) {
            for (Item* item : m.items) {
                if (!item) continue;
                if (item->kind == AstNodeKind::ItemStruct) {
                    (void)types_.struct_(static_cast<const ItemStruct*>(item));
                } else if (item->kind == AstNodeKind::ItemEnum) {
                    (void)types_.enum_(static_cast<const ItemEnum*>(item));
                }
            }
        }
    }

    void collect_type_layouts() {
        for (ModuleId mid = 0; mid < crate_.modules.size(); mid++) {
            const Module& m = crate_.modules[mid];
            for (Item* item : m.items) {
                if (!item) continue;
                if (item->kind == AstNodeKind::ItemStruct) {
                    collect_struct_info(mid,
                                        static_cast<const ItemStruct*>(item));
                } else if (item->kind == AstNodeKind::ItemEnum) {
                    collect_enum_info(mid, static_cast<const ItemEnum*>(item));
                }
            }
        }
    }

    void collect_struct_info(ModuleId mid, const ItemStruct* s) {
        if (struct_info_.contains(s)) return;
        StructInfo info{};
        for (const FieldDecl* f : s->fields) {
            if (!f) continue;
            if (info.fields.contains(f->name)) {
                error(f->span, "duplicate field `" + f->name + "` in struct `" +
                                   s->name + "`");
                continue;
            }
            TypeId ty = lower_type(mid, f->type, /*allow_unsized=*/false,
                                   std::nullopt, /*allow_self=*/false);
            info.fields_in_order.push_back(
                StructInfo::Field{.name = f->name, .type = ty});
            info.fields.insert({f->name, ty});
        }
        struct_info_.insert({s, std::move(info)});
    }

    void collect_enum_info(ModuleId mid, const ItemEnum* e) {
        if (enum_info_.contains(e)) return;
        EnumInfo info{};
        for (const VariantDecl* v : e->variants) {
            if (!v) continue;
            if (info.variants.contains(v->name)) {
                error(v->span, "duplicate variant `" + v->name + "` in enum `" +
                                   e->name + "`");
                continue;
            }
            info.variants_in_order.push_back(v->name);
            VariantInfo vi{};
            vi.ast = v;
            for (const Type* pt : v->payload) {
                TypeId ty = lower_type(mid, pt, /*allow_unsized=*/false,
                                       std::nullopt, /*allow_self=*/false);
                vi.payload.push_back(ty);
            }
            info.variants.insert({v->name, std::move(vi)});
        }

        // v0.0.13: `enum[tag(<int>)]` for fieldless (C-style) enums.
        const Attr* tag_attr = nullptr;
        for (const Attr* a : e->attrs) {
            if (!a || !a->name) continue;

            // Legacy: `enum[repr(i32)]` was used in earlier prototypes; the
            // v0.1 spec uses `tag(i32)`.
            if (path_is_ident(a->name, "repr") && a->arg_path &&
                a->arg_path->segments.size() == 1 && a->arg_path->segments[0] &&
                types_.parse_int_kind(a->arg_path->segments[0]->text)) {
                error(a->span,
                      "`enum[repr(<int>)]` is removed; use `enum[tag(<int>)]`");
            }

            if (!path_is_ident(a->name, "tag")) continue;
            if (tag_attr) {
                error(a->span, "duplicate tag `tag`");
                continue;
            }
            tag_attr = a;
        }

        if (tag_attr) {
            if (tag_attr->arg_string)
                error(tag_attr->span,
                      "`tag` expects an integer type argument, not a string");
            if (!tag_attr->arg_path ||
                tag_attr->arg_path->segments.size() != 1 ||
                !tag_attr->arg_path->segments[0]) {
                error(tag_attr->span,
                      "`tag` requires an integer type argument (e.g. "
                      "`enum[tag(i32)] ...`)");
            } else {
                auto ik = types_.parse_int_kind(
                    tag_attr->arg_path->segments[0]->text);
                if (!ik) {
                    error(tag_attr->span,
                          "invalid `tag` type (expected an integer type)");
                } else {
                    info.tag_int = *ik;
                }
            }

            // `tag(<int>)` is only allowed on fieldless enums.
            for (const VariantDecl* v : e->variants) {
                if (!v) continue;
                if (!v->payload.empty()) {
                    error(v->span,
                          "`enum[tag(<int>)]` requires a fieldless enum (no "
                          "payload variants)");
                    info.tag_int = std::nullopt;
                    break;
                }
            }
        }

        enum_info_.insert({e, std::move(info)});
    }

    void collect_signatures() {
        for (ModuleId mid = 0; mid < crate_.modules.size(); mid++) {
            const Module& m = crate_.modules[mid];
            for (Item* item : m.items) {
                if (!item) continue;
                switch (item->kind) {
                    case AstNodeKind::ItemConst: {
                        auto* c = static_cast<const ItemConst*>(item);
                        TypeId ty =
                            lower_type(mid, c->type, /*allow_unsized=*/false,
                                       std::nullopt, /*allow_self=*/false);
                        const_types_.insert({c, ty});
                        break;
                    }
                    case AstNodeKind::ItemStatic: {
                        auto* s = static_cast<const ItemStatic*>(item);
                        TypeId ty =
                            lower_type(mid, s->type, /*allow_unsized=*/false,
                                       std::nullopt, /*allow_self=*/false);
                        static_types_.insert({s, ty});
                        break;
                    }
                    case AstNodeKind::ItemFn: {
                        collect_fn_sig(mid, static_cast<const ItemFn*>(item),
                                       std::nullopt, /*allow_self=*/false);
                        break;
                    }
                    case AstNodeKind::ItemImplInherent: {
                        auto* impl = static_cast<const ItemImplInherent*>(item);
                        TypeId self_ty = lower_type_path(
                            mid, impl->type_name, /*allow_unsized=*/false,
                            std::nullopt, false);
                        for (const ItemFn* mfn : impl->methods)
                            collect_fn_sig(mid, mfn, self_ty,
                                           /*allow_self=*/true);
                        break;
                    }
                    default:
                        break;
                }
            }
        }
    }

    void collect_fn_sig(ModuleId mid, const ItemFn* fn,
                        std::optional<TypeId> self_ty, bool allow_self) {
        if (!fn || !fn->decl || !fn->decl->sig) return;
        validate_fn_abi_tags(fn, self_ty);
        fn_info_.insert(
            {fn, lower_fn_sig(mid, fn->decl->sig, self_ty, allow_self)});
    }

    FnInfo lower_fn_sig(ModuleId mid, const FnSig* sig,
                        std::optional<TypeId> self_ty, bool allow_self) {
        FnInfo out{};
        out.is_variadic = sig->is_variadic;
        out.ret = sig->ret ? lower_type(mid, sig->ret, /*allow_unsized=*/false,
                                        self_ty, allow_self)
                           : types_.unit();
        for (const Param* p : sig->params) {
            if (!p) continue;
            TypeId pt = lower_type(mid, p->type, /*allow_unsized=*/false,
                                   self_ty, allow_self);
            out.params.push_back(pt);
            out.comptime_params.push_back(p->is_comptime);
        }
        return out;
    }

    TypeId lower_type(ModuleId mid, const Type* ty, bool allow_unsized,
                      std::optional<TypeId> self_ty, bool allow_self) {
        if (!ty) return types_.error();
        switch (ty->kind) {
            case AstNodeKind::TypeUnit:
                return types_.unit();
            case AstNodeKind::TypeType:
                return types_.type_type();
            case AstNodeKind::TypeNever:
                return types_.never();
            case AstNodeKind::TypePath:
                return lower_type_path(mid,
                                       static_cast<const TypePath*>(ty)->path,
                                       allow_unsized, self_ty, allow_self);
            case AstNodeKind::TypePtr: {
                auto* p = static_cast<const TypePtr*>(ty);
                TypeId pointee =
                    lower_type(mid, p->pointee, /*allow_unsized=*/true, self_ty,
                               allow_self);
                return types_.ptr(p->mutability, pointee);
            }
            case AstNodeKind::TypeSlice: {
                if (!allow_unsized)
                    error(ty->span, "slice types must appear behind a pointer");
                TypeId elem =
                    lower_type(mid, static_cast<const TypeSlice*>(ty)->elem,
                               /*allow_unsized=*/false, self_ty, allow_self);
                return types_.slice(elem);
            }
            case AstNodeKind::TypeArray: {
                auto* a = static_cast<const TypeArray*>(ty);
                TypeId elem = lower_type(mid, a->elem, /*allow_unsized=*/false,
                                         self_ty, allow_self);
                return types_.array(elem, a->len);
            }
            case AstNodeKind::TypeFn: {
                if (!allow_unsized)
                    error(ty->span,
                          "function types must appear behind a pointer");
                auto* ft = static_cast<const TypeFn*>(ty);
                std::vector<TypeId> params{};
                params.reserve(ft->params.size());
                for (const Type* p : ft->params)
                    params.push_back(lower_type(mid, p, /*allow_unsized=*/false,
                                                self_ty, allow_self));
                TypeId ret =
                    ft->ret ? lower_type(mid, ft->ret, /*allow_unsized=*/false,
                                         self_ty, allow_self)
                            : types_.unit();
                return types_.fn(std::move(params), ret);
            }
            case AstNodeKind::TypeTuple: {
                auto* t = static_cast<const TypeTuple*>(ty);
                std::vector<TypeId> elems;
                for (const Type* e : t->elems)
                    elems.push_back(lower_type(mid, e, /*allow_unsized=*/false,
                                               self_ty, allow_self));
                return types_.tuple(std::move(elems));
            }
            default:
                break;
        }
        return types_.error();
    }

    const Item* resolve_type_item(ModuleId mid, const Path* path,
                                  bool emit_errors) {
        if (!path || path->segments.empty()) return nullptr;
        ModuleId cur = mid;
        for (size_t i = 0; i + 1 < path->segments.size(); i++) {
            const Ident* seg = path->segments[i];
            auto it = crate_.modules[cur].submodules.find(seg->text);
            if (it == crate_.modules[cur].submodules.end()) {
                if (emit_errors)
                    error(seg->span, "cannot find module `" + seg->text + "`");
                return nullptr;
            }
            cur = it->second;
        }
        const Ident* last = path->segments.back();
        auto it = crate_.modules[cur].types.find(last->text);
        if (it == crate_.modules[cur].types.end()) {
            if (emit_errors)
                error(last->span, "cannot find type `" + last->text + "`");
            return nullptr;
        }
        return it->second;
    }

    TypeId lower_type_path(ModuleId mid, const Path* path, bool allow_unsized,
                           std::optional<TypeId> self_ty, bool allow_self) {
        if (!path || path->segments.empty()) return types_.error();

        if (path->segments.size() == 1) {
            std::string_view name = path->segments[0]->text;
            if (name == "Self") {
                if (self_ty) return *self_ty;
                if (!allow_self) {
                    error(path->span, "`Self` is not allowed here");
                    return types_.error();
                }
                return types_.self();
            }
            if (name == "bool") return types_.bool_();
            if (auto ik = types_.parse_int_kind(name)) return types_.int_(*ik);
            if (auto fk = types_.parse_float_kind(name))
                return types_.float_(*fk);
        }

        const Item* item = resolve_type_item(mid, path, /*emit_errors=*/true);
        if (!item) return types_.error();

        switch (item->kind) {
            case AstNodeKind::ItemStruct:
                return types_.struct_(static_cast<const ItemStruct*>(item));
            case AstNodeKind::ItemEnum:
                return types_.enum_(static_cast<const ItemEnum*>(item));
            case AstNodeKind::ItemTypeAlias: {
                auto* ta = static_cast<const ItemTypeAlias*>(item);
                return lower_type(mid, ta->aliased, allow_unsized, self_ty,
                                  allow_self);
            }
            default:
                break;
        }
        return types_.error();
    }

    std::optional<TypeId> lower_type_value_expr(ModuleId mid, const Expr* e) {
        if (!e) return std::nullopt;
        if (e->kind != AstNodeKind::ExprPath) {
            error(e->span, "expected a type value (path)");
            return std::nullopt;
        }
        const Path* p = static_cast<const ExprPath*>(e)->path;
        return lower_type_path(mid, p, /*allow_unsized=*/false, std::nullopt,
                               /*allow_self=*/false);
    }

    void check_bodies() {
        for (ModuleId mid = 0; mid < crate_.modules.size(); mid++) {
            const Module& m = crate_.modules[mid];
            for (Item* item : m.items) {
                if (!item) continue;
                switch (item->kind) {
                    case AstNodeKind::ItemFn:
                        check_fn_body(mid, static_cast<const ItemFn*>(item),
                                      std::nullopt);
                        break;
                    case AstNodeKind::ItemImplInherent: {
                        auto* impl = static_cast<const ItemImplInherent*>(item);
                        TypeId self_ty = lower_type_path(
                            mid, impl->type_name, false, std::nullopt, false);
                        for (const ItemFn* mfn : impl->methods)
                            check_fn_body(mid, mfn, self_ty);
                        break;
                    }
                    default:
                        break;
                }
            }
        }
    }

    void check_comptime() {
        if (session_.has_errors()) return;
        if (!comptime_eval_) return;
        ComptimeEvaluator& eval = *comptime_eval_;

        // Evaluate array lengths that appear in item signatures and layouts.
        for (ModuleId mid = 0; mid < crate_.modules.size(); mid++) {
            const Module& m = crate_.modules[mid];
            for (Item* item : m.items) {
                if (!item) continue;
                switch (item->kind) {
                    case AstNodeKind::ItemStruct: {
                        auto* s = static_cast<const ItemStruct*>(item);
                        for (const FieldDecl* f : s->fields) {
                            if (f) check_type_arrays(mid, f->type, eval);
                        }
                        break;
                    }
                    case AstNodeKind::ItemEnum: {
                        auto* e = static_cast<const ItemEnum*>(item);
                        for (const VariantDecl* v : e->variants) {
                            if (!v) continue;
                            for (const Type* t : v->payload)
                                check_type_arrays(mid, t, eval);
                        }
                        break;
                    }
                    case AstNodeKind::ItemFn: {
                        auto* fn = static_cast<const ItemFn*>(item);
                        if (fn->decl && fn->decl->sig) {
                            for (const Param* p : fn->decl->sig->params) {
                                if (p) check_type_arrays(mid, p->type, eval);
                            }
                            if (fn->decl->sig->ret)
                                check_type_arrays(mid, fn->decl->sig->ret,
                                                  eval);
                        }
                        break;
                    }
                    case AstNodeKind::ItemImplInherent: {
                        auto* impl = static_cast<const ItemImplInherent*>(item);
                        for (const ItemFn* mfn : impl->methods) {
                            if (!mfn || !mfn->decl || !mfn->decl->sig) continue;
                            for (const Param* p : mfn->decl->sig->params) {
                                if (p) check_type_arrays(mid, p->type, eval);
                            }
                            if (mfn->decl->sig->ret)
                                check_type_arrays(mid, mfn->decl->sig->ret,
                                                  eval);
                        }
                        break;
                    }
                    case AstNodeKind::ItemConst: {
                        auto* c = static_cast<const ItemConst*>(item);
                        check_type_arrays(mid, c->type, eval);
                        break;
                    }
                    case AstNodeKind::ItemStatic: {
                        auto* s = static_cast<const ItemStatic*>(item);
                        check_type_arrays(mid, s->type, eval);
                        break;
                    }
                    case AstNodeKind::ItemTypeAlias: {
                        auto* ta = static_cast<const ItemTypeAlias*>(item);
                        check_type_arrays(mid, ta->aliased, eval);
                        break;
                    }
                    default:
                        break;
                }
            }
        }
        if (session_.has_errors()) return;

        // Evaluate enum discriminants (fieldless only, v0.0.x).
        auto fits_in_int_kind = [&](std::int64_t v, IntKind k) -> bool {
            return fits_in_int_kind_value(v, k, target_layout_.pointer_bits);
        };

        for (ModuleId mid = 0; mid < crate_.modules.size(); mid++) {
            for (Item* item : crate_.modules[mid].items) {
                if (!item || item->kind != AstNodeKind::ItemEnum) continue;
                auto* e = static_cast<const ItemEnum*>(item);
                auto info_it = enum_info_.find(e);
                if (info_it == enum_info_.end()) continue;

                EnumInfo& info = info_it->second;
                info.discriminants.clear();

                bool fieldless = true;
                for (const VariantDecl* v : e->variants) {
                    if (!v) continue;
                    if (!v->payload.empty()) fieldless = false;
                }

                if (!fieldless) {
                    for (const VariantDecl* v : e->variants) {
                        if (!v || !v->discriminant) continue;
                        error(v->span,
                              "enum discriminants are only supported on "
                              "fieldless enums in v0.0.x");
                    }
                    continue;
                }

                // Default tag width (implementation detail): match the layout
                // engine's current choice.
                const size_t n = info.variants_in_order.size();
                std::uint64_t tag_bytes = 4;
                if (n <= 0x100)
                    tag_bytes = 1;
                else if (n <= 0x10000)
                    tag_bytes = 2;
                else if (n <= 0x1'0000'0000ULL)
                    tag_bytes = 4;
                else
                    tag_bytes = 8;
                const std::uint64_t tag_bits = tag_bytes * 8;

                auto fits_default_tag = [&](std::int64_t v) -> bool {
                    if (v < 0) return false;
                    if (tag_bits >= 64) return true;
                    const std::uint64_t max =
                        (std::uint64_t(1) << tag_bits) - 1;
                    return static_cast<std::uint64_t>(v) <= max;
                };

                std::unordered_map<std::int64_t, std::string> seen{};
                std::int64_t next = 0;

                for (const VariantDecl* v : e->variants) {
                    if (!v) continue;

                    if (v->discriminant) {
                        TypeId expected = info.tag_int
                                              ? types_.int_(*info.tag_int)
                                              : types_.int_(IntKind::I64);
                        Env env{};
                        (void)check_expr(mid, v->discriminant, env, expected);
                        if (!session_.has_errors()) {
                            auto cv = eval.eval_expr(mid, v->discriminant);
                            if (!cv || cv->kind != ComptimeValue::Kind::Int) {
                                error(v->discriminant->span,
                                      "enum discriminant must be an integer "
                                      "comptime expression");
                            } else {
                                next = cv->int_value;
                            }
                        }
                    }

                    if (info.tag_int) {
                        if (!fits_in_int_kind(next, *info.tag_int)) {
                            error(v->span,
                                  "enum discriminant does not fit in `tag(" +
                                      types_.to_string(
                                          types_.int_(*info.tag_int)) +
                                      ")`");
                        }
                    } else {
                        if (!fits_default_tag(next)) {
                            error(
                                v->span,
                                "enum discriminant does not fit in the default "
                                "tag size for this enum; add `enum[tag(...)]`");
                        }
                    }

                    if (auto it = seen.find(next); it != seen.end()) {
                        error(v->span,
                              "duplicate enum discriminant value (also used by "
                              "`" +
                                  it->second + "`)");
                    } else {
                        seen.insert({next, v->name});
                    }

                    info.discriminants.insert({v->name, next});

                    if (next == std::numeric_limits<std::int64_t>::max()) {
                        error(v->span, "enum discriminant overflow");
                        break;
                    }
                    next++;
                }
            }
        }
        if (session_.has_errors()) return;

        // Type-check and evaluate const/static initializers.
        for (ModuleId mid = 0; mid < crate_.modules.size(); mid++) {
            const Module& m = crate_.modules[mid];
            for (Item* item : m.items) {
                if (!item) continue;
                switch (item->kind) {
                    case AstNodeKind::ItemConst: {
                        auto* c = static_cast<const ItemConst*>(item);
                        TypeId expected = const_types_.contains(c)
                                              ? const_types_.at(c)
                                              : types_.error();
                        Env env{};
                        TypeId got =
                            check_expr(mid, c->value, env, expected).type;
                        if (!types_.can_coerce(got, expected)) {
                            error(
                                c->span,
                                "const initializer type mismatch: expected `" +
                                    types_.to_string(expected) + "`, got `" +
                                    types_.to_string(got) + "`");
                        }
                        if (!session_.has_errors()) (void)eval.eval_const(c);
                        break;
                    }
                    case AstNodeKind::ItemStatic: {
                        auto* s = static_cast<const ItemStatic*>(item);
                        TypeId expected = static_types_.contains(s)
                                              ? static_types_.at(s)
                                              : types_.error();
                        Env env{};
                        TypeId got =
                            check_expr(mid, s->value, env, expected).type;
                        if (!types_.can_coerce(got, expected)) {
                            error(
                                s->span,
                                "static initializer type mismatch: expected `" +
                                    types_.to_string(expected) + "`, got `" +
                                    types_.to_string(got) + "`");
                        }
                        if (!session_.has_errors()) (void)eval.eval_static(s);
                        break;
                    }
                    default:
                        break;
                }
            }
        }
    }

    void check_type_arrays(ModuleId mid, const Type* ty,
                           ComptimeEvaluator& eval) {
        if (!ty) return;
        switch (ty->kind) {
            case AstNodeKind::TypeArray: {
                auto* a = static_cast<const TypeArray*>(ty);
                if (a->len) {
                    Env env{};
                    (void)check_expr(mid, a->len, env,
                                     types_.int_(IntKind::Usize));
                    if (!session_.has_errors()) {
                        if (auto v = eval.eval_usize(mid, a->len)) {
                            array_lens_[a->len] = *v;
                            types_.set_array_len_value(a->len, *v);
                        }
                    }
                }
                check_type_arrays(mid, a->elem, eval);
                return;
            }
            case AstNodeKind::TypePtr: {
                auto* p = static_cast<const TypePtr*>(ty);
                check_type_arrays(mid, p->pointee, eval);
                return;
            }
            case AstNodeKind::TypeSlice:
                check_type_arrays(mid, static_cast<const TypeSlice*>(ty)->elem,
                                  eval);
                return;
            case AstNodeKind::TypeTuple: {
                auto* t = static_cast<const TypeTuple*>(ty);
                for (const Type* e : t->elems) check_type_arrays(mid, e, eval);
                return;
            }
            case AstNodeKind::TypeFn: {
                auto* ft = static_cast<const TypeFn*>(ty);
                for (const Type* p : ft->params)
                    check_type_arrays(mid, p, eval);
                if (ft->ret) check_type_arrays(mid, ft->ret, eval);
                return;
            }
            default:
                return;
        }
    }

    void check_fn_body(ModuleId mid, const ItemFn* fn,
                       std::optional<TypeId> self_ty) {
        if (!fn || !fn->decl || !fn->decl->sig || !fn->body) return;
        auto it = fn_info_.find(fn);
        if (it == fn_info_.end()) return;

        TypeId ret_ty = it->second.ret;
        Env env{};
        env.fn_ret = ret_ty;
        env.push_scope();

        // Params
        size_t idx = 0;
        for (const Param* p : fn->decl->sig->params) {
            if (!p) continue;
            TypeId pt = it->second.params.at(idx++);
            env.declare(
                p->name,
                VarInfo{.type = pt, .is_mut = false, .state = VarState::Live});
        }

        ExprResult body = check_block(mid, fn->body, env, ret_ty);
        (void)body;

        env.pop_scope();
        (void)self_ty;
    }

    ExprResult check_block(ModuleId mid, const Block* block, Env& env,
                           std::optional<TypeId> expected) {
        if (!block) return {.type = types_.unit()};
        env.push_scope();

        for (const Stmt* s : block->stmts) {
            if (!s) continue;
            ExprResult r = check_stmt(mid, s, env, expected);
            if (r.diverged) {
                env.pop_scope();
                return {.type = r.type, .diverged = true};
            }
        }

        ExprResult out{};
        if (block->tail) {
            out = check_expr(mid, block->tail, env, expected);
        } else {
            out.type = types_.unit();
        }
        env.pop_scope();
        return out;
    }

    ExprResult check_stmt(ModuleId mid, const Stmt* stmt, Env& env,
                          std::optional<TypeId> expected_ret) {
        switch (stmt->kind) {
            case AstNodeKind::StmtLet:
                check_let(mid, static_cast<const StmtLet*>(stmt), env);
                return {.type = types_.unit()};
            case AstNodeKind::StmtExpr:
                (void)check_expr(mid, static_cast<const StmtExpr*>(stmt)->expr,
                                 env, std::nullopt);
                return {.type = types_.unit()};
            case AstNodeKind::StmtReturn: {
                auto* r = static_cast<const StmtReturn*>(stmt);
                if (!env.fn_ret) {
                    error(stmt->span,
                          "`return` is only allowed inside function bodies");
                }
                TypeId expected =
                    env.fn_ret.value_or(expected_ret.value_or(types_.unit()));
                TypeId got = r->value
                                 ? check_expr(mid, r->value, env, expected).type
                                 : types_.unit();
                if (!types_.can_coerce(got, expected)) {
                    error(stmt->span, "return type mismatch: expected `" +
                                          types_.to_string(expected) +
                                          "`, got `" + types_.to_string(got) +
                                          "`");
                }
                return {.type = types_.never(), .diverged = true};
            }
            case AstNodeKind::StmtBreak:
            case AstNodeKind::StmtContinue:
                return {.type = types_.never(), .diverged = true};
            default:
                break;
        }
        return {.type = types_.unit()};
    }

    void check_let(ModuleId mid, const StmtLet* s, Env& env) {
        TypeId ann = s->type_ann ? lower_type(mid, s->type_ann, false,
                                              std::nullopt, false)
                                 : types_.error();
        if (s->type_ann && comptime_eval_ && !session_.has_errors())
            check_type_arrays(mid, s->type_ann, *comptime_eval_);
        std::optional<TypeId> expected =
            s->type_ann ? std::optional<TypeId>(ann) : std::nullopt;

        TypeId init_ty = types_.error();
        if (s->init) init_ty = check_expr(mid, s->init, env, expected).type;

        TypeId pat_ty = expected.value_or(init_ty);
        if (!s->type_ann && !s->init) {
            error(s->span,
                  "`let` requires a type annotation or an initializer");
            pat_ty = types_.error();
        }

        if (s->type_ann && s->init && !types_.can_coerce(init_ty, ann)) {
            error(s->span, "type mismatch in `let`: expected `" +
                               types_.to_string(ann) + "`, got `" +
                               types_.to_string(init_ty) + "`");
        }

        VarState init_state = s->init ? VarState::Live : VarState::Uninit;
        bind_pattern(mid, s->pat, pat_ty, env, init_state);
    }

    void bind_pattern(ModuleId mid, const Pattern* pat, TypeId scrutinee,
                      Env& env, VarState init_state) {
        if (!pat) return;
        switch (pat->kind) {
            case AstNodeKind::PatWildcard:
                return;
            case AstNodeKind::PatBinding: {
                auto* b = static_cast<const PatBinding*>(pat);
                binding_types_[b] = scrutinee;
                env.declare(b->name, VarInfo{.type = scrutinee,
                                             .is_mut = b->is_mut,
                                             .state = init_state});
                return;
            }
            case AstNodeKind::PatInt:
                // handled by matcher; no binding
                return;
            case AstNodeKind::PatBool:
                return;
            case AstNodeKind::PatTuple: {
                auto* t = static_cast<const PatTuple*>(pat);
                const TypeData& td = types_.get(scrutinee);
                if (td.kind != TypeKind::Tuple) {
                    error(pat->span, "tuple pattern does not match type `" +
                                         types_.to_string(scrutinee) + "`");
                    return;
                }
                if (t->elems.size() != td.tuple_elems.size()) {
                    error(pat->span, "tuple pattern arity mismatch");
                    return;
                }
                for (size_t i = 0; i < t->elems.size(); i++)
                    bind_pattern(mid, t->elems[i], td.tuple_elems[i], env,
                                 init_state);
                return;
            }
            case AstNodeKind::PatStruct: {
                auto* sp = static_cast<const PatStruct*>(pat);
                TypeId ty = lower_type_path(mid, sp->type_name, false,
                                            std::nullopt, false);
                if (!types_.equal(ty, scrutinee)) {
                    error(pat->span, "struct pattern type mismatch");
                    return;
                }
                const TypeData& td = types_.get(ty);
                if (td.kind != TypeKind::Struct || !td.struct_def) return;
                const StructInfo& si = struct_info_.at(td.struct_def);
                for (const PatField* f : sp->fields) {
                    if (!f) continue;
                    auto it = si.fields.find(f->name);
                    if (it == si.fields.end()) {
                        error(f->span, "unknown field `" + f->name + "`");
                        continue;
                    }
                    bind_pattern(mid, f->pat, it->second, env, init_state);
                }
                return;
            }
            case AstNodeKind::PatVariant: {
                auto* vp = static_cast<const PatVariant*>(pat);
                auto [enum_ty, variant] = resolve_variant_path(mid, vp->path);
                if (!variant) {
                    error(pat->span, "unknown enum variant in pattern");
                    return;
                }
                if (!types_.equal(enum_ty, scrutinee)) {
                    error(pat->span, "enum pattern type mismatch");
                    return;
                }
                if (vp->args.size() != variant->payload.size()) {
                    error(pat->span, "variant pattern arity mismatch");
                    return;
                }
                for (size_t i = 0; i < vp->args.size(); i++)
                    bind_pattern(mid, vp->args[i], variant->payload[i], env,
                                 init_state);
                return;
            }
            case AstNodeKind::PatPath: {
                // unit variant; nothing to bind
                return;
            }
            case AstNodeKind::PatOr: {
                // Conservative: allow only literal-or patterns (no bindings).
                if (pattern_has_binding(static_cast<const PatOr*>(pat))) {
                    error(pat->span,
                          "bindings in `|` patterns are not supported yet");
                }
                return;
            }
            default:
                return;
        }
    }

    static bool pattern_has_binding(const Pattern* pat) {
        if (!pat) return false;
        if (pat->kind == AstNodeKind::PatBinding) return true;
        if (pat->kind == AstNodeKind::PatOr) {
            auto* o = static_cast<const PatOr*>(pat);
            return pattern_has_binding(o->lhs) || pattern_has_binding(o->rhs);
        }
        if (pat->kind == AstNodeKind::PatTuple) {
            auto* t = static_cast<const PatTuple*>(pat);
            for (const Pattern* e : t->elems) {
                if (pattern_has_binding(e)) return true;
            }
        }
        if (pat->kind == AstNodeKind::PatVariant) {
            auto* v = static_cast<const PatVariant*>(pat);
            for (const Pattern* e : v->args) {
                if (pattern_has_binding(e)) return true;
            }
        }
        if (pat->kind == AstNodeKind::PatStruct) {
            auto* s = static_cast<const PatStruct*>(pat);
            for (const PatField* f : s->fields) {
                if (pattern_has_binding(f->pat)) return true;
            }
        }
        return false;
    }

    std::pair<TypeId, const VariantInfo*> resolve_variant_path(
        ModuleId mid, const Path* path) {
        if (!path || path->segments.size() < 2)
            return {types_.error(), nullptr};
        // Resolve prefix as type path.
        Path prefix{path->span, std::vector<Ident*>{path->segments.begin(),
                                                    path->segments.end() - 1}};
        TypeId enum_ty =
            lower_type_path(mid, &prefix, false, std::nullopt, false);
        const TypeData& td = types_.get(enum_ty);
        if (td.kind != TypeKind::Enum || !td.enum_def)
            return {types_.error(), nullptr};
        const EnumInfo& ei = enum_info_.at(td.enum_def);
        std::string_view vname = path->segments.back()->text;
        auto it = ei.variants.find(std::string(vname));
        if (it == ei.variants.end()) return {enum_ty, nullptr};
        return {enum_ty, &it->second};
    }

    ExprResult check_expr(ModuleId mid, const Expr* expr, Env& env,
                          std::optional<TypeId> expected) {
        if (!expr) return {.type = types_.error()};
        ExprResult r{.type = types_.error()};
        switch (expr->kind) {
            case AstNodeKind::ExprUnit:
                r = {.type = types_.unit()};
                break;
            case AstNodeKind::ExprBool:
                r = {.type = types_.bool_()};
                break;
            case AstNodeKind::ExprInt: {
                auto* lit = static_cast<const ExprInt*>(expr);
                TypeId chosen = types_.int_(IntKind::I32);
                if (expected && types_.get(*expected).kind == TypeKind::Int)
                    chosen = *expected;
                const TypeData& td = types_.get(chosen);
                if (td.kind == TypeKind::Int &&
                    !fits_in_int_kind_value(lit->value, td.int_kind,
                                            target_layout_.pointer_bits)) {
                    error(expr->span, "integer literal does not fit in `" +
                                          types_.to_string(chosen) + "`");
                }
                r = {.type = chosen};
                break;
            }
            case AstNodeKind::ExprFloat: {
                auto* lit = static_cast<const ExprFloat*>(expr);
                TypeId chosen = types_.float_(FloatKind::F64);
                if (expected && types_.get(*expected).kind == TypeKind::Float)
                    chosen = *expected;
                const TypeData& td = types_.get(chosen);
                if (td.kind == TypeKind::Float) {
                    if (!std::isfinite(lit->value)) {
                        error(expr->span, "float literal is not finite");
                    } else if (td.float_kind == FloatKind::F32) {
                        double a = std::abs(lit->value);
                        if (a > static_cast<double>(
                                    std::numeric_limits<float>::max())) {
                            error(expr->span,
                                  "float literal does not fit in `f32`");
                        }
                    }
                }
                r = {.type = chosen};
                break;
            }
            case AstNodeKind::ExprString:
                // v0.0.15: `"..."` is a byte slice pointer (`const* [u8]`);
                // `c"..."` is a C string pointer (`const* u8`).
                if (static_cast<const ExprString*>(expr)->is_c_string) {
                    r = {.type = types_.ptr(Mutability::Const,
                                            types_.int_(IntKind::U8))};
                } else {
                    TypeId u8 = types_.int_(IntKind::U8);
                    r = {.type =
                             types_.ptr(Mutability::Const, types_.slice(u8))};
                }
                break;
            case AstNodeKind::ExprBlock:
                r = check_block(mid, static_cast<const ExprBlock*>(expr)->block,
                                env, expected);
                break;
            case AstNodeKind::ExprComptime:
                r = check_block(mid,
                                static_cast<const ExprComptime*>(expr)->block,
                                env, expected);
                break;
            case AstNodeKind::ExprPath:
                r = check_path_expr(mid,
                                    static_cast<const ExprPath*>(expr)->path,
                                    env, /*as_value=*/true);
                break;
            case AstNodeKind::ExprStructLit:
                r = check_struct_lit(mid,
                                     static_cast<const ExprStructLit*>(expr),
                                     env, expected);
                break;
            case AstNodeKind::ExprTuple:
                r = check_tuple_expr(mid, static_cast<const ExprTuple*>(expr),
                                     env, expected);
                break;
            case AstNodeKind::ExprArrayLit:
                r = check_array_lit(mid, static_cast<const ExprArrayLit*>(expr),
                                    env, expected);
                break;
            case AstNodeKind::ExprArrayRepeat:
                r = check_array_repeat(
                    mid, static_cast<const ExprArrayRepeat*>(expr), env,
                    expected);
                break;
            case AstNodeKind::ExprField:
                r = check_field_expr(mid, static_cast<const ExprField*>(expr),
                                     env);
                break;
            case AstNodeKind::ExprIndex:
                r = check_index_expr(mid, static_cast<const ExprIndex*>(expr),
                                     env);
                break;
            case AstNodeKind::ExprAssign:
                r = check_assign_expr(mid, static_cast<const ExprAssign*>(expr),
                                      env);
                break;
            case AstNodeKind::ExprUnary:
                r = check_unary_expr(mid, static_cast<const ExprUnary*>(expr),
                                     env, expected);
                break;
            case AstNodeKind::ExprBinary:
                r = check_binary_expr(mid, static_cast<const ExprBinary*>(expr),
                                      env, expected);
                break;
            case AstNodeKind::ExprCast:
                r = check_cast_expr(mid, static_cast<const ExprCast*>(expr),
                                    env);
                break;
            case AstNodeKind::ExprCall:
                r = check_call_expr(mid, static_cast<const ExprCall*>(expr),
                                    env, expected);
                break;
            case AstNodeKind::ExprMethodCall:
                r = check_method_call(mid,
                                      static_cast<const ExprMethodCall*>(expr),
                                      env, expected);
                break;
            case AstNodeKind::ExprIf:
                r = check_if_expr(mid, static_cast<const ExprIf*>(expr), env,
                                  expected);
                break;
            case AstNodeKind::ExprMatch:
                r = check_match_expr(mid, static_cast<const ExprMatch*>(expr),
                                     env, expected);
                break;
            case AstNodeKind::ExprWhile:
                r = check_while_expr(mid, static_cast<const ExprWhile*>(expr),
                                     env);
                break;
            case AstNodeKind::ExprLoop:
                r = check_loop_expr(mid, static_cast<const ExprLoop*>(expr),
                                    env);
                break;
            default:
                r = {.type = types_.error()};
                break;
        }
        expr_types_[expr] = r.type;
        return r;
    }

    ExprResult check_path_expr(ModuleId mid, const Path* path, Env& env,
                               bool as_value) {
        if (!path || path->segments.empty()) return {.type = types_.error()};

        // Local lookup only for single-segment paths.
        if (path->segments.size() == 1) {
            std::string_view name = path->segments[0]->text;
            if (VarInfo* v = env.lookup(name)) {
                if (as_value) consume_local(*path->segments[0], *v);
                return {.type = v->type};
            }
        }

        // Module-qualified values.
        if (path->segments.size() >= 2) {
            if (auto t = resolve_value_path(mid, path)) return {.type = *t};

            // Enum unit variant.
            auto [enum_ty, variant] = resolve_variant_path(mid, path);
            if (variant && variant->payload.empty()) return {.type = enum_ty};
        }

        // Single-segment values (const/static/fn).
        if (path->segments.size() == 1) {
            if (auto t = resolve_value_path(mid, path)) return {.type = *t};
        }

        error(path->span, "unresolved path in expression");
        return {.type = types_.error()};
    }

    std::optional<TypeId> resolve_value_path(ModuleId mid, const Path* path) {
        if (!path || path->segments.empty()) return std::nullopt;

        ModuleId cur = mid;
        for (size_t i = 0; i + 1 < path->segments.size(); i++) {
            const Ident* seg = path->segments[i];
            auto it = crate_.modules[cur].submodules.find(seg->text);
            if (it == crate_.modules[cur].submodules.end()) return std::nullopt;
            cur = it->second;
        }

        const Ident* last = path->segments.back();
        auto it = crate_.modules[cur].values.find(last->text);
        if (it == crate_.modules[cur].values.end()) return std::nullopt;

        const Item* item = it->second;
        switch (item->kind) {
            case AstNodeKind::ItemConst: {
                auto* c = static_cast<const ItemConst*>(item);
                auto ct = const_types_.find(c);
                return ct == const_types_.end()
                           ? std::optional<TypeId>(types_.error())
                           : std::optional<TypeId>(ct->second);
            }
            case AstNodeKind::ItemStatic: {
                auto* s = static_cast<const ItemStatic*>(item);
                auto st = static_types_.find(s);
                return st == static_types_.end()
                           ? std::optional<TypeId>(types_.error())
                           : std::optional<TypeId>(st->second);
            }
            case AstNodeKind::ItemFn: {
                auto* f = static_cast<const ItemFn*>(item);
                auto fi = fn_info_.find(f);
                if (fi == fn_info_.end())
                    return std::optional<TypeId>(types_.error());
                const FnInfo& sig = fi->second;
                if (!sig.comptime_params.empty() &&
                    std::any_of(sig.comptime_params.begin(),
                                sig.comptime_params.end(),
                                [](bool b) { return b; })) {
                    error(path->span,
                          "functions with `comptime` parameters are not "
                          "first-class values yet");
                    return std::optional<TypeId>(types_.error());
                }
                TypeId fn_ty = types_.fn(
                    std::vector<TypeId>(sig.params.begin(), sig.params.end()),
                    sig.ret);
                return std::optional<TypeId>(
                    types_.ptr(Mutability::Const, fn_ty));
            }
            default:
                break;
        }
        return std::nullopt;
    }

    void consume_local(const Ident& name, VarInfo& v) {
        if (v.state == VarState::Uninit) {
            error(name.span, "use of uninitialized local `" + name.text + "`");
            return;
        }
        if (v.state == VarState::Moved) {
            error(name.span, "use after move of local `" + name.text + "`");
            return;
        }
        if (!types_.is_copy(v.type)) v.state = VarState::Moved;
    }

    ExprResult check_struct_lit(ModuleId mid, const ExprStructLit* lit,
                                Env& env, std::optional<TypeId> expected) {
        TypeId ty =
            lower_type_path(mid, lit->type_name, false, std::nullopt, false);
        const TypeData& td = types_.get(ty);
        if (td.kind != TypeKind::Struct || !td.struct_def) {
            error(lit->span, "struct literal requires a struct type");
            return {.type = types_.error()};
        }
        const StructInfo& si = struct_info_.at(td.struct_def);

        std::unordered_map<std::string, bool> seen{};
        for (const FieldInit* f : lit->inits) {
            if (!f) continue;
            auto ft = si.fields.find(f->name);
            if (ft == si.fields.end()) {
                error(f->span, "unknown field `" + f->name + "`");
                continue;
            }
            if (seen.contains(f->name)) {
                error(f->span, "duplicate field initializer `" + f->name + "`");
                continue;
            }
            seen.insert({f->name, true});
            (void)check_expr(mid, f->value, env, ft->second);
        }

        (void)expected;
        return {.type = ty};
    }

    ExprResult check_tuple_expr(ModuleId mid, const ExprTuple* t, Env& env,
                                std::optional<TypeId> expected) {
        std::vector<TypeId> elems;
        for (const Expr* e : t->elems) {
            elems.push_back(check_expr(mid, e, env, std::nullopt).type);
        }
        (void)expected;
        return {.type = types_.tuple(std::move(elems))};
    }

    std::optional<std::uint64_t> array_len_value(ModuleId mid, TypeId arr_ty) {
        const TypeData& d = types_.get(arr_ty);
        if (d.kind != TypeKind::Array) return std::nullopt;
        if (d.array_len_value) return d.array_len_value;
        if (!d.array_len_expr) return std::nullopt;
        if (auto it = array_lens_.find(d.array_len_expr);
            it != array_lens_.end())
            return it->second;
        if (!comptime_eval_) return std::nullopt;
        if (auto v = comptime_eval_->eval_usize(mid, d.array_len_expr)) {
            array_lens_[d.array_len_expr] = *v;
            types_.set_array_len_value(d.array_len_expr, *v);
            return *v;
        }
        return std::nullopt;
    }

    ExprResult check_array_lit(ModuleId mid, const ExprArrayLit* a, Env& env,
                               std::optional<TypeId> expected) {
        const size_t n_elems = a ? a->elems.size() : 0;
        const std::uint64_t n = static_cast<std::uint64_t>(n_elems);

        if (expected && types_.get(*expected).kind == TypeKind::Array) {
            const TypeData& ed = types_.get(*expected);
            if (auto len = array_len_value(mid, *expected)) {
                if (*len != n) {
                    error(a ? a->span : Span{},
                          "array literal length mismatch: expected " +
                              std::to_string(*len) + ", got " +
                              std::to_string(n));
                }
            }
            if (!types_.is_sized(ed.elem)) {
                error(a ? a->span : Span{}, "array element type must be sized");
                return {.type = types_.error()};
            }
            for (const Expr* e : a->elems) {
                TypeId got = check_expr(mid, e, env, ed.elem).type;
                if (!types_.can_coerce(got, ed.elem)) {
                    error(e ? e->span : Span{},
                          "array element type mismatch: expected `" +
                              types_.to_string(ed.elem) + "`, got `" +
                              types_.to_string(got) + "`");
                }
            }
            return {.type = *expected};
        }

        if (n == 0) {
            error(a ? a->span : Span{},
                  "empty array literal requires an expected `[T; 0]` type");
            return {.type = types_.error()};
        }

        TypeId elem_ty = check_expr(mid, a->elems[0], env, std::nullopt).type;
        if (!types_.is_sized(elem_ty)) {
            error(a ? a->span : Span{}, "array element type must be sized");
            return {.type = types_.error()};
        }
        for (size_t i = 1; i < a->elems.size(); i++) {
            TypeId got = check_expr(mid, a->elems[i], env, elem_ty).type;
            if (!types_.can_coerce(got, elem_ty)) {
                error(a->elems[i] ? a->elems[i]->span : Span{},
                      "array element type mismatch: expected `" +
                          types_.to_string(elem_ty) + "`, got `" +
                          types_.to_string(got) + "`");
            }
        }

        TypeId out = types_.array(elem_ty, a);
        array_lens_[a] = n;
        types_.set_array_len_value(a, n);
        return {.type = out};
    }

    ExprResult check_array_repeat(ModuleId mid, const ExprArrayRepeat* a,
                                  Env& env, std::optional<TypeId> expected) {
        TypeId usize_ty = types_.int_(IntKind::Usize);
        (void)check_expr(mid, a->count, env, usize_ty);

        std::optional<std::uint64_t> n{};
        if (comptime_eval_ && !session_.has_errors()) {
            if (auto v = comptime_eval_->eval_usize(mid, a->count)) {
                n = *v;
                array_lens_[a->count] = *v;
                types_.set_array_len_value(a->count, *v);
            }
        }

        std::optional<TypeId> expected_elem{};
        if (expected && types_.get(*expected).kind == TypeKind::Array)
            expected_elem = types_.get(*expected).elem;

        TypeId elem_ty = check_expr(mid, a->elem, env, expected_elem).type;
        if (expected_elem && !types_.can_coerce(elem_ty, *expected_elem)) {
            error(a->elem ? a->elem->span : Span{},
                  "array repeat element type mismatch: expected `" +
                      types_.to_string(*expected_elem) + "`, got `" +
                      types_.to_string(elem_ty) + "`");
        }
        if (!types_.is_sized(elem_ty)) {
            error(a ? a->span : Span{}, "array element type must be sized");
            return {.type = types_.error()};
        }

        if (n && *n >= 2 && !types_.is_copy(elem_ty)) {
            error(a ? a->span : Span{},
                  "array repeat requires a `Copy` element type when N >= 2");
        }

        if (expected && types_.get(*expected).kind == TypeKind::Array) {
            if (n) {
                if (auto len = array_len_value(mid, *expected)) {
                    if (*len != *n) {
                        error(a ? a->span : Span{},
                              "array repeat count mismatch: expected " +
                                  std::to_string(*len) + ", got " +
                                  std::to_string(*n));
                    }
                }
            }
            return {.type = *expected};
        }

        TypeId out = types_.array(elem_ty, a->count);
        return {.type = out};
    }

    Place check_place(ModuleId mid, const Expr* e, Env& env,
                      bool allow_reinit_local = false) {
        if (!e) return {.type = types_.error()};
        switch (e->kind) {
            case AstNodeKind::ExprPath: {
                auto* p = static_cast<const ExprPath*>(e);
                if (!p->path || p->path->segments.empty()) break;

                // Local lookup only for single-segment paths.
                if (p->path->segments.size() == 1) {
                    std::string_view name = p->path->segments[0]->text;
                    VarInfo* v = env.lookup(name);
                    if (v) {
                        if (!allow_reinit_local) {
                            if (v->state == VarState::Uninit)
                                error(e->span, "use of uninitialized local `" +
                                                   std::string(name) + "`");
                            if (v->state == VarState::Moved)
                                error(e->span, "use after move of local `" +
                                                   std::string(name) + "`");
                        }
                        return {.type = v->type,
                                .writable = v->is_mut,
                                .root_local = std::string(name)};
                    }
                }

                // Const/static as a readable (non-writable) place.
                if (auto t = resolve_value_path(mid, p->path)) {
                    return {.type = *t, .writable = false, .root_local = ""};
                }
                break;
            }
            case AstNodeKind::ExprField: {
                auto* f = static_cast<const ExprField*>(e);
                Place base = check_place(mid, f->base, env);
                if (f->base) expr_types_[f->base] = base.type;
                TypeId bty = base.type;
                // Auto-deref pointers.
                const TypeData& td = types_.get(bty);
                if (td.kind == TypeKind::Ptr) {
                    bty = td.pointee;
                    base.root_local.clear();
                    base.writable = (td.mutability == Mutability::Mut);
                }
                const TypeData& bd = types_.get(bty);
                if (bd.kind == TypeKind::Struct && bd.struct_def) {
                    const StructInfo& si = struct_info_.at(bd.struct_def);
                    auto it = si.fields.find(f->field);
                    if (it == si.fields.end()) break;
                    return {.type = it->second,
                            .writable = base.writable,
                            .root_local = base.root_local};
                }
                if (bd.kind == TypeKind::Tuple) {
                    auto idx = parse_decimal_index(f->field);
                    if (!idx || *idx >= bd.tuple_elems.size()) {
                        error(f->span, "tuple index out of range");
                        return {.type = types_.error()};
                    }
                    return {.type = bd.tuple_elems[*idx],
                            .writable = base.writable,
                            .root_local = base.root_local};
                }
                break;
            }
            case AstNodeKind::ExprIndex: {
                auto* ix = static_cast<const ExprIndex*>(e);
                Place base = check_place(mid, ix->base, env);
                if (ix->base) expr_types_[ix->base] = base.type;
                TypeId bty = base.type;
                const TypeData& bd = types_.get(bty);
                if (bd.kind == TypeKind::Array) {
                    return {.type = bd.elem,
                            .writable = base.writable,
                            .root_local = base.root_local};
                }
                if (bd.kind == TypeKind::Ptr) {
                    const TypeData& pd = types_.get(bd.pointee);
                    if (pd.kind == TypeKind::Slice) {
                        base.root_local.clear();
                        base.writable = (bd.mutability == Mutability::Mut);
                        return {.type = pd.elem,
                                .writable = base.writable,
                                .root_local = base.root_local};
                    }
                    if (types_.is_sized(bd.pointee)) {
                        base.root_local.clear();
                        base.writable = (bd.mutability == Mutability::Mut);
                        return {.type = bd.pointee,
                                .writable = base.writable,
                                .root_local = base.root_local};
                    }
                }
                break;
            }
            case AstNodeKind::ExprUnary: {
                auto* u = static_cast<const ExprUnary*>(e);
                if (u->op != UnaryOp::Deref) break;
                TypeId t = check_expr(mid, u->expr, env, std::nullopt).type;
                if (u->expr) expr_types_[u->expr] = t;
                const TypeData& td = types_.get(t);
                if (td.kind != TypeKind::Ptr) break;
                if (types_.get(td.pointee).kind == TypeKind::Slice) break;
                return {.type = td.pointee,
                        .writable = (td.mutability == Mutability::Mut),
                        .root_local = ""};
            }
            default:
                break;
        }
        error(e->span, "expression is not assignable");
        return {.type = types_.error()};
    }

    ExprResult check_field_expr(ModuleId mid, const ExprField* f, Env& env) {
        auto base_place_like = [](const Expr* e) -> bool {
            if (!e) return false;
            switch (e->kind) {
                case AstNodeKind::ExprPath:
                case AstNodeKind::ExprField:
                case AstNodeKind::ExprIndex:
                    return true;
                case AstNodeKind::ExprUnary: {
                    auto* u = static_cast<const ExprUnary*>(e);
                    return u->op == UnaryOp::Deref;
                }
                default:
                    return false;
            }
        };

        // If the base is place-like, preserve the existing conservative
        // move-checking behavior (avoid moving the whole local just to read a
        // `Copy` field).
        if (base_place_like(f->base)) {
            Place place = check_place(mid, f, env);
            if (!types_.is_copy(place.type) && !place.root_local.empty()) {
                if (VarInfo* v = env.lookup(place.root_local))
                    v->state = VarState::Moved;
            }
            return {.type = place.type};
        }

        // Otherwise, allow reading fields from rvalues (e.g. `foo().x`).
        TypeId base_ty = check_expr(mid, f->base, env, std::nullopt).type;
        TypeId bty = base_ty;
        const TypeData& bd0 = types_.get(bty);
        if (bd0.kind == TypeKind::Ptr) bty = bd0.pointee;

        const TypeData& bd = types_.get(bty);
        if (bd.kind == TypeKind::Struct && bd.struct_def) {
            const StructInfo& si = struct_info_.at(bd.struct_def);
            auto it = si.fields.find(f->field);
            if (it == si.fields.end()) {
                error(f->span, "unknown field `" + f->field + "`");
                return {.type = types_.error()};
            }
            return {.type = it->second};
        }
        if (bd.kind == TypeKind::Tuple) {
            auto idx = parse_decimal_index(f->field);
            if (!idx || *idx >= bd.tuple_elems.size()) {
                error(f->span, "tuple index out of range");
                return {.type = types_.error()};
            }
            return {.type = bd.tuple_elems[*idx]};
        }
        error(f->span, "field access requires a struct or tuple type");
        return {.type = types_.error()};
    }

    ExprResult check_index_expr(ModuleId mid, const ExprIndex* ix, Env& env) {
        (void)check_expr(mid, ix->index, env, types_.int_(IntKind::Usize));
        auto base_place_like = [](const Expr* e) -> bool {
            if (!e) return false;
            switch (e->kind) {
                case AstNodeKind::ExprPath:
                case AstNodeKind::ExprField:
                case AstNodeKind::ExprIndex:
                    return true;
                case AstNodeKind::ExprUnary: {
                    auto* u = static_cast<const ExprUnary*>(e);
                    return u->op == UnaryOp::Deref;
                }
                default:
                    return false;
            }
        };

        if (base_place_like(ix->base)) {
            Place place = check_place(mid, ix, env);
            if (!types_.is_copy(place.type) && !place.root_local.empty()) {
                if (VarInfo* v = env.lookup(place.root_local))
                    v->state = VarState::Moved;
            }
            return {.type = place.type};
        }

        // Allow indexing into rvalues when the base is an array value or a
        // pointer value (including slice pointers).
        TypeId base_ty = check_expr(mid, ix->base, env, std::nullopt).type;
        const TypeData& bd = types_.get(base_ty);
        if (bd.kind == TypeKind::Array) return {.type = bd.elem};
        if (bd.kind == TypeKind::Ptr) {
            const TypeData& pd = types_.get(bd.pointee);
            if (pd.kind == TypeKind::Slice) return {.type = pd.elem};
            if (types_.is_sized(bd.pointee)) return {.type = bd.pointee};
        }
        error(ix->span,
              "indexing requires an array, slice pointer, or pointer");
        return {.type = types_.error()};
    }

    ExprResult check_assign_expr(ModuleId mid, const ExprAssign* a, Env& env) {
        Place lhs = check_place(mid, a->lhs, env, /*allow_reinit_local=*/true);
        if (a->lhs) expr_types_[a->lhs] = lhs.type;
        if (!lhs.writable)
            error(a->lhs->span, "assignment target is not writable");
        TypeId rhs = check_expr(mid, a->rhs, env, lhs.type).type;
        if (!types_.can_coerce(rhs, lhs.type)) {
            error(a->span, "assignment type mismatch: expected `" +
                               types_.to_string(lhs.type) + "`, got `" +
                               types_.to_string(rhs) + "`");
        }
        // Reinitialize locals on `x = ...`.
        if (!lhs.root_local.empty()) {
            if (VarInfo* v = env.lookup(lhs.root_local))
                v->state = VarState::Live;
        }
        return {.type = types_.unit()};
    }

    ExprResult check_unary_expr(ModuleId mid, const ExprUnary* u, Env& env,
                                std::optional<TypeId> expected) {
        switch (u->op) {
            case UnaryOp::Neg: {
                TypeId t = check_expr(mid, u->expr, env, expected).type;
                const TypeData& td = types_.get(t);
                if (td.kind != TypeKind::Int && td.kind != TypeKind::Float)
                    error(u->span, "unary `-` requires an integer or float");
                return {.type = t};
            }
            case UnaryOp::Not: {
                TypeId t = check_expr(mid, u->expr, env, types_.bool_()).type;
                if (!types_.equal(t, types_.bool_()))
                    error(u->span, "unary `!` requires bool");
                return {.type = types_.bool_()};
            }
            case UnaryOp::BitNot: {
                TypeId t = check_expr(mid, u->expr, env, expected).type;
                if (types_.get(t).kind != TypeKind::Int)
                    error(u->span, "unary `~` requires an integer");
                return {.type = t};
            }
            case UnaryOp::Deref: {
                TypeId t = check_expr(mid, u->expr, env, std::nullopt).type;
                const TypeData& td = types_.get(t);
                if (td.kind != TypeKind::Ptr) {
                    error(u->span, "unary `*` requires a pointer");
                    return {.type = types_.error()};
                }
                return {.type = td.pointee};
            }
            case UnaryOp::AddrOf:
            case UnaryOp::AddrOfMut: {
                Place p = check_place(mid, u->expr, env);
                if (u->expr) expr_types_[u->expr] = p.type;
                if (!types_.is_sized(p.type))
                    error(u->expr ? u->expr->span : u->span,
                          "address-of requires a sized place");
                if (u->op == UnaryOp::AddrOfMut && !p.writable) {
                    error(u->expr ? u->expr->span : u->span,
                          "`&mut` requires a mutable place");
                }
                Mutability mut = (u->op == UnaryOp::AddrOfMut)
                                     ? Mutability::Mut
                                     : Mutability::Const;
                return {.type = types_.ptr(mut, p.type)};
            }
        }
        return {.type = types_.error()};
    }

    ExprResult check_binary_expr(ModuleId mid, const ExprBinary* b, Env& env,
                                 std::optional<TypeId> expected) {
        switch (b->op) {
            case BinaryOp::Add:
            case BinaryOp::Sub:
            case BinaryOp::Mul:
            case BinaryOp::Div: {
                TypeId lhs = check_expr(mid, b->lhs, env, expected).type;
                TypeId rhs = check_expr(mid, b->rhs, env, lhs).type;
                const TypeData& ld = types_.get(lhs);
                if ((ld.kind != TypeKind::Int && ld.kind != TypeKind::Float) ||
                    !types_.can_coerce(rhs, lhs)) {
                    error(b->span,
                          "binary arithmetic requires matching numeric types");
                }
                return {.type = lhs};
            }
            case BinaryOp::Mod: {
                TypeId lhs = check_expr(mid, b->lhs, env, expected).type;
                TypeId rhs = check_expr(mid, b->rhs, env, lhs).type;
                if (types_.get(lhs).kind != TypeKind::Int ||
                    !types_.can_coerce(rhs, lhs)) {
                    error(b->span,
                          "binary `%` requires matching integer types");
                }
                return {.type = lhs};
            }
            case BinaryOp::BitAnd:
            case BinaryOp::BitOr:
            case BinaryOp::BitXor:
            case BinaryOp::Shl:
            case BinaryOp::Shr: {
                TypeId lhs = check_expr(mid, b->lhs, env, expected).type;
                TypeId rhs = check_expr(mid, b->rhs, env, lhs).type;
                if (types_.get(lhs).kind != TypeKind::Int ||
                    !types_.can_coerce(rhs, lhs)) {
                    error(b->span,
                          "bitwise operators require matching integer types");
                }
                return {.type = lhs};
            }
            case BinaryOp::Eq:
            case BinaryOp::Ne:
            case BinaryOp::Lt:
            case BinaryOp::Le:
            case BinaryOp::Gt:
            case BinaryOp::Ge: {
                TypeId lhs = check_expr(mid, b->lhs, env, std::nullopt).type;
                TypeId rhs = check_expr(mid, b->rhs, env, lhs).type;
                if (!types_.can_coerce(rhs, lhs)) {
                    error(b->span,
                          "comparison operands must have matching types");
                } else {
                    const TypeData& ld = types_.get(lhs);
                    bool ok = false;
                    if (b->op == BinaryOp::Eq || b->op == BinaryOp::Ne) {
                        ok = (ld.kind == TypeKind::Int ||
                              ld.kind == TypeKind::Float ||
                              ld.kind == TypeKind::Bool ||
                              ld.kind == TypeKind::Ptr);
                    } else {
                        ok = (ld.kind == TypeKind::Int ||
                              ld.kind == TypeKind::Float ||
                              ld.kind == TypeKind::Bool);
                    }
                    if (!ok)
                        error(b->span,
                              "unsupported operand type in comparison");
                }
                return {.type = types_.bool_()};
            }
            case BinaryOp::And:
            case BinaryOp::Or: {
                TypeId lhs = check_expr(mid, b->lhs, env, types_.bool_()).type;
                TypeId rhs = check_expr(mid, b->rhs, env, types_.bool_()).type;
                if (!types_.equal(lhs, types_.bool_()) ||
                    !types_.equal(rhs, types_.bool_())) {
                    error(b->span, "logical operators require bool");
                }
                return {.type = types_.bool_()};
            }
        }
        return {.type = types_.error()};
    }

    ExprResult check_cast_expr(ModuleId mid, const ExprCast* c, Env& env) {
        TypeId dst = lower_type(mid, c->to, false, std::nullopt, false);

        std::optional<TypeId> expected_src = std::nullopt;
        const TypeData& dd_pre = types_.get(dst);
        if (c->value && c->value->kind == AstNodeKind::ExprInt &&
            dd_pre.kind == TypeKind::Int) {
            expected_src = dst;
        } else if (c->value && c->value->kind == AstNodeKind::ExprFloat &&
                   dd_pre.kind == TypeKind::Float) {
            expected_src = dst;
        } else if (c->value && c->value->kind == AstNodeKind::ExprInt &&
                   dd_pre.kind == TypeKind::Ptr) {
            expected_src = types_.int_(IntKind::Usize);
        }

        TypeId src = check_expr(mid, c->value, env, expected_src).type;

        const TypeData& sd = types_.get(src);
        const TypeData& dd = types_.get(dst);

        bool ok = false;
        if (sd.kind == TypeKind::Int && dd.kind == TypeKind::Int) ok = true;
        if (sd.kind == TypeKind::Float && dd.kind == TypeKind::Float) ok = true;
        if (sd.kind == TypeKind::Int && dd.kind == TypeKind::Float) ok = true;
        if (sd.kind == TypeKind::Float && dd.kind == TypeKind::Int) ok = true;
        if (sd.kind == TypeKind::Int && dd.kind == TypeKind::Ptr) ok = true;
        if (sd.kind == TypeKind::Ptr && dd.kind == TypeKind::Int) ok = true;
        if (sd.kind == TypeKind::Ptr && dd.kind == TypeKind::Ptr) ok = true;
        if (sd.kind == TypeKind::Never) ok = true;

        if (!ok) {
            error(c->span, "invalid cast from `" + types_.to_string(src) +
                               "` to `" + types_.to_string(dst) + "`");
            return {.type = types_.error()};
        }

        return {.type = dst};
    }

    ExprResult check_call_expr(ModuleId mid, const ExprCall* call, Env& env,
                               std::optional<TypeId> expected) {
        const Path* callee_path = nullptr;
        if (call->callee && call->callee->kind == AstNodeKind::ExprPath) {
            callee_path = static_cast<const ExprPath*>(call->callee)->path;
        }

        if (callee_path) {
            // Builtins (comptime).
            if (callee_path->segments.size() == 2 &&
                callee_path->segments[0]->text == "builtin" &&
                callee_path->segments[1]->text == "size_of") {
                if (call->args.size() != 1) {
                    error(call->span, "builtin::size_of expects 1 argument");
                    return {.type = types_.int_(IntKind::Usize)};
                }
                auto ty = lower_type_value_expr(mid, call->args[0]);
                if (ty && !types_.is_sized(*ty))
                    error(call->args[0]->span,
                          "builtin::size_of requires a sized type");
                return {.type = types_.int_(IntKind::Usize)};
            }
            if (callee_path->segments.size() == 2 &&
                callee_path->segments[0]->text == "builtin" &&
                callee_path->segments[1]->text == "align_of") {
                if (call->args.size() != 1) {
                    error(call->span, "builtin::align_of expects 1 argument");
                    return {.type = types_.int_(IntKind::Usize)};
                }
                auto ty = lower_type_value_expr(mid, call->args[0]);
                if (ty && !types_.is_sized(*ty))
                    error(call->args[0]->span,
                          "builtin::align_of requires a sized type");
                return {.type = types_.int_(IntKind::Usize)};
            }
            if (callee_path->segments.size() == 2 &&
                callee_path->segments[0]->text == "builtin" &&
                callee_path->segments[1]->text == "type_info") {
                if (call->args.size() != 1) {
                    error(call->span, "builtin::type_info expects 1 argument");
                    return {.type = typeinfo_type()};
                }
                auto ty = lower_type_value_expr(mid, call->args[0]);
                if (ty && !types_.is_sized(*ty))
                    error(call->args[0]->span,
                          "builtin::type_info currently requires a sized type");
                return {.type = typeinfo_type()};
            }
            if (callee_path->segments.size() == 2 &&
                callee_path->segments[0]->text == "builtin" &&
                callee_path->segments[1]->text == "addr_of") {
                error(call->span,
                      "builtin::addr_of was removed; use unary `&place`");
                if (call->args.size() == 1)
                    (void)check_place(mid, call->args[0], env);
                return {.type = types_.error()};
            }
            if (callee_path->segments.size() == 2 &&
                callee_path->segments[0]->text == "builtin" &&
                callee_path->segments[1]->text == "addr_of_mut") {
                error(
                    call->span,
                    "builtin::addr_of_mut was removed; use unary `&mut place`");
                if (call->args.size() == 1)
                    (void)check_place(mid, call->args[0], env);
                return {.type = types_.error()};
            }
            if (callee_path->segments.size() == 2 &&
                callee_path->segments[0]->text == "builtin" &&
                callee_path->segments[1]->text == "compile_error") {
                TypeId msg_ty = types_.ptr(
                    Mutability::Const, types_.slice(types_.int_(IntKind::U8)));
                if (call->args.size() != 1) {
                    error(call->span,
                          "builtin::compile_error expects 1 argument");
                    return {.type = types_.never(), .diverged = true};
                }
                TypeId got = check_expr(mid, call->args[0], env, msg_ty).type;
                if (!types_.can_coerce(got, msg_ty)) {
                    error(call->args[0]->span,
                          "argument type mismatch: expected `" +
                              types_.to_string(msg_ty) + "`, got `" +
                              types_.to_string(got) + "`");
                }
                return {.type = types_.never(), .diverged = true};
            }

            // Try value call (free function) first.
            if (const ItemFn* fn = resolve_fn_path(mid, callee_path))
                return check_direct_call(mid, fn, call->args, env, expected);

            // Try enum variant constructor `Enum::Variant(...)`.
            auto [enum_ty, variant] = resolve_variant_path(mid, callee_path);
            if (variant) {
                if (variant->payload.size() != call->args.size()) {
                    error(call->span, "variant constructor arity mismatch");
                    return {.type = enum_ty};
                }
                for (size_t i = 0; i < call->args.size(); i++) {
                    (void)check_expr(mid, call->args[i], env,
                                     variant->payload[i]);
                }
                return {.type = enum_ty};
            }

            // Try tuple-struct constructor `Name(e0, e1, ...)`.
            if (const Item* ty_item = resolve_type_item(
                    mid, callee_path, /*emit_errors=*/false)) {
                if (ty_item->kind == AstNodeKind::ItemStruct) {
                    auto* s = static_cast<const ItemStruct*>(ty_item);
                    auto si_it = struct_info_.find(s);
                    if (si_it != struct_info_.end()) {
                        bool is_tuple = true;
                        for (size_t i = 0;
                             i < si_it->second.fields_in_order.size(); i++) {
                            if (si_it->second.fields_in_order[i].name !=
                                std::to_string(i)) {
                                is_tuple = false;
                                break;
                            }
                        }
                        if (is_tuple) {
                            if (call->args.size() !=
                                si_it->second.fields_in_order.size()) {
                                error(
                                    call->span,
                                    "tuple struct constructor arity mismatch");
                            } else {
                                for (size_t i = 0; i < call->args.size(); i++) {
                                    TypeId expected_arg =
                                        si_it->second.fields_in_order[i].type;
                                    (void)check_expr(mid, call->args[i], env,
                                                     expected_arg);
                                }
                            }
                            return {.type = types_.struct_(s)};
                        }
                    }
                }
            }

            // Try inherent associated function call `Type::f(...)`.
            if (callee_path->segments.size() >= 2) {
                Path prefix{
                    callee_path->span,
                    std::vector<Ident*>{callee_path->segments.begin(),
                                        callee_path->segments.end() - 1}};
                TypeId ty =
                    lower_type_path(mid, &prefix, false, std::nullopt, false);
                const TypeData& td = types_.get(ty);
                if (td.kind == TypeKind::Struct || td.kind == TypeKind::Enum) {
                    const Item* def =
                        td.kind == TypeKind::Struct
                            ? static_cast<const Item*>(td.struct_def)
                            : static_cast<const Item*>(td.enum_def);
                    if (auto it = crate_.inherent_methods.find(def);
                        it != crate_.inherent_methods.end()) {
                        std::string_view name =
                            callee_path->segments.back()->text;
                        if (auto mi = it->second.find(std::string(name));
                            mi != it->second.end()) {
                            return check_direct_call(mid, mi->second,
                                                     call->args, env, expected);
                        }
                    }
                }
            }
        }

        // Fall back to calling through a function pointer: `fp(args...)`.
        TypeId callee_ty =
            check_expr(mid, call->callee, env, std::nullopt).type;
        const TypeData& cd = types_.get(callee_ty);
        if (cd.kind == TypeKind::Ptr) {
            const TypeData& pd = types_.get(cd.pointee);
            if (pd.kind == TypeKind::Fn) {
                if (call->args.size() != pd.fn_params.size()) {
                    error(call->span, "call arity mismatch");
                    return {.type = pd.fn_ret};
                }
                for (size_t i = 0; i < call->args.size(); i++) {
                    TypeId expected_arg = pd.fn_params[i];
                    TypeId got =
                        check_expr(mid, call->args[i], env, expected_arg).type;
                    if (!types_.can_coerce(got, expected_arg)) {
                        error(call->args[i]->span,
                              "argument type mismatch: expected `" +
                                  types_.to_string(expected_arg) + "`, got `" +
                                  types_.to_string(got) + "`");
                    }
                }
                (void)expected;
                return {.type = pd.fn_ret};
            }
        }

        error(call->span, "unresolved call target");
        return {.type = types_.error()};
    }

    const ItemFn* resolve_fn_path(ModuleId mid, const Path* path) {
        if (!path || path->segments.empty()) return nullptr;
        if (path->segments.size() == 1) {
            auto it = crate_.modules[mid].values.find(path->segments[0]->text);
            if (it == crate_.modules[mid].values.end()) return nullptr;
            if (it->second->kind == AstNodeKind::ItemFn)
                return static_cast<const ItemFn*>(it->second);
            return nullptr;
        }

        ModuleId cur = mid;
        for (size_t i = 0; i + 1 < path->segments.size(); i++) {
            auto it =
                crate_.modules[cur].submodules.find(path->segments[i]->text);
            if (it == crate_.modules[cur].submodules.end()) return nullptr;
            cur = it->second;
        }
        auto it = crate_.modules[cur].values.find(path->segments.back()->text);
        if (it == crate_.modules[cur].values.end()) return nullptr;
        if (it->second->kind == AstNodeKind::ItemFn)
            return static_cast<const ItemFn*>(it->second);
        return nullptr;
    }

    ExprResult check_direct_call(ModuleId mid, const ItemFn* fn,
                                 const std::vector<Expr*>& args, Env& env,
                                 std::optional<TypeId> expected) {
        auto it = fn_info_.find(fn);
        if (it == fn_info_.end()) return {.type = types_.error()};

        const FnInfo& sig = it->second;
        const size_t fixed = sig.params.size();
        const bool has_comptime_params =
            !sig.comptime_params.empty() &&
            std::any_of(sig.comptime_params.begin(), sig.comptime_params.end(),
                        [](bool b) { return b; });

        // Used to validate comptime arguments (v0.0.17+). Constructed lazily
        // because it is relatively heavy.
        std::optional<LayoutEngine> layout{};
        std::optional<ComptimeEvaluator> eval{};
        if (has_comptime_params) {
            layout.emplace(session_, types_, struct_info_, enum_info_,
                           array_lens_, target_layout_);
            eval.emplace(session_, crate_, &types_, &*layout);
        }
        if (!sig.is_variadic) {
            if (args.size() != fixed) {
                error(fn->span, "call arity mismatch");
                return {.type = sig.ret};
            }
        } else {
            if (args.size() < fixed) {
                error(fn->span,
                      "call arity mismatch (variadic function requires at "
                      "least " +
                          std::to_string(fixed) + " args)");
                return {.type = sig.ret};
            }
        }

        for (size_t i = 0; i < fixed && i < args.size(); i++) {
            TypeId got = check_expr(mid, args[i], env, sig.params[i]).type;
            if (!types_.can_coerce(got, sig.params[i])) {
                error(args[i]->span, "argument type mismatch: expected `" +
                                         types_.to_string(sig.params[i]) +
                                         "`, got `" + types_.to_string(got) +
                                         "`");
            }

            if (has_comptime_params && i < sig.comptime_params.size() &&
                sig.comptime_params[i]) {
                const size_t before = session_.diags.size();
                auto v = eval->eval_expr(mid, args[i]);
                if (!v && session_.diags.size() == before) {
                    // If the interpreter didn't emit a diagnostic, provide a
                    // generic one.
                    error(args[i]->span,
                          "argument must be a comptime constant");
                }
            }
        }

        if (sig.is_variadic) {
            for (size_t i = fixed; i < args.size(); i++) {
                TypeId got = check_expr(mid, args[i], env, std::nullopt).type;
                const TypeData& gd = types_.get(got);
                bool ok = false;
                switch (gd.kind) {
                    case TypeKind::Bool:
                    case TypeKind::Int:
                    case TypeKind::Float:
                        ok = true;
                        break;
                    case TypeKind::Ptr: {
                        const TypeData& pd = types_.get(gd.pointee);
                        ok = pd.kind != TypeKind::Slice;
                        break;
                    }
                    default:
                        ok = false;
                        break;
                }
                if (!ok) {
                    error(args[i]->span, "invalid C vararg argument type `" +
                                             types_.to_string(got) + "`");
                }
            }
        }
        (void)expected;
        return {.type = sig.ret};
    }

    ExprResult check_method_call(ModuleId mid, const ExprMethodCall* mc,
                                 Env& env, std::optional<TypeId> expected) {
        // Avoid moving locals when analyzing the receiver; we'll auto-borrow if
        // needed.
        TypeId recv_ty = 0;
        if (mc->receiver && mc->receiver->kind == AstNodeKind::ExprPath) {
            recv_ty = check_path_expr(
                          mid, static_cast<const ExprPath*>(mc->receiver)->path,
                          env, /*as_value=*/false)
                          .type;
            expr_types_[mc->receiver] = recv_ty;
        } else {
            recv_ty = check_expr(mid, mc->receiver, env, std::nullopt).type;
        }
        const TypeData& rd = types_.get(recv_ty);

        // Determine nominal receiver type.
        TypeId nominal = recv_ty;
        if (rd.kind == TypeKind::Ptr) nominal = rd.pointee;

        const TypeData& nd = types_.get(nominal);
        const Item* def = nullptr;
        if (nd.kind == TypeKind::Struct)
            def = static_cast<const Item*>(nd.struct_def);
        if (nd.kind == TypeKind::Enum)
            def = static_cast<const Item*>(nd.enum_def);
        if (!def) {
            error(mc->span, "method calls require a nominal receiver type");
            return {.type = types_.error()};
        }

        // Inherent first.
        const ItemFn* method = nullptr;
        if (auto it = crate_.inherent_methods.find(def);
            it != crate_.inherent_methods.end()) {
            auto mi = it->second.find(mc->method);
            if (mi != it->second.end()) method = mi->second;
        }

        if (!method) {
            error(mc->span, "cannot resolve method `" + mc->method + "`");
            return {.type = types_.error()};
        }

        auto fi = fn_info_.find(method);
        if (fi == fn_info_.end()) return {.type = types_.error()};

        const FnInfo& sig = fi->second;
        if (sig.params.empty()) {
            error(method->span, "method is missing a receiver parameter");
            return {.type = sig.ret};
        }

        TypeId self_param = sig.params[0];
        TypeId recv_arg = recv_ty;

        // Auto-borrow locals for pointer receivers.
        if (types_.get(self_param).kind == TypeKind::Ptr &&
            rd.kind != TypeKind::Ptr) {
            Place place = check_place(mid, mc->receiver, env);
            if (place.root_local.empty()) {
                error(mc->receiver->span,
                      "cannot take address of this receiver");
            } else {
                // Determine mutability required.
                const TypeData& sp = types_.get(self_param);
                if (sp.mutability == Mutability::Mut && !place.writable) {
                    error(mc->receiver->span,
                          "method requires a mutable receiver");
                }
                recv_arg = types_.ptr(sp.mutability, place.type);
            }
        }

        if (!types_.can_coerce(recv_arg, self_param)) {
            error(mc->receiver->span, "receiver type mismatch: expected `" +
                                          types_.to_string(self_param) +
                                          "`, got `" +
                                          types_.to_string(recv_arg) + "`");
        }

        if (mc->args.size() + 1 != sig.params.size()) {
            error(mc->span, "method call arity mismatch");
            return {.type = sig.ret};
        }

        for (size_t i = 0; i < mc->args.size(); i++) {
            TypeId expected_arg = sig.params[i + 1];
            TypeId got = check_expr(mid, mc->args[i], env, expected_arg).type;
            if (!types_.can_coerce(got, expected_arg)) {
                error(mc->args[i]->span, "argument type mismatch: expected `" +
                                             types_.to_string(expected_arg) +
                                             "`, got `" +
                                             types_.to_string(got) + "`");
            }
        }

        (void)expected;
        return {.type = sig.ret};
    }

    ExprResult check_if_expr(ModuleId mid, const ExprIf* iff, Env& env,
                             std::optional<TypeId> expected) {
        TypeId cond = check_expr(mid, iff->cond, env, types_.bool_()).type;
        if (!types_.equal(cond, types_.bool_()))
            error(iff->cond->span, "if condition must be bool");

        Env then_env = env;
        ExprResult then_r =
            check_block(mid, iff->then_block, then_env, expected);

        if (!iff->else_expr) {
            env = then_env;
            return {.type = types_.unit()};
        }

        Env else_env = env;
        ExprResult else_r = check_expr(mid, iff->else_expr, else_env, expected);

        if (!types_.can_coerce(else_r.type, then_r.type) &&
            !types_.can_coerce(then_r.type, else_r.type)) {
            error(iff->span, "if branches have incompatible types");
        }

        TypeId out_ty = then_r.type;
        if (types_.can_coerce(then_r.type, else_r.type))
            out_ty = else_r.type;
        else if (types_.can_coerce(else_r.type, then_r.type))
            out_ty = then_r.type;

        Env out = then_env;
        join_env(out, else_env);
        env = out;
        return {.type = out_ty};
    }

    ExprResult check_match_expr(ModuleId mid, const ExprMatch* m, Env& env,
                                std::optional<TypeId> expected) {
        TypeId scrutinee =
            check_expr(mid, m->scrutinee, env, std::nullopt).type;

        std::optional<TypeId> arm_type{};
        bool any_arm = false;
        Env joined_env = env;
        bool first_env = true;

        auto unify = [&](TypeId a, TypeId b, Span at) -> TypeId {
            if (types_.can_coerce(a, b)) return b;
            if (types_.can_coerce(b, a)) return a;
            error(at, "match arm type mismatch");
            return a;
        };

        // ---- Exhaustiveness (v0.0.18; conservative) ----
        const auto pat_is_catchall = [&](const Pattern* pat) -> bool {
            if (!pat) return false;
            return pat->kind == AstNodeKind::PatWildcard ||
                   pat->kind == AstNodeKind::PatBinding;
        };

        bool bool_true = false;
        bool bool_false = false;
        bool has_catchall = false;

        std::unordered_map<std::string, bool> enum_covered{};
        const ItemEnum* enum_def = nullptr;
        if (types_.get(scrutinee).kind == TypeKind::Enum)
            enum_def = types_.get(scrutinee).enum_def;
        if (enum_def) {
            auto it = enum_info_.find(enum_def);
            if (it != enum_info_.end()) {
                for (const std::string& v : it->second.variants_in_order)
                    enum_covered.insert({v, false});
            }
        }

        std::function<void(const Pattern*)> collect_bool{};
        collect_bool = [&](const Pattern* pat) {
            if (!pat || has_catchall) return;
            if (pat_is_catchall(pat)) {
                has_catchall = true;
                return;
            }
            switch (pat->kind) {
                case AstNodeKind::PatBool: {
                    auto* bp = static_cast<const PatBool*>(pat);
                    if (bp->value)
                        bool_true = true;
                    else
                        bool_false = true;
                    return;
                }
                case AstNodeKind::PatOr: {
                    auto* o = static_cast<const PatOr*>(pat);
                    collect_bool(o->lhs);
                    collect_bool(o->rhs);
                    return;
                }
                default:
                    return;
            }
        };

        std::function<void(const Pattern*)> collect_enum{};
        collect_enum = [&](const Pattern* pat) {
            if (!pat || has_catchall || !enum_def) return;
            if (pat_is_catchall(pat)) {
                has_catchall = true;
                return;
            }
            if (pat->kind == AstNodeKind::PatOr) {
                auto* o = static_cast<const PatOr*>(pat);
                collect_enum(o->lhs);
                collect_enum(o->rhs);
                return;
            }
            if (pat->kind == AstNodeKind::PatPath) {
                auto* pp = static_cast<const PatPath*>(pat);
                auto [ety, vi] = resolve_variant_path(mid, pp->path);
                if (!vi || !types_.equal(ety, scrutinee)) return;
                if (!vi->payload.empty()) return;
                if (vi->ast) enum_covered[vi->ast->name] = true;
                return;
            }
            if (pat->kind == AstNodeKind::PatVariant) {
                auto* vp = static_cast<const PatVariant*>(pat);
                auto [ety, vi] = resolve_variant_path(mid, vp->path);
                if (!vi || !types_.equal(ety, scrutinee) || !vi->ast) return;
                if (vi->payload.empty()) {
                    enum_covered[vi->ast->name] = true;
                    return;
                }
                // Conservative: only treat `Variant(_, _, ...)` as covering the
                // full variant.
                if (vp->args.size() != vi->payload.size()) return;
                for (const Pattern* a : vp->args) {
                    if (!pat_is_catchall(a)) return;
                }
                enum_covered[vi->ast->name] = true;
                return;
            }
        };

        for (const MatchArm* arm : m->arms) {
            if (!arm) continue;
            any_arm = true;

            Env arm_env = env;
            arm_env.push_scope();
            check_pattern(mid, arm->pat, scrutinee, arm_env);
            if (arm->guard) {
                TypeId gt =
                    check_expr(mid, arm->guard, arm_env, types_.bool_()).type;
                if (!types_.equal(gt, types_.bool_()))
                    error(arm->guard->span, "match guard must be bool");
            }
            ExprResult r = check_expr(mid, arm->body, arm_env, expected);
            arm_env.pop_scope();

            if (!arm_type) arm_type = expected.value_or(r.type);
            *arm_type = unify(*arm_type, r.type, arm->span);

            // Guards do not contribute to exhaustiveness (conservative).
            if (!arm->guard) {
                const TypeData& sd = types_.get(scrutinee);
                if (sd.kind == TypeKind::Bool) {
                    collect_bool(arm->pat);
                } else if (sd.kind == TypeKind::Int) {
                    if (pat_is_catchall(arm->pat)) has_catchall = true;
                } else if (sd.kind == TypeKind::Enum) {
                    collect_enum(arm->pat);
                }
            }

            if (first_env) {
                joined_env = arm_env;
                first_env = false;
            } else {
                join_env(joined_env, arm_env);
            }
        }

        // Emit exhaustiveness diagnostics after pattern checking/type
        // unification, to avoid cascaded errors in earlier stages.
        if (any_arm) {
            const TypeData& sd = types_.get(scrutinee);
            if (sd.kind == TypeKind::Bool) {
                if (!has_catchall && !(bool_true && bool_false)) {
                    error(m->span,
                          "non-exhaustive match on `bool` (missing "
                          "`true`/`false` or `_`)");
                }
            } else if (sd.kind == TypeKind::Int) {
                if (!has_catchall) {
                    error(m->span,
                          "match on integer types must include a `_` catch-all "
                          "arm in v0.1");
                }
            } else if (sd.kind == TypeKind::Enum && enum_def) {
                if (!has_catchall) {
                    std::vector<std::string> missing{};
                    for (const auto& [name, ok] : enum_covered) {
                        if (!ok) missing.push_back(name);
                    }
                    if (!missing.empty()) {
                        error(m->span, "non-exhaustive match on enum `" +
                                           enum_def->name +
                                           "` (missing variants)");
                    }
                }
            }
        }

        env = any_arm ? joined_env : env;
        return {.type = arm_type.value_or(types_.unit())};
    }

    ExprResult check_while_expr(ModuleId mid, const ExprWhile* wh, Env& env) {
        if (!wh) return {.type = types_.unit()};
        TypeId cond = check_expr(mid, wh->cond, env, types_.bool_()).type;
        if (!types_.equal(cond, types_.bool_()))
            error(wh->cond->span, "while condition must be bool");

        Env body_env = env;
        (void)check_block(mid, wh->body, body_env, std::nullopt);
        join_env(env, body_env);
        return {.type = types_.unit()};
    }

    ExprResult check_loop_expr(ModuleId mid, const ExprLoop* lp, Env& env) {
        if (!lp) return {.type = types_.unit()};
        Env body_env = env;
        (void)check_block(mid, lp->body, body_env, std::nullopt);
        env = body_env;
        return {.type = types_.unit()};
    }

    void check_pattern(ModuleId mid, const Pattern* pat, TypeId scrutinee,
                       Env& env) {
        if (!pat) return;
        switch (pat->kind) {
            case AstNodeKind::PatWildcard:
                return;
            case AstNodeKind::PatInt:
                if (types_.get(scrutinee).kind != TypeKind::Int)
                    error(pat->span,
                          "integer pattern on non-integer scrutinee");
                return;
            case AstNodeKind::PatBool:
                if (!types_.equal(scrutinee, types_.bool_()))
                    error(pat->span, "bool pattern on non-bool scrutinee");
                return;
            case AstNodeKind::PatBinding:
                bind_pattern(mid, pat, scrutinee, env, VarState::Live);
                return;
            case AstNodeKind::PatTuple:
            case AstNodeKind::PatStruct:
            case AstNodeKind::PatVariant:
            case AstNodeKind::PatPath:
            case AstNodeKind::PatOr:
                bind_pattern(mid, pat, scrutinee, env, VarState::Live);
                return;
            default:
                return;
        }
    }
};

}  // namespace

std::optional<CheckedCrate> check_crate(Session& session,
                                        const ResolvedCrate& crate,
                                        const TargetLayout& target_layout) {
    Checker checker(session, crate, target_layout);
    if (!checker.run()) return std::nullopt;
    return std::move(checker).finish();
}

}  // namespace cog
