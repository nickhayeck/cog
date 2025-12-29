#include "comptime.hpp"

#include <algorithm>
#include <cstdint>
#include <limits>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>

#include "diag.hpp"
#include "layout.hpp"
#include "types.hpp"

namespace cog {
namespace {

static bool path_is_builtin(const Path* path, std::string_view name) {
    if (!path || path->segments.size() != 2) return false;
    return path->segments[0]->text == "builtin" &&
           path->segments[1]->text == name;
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

}  // namespace

TypeId ComptimeEvaluator::lower_type(ModuleId mid, const Type* ty,
                                     bool allow_unsized) {
    if (!types_) {
        error(ty ? ty->span : Span{},
              "internal error: type lowering is unavailable in this comptime "
              "context");
        return 0;
    }
    if (!ty) return types_->error();
    switch (ty->kind) {
        case AstNodeKind::TypePath:
            return lower_type_path(mid, static_cast<const TypePath*>(ty)->path,
                                   allow_unsized);
        case AstNodeKind::TypeCall: {
            auto* c = static_cast<const TypeCall*>(ty);
            if (!c->callee) return types_->error();
            const Item* item = resolve_value_item(mid, c->callee);
            if (!item || item->kind != AstNodeKind::ItemFn) {
                error(ty->span,
                      "type-level call target must be a function returning "
                      "`type`");
                return types_->error();
            }
            const ItemFn* fn = static_cast<const ItemFn*>(item);
            if (!fn->decl || !fn->decl->sig) return types_->error();
            TypeId ret =
                fn->decl->sig->ret
                    ? lower_type(mid, fn->decl->sig->ret, /*allow_unsized=*/true)
                    : types_->unit();
            if (!types_->equal(ret, types_->type_type())) {
                error(ty->span,
                      "type-level call target must return `type` (got `" +
                          types_->to_string(ret) + "`)");
                return types_->error();
            }
            if (fn->decl->sig->params.size() != c->args.size()) {
                error(ty->span, "type-level call arity mismatch");
                return types_->error();
            }
            std::vector<ComptimeValue> args{};
            args.reserve(c->args.size());
            for (size_t i = 0; i < c->args.size(); i++) {
                const Param* p = fn->decl->sig->params[i];
                if (!p || !p->is_comptime) {
                    error(ty->span,
                          "type-level call targets must take only `comptime` "
                          "parameters in v0.1");
                    return types_->error();
                }
                TypeId pt =
                    lower_type(mid, p->type, /*allow_unsized=*/true);
                if (!types_->equal(pt, types_->type_type())) {
                    error(ty->span,
                          "type-level call parameters must have type `type` in "
                          "v0.1");
                    return types_->error();
                }
                TypeId at =
                    lower_type(mid, c->args[i], /*allow_unsized=*/true);
                args.push_back(ComptimeValue::type_(at));
            }
            auto rv = eval_fn(fn, std::move(args), ty->span);
            if (!rv || rv->kind != ComptimeValue::Kind::Type) {
                error(ty->span, "type-level call did not produce a `type`");
                return types_->error();
            }
            if (!allow_unsized && !types_->is_sized(rv->type_value)) {
                error(ty->span, "unsized type must appear behind a pointer");
                return types_->error();
            }
            return rv->type_value;
        }
        case AstNodeKind::TypeType:
            return types_->type_type();
        case AstNodeKind::TypeUnit:
            return types_->unit();
        case AstNodeKind::TypeNever:
            return types_->never();
        case AstNodeKind::TypePtr: {
            auto* p = static_cast<const TypePtr*>(ty);
            return types_->ptr(
                p->mutability,
                lower_type(mid, p->pointee, /*allow_unsized=*/true));
        }
        case AstNodeKind::TypeSlice: {
            if (!allow_unsized) {
                error(ty->span, "unsized slice types are not allowed here");
                return types_->error();
            }
            auto* s = static_cast<const TypeSlice*>(ty);
            return types_->slice(
                lower_type(mid, s->elem, /*allow_unsized=*/false));
        }
        case AstNodeKind::TypeArray: {
            auto* a = static_cast<const TypeArray*>(ty);
            return types_->array(
                lower_type(mid, a->elem, /*allow_unsized=*/false), a->len);
        }
        case AstNodeKind::TypeFn: {
            if (!allow_unsized) {
                error(ty->span, "function types are not allowed here");
                return types_->error();
            }
            auto* ft = static_cast<const TypeFn*>(ty);
            std::vector<TypeId> params{};
            params.reserve(ft->params.size());
            for (const Type* p : ft->params)
                params.push_back(lower_type(mid, p, /*allow_unsized=*/false));
            TypeId ret = ft->ret
                             ? lower_type(mid, ft->ret, /*allow_unsized=*/false)
                             : types_->unit();
            return types_->fn(std::move(params), ret);
        }
        case AstNodeKind::TypeTuple: {
            auto* t = static_cast<const TypeTuple*>(ty);
            std::vector<TypeId> elems{};
            elems.reserve(t->elems.size());
            for (const Type* e : t->elems)
                elems.push_back(lower_type(mid, e, /*allow_unsized=*/false));
            return types_->tuple(std::move(elems));
        }
        default:
            error(ty->span, "unsupported type at comptime");
            return types_->error();
    }
}

TypeId ComptimeEvaluator::lower_type_path(ModuleId mid, const Path* path,
                                          bool allow_unsized) {
    if (!types_) {
        error(path ? path->span : Span{},
              "internal error: type lowering is unavailable in this comptime "
              "context");
        return 0;
    }
    if (!path || path->segments.empty()) return types_->error();
    if (path->segments.size() == 1) {
        std::string_view name = path->segments[0]->text;
        if (name == "bool") return types_->bool_();
        if (auto ik = types_->parse_int_kind(name)) return types_->int_(*ik);
        if (auto fk = types_->parse_float_kind(name))
            return types_->float_(*fk);
    }
    const Item* item = resolve_type_item(mid, path);
    if (!item) {
        error(path->span, "unknown type in comptime type expression");
        return types_->error();
    }
    switch (item->kind) {
        case AstNodeKind::ItemStruct:
            return types_->struct_(static_cast<const ItemStruct*>(item));
        case AstNodeKind::ItemEnum:
            return types_->enum_(static_cast<const ItemEnum*>(item));
        case AstNodeKind::ItemTypeAlias: {
            auto* ta = static_cast<const ItemTypeAlias*>(item);
            return lower_type(mid, ta->aliased, allow_unsized);
        }
        default:
            break;
    }
    return types_->error();
}

std::optional<TypeId> ComptimeEvaluator::lower_type_value_expr(
    ModuleId mid, const Expr* expr) {
    if (!expr) return std::nullopt;
    if (expr->kind != AstNodeKind::ExprPath) {
        error(expr->span, "expected a type value (path)");
        return std::nullopt;
    }
    const Path* p = static_cast<const ExprPath*>(expr)->path;
    return lower_type_path(mid, p, /*allow_unsized=*/false);
}

ComptimeEvaluator::ComptimeEvaluator(Session& session,
                                     const ResolvedCrate& crate,
                                     TypeStore* types, LayoutEngine* layout,
                                     TypeConstructContext type_ctx)
    : session_(session),
      crate_(crate),
      types_(types),
      layout_(layout),
      type_ctx_(std::move(type_ctx)) {
    for (ModuleId mid = 0; mid < crate_.modules.size(); mid++) {
        const Module& m = crate_.modules[mid];
        for (const Item* item : m.items) {
            if (!item) continue;
            if (item->kind == AstNodeKind::ItemConst)
                const_module_.insert(
                    {static_cast<const ItemConst*>(item), mid});
            if (item->kind == AstNodeKind::ItemStatic)
                static_module_.insert(
                    {static_cast<const ItemStatic*>(item), mid});
            if (item->kind == AstNodeKind::ItemFn)
                fn_module_.insert({static_cast<const ItemFn*>(item), mid});
        }
    }

    typeinfo_struct_ = find_struct_global("TypeInfo");
    typekind_enum_ = find_enum_in_builtin_module("TypeKind");
}

void ComptimeEvaluator::error(Span span, std::string message) {
    session_.diags.push_back(Diagnostic{.severity = Severity::Error,
                                        .span = span,
                                        .message = std::move(message)});
}

bool ComptimeEvaluator::consume_step(Span span) {
    if (step_budget_ == 0) {
        error(span, "comptime step budget exceeded");
        return false;
    }
    step_budget_--;
    return true;
}

bool ComptimeEvaluator::consume_heap(Span span, std::uint64_t units) {
    if (heap_budget_units_ < units) {
        error(span, "comptime heap budget exceeded");
        return false;
    }
    heap_budget_units_ -= units;
    return true;
}

std::optional<std::uint64_t> ComptimeEvaluator::heap_alloc(Span span,
                                                           ComptimeValue v) {
    // Coarse heap accounting: charge a small header, plus a linear cost for
    // nested aggregates.
    std::uint64_t units = 8;
    switch (v.kind) {
        case ComptimeValue::Kind::String:
            units += static_cast<std::uint64_t>(v.string_value.size());
            break;
        case ComptimeValue::Kind::Array:
            units += static_cast<std::uint64_t>(v.array_elems.size()) * 2;
            break;
        case ComptimeValue::Kind::Tuple:
            units += static_cast<std::uint64_t>(v.tuple_elems.size()) * 2;
            break;
        case ComptimeValue::Kind::Struct:
            units += static_cast<std::uint64_t>(v.struct_fields.size()) * 2;
            break;
        case ComptimeValue::Kind::Enum:
            units += static_cast<std::uint64_t>(v.enum_payload.size()) * 2;
            break;
        default:
            break;
    }
    if (!consume_heap(span, units)) return std::nullopt;
    heap_.push_back(HeapObject{.value = std::move(v)});
    return heap_.size();  // 1-based IDs
}

const ComptimeValue* ComptimeEvaluator::heap_deref(Span span,
                                                   const ComptimeValue& ref) {
    if (ref.kind != ComptimeValue::Kind::Ref) {
        error(span, "expected a comptime reference (`&place` result)");
        return nullptr;
    }
    if (ref.ptr_value == 0 || ref.ptr_value > heap_.size()) {
        error(span, "invalid comptime reference");
        return nullptr;
    }
    return &heap_[static_cast<size_t>(ref.ptr_value - 1)].value;
}

std::string ComptimeEvaluator::type_key(TypeId t) {
    if (!types_) return "<no-types>";
    const TypeData& d = types_->get(t);
    switch (d.kind) {
        case TypeKind::Error:
            return "err";
        case TypeKind::Unit:
            return "unit";
        case TypeKind::Bool:
            return "bool";
        case TypeKind::Int:
        case TypeKind::Float:
        case TypeKind::Never:
        case TypeKind::TypeType:
        case TypeKind::Self:
            // `to_string` is stable for primitives.
            return types_->to_string(t);
        case TypeKind::Ptr: {
            std::ostringstream out;
            out << (d.mutability == Mutability::Mut ? "mut*" : "const*");
            out << "<" << type_key(d.pointee) << ">";
            return out.str();
        }
        case TypeKind::Slice: {
            std::ostringstream out;
            out << "slice<" << type_key(d.elem) << ">";
            return out.str();
        }
        case TypeKind::Array: {
            std::ostringstream out;
            out << "array<" << type_key(d.elem) << ">";
            out << "[";
            if (d.array_len_value) {
                out << *d.array_len_value;
            } else {
                out << "expr@" << reinterpret_cast<std::uintptr_t>(d.array_len_expr);
            }
            out << "]";
            return out.str();
        }
        case TypeKind::Tuple: {
            std::ostringstream out;
            out << "tuple(";
            for (size_t i = 0; i < d.tuple_elems.size(); i++) {
                if (i) out << ",";
                out << type_key(d.tuple_elems[i]);
            }
            out << ")";
            return out.str();
        }
        case TypeKind::Fn: {
            std::ostringstream out;
            out << "fn(";
            for (size_t i = 0; i < d.fn_params.size(); i++) {
                if (i) out << ",";
                out << type_key(d.fn_params[i]);
            }
            out << ")->" << type_key(d.fn_ret);
            return out.str();
        }
        case TypeKind::Struct: {
            std::ostringstream out;
            out << "struct@" << reinterpret_cast<std::uintptr_t>(d.struct_def);
            return out.str();
        }
        case TypeKind::Enum: {
            std::ostringstream out;
            out << "enum@" << reinterpret_cast<std::uintptr_t>(d.enum_def);
            return out.str();
        }
    }
    return "<type>";
}

std::string ComptimeEvaluator::value_key(
    const ComptimeValue& v, std::unordered_set<std::uint64_t>& visiting) {
    switch (v.kind) {
        case ComptimeValue::Kind::Error:
            return "err";
        case ComptimeValue::Kind::Unit:
            return "unit";
        case ComptimeValue::Kind::Bool:
            return v.bool_value ? "bool:true" : "bool:false";
        case ComptimeValue::Kind::Int:
            return "int:" + std::to_string(v.int_value);
        case ComptimeValue::Kind::Float:
            return "float:" + std::to_string(v.float_value);
        case ComptimeValue::Kind::Type:
            return "type:" + type_key(v.type_value);
        case ComptimeValue::Kind::Ptr:
            return "ptr:" + std::to_string(v.ptr_value);
        case ComptimeValue::Kind::Ref: {
            if (v.ptr_value == 0) return "ref:null";
            if (visiting.contains(v.ptr_value)) return "ref:<cycle>";
            visiting.insert(v.ptr_value);
            const ComptimeValue* pointee = heap_deref(Span{}, v);
            std::string inner = pointee ? value_key(*pointee, visiting)
                                        : std::string("<invalid>");
            visiting.erase(v.ptr_value);
            return "ref<" + inner + ">";
        }
        case ComptimeValue::Kind::String:
            return "str:" + v.string_value;
        case ComptimeValue::Kind::Array: {
            std::ostringstream out;
            out << "arr[";
            for (size_t i = 0; i < v.array_elems.size(); i++) {
                if (i) out << ",";
                out << value_key(v.array_elems[i], visiting);
            }
            out << "]";
            return out.str();
        }
        case ComptimeValue::Kind::Tuple: {
            std::ostringstream out;
            out << "tup(";
            for (size_t i = 0; i < v.tuple_elems.size(); i++) {
                if (i) out << ",";
                out << value_key(v.tuple_elems[i], visiting);
            }
            out << ")";
            return out.str();
        }
        case ComptimeValue::Kind::Struct: {
            std::ostringstream out;
            out << "struct@"
                << reinterpret_cast<std::uintptr_t>(v.struct_def) << "{";
            if (v.struct_def) {
                bool first = true;
                for (const FieldDecl* f : v.struct_def->fields) {
                    if (!f) continue;
                    if (!first) out << ",";
                    first = false;
                    out << f->name << "=";
                    auto it = v.struct_fields.find(f->name);
                    if (it == v.struct_fields.end()) {
                        out << "<missing>";
                    } else {
                        out << value_key(it->second, visiting);
                    }
                }
            } else {
                // Fall back to sorted keys if we don't have a field list.
                std::vector<std::string> keys{};
                keys.reserve(v.struct_fields.size());
                for (const auto& [k, _] : v.struct_fields) keys.push_back(k);
                std::sort(keys.begin(), keys.end());
                bool first = true;
                for (const std::string& k : keys) {
                    if (!first) out << ",";
                    first = false;
                    out << k << "=" << value_key(v.struct_fields.at(k), visiting);
                }
            }
            out << "}";
            return out.str();
        }
        case ComptimeValue::Kind::Enum: {
            std::ostringstream out;
            out << "enum@" << reinterpret_cast<std::uintptr_t>(v.enum_def) << "::"
                << v.enum_variant << "(";
            for (size_t i = 0; i < v.enum_payload.size(); i++) {
                if (i) out << ",";
                out << value_key(v.enum_payload[i], visiting);
            }
            out << ")";
            return out.str();
        }
    }
    return "<value>";
}

ComptimeEvaluator::Binding* ComptimeEvaluator::Env::lookup(
    std::string_view name) {
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
        auto found = it->find(std::string(name));
        if (found != it->end()) return &found->second;
    }
    return nullptr;
}

void ComptimeEvaluator::Env::declare(std::string name, Binding b) {
    scopes.back().insert({std::move(name), std::move(b)});
}

const Item* ComptimeEvaluator::resolve_type_item(ModuleId mid,
                                                 const Path* path) {
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

const Item* ComptimeEvaluator::resolve_value_item(ModuleId mid,
                                                  const Path* path) {
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

const ItemStruct* ComptimeEvaluator::resolve_struct(ModuleId mid,
                                                    const Path* path) {
    const Item* item = resolve_type_item(mid, path);
    if (!item || item->kind != AstNodeKind::ItemStruct) return nullptr;
    return static_cast<const ItemStruct*>(item);
}

const ItemEnum* ComptimeEvaluator::resolve_enum(ModuleId mid,
                                                const Path* path) {
    const Item* item = resolve_type_item(mid, path);
    if (!item || item->kind != AstNodeKind::ItemEnum) return nullptr;
    return static_cast<const ItemEnum*>(item);
}

const ItemStruct* ComptimeEvaluator::find_struct_global(std::string_view name) {
    for (const Module& m : crate_.modules) {
        auto it = m.types.find(std::string(name));
        if (it == m.types.end()) continue;
        if (it->second && it->second->kind == AstNodeKind::ItemStruct)
            return static_cast<const ItemStruct*>(it->second);
    }
    return nullptr;
}

const ItemEnum* ComptimeEvaluator::find_enum_in_builtin_module(
    std::string_view name) {
    if (crate_.root >= crate_.modules.size()) return nullptr;
    auto it = crate_.modules[crate_.root].submodules.find("builtin");
    if (it == crate_.modules[crate_.root].submodules.end()) return nullptr;
    ModuleId bid = it->second;
    if (bid >= crate_.modules.size()) return nullptr;
    auto tit = crate_.modules[bid].types.find(std::string(name));
    if (tit == crate_.modules[bid].types.end()) return nullptr;
    if (!tit->second || tit->second->kind != AstNodeKind::ItemEnum)
        return nullptr;
    return static_cast<const ItemEnum*>(tit->second);
}

ComptimeEvaluator::ResolvedVariant ComptimeEvaluator::resolve_variant(
    ModuleId mid, const Path* path) {
    if (!path || path->segments.size() < 2) return {};

    std::vector<Ident*> prefix_segs(path->segments.begin(),
                                    path->segments.end() - 1);
    Path prefix{path->span, std::move(prefix_segs)};
    const ItemEnum* def = resolve_enum(mid, &prefix);
    if (!def) return {};

    std::string_view vname = path->segments.back()->text;
    for (const VariantDecl* v : def->variants) {
        if (!v) continue;
        if (v->name == vname) return {.def = def, .variant = v};
    }
    return {.def = def, .variant = nullptr};
}

std::optional<ComptimeValue> ComptimeEvaluator::eval_const(const ItemConst* c) {
    if (!c) return std::nullopt;

    auto cached = const_cache_.find(c);
    if (cached != const_cache_.end()) {
        if (cached->second.state == CacheState::Done)
            return cached->second.value;
        error(c->span, "cyclic `const` evaluation");
        return std::nullopt;
    }

    const_cache_.insert({c, CachedValue{.state = CacheState::InProgress,
                                        .value = ComptimeValue::error()}});

    ModuleId mid = 0;
    if (auto it = const_module_.find(c); it != const_module_.end())
        mid = it->second;

    Env env{};
    auto flow = eval_expr_inner(mid, c->value, env, /*loop_depth=*/0);
    if (!flow) return std::nullopt;
    if (flow->control != Control::None) {
        error(c->span,
              "`break`/`continue`/`return` are not allowed in `const` "
              "initializers");
        return std::nullopt;
    }

    const_cache_[c] =
        CachedValue{.state = CacheState::Done, .value = std::move(flow->value)};
    return const_cache_[c].value;
}

std::optional<ComptimeValue> ComptimeEvaluator::eval_static(
    const ItemStatic* s) {
    if (!s) return std::nullopt;

    auto cached = static_cache_.find(s);
    if (cached != static_cache_.end()) {
        if (cached->second.state == CacheState::Done)
            return cached->second.value;
        error(s->span, "cyclic `static` evaluation");
        return std::nullopt;
    }

    static_cache_.insert({s, CachedValue{.state = CacheState::InProgress,
                                         .value = ComptimeValue::error()}});

    ModuleId mid = 0;
    if (auto it = static_module_.find(s); it != static_module_.end())
        mid = it->second;

    Env env{};
    auto flow = eval_expr_inner(mid, s->value, env, /*loop_depth=*/0);
    if (!flow) return std::nullopt;
    if (flow->control != Control::None) {
        error(s->span,
              "`break`/`continue`/`return` are not allowed in `static` "
              "initializers");
        return std::nullopt;
    }

    static_cache_[s] =
        CachedValue{.state = CacheState::Done, .value = std::move(flow->value)};
    return static_cache_[s].value;
}

std::optional<ComptimeValue> ComptimeEvaluator::eval_expr(ModuleId mid,
                                                          const Expr* expr) {
    Env env{};
    auto flow = eval_expr_inner(mid, expr, env, /*loop_depth=*/0);
    if (!flow) return std::nullopt;
    if (flow->control != Control::None) {
        error(expr ? expr->span : Span{},
              "top-level comptime expression must not "
              "`break`/`continue`/`return`");
        return std::nullopt;
    }
    return flow->value;
}

std::optional<std::uint64_t> ComptimeEvaluator::eval_usize(ModuleId mid,
                                                           const Expr* expr) {
    auto v = eval_expr(mid, expr);
    if (!v) return std::nullopt;
    if (v->kind != ComptimeValue::Kind::Int) {
        error(expr ? expr->span : Span{}, "expected integer comptime value");
        return std::nullopt;
    }
    if (v->int_value < 0) {
        error(expr ? expr->span : Span{},
              "expected non-negative integer comptime value");
        return std::nullopt;
    }
    return static_cast<std::uint64_t>(v->int_value);
}

std::optional<ComptimeValue> ComptimeEvaluator::eval_fn(
    const ItemFn* fn, std::vector<ComptimeValue> args, Span use_site) {
    if (!fn || !fn->decl || !fn->decl->sig) return std::nullopt;
    if (!fn->body) {
        error(use_site, "cannot call `extern` functions at comptime");
        return std::nullopt;
    }
    if (fn->decl->sig->is_variadic) {
        error(use_site, "variadic calls are not supported at comptime");
        return std::nullopt;
    }
    if (fn->decl->sig->params.size() != args.size()) {
        error(use_site, "comptime call arity mismatch");
        return std::nullopt;
    }

    ModuleId fn_mid = 0;
    if (auto it = fn_module_.find(fn); it != fn_module_.end())
        fn_mid = it->second;

    if (recursion_depth_ >= recursion_limit_) {
        error(use_site, "comptime recursion limit exceeded");
        return std::nullopt;
    }
    recursion_depth_++;

    Env env{};
    env.push_scope();
    for (size_t i = 0; i < fn->decl->sig->params.size(); i++) {
        const Param* p = fn->decl->sig->params[i];
        if (!p) continue;
        env.declare(p->name,
                    Binding{.value = std::move(args[i]), .is_mut = false});
    }

    auto body = eval_block(fn_mid, fn->body, env, /*loop_depth=*/0);
    recursion_depth_--;
    if (!body) return std::nullopt;
    if (body->control == Control::Return) return body->value;
    if (body->control != Control::None) {
        error(use_site,
              "internal error: unexpected control flow escaped a comptime "
              "function");
        return std::nullopt;
    }
    return body->value;
}

std::optional<ComptimeEvaluator::Flow> ComptimeEvaluator::eval_expr_inner(
    ModuleId mid, const Expr* expr, Env& env, int loop_depth) {
    if (!expr)
        return Flow{.control = Control::None, .value = ComptimeValue::unit()};
    if (!consume_step(expr->span)) return std::nullopt;

    switch (expr->kind) {
        case AstNodeKind::ExprUnit:
            return Flow{.control = Control::None,
                        .value = ComptimeValue::unit()};
        case AstNodeKind::ExprBool:
            return Flow{.control = Control::None,
                        .value = ComptimeValue::bool_(
                            static_cast<const ExprBool*>(expr)->value)};
        case AstNodeKind::ExprInt:
            return Flow{.control = Control::None,
                        .value = ComptimeValue::int_(
                            static_cast<const ExprInt*>(expr)->value)};
        case AstNodeKind::ExprFloat:
            return Flow{.control = Control::None,
                        .value = ComptimeValue::float_(
                            static_cast<const ExprFloat*>(expr)->value)};
        case AstNodeKind::ExprString: {
            auto* s = static_cast<const ExprString*>(expr);
            // Coarse comptime heap accounting: charge by byte length (plus a
            // small header).
            if (!consume_heap(
                    expr->span,
                    /*units=*/8 + static_cast<std::uint64_t>(s->value.size())))
                return std::nullopt;
            return Flow{.control = Control::None,
                        .value = ComptimeValue::string(s->value)};
        }

        case AstNodeKind::ExprPath: {
            auto* p = static_cast<const ExprPath*>(expr);
            if (!p->path || p->path->segments.empty()) return std::nullopt;

            // Local variables (single segment only).
            if (p->path->segments.size() == 1) {
                if (Binding* b = env.lookup(p->path->segments[0]->text)) {
                    return Flow{.control = Control::None, .value = b->value};
                }
            }

            // Const/static items.
            if (const Item* item = resolve_value_item(mid, p->path)) {
                if (item->kind == AstNodeKind::ItemConst) {
                    auto v = eval_const(static_cast<const ItemConst*>(item));
                    if (!v) return std::nullopt;
                    return Flow{.control = Control::None, .value = *v};
                }
                if (item->kind == AstNodeKind::ItemStatic) {
                    auto v = eval_static(static_cast<const ItemStatic*>(item));
                    if (!v) return std::nullopt;
                    return Flow{.control = Control::None, .value = *v};
                }
            }

            // Enum unit variant.
            ResolvedVariant rv = resolve_variant(mid, p->path);
            if (rv.def && rv.variant && rv.variant->payload.empty()) {
                if (!consume_heap(expr->span, /*units=*/4)) return std::nullopt;
                ComptimeValue v{};
                v.kind = ComptimeValue::Kind::Enum;
                v.enum_def = rv.def;
                v.enum_variant = rv.variant->name;
                return Flow{.control = Control::None, .value = std::move(v)};
            }

            // Type value (only in comptime evaluation).
            if (types_) {
                TypeId ty = lower_type_path(mid, p->path, /*allow_unsized=*/true);
                if (!types_->equal(ty, types_->error())) {
                    return Flow{.control = Control::None,
                                .value = ComptimeValue::type_(ty)};
                }
            }

            error(expr->span, "unresolved path in comptime expression");
            return std::nullopt;
        }

        case AstNodeKind::ExprTuple: {
            auto* t = static_cast<const ExprTuple*>(expr);
            if (!consume_heap(
                    expr->span,
                    /*units=*/8 + static_cast<std::uint64_t>(t->elems.size())))
                return std::nullopt;
            ComptimeValue out{};
            out.kind = ComptimeValue::Kind::Tuple;
            for (const Expr* e : t->elems) {
                auto ev = eval_expr_inner(mid, e, env, loop_depth);
                if (!ev || ev->control != Control::None) return std::nullopt;
                out.tuple_elems.push_back(std::move(ev->value));
            }
            return Flow{.control = Control::None, .value = std::move(out)};
        }

        case AstNodeKind::ExprArrayLit: {
            auto* a = static_cast<const ExprArrayLit*>(expr);
            if (!consume_heap(
                    expr->span,
                    /*units=*/8 + static_cast<std::uint64_t>(a->elems.size())))
                return std::nullopt;
            std::vector<ComptimeValue> elems{};
            elems.reserve(a->elems.size());
            for (const Expr* e : a->elems) {
                auto ev = eval_expr_inner(mid, e, env, loop_depth);
                if (!ev || ev->control != Control::None) return std::nullopt;
                elems.push_back(std::move(ev->value));
            }
            return Flow{.control = Control::None,
                        .value = ComptimeValue::array(std::move(elems))};
        }

        case AstNodeKind::ExprArrayRepeat: {
            auto* a = static_cast<const ExprArrayRepeat*>(expr);
            auto count_v = eval_expr_inner(mid, a->count, env, loop_depth);
            if (!count_v || count_v->control != Control::None)
                return std::nullopt;
            if (count_v->value.kind != ComptimeValue::Kind::Int) {
                error(expr->span,
                      "array repeat count must be an integer at comptime");
                return std::nullopt;
            }
            if (count_v->value.int_value < 0) {
                error(expr->span,
                      "array repeat count must be non-negative at comptime");
                return std::nullopt;
            }
            std::uint64_t n =
                static_cast<std::uint64_t>(count_v->value.int_value);

            auto elem_v = eval_expr_inner(mid, a->elem, env, loop_depth);
            if (!elem_v || elem_v->control != Control::None)
                return std::nullopt;

            if (!consume_heap(expr->span, /*units=*/8 + n)) return std::nullopt;
            std::vector<ComptimeValue> elems{};
            elems.reserve(static_cast<size_t>(n));
            for (std::uint64_t i = 0; i < n; i++) {
                elems.push_back(elem_v->value);
            }
            return Flow{.control = Control::None,
                        .value = ComptimeValue::array(std::move(elems))};
        }

        case AstNodeKind::ExprStructLit: {
            auto* lit = static_cast<const ExprStructLit*>(expr);
            const ItemStruct* def = resolve_struct(mid, lit->type_name);
            if (!def) {
                error(expr->span,
                      "unknown struct type in comptime struct literal");
                return std::nullopt;
            }

            if (!consume_heap(expr->span,
                              /*units=*/16 + static_cast<std::uint64_t>(
                                                 lit->inits.size())))
                return std::nullopt;

            std::unordered_map<std::string, const FieldDecl*> fields{};
            for (const FieldDecl* f : def->fields) {
                if (!f) continue;
                fields.insert({f->name, f});
            }

            ComptimeValue out{};
            out.kind = ComptimeValue::Kind::Struct;
            out.struct_def = def;

            for (const FieldInit* init : lit->inits) {
                if (!init) continue;
                if (!fields.contains(init->name)) {
                    error(init->span, "unknown field `" + init->name +
                                          "` in comptime struct literal");
                    return std::nullopt;
                }
                if (out.struct_fields.contains(init->name)) {
                    error(init->span,
                          "duplicate field initializer `" + init->name + "`");
                    return std::nullopt;
                }

                auto ev = eval_expr_inner(mid, init->value, env, loop_depth);
                if (!ev || ev->control != Control::None) return std::nullopt;
                out.struct_fields.insert({init->name, std::move(ev->value)});
            }

            for (const auto& [name, _] : fields) {
                if (!out.struct_fields.contains(name)) {
                    error(expr->span, "missing field `" + name +
                                          "` in comptime struct literal");
                    return std::nullopt;
                }
            }

            return Flow{.control = Control::None, .value = std::move(out)};
        }

        case AstNodeKind::ExprField: {
            auto* f = static_cast<const ExprField*>(expr);
            auto base = eval_expr_inner(mid, f->base, env, loop_depth);
            if (!base || base->control != Control::None) return std::nullopt;
            if (base->value.kind != ComptimeValue::Kind::Struct) {
                error(expr->span,
                      "field access in comptime requires a struct value");
                return std::nullopt;
            }
            auto it = base->value.struct_fields.find(f->field);
            if (it == base->value.struct_fields.end()) {
                error(expr->span, "unknown field `" + f->field +
                                      "` in comptime field access");
                return std::nullopt;
            }
            return Flow{.control = Control::None, .value = it->second};
        }

        case AstNodeKind::ExprBlock:
            return eval_block(mid, static_cast<const ExprBlock*>(expr)->block,
                              env, loop_depth);
        case AstNodeKind::ExprComptime:
            return eval_block(mid,
                              static_cast<const ExprComptime*>(expr)->block,
                              env, loop_depth);

        case AstNodeKind::ExprIf: {
            auto* iff = static_cast<const ExprIf*>(expr);
            auto cond = eval_expr_inner(mid, iff->cond, env, loop_depth);
            if (!cond || cond->control != Control::None) return std::nullopt;
            if (cond->value.kind != ComptimeValue::Kind::Bool) {
                error(iff->cond->span, "if condition must be bool at comptime");
                return std::nullopt;
            }
            if (cond->value.bool_value)
                return eval_block(mid, iff->then_block, env, loop_depth);
            if (iff->else_expr)
                return eval_expr_inner(mid, iff->else_expr, env, loop_depth);
            return Flow{.control = Control::None,
                        .value = ComptimeValue::unit()};
        }

        case AstNodeKind::ExprWhile: {
            auto* wh = static_cast<const ExprWhile*>(expr);
            while (true) {
                auto cond = eval_expr_inner(mid, wh->cond, env, loop_depth);
                if (!cond || cond->control != Control::None)
                    return std::nullopt;
                if (cond->value.kind != ComptimeValue::Kind::Bool) {
                    error(wh->cond->span,
                          "while condition must be bool at comptime");
                    return std::nullopt;
                }
                if (!cond->value.bool_value) break;

                auto body = eval_block(mid, wh->body, env, loop_depth + 1);
                if (!body) return std::nullopt;
                if (body->control == Control::None) continue;
                if (body->control == Control::Continue) continue;
                if (body->control == Control::Break) break;
                if (body->control == Control::Return) return body;
            }
            return Flow{.control = Control::None,
                        .value = ComptimeValue::unit()};
        }

        case AstNodeKind::ExprLoop: {
            auto* lp = static_cast<const ExprLoop*>(expr);
            while (true) {
                auto body = eval_block(mid, lp->body, env, loop_depth + 1);
                if (!body) return std::nullopt;
                if (body->control == Control::None) continue;
                if (body->control == Control::Continue) continue;
                if (body->control == Control::Break) break;
                if (body->control == Control::Return) return body;
            }
            return Flow{.control = Control::None,
                        .value = ComptimeValue::unit()};
        }

        case AstNodeKind::ExprMatch: {
            auto* m = static_cast<const ExprMatch*>(expr);
            auto scrut = eval_expr_inner(mid, m->scrutinee, env, loop_depth);
            if (!scrut || scrut->control != Control::None) return std::nullopt;

            for (const MatchArm* arm : m->arms) {
                if (!arm) continue;

                Env arm_env = env;
                arm_env.push_scope();
                bool matched =
                    match_pattern(mid, arm->pat, scrut->value, arm_env);
                if (!matched) {
                    arm_env.pop_scope();
                    continue;
                }

                if (arm->guard) {
                    auto g =
                        eval_expr_inner(mid, arm->guard, arm_env, loop_depth);
                    if (!g || g->control != Control::None) return std::nullopt;
                    if (g->value.kind != ComptimeValue::Kind::Bool) {
                        error(arm->guard->span,
                              "match guard must be bool at comptime");
                        return std::nullopt;
                    }
                    if (!g->value.bool_value) {
                        arm_env.pop_scope();
                        continue;
                    }
                }

                auto body =
                    eval_expr_inner(mid, arm->body, arm_env, loop_depth);
                arm_env.pop_scope();
                return body;
            }

            error(expr->span, "no matching `match` arm at comptime");
            return std::nullopt;
        }

        case AstNodeKind::ExprUnary: {
            auto* u = static_cast<const ExprUnary*>(expr);
            auto v = eval_expr_inner(mid, u->expr, env, loop_depth);
            if (!v || v->control != Control::None) return std::nullopt;
            switch (u->op) {
                case UnaryOp::Neg:
                    if (v->value.kind == ComptimeValue::Kind::Int) {
                        return Flow{
                            .control = Control::None,
                            .value = ComptimeValue::int_(-v->value.int_value)};
                    }
                    if (v->value.kind == ComptimeValue::Kind::Float) {
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::float_(
                                        -v->value.float_value)};
                    }
                    error(expr->span,
                          "unary `-` requires integer or float at comptime");
                    return std::nullopt;
                case UnaryOp::Not:
                    if (v->value.kind != ComptimeValue::Kind::Bool) {
                        error(expr->span,
                              "unary `!` requires bool at comptime");
                        return std::nullopt;
                    }
                    return Flow{
                        .control = Control::None,
                        .value = ComptimeValue::bool_(!v->value.bool_value)};
                case UnaryOp::BitNot: {
                    if (v->value.kind != ComptimeValue::Kind::Int) {
                        error(expr->span,
                              "unary `~` requires integer at comptime");
                        return std::nullopt;
                    }
                    std::uint64_t x =
                        static_cast<std::uint64_t>(v->value.int_value);
                    return Flow{.control = Control::None,
                                .value = ComptimeValue::int_(
                                    static_cast<std::int64_t>(~x))};
                }
                case UnaryOp::Deref:
                    error(
                        expr->span,
                        "pointer dereference is not supported at comptime yet");
                    return std::nullopt;
                case UnaryOp::AddrOf:
                case UnaryOp::AddrOfMut:
                    if (!u->expr || u->expr->kind != AstNodeKind::ExprPath) {
                        error(expr->span,
                              "comptime `&`/`&mut` is only supported on local "
                              "paths for now");
                        return std::nullopt;
                    }
                    {
                        const Path* p =
                            static_cast<const ExprPath*>(u->expr)->path;
                        if (!p || p->segments.size() != 1) {
                            error(expr->span,
                                  "comptime `&`/`&mut` is only supported on "
                                  "local paths for now");
                            return std::nullopt;
                        }
                        Binding* b = env.lookup(p->segments[0]->text);
                        if (!b) {
                            error(expr->span,
                                  "comptime `&`/`&mut` requires a local");
                            return std::nullopt;
                        }
                        if (u->op == UnaryOp::AddrOfMut && !b->is_mut) {
                            error(expr->span,
                                  "comptime `&mut` requires a mutable local");
                            return std::nullopt;
                        }
                        auto id = heap_alloc(expr->span, b->value);
                        if (!id) return std::nullopt;
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::ref_(*id)};
                    }
            }
            return std::nullopt;
        }

        case AstNodeKind::ExprBinary: {
            auto* b = static_cast<const ExprBinary*>(expr);

            // Short-circuit for bool ops.
            if (b->op == BinaryOp::And || b->op == BinaryOp::Or) {
                auto lhs = eval_expr_inner(mid, b->lhs, env, loop_depth);
                if (!lhs || lhs->control != Control::None) return std::nullopt;
                if (lhs->value.kind != ComptimeValue::Kind::Bool) {
                    error(expr->span,
                          "logical operators require bool at comptime");
                    return std::nullopt;
                }
                if (b->op == BinaryOp::And && !lhs->value.bool_value)
                    return Flow{.control = Control::None, .value = lhs->value};
                if (b->op == BinaryOp::Or && lhs->value.bool_value)
                    return Flow{.control = Control::None, .value = lhs->value};
                auto rhs = eval_expr_inner(mid, b->rhs, env, loop_depth);
                if (!rhs || rhs->control != Control::None) return std::nullopt;
                if (rhs->value.kind != ComptimeValue::Kind::Bool) {
                    error(expr->span,
                          "logical operators require bool at comptime");
                    return std::nullopt;
                }
                return Flow{.control = Control::None, .value = rhs->value};
            }

            auto lhs = eval_expr_inner(mid, b->lhs, env, loop_depth);
            auto rhs = eval_expr_inner(mid, b->rhs, env, loop_depth);
            if (!lhs || !rhs) return std::nullopt;
            if (lhs->control != Control::None || rhs->control != Control::None)
                return std::nullopt;

            if (lhs->value.kind == ComptimeValue::Kind::Int &&
                rhs->value.kind == ComptimeValue::Kind::Int) {
                std::int64_t a = lhs->value.int_value;
                std::int64_t c = rhs->value.int_value;
                const std::uint64_t au = static_cast<std::uint64_t>(a);
                const std::uint64_t cu = static_cast<std::uint64_t>(c);

                auto shift_amount = [&]() -> std::optional<unsigned> {
                    if (c < 0) return std::nullopt;
                    if (c >= 64) return std::nullopt;
                    return static_cast<unsigned>(c);
                };

                switch (b->op) {
                    case BinaryOp::Add:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::int_(a + c)};
                    case BinaryOp::Sub:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::int_(a - c)};
                    case BinaryOp::Mul:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::int_(a * c)};
                    case BinaryOp::Div:
                        if (c == 0) {
                            error(expr->span, "division by zero at comptime");
                            return std::nullopt;
                        }
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::int_(a / c)};
                    case BinaryOp::Mod:
                        if (c == 0) {
                            error(expr->span, "modulo by zero at comptime");
                            return std::nullopt;
                        }
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::int_(a % c)};
                    case BinaryOp::BitAnd:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::int_(
                                        static_cast<std::int64_t>(au & cu))};
                    case BinaryOp::BitOr:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::int_(
                                        static_cast<std::int64_t>(au | cu))};
                    case BinaryOp::BitXor:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::int_(
                                        static_cast<std::int64_t>(au ^ cu))};
                    case BinaryOp::Shl: {
                        auto sh = shift_amount();
                        if (!sh) {
                            error(expr->span,
                                  "invalid shift amount at comptime");
                            return std::nullopt;
                        }
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::int_(
                                        static_cast<std::int64_t>(au << *sh))};
                    }
                    case BinaryOp::Shr: {
                        auto sh = shift_amount();
                        if (!sh) {
                            error(expr->span,
                                  "invalid shift amount at comptime");
                            return std::nullopt;
                        }
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::int_(
                                        static_cast<std::int64_t>(au >> *sh))};
                    }
                    case BinaryOp::Eq:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::bool_(a == c)};
                    case BinaryOp::Ne:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::bool_(a != c)};
                    case BinaryOp::Lt:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::bool_(a < c)};
                    case BinaryOp::Le:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::bool_(a <= c)};
                    case BinaryOp::Gt:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::bool_(a > c)};
                    case BinaryOp::Ge:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::bool_(a >= c)};
                    case BinaryOp::And:
                    case BinaryOp::Or:
                        break;
                }
            }

            if (lhs->value.kind == ComptimeValue::Kind::Float &&
                rhs->value.kind == ComptimeValue::Kind::Float) {
                double a = lhs->value.float_value;
                double c = rhs->value.float_value;
                switch (b->op) {
                    case BinaryOp::Add:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::float_(a + c)};
                    case BinaryOp::Sub:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::float_(a - c)};
                    case BinaryOp::Mul:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::float_(a * c)};
                    case BinaryOp::Div:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::float_(a / c)};
                    case BinaryOp::Eq:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::bool_(a == c)};
                    case BinaryOp::Ne:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::bool_(a != c)};
                    case BinaryOp::Lt:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::bool_(a < c)};
                    case BinaryOp::Le:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::bool_(a <= c)};
                    case BinaryOp::Gt:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::bool_(a > c)};
                    case BinaryOp::Ge:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::bool_(a >= c)};
                    default:
                        break;
                }
            }

            if (lhs->value.kind == ComptimeValue::Kind::Bool &&
                rhs->value.kind == ComptimeValue::Kind::Bool) {
                bool a = lhs->value.bool_value;
                bool c = rhs->value.bool_value;
                switch (b->op) {
                    case BinaryOp::Eq:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::bool_(a == c)};
                    case BinaryOp::Ne:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::bool_(a != c)};
                    case BinaryOp::Lt:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::bool_(!a && c)};
                    case BinaryOp::Le:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::bool_(!a || c)};
                    case BinaryOp::Gt:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::bool_(a && !c)};
                    case BinaryOp::Ge:
                        return Flow{.control = Control::None,
                                    .value = ComptimeValue::bool_(a || !c)};
                    default:
                        break;
                }
            }

            error(expr->span,
                  "unsupported operand types for binary operator at comptime");
            return std::nullopt;
        }

        case AstNodeKind::ExprCast: {
            auto* c = static_cast<const ExprCast*>(expr);
            auto v = eval_expr_inner(mid, c->value, env, loop_depth);
            if (!v || v->control != Control::None) return std::nullopt;

            if (!c->to) return std::nullopt;
            switch (c->to->kind) {
                case AstNodeKind::TypePtr: {
                    // Allow int <-> ptr casts.
                    if (v->value.kind == ComptimeValue::Kind::Ptr) return v;
                    if (v->value.kind != ComptimeValue::Kind::Int) {
                        error(expr->span,
                              "cast to pointer requires integer at comptime");
                        return std::nullopt;
                    }
                    return Flow{
                        .control = Control::None,
                        .value = ComptimeValue::ptr_(
                            static_cast<std::uint64_t>(v->value.int_value))};
                }
                case AstNodeKind::TypePath: {
                    // Allow basic integer casts; treat as no-op on the numeric
                    // value.
                    if (v->value.kind == ComptimeValue::Kind::Ptr) {
                        return Flow{
                            .control = Control::None,
                            .value = ComptimeValue::int_(
                                static_cast<std::int64_t>(v->value.ptr_value))};
                    }
                    if (v->value.kind != ComptimeValue::Kind::Int) return v;
                    return v;
                }
                case AstNodeKind::TypeUnit:
                    return Flow{.control = Control::None,
                                .value = ComptimeValue::unit()};
                default:
                    error(expr->span, "unsupported cast at comptime");
                    return std::nullopt;
            }
        }

        case AstNodeKind::ExprAssign: {
            auto* a = static_cast<const ExprAssign*>(expr);
            if (!a->lhs) return std::nullopt;

            // `x = rhs`
            if (a->lhs->kind == AstNodeKind::ExprPath) {
                const Path* p = static_cast<const ExprPath*>(a->lhs)->path;
                if (!p || p->segments.size() != 1) {
                    error(expr->span,
                          "comptime assignment is only supported to locals for "
                          "now");
                    return std::nullopt;
                }
                Binding* b = env.lookup(p->segments[0]->text);
                if (!b) {
                    error(expr->span,
                          "assignment to unknown local at comptime");
                    return std::nullopt;
                }
                if (!b->is_mut) {
                    error(expr->span,
                          "assignment to immutable local at comptime");
                    return std::nullopt;
                }
                auto rhs = eval_expr_inner(mid, a->rhs, env, loop_depth);
                if (!rhs || rhs->control != Control::None) return std::nullopt;
                b->value = rhs->value;
                return Flow{.control = Control::None,
                            .value = ComptimeValue::unit()};
            }

            // `arr[i] = rhs` (v0.0.19: enough to build lookup tables).
            if (a->lhs->kind == AstNodeKind::ExprIndex) {
                auto* ix = static_cast<const ExprIndex*>(a->lhs);
                if (!ix->base || ix->base->kind != AstNodeKind::ExprPath) {
                    error(expr->span,
                          "comptime assignment is only supported to local "
                          "paths and local array indexing for now");
                    return std::nullopt;
                }
                const Path* p = static_cast<const ExprPath*>(ix->base)->path;
                if (!p || p->segments.size() != 1) {
                    error(expr->span,
                          "comptime assignment is only supported to local "
                          "paths and local array indexing for now");
                    return std::nullopt;
                }
                Binding* b = env.lookup(p->segments[0]->text);
                if (!b) {
                    error(expr->span,
                          "assignment to unknown local at comptime");
                    return std::nullopt;
                }
                if (!b->is_mut) {
                    error(expr->span,
                          "assignment to immutable local at comptime");
                    return std::nullopt;
                }
                if (b->value.kind != ComptimeValue::Kind::Array) {
                    error(
                        expr->span,
                        "indexed comptime assignment requires an array value");
                    return std::nullopt;
                }
                auto idx_v = eval_expr_inner(mid, ix->index, env, loop_depth);
                if (!idx_v || idx_v->control != Control::None)
                    return std::nullopt;
                if (idx_v->value.kind != ComptimeValue::Kind::Int) {
                    error(expr->span,
                          "indexed comptime assignment requires an integer "
                          "index");
                    return std::nullopt;
                }
                if (idx_v->value.int_value < 0) {
                    error(expr->span, "index must be non-negative at comptime");
                    return std::nullopt;
                }
                std::uint64_t idx =
                    static_cast<std::uint64_t>(idx_v->value.int_value);
                if (idx >= b->value.array_elems.size()) {
                    error(expr->span, "index out of bounds at comptime");
                    return std::nullopt;
                }

                auto rhs = eval_expr_inner(mid, a->rhs, env, loop_depth);
                if (!rhs || rhs->control != Control::None) return std::nullopt;
                b->value.array_elems[idx] = rhs->value;
                return Flow{.control = Control::None,
                            .value = ComptimeValue::unit()};
            }

            error(expr->span,
                  "unsupported assignment target at comptime (v0.0.x)");
            return std::nullopt;
        }

        case AstNodeKind::ExprCall: {
            auto* call = static_cast<const ExprCall*>(expr);
            if (!call->callee || call->callee->kind != AstNodeKind::ExprPath) {
                error(expr->span, "comptime calls require a path callee");
                return std::nullopt;
            }

            const Path* callee =
                static_cast<const ExprPath*>(call->callee)->path;
            if (!callee) return std::nullopt;

            if (path_is_builtin(callee, "size_of") ||
                path_is_builtin(callee, "align_of")) {
                if (call->args.size() != 1) {
                    error(expr->span,
                          "builtin::size_of/align_of expects 1 argument");
                    return std::nullopt;
                }
                if (!types_ || !layout_) {
                    error(expr->span,
                          "builtin::size_of/align_of are not available in this "
                          "comptime context");
                    return std::nullopt;
                }
                auto tv = eval_expr_inner(mid, call->args[0], env, loop_depth);
                if (!tv || tv->control != Control::None) return std::nullopt;
                if (tv->value.kind != ComptimeValue::Kind::Type) {
                    error(call->args[0] ? call->args[0]->span : expr->span,
                          "builtin::size_of/align_of expect a `type` value");
                    return std::nullopt;
                }
                TypeId ty = tv->value.type_value;
                const TypeData& td = types_->get(ty);
                if (td.kind == TypeKind::TypeType) {
                    error(expr->span,
                          "builtin::size_of/align_of require a runtime type");
                    return std::nullopt;
                }
                std::optional<std::uint64_t> v{};
                if (path_is_builtin(callee, "size_of"))
                    v = layout_->size_of(ty, expr->span);
                if (path_is_builtin(callee, "align_of"))
                    v = layout_->align_of(ty, expr->span);
                if (!v) return std::nullopt;
                if (*v > static_cast<std::uint64_t>(
                             std::numeric_limits<std::int64_t>::max())) {
                    error(expr->span, "comptime integer overflow");
                    return std::nullopt;
                }
                return Flow{.control = Control::None,
                            .value = ComptimeValue::int_(
                                static_cast<std::int64_t>(*v))};
            }

            if (path_is_builtin(callee, "type_info")) {
                if (call->args.size() != 1) {
                    error(expr->span, "builtin::type_info expects 1 argument");
                    return std::nullopt;
                }
                if (!types_ || !layout_) {
                    error(expr->span,
                          "builtin::type_info is not available in this "
                          "comptime context");
                    return std::nullopt;
                }
                if (!typeinfo_struct_ || !typekind_enum_) {
                    error(expr->span,
                          "internal error: missing builtin `TypeInfo`/`TypeKind`");
                    return std::nullopt;
                }
                auto tv = eval_expr_inner(mid, call->args[0], env, loop_depth);
                if (!tv || tv->control != Control::None) return std::nullopt;
                if (tv->value.kind != ComptimeValue::Kind::Type) {
                    error(call->args[0] ? call->args[0]->span : expr->span,
                          "builtin::type_info expects a `type` value");
                    return std::nullopt;
                }
                TypeId ty = tv->value.type_value;

                const TypeData& td = types_->get(ty);
                if (td.kind == TypeKind::TypeType) {
                    error(expr->span,
                          "builtin::type_info requires a runtime type");
                    return std::nullopt;
                }

                auto kind_name = [&]() -> std::optional<std::string> {
                    switch (td.kind) {
                        case TypeKind::Int:
                            return "Int";
                        case TypeKind::Float:
                            return "Float";
                        case TypeKind::Bool:
                            return "Bool";
                        case TypeKind::Unit:
                            return "Unit";
                        case TypeKind::Never:
                            return "Never";
                        case TypeKind::Ptr:
                            return "Ptr";
                        case TypeKind::Slice:
                            return "Slice";
                        case TypeKind::Array:
                            return "Array";
                        case TypeKind::Tuple:
                            return "Tuple";
                        case TypeKind::Struct:
                            return "Struct";
                        case TypeKind::Enum:
                            return "Enum";
                        case TypeKind::Fn:
                            return "Fn";
                        case TypeKind::Error:
                        case TypeKind::TypeType:
                        case TypeKind::Self:
                            return std::nullopt;
                    }
                    return std::nullopt;
                }();
                if (!kind_name) {
                    error(expr->span,
                          "builtin::type_info does not support this type kind");
                    return std::nullopt;
                }

                std::uint64_t sz = 0;
                std::uint64_t al = 0;
                if (types_->is_sized(ty)) {
                    auto sz_opt = layout_->size_of(ty, expr->span);
                    auto al_opt = layout_->align_of(ty, expr->span);
                    if (!sz_opt || !al_opt) return std::nullopt;
                    sz = *sz_opt;
                    al = *al_opt;
                }

                if (sz > static_cast<std::uint64_t>(
                             std::numeric_limits<std::int64_t>::max()) ||
                    al > static_cast<std::uint64_t>(
                             std::numeric_limits<std::int64_t>::max())) {
                    error(expr->span,
                          "comptime integer overflow in builtin::type_info");
                    return std::nullopt;
                }

                // Coarse heap accounting for the TypeInfo struct value.
                if (!consume_heap(expr->span, /*units=*/4)) return std::nullopt;

                ComptimeValue kind{};
                kind.kind = ComptimeValue::Kind::Enum;
                kind.enum_def = typekind_enum_;
                kind.enum_variant = *kind_name;

                ComptimeValue out{};
                out.kind = ComptimeValue::Kind::Struct;
                out.struct_def = typeinfo_struct_;
                out.struct_fields.insert({"kind", std::move(kind)});
                out.struct_fields.insert(
                    {"size",
                     ComptimeValue::int_(static_cast<std::int64_t>(sz))});
                out.struct_fields.insert(
                    {"align",
                     ComptimeValue::int_(static_cast<std::int64_t>(al))});
                return Flow{.control = Control::None, .value = std::move(out)};
            }

            if (path_is_builtin(callee, "addr_of") ||
                path_is_builtin(callee, "addr_of_mut")) {
                error(expr->span,
                      "builtin::addr_of was removed; use `&place` / `&mut "
                      "place`");
                return std::nullopt;
            }

            if (path_is_builtin(callee, "compile_error")) {
                if (call->args.size() != 1) {
                    error(expr->span,
                          "builtin::compile_error expects 1 argument");
                    return std::nullopt;
                }
                auto msgv =
                    eval_expr_inner(mid, call->args[0], env, loop_depth);
                if (!msgv || msgv->control != Control::None)
                    return std::nullopt;
                if (msgv->value.kind != ComptimeValue::Kind::String) {
                    error(call->args[0]->span,
                          "builtin::compile_error currently expects a string "
                          "literal");
                    return std::nullopt;
                }
                error(expr->span, msgv->value.string_value);
                return std::nullopt;
            }

            // ---- Type construction builtins (v0.0.23) ----
            auto require_types = [&]() -> bool {
                if (!types_) {
                    error(expr->span,
                          "type construction builtins are unavailable in this "
                          "comptime context");
                    return false;
                }
                return true;
            };

            auto require_type_ctx = [&]() -> bool {
                if (!type_ctx_.cache || !type_ctx_.arena || !type_ctx_.struct_info ||
                    !type_ctx_.enum_info) {
                    error(expr->span,
                          "internal error: missing type construction context");
                    return false;
                }
                return true;
            };

            auto as_type_value = [&](const Expr* e) -> std::optional<TypeId> {
                auto v = eval_expr_inner(mid, e, env, loop_depth);
                if (!v || v->control != Control::None) return std::nullopt;
                if (v->value.kind != ComptimeValue::Kind::Type) {
                    error(e ? e->span : expr->span,
                          "expected a `type` value");
                    return std::nullopt;
                }
                return v->value.type_value;
            };

            auto as_array_elems =
                [&](const ComptimeValue& v,
                    Span at) -> const std::vector<ComptimeValue>* {
                if (v.kind == ComptimeValue::Kind::Array) return &v.array_elems;
                if (v.kind == ComptimeValue::Kind::Ref) {
                    const ComptimeValue* p = heap_deref(at, v);
                    if (!p) return nullptr;
                    if (p->kind != ComptimeValue::Kind::Array) {
                        error(at, "expected a reference to an array value");
                        return nullptr;
                    }
                    return &p->array_elems;
                }
                error(at, "expected an array value or `&` reference to one");
                return nullptr;
            };

            auto cache_lookup = [&](std::string key) -> std::optional<TypeId> {
                if (!type_ctx_.cache) return std::nullopt;
                auto it = type_ctx_.cache->find(key);
                if (it == type_ctx_.cache->end()) return std::nullopt;
                return it->second;
            };

            auto cache_store = [&](std::string key, TypeId t) {
                if (!type_ctx_.cache) return;
                type_ctx_.cache->insert({std::move(key), t});
            };

            if (path_is_builtin(callee, "type_unit")) {
                if (call->args.size() != 0) {
                    error(expr->span, "builtin::type_unit expects 0 arguments");
                    return std::nullopt;
                }
                if (!require_types()) return std::nullopt;
                return Flow{.control = Control::None,
                            .value = ComptimeValue::type_(types_->unit())};
            }

            if (path_is_builtin(callee, "type_never")) {
                if (call->args.size() != 0) {
                    error(expr->span, "builtin::type_never expects 0 arguments");
                    return std::nullopt;
                }
                if (!require_types()) return std::nullopt;
                return Flow{.control = Control::None,
                            .value = ComptimeValue::type_(types_->never())};
            }

            if (path_is_builtin(callee, "type_ptr_const") ||
                path_is_builtin(callee, "type_ptr_mut")) {
                if (call->args.size() != 1) {
                    error(expr->span,
                          "builtin::type_ptr_* expects 1 argument");
                    return std::nullopt;
                }
                if (!require_types() || !require_type_ctx()) return std::nullopt;
                auto child = as_type_value(call->args[0]);
                if (!child) return std::nullopt;
                if (types_->get(*child).kind == TypeKind::TypeType) {
                    error(expr->span,
                          "cannot form a pointer to `type` (runtime-erased)");
                    return std::nullopt;
                }
                Mutability mut = path_is_builtin(callee, "type_ptr_mut")
                                     ? Mutability::Mut
                                     : Mutability::Const;
                std::string key =
                    "type_ptr:" + std::string(mut == Mutability::Mut ? "mut:" : "const:") +
                    type_key(*child);
                if (auto hit = cache_lookup(key))
                    return Flow{.control = Control::None,
                                .value = ComptimeValue::type_(*hit)};
                TypeId out = types_->ptr(mut, *child);
                cache_store(std::move(key), out);
                return Flow{.control = Control::None,
                            .value = ComptimeValue::type_(out)};
            }

            if (path_is_builtin(callee, "type_slice")) {
                if (call->args.size() != 1) {
                    error(expr->span, "builtin::type_slice expects 1 argument");
                    return std::nullopt;
                }
                if (!require_types() || !require_type_ctx()) return std::nullopt;
                auto child = as_type_value(call->args[0]);
                if (!child) return std::nullopt;
                const TypeData& cd = types_->get(*child);
                if (cd.kind == TypeKind::TypeType) {
                    error(expr->span, "cannot form a slice of `type`");
                    return std::nullopt;
                }
                if (!types_->is_sized(*child)) {
                    error(expr->span, "slice element type must be sized");
                    return std::nullopt;
                }
                std::string key = "type_slice:" + type_key(*child);
                if (auto hit = cache_lookup(key))
                    return Flow{.control = Control::None,
                                .value = ComptimeValue::type_(*hit)};
                TypeId out = types_->slice(*child);
                cache_store(std::move(key), out);
                return Flow{.control = Control::None,
                            .value = ComptimeValue::type_(out)};
            }

            if (path_is_builtin(callee, "type_array")) {
                if (call->args.size() != 2) {
                    error(expr->span, "builtin::type_array expects 2 arguments");
                    return std::nullopt;
                }
                if (!require_types() || !require_type_ctx()) return std::nullopt;
                auto child = as_type_value(call->args[0]);
                if (!child) return std::nullopt;
                if (types_->get(*child).kind == TypeKind::TypeType) {
                    error(expr->span, "cannot form an array of `type`");
                    return std::nullopt;
                }
                if (!types_->is_sized(*child)) {
                    error(expr->span, "array element type must be sized");
                    return std::nullopt;
                }
                auto n_v = eval_expr_inner(mid, call->args[1], env, loop_depth);
                if (!n_v || n_v->control != Control::None) return std::nullopt;
                if (n_v->value.kind != ComptimeValue::Kind::Int) {
                    error(call->args[1] ? call->args[1]->span : expr->span,
                          "array length must be an integer comptime value");
                    return std::nullopt;
                }
                if (n_v->value.int_value < 0) {
                    error(expr->span, "array length must be non-negative");
                    return std::nullopt;
                }
                std::uint64_t n = static_cast<std::uint64_t>(n_v->value.int_value);
                std::string key =
                    "type_array:" + type_key(*child) + ":" + std::to_string(n);
                if (auto hit = cache_lookup(key))
                    return Flow{.control = Control::None,
                                .value = ComptimeValue::type_(*hit)};
                TypeId out = types_->array_const(*child, n);
                cache_store(std::move(key), out);
                return Flow{.control = Control::None,
                            .value = ComptimeValue::type_(out)};
            }

            if (path_is_builtin(callee, "type_tuple")) {
                if (call->args.size() != 1) {
                    error(expr->span, "builtin::type_tuple expects 1 argument");
                    return std::nullopt;
                }
                if (!require_types() || !require_type_ctx()) return std::nullopt;
                auto ev = eval_expr_inner(mid, call->args[0], env, loop_depth);
                if (!ev || ev->control != Control::None) return std::nullopt;
                const auto* elems = as_array_elems(ev->value, expr->span);
                if (!elems) return std::nullopt;
                std::vector<TypeId> tys{};
                tys.reserve(elems->size());
                for (const ComptimeValue& tv : *elems) {
                    if (tv.kind != ComptimeValue::Kind::Type) {
                        error(expr->span,
                              "builtin::type_tuple expects an array of `type` "
                              "values");
                        return std::nullopt;
                    }
                    if (types_->get(tv.type_value).kind == TypeKind::TypeType) {
                        error(expr->span, "tuple elements must be runtime types");
                        return std::nullopt;
                    }
                    if (!types_->is_sized(tv.type_value)) {
                        error(expr->span,
                              "tuple element types must be sized (v0.1)");
                        return std::nullopt;
                    }
                    tys.push_back(tv.type_value);
                }
                std::ostringstream key_out;
                key_out << "type_tuple:";
                for (size_t i = 0; i < tys.size(); i++) {
                    if (i) key_out << ",";
                    key_out << type_key(tys[i]);
                }
                std::string key = key_out.str();
                if (auto hit = cache_lookup(key))
                    return Flow{.control = Control::None,
                                .value = ComptimeValue::type_(*hit)};
                TypeId out = types_->tuple(std::move(tys));
                cache_store(std::move(key), out);
                return Flow{.control = Control::None,
                            .value = ComptimeValue::type_(out)};
            }

            if (path_is_builtin(callee, "type_fn")) {
                if (call->args.size() != 2) {
                    error(expr->span, "builtin::type_fn expects 2 arguments");
                    return std::nullopt;
                }
                if (!require_types() || !require_type_ctx()) return std::nullopt;
                auto params_v =
                    eval_expr_inner(mid, call->args[0], env, loop_depth);
                if (!params_v || params_v->control != Control::None)
                    return std::nullopt;
                const auto* params =
                    as_array_elems(params_v->value, call->args[0]->span);
                if (!params) return std::nullopt;
                auto ret = as_type_value(call->args[1]);
                if (!ret) return std::nullopt;
                if (types_->get(*ret).kind == TypeKind::TypeType) {
                    error(expr->span,
                          "function return type must be a runtime type");
                    return std::nullopt;
                }
                if (!types_->is_sized(*ret)) {
                    error(expr->span,
                          "function return type must be sized in v0.1");
                    return std::nullopt;
                }
                std::vector<TypeId> pts{};
                pts.reserve(params->size());
                for (const ComptimeValue& tv : *params) {
                    if (tv.kind != ComptimeValue::Kind::Type) {
                        error(expr->span,
                              "builtin::type_fn expects an array of `type` "
                              "values for params");
                        return std::nullopt;
                    }
                    if (types_->get(tv.type_value).kind == TypeKind::TypeType) {
                        error(expr->span,
                              "function parameter types must be runtime types");
                        return std::nullopt;
                    }
                    if (!types_->is_sized(tv.type_value)) {
                        error(expr->span,
                              "function parameter types must be sized in v0.1");
                        return std::nullopt;
                    }
                    pts.push_back(tv.type_value);
                }
                std::ostringstream key_out;
                key_out << "type_fn(";
                for (size_t i = 0; i < pts.size(); i++) {
                    if (i) key_out << ",";
                    key_out << type_key(pts[i]);
                }
                key_out << ")->" << type_key(*ret);
                std::string key = key_out.str();
                if (auto hit = cache_lookup(key))
                    return Flow{.control = Control::None,
                                .value = ComptimeValue::type_(*hit)};
                TypeId out = types_->fn(std::move(pts), *ret);
                cache_store(std::move(key), out);
                return Flow{.control = Control::None,
                            .value = ComptimeValue::type_(out)};
            }

            if (path_is_builtin(callee, "type_struct")) {
                if (call->args.size() != 1) {
                    error(expr->span, "builtin::type_struct expects 1 argument");
                    return std::nullopt;
                }
                if (!require_types() || !require_type_ctx()) return std::nullopt;
                auto dv = eval_expr_inner(mid, call->args[0], env, loop_depth);
                if (!dv || dv->control != Control::None) return std::nullopt;
                if (dv->value.kind != ComptimeValue::Kind::Struct ||
                    !dv->value.struct_def) {
                    error(expr->span,
                          "builtin::type_struct expects a StructDesc value");
                    return std::nullopt;
                }

                std::unordered_set<std::uint64_t> visiting{};
                std::string key = "type_struct:" + value_key(dv->value, visiting);
                if (auto hit = cache_lookup(key))
                    return Flow{.control = Control::None,
                                .value = ComptimeValue::type_(*hit)};

                const ComptimeValue& desc = dv->value;
                auto it_name = desc.struct_fields.find("name");
                auto it_repr = desc.struct_fields.find("repr");
                auto it_fields = desc.struct_fields.find("fields");
                if (it_name == desc.struct_fields.end() ||
                    it_repr == desc.struct_fields.end() ||
                    it_fields == desc.struct_fields.end()) {
                    error(expr->span, "malformed StructDesc value");
                    return std::nullopt;
                }
                if (it_name->second.kind != ComptimeValue::Kind::String) {
                    error(expr->span, "StructDesc.name must be a string");
                    return std::nullopt;
                }
                if (it_repr->second.kind != ComptimeValue::Kind::Enum) {
                    error(expr->span, "StructDesc.repr must be an enum value");
                    return std::nullopt;
                }

                std::vector<Attr*> attrs{};
                if (it_repr->second.enum_variant == "Packed" ||
                    it_repr->second.enum_variant == "C") {
                    // struct[repr(packed)] / struct[repr(C)]
                    auto* repr_id =
                        type_ctx_.arena->make<Ident>(expr->span, "repr");
                    auto* arg_id =
                        type_ctx_.arena->make<Ident>(
                            expr->span,
                            it_repr->second.enum_variant == "Packed" ? "packed"
                                                                     : "C");
                    auto* repr_path =
                        type_ctx_.arena->make<Path>(expr->span,
                                                    std::vector<Ident*>{repr_id});
                    auto* arg_path =
                        type_ctx_.arena->make<Path>(
                            expr->span, std::vector<Ident*>{arg_id});
                    attrs.push_back(type_ctx_.arena->make<Attr>(
                        expr->span, repr_path, arg_path));
                }

                std::vector<FieldDecl*> field_asts{};
                StructInfo si{};
                const auto* field_vals =
                    as_array_elems(it_fields->second, expr->span);
                if (!field_vals) return std::nullopt;
                for (const ComptimeValue& fv : *field_vals) {
                    if (fv.kind != ComptimeValue::Kind::Struct || !fv.struct_def) {
                        error(expr->span, "StructDesc.fields must contain StructField values");
                        return std::nullopt;
                    }
                    auto fn_it = fv.struct_fields.find("name");
                    auto ft_it = fv.struct_fields.find("ty");
                    auto fv_it = fv.struct_fields.find("vis");
                    if (fn_it == fv.struct_fields.end() ||
                        ft_it == fv.struct_fields.end() ||
                        fv_it == fv.struct_fields.end()) {
                        error(expr->span, "malformed StructField value");
                        return std::nullopt;
                    }
                    if (fn_it->second.kind != ComptimeValue::Kind::String) {
                        error(expr->span, "StructField.name must be a string");
                        return std::nullopt;
                    }
                    if (ft_it->second.kind != ComptimeValue::Kind::Type) {
                        error(expr->span, "StructField.ty must be a `type` value");
                        return std::nullopt;
                    }
                    if (types_->get(ft_it->second.type_value).kind == TypeKind::TypeType) {
                        error(expr->span, "StructField.ty must be a runtime type");
                        return std::nullopt;
                    }
                    if (!types_->is_sized(ft_it->second.type_value)) {
                        error(expr->span, "StructField.ty must be sized");
                        return std::nullopt;
                    }
                    if (fv_it->second.kind != ComptimeValue::Kind::Enum) {
                        error(expr->span, "StructField.vis must be an enum value");
                        return std::nullopt;
                    }

                    Visibility fvis = Visibility::Private;
                    if (fv_it->second.enum_variant == "Pub") fvis = Visibility::Pub;
                    if (fv_it->second.enum_variant == "PubCrate")
                        fvis = Visibility::PubCrate;

                    std::string fname = fn_it->second.string_value;
                    if (si.fields.contains(fname)) {
                        error(expr->span, "duplicate field `" + fname + "` in constructed struct");
                        return std::nullopt;
                    }

                    field_asts.push_back(type_ctx_.arena->make<FieldDecl>(
                        expr->span, std::vector<Attr*>{}, fvis, fname,
                        /*type=*/nullptr));
                    si.fields_in_order.push_back(
                        StructInfo::Field{.name = fname, .type = ft_it->second.type_value});
                    si.fields.insert({fname, ft_it->second.type_value});
                }

                std::string sname = it_name->second.string_value;
                auto* def = type_ctx_.arena->make<ItemStruct>(
                    expr->span, std::move(attrs), Visibility::Private, sname,
                    std::move(field_asts));
                type_ctx_.struct_info->insert({def, std::move(si)});

                TypeId out = types_->struct_(def);
                cache_store(std::move(key), out);
                return Flow{.control = Control::None,
                            .value = ComptimeValue::type_(out)};
            }

            if (path_is_builtin(callee, "type_enum")) {
                if (call->args.size() != 1) {
                    error(expr->span, "builtin::type_enum expects 1 argument");
                    return std::nullopt;
                }
                if (!require_types() || !require_type_ctx()) return std::nullopt;
                auto dv = eval_expr_inner(mid, call->args[0], env, loop_depth);
                if (!dv || dv->control != Control::None) return std::nullopt;
                if (dv->value.kind != ComptimeValue::Kind::Struct ||
                    !dv->value.struct_def) {
                    error(expr->span,
                          "builtin::type_enum expects an EnumDesc value");
                    return std::nullopt;
                }

                std::unordered_set<std::uint64_t> visiting{};
                std::string key = "type_enum:" + value_key(dv->value, visiting);
                if (auto hit = cache_lookup(key))
                    return Flow{.control = Control::None,
                                .value = ComptimeValue::type_(*hit)};

                const ComptimeValue& desc = dv->value;
                auto it_name = desc.struct_fields.find("name");
                auto it_variants = desc.struct_fields.find("variants");
                auto it_tag = desc.struct_fields.find("tag_type");
                if (it_name == desc.struct_fields.end() ||
                    it_variants == desc.struct_fields.end() ||
                    it_tag == desc.struct_fields.end()) {
                    error(expr->span, "malformed EnumDesc value");
                    return std::nullopt;
                }
                if (it_name->second.kind != ComptimeValue::Kind::String) {
                    error(expr->span, "EnumDesc.name must be a string");
                    return std::nullopt;
                }

                std::optional<IntKind> tag_int{};
                std::unordered_map<std::string, std::int64_t> discriminants{};

                // Parse tag_type: MaybeType::{None, Some(type)}
                if (it_tag->second.kind != ComptimeValue::Kind::Enum ||
                    !it_tag->second.enum_def) {
                    error(expr->span, "EnumDesc.tag_type must be MaybeType");
                    return std::nullopt;
                }
                if (it_tag->second.enum_variant == "Some") {
                    if (it_tag->second.enum_payload.size() != 1 ||
                        it_tag->second.enum_payload[0].kind !=
                            ComptimeValue::Kind::Type) {
                        error(expr->span,
                              "EnumDesc.tag_type=Some expects a single `type` payload");
                        return std::nullopt;
                    }
                    TypeId tt = it_tag->second.enum_payload[0].type_value;
                    const TypeData& td = types_->get(tt);
                    if (td.kind != TypeKind::Int) {
                        error(expr->span,
                              "EnumDesc.tag_type must be an integer type");
                        return std::nullopt;
                    }
                    tag_int = td.int_kind;
                }

                std::string ename = it_name->second.string_value;

                auto* def = type_ctx_.arena->make<ItemEnum>(
                    expr->span, std::vector<Attr*>{}, Visibility::Private, ename,
                    std::vector<VariantDecl*>{});

                EnumInfo ei{};
                ei.tag_int = tag_int;

                const auto* vars =
                    as_array_elems(it_variants->second, expr->span);
                if (!vars) return std::nullopt;

                // Fieldless discriminant evaluation when tag_type is Some(int).
                // Note: the v0.1 target model requires `isize/usize` to track
                // the selected target. For now, assume a 64-bit host when
                // validating `tag(isize/usize)` in constructed enums.
                const std::uint32_t ptr_bits = 64;
                auto is_signed = [&](IntKind k) -> bool {
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
                };
                auto int_bits = [&](IntKind k) -> std::uint32_t {
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
                };
                auto fits = [&](std::int64_t v, IntKind k) -> bool {
                    std::uint32_t bits = int_bits(k);
                    if (bits >= 64) {
                        if (!is_signed(k)) return v >= 0;
                        return true;
                    }
                    if (is_signed(k)) {
                        const std::int64_t min = -(std::int64_t(1) << (bits - 1));
                        const std::int64_t max = (std::int64_t(1) << (bits - 1)) - 1;
                        return v >= min && v <= max;
                    }
                    if (v < 0) return false;
                    const std::uint64_t max = (std::uint64_t(1) << bits) - 1;
                    return static_cast<std::uint64_t>(v) <= max;
                };

                std::unordered_map<std::int64_t, std::string> seen_disc{};
                std::int64_t next_disc = 0;

                for (const ComptimeValue& vv : *vars) {
                    if (vv.kind != ComptimeValue::Kind::Struct || !vv.struct_def) {
                        error(expr->span,
                              "EnumDesc.variants must contain EnumVariant values");
                        return std::nullopt;
                    }
                    auto vn_it = vv.struct_fields.find("name");
                    auto vp_it = vv.struct_fields.find("payload");
                    auto vd_it = vv.struct_fields.find("discriminant");
                    if (vn_it == vv.struct_fields.end() ||
                        vp_it == vv.struct_fields.end() ||
                        vd_it == vv.struct_fields.end()) {
                        error(expr->span, "malformed EnumVariant value");
                        return std::nullopt;
                    }
                    if (vn_it->second.kind != ComptimeValue::Kind::String) {
                        error(expr->span, "EnumVariant.name must be a string");
                        return std::nullopt;
                    }
                    std::string vname = vn_it->second.string_value;
                    if (ei.variants.contains(vname)) {
                        error(expr->span,
                              "duplicate variant `" + vname + "` in constructed enum");
                        return std::nullopt;
                    }
                    const auto* payload_vals =
                        as_array_elems(vp_it->second, expr->span);
                    if (!payload_vals) return std::nullopt;

                    VariantInfo vi{};
                    std::vector<TypeId> payload_tys{};
                    payload_tys.reserve(payload_vals->size());
                    for (const ComptimeValue& tv : *payload_vals) {
                        if (tv.kind != ComptimeValue::Kind::Type) {
                            error(expr->span,
                                  "EnumVariant.payload must be an array of `type` values");
                            return std::nullopt;
                        }
                        if (types_->get(tv.type_value).kind == TypeKind::TypeType) {
                            error(expr->span,
                                  "enum payload types must be runtime types");
                            return std::nullopt;
                        }
                        if (!types_->is_sized(tv.type_value)) {
                            error(expr->span,
                                  "enum payload types must be sized");
                            return std::nullopt;
                        }
                        payload_tys.push_back(tv.type_value);
                    }

                    // If tag_int is present, enforce fieldless variants and compute discriminants.
                    if (tag_int) {
                        if (!payload_tys.empty()) {
                            error(expr->span,
                                  "tagged enums (tag_type=Some) must be fieldless");
                            return std::nullopt;
                        }
                        std::int64_t disc = next_disc;
                        if (vd_it->second.kind == ComptimeValue::Kind::Enum &&
                            vd_it->second.enum_variant == "Some") {
                            if (vd_it->second.enum_payload.size() != 1 ||
                                vd_it->second.enum_payload[0].kind !=
                                    ComptimeValue::Kind::Int) {
                                error(expr->span,
                                      "EnumVariant.discriminant=Some expects an integer payload");
                                return std::nullopt;
                            }
                            disc = vd_it->second.enum_payload[0].int_value;
                            next_disc = disc;
                        }
                        if (!fits(disc, *tag_int)) {
                            error(expr->span,
                                  "enum discriminant does not fit in the tag type");
                            return std::nullopt;
                        }
                        if (auto it = seen_disc.find(disc); it != seen_disc.end()) {
                            error(expr->span,
                                  "duplicate enum discriminant value (also used by `" +
                                      it->second + "`)");
                            return std::nullopt;
                        }
                        seen_disc.insert({disc, vname});
                        discriminants.insert({vname, disc});
                        if (next_disc == std::numeric_limits<std::int64_t>::max()) {
                            error(expr->span, "enum discriminant overflow");
                            return std::nullopt;
                        }
                        next_disc++;
                    }

                    // Construct a minimal AST VariantDecl (names + arity).
                    std::vector<Type*> payload_syntax(payload_tys.size(), nullptr);
                    auto* vdecl = type_ctx_.arena->make<VariantDecl>(
                        expr->span, vname, std::move(payload_syntax),
                        /*discriminant=*/nullptr);

                    vi.ast = vdecl;
                    vi.payload = std::move(payload_tys);

                    def->variants.push_back(vdecl);
                    ei.variants_in_order.push_back(vname);
                    ei.variants.insert({vname, std::move(vi)});
                }

                ei.discriminants = std::move(discriminants);
                type_ctx_.enum_info->insert({def, std::move(ei)});

                TypeId out = types_->enum_(def);
                cache_store(std::move(key), out);
                return Flow{.control = Control::None,
                            .value = ComptimeValue::type_(out)};
            }

            // Enum variant constructor `Enum::Variant(...)`.
            ResolvedVariant rv = resolve_variant(mid, callee);
            if (rv.def && rv.variant) {
                if (rv.variant->payload.size() != call->args.size()) {
                    error(expr->span,
                          "variant constructor arity mismatch at comptime");
                    return std::nullopt;
                }
                if (!consume_heap(expr->span,
                                  /*units=*/16 + static_cast<std::uint64_t>(
                                                     call->args.size())))
                    return std::nullopt;
                ComptimeValue out{};
                out.kind = ComptimeValue::Kind::Enum;
                out.enum_def = rv.def;
                out.enum_variant = rv.variant->name;
                for (const Expr* a : call->args) {
                    auto ev = eval_expr_inner(mid, a, env, loop_depth);
                    if (!ev || ev->control != Control::None)
                        return std::nullopt;
                    out.enum_payload.push_back(std::move(ev->value));
                }
                return Flow{.control = Control::None, .value = std::move(out)};
            }

            // Non-extern function call.
            const Item* item = resolve_value_item(mid, callee);
            if (!item || item->kind != AstNodeKind::ItemFn) {
                error(expr->span, "unresolved call target at comptime");
                return std::nullopt;
            }
            const ItemFn* fn = static_cast<const ItemFn*>(item);
            if (!fn->decl || !fn->decl->sig) return std::nullopt;
            if (!fn->body) {
                error(expr->span, "cannot call `extern` functions at comptime");
                return std::nullopt;
            }
            if (fn->decl->sig->is_variadic) {
                error(expr->span,
                      "variadic calls are not supported at comptime");
                return std::nullopt;
            }
            if (fn->decl->sig->params.size() != call->args.size()) {
                error(expr->span, "call arity mismatch at comptime");
                return std::nullopt;
            }

            std::vector<ComptimeValue> args{};
            args.reserve(call->args.size());
            for (const Expr* a : call->args) {
                auto ev = eval_expr_inner(mid, a, env, loop_depth);
                if (!ev || ev->control != Control::None) return std::nullopt;
                args.push_back(std::move(ev->value));
            }

            ModuleId fn_mid = mid;
            if (auto it = fn_module_.find(fn); it != fn_module_.end())
                fn_mid = it->second;

            if (recursion_depth_ >= recursion_limit_) {
                error(expr->span, "comptime recursion limit exceeded");
                return std::nullopt;
            }
            recursion_depth_++;

            Env fn_env{};
            fn_env.push_scope();
            for (size_t i = 0; i < fn->decl->sig->params.size(); i++) {
                const Param* p = fn->decl->sig->params[i];
                if (!p) continue;
                fn_env.declare(p->name, Binding{.value = std::move(args[i]),
                                                .is_mut = false});
            }

            auto body = eval_block(fn_mid, fn->body, fn_env, /*loop_depth=*/0);
            recursion_depth_--;
            if (!body) return std::nullopt;
            if (body->control == Control::Return)
                return Flow{.control = Control::None,
                            .value = std::move(body->value)};
            if (body->control != Control::None) {
                error(expr->span,
                      "internal error: unexpected control flow escaped a "
                      "comptime function");
                return std::nullopt;
            }
            return Flow{.control = Control::None,
                        .value = std::move(body->value)};
        }

        case AstNodeKind::ExprMethodCall:
            error(expr->span, "method calls at comptime are not supported yet");
            return std::nullopt;

        case AstNodeKind::ExprIndex: {
            auto* ix = static_cast<const ExprIndex*>(expr);
            auto base = eval_expr_inner(mid, ix->base, env, loop_depth);
            if (!base || base->control != Control::None) return std::nullopt;
            auto idx_v = eval_expr_inner(mid, ix->index, env, loop_depth);
            if (!idx_v || idx_v->control != Control::None) return std::nullopt;

            if (idx_v->value.kind != ComptimeValue::Kind::Int) {
                error(expr->span, "index must be an integer at comptime");
                return std::nullopt;
            }
            if (idx_v->value.int_value < 0) {
                error(expr->span, "index must be non-negative at comptime");
                return std::nullopt;
            }
            std::uint64_t idx =
                static_cast<std::uint64_t>(idx_v->value.int_value);

            if (base->value.kind != ComptimeValue::Kind::Array) {
                error(expr->span,
                      "indexing at comptime is only supported for arrays");
                return std::nullopt;
            }
            if (idx >= base->value.array_elems.size()) {
                error(expr->span, "index out of bounds at comptime");
                return std::nullopt;
            }
            return Flow{.control = Control::None,
                        .value = base->value.array_elems[idx]};
        }

        default:
            break;
    }

    error(expr->span, "unsupported comptime expression");
    return std::nullopt;
}

std::optional<ComptimeEvaluator::Flow> ComptimeEvaluator::eval_block(
    ModuleId mid, const Block* block, Env& env, int loop_depth) {
    if (!block)
        return Flow{.control = Control::None, .value = ComptimeValue::unit()};
    env.push_scope();
    for (const Stmt* s : block->stmts) {
        if (!s) continue;
        auto r = eval_stmt(mid, s, env, loop_depth);
        if (!r) {
            env.pop_scope();
            return std::nullopt;
        }
        if (r->control != Control::None) {
            env.pop_scope();
            return r;
        }
    }
    if (block->tail) {
        auto r = eval_expr_inner(mid, block->tail, env, loop_depth);
        env.pop_scope();
        return r;
    }
    env.pop_scope();
    return Flow{.control = Control::None, .value = ComptimeValue::unit()};
}

std::optional<ComptimeEvaluator::Flow> ComptimeEvaluator::eval_stmt(
    ModuleId mid, const Stmt* stmt, Env& env, int loop_depth) {
    if (!stmt)
        return Flow{.control = Control::None, .value = ComptimeValue::unit()};
    if (!consume_step(stmt->span)) return std::nullopt;

    switch (stmt->kind) {
        case AstNodeKind::StmtLet: {
            auto* s = static_cast<const StmtLet*>(stmt);
            if (!s->init) {
                error(stmt->span, "comptime `let` requires an initializer");
                return std::nullopt;
            }
            auto v = eval_expr_inner(mid, s->init, env, loop_depth);
            if (!v || v->control != Control::None) return std::nullopt;
            if (!bind_pattern(mid, s->pat, v->value, env)) return std::nullopt;
            return Flow{.control = Control::None,
                        .value = ComptimeValue::unit()};
        }
        case AstNodeKind::StmtExpr: {
            auto* s = static_cast<const StmtExpr*>(stmt);
            auto r = eval_expr_inner(mid, s->expr, env, loop_depth);
            if (!r) return std::nullopt;
            if (r->control != Control::None) return r;
            return Flow{.control = Control::None,
                        .value = ComptimeValue::unit()};
        }
        case AstNodeKind::StmtReturn: {
            auto* r = static_cast<const StmtReturn*>(stmt);
            if (r->value) {
                auto v = eval_expr_inner(mid, r->value, env, loop_depth);
                if (!v || v->control != Control::None) return std::nullopt;
                return Flow{.control = Control::Return,
                            .value = std::move(v->value)};
            }
            return Flow{.control = Control::Return,
                        .value = ComptimeValue::unit()};
        }
        case AstNodeKind::StmtBreak: {
            if (loop_depth <= 0) {
                error(stmt->span, "`break` outside of a loop at comptime");
                return std::nullopt;
            }
            auto* b = static_cast<const StmtBreak*>(stmt);
            if (b->value) {
                auto v = eval_expr_inner(mid, b->value, env, loop_depth);
                if (!v || v->control != Control::None) return std::nullopt;
                return Flow{.control = Control::Break,
                            .value = std::move(v->value)};
            }
            return Flow{.control = Control::Break,
                        .value = ComptimeValue::unit()};
        }
        case AstNodeKind::StmtContinue: {
            if (loop_depth <= 0) {
                error(stmt->span, "`continue` outside of a loop at comptime");
                return std::nullopt;
            }
            return Flow{.control = Control::Continue,
                        .value = ComptimeValue::unit()};
        }
        default:
            break;
    }
    error(stmt->span, "unsupported comptime statement");
    return std::nullopt;
}

bool ComptimeEvaluator::bind_pattern(ModuleId mid, const Pattern* pat,
                                     const ComptimeValue& v, Env& env) {
    if (!pat) return true;
    // `let` is intended to be irrefutable; treat mismatch as an error.
    Env tmp = env;
    if (!match_pattern(mid, pat, v, tmp)) {
        error(pat->span, "pattern does not match value at comptime");
        return false;
    }
    env = std::move(tmp);
    return true;
}

bool ComptimeEvaluator::match_pattern(ModuleId mid, const Pattern* pat,
                                      const ComptimeValue& v, Env& env) {
    if (!pat) return true;
    switch (pat->kind) {
        case AstNodeKind::PatWildcard:
            return true;
        case AstNodeKind::PatInt: {
            if (v.kind != ComptimeValue::Kind::Int) return false;
            return v.int_value == static_cast<const PatInt*>(pat)->value;
        }
        case AstNodeKind::PatBool: {
            if (v.kind != ComptimeValue::Kind::Bool) return false;
            return v.bool_value == static_cast<const PatBool*>(pat)->value;
        }
        case AstNodeKind::PatBinding: {
            auto* b = static_cast<const PatBinding*>(pat);
            env.declare(b->name, Binding{.value = v, .is_mut = b->is_mut});
            return true;
        }
        case AstNodeKind::PatTuple: {
            if (v.kind != ComptimeValue::Kind::Tuple) return false;
            auto* t = static_cast<const PatTuple*>(pat);
            if (t->elems.size() != v.tuple_elems.size()) return false;
            for (size_t i = 0; i < t->elems.size(); i++) {
                if (!match_pattern(mid, t->elems[i], v.tuple_elems[i], env))
                    return false;
            }
            return true;
        }
        case AstNodeKind::PatStruct: {
            if (v.kind != ComptimeValue::Kind::Struct) return false;
            auto* sp = static_cast<const PatStruct*>(pat);
            const ItemStruct* def = resolve_struct(mid, sp->type_name);
            if (!def || v.struct_def != def) return false;
            for (const PatField* f : sp->fields) {
                if (!f) continue;
                auto it = v.struct_fields.find(f->name);
                if (it == v.struct_fields.end()) return false;
                if (!match_pattern(mid, f->pat, it->second, env)) return false;
            }
            return true;
        }
        case AstNodeKind::PatVariant: {
            if (v.kind != ComptimeValue::Kind::Enum) return false;
            auto* vp = static_cast<const PatVariant*>(pat);
            ResolvedVariant rv = resolve_variant(mid, vp->path);
            if (!rv.def || !rv.variant) return false;
            if (v.enum_def != rv.def) return false;
            if (v.enum_variant != rv.variant->name) return false;
            if (vp->args.size() != v.enum_payload.size()) return false;
            for (size_t i = 0; i < vp->args.size(); i++) {
                if (!match_pattern(mid, vp->args[i], v.enum_payload[i], env))
                    return false;
            }
            return true;
        }
        case AstNodeKind::PatPath: {
            if (v.kind != ComptimeValue::Kind::Enum) return false;
            auto* pp = static_cast<const PatPath*>(pat);
            ResolvedVariant rv = resolve_variant(mid, pp->path);
            if (!rv.def || !rv.variant) return false;
            if (!rv.variant->payload.empty()) return false;
            return v.enum_def == rv.def && v.enum_variant == rv.variant->name;
        }
        case AstNodeKind::PatOr: {
            auto* o = static_cast<const PatOr*>(pat);
            if (pattern_has_binding(o)) {
                error(pat->span,
                      "bindings in `|` patterns are not supported at comptime "
                      "yet");
                return false;
            }
            Env tmp = env;
            if (match_pattern(mid, o->lhs, v, tmp)) {
                env = std::move(tmp);
                return true;
            }
            return match_pattern(mid, o->rhs, v, env);
        }
        default:
            break;
    }
    error(pat->span, "unsupported pattern at comptime");
    return false;
}

}  // namespace cog
