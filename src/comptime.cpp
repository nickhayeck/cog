#include "comptime.hpp"

#include "diag.hpp"
#include "layout.hpp"
#include "types.hpp"

#include <cstdint>
#include <limits>
#include <string>
#include <string_view>
#include <utility>

namespace cog {
namespace {

static bool path_is_builtin(const Path* path, std::string_view name) {
  if (!path || path->segments.size() != 2) return false;
  return path->segments[0]->text == "builtin" && path->segments[1]->text == name;
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

TypeId ComptimeEvaluator::lower_type(ModuleId mid, const Type* ty, bool allow_unsized) {
  if (!types_) {
    error(ty ? ty->span : Span{}, "internal error: type lowering is unavailable in this comptime context");
    return 0;
  }
  if (!ty) return types_->error();
  switch (ty->kind) {
    case AstNodeKind::TypePath:
      return lower_type_path(mid, static_cast<const TypePath*>(ty)->path, allow_unsized);
    case AstNodeKind::TypeType:
      return types_->type_type();
    case AstNodeKind::TypeUnit:
      return types_->unit();
    case AstNodeKind::TypeNever:
      return types_->never();
    case AstNodeKind::TypePtr: {
      auto* p = static_cast<const TypePtr*>(ty);
      return types_->ptr(p->mutability, lower_type(mid, p->pointee, /*allow_unsized=*/true));
    }
    case AstNodeKind::TypeSlice: {
      if (!allow_unsized) {
        error(ty->span, "unsized slice types are not allowed here");
        return types_->error();
      }
      auto* s = static_cast<const TypeSlice*>(ty);
      return types_->slice(lower_type(mid, s->elem, /*allow_unsized=*/false));
    }
    case AstNodeKind::TypeArray: {
      auto* a = static_cast<const TypeArray*>(ty);
      return types_->array(lower_type(mid, a->elem, /*allow_unsized=*/false), a->len);
    }
    case AstNodeKind::TypeTuple: {
      auto* t = static_cast<const TypeTuple*>(ty);
      std::vector<TypeId> elems{};
      elems.reserve(t->elems.size());
	      for (const Type* e : t->elems) elems.push_back(lower_type(mid, e, /*allow_unsized=*/false));
	      return types_->tuple(std::move(elems));
	    }
    default:
      error(ty->span, "unsupported type at comptime");
      return types_->error();
  }
}

TypeId ComptimeEvaluator::lower_type_path(ModuleId mid, const Path* path, bool allow_unsized) {
  if (!types_) {
    error(path ? path->span : Span{}, "internal error: type lowering is unavailable in this comptime context");
    return 0;
  }
  if (!path || path->segments.empty()) return types_->error();
  if (path->segments.size() == 1) {
    std::string_view name = path->segments[0]->text;
    if (name == "bool") return types_->bool_();
    if (auto ik = types_->parse_int_kind(name)) return types_->int_(*ik);
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

std::optional<TypeId> ComptimeEvaluator::lower_type_value_expr(ModuleId mid, const Expr* expr) {
  if (!expr) return std::nullopt;
  if (expr->kind != AstNodeKind::ExprPath) {
    error(expr->span, "expected a type value (path)");
    return std::nullopt;
  }
  const Path* p = static_cast<const ExprPath*>(expr)->path;
  return lower_type_path(mid, p, /*allow_unsized=*/false);
}

ComptimeEvaluator::ComptimeEvaluator(Session& session, const ResolvedCrate& crate, TypeStore* types, LayoutEngine* layout)
    : session_(session), crate_(crate), types_(types), layout_(layout) {
  for (ModuleId mid = 0; mid < crate_.modules.size(); mid++) {
    const Module& m = crate_.modules[mid];
    for (const Item* item : m.items) {
      if (!item) continue;
      if (item->kind == AstNodeKind::ItemConst) const_module_.insert({static_cast<const ItemConst*>(item), mid});
      if (item->kind == AstNodeKind::ItemStatic) static_module_.insert({static_cast<const ItemStatic*>(item), mid});
    }
  }
}

void ComptimeEvaluator::error(Span span, std::string message) {
  session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = span, .message = std::move(message)});
}

bool ComptimeEvaluator::consume_step(Span span) {
  if (step_budget_ == 0) {
    error(span, "comptime step budget exceeded");
    return false;
  }
  step_budget_--;
  return true;
}

ComptimeEvaluator::Binding* ComptimeEvaluator::Env::lookup(std::string_view name) {
  for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
    auto found = it->find(std::string(name));
    if (found != it->end()) return &found->second;
  }
  return nullptr;
}

void ComptimeEvaluator::Env::declare(std::string name, Binding b) {
  scopes.back().insert({std::move(name), std::move(b)});
}

const Item* ComptimeEvaluator::resolve_type_item(ModuleId mid, const Path* path) {
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

const Item* ComptimeEvaluator::resolve_value_item(ModuleId mid, const Path* path) {
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

const ItemStruct* ComptimeEvaluator::resolve_struct(ModuleId mid, const Path* path) {
  const Item* item = resolve_type_item(mid, path);
  if (!item || item->kind != AstNodeKind::ItemStruct) return nullptr;
  return static_cast<const ItemStruct*>(item);
}

const ItemEnum* ComptimeEvaluator::resolve_enum(ModuleId mid, const Path* path) {
  const Item* item = resolve_type_item(mid, path);
  if (!item || item->kind != AstNodeKind::ItemEnum) return nullptr;
  return static_cast<const ItemEnum*>(item);
}

ComptimeEvaluator::ResolvedVariant ComptimeEvaluator::resolve_variant(ModuleId mid, const Path* path) {
  if (!path || path->segments.size() < 2) return {};

  std::vector<Ident*> prefix_segs(path->segments.begin(), path->segments.end() - 1);
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
    if (cached->second.state == CacheState::Done) return cached->second.value;
    error(c->span, "cyclic `const` evaluation");
    return std::nullopt;
  }

  const_cache_.insert({c, CachedValue{.state = CacheState::InProgress, .value = ComptimeValue::error()}});

  ModuleId mid = 0;
  if (auto it = const_module_.find(c); it != const_module_.end()) mid = it->second;

  Env env{};
  auto flow = eval_expr_inner(mid, c->value, env, /*loop_depth=*/0);
  if (!flow) return std::nullopt;
  if (flow->control != Control::None) {
    error(c->span, "`break`/`continue`/`return` are not allowed in `const` initializers");
    return std::nullopt;
  }

  const_cache_[c] = CachedValue{.state = CacheState::Done, .value = std::move(flow->value)};
  return const_cache_[c].value;
}

std::optional<ComptimeValue> ComptimeEvaluator::eval_static(const ItemStatic* s) {
  if (!s) return std::nullopt;

  auto cached = static_cache_.find(s);
  if (cached != static_cache_.end()) {
    if (cached->second.state == CacheState::Done) return cached->second.value;
    error(s->span, "cyclic `static` evaluation");
    return std::nullopt;
  }

  static_cache_.insert({s, CachedValue{.state = CacheState::InProgress, .value = ComptimeValue::error()}});

  ModuleId mid = 0;
  if (auto it = static_module_.find(s); it != static_module_.end()) mid = it->second;

  Env env{};
  auto flow = eval_expr_inner(mid, s->value, env, /*loop_depth=*/0);
  if (!flow) return std::nullopt;
  if (flow->control != Control::None) {
    error(s->span, "`break`/`continue`/`return` are not allowed in `static` initializers");
    return std::nullopt;
  }

  static_cache_[s] = CachedValue{.state = CacheState::Done, .value = std::move(flow->value)};
  return static_cache_[s].value;
}

std::optional<ComptimeValue> ComptimeEvaluator::eval_expr(ModuleId mid, const Expr* expr) {
  Env env{};
  auto flow = eval_expr_inner(mid, expr, env, /*loop_depth=*/0);
  if (!flow) return std::nullopt;
  if (flow->control != Control::None) {
    error(expr ? expr->span : Span{}, "top-level comptime expression must not `break`/`continue`/`return`");
    return std::nullopt;
  }
  return flow->value;
}

std::optional<std::uint64_t> ComptimeEvaluator::eval_usize(ModuleId mid, const Expr* expr) {
  auto v = eval_expr(mid, expr);
  if (!v) return std::nullopt;
  if (v->kind != ComptimeValue::Kind::Int) {
    error(expr ? expr->span : Span{}, "expected integer comptime value");
    return std::nullopt;
  }
  if (v->int_value < 0) {
    error(expr ? expr->span : Span{}, "expected non-negative integer comptime value");
    return std::nullopt;
  }
  return static_cast<std::uint64_t>(v->int_value);
}

std::optional<ComptimeEvaluator::Flow> ComptimeEvaluator::eval_expr_inner(ModuleId mid, const Expr* expr, Env& env, int loop_depth) {
  if (!expr) return Flow{.control = Control::None, .value = ComptimeValue::unit()};
  if (!consume_step(expr->span)) return std::nullopt;

  switch (expr->kind) {
    case AstNodeKind::ExprUnit:
      return Flow{.control = Control::None, .value = ComptimeValue::unit()};
    case AstNodeKind::ExprBool:
      return Flow{.control = Control::None, .value = ComptimeValue::bool_(static_cast<const ExprBool*>(expr)->value)};
    case AstNodeKind::ExprInt:
      return Flow{.control = Control::None, .value = ComptimeValue::int_(static_cast<const ExprInt*>(expr)->value)};
    case AstNodeKind::ExprString:
      return Flow{.control = Control::None, .value = ComptimeValue::string(static_cast<const ExprString*>(expr)->value)};

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
        ComptimeValue v{};
        v.kind = ComptimeValue::Kind::Enum;
        v.enum_def = rv.def;
        v.enum_variant = rv.variant->name;
        return Flow{.control = Control::None, .value = std::move(v)};
      }

      error(expr->span, "unresolved path in comptime expression");
      return std::nullopt;
    }

    case AstNodeKind::ExprTuple: {
      auto* t = static_cast<const ExprTuple*>(expr);
      ComptimeValue out{};
      out.kind = ComptimeValue::Kind::Tuple;
      for (const Expr* e : t->elems) {
        auto ev = eval_expr_inner(mid, e, env, loop_depth);
        if (!ev || ev->control != Control::None) return std::nullopt;
        out.tuple_elems.push_back(std::move(ev->value));
      }
      return Flow{.control = Control::None, .value = std::move(out)};
    }

    case AstNodeKind::ExprStructLit: {
      auto* lit = static_cast<const ExprStructLit*>(expr);
      const ItemStruct* def = resolve_struct(mid, lit->type_name);
      if (!def) {
        error(expr->span, "unknown struct type in comptime struct literal");
        return std::nullopt;
      }

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
          error(init->span, "unknown field `" + init->name + "` in comptime struct literal");
          return std::nullopt;
        }
        if (out.struct_fields.contains(init->name)) {
          error(init->span, "duplicate field initializer `" + init->name + "`");
          return std::nullopt;
        }

        auto ev = eval_expr_inner(mid, init->value, env, loop_depth);
        if (!ev || ev->control != Control::None) return std::nullopt;
        out.struct_fields.insert({init->name, std::move(ev->value)});
      }

      for (const auto& [name, _] : fields) {
        if (!out.struct_fields.contains(name)) {
          error(expr->span, "missing field `" + name + "` in comptime struct literal");
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
        error(expr->span, "field access in comptime requires a struct value");
        return std::nullopt;
      }
      auto it = base->value.struct_fields.find(f->field);
      if (it == base->value.struct_fields.end()) {
        error(expr->span, "unknown field `" + f->field + "` in comptime field access");
        return std::nullopt;
      }
      return Flow{.control = Control::None, .value = it->second};
    }

    case AstNodeKind::ExprBlock:
      return eval_block(mid, static_cast<const ExprBlock*>(expr)->block, env, loop_depth);
    case AstNodeKind::ExprComptime:
      return eval_block(mid, static_cast<const ExprComptime*>(expr)->block, env, loop_depth);

    case AstNodeKind::ExprIf: {
      auto* iff = static_cast<const ExprIf*>(expr);
      auto cond = eval_expr_inner(mid, iff->cond, env, loop_depth);
      if (!cond || cond->control != Control::None) return std::nullopt;
      if (cond->value.kind != ComptimeValue::Kind::Bool) {
        error(iff->cond->span, "if condition must be bool at comptime");
        return std::nullopt;
      }
      if (cond->value.bool_value) return eval_block(mid, iff->then_block, env, loop_depth);
      if (iff->else_expr) return eval_expr_inner(mid, iff->else_expr, env, loop_depth);
      return Flow{.control = Control::None, .value = ComptimeValue::unit()};
    }

    case AstNodeKind::ExprWhile: {
      auto* wh = static_cast<const ExprWhile*>(expr);
      while (true) {
        auto cond = eval_expr_inner(mid, wh->cond, env, loop_depth);
        if (!cond || cond->control != Control::None) return std::nullopt;
        if (cond->value.kind != ComptimeValue::Kind::Bool) {
          error(wh->cond->span, "while condition must be bool at comptime");
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
      return Flow{.control = Control::None, .value = ComptimeValue::unit()};
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
      return Flow{.control = Control::None, .value = ComptimeValue::unit()};
    }

    case AstNodeKind::ExprMatch: {
      auto* m = static_cast<const ExprMatch*>(expr);
      auto scrut = eval_expr_inner(mid, m->scrutinee, env, loop_depth);
      if (!scrut || scrut->control != Control::None) return std::nullopt;

      for (const MatchArm* arm : m->arms) {
        if (!arm) continue;

        Env arm_env = env;
        arm_env.push_scope();
        bool matched = match_pattern(mid, arm->pat, scrut->value, arm_env);
        if (!matched) {
          arm_env.pop_scope();
          continue;
        }

        if (arm->guard) {
          auto g = eval_expr_inner(mid, arm->guard, arm_env, loop_depth);
          if (!g || g->control != Control::None) return std::nullopt;
          if (g->value.kind != ComptimeValue::Kind::Bool) {
            error(arm->guard->span, "match guard must be bool at comptime");
            return std::nullopt;
          }
          if (!g->value.bool_value) {
            arm_env.pop_scope();
            continue;
          }
        }

        auto body = eval_expr_inner(mid, arm->body, arm_env, loop_depth);
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
          if (v->value.kind != ComptimeValue::Kind::Int) {
            error(expr->span, "unary `-` requires integer at comptime");
            return std::nullopt;
          }
          return Flow{.control = Control::None, .value = ComptimeValue::int_(-v->value.int_value)};
        case UnaryOp::Not:
          if (v->value.kind != ComptimeValue::Kind::Bool) {
            error(expr->span, "unary `!` requires bool at comptime");
            return std::nullopt;
          }
          return Flow{.control = Control::None, .value = ComptimeValue::bool_(!v->value.bool_value)};
        case UnaryOp::Deref:
          error(expr->span, "pointer dereference is not supported at comptime yet");
          return std::nullopt;
        case UnaryOp::AddrOf:
        case UnaryOp::AddrOfMut:
          error(expr->span, "address-of is not supported at comptime yet");
          return std::nullopt;
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
          error(expr->span, "logical operators require bool at comptime");
          return std::nullopt;
        }
        if (b->op == BinaryOp::And && !lhs->value.bool_value) return Flow{.control = Control::None, .value = lhs->value};
        if (b->op == BinaryOp::Or && lhs->value.bool_value) return Flow{.control = Control::None, .value = lhs->value};
        auto rhs = eval_expr_inner(mid, b->rhs, env, loop_depth);
        if (!rhs || rhs->control != Control::None) return std::nullopt;
        if (rhs->value.kind != ComptimeValue::Kind::Bool) {
          error(expr->span, "logical operators require bool at comptime");
          return std::nullopt;
        }
        return Flow{.control = Control::None, .value = rhs->value};
      }

      auto lhs = eval_expr_inner(mid, b->lhs, env, loop_depth);
      auto rhs = eval_expr_inner(mid, b->rhs, env, loop_depth);
      if (!lhs || !rhs) return std::nullopt;
      if (lhs->control != Control::None || rhs->control != Control::None) return std::nullopt;

      if (lhs->value.kind != ComptimeValue::Kind::Int || rhs->value.kind != ComptimeValue::Kind::Int) {
        error(expr->span, "binary operators require integers at comptime (for now)");
        return std::nullopt;
      }

      std::int64_t a = lhs->value.int_value;
      std::int64_t c = rhs->value.int_value;
      switch (b->op) {
        case BinaryOp::Add:
          return Flow{.control = Control::None, .value = ComptimeValue::int_(a + c)};
        case BinaryOp::Sub:
          return Flow{.control = Control::None, .value = ComptimeValue::int_(a - c)};
        case BinaryOp::Mul:
          return Flow{.control = Control::None, .value = ComptimeValue::int_(a * c)};
        case BinaryOp::Div:
          if (c == 0) {
            error(expr->span, "division by zero at comptime");
            return std::nullopt;
          }
          return Flow{.control = Control::None, .value = ComptimeValue::int_(a / c)};
        case BinaryOp::Mod:
          if (c == 0) {
            error(expr->span, "modulo by zero at comptime");
            return std::nullopt;
          }
          return Flow{.control = Control::None, .value = ComptimeValue::int_(a % c)};
        case BinaryOp::Eq:
          return Flow{.control = Control::None, .value = ComptimeValue::bool_(a == c)};
        case BinaryOp::Ne:
          return Flow{.control = Control::None, .value = ComptimeValue::bool_(a != c)};
        case BinaryOp::Lt:
          return Flow{.control = Control::None, .value = ComptimeValue::bool_(a < c)};
        case BinaryOp::Le:
          return Flow{.control = Control::None, .value = ComptimeValue::bool_(a <= c)};
        case BinaryOp::Gt:
          return Flow{.control = Control::None, .value = ComptimeValue::bool_(a > c)};
        case BinaryOp::Ge:
          return Flow{.control = Control::None, .value = ComptimeValue::bool_(a >= c)};
        case BinaryOp::And:
        case BinaryOp::Or:
          break;
      }
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
            error(expr->span, "cast to pointer requires integer at comptime");
            return std::nullopt;
          }
          return Flow{.control = Control::None, .value = ComptimeValue::ptr_(static_cast<std::uint64_t>(v->value.int_value))};
        }
        case AstNodeKind::TypePath: {
          // Allow basic integer casts; treat as no-op on the numeric value.
          if (v->value.kind == ComptimeValue::Kind::Ptr) {
            return Flow{.control = Control::None, .value = ComptimeValue::int_(static_cast<std::int64_t>(v->value.ptr_value))};
          }
          if (v->value.kind != ComptimeValue::Kind::Int) return v;
          return v;
        }
        case AstNodeKind::TypeUnit:
          return Flow{.control = Control::None, .value = ComptimeValue::unit()};
        default:
          error(expr->span, "unsupported cast at comptime");
          return std::nullopt;
      }
    }

    case AstNodeKind::ExprAssign: {
      auto* a = static_cast<const ExprAssign*>(expr);
      if (!a->lhs || a->lhs->kind != AstNodeKind::ExprPath) {
        error(expr->span, "comptime assignment is only supported to locals for now");
        return std::nullopt;
      }
      const Path* p = static_cast<const ExprPath*>(a->lhs)->path;
      if (!p || p->segments.size() != 1) {
        error(expr->span, "comptime assignment is only supported to locals for now");
        return std::nullopt;
      }
      Binding* b = env.lookup(p->segments[0]->text);
      if (!b) {
        error(expr->span, "assignment to unknown local at comptime");
        return std::nullopt;
      }
      if (!b->is_mut) {
        error(expr->span, "assignment to immutable local at comptime");
        return std::nullopt;
      }
      auto rhs = eval_expr_inner(mid, a->rhs, env, loop_depth);
      if (!rhs || rhs->control != Control::None) return std::nullopt;
      b->value = rhs->value;
      return Flow{.control = Control::None, .value = ComptimeValue::unit()};
    }

    case AstNodeKind::ExprCall: {
      auto* call = static_cast<const ExprCall*>(expr);
      if (!call->callee || call->callee->kind != AstNodeKind::ExprPath) {
        error(expr->span, "comptime calls require a path callee");
        return std::nullopt;
      }

      const Path* callee = static_cast<const ExprPath*>(call->callee)->path;
      if (!callee) return std::nullopt;

      if (path_is_builtin(callee, "size_of") || path_is_builtin(callee, "align_of")) {
        if (call->args.size() != 1) {
          error(expr->span, "builtin::size_of/align_of expects 1 argument");
          return std::nullopt;
        }
        if (!types_ || !layout_) {
          error(expr->span, "builtin::size_of/align_of are not available in this comptime context");
          return std::nullopt;
        }
        auto ty = lower_type_value_expr(mid, call->args[0]);
        if (!ty) return std::nullopt;
        std::optional<std::uint64_t> v{};
        if (path_is_builtin(callee, "size_of")) v = layout_->size_of(*ty, expr->span);
        if (path_is_builtin(callee, "align_of")) v = layout_->align_of(*ty, expr->span);
        if (!v) return std::nullopt;
        if (*v > static_cast<std::uint64_t>(std::numeric_limits<std::int64_t>::max())) {
          error(expr->span, "comptime integer overflow");
          return std::nullopt;
        }
        return Flow{.control = Control::None, .value = ComptimeValue::int_(static_cast<std::int64_t>(*v))};
      }

      if (path_is_builtin(callee, "addr_of") || path_is_builtin(callee, "addr_of_mut")) {
        error(expr->span, "builtin::addr_of was removed; use `&place` / `&mut place` (not supported at comptime yet)");
        return std::nullopt;
      }

      if (path_is_builtin(callee, "compile_error")) {
        if (call->args.size() != 1) {
          error(expr->span, "builtin::compile_error expects 1 argument");
          return std::nullopt;
        }
        auto msgv = eval_expr_inner(mid, call->args[0], env, loop_depth);
        if (!msgv || msgv->control != Control::None) return std::nullopt;
        if (msgv->value.kind != ComptimeValue::Kind::String) {
          error(call->args[0]->span, "builtin::compile_error currently expects a string literal");
          return std::nullopt;
        }
        error(expr->span, msgv->value.string_value);
        return std::nullopt;
      }

      // Enum variant constructor `Enum::Variant(...)`.
      ResolvedVariant rv = resolve_variant(mid, callee);
      if (rv.def && rv.variant) {
        if (rv.variant->payload.size() != call->args.size()) {
          error(expr->span, "variant constructor arity mismatch at comptime");
          return std::nullopt;
        }
        ComptimeValue out{};
        out.kind = ComptimeValue::Kind::Enum;
        out.enum_def = rv.def;
        out.enum_variant = rv.variant->name;
        for (const Expr* a : call->args) {
          auto ev = eval_expr_inner(mid, a, env, loop_depth);
          if (!ev || ev->control != Control::None) return std::nullopt;
          out.enum_payload.push_back(std::move(ev->value));
        }
        return Flow{.control = Control::None, .value = std::move(out)};
      }

      error(expr->span, "function calls at comptime are not supported yet");
      return std::nullopt;
    }

    case AstNodeKind::ExprMethodCall:
      error(expr->span, "method calls at comptime are not supported yet");
      return std::nullopt;

    case AstNodeKind::ExprIndex:
      error(expr->span, "indexing at comptime is not supported yet");
      return std::nullopt;

    default:
      break;
  }

  error(expr->span, "unsupported comptime expression");
  return std::nullopt;
}

std::optional<ComptimeEvaluator::Flow> ComptimeEvaluator::eval_block(ModuleId mid, const Block* block, Env& env, int loop_depth) {
  if (!block) return Flow{.control = Control::None, .value = ComptimeValue::unit()};
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

std::optional<ComptimeEvaluator::Flow> ComptimeEvaluator::eval_stmt(ModuleId mid, const Stmt* stmt, Env& env, int loop_depth) {
  if (!stmt) return Flow{.control = Control::None, .value = ComptimeValue::unit()};
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
      return Flow{.control = Control::None, .value = ComptimeValue::unit()};
    }
    case AstNodeKind::StmtExpr: {
      auto* s = static_cast<const StmtExpr*>(stmt);
      auto r = eval_expr_inner(mid, s->expr, env, loop_depth);
      if (!r) return std::nullopt;
      if (r->control != Control::None) return r;
      return Flow{.control = Control::None, .value = ComptimeValue::unit()};
    }
    case AstNodeKind::StmtReturn: {
      auto* r = static_cast<const StmtReturn*>(stmt);
      if (r->value) {
        auto v = eval_expr_inner(mid, r->value, env, loop_depth);
        if (!v || v->control != Control::None) return std::nullopt;
        return Flow{.control = Control::Return, .value = std::move(v->value)};
      }
      return Flow{.control = Control::Return, .value = ComptimeValue::unit()};
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
        return Flow{.control = Control::Break, .value = std::move(v->value)};
      }
      return Flow{.control = Control::Break, .value = ComptimeValue::unit()};
    }
    case AstNodeKind::StmtContinue: {
      if (loop_depth <= 0) {
        error(stmt->span, "`continue` outside of a loop at comptime");
        return std::nullopt;
      }
      return Flow{.control = Control::Continue, .value = ComptimeValue::unit()};
    }
    default:
      break;
  }
  error(stmt->span, "unsupported comptime statement");
  return std::nullopt;
}

bool ComptimeEvaluator::bind_pattern(ModuleId mid, const Pattern* pat, const ComptimeValue& v, Env& env) {
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

bool ComptimeEvaluator::match_pattern(ModuleId mid, const Pattern* pat, const ComptimeValue& v, Env& env) {
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
        if (!match_pattern(mid, t->elems[i], v.tuple_elems[i], env)) return false;
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
        if (!match_pattern(mid, vp->args[i], v.enum_payload[i], env)) return false;
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
        error(pat->span, "bindings in `|` patterns are not supported at comptime yet");
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
