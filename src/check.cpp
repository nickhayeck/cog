#include "check.hpp"

#include "comptime.hpp"
#include "layout.hpp"
#include "sem.hpp"
#include "types.hpp"

#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

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

  void declare(std::string name, VarInfo info) { scopes.back().insert({std::move(name), info}); }
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
  Checker(Session& session, const ResolvedCrate& crate) : session_(session), crate_(crate) {}

  bool run() {
    predeclare_nominals();
    collect_type_layouts();
    collect_signatures();
    check_trait_impls();
    check_comptime();
    check_bodies();
    return !session_.has_errors();
  }

  CheckedCrate finish() && {
    CheckedCrate out{};
    out.types = std::move(types_);
    out.struct_info = std::move(struct_info_);
    out.enum_info = std::move(enum_info_);
    out.trait_methods = std::move(trait_methods_);
    out.const_types = std::move(const_types_);
    out.static_types = std::move(static_types_);
    out.fn_info = std::move(fn_info_);
    out.expr_types = std::move(expr_types_);
    out.array_lens = std::move(array_lens_);
    return out;
  }

 private:
  Session& session_;
  const ResolvedCrate& crate_;
  TypeStore types_;

  std::unordered_map<const ItemStruct*, StructInfo> struct_info_{};
  std::unordered_map<const ItemEnum*, EnumInfo> enum_info_{};
  std::unordered_map<const ItemTrait*, TraitMethodSet> trait_methods_{};
  std::unordered_map<const ItemConst*, TypeId> const_types_{};
  std::unordered_map<const ItemStatic*, TypeId> static_types_{};
  std::unordered_map<const ItemFn*, FnInfo> fn_info_{};
  std::unordered_map<const Expr*, TypeId> expr_types_{};
  std::unordered_map<const Expr*, std::uint64_t> array_lens_{};

  void error(Span span, std::string message) {
    session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = span, .message = std::move(message)});
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
          collect_struct_info(mid, static_cast<const ItemStruct*>(item));
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
        error(f->span, "duplicate field `" + f->name + "` in struct `" + s->name + "`");
        continue;
      }
      TypeId ty = lower_type(mid, f->type, /*allow_unsized=*/false, std::nullopt, /*allow_self=*/false);
      info.fields_in_order.push_back(StructInfo::Field{.name = f->name, .type = ty});
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
        error(v->span, "duplicate variant `" + v->name + "` in enum `" + e->name + "`");
        continue;
      }
      info.variants_in_order.push_back(v->name);
      VariantInfo vi{};
      vi.ast = v;
      for (const Type* pt : v->payload) {
        TypeId ty = lower_type(mid, pt, /*allow_unsized=*/false, std::nullopt, /*allow_self=*/false);
        vi.payload.push_back(ty);
      }
      info.variants.insert({v->name, std::move(vi)});
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
            TypeId ty = lower_type(mid, c->type, /*allow_unsized=*/false, std::nullopt, /*allow_self=*/false);
            const_types_.insert({c, ty});
            break;
          }
          case AstNodeKind::ItemStatic: {
            auto* s = static_cast<const ItemStatic*>(item);
            TypeId ty = lower_type(mid, s->type, /*allow_unsized=*/false, std::nullopt, /*allow_self=*/false);
            static_types_.insert({s, ty});
            break;
          }
          case AstNodeKind::ItemFn: {
            collect_fn_sig(mid, static_cast<const ItemFn*>(item), std::nullopt, /*allow_self=*/false);
            break;
          }
          case AstNodeKind::ItemTrait: {
            auto* tr = static_cast<const ItemTrait*>(item);
            collect_trait_method_set(mid, tr);
            break;
          }
          case AstNodeKind::ItemImplInherent: {
            auto* impl = static_cast<const ItemImplInherent*>(item);
            TypeId self_ty = lower_type_path(mid, impl->type_name, /*allow_unsized=*/false, std::nullopt, false);
            for (const ItemFn* mfn : impl->methods) collect_fn_sig(mid, mfn, self_ty, /*allow_self=*/true);
            break;
          }
          case AstNodeKind::ItemImplTrait: {
            auto* impl = static_cast<const ItemImplTrait*>(item);
            TypeId self_ty = lower_type_path(mid, impl->for_type_name, /*allow_unsized=*/false, std::nullopt, false);
            for (const ItemFn* mfn : impl->methods) collect_fn_sig(mid, mfn, self_ty, /*allow_self=*/true);
            break;
          }
          default:
            break;
        }
      }
    }
  }

  static std::string fmt_sig(const TypeStore& types, const FnInfo& f) {
    std::string out{};
    out += "(";
    for (size_t i = 0; i < f.params.size(); i++) {
      if (i) out += ", ";
      out += types.to_string(f.params[i]);
    }
    out += ") -> ";
    out += types.to_string(f.ret);
    return out;
  }

  bool type_contains_self(TypeId t) const {
    const TypeData& d = types_.get(t);
    switch (d.kind) {
      case TypeKind::Self:
        return true;
      case TypeKind::Ptr:
        return type_contains_self(d.pointee);
      case TypeKind::Slice:
        return type_contains_self(d.elem);
      case TypeKind::Array:
        return type_contains_self(d.elem);
      case TypeKind::Tuple:
        for (TypeId e : d.tuple_elems) {
          if (type_contains_self(e)) return true;
        }
        return false;
      case TypeKind::Error:
      case TypeKind::Unit:
      case TypeKind::Bool:
      case TypeKind::Int:
      case TypeKind::TypeType:
      case TypeKind::Struct:
      case TypeKind::Enum:
      case TypeKind::DynTrait:
        return false;
    }
    return false;
  }

  TypeId substitute_self(TypeId t, TypeId self_ty) {
    const TypeData& d = types_.get(t);
    switch (d.kind) {
      case TypeKind::Self:
        return self_ty;
      case TypeKind::Ptr:
        return types_.ptr(d.mutability, substitute_self(d.pointee, self_ty));
      case TypeKind::Slice:
        return types_.slice(substitute_self(d.elem, self_ty));
      case TypeKind::Array:
        return types_.array(substitute_self(d.elem, self_ty), d.array_len_expr);
      case TypeKind::Tuple: {
        std::vector<TypeId> elems{};
        elems.reserve(d.tuple_elems.size());
        for (TypeId e : d.tuple_elems) elems.push_back(substitute_self(e, self_ty));
        return types_.tuple(std::move(elems));
      }
      case TypeKind::Error:
      case TypeKind::Unit:
      case TypeKind::Bool:
      case TypeKind::Int:
      case TypeKind::TypeType:
      case TypeKind::Struct:
      case TypeKind::Enum:
      case TypeKind::DynTrait:
        return t;
    }
    return t;
  }

  void collect_trait_method_set(ModuleId mid, const ItemTrait* tr) {
    if (!tr) return;
    if (trait_methods_.contains(tr)) return;

    TraitMethodSet set{};
    std::unordered_set<std::string> seen{};

    for (const FnDecl* decl : tr->methods) {
      if (!decl || !decl->sig) continue;
      if (seen.contains(decl->name)) {
        error(decl->span, "duplicate method `" + decl->name + "` in trait `" + tr->name + "`");
        continue;
      }
      seen.insert(decl->name);
      set.order.push_back(decl->name);

      FnInfo sig = lower_fn_sig(mid, decl->sig, std::nullopt, /*allow_self=*/true);

      bool object_safe = true;
      if (sig.params.empty()) {
        object_safe = false;
      } else {
        const TypeData& self_param = types_.get(sig.params[0]);
        if (self_param.kind != TypeKind::Ptr) {
          object_safe = false;
        } else if (types_.get(self_param.pointee).kind != TypeKind::Self) {
          object_safe = false;
        }
      }
      for (size_t i = 1; i < sig.params.size() && object_safe; i++) {
        if (type_contains_self(sig.params[i])) object_safe = false;
      }
      if (object_safe && type_contains_self(sig.ret)) object_safe = false;

      set.methods.insert({decl->name, TraitMethodInfo{.sig = std::move(sig), .object_safe = object_safe}});
    }

    trait_methods_.insert({tr, std::move(set)});
  }

  void check_trait_impls() {
    for (ModuleId mid = 0; mid < crate_.modules.size(); mid++) {
      const Module& m = crate_.modules[mid];
      for (Item* item : m.items) {
        if (!item) continue;
        if (item->kind != AstNodeKind::ItemImplTrait) continue;
        auto* impl = static_cast<const ItemImplTrait*>(item);

        const Item* trait_item = resolve_type_item(mid, impl->trait_name);
        if (!trait_item || trait_item->kind != AstNodeKind::ItemTrait) {
          error(impl->trait_name ? impl->trait_name->span : impl->span, "unknown trait in impl");
          continue;
        }
        auto* tr = static_cast<const ItemTrait*>(trait_item);
        auto ts_it = trait_methods_.find(tr);
        if (ts_it == trait_methods_.end()) continue;
        const TraitMethodSet& tset = ts_it->second;

        TypeId self_ty = lower_type_path(mid, impl->for_type_name, /*allow_unsized=*/false, std::nullopt, false);
        const TypeData& sd = types_.get(self_ty);
        if (sd.kind != TypeKind::Struct && sd.kind != TypeKind::Enum) {
          error(impl->for_type_name ? impl->for_type_name->span : impl->span, "trait impl target must be a nominal type");
          continue;
        }

        std::unordered_map<std::string, const ItemFn*> impl_methods{};
        for (const ItemFn* mfn : impl->methods) {
          if (!mfn || !mfn->decl) continue;
          if (impl_methods.contains(mfn->decl->name)) {
            error(mfn->span, "duplicate method `" + mfn->decl->name + "` in trait impl");
            continue;
          }
          impl_methods.insert({mfn->decl->name, mfn});
        }

        // No extra methods.
        for (const auto& [name, mfn] : impl_methods) {
          if (!tset.methods.contains(name)) {
            error(mfn->span, "method `" + name + "` is not a member of trait `" + tr->name + "`");
          }
        }

        // All required methods.
        for (const auto& [name, tm] : tset.methods) {
          auto it = impl_methods.find(name);
          if (it == impl_methods.end()) {
            error(impl->span, "missing method `" + name + "` in impl of `" + tr->name + "`");
            continue;
          }

          auto fi = fn_info_.find(it->second);
          if (fi == fn_info_.end()) continue;
          const FnInfo& impl_sig = fi->second;
          const FnInfo& trait_sig = tm.sig;

          if (impl_sig.params.size() != trait_sig.params.size()) {
            error(it->second->span, "signature mismatch for `" + name + "`: expected " + fmt_sig(types_, trait_sig) + ", got " +
                                       fmt_sig(types_, impl_sig));
            continue;
          }

          bool ok = true;
          for (size_t i = 0; i < impl_sig.params.size(); i++) {
            TypeId expected = substitute_self(trait_sig.params[i], self_ty);
            if (!types_.equal(impl_sig.params[i], expected)) ok = false;
          }
          TypeId expected_ret = substitute_self(trait_sig.ret, self_ty);
          if (!types_.equal(impl_sig.ret, expected_ret)) ok = false;

          if (!ok) {
            error(it->second->span, "signature mismatch for `" + name + "`: expected " + fmt_sig(types_, trait_sig) + ", got " +
                                       fmt_sig(types_, impl_sig));
          }
        }
      }
    }
  }

  void collect_fn_sig(ModuleId mid, const ItemFn* fn, std::optional<TypeId> self_ty, bool allow_self) {
    if (!fn || !fn->decl || !fn->decl->sig) return;
    fn_info_.insert({fn, lower_fn_sig(mid, fn->decl->sig, self_ty, allow_self)});
  }

  FnInfo lower_fn_sig(ModuleId mid, const FnSig* sig, std::optional<TypeId> self_ty, bool allow_self) {
    FnInfo out{};
    out.ret = sig->ret ? lower_type(mid, sig->ret, /*allow_unsized=*/false, self_ty, allow_self) : types_.unit();
    for (const Param* p : sig->params) {
      if (!p) continue;
      TypeId pt = lower_type(mid, p->type, /*allow_unsized=*/false, self_ty, allow_self);
      out.params.push_back(pt);
    }
    return out;
  }

  TypeId lower_type(ModuleId mid, const Type* ty, bool allow_unsized, std::optional<TypeId> self_ty, bool allow_self) {
    if (!ty) return types_.error();
    switch (ty->kind) {
      case AstNodeKind::TypeUnit:
        return types_.unit();
      case AstNodeKind::TypeType:
        return types_.type_type();
      case AstNodeKind::TypePath:
        return lower_type_path(mid, static_cast<const TypePath*>(ty)->path, allow_unsized, self_ty, allow_self);
      case AstNodeKind::TypePtr: {
        auto* p = static_cast<const TypePtr*>(ty);
        TypeId pointee = lower_type(mid, p->pointee, /*allow_unsized=*/true, self_ty, allow_self);
        return types_.ptr(p->mutability, pointee);
      }
      case AstNodeKind::TypeSlice: {
        if (!allow_unsized) error(ty->span, "slice types must appear behind a pointer");
        TypeId elem = lower_type(mid, static_cast<const TypeSlice*>(ty)->elem, /*allow_unsized=*/false, self_ty, allow_self);
        return types_.slice(elem);
      }
      case AstNodeKind::TypeArray: {
        auto* a = static_cast<const TypeArray*>(ty);
        TypeId elem = lower_type(mid, a->elem, /*allow_unsized=*/false, self_ty, allow_self);
        return types_.array(elem, a->len);
      }
      case AstNodeKind::TypeTuple: {
        auto* t = static_cast<const TypeTuple*>(ty);
        std::vector<TypeId> elems;
        for (const Type* e : t->elems) elems.push_back(lower_type(mid, e, /*allow_unsized=*/false, self_ty, allow_self));
        return types_.tuple(std::move(elems));
      }
      case AstNodeKind::TypeDyn: {
        if (!allow_unsized) error(ty->span, "`dyn Trait` must appear behind a pointer");
        const Item* tr_item = resolve_type_item(mid, static_cast<const TypeDyn*>(ty)->trait);
        if (!tr_item || tr_item->kind != AstNodeKind::ItemTrait) {
          error(ty->span, "unknown trait in `dyn` type");
          return types_.error();
        }
        return types_.dyn_trait(static_cast<const ItemTrait*>(tr_item));
      }
      default:
        break;
    }
    return types_.error();
  }

  const Item* resolve_type_item(ModuleId mid, const Path* path) {
    if (!path || path->segments.empty()) return nullptr;
    ModuleId cur = mid;
    for (size_t i = 0; i + 1 < path->segments.size(); i++) {
      const Ident* seg = path->segments[i];
      auto it = crate_.modules[cur].submodules.find(seg->text);
      if (it == crate_.modules[cur].submodules.end()) {
        error(seg->span, "cannot find module `" + seg->text + "`");
        return nullptr;
      }
      cur = it->second;
    }
    const Ident* last = path->segments.back();
    auto it = crate_.modules[cur].types.find(last->text);
    if (it == crate_.modules[cur].types.end()) {
      error(last->span, "cannot find type `" + last->text + "`");
      return nullptr;
    }
    return it->second;
  }

  TypeId lower_type_path(ModuleId mid, const Path* path, bool allow_unsized, std::optional<TypeId> self_ty, bool allow_self) {
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
    }

    const Item* item = resolve_type_item(mid, path);
    if (!item) return types_.error();

    switch (item->kind) {
      case AstNodeKind::ItemStruct:
        return types_.struct_(static_cast<const ItemStruct*>(item));
      case AstNodeKind::ItemEnum:
        return types_.enum_(static_cast<const ItemEnum*>(item));
      case AstNodeKind::ItemTypeAlias: {
        auto* ta = static_cast<const ItemTypeAlias*>(item);
        return lower_type(mid, ta->aliased, allow_unsized, self_ty, allow_self);
      }
      case AstNodeKind::ItemTrait:
        error(path->span, "traits are only usable via `dyn Trait`");
        return types_.error();
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
    return lower_type_path(mid, p, /*allow_unsized=*/false, std::nullopt, /*allow_self=*/false);
  }

  void check_bodies() {
    for (ModuleId mid = 0; mid < crate_.modules.size(); mid++) {
      const Module& m = crate_.modules[mid];
      for (Item* item : m.items) {
        if (!item) continue;
        switch (item->kind) {
          case AstNodeKind::ItemFn:
            check_fn_body(mid, static_cast<const ItemFn*>(item), std::nullopt);
            break;
          case AstNodeKind::ItemImplInherent: {
            auto* impl = static_cast<const ItemImplInherent*>(item);
            TypeId self_ty = lower_type_path(mid, impl->type_name, false, std::nullopt, false);
            for (const ItemFn* mfn : impl->methods) check_fn_body(mid, mfn, self_ty);
            break;
          }
          case AstNodeKind::ItemImplTrait: {
            auto* impl = static_cast<const ItemImplTrait*>(item);
            TypeId self_ty = lower_type_path(mid, impl->for_type_name, false, std::nullopt, false);
            for (const ItemFn* mfn : impl->methods) check_fn_body(mid, mfn, self_ty);
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

    LayoutEngine layout(session_, types_, struct_info_, enum_info_, array_lens_);
    ComptimeEvaluator eval(session_, crate_, &types_, &layout);

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
              for (const Type* t : v->payload) check_type_arrays(mid, t, eval);
            }
            break;
          }
          case AstNodeKind::ItemFn: {
            auto* fn = static_cast<const ItemFn*>(item);
            if (fn->decl && fn->decl->sig) {
              for (const Param* p : fn->decl->sig->params) {
                if (p) check_type_arrays(mid, p->type, eval);
              }
              if (fn->decl->sig->ret) check_type_arrays(mid, fn->decl->sig->ret, eval);
            }
            break;
          }
          case AstNodeKind::ItemTrait: {
            auto* tr = static_cast<const ItemTrait*>(item);
            for (const FnDecl* decl : tr->methods) {
              if (!decl || !decl->sig) continue;
              for (const Param* p : decl->sig->params) {
                if (p) check_type_arrays(mid, p->type, eval);
              }
              if (decl->sig->ret) check_type_arrays(mid, decl->sig->ret, eval);
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
              if (mfn->decl->sig->ret) check_type_arrays(mid, mfn->decl->sig->ret, eval);
            }
            break;
          }
          case AstNodeKind::ItemImplTrait: {
            auto* impl = static_cast<const ItemImplTrait*>(item);
            for (const ItemFn* mfn : impl->methods) {
              if (!mfn || !mfn->decl || !mfn->decl->sig) continue;
              for (const Param* p : mfn->decl->sig->params) {
                if (p) check_type_arrays(mid, p->type, eval);
              }
              if (mfn->decl->sig->ret) check_type_arrays(mid, mfn->decl->sig->ret, eval);
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

    // Type-check and evaluate const/static initializers.
    for (ModuleId mid = 0; mid < crate_.modules.size(); mid++) {
      const Module& m = crate_.modules[mid];
      for (Item* item : m.items) {
        if (!item) continue;
        switch (item->kind) {
          case AstNodeKind::ItemConst: {
            auto* c = static_cast<const ItemConst*>(item);
            TypeId expected = const_types_.contains(c) ? const_types_.at(c) : types_.error();
            Env env{};
            TypeId got = check_expr(mid, c->value, env, expected).type;
            if (!types_.can_coerce(got, expected)) {
              error(c->span, "const initializer type mismatch: expected `" + types_.to_string(expected) + "`, got `" +
                                 types_.to_string(got) + "`");
            }
            if (!session_.has_errors()) (void)eval.eval_const(c);
            break;
          }
          case AstNodeKind::ItemStatic: {
            auto* s = static_cast<const ItemStatic*>(item);
            TypeId expected = static_types_.contains(s) ? static_types_.at(s) : types_.error();
            Env env{};
            TypeId got = check_expr(mid, s->value, env, expected).type;
            if (!types_.can_coerce(got, expected)) {
              error(s->span, "static initializer type mismatch: expected `" + types_.to_string(expected) + "`, got `" +
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

  void check_type_arrays(ModuleId mid, const Type* ty, ComptimeEvaluator& eval) {
    if (!ty) return;
    switch (ty->kind) {
      case AstNodeKind::TypeArray: {
        auto* a = static_cast<const TypeArray*>(ty);
        if (a->len) {
          Env env{};
          (void)check_expr(mid, a->len, env, types_.int_(IntKind::Usize));
          if (!session_.has_errors()) {
            if (auto v = eval.eval_usize(mid, a->len)) array_lens_[a->len] = *v;
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
        check_type_arrays(mid, static_cast<const TypeSlice*>(ty)->elem, eval);
        return;
      case AstNodeKind::TypeTuple: {
        auto* t = static_cast<const TypeTuple*>(ty);
        for (const Type* e : t->elems) check_type_arrays(mid, e, eval);
        return;
      }
      default:
        return;
    }
  }

  void check_fn_body(ModuleId mid, const ItemFn* fn, std::optional<TypeId> self_ty) {
    if (!fn || !fn->decl || !fn->decl->sig || !fn->body) return;
    auto it = fn_info_.find(fn);
    if (it == fn_info_.end()) return;

    TypeId ret_ty = it->second.ret;
    Env env{};
    env.push_scope();

    // Params
    size_t idx = 0;
    for (const Param* p : fn->decl->sig->params) {
      if (!p) continue;
      TypeId pt = it->second.params.at(idx++);
      env.declare(p->name, VarInfo{.type = pt, .is_mut = false, .state = VarState::Live});
    }

    ExprResult body = check_block(mid, fn->body, env, ret_ty);
    (void)body;

    env.pop_scope();
    (void)self_ty;
  }

  ExprResult check_block(ModuleId mid, const Block* block, Env& env, std::optional<TypeId> expected) {
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

  ExprResult check_stmt(ModuleId mid, const Stmt* stmt, Env& env, std::optional<TypeId> expected_ret) {
    switch (stmt->kind) {
      case AstNodeKind::StmtLet:
        check_let(mid, static_cast<const StmtLet*>(stmt), env);
        return {.type = types_.unit()};
      case AstNodeKind::StmtExpr:
        (void)check_expr(mid, static_cast<const StmtExpr*>(stmt)->expr, env, std::nullopt);
        return {.type = types_.unit()};
      case AstNodeKind::StmtReturn: {
        auto* r = static_cast<const StmtReturn*>(stmt);
        TypeId expected = expected_ret.value_or(types_.unit());
        TypeId got = r->value ? check_expr(mid, r->value, env, expected).type : types_.unit();
        if (!types_.can_coerce(got, expected)) {
          error(stmt->span, "return type mismatch: expected `" + types_.to_string(expected) + "`, got `" +
                                types_.to_string(got) + "`");
        }
        return {.type = expected, .diverged = true};
      }
      case AstNodeKind::StmtBreak:
      case AstNodeKind::StmtContinue:
        return {.type = types_.unit(), .diverged = true};
      default:
        break;
    }
    return {.type = types_.unit()};
  }

  void check_let(ModuleId mid, const StmtLet* s, Env& env) {
    TypeId ann = s->type_ann ? lower_type(mid, s->type_ann, false, std::nullopt, false) : types_.error();
    std::optional<TypeId> expected = s->type_ann ? std::optional<TypeId>(ann) : std::nullopt;

    TypeId init_ty = types_.error();
    if (s->init) init_ty = check_expr(mid, s->init, env, expected).type;

    TypeId pat_ty = expected.value_or(init_ty);
    if (!s->type_ann && !s->init) {
      error(s->span, "`let` requires a type annotation or an initializer");
      pat_ty = types_.error();
    }

    if (s->type_ann && s->init && !types_.can_coerce(init_ty, ann)) {
      error(s->span, "type mismatch in `let`: expected `" + types_.to_string(ann) + "`, got `" + types_.to_string(init_ty) +
                         "`");
    }

    VarState init_state = s->init ? VarState::Live : VarState::Uninit;
    bind_pattern(mid, s->pat, pat_ty, env, init_state);
  }

  void bind_pattern(ModuleId mid, const Pattern* pat, TypeId scrutinee, Env& env, VarState init_state) {
    if (!pat) return;
    switch (pat->kind) {
      case AstNodeKind::PatWildcard:
        return;
      case AstNodeKind::PatBinding: {
        auto* b = static_cast<const PatBinding*>(pat);
        env.declare(b->name, VarInfo{.type = scrutinee, .is_mut = b->is_mut, .state = init_state});
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
          error(pat->span, "tuple pattern does not match type `" + types_.to_string(scrutinee) + "`");
          return;
        }
        if (t->elems.size() != td.tuple_elems.size()) {
          error(pat->span, "tuple pattern arity mismatch");
          return;
        }
        for (size_t i = 0; i < t->elems.size(); i++) bind_pattern(mid, t->elems[i], td.tuple_elems[i], env, init_state);
        return;
      }
      case AstNodeKind::PatStruct: {
        auto* sp = static_cast<const PatStruct*>(pat);
        TypeId ty = lower_type_path(mid, sp->type_name, false, std::nullopt, false);
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
        for (size_t i = 0; i < vp->args.size(); i++) bind_pattern(mid, vp->args[i], variant->payload[i], env, init_state);
        return;
      }
      case AstNodeKind::PatPath: {
        // unit variant; nothing to bind
        return;
      }
      case AstNodeKind::PatOr: {
        // Conservative: allow only literal-or patterns (no bindings).
        if (pattern_has_binding(static_cast<const PatOr*>(pat))) {
          error(pat->span, "bindings in `|` patterns are not supported yet");
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

  std::pair<TypeId, const VariantInfo*> resolve_variant_path(ModuleId mid, const Path* path) {
    if (!path || path->segments.size() < 2) return {types_.error(), nullptr};
    // Resolve prefix as type path.
    Path prefix{path->span, std::vector<Ident*>{path->segments.begin(), path->segments.end() - 1}};
    TypeId enum_ty = lower_type_path(mid, &prefix, false, std::nullopt, false);
    const TypeData& td = types_.get(enum_ty);
    if (td.kind != TypeKind::Enum || !td.enum_def) return {types_.error(), nullptr};
    const EnumInfo& ei = enum_info_.at(td.enum_def);
    std::string_view vname = path->segments.back()->text;
    auto it = ei.variants.find(std::string(vname));
    if (it == ei.variants.end()) return {enum_ty, nullptr};
    return {enum_ty, &it->second};
  }

  ExprResult check_expr(ModuleId mid, const Expr* expr, Env& env, std::optional<TypeId> expected) {
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
        if (expected && types_.get(*expected).kind == TypeKind::Int) {
          r = {.type = *expected};
        } else {
          r = {.type = types_.int_(IntKind::I32)};
        }
        break;
      }
      case AstNodeKind::ExprString:
        r = {.type = types_.ptr(Mutability::Const, types_.slice(types_.int_(IntKind::U8)))};
        break;
      case AstNodeKind::ExprBlock:
        r = check_block(mid, static_cast<const ExprBlock*>(expr)->block, env, expected);
        break;
      case AstNodeKind::ExprComptime:
        r = check_block(mid, static_cast<const ExprComptime*>(expr)->block, env, expected);
        break;
      case AstNodeKind::ExprPath:
        r = check_path_expr(mid, static_cast<const ExprPath*>(expr)->path, env, /*as_value=*/true);
        break;
      case AstNodeKind::ExprStructLit:
        r = check_struct_lit(mid, static_cast<const ExprStructLit*>(expr), env, expected);
        break;
      case AstNodeKind::ExprTuple:
        r = check_tuple_expr(mid, static_cast<const ExprTuple*>(expr), env, expected);
        break;
      case AstNodeKind::ExprField:
        r = check_field_expr(mid, static_cast<const ExprField*>(expr), env);
        break;
      case AstNodeKind::ExprIndex:
        r = check_index_expr(mid, static_cast<const ExprIndex*>(expr), env);
        break;
      case AstNodeKind::ExprAssign:
        r = check_assign_expr(mid, static_cast<const ExprAssign*>(expr), env);
        break;
      case AstNodeKind::ExprUnary:
        r = check_unary_expr(mid, static_cast<const ExprUnary*>(expr), env, expected);
        break;
      case AstNodeKind::ExprBinary:
        r = check_binary_expr(mid, static_cast<const ExprBinary*>(expr), env, expected);
        break;
      case AstNodeKind::ExprCast:
        r = check_cast_expr(mid, static_cast<const ExprCast*>(expr), env);
        break;
      case AstNodeKind::ExprCall:
        r = check_call_expr(mid, static_cast<const ExprCall*>(expr), env, expected);
        break;
      case AstNodeKind::ExprMethodCall:
        r = check_method_call(mid, static_cast<const ExprMethodCall*>(expr), env, expected);
        break;
      case AstNodeKind::ExprIf:
        r = check_if_expr(mid, static_cast<const ExprIf*>(expr), env, expected);
        break;
      case AstNodeKind::ExprMatch:
        r = check_match_expr(mid, static_cast<const ExprMatch*>(expr), env, expected);
        break;
      case AstNodeKind::ExprWhile:
        r = check_while_expr(mid, static_cast<const ExprWhile*>(expr), env);
        break;
      case AstNodeKind::ExprLoop:
        r = check_loop_expr(mid, static_cast<const ExprLoop*>(expr), env);
        break;
      default:
        r = {.type = types_.error()};
        break;
    }
    expr_types_[expr] = r.type;
    return r;
  }

  ExprResult check_path_expr(ModuleId mid, const Path* path, Env& env, bool as_value) {
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
        return ct == const_types_.end() ? std::optional<TypeId>(types_.error()) : std::optional<TypeId>(ct->second);
      }
      case AstNodeKind::ItemStatic: {
        auto* s = static_cast<const ItemStatic*>(item);
        auto st = static_types_.find(s);
        return st == static_types_.end() ? std::optional<TypeId>(types_.error()) : std::optional<TypeId>(st->second);
      }
      case AstNodeKind::ItemFn: {
        return std::nullopt;  // functions are not first-class values yet
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

  ExprResult check_struct_lit(ModuleId mid, const ExprStructLit* lit, Env& env, std::optional<TypeId> expected) {
    TypeId ty = lower_type_path(mid, lit->type_name, false, std::nullopt, false);
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

  ExprResult check_tuple_expr(ModuleId mid, const ExprTuple* t, Env& env, std::optional<TypeId> expected) {
    std::vector<TypeId> elems;
    for (const Expr* e : t->elems) {
      elems.push_back(check_expr(mid, e, env, std::nullopt).type);
    }
    (void)expected;
    return {.type = types_.tuple(std::move(elems))};
  }

  Place check_place(ModuleId mid, const Expr* e, Env& env, bool allow_reinit_local = false) {
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
              if (v->state == VarState::Uninit) error(e->span, "use of uninitialized local `" + std::string(name) + "`");
              if (v->state == VarState::Moved) error(e->span, "use after move of local `" + std::string(name) + "`");
            }
            return {.type = v->type, .writable = v->is_mut, .root_local = std::string(name)};
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
        if (bd.kind != TypeKind::Struct || !bd.struct_def) break;
        const StructInfo& si = struct_info_.at(bd.struct_def);
        auto it = si.fields.find(f->field);
        if (it == si.fields.end()) break;
        return {.type = it->second, .writable = base.writable, .root_local = base.root_local};
      }
      case AstNodeKind::ExprIndex: {
        auto* ix = static_cast<const ExprIndex*>(e);
        Place base = check_place(mid, ix->base, env);
        if (ix->base) expr_types_[ix->base] = base.type;
        TypeId bty = base.type;
        const TypeData& bd = types_.get(bty);
        if (bd.kind == TypeKind::Array) {
          return {.type = bd.elem, .writable = base.writable, .root_local = base.root_local};
        }
        if (bd.kind == TypeKind::Ptr) {
          const TypeData& pd = types_.get(bd.pointee);
          if (pd.kind == TypeKind::Slice) {
            base.root_local.clear();
            base.writable = (bd.mutability == Mutability::Mut);
            return {.type = pd.elem, .writable = base.writable, .root_local = base.root_local};
          }
        }
        break;
      }
      default:
        break;
    }
    error(e->span, "expression is not assignable");
    return {.type = types_.error()};
  }

  ExprResult check_field_expr(ModuleId mid, const ExprField* f, Env& env) {
    Place place = check_place(mid, f, env);
    if (!types_.is_copy(place.type) && !place.root_local.empty()) {
      if (VarInfo* v = env.lookup(place.root_local)) v->state = VarState::Moved;
    }
    return {.type = place.type};
  }

  ExprResult check_index_expr(ModuleId mid, const ExprIndex* ix, Env& env) {
    (void)check_expr(mid, ix->index, env, types_.int_(IntKind::Usize));
    Place place = check_place(mid, ix, env);
    if (!types_.is_copy(place.type) && !place.root_local.empty()) {
      if (VarInfo* v = env.lookup(place.root_local)) v->state = VarState::Moved;
    }
    return {.type = place.type};
  }

  ExprResult check_assign_expr(ModuleId mid, const ExprAssign* a, Env& env) {
    Place lhs = check_place(mid, a->lhs, env, /*allow_reinit_local=*/true);
    if (a->lhs) expr_types_[a->lhs] = lhs.type;
    if (!lhs.writable) error(a->lhs->span, "assignment target is not writable");
    TypeId rhs = check_expr(mid, a->rhs, env, lhs.type).type;
    if (!types_.can_coerce(rhs, lhs.type)) {
      error(a->span,
            "assignment type mismatch: expected `" + types_.to_string(lhs.type) + "`, got `" + types_.to_string(rhs) + "`");
    }
    // Reinitialize locals on `x = ...`.
    if (!lhs.root_local.empty()) {
      if (VarInfo* v = env.lookup(lhs.root_local)) v->state = VarState::Live;
    }
    return {.type = types_.unit()};
  }

  ExprResult check_unary_expr(ModuleId mid, const ExprUnary* u, Env& env, std::optional<TypeId> expected) {
    switch (u->op) {
      case UnaryOp::Neg: {
        TypeId t = check_expr(mid, u->expr, env, expected).type;
        if (types_.get(t).kind != TypeKind::Int) error(u->span, "unary `-` requires an integer");
        return {.type = t};
      }
      case UnaryOp::Not: {
        TypeId t = check_expr(mid, u->expr, env, types_.bool_()).type;
        if (!types_.equal(t, types_.bool_())) error(u->span, "unary `!` requires bool");
        return {.type = types_.bool_()};
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
    }
    return {.type = types_.error()};
  }

  ExprResult check_binary_expr(ModuleId mid, const ExprBinary* b, Env& env, std::optional<TypeId> expected) {
    switch (b->op) {
      case BinaryOp::Add:
      case BinaryOp::Sub:
      case BinaryOp::Mul:
      case BinaryOp::Div:
      case BinaryOp::Mod: {
        TypeId lhs = check_expr(mid, b->lhs, env, expected).type;
        TypeId rhs = check_expr(mid, b->rhs, env, lhs).type;
        if (types_.get(lhs).kind != TypeKind::Int || !types_.can_coerce(rhs, lhs)) {
          error(b->span, "binary arithmetic requires matching integer types");
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
        if (!types_.can_coerce(rhs, lhs)) error(b->span, "comparison operands must have matching types");
        return {.type = types_.bool_()};
      }
      case BinaryOp::And:
      case BinaryOp::Or: {
        TypeId lhs = check_expr(mid, b->lhs, env, types_.bool_()).type;
        TypeId rhs = check_expr(mid, b->rhs, env, types_.bool_()).type;
        if (!types_.equal(lhs, types_.bool_()) || !types_.equal(rhs, types_.bool_())) {
          error(b->span, "logical operators require bool");
        }
        return {.type = types_.bool_()};
      }
    }
    return {.type = types_.error()};
  }

  ExprResult check_cast_expr(ModuleId mid, const ExprCast* c, Env& env) {
    TypeId src = check_expr(mid, c->value, env, std::nullopt).type;
    TypeId dst = lower_type(mid, c->to, false, std::nullopt, false);
    (void)src;
    return {.type = dst};
  }

  ExprResult check_call_expr(ModuleId mid, const ExprCall* call, Env& env, std::optional<TypeId> expected) {
    // Only support calling a path for now.
    if (!call->callee || call->callee->kind != AstNodeKind::ExprPath) {
      error(call->span, "calling non-path expressions is not supported yet");
      return {.type = types_.error()};
    }

    const Path* callee = static_cast<const ExprPath*>(call->callee)->path;
    if (!callee) return {.type = types_.error()};

    // Builtins (comptime).
    if (callee->segments.size() == 2 && callee->segments[0]->text == "builtin" && callee->segments[1]->text == "size_of") {
      if (call->args.size() != 1) {
        error(call->span, "builtin::size_of expects 1 argument");
        return {.type = types_.int_(IntKind::Usize)};
      }
      auto ty = lower_type_value_expr(mid, call->args[0]);
      if (ty && !types_.is_sized(*ty)) error(call->args[0]->span, "builtin::size_of requires a sized type");
      return {.type = types_.int_(IntKind::Usize)};
    }
    if (callee->segments.size() == 2 && callee->segments[0]->text == "builtin" && callee->segments[1]->text == "align_of") {
      if (call->args.size() != 1) {
        error(call->span, "builtin::align_of expects 1 argument");
        return {.type = types_.int_(IntKind::Usize)};
      }
      auto ty = lower_type_value_expr(mid, call->args[0]);
      if (ty && !types_.is_sized(*ty)) error(call->args[0]->span, "builtin::align_of requires a sized type");
      return {.type = types_.int_(IntKind::Usize)};
    }
    if (callee->segments.size() == 2 && callee->segments[0]->text == "builtin" && callee->segments[1]->text == "addr_of") {
      if (call->args.size() != 1) {
        error(call->span, "builtin::addr_of expects 1 argument");
        return {.type = types_.error()};
      }
      Place p = check_place(mid, call->args[0], env);
      if (!types_.is_sized(p.type)) error(call->args[0]->span, "builtin::addr_of requires a sized place");
      return {.type = types_.ptr(Mutability::Const, p.type)};
    }
    if (callee->segments.size() == 2 && callee->segments[0]->text == "builtin" && callee->segments[1]->text == "addr_of_mut") {
      if (call->args.size() != 1) {
        error(call->span, "builtin::addr_of_mut expects 1 argument");
        return {.type = types_.error()};
      }
      Place p = check_place(mid, call->args[0], env);
      if (!p.writable) error(call->args[0]->span, "builtin::addr_of_mut requires a mutable place");
      if (!types_.is_sized(p.type)) error(call->args[0]->span, "builtin::addr_of_mut requires a sized place");
      return {.type = types_.ptr(Mutability::Mut, p.type)};
    }
    if (callee->segments.size() == 2 && callee->segments[0]->text == "builtin" && callee->segments[1]->text == "compile_error") {
      TypeId msg_ty = types_.ptr(Mutability::Const, types_.slice(types_.int_(IntKind::U8)));
      if (call->args.size() != 1) {
        error(call->span, "builtin::compile_error expects 1 argument");
        return {.type = expected.value_or(types_.unit()), .diverged = true};
      }
      TypeId got = check_expr(mid, call->args[0], env, msg_ty).type;
      if (!types_.can_coerce(got, msg_ty)) {
        error(call->args[0]->span,
              "argument type mismatch: expected `" + types_.to_string(msg_ty) + "`, got `" + types_.to_string(got) + "`");
      }
      return {.type = expected.value_or(types_.unit()), .diverged = true};
    }

    // Try value call (free function) first.
    const ItemFn* fn = resolve_fn_path(mid, callee);
    if (fn) return check_direct_call(mid, fn, call->args, env, expected);

    // Try enum variant constructor `Enum::Variant(...)`.
    auto [enum_ty, variant] = resolve_variant_path(mid, callee);
    if (variant) {
      if (variant->payload.size() != call->args.size()) {
        error(call->span, "variant constructor arity mismatch");
        return {.type = enum_ty};
      }
      for (size_t i = 0; i < call->args.size(); i++) {
        (void)check_expr(mid, call->args[i], env, variant->payload[i]);
      }
      return {.type = enum_ty};
    }

    // Try inherent associated function call `Type::f(...)`.
    if (callee->segments.size() >= 2) {
      Path prefix{callee->span, std::vector<Ident*>{callee->segments.begin(), callee->segments.end() - 1}};
      TypeId ty = lower_type_path(mid, &prefix, false, std::nullopt, false);
      const TypeData& td = types_.get(ty);
      if (td.kind == TypeKind::Struct || td.kind == TypeKind::Enum) {
        const Item* def = td.kind == TypeKind::Struct ? static_cast<const Item*>(td.struct_def) : static_cast<const Item*>(td.enum_def);
        if (auto it = crate_.inherent_methods.find(def); it != crate_.inherent_methods.end()) {
          std::string_view name = callee->segments.back()->text;
          if (auto mi = it->second.find(std::string(name)); mi != it->second.end()) {
            return check_direct_call(mid, mi->second, call->args, env, expected);
          }
        }
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
      if (it->second->kind == AstNodeKind::ItemFn) return static_cast<const ItemFn*>(it->second);
      return nullptr;
    }

    ModuleId cur = mid;
    for (size_t i = 0; i + 1 < path->segments.size(); i++) {
      auto it = crate_.modules[cur].submodules.find(path->segments[i]->text);
      if (it == crate_.modules[cur].submodules.end()) return nullptr;
      cur = it->second;
    }
    auto it = crate_.modules[cur].values.find(path->segments.back()->text);
    if (it == crate_.modules[cur].values.end()) return nullptr;
    if (it->second->kind == AstNodeKind::ItemFn) return static_cast<const ItemFn*>(it->second);
    return nullptr;
  }

  ExprResult check_direct_call(
      ModuleId mid,
      const ItemFn* fn,
      const std::vector<Expr*>& args,
      Env& env,
      std::optional<TypeId> expected) {
    auto it = fn_info_.find(fn);
    if (it == fn_info_.end()) return {.type = types_.error()};

    const FnInfo& sig = it->second;
    if (args.size() != sig.params.size()) {
      error(fn->span, "call arity mismatch");
      return {.type = sig.ret};
    }
    for (size_t i = 0; i < args.size(); i++) {
      TypeId got = check_expr(mid, args[i], env, sig.params[i]).type;
      if (!types_.can_coerce(got, sig.params[i])) {
        error(args[i]->span,
              "argument type mismatch: expected `" + types_.to_string(sig.params[i]) + "`, got `" + types_.to_string(got) + "`");
      }
    }
    (void)expected;
    return {.type = sig.ret};
  }

  ExprResult check_method_call(ModuleId mid, const ExprMethodCall* mc, Env& env, std::optional<TypeId> expected) {
    // Avoid moving locals when analyzing the receiver; we'll auto-borrow if needed.
    TypeId recv_ty = 0;
    if (mc->receiver && mc->receiver->kind == AstNodeKind::ExprPath) {
      recv_ty = check_path_expr(mid, static_cast<const ExprPath*>(mc->receiver)->path, env, /*as_value=*/false).type;
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
    if (nd.kind == TypeKind::Struct) def = static_cast<const Item*>(nd.struct_def);
    if (nd.kind == TypeKind::Enum) def = static_cast<const Item*>(nd.enum_def);
    if (nd.kind == TypeKind::DynTrait) {
      if (!nd.trait_def) {
        error(mc->span, "invalid `dyn` receiver type");
        return {.type = types_.error()};
      }
      if (rd.kind != TypeKind::Ptr) {
        error(mc->span, "calling methods on `dyn` requires a pointer receiver");
        return {.type = types_.error()};
      }

      auto ts_it = trait_methods_.find(nd.trait_def);
      if (ts_it == trait_methods_.end()) {
        error(mc->span, "unknown trait in `dyn` receiver");
        return {.type = types_.error()};
      }
      auto mi = ts_it->second.methods.find(mc->method);
      if (mi == ts_it->second.methods.end()) {
        error(mc->span, "cannot resolve method `" + mc->method + "` on `dyn " + nd.trait_def->name + "`");
        return {.type = types_.error()};
      }
      const TraitMethodInfo& tm = mi->second;
      const FnInfo& sig = tm.sig;
      if (sig.params.empty()) {
        error(mc->span, "trait method is missing a receiver parameter");
        return {.type = sig.ret};
      }
      if (!tm.object_safe) {
        error(mc->span, "method `" + mc->method + "` is not callable on `dyn " + nd.trait_def->name + "` (not object-safe)");
        return {.type = sig.ret};
      }

      TypeId self_param = sig.params[0];
      const TypeData& sp = types_.get(self_param);
      if (sp.kind != TypeKind::Ptr || types_.get(sp.pointee).kind != TypeKind::Self) {
        error(mc->span, "dyn-dispatchable methods must take `self: const* Self` or `self: mut* Self`");
        return {.type = sig.ret};
      }

      TypeId expected_recv = types_.ptr(sp.mutability, nominal);
      if (!types_.can_coerce(recv_ty, expected_recv)) {
        error(mc->receiver->span,
              "receiver type mismatch: expected `" + types_.to_string(expected_recv) + "`, got `" + types_.to_string(recv_ty) + "`");
      }

      if (mc->args.size() + 1 != sig.params.size()) {
        error(mc->span, "method call arity mismatch");
        return {.type = sig.ret};
      }

      for (size_t i = 0; i < mc->args.size(); i++) {
        TypeId expected_arg = sig.params[i + 1];
        TypeId got = check_expr(mid, mc->args[i], env, expected_arg).type;
        if (!types_.can_coerce(got, expected_arg)) {
          error(mc->args[i]->span,
                "argument type mismatch: expected `" + types_.to_string(expected_arg) + "`, got `" + types_.to_string(got) + "`");
        }
      }

      (void)expected;
      return {.type = sig.ret};
    }
    if (!def) {
      error(mc->span, "method calls require a nominal receiver type");
      return {.type = types_.error()};
    }

    // Inherent first.
    const ItemFn* method = nullptr;
    if (auto it = crate_.inherent_methods.find(def); it != crate_.inherent_methods.end()) {
      auto mi = it->second.find(mc->method);
      if (mi != it->second.end()) method = mi->second;
    }

    // Traits in scope.
    if (!method) {
      const Module& m = crate_.modules[mid];
      for (const auto& [_, ty_item] : m.types) {
        if (!ty_item || ty_item->kind != AstNodeKind::ItemTrait) continue;
        TraitImplKey key{.trait = static_cast<const ItemTrait*>(ty_item), .type = def};
        auto impl_it = crate_.trait_impl_methods.find(key);
        if (impl_it == crate_.trait_impl_methods.end()) continue;
        auto mi = impl_it->second.find(mc->method);
        if (mi != impl_it->second.end()) {
          method = mi->second;
          break;
        }
      }
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
    if (types_.get(self_param).kind == TypeKind::Ptr && rd.kind != TypeKind::Ptr) {
      Place place = check_place(mid, mc->receiver, env);
      if (place.root_local.empty()) {
        error(mc->receiver->span, "cannot take address of this receiver");
      } else {
        // Determine mutability required.
        const TypeData& sp = types_.get(self_param);
        if (sp.mutability == Mutability::Mut && !place.writable) {
          error(mc->receiver->span, "method requires a mutable receiver");
        }
        recv_arg = types_.ptr(sp.mutability, place.type);
      }
    }

    if (!types_.can_coerce(recv_arg, self_param)) {
      error(mc->receiver->span,
            "receiver type mismatch: expected `" + types_.to_string(self_param) + "`, got `" + types_.to_string(recv_arg) + "`");
    }

    if (mc->args.size() + 1 != sig.params.size()) {
      error(mc->span, "method call arity mismatch");
      return {.type = sig.ret};
    }

    for (size_t i = 0; i < mc->args.size(); i++) {
      TypeId expected_arg = sig.params[i + 1];
      TypeId got = check_expr(mid, mc->args[i], env, expected_arg).type;
      if (!types_.can_coerce(got, expected_arg)) {
        error(mc->args[i]->span,
              "argument type mismatch: expected `" + types_.to_string(expected_arg) + "`, got `" + types_.to_string(got) + "`");
      }
    }

    (void)expected;
    return {.type = sig.ret};
  }

  ExprResult check_if_expr(ModuleId mid, const ExprIf* iff, Env& env, std::optional<TypeId> expected) {
    TypeId cond = check_expr(mid, iff->cond, env, types_.bool_()).type;
    if (!types_.equal(cond, types_.bool_())) error(iff->cond->span, "if condition must be bool");

    Env then_env = env;
    ExprResult then_r = check_block(mid, iff->then_block, then_env, expected);

    if (!iff->else_expr) {
      env = then_env;
      return {.type = types_.unit()};
    }

    Env else_env = env;
    ExprResult else_r = check_expr(mid, iff->else_expr, else_env, expected);

    if (!types_.can_coerce(else_r.type, then_r.type) && !types_.can_coerce(then_r.type, else_r.type)) {
      error(iff->span, "if branches have incompatible types");
    }

    Env out = then_env;
    join_env(out, else_env);
    env = out;
    return {.type = then_r.type};
  }

  ExprResult check_match_expr(ModuleId mid, const ExprMatch* m, Env& env, std::optional<TypeId> expected) {
    TypeId scrutinee = check_expr(mid, m->scrutinee, env, std::nullopt).type;

    std::optional<TypeId> arm_type{};
    bool any_arm = false;
    Env joined_env = env;
    bool first_env = true;

    for (const MatchArm* arm : m->arms) {
      if (!arm) continue;
      any_arm = true;

      Env arm_env = env;
      arm_env.push_scope();
      check_pattern(mid, arm->pat, scrutinee, arm_env);
      if (arm->guard) {
        TypeId gt = check_expr(mid, arm->guard, arm_env, types_.bool_()).type;
        if (!types_.equal(gt, types_.bool_())) error(arm->guard->span, "match guard must be bool");
      }
      ExprResult r = check_expr(mid, arm->body, arm_env, expected);
      arm_env.pop_scope();

      if (!arm_type) {
        arm_type = r.type;
      } else if (!types_.can_coerce(r.type, *arm_type) && !types_.can_coerce(*arm_type, r.type)) {
        error(arm->span, "match arm type mismatch");
      }

      if (first_env) {
        joined_env = arm_env;
        first_env = false;
      } else {
        join_env(joined_env, arm_env);
      }
    }

    env = any_arm ? joined_env : env;
    return {.type = arm_type.value_or(types_.unit())};
  }

  ExprResult check_while_expr(ModuleId mid, const ExprWhile* wh, Env& env) {
    if (!wh) return {.type = types_.unit()};
    TypeId cond = check_expr(mid, wh->cond, env, types_.bool_()).type;
    if (!types_.equal(cond, types_.bool_())) error(wh->cond->span, "while condition must be bool");

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

  void check_pattern(ModuleId mid, const Pattern* pat, TypeId scrutinee, Env& env) {
    if (!pat) return;
    switch (pat->kind) {
      case AstNodeKind::PatWildcard:
        return;
      case AstNodeKind::PatInt:
        if (types_.get(scrutinee).kind != TypeKind::Int) error(pat->span, "integer pattern on non-integer scrutinee");
        return;
      case AstNodeKind::PatBool:
        if (!types_.equal(scrutinee, types_.bool_())) error(pat->span, "bool pattern on non-bool scrutinee");
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

std::optional<CheckedCrate> check_crate(Session& session, const ResolvedCrate& crate) {
  Checker checker(session, crate);
  if (!checker.run()) return std::nullopt;
  return std::move(checker).finish();
}

}  // namespace cog
