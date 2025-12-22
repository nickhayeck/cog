#include "emit_llvm.hpp"

#include "comptime.hpp"
#include "layout.hpp"

#include <algorithm>
#include <cstdint>
#include <fstream>
#include <sstream>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

namespace cog {
namespace {

struct LlvmValue {
  std::string ty{};
  std::string v{};
};

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
    if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) {
      out.push_back(c);
    } else {
      out.push_back('_');
    }
  }
  return out;
}

struct ItemLocator {
  const ResolvedCrate& crate;
  std::unordered_map<const Item*, ModuleId> item_module{};
  std::unordered_map<const ItemFn*, std::string> fn_symbol{};

  explicit ItemLocator(const ResolvedCrate& crate) : crate(crate) {
    for (ModuleId mid = 0; mid < crate.modules.size(); mid++) {
      for (const Item* item : crate.modules[mid].items) {
        if (!item) continue;
        item_module[item] = mid;
        if (item->kind == AstNodeKind::ItemFn) {
          auto* fn = static_cast<const ItemFn*>(item);
          if (fn->decl) fn_symbol.insert({fn, mangle_in_module(mid, sanitize(fn->decl->name))});
        }
        if (item->kind == AstNodeKind::ItemImplInherent) {
          auto* impl = static_cast<const ItemImplInherent*>(item);
          for (const ItemFn* m : impl->methods) {
            item_module[static_cast<const Item*>(m)] = mid;
            if (m && m->decl) {
              fn_symbol.insert(
                  {m,
                   mangle_in_module(mid, "inherent" + mangle_path(impl->type_name) + "$" + sanitize(m->decl->name))});
            }
          }
        }
        if (item->kind == AstNodeKind::ItemImplTrait) {
          auto* impl = static_cast<const ItemImplTrait*>(item);
          for (const ItemFn* m : impl->methods) {
            item_module[static_cast<const Item*>(m)] = mid;
            if (m && m->decl) {
              fn_symbol.insert({m,
                                mangle_in_module(
                                    mid,
                                    "impl" + mangle_path(impl->trait_name) + "$for" + mangle_path(impl->for_type_name) +
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
    return mangle_in_module(module_of(static_cast<const Item*>(fn)), sanitize(fn->decl->name));
  }
};

class LlvmEmitter {
 public:
  LlvmEmitter(Session& session, const ResolvedCrate& crate, CheckedCrate& checked)
      : session_(session),
        crate_(crate),
        checked_(checked),
        locator_(crate),
        layout_(session, checked.types, checked.struct_info, checked.enum_info, checked.array_lens),
        eval_(session, crate, &checked.types, &layout_) {}

  bool emit_to_file(const std::filesystem::path& out_ll, bool emit_main_wrapper) {
    std::ofstream out(out_ll);
    if (!out.is_open()) {
      session_.diags.push_back(Diagnostic{
          .severity = Severity::Error,
          .span = Span{},
          .message = "failed to open output file `" + out_ll.string() + "`",
      });
      return false;
    }

    build_struct_types();
    build_enum_types();
    build_trait_object_types();
    build_trait_vtables();
    build_function_decls();

    out << "; Cog (prototype) LLVM IR\n";
    out << "target triple = \"" << target_triple() << "\"\n\n";

    for (const auto& line : type_defs_) out << line << "\n";
    if (!type_defs_.empty()) out << "\n";

    for (const auto& line : globals_) out << line << "\n";
    if (!globals_.empty()) out << "\n";

    for (auto& fn : functions_) out << fn << "\n\n";

    if (emit_main_wrapper) emit_main_shim(out);
    return !session_.has_errors();
  }

 private:
  Session& session_;
  const ResolvedCrate& crate_;
  CheckedCrate& checked_;
  ItemLocator locator_;

  LayoutEngine layout_;
  ComptimeEvaluator eval_;

  std::vector<std::string> type_defs_{};
  std::vector<std::string> globals_{};
  std::vector<std::string> functions_{};

  std::unordered_map<const ItemStruct*, std::string> struct_type_names_{};
  std::unordered_map<const ItemEnum*, std::string> enum_type_names_{};
  std::unordered_map<const ItemTrait*, std::string> dyn_type_names_{};
  std::unordered_map<const ItemTrait*, std::string> vtable_type_names_{};
  std::unordered_set<const ItemFn*> emitted_fns_{};

  std::uint32_t ptr_bits() const { return static_cast<std::uint32_t>(sizeof(void*) * 8); }

  std::string target_triple() const {
#if defined(__APPLE__) && defined(__aarch64__)
    return "arm64-apple-darwin";
#elif defined(__APPLE__) && defined(__x86_64__)
    return "x86_64-apple-darwin";
#elif defined(__linux__) && defined(__x86_64__)
    return "x86_64-unknown-linux-gnu";
#else
    return "unknown-unknown-unknown";
#endif
  }

  std::string llvm_int_type(IntKind k) const {
    return "i" + std::to_string(int_bits(k, ptr_bits()));
  }

  std::string llvm_struct_type_name(const ItemStruct* s) const {
    ModuleId mid = locator_.module_of(static_cast<const Item*>(s));
    std::string name = "%struct";
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
    std::string name = "%enum";
    for (auto part : locator_.module_path(mid)) {
      name += ".";
      name += sanitize(part);
    }
    name += ".";
    name += sanitize(e ? e->name : "<enum>");
    return name;
  }

  std::string llvm_trait_suffix(const ItemTrait* tr) const {
    ModuleId mid = locator_.module_of(static_cast<const Item*>(tr));
    std::string suffix{};
    for (auto part : locator_.module_path(mid)) {
      if (!suffix.empty()) suffix += ".";
      suffix += sanitize(part);
    }
    if (!suffix.empty()) suffix += ".";
    suffix += sanitize(tr ? tr->name : "<trait>");
    return suffix;
  }

  std::string llvm_vtable_type_name(const ItemTrait* tr) {
    auto it = vtable_type_names_.find(tr);
    if (it != vtable_type_names_.end()) return it->second;
    std::string name = "%vtable." + llvm_trait_suffix(tr);
    vtable_type_names_.insert({tr, name});
    return name;
  }

  std::string llvm_dyn_type_name(const ItemTrait* tr) {
    auto it = dyn_type_names_.find(tr);
    if (it != dyn_type_names_.end()) return it->second;
    std::string name = "%dyn." + llvm_trait_suffix(tr);
    dyn_type_names_.insert({tr, name});
    return name;
  }

  std::string llvm_type(TypeId ty) {
    const TypeData& d = checked_.types.get(ty);
    switch (d.kind) {
      case TypeKind::Error:
        return "i8";
      case TypeKind::Unit:
        return "void";
      case TypeKind::Bool:
        return "i1";
      case TypeKind::Int:
        return llvm_int_type(d.int_kind);
      case TypeKind::Ptr:
        if (checked_.types.get(d.pointee).kind == TypeKind::DynTrait) {
          const TypeData& pd = checked_.types.get(d.pointee);
          if (pd.trait_def) return llvm_dyn_type_name(pd.trait_def);
        }
        return llvm_type(d.pointee) + "*";
      case TypeKind::Struct: {
        if (!d.struct_def) return "i8";
        auto it = struct_type_names_.find(d.struct_def);
        if (it != struct_type_names_.end()) return it->second;
        std::string name = llvm_struct_type_name(d.struct_def);
        struct_type_names_.insert({d.struct_def, name});
        return name;
      }
      case TypeKind::Enum: {
        if (!d.enum_def) return "i8";
        auto it = enum_type_names_.find(d.enum_def);
        if (it != enum_type_names_.end()) return it->second;
        std::string name = llvm_enum_type_name(d.enum_def);
        enum_type_names_.insert({d.enum_def, name});
        return name;
      }
      default:
        session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = Span{}, .message = "unsupported type in LLVM emission"});
        return "i8";
    }
  }

  TypeId type_of(const Expr* e) const {
    if (!e) return checked_.types.error();
    auto it = checked_.expr_types.find(e);
    if (it == checked_.expr_types.end()) return checked_.types.error();
    return it->second;
  }

  std::string mangle_fn(const ItemFn* fn) const {
    return locator_.symbol_for(fn);
  }

  void build_struct_types() {
    for (ModuleId mid = 0; mid < crate_.modules.size(); mid++) {
      for (const Item* item : crate_.modules[mid].items) {
        if (!item) continue;
        if (item->kind != AstNodeKind::ItemStruct) continue;
        auto* s = static_cast<const ItemStruct*>(item);
        std::string name = llvm_struct_type_name(s);
        struct_type_names_[s] = name;
      }
    }

    for (const auto& [s, name] : struct_type_names_) {
      auto it = checked_.struct_info.find(s);
      if (it == checked_.struct_info.end()) continue;
      std::ostringstream def;
      def << name << " = type { ";
      for (size_t i = 0; i < it->second.fields_in_order.size(); i++) {
        if (i) def << ", ";
        def << llvm_type(it->second.fields_in_order[i].type);
      }
      def << " }";
      type_defs_.push_back(def.str());
    }
  }

  static std::string llvm_int_bytes(std::uint64_t bytes) {
    return "i" + std::to_string(bytes * 8);
  }

  void build_enum_types() {
    for (ModuleId mid = 0; mid < crate_.modules.size(); mid++) {
      for (const Item* item : crate_.modules[mid].items) {
        if (!item) continue;
        if (item->kind != AstNodeKind::ItemEnum) continue;
        auto* e = static_cast<const ItemEnum*>(item);
        enum_type_names_[e] = llvm_enum_type_name(e);
      }
    }

    for (const auto& [e, name] : enum_type_names_) {
      if (!e) continue;
      auto el = layout_.enum_layout(e, Span{});
      if (!el) continue;

      std::string tag_ty = llvm_int_bytes(el->tag_size);
      if (el->payload_size == 0) {
        type_defs_.push_back(name + " = type { " + tag_ty + " }");
        continue;
      }

      std::uint64_t align = std::max<std::uint64_t>(el->payload_align, 1);
      std::uint64_t words = el->payload_size / align;
      std::string payload_elem_ty = llvm_int_bytes(align);
      type_defs_.push_back(name + " = type { " + tag_ty + ", [" + std::to_string(words) + " x " + payload_elem_ty + "] }");
    }
  }

  std::string llvm_trait_method_fnptr_type(const TraitMethodInfo& tm) {
    const FnInfo& sig = tm.sig;
    std::ostringstream out;
    out << llvm_type(sig.ret) << " (i8*";
    for (size_t i = 1; i < sig.params.size(); i++) out << ", " << llvm_type(sig.params[i]);
    out << ")*";
    return out.str();
  }

  void build_trait_object_types() {
    for (const auto& [tr, set] : checked_.trait_methods) {
      if (!tr) continue;
      std::string vt_name = llvm_vtable_type_name(tr);
      std::string dyn_name = llvm_dyn_type_name(tr);

      std::ostringstream vt_def;
      vt_def << vt_name << " = type { ";
      for (size_t i = 0; i < set.order.size(); i++) {
        if (i) vt_def << ", ";
        auto it = set.methods.find(set.order[i]);
        if (it == set.methods.end()) {
          vt_def << "i8*";
        } else {
          vt_def << llvm_trait_method_fnptr_type(it->second);
        }
      }
      vt_def << " }";
      type_defs_.push_back(vt_def.str());

      type_defs_.push_back(dyn_name + " = type { i8*, " + vt_name + "* }");
    }
  }

  std::string type_suffix_for_item(const Item* item) const {
    ModuleId mid = locator_.module_of(item);
    std::string out{};
    for (auto part : locator_.module_path(mid)) {
      out += "$";
      out += sanitize(part);
    }
    if (item && item->kind == AstNodeKind::ItemStruct) {
      out += "$";
      out += sanitize(static_cast<const ItemStruct*>(item)->name);
    } else if (item && item->kind == AstNodeKind::ItemEnum) {
      out += "$";
      out += sanitize(static_cast<const ItemEnum*>(item)->name);
    } else {
      out += "$<type>";
    }
    return out;
  }

  std::string trait_suffix_for_item(const ItemTrait* tr) const {
    ModuleId mid = locator_.module_of(static_cast<const Item*>(tr));
    std::string out{};
    for (auto part : locator_.module_path(mid)) {
      out += "$";
      out += sanitize(part);
    }
    out += "$";
    out += sanitize(tr ? tr->name : "<trait>");
    return out;
  }

  std::string vtable_global_sym(const ItemTrait* tr, const Item* type_item, ModuleId mid) const {
    return locator_.mangle_in_module(mid, "vtable" + trait_suffix_for_item(tr) + "$for" + type_suffix_for_item(type_item));
  }

  std::string wrapper_sym(const ItemTrait* tr, const Item* type_item, std::string_view method, ModuleId mid) const {
    return locator_.mangle_in_module(
        mid,
        "dyncall" + trait_suffix_for_item(tr) + "$for" + type_suffix_for_item(type_item) + "$" + sanitize(method));
  }

  void build_trait_vtables() {
    std::unordered_set<std::string> emitted_wrappers{};
    std::unordered_set<std::string> emitted_vtables{};

    for (const auto& [key, impl_methods] : crate_.trait_impl_methods) {
      if (!key.trait || !key.type) continue;
      auto tset_it = checked_.trait_methods.find(key.trait);
      if (tset_it == checked_.trait_methods.end()) continue;
      const TraitMethodSet& set = tset_it->second;

      ModuleId mid = locator_.module_of(key.type);
      if (!impl_methods.empty() && impl_methods.begin()->second) {
        mid = locator_.module_of(static_cast<const Item*>(impl_methods.begin()->second));
      }
      std::string vt_sym = vtable_global_sym(key.trait, key.type, mid);
      if (emitted_vtables.contains(vt_sym)) continue;
      emitted_vtables.insert(vt_sym);

      std::string vt_ty = llvm_vtable_type_name(key.trait);
      std::ostringstream init;
      init << "@" << vt_sym << " = constant " << vt_ty << " { ";

      for (size_t i = 0; i < set.order.size(); i++) {
        if (i) init << ", ";
        const std::string& mname = set.order[i];
        auto mi = set.methods.find(mname);
        if (mi == set.methods.end()) {
          init << "i8* null";
          continue;
        }
        const TraitMethodInfo& tm = mi->second;
        std::string fnptr_ty = llvm_trait_method_fnptr_type(tm);
        if (!tm.object_safe) {
          init << fnptr_ty << " null";
          continue;
        }
        auto impl_it = impl_methods.find(mname);
        if (impl_it == impl_methods.end() || !impl_it->second) {
          init << fnptr_ty << " null";
          continue;
        }
        const ItemFn* impl_fn = impl_it->second;

        std::string w_sym = wrapper_sym(key.trait, key.type, mname, mid);
        init << fnptr_ty << " @" << w_sym;

        if (!emitted_wrappers.contains(w_sym)) {
          emitted_wrappers.insert(w_sym);
          emit_dyn_wrapper(w_sym, key.trait, key.type, impl_fn, tm);
        }
      }

      init << " }";
      globals_.push_back(init.str());
    }
  }

  void emit_dyn_wrapper(
      const std::string& sym,
      const ItemTrait* tr,
      const Item* type_item,
      const ItemFn* impl_fn,
      const TraitMethodInfo& tm) {
    auto sig_it = checked_.fn_info.find(impl_fn);
    if (sig_it == checked_.fn_info.end()) return;
    const FnInfo& impl_sig = sig_it->second;
    if (impl_sig.params.empty()) return;

    std::string self_ll_ty = llvm_type(impl_sig.params[0]);
    std::string ret_ty = llvm_type(tm.sig.ret);

    std::ostringstream header;
    header << "define " << ret_ty << " @" << sym << "(i8* %self";
    for (size_t i = 1; i < tm.sig.params.size(); i++) {
      header << ", " << llvm_type(tm.sig.params[i]) << " %arg" << (i - 1);
    }
    header << ") {\n";
    header << "entry:\n";
    header << "  %self_typed = bitcast i8* %self to " << self_ll_ty << "\n";

    std::ostringstream args;
    args << self_ll_ty << " %self_typed";
    for (size_t i = 1; i < tm.sig.params.size(); i++) {
      args << ", " << llvm_type(tm.sig.params[i]) << " %arg" << (i - 1);
    }

    if (ret_ty == "void") {
      header << "  call void @" << mangle_fn(impl_fn) << "(" << args.str() << ")\n";
      header << "  ret void\n";
    } else {
      header << "  %r = call " << ret_ty << " @" << mangle_fn(impl_fn) << "(" << args.str() << ")\n";
      header << "  ret " << ret_ty << " %r\n";
    }
    header << "}";
    functions_.push_back(header.str());
    (void)tr;
    (void)type_item;
  }

  void build_function_decls() {
    for (ModuleId mid = 0; mid < crate_.modules.size(); mid++) {
      for (const Item* item : crate_.modules[mid].items) {
        if (!item) continue;
        if (item->kind == AstNodeKind::ItemFn) emit_function(static_cast<const ItemFn*>(item), mid);
        if (item->kind == AstNodeKind::ItemImplInherent) {
          auto* impl = static_cast<const ItemImplInherent*>(item);
          for (const ItemFn* m : impl->methods) emit_function(m, mid);
        }
        if (item->kind == AstNodeKind::ItemImplTrait) {
          auto* impl = static_cast<const ItemImplTrait*>(item);
          for (const ItemFn* m : impl->methods) emit_function(m, mid);
        }
      }
    }
  }

  struct LlvmBlock {
    std::string label{};
    std::vector<std::string> insts{};
    std::optional<std::string> term{};
  };

  struct FnCtx {
    ModuleId mid = 0;
    const ItemFn* fn = nullptr;
    std::vector<std::string> entry_allocas{};
    std::vector<LlvmBlock> blocks{};
    size_t cur = 0;
    std::uint64_t temp = 0;
    std::vector<std::unordered_map<std::string, std::pair<TypeId, std::string>>> scopes{};
    struct LoopTargets {
      std::string break_label{};
      std::string continue_label{};
    };
    std::vector<LoopTargets> loops{};

    void push_scope() { scopes.emplace_back(); }
    void pop_scope() {
      if (!scopes.empty()) scopes.pop_back();
    }
  };

  std::string new_temp(FnCtx& f) { return "%t" + std::to_string(++f.temp); }

  LlvmBlock& cur_block(FnCtx& f) { return f.blocks[f.cur]; }

  void emit_inst(FnCtx& f, std::string line) {
    LlvmBlock& b = cur_block(f);
    if (b.term) return;
    b.insts.push_back(std::move(line));
  }

  void emit_term(FnCtx& f, std::string line) {
    LlvmBlock& b = cur_block(f);
    if (b.term) return;
    b.term = std::move(line);
  }

  size_t new_block(FnCtx& f, std::string label) {
    f.blocks.push_back(LlvmBlock{.label = std::move(label)});
    return f.blocks.size() - 1;
  }

  void set_block(FnCtx& f, size_t idx) { f.cur = idx; }

  std::optional<std::pair<TypeId, std::string>> lookup_local(FnCtx& f, std::string_view name) {
    for (auto it = f.scopes.rbegin(); it != f.scopes.rend(); ++it) {
      auto found = it->find(std::string(name));
      if (found != it->end()) return found->second;
    }
    return std::nullopt;
  }

  void declare_local(FnCtx& f, std::string name, TypeId ty, std::string alloca_name) {
    if (f.scopes.empty()) f.push_scope();
    f.scopes.back().insert({std::move(name), {ty, std::move(alloca_name)}});
  }

  std::optional<size_t> struct_field_index(const ItemStruct* def, std::string_view name) const {
    if (!def) return std::nullopt;
    auto it = checked_.struct_info.find(def);
    if (it == checked_.struct_info.end()) return std::nullopt;
    for (size_t i = 0; i < it->second.fields_in_order.size(); i++) {
      if (it->second.fields_in_order[i].name == name) return i;
    }
    return std::nullopt;
  }

  std::optional<size_t> enum_variant_index(const ItemEnum* def, std::string_view name) const {
    if (!def) return std::nullopt;
    auto it = checked_.enum_info.find(def);
    if (it == checked_.enum_info.end()) return std::nullopt;
    for (size_t i = 0; i < it->second.variants_in_order.size(); i++) {
      if (it->second.variants_in_order[i] == name) return i;
    }
    return std::nullopt;
  }

  const VariantInfo* enum_variant_info(const ItemEnum* def, std::string_view name) const {
    if (!def) return nullptr;
    auto it = checked_.enum_info.find(def);
    if (it == checked_.enum_info.end()) return nullptr;
    auto vit = it->second.variants.find(std::string(name));
    if (vit == it->second.variants.end()) return nullptr;
    return &vit->second;
  }

  LlvmValue emit_enum_ctor(FnCtx& f, TypeId enum_ty, const ItemEnum* def, std::string_view variant, const std::vector<Expr*>& args) {
    auto el = layout_.enum_layout(def, Span{});
    if (!el) return LlvmValue{.ty = llvm_type(enum_ty), .v = "undef"};

    auto vidx = enum_variant_index(def, variant);
    const VariantInfo* vi = enum_variant_info(def, variant);
    if (!vidx || !vi) return LlvmValue{.ty = llvm_type(enum_ty), .v = "undef"};

    std::uint64_t id = ++f.temp;
    std::string slot = "%e" + std::to_string(id);
    std::string llty = llvm_type(enum_ty);
    f.entry_allocas.push_back(slot + " = alloca " + llty);

    std::string tag_ptr = new_temp(f);
    std::string tag_ll_ty = llvm_int_bytes(el->tag_size);
    emit_inst(f, tag_ptr + " = getelementptr inbounds " + llty + ", " + llty + "* " + slot + ", i32 0, i32 0");
    emit_inst(f, "store " + tag_ll_ty + " " + std::to_string(*vidx) + ", " + tag_ll_ty + "* " + tag_ptr);

    if (!vi->payload.empty()) {
      if (args.size() != vi->payload.size()) {
        session_.diags.push_back(Diagnostic{
            .severity = Severity::Error,
            .span = Span{},
            .message = "enum constructor arity mismatch in codegen",
        });
      } else if (el->payload_size != 0) {
        std::uint64_t payload_align = std::max<std::uint64_t>(el->payload_align, 1);
        std::uint64_t payload_words = el->payload_size / payload_align;
        std::string payload_elem_ty = llvm_int_bytes(payload_align);
        std::string payload_field_ty =
            "[" + std::to_string(payload_words) + " x " + payload_elem_ty + "]";

        std::string payload_ptr = new_temp(f);
        emit_inst(
            f,
            payload_ptr + " = getelementptr inbounds " + llty + ", " + llty + "* " + slot + ", i32 0, i32 1");
        std::string payload_i8 = new_temp(f);
        emit_inst(f, payload_i8 + " = bitcast " + payload_field_ty + "* " + payload_ptr + " to i8*");

        std::uint64_t off = 0;
        for (size_t i = 0; i < args.size(); i++) {
          TypeId pt = vi->payload[i];
          auto al = layout_.align_of(pt, Span{});
          auto sz = layout_.size_of(pt, Span{});
          if (!al || !sz) return LlvmValue{.ty = llvm_type(enum_ty), .v = "undef"};
          off = align_up(off, *al);

          LlvmValue av = emit_expr(f, args[i]);
          std::string gep = new_temp(f);
          emit_inst(f, gep + " = getelementptr inbounds i8, i8* " + payload_i8 + ", i64 " + std::to_string(off));
          std::string castp = new_temp(f);
          emit_inst(f, castp + " = bitcast i8* " + gep + " to " + av.ty + "*");
          emit_inst(f, "store " + av.ty + " " + av.v + ", " + av.ty + "* " + castp);

          off += *sz;
        }
      }
    }

    return emit_load(f, enum_ty, slot);
  }

  std::optional<std::string> emit_enum_payload_base_i8(
      FnCtx& f, TypeId enum_ty, const ItemEnum* def, std::string_view enum_ptr, const EnumLayout& el) {
    if (!def) return std::nullopt;
    if (el.payload_size == 0) return std::nullopt;

    std::uint64_t payload_align = std::max<std::uint64_t>(el.payload_align, 1);
    std::uint64_t payload_words = el.payload_size / payload_align;
    std::string payload_elem_ty = llvm_int_bytes(payload_align);
    std::string payload_field_ty =
        "[" + std::to_string(payload_words) + " x " + payload_elem_ty + "]";

    std::string llty = llvm_type(enum_ty);
    std::string payload_ptr = new_temp(f);
    emit_inst(f, payload_ptr + " = getelementptr inbounds " + llty + ", " + llty + "* " + std::string(enum_ptr) + ", i32 0, i32 1");
    std::string payload_i8 = new_temp(f);
    emit_inst(f, payload_i8 + " = bitcast " + payload_field_ty + "* " + payload_ptr + " to i8*");
    return payload_i8;
  }

  LlvmValue emit_pat_test(FnCtx& f, const Pattern* pat, TypeId scrut_ty, std::string_view scrut_ptr) {
    if (!pat) return emit_bool_const(true);

    switch (pat->kind) {
      case AstNodeKind::PatWildcard:
        return emit_bool_const(true);
      case AstNodeKind::PatBinding:
        return emit_bool_const(true);
      case AstNodeKind::PatInt: {
        LlvmValue sv = emit_load(f, scrut_ty, scrut_ptr);
        auto* ip = static_cast<const PatInt*>(pat);
        LlvmValue cv = emit_int_const(scrut_ty, ip->value);
        std::string tmp = new_temp(f);
        emit_inst(f, tmp + " = icmp eq " + sv.ty + " " + sv.v + ", " + cv.v);
        return LlvmValue{.ty = "i1", .v = tmp};
      }
      case AstNodeKind::PatBool: {
        LlvmValue sv = emit_load(f, scrut_ty, scrut_ptr);
        auto* bp = static_cast<const PatBool*>(pat);
        std::string tmp = new_temp(f);
        emit_inst(f, tmp + " = icmp eq i1 " + sv.v + ", " + std::string(bp->value ? "1" : "0"));
        return LlvmValue{.ty = "i1", .v = tmp};
      }
      case AstNodeKind::PatOr: {
        auto* o = static_cast<const PatOr*>(pat);
        LlvmValue lhs = emit_pat_test(f, o->lhs, scrut_ty, scrut_ptr);
        LlvmValue rhs = emit_pat_test(f, o->rhs, scrut_ty, scrut_ptr);
        std::string tmp = new_temp(f);
        emit_inst(f, tmp + " = or i1 " + lhs.v + ", " + rhs.v);
        return LlvmValue{.ty = "i1", .v = tmp};
      }
      case AstNodeKind::PatPath:
      case AstNodeKind::PatVariant: {
        const TypeData& td = checked_.types.get(scrut_ty);
        if (td.kind != TypeKind::Enum || !td.enum_def) {
          session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = pat->span, .message = "enum pattern on non-enum in codegen"});
          return emit_bool_const(false);
        }
        const ItemEnum* def = td.enum_def;
        auto el = layout_.enum_layout(def, Span{});
        if (!el) return emit_bool_const(false);

        const Path* p = nullptr;
        const std::vector<Pattern*>* args = nullptr;
        if (pat->kind == AstNodeKind::PatPath) {
          p = static_cast<const PatPath*>(pat)->path;
        } else {
          auto* vp = static_cast<const PatVariant*>(pat);
          p = vp->path;
          args = &vp->args;
        }
        if (!p || p->segments.empty()) return emit_bool_const(false);
        std::string_view vname = p->segments.back()->text;
        auto vidx = enum_variant_index(def, vname);
        const VariantInfo* vi = enum_variant_info(def, vname);
        if (!vidx || !vi) return emit_bool_const(false);

        std::string llty = llvm_type(scrut_ty);
        std::string tag_ptr = new_temp(f);
        std::string tag_ll_ty = llvm_int_bytes(el->tag_size);
        emit_inst(f, tag_ptr + " = getelementptr inbounds " + llty + ", " + llty + "* " + std::string(scrut_ptr) + ", i32 0, i32 0");
        std::string tag = new_temp(f);
        emit_inst(f, tag + " = load " + tag_ll_ty + ", " + tag_ll_ty + "* " + tag_ptr);
        std::string tag_ok = new_temp(f);
        emit_inst(f, tag_ok + " = icmp eq " + tag_ll_ty + " " + tag + ", " + std::to_string(*vidx));

        // Unit variant has no payload checks.
        if (!args || args->empty()) return LlvmValue{.ty = "i1", .v = tag_ok};

        if (args->size() != vi->payload.size()) {
          session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = pat->span, .message = "variant pattern arity mismatch in codegen"});
          return LlvmValue{.ty = "i1", .v = tag_ok};
        }

        auto payload_base = emit_enum_payload_base_i8(f, scrut_ty, def, scrut_ptr, *el);
        if (!payload_base) return LlvmValue{.ty = "i1", .v = tag_ok};

        std::string cond = tag_ok;
        std::uint64_t off = 0;
        for (size_t i = 0; i < args->size(); i++) {
          const Pattern* ap = (*args)[i];
          if (!ap) continue;
          if (ap->kind == AstNodeKind::PatWildcard || ap->kind == AstNodeKind::PatBinding) {
            auto al = layout_.align_of(vi->payload[i], Span{});
            auto sz = layout_.size_of(vi->payload[i], Span{});
            if (!al || !sz) break;
            off = align_up(off, *al);
            off += *sz;
            continue;
          }
          if (ap->kind != AstNodeKind::PatInt && ap->kind != AstNodeKind::PatBool) {
            session_.diags.push_back(Diagnostic{
                .severity = Severity::Error,
                .span = ap->span,
                .message = "unsupported nested pattern in enum match codegen",
            });
            return emit_bool_const(false);
          }

          TypeId pt = vi->payload[i];
          auto al = layout_.align_of(pt, Span{});
          auto sz = layout_.size_of(pt, Span{});
          if (!al || !sz) break;
          off = align_up(off, *al);

          std::string gep = new_temp(f);
          emit_inst(f, gep + " = getelementptr inbounds i8, i8* " + *payload_base + ", i64 " + std::to_string(off));
          std::string castp = new_temp(f);
          std::string pty = llvm_type(pt);
          emit_inst(f, castp + " = bitcast i8* " + gep + " to " + pty + "*");
          LlvmValue pv = emit_load(f, pt, castp);

          std::string test = new_temp(f);
          if (ap->kind == AstNodeKind::PatInt) {
            auto* ip = static_cast<const PatInt*>(ap);
            LlvmValue cv = emit_int_const(pt, ip->value);
            emit_inst(f, test + " = icmp eq " + pv.ty + " " + pv.v + ", " + cv.v);
          } else {
            auto* bp = static_cast<const PatBool*>(ap);
            emit_inst(f, test + " = icmp eq i1 " + pv.v + ", " + std::string(bp->value ? "1" : "0"));
          }

          std::string both = new_temp(f);
          emit_inst(f, both + " = and i1 " + cond + ", " + test);
          cond = both;

          off += *sz;
        }

        return LlvmValue{.ty = "i1", .v = cond};
      }
      default:
        session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = pat->span, .message = "unsupported pattern in codegen"});
        return emit_bool_const(false);
    }
  }

  void emit_pat_bindings(FnCtx& f, const Pattern* pat, TypeId scrut_ty, std::string_view scrut_ptr) {
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
        std::string slot = "%l" + std::to_string(++f.temp);
        std::string lty = llvm_type(scrut_ty);
        f.entry_allocas.push_back(slot + " = alloca " + lty);
        LlvmValue v = emit_load(f, scrut_ty, scrut_ptr);
        emit_store(f, scrut_ty, slot, v);
        declare_local(f, b->name, scrut_ty, slot);
        return;
      }
      case AstNodeKind::PatVariant: {
        auto* vp = static_cast<const PatVariant*>(pat);
        const TypeData& td = checked_.types.get(scrut_ty);
        if (td.kind != TypeKind::Enum || !td.enum_def) return;
        const ItemEnum* def = td.enum_def;
        if (!vp->path || vp->path->segments.empty()) return;
        std::string_view vname = vp->path->segments.back()->text;
        const VariantInfo* vi = enum_variant_info(def, vname);
        if (!vi) return;
        if (vp->args.size() != vi->payload.size()) return;

        auto el = layout_.enum_layout(def, Span{});
        if (!el) return;
        auto payload_base = emit_enum_payload_base_i8(f, scrut_ty, def, scrut_ptr, *el);
        if (!payload_base) return;

        std::uint64_t off = 0;
        for (size_t i = 0; i < vp->args.size(); i++) {
          TypeId pt = vi->payload[i];
          auto al = layout_.align_of(pt, Span{});
          auto sz = layout_.size_of(pt, Span{});
          if (!al || !sz) return;
          off = align_up(off, *al);

          const Pattern* ap = vp->args[i];
          if (ap) {
            // Only bind `_`/ident patterns for now.
            if (ap->kind == AstNodeKind::PatBinding) {
              std::string gep = new_temp(f);
              emit_inst(f, gep + " = getelementptr inbounds i8, i8* " + *payload_base + ", i64 " + std::to_string(off));
              std::string castp = new_temp(f);
              std::string pty = llvm_type(pt);
              emit_inst(f, castp + " = bitcast i8* " + gep + " to " + pty + "*");

              auto* b = static_cast<const PatBinding*>(ap);
              std::string slot = "%l" + std::to_string(++f.temp);
              f.entry_allocas.push_back(slot + " = alloca " + pty);
              LlvmValue v = emit_load(f, pt, castp);
              emit_store(f, pt, slot, v);
              declare_local(f, b->name, pt, slot);
            } else if (ap->kind != AstNodeKind::PatWildcard && ap->kind != AstNodeKind::PatInt && ap->kind != AstNodeKind::PatBool) {
              session_.diags.push_back(Diagnostic{
                  .severity = Severity::Error,
                  .span = ap->span,
                  .message = "unsupported nested binding pattern in enum match codegen",
              });
            }
          }

          off += *sz;
        }
        return;
      }
      default:
        session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = pat->span, .message = "unsupported binding pattern in codegen"});
        return;
    }
  }

  LlvmValue emit_expr(FnCtx& f, const Expr* e);
  void emit_stmt(FnCtx& f, const Stmt* s);
  LlvmValue emit_block(FnCtx& f, const Block* b);

  std::optional<std::pair<TypeId, std::string>> emit_place_ptr(FnCtx& f, const Expr* e) {
    if (!e) return std::nullopt;
    switch (e->kind) {
      case AstNodeKind::ExprPath: {
        auto* p = static_cast<const ExprPath*>(e);
        if (!p->path || p->path->segments.size() != 1) return std::nullopt;
        auto local = lookup_local(f, p->path->segments[0]->text);
        if (!local) return std::nullopt;
        return std::pair<TypeId, std::string>{local->first, local->second};
      }
      case AstNodeKind::ExprUnary: {
        auto* u = static_cast<const ExprUnary*>(e);
        if (u->op != UnaryOp::Deref) return std::nullopt;
        TypeId pt = type_of(u->expr);
        const TypeData& pd = checked_.types.get(pt);
        if (pd.kind != TypeKind::Ptr) return std::nullopt;
        LlvmValue pv = emit_expr(f, u->expr);
        return std::pair<TypeId, std::string>{pd.pointee, pv.v};
      }
      case AstNodeKind::ExprField: {
        auto* fe = static_cast<const ExprField*>(e);
        TypeId base_ty = type_of(fe->base);
        const TypeData& bd = checked_.types.get(base_ty);

        const ItemStruct* sdef = nullptr;
        TypeId struct_ty = base_ty;
        std::string base_ptr{};

        if (bd.kind == TypeKind::Ptr) {
          struct_ty = bd.pointee;
          const TypeData& sd = checked_.types.get(struct_ty);
          if (sd.kind != TypeKind::Struct || !sd.struct_def) return std::nullopt;
          sdef = sd.struct_def;
          LlvmValue bp = emit_expr(f, fe->base);
          base_ptr = bp.v;
        } else if (bd.kind == TypeKind::Struct) {
          sdef = bd.struct_def;
          if (fe->base->kind == AstNodeKind::ExprPath) {
            auto* p = static_cast<const ExprPath*>(fe->base);
            if (p->path && p->path->segments.size() == 1) {
              auto local = lookup_local(f, p->path->segments[0]->text);
              if (local) base_ptr = local->second;
            }
          }
          if (base_ptr.empty()) return std::nullopt;
        } else {
          return std::nullopt;
        }

        auto idx = struct_field_index(sdef, fe->field);
        if (!idx) return std::nullopt;

        TypeId field_ty = type_of(e);
        std::string tmp = new_temp(f);
        std::string st = llvm_type(struct_ty);
        emit_inst(f, tmp + " = getelementptr inbounds " + st + ", " + st + "* " + base_ptr + ", i32 0, i32 " +
                         std::to_string(*idx));
        return std::pair<TypeId, std::string>{field_ty, tmp};
      }
      default:
        break;
    }
    return std::nullopt;
  }

  LlvmValue emit_load(FnCtx& f, TypeId ty, std::string_view ptr) {
    std::string tmp = new_temp(f);
    std::string lty = llvm_type(ty);
    emit_inst(f, tmp + " = load " + lty + ", " + lty + "* " + std::string(ptr));
    return LlvmValue{.ty = lty, .v = tmp};
  }

  void emit_store(FnCtx& f, TypeId ty, std::string_view ptr, const LlvmValue& v) {
    std::string lty = llvm_type(ty);
    emit_inst(f, "store " + lty + " " + v.v + ", " + lty + "* " + std::string(ptr));
  }

  LlvmValue emit_int_const(TypeId ty, std::int64_t v) {
    const TypeData& d = checked_.types.get(ty);
    if (d.kind != TypeKind::Int) return LlvmValue{.ty = "i32", .v = "0"};
    return LlvmValue{.ty = llvm_int_type(d.int_kind), .v = std::to_string(v)};
  }

  LlvmValue emit_bool_const(bool b) { return LlvmValue{.ty = "i1", .v = b ? "1" : "0"}; }

  LlvmValue emit_cast(FnCtx& f, const LlvmValue& from, TypeId from_ty, TypeId to_ty) {
    const TypeData& fd = checked_.types.get(from_ty);
    const TypeData& td = checked_.types.get(to_ty);
    std::string dst_ty = llvm_type(to_ty);

    // Trait object unsizing cast: `*T as *dyn Trait`.
    if (fd.kind == TypeKind::Ptr && td.kind == TypeKind::Ptr && checked_.types.get(td.pointee).kind == TypeKind::DynTrait) {
      const TypeData& dyn_d = checked_.types.get(td.pointee);
      const ItemTrait* tr = dyn_d.trait_def;
      if (!tr) return LlvmValue{.ty = dst_ty, .v = "undef"};

      const TypeData& from_pointee = checked_.types.get(fd.pointee);
      const Item* type_item = nullptr;
      if (from_pointee.kind == TypeKind::Struct) type_item = static_cast<const Item*>(from_pointee.struct_def);
      if (from_pointee.kind == TypeKind::Enum) type_item = static_cast<const Item*>(from_pointee.enum_def);
      if (!type_item) {
        session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = Span{}, .message = "dyn cast requires a nominal source pointer"});
        return LlvmValue{.ty = dst_ty, .v = "undef"};
      }

      TraitImplKey key{.trait = tr, .type = type_item};
      auto impl_it = crate_.trait_impl_methods.find(key);
      if (impl_it == crate_.trait_impl_methods.end()) {
        session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = Span{}, .message = "no impl for dyn cast target trait"});
        return LlvmValue{.ty = dst_ty, .v = "undef"};
      }

      ModuleId mid = 0;
      if (!impl_it->second.empty() && impl_it->second.begin()->second) {
        mid = locator_.module_of(static_cast<const Item*>(impl_it->second.begin()->second));
      } else {
        mid = locator_.module_of(type_item);
      }
      std::string vt_sym = vtable_global_sym(tr, type_item, mid);
      std::string vt_ty = llvm_vtable_type_name(tr);

      std::string data = new_temp(f);
      emit_inst(f, data + " = bitcast " + from.ty + " " + from.v + " to i8*");

      std::string agg1 = new_temp(f);
      emit_inst(f, agg1 + " = insertvalue " + dst_ty + " undef, i8* " + data + ", 0");
      std::string agg2 = new_temp(f);
      emit_inst(f, agg2 + " = insertvalue " + dst_ty + " " + agg1 + ", " + vt_ty + "* @" + vt_sym + ", 1");
      return LlvmValue{.ty = dst_ty, .v = agg2};
    }

    if (fd.kind == TypeKind::Int && td.kind == TypeKind::Int) {
      std::uint32_t fb = int_bits(fd.int_kind, ptr_bits());
      std::uint32_t tb = int_bits(td.int_kind, ptr_bits());
      if (fb == tb) return LlvmValue{.ty = dst_ty, .v = from.v};
      std::string tmp = new_temp(f);
      bool sign = is_signed_int(fd.int_kind);
      if (fb < tb) {
        emit_inst(f, tmp + " = " + std::string(sign ? "sext" : "zext") + " " + from.ty + " " + from.v + " to " + dst_ty);
      } else {
        emit_inst(f, tmp + " = trunc " + from.ty + " " + from.v + " to " + dst_ty);
      }
      return LlvmValue{.ty = dst_ty, .v = tmp};
    }

    if (fd.kind == TypeKind::Int && td.kind == TypeKind::Ptr) {
      std::string tmp = new_temp(f);
      emit_inst(f, tmp + " = inttoptr " + from.ty + " " + from.v + " to " + dst_ty);
      return LlvmValue{.ty = dst_ty, .v = tmp};
    }

    if (fd.kind == TypeKind::Ptr && td.kind == TypeKind::Int) {
      std::string tmp = new_temp(f);
      emit_inst(f, tmp + " = ptrtoint " + from.ty + " " + from.v + " to " + dst_ty);
      return LlvmValue{.ty = dst_ty, .v = tmp};
    }

    if (fd.kind == TypeKind::Ptr && td.kind == TypeKind::Ptr) {
      std::string tmp = new_temp(f);
      emit_inst(f, tmp + " = bitcast " + from.ty + " " + from.v + " to " + dst_ty);
      return LlvmValue{.ty = dst_ty, .v = tmp};
    }

    return LlvmValue{.ty = dst_ty, .v = from.v};
  }

  std::optional<const ItemFn*> resolve_fn_path(ModuleId mid, const Path* path) const {
    if (!path || path->segments.empty()) return std::nullopt;
    if (path->segments.size() == 1) {
      auto it = crate_.modules[mid].values.find(path->segments[0]->text);
      if (it == crate_.modules[mid].values.end()) return std::nullopt;
      if (it->second->kind == AstNodeKind::ItemFn) return static_cast<const ItemFn*>(it->second);
      return std::nullopt;
    }
    ModuleId cur = mid;
    for (size_t i = 0; i + 1 < path->segments.size(); i++) {
      auto it = crate_.modules[cur].submodules.find(path->segments[i]->text);
      if (it == crate_.modules[cur].submodules.end()) return std::nullopt;
      cur = it->second;
    }
    auto it = crate_.modules[cur].values.find(path->segments.back()->text);
    if (it == crate_.modules[cur].values.end()) return std::nullopt;
    if (it->second->kind == AstNodeKind::ItemFn) return static_cast<const ItemFn*>(it->second);
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

  void emit_function(const ItemFn* fn, ModuleId mid) {
    if (!fn || !fn->decl || !fn->decl->sig || !fn->body) return;
    if (emitted_fns_.contains(fn)) return;
    emitted_fns_.insert(fn);

    auto sig_it = checked_.fn_info.find(fn);
    if (sig_it == checked_.fn_info.end()) return;
    const FnInfo& sig = sig_it->second;

    FnCtx f{};
    f.mid = mid;
    f.fn = fn;
    f.blocks.clear();
    f.entry_allocas.clear();
    f.temp = 0;
    f.scopes.clear();

    size_t entry = new_block(f, "entry");
    set_block(f, entry);
    f.push_scope();

    std::ostringstream header;
    std::string ret_ty = llvm_type(sig.ret);
    header << "define " << ret_ty << " @" << mangle_fn(fn) << "(";
    for (size_t i = 0; i < sig.params.size(); i++) {
      if (i) header << ", ";
      header << llvm_type(sig.params[i]) << " %arg" << i;
    }
    header << ") {\n";

    // Allocate slots for parameters and bind names (so we can treat params like locals).
    size_t idx = 0;
    for (const Param* p : fn->decl->sig->params) {
      if (!p) continue;
      TypeId pty = sig.params.at(idx);
      std::string slot = "%p" + std::to_string(idx);
      std::string lty = llvm_type(pty);
      f.entry_allocas.push_back(slot + " = alloca " + lty);
      emit_inst(f, "store " + lty + " %arg" + std::to_string(idx) + ", " + lty + "* " + slot);
      declare_local(f, p->name, pty, slot);
      idx++;
    }

    LlvmValue body = emit_block(f, fn->body);

    if (!cur_block(f).term) {
      if (checked_.types.equal(sig.ret, checked_.types.unit())) {
        emit_term(f, "ret void");
      } else if (!body.v.empty()) {
        emit_term(f, "ret " + body.ty + " " + body.v);
      } else {
        emit_term(f, "ret " + ret_ty + " 0");
      }
    }

    std::ostringstream out;
    out << header.str();
    for (auto& b : f.blocks) {
      out << b.label << ":\n";
      if (b.label == "entry") {
        for (auto& a : f.entry_allocas) out << "  " << a << "\n";
      }
      for (auto& i : b.insts) out << "  " << i << "\n";
      out << "  " << b.term.value_or("ret void") << "\n";
    }
    out << "}";
    functions_.push_back(out.str());
  }

  void emit_main_shim(std::ostream& out) {
    const Module& root = crate_.modules[crate_.root];
    auto it = root.values.find("main");
    if (it == root.values.end() || !it->second || it->second->kind != AstNodeKind::ItemFn) return;
    auto* main_fn = static_cast<const ItemFn*>(it->second);

    auto sig_it = checked_.fn_info.find(main_fn);
    if (sig_it == checked_.fn_info.end()) return;
    const FnInfo& sig = sig_it->second;
    if (!checked_.types.equal(sig.ret, checked_.types.int_(IntKind::I32)) || !sig.params.empty()) return;

    out << "define i32 @main(i32 %argc, i8** %argv) {\n";
    out << "entry:\n";
    out << "  %r = call i32 @" << mangle_fn(main_fn) << "()\n";
    out << "  ret i32 %r\n";
    out << "}\n\n";
    (void)out;
  }
};

LlvmValue LlvmEmitter::emit_expr(FnCtx& f, const Expr* e) {
  if (!e) return LlvmValue{.ty = "void", .v = ""};
  TypeId ty = type_of(e);
  const TypeData& td = checked_.types.get(ty);

  switch (e->kind) {
    case AstNodeKind::ExprUnit:
      return LlvmValue{.ty = "void", .v = ""};
    case AstNodeKind::ExprBool:
      return emit_bool_const(static_cast<const ExprBool*>(e)->value);
    case AstNodeKind::ExprInt:
      return emit_int_const(ty, static_cast<const ExprInt*>(e)->value);
    case AstNodeKind::ExprStructLit: {
      auto* sl = static_cast<const ExprStructLit*>(e);
      if (td.kind != TypeKind::Struct || !td.struct_def) {
        session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = e->span, .message = "struct literal has non-struct type"});
        return LlvmValue{.ty = llvm_type(ty), .v = "undef"};
      }
      auto it = checked_.struct_info.find(td.struct_def);
      if (it == checked_.struct_info.end()) return LlvmValue{.ty = llvm_type(ty), .v = "undef"};

      std::unordered_map<std::string_view, const Expr*> init_map{};
      for (const FieldInit* fi : sl->inits) {
        if (fi) init_map[fi->name] = fi->value;
      }

      std::string agg = "undef";
      std::string agg_ty = llvm_type(ty);
      for (size_t i = 0; i < it->second.fields_in_order.size(); i++) {
        const auto& fld = it->second.fields_in_order[i];
        auto init_it = init_map.find(fld.name);
        if (init_it == init_map.end() || !init_it->second) {
          session_.diags.push_back(Diagnostic{
              .severity = Severity::Error,
              .span = e->span,
              .message = "missing field initializer `" + fld.name + "` in codegen",
          });
          continue;
        }
        LlvmValue fv = emit_expr(f, init_it->second);
        std::string tmp = new_temp(f);
        emit_inst(f, tmp + " = insertvalue " + agg_ty + " " + agg + ", " + llvm_type(fld.type) + " " + fv.v + ", " +
                         std::to_string(i));
        agg = tmp;
      }
      return LlvmValue{.ty = agg_ty, .v = agg};
    }
    case AstNodeKind::ExprPath: {
      auto* p = static_cast<const ExprPath*>(e);
      if (!p->path || p->path->segments.empty()) return LlvmValue{.ty = "i8", .v = "0"};
      if (p->path->segments.size() == 1) {
        if (auto local = lookup_local(f, p->path->segments[0]->text)) {
          return emit_load(f, local->first, local->second);
        }
      }
      if (const Item* item = resolve_value_item(f.mid, p->path)) {
        if (item->kind == AstNodeKind::ItemConst) {
          auto v = eval_.eval_const(static_cast<const ItemConst*>(item));
          if (!v) return LlvmValue{.ty = llvm_type(ty), .v = "0"};
          if (v->kind == ComptimeValue::Kind::Int) return emit_int_const(ty, v->int_value);
          if (v->kind == ComptimeValue::Kind::Bool) return emit_bool_const(v->bool_value);
        }
      }

      // Enum unit variant value `Enum::Variant`.
      const TypeData& td = checked_.types.get(ty);
      if (td.kind == TypeKind::Enum && td.enum_def && p->path->segments.size() >= 2) {
        std::string_view vname = p->path->segments.back()->text;
        if (const VariantInfo* vi = enum_variant_info(td.enum_def, vname)) {
          if (vi->payload.empty()) {
            std::vector<Expr*> no_args{};
            return emit_enum_ctor(f, ty, td.enum_def, vname, no_args);
          }
        }
      }

      session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = e->span, .message = "unsupported path expression in codegen"});
      return LlvmValue{.ty = llvm_type(ty), .v = "0"};
    }
    case AstNodeKind::ExprBlock:
      return emit_block(f, static_cast<const ExprBlock*>(e)->block);
    case AstNodeKind::ExprIf: {
      auto* iff = static_cast<const ExprIf*>(e);
      LlvmValue cond = emit_expr(f, iff->cond);

      std::uint64_t id = ++f.temp;
      std::string then_label = "then" + std::to_string(id);
      std::string else_label = "else" + std::to_string(id);
      std::string end_label = "endif" + std::to_string(id);

      size_t then_idx = new_block(f, then_label);
      size_t else_idx = new_block(f, else_label);
      size_t end_idx = new_block(f, end_label);

      std::string out_ty = llvm_type(ty);
      std::optional<std::string> out_slot{};
      if (out_ty != "void") {
        out_slot = "%if" + std::to_string(id);
        f.entry_allocas.push_back(*out_slot + " = alloca " + out_ty);
      }

      emit_term(f, "br i1 " + cond.v + ", label %" + then_label + ", label %" + else_label);

      set_block(f, then_idx);
      LlvmValue tv = emit_block(f, iff->then_block);
      if (!cur_block(f).term) {
        if (out_slot) emit_inst(f, "store " + out_ty + " " + tv.v + ", " + out_ty + "* " + *out_slot);
        emit_term(f, "br label %" + end_label);
      }

      set_block(f, else_idx);
      LlvmValue ev{.ty = "void", .v = ""};
      if (iff->else_expr) ev = emit_expr(f, iff->else_expr);
      if (!cur_block(f).term) {
        if (out_slot) emit_inst(f, "store " + out_ty + " " + ev.v + ", " + out_ty + "* " + *out_slot);
        emit_term(f, "br label %" + end_label);
      }

      set_block(f, end_idx);
      if (!out_slot) return LlvmValue{.ty = "void", .v = ""};
      std::string tmp = new_temp(f);
      emit_inst(f, tmp + " = load " + out_ty + ", " + out_ty + "* " + *out_slot);
      return LlvmValue{.ty = out_ty, .v = tmp};
    }
    case AstNodeKind::ExprWhile: {
      auto* wh = static_cast<const ExprWhile*>(e);
      std::uint64_t id = ++f.temp;
      std::string cond_label = "while.cond" + std::to_string(id);
      std::string body_label = "while.body" + std::to_string(id);
      std::string end_label = "while.end" + std::to_string(id);

      size_t cond_idx = new_block(f, cond_label);
      size_t body_idx = new_block(f, body_label);
      size_t end_idx = new_block(f, end_label);

      emit_term(f, "br label %" + cond_label);

      set_block(f, cond_idx);
      LlvmValue cond = emit_expr(f, wh->cond);
      emit_term(f, "br i1 " + cond.v + ", label %" + body_label + ", label %" + end_label);

      set_block(f, body_idx);
      f.loops.push_back(FnCtx::LoopTargets{.break_label = end_label, .continue_label = cond_label});
      (void)emit_block(f, wh->body);
      f.loops.pop_back();
      if (!cur_block(f).term) emit_term(f, "br label %" + cond_label);

      set_block(f, end_idx);
      return LlvmValue{.ty = "void", .v = ""};
    }
    case AstNodeKind::ExprLoop: {
      auto* lp = static_cast<const ExprLoop*>(e);
      std::uint64_t id = ++f.temp;
      std::string body_label = "loop.body" + std::to_string(id);
      std::string end_label = "loop.end" + std::to_string(id);

      size_t body_idx = new_block(f, body_label);
      size_t end_idx = new_block(f, end_label);

      emit_term(f, "br label %" + body_label);

      set_block(f, body_idx);
      f.loops.push_back(FnCtx::LoopTargets{.break_label = end_label, .continue_label = body_label});
      (void)emit_block(f, lp->body);
      f.loops.pop_back();
      if (!cur_block(f).term) emit_term(f, "br label %" + body_label);

      set_block(f, end_idx);
      return LlvmValue{.ty = "void", .v = ""};
    }
    case AstNodeKind::ExprMatch: {
      auto* m = static_cast<const ExprMatch*>(e);

      std::uint64_t id = ++f.temp;
      std::string end_label = "match.end" + std::to_string(id);
      std::string nomatch_label = "match.nomatch" + std::to_string(id);
      size_t end_idx = new_block(f, end_label);
      size_t nomatch_idx = new_block(f, nomatch_label);

      TypeId scrut_ty = type_of(m->scrutinee);
      std::string scrut_ll_ty = llvm_type(scrut_ty);
      std::string scrut_slot = "%match.scrut" + std::to_string(id);
      f.entry_allocas.push_back(scrut_slot + " = alloca " + scrut_ll_ty);
      LlvmValue sv = emit_expr(f, m->scrutinee);
      emit_inst(f, "store " + scrut_ll_ty + " " + sv.v + ", " + scrut_ll_ty + "* " + scrut_slot);

      std::string out_ty = llvm_type(ty);
      std::optional<std::string> out_slot{};
      if (out_ty != "void") {
        out_slot = "%match.out" + std::to_string(id);
        f.entry_allocas.push_back(*out_slot + " = alloca " + out_ty);
      }

      size_t cond_idx = f.cur;
      for (size_t i = 0; i < m->arms.size(); i++) {
        const MatchArm* arm = m->arms[i];
        if (!arm) continue;

        std::string arm_label = "match.arm" + std::to_string(id) + "." + std::to_string(i);
        size_t arm_idx = new_block(f, arm_label);

        std::string next_label = (i + 1 < m->arms.size()) ? ("match.next" + std::to_string(id) + "." + std::to_string(i))
                                                          : nomatch_label;
        size_t next_idx = (i + 1 < m->arms.size()) ? new_block(f, next_label) : nomatch_idx;

        set_block(f, cond_idx);
        LlvmValue ok = emit_pat_test(f, arm->pat, scrut_ty, scrut_slot);
        emit_term(f, "br i1 " + ok.v + ", label %" + arm_label + ", label %" + next_label);

        set_block(f, arm_idx);
        f.push_scope();
        emit_pat_bindings(f, arm->pat, scrut_ty, scrut_slot);

        if (arm->guard) {
          LlvmValue gv = emit_expr(f, arm->guard);
          std::string body_label = "match.body" + std::to_string(id) + "." + std::to_string(i);
          size_t body_idx = new_block(f, body_label);
          emit_term(f, "br i1 " + gv.v + ", label %" + body_label + ", label %" + next_label);

          set_block(f, body_idx);
          LlvmValue rv = emit_expr(f, arm->body);
          if (!cur_block(f).term) {
            if (out_slot) emit_inst(f, "store " + out_ty + " " + rv.v + ", " + out_ty + "* " + *out_slot);
            emit_term(f, "br label %" + end_label);
          }
        } else {
          LlvmValue rv = emit_expr(f, arm->body);
          if (!cur_block(f).term) {
            if (out_slot) emit_inst(f, "store " + out_ty + " " + rv.v + ", " + out_ty + "* " + *out_slot);
            emit_term(f, "br label %" + end_label);
          }
        }

        f.pop_scope();
        cond_idx = next_idx;
      }

      set_block(f, nomatch_idx);
      if (!cur_block(f).term) emit_term(f, "unreachable");

      set_block(f, end_idx);
      if (!out_slot) return LlvmValue{.ty = "void", .v = ""};
      std::string tmp = new_temp(f);
      emit_inst(f, tmp + " = load " + out_ty + ", " + out_ty + "* " + *out_slot);
      return LlvmValue{.ty = out_ty, .v = tmp};
    }
    case AstNodeKind::ExprField: {
      auto place = emit_place_ptr(f, e);
      if (!place) {
        session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = e->span, .message = "unsupported field access in codegen"});
        return LlvmValue{.ty = llvm_type(ty), .v = "0"};
      }
      return emit_load(f, place->first, place->second);
    }
    case AstNodeKind::ExprAssign: {
      auto* a = static_cast<const ExprAssign*>(e);
      if (!a->lhs) return LlvmValue{.ty = "void", .v = ""};
      auto place = emit_place_ptr(f, a->lhs);
      if (!place) {
        session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = e->span, .message = "unsupported assignment target in codegen"});
        return LlvmValue{.ty = "void", .v = ""};
      }
      LlvmValue rv = emit_expr(f, a->rhs);
      emit_store(f, place->first, place->second, rv);
      return LlvmValue{.ty = "void", .v = ""};
    }
    case AstNodeKind::ExprUnary: {
      auto* u = static_cast<const ExprUnary*>(e);
      LlvmValue v = emit_expr(f, u->expr);
      if (u->op == UnaryOp::Neg) {
        std::string tmp = new_temp(f);
        emit_inst(f, tmp + " = sub " + v.ty + " 0, " + v.v);
        return LlvmValue{.ty = v.ty, .v = tmp};
      }
      if (u->op == UnaryOp::Not) {
        std::string tmp = new_temp(f);
        emit_inst(f, tmp + " = xor i1 " + v.v + ", 1");
        return LlvmValue{.ty = "i1", .v = tmp};
      }
      if (u->op == UnaryOp::Deref) {
        TypeId ptr_ty = type_of(u->expr);
        const TypeData& pd = checked_.types.get(ptr_ty);
        if (pd.kind != TypeKind::Ptr) return LlvmValue{.ty = llvm_type(ty), .v = "0"};
        return emit_load(f, pd.pointee, v.v);
      }
      return v;
    }
    case AstNodeKind::ExprBinary: {
      auto* b = static_cast<const ExprBinary*>(e);
      LlvmValue lhs = emit_expr(f, b->lhs);
      LlvmValue rhs = emit_expr(f, b->rhs);
      std::string tmp = new_temp(f);
      TypeId lhs_ty = type_of(b->lhs);
      const TypeData& ld = checked_.types.get(lhs_ty);

      switch (b->op) {
        case BinaryOp::Add:
        case BinaryOp::Sub:
        case BinaryOp::Mul:
        case BinaryOp::Div:
        case BinaryOp::Mod: {
          if (ld.kind != TypeKind::Int) break;
          bool sign = is_signed_int(ld.int_kind);
          switch (b->op) {
            case BinaryOp::Add:
              emit_inst(f, tmp + " = add " + lhs.ty + " " + lhs.v + ", " + rhs.v);
              return LlvmValue{.ty = lhs.ty, .v = tmp};
            case BinaryOp::Sub:
              emit_inst(f, tmp + " = sub " + lhs.ty + " " + lhs.v + ", " + rhs.v);
              return LlvmValue{.ty = lhs.ty, .v = tmp};
            case BinaryOp::Mul:
              emit_inst(f, tmp + " = mul " + lhs.ty + " " + lhs.v + ", " + rhs.v);
              return LlvmValue{.ty = lhs.ty, .v = tmp};
            case BinaryOp::Div:
              emit_inst(f,
                        tmp + " = " + std::string(sign ? "sdiv" : "udiv") + " " + lhs.ty + " " + lhs.v + ", " + rhs.v);
              return LlvmValue{.ty = lhs.ty, .v = tmp};
            case BinaryOp::Mod:
              emit_inst(f,
                        tmp + " = " + std::string(sign ? "srem" : "urem") + " " + lhs.ty + " " + lhs.v + ", " + rhs.v);
              return LlvmValue{.ty = lhs.ty, .v = tmp};
            default:
              break;
          }
          break;
        }
        case BinaryOp::Eq:
          emit_inst(f, tmp + " = icmp eq " + lhs.ty + " " + lhs.v + ", " + rhs.v);
          return LlvmValue{.ty = "i1", .v = tmp};
        case BinaryOp::Ne:
          emit_inst(f, tmp + " = icmp ne " + lhs.ty + " " + lhs.v + ", " + rhs.v);
          return LlvmValue{.ty = "i1", .v = tmp};
        case BinaryOp::Lt:
        case BinaryOp::Le:
        case BinaryOp::Gt:
        case BinaryOp::Ge: {
          if (ld.kind == TypeKind::Int) {
            bool sign = is_signed_int(ld.int_kind);
            std::string pred{};
            if (b->op == BinaryOp::Lt) pred = sign ? "slt" : "ult";
            if (b->op == BinaryOp::Le) pred = sign ? "sle" : "ule";
            if (b->op == BinaryOp::Gt) pred = sign ? "sgt" : "ugt";
            if (b->op == BinaryOp::Ge) pred = sign ? "sge" : "uge";
            emit_inst(f, tmp + " = icmp " + pred + " " + lhs.ty + " " + lhs.v + ", " + rhs.v);
            return LlvmValue{.ty = "i1", .v = tmp};
          }
          if (ld.kind == TypeKind::Bool) {
            std::string pred{};
            if (b->op == BinaryOp::Lt) pred = "ult";
            if (b->op == BinaryOp::Le) pred = "ule";
            if (b->op == BinaryOp::Gt) pred = "ugt";
            if (b->op == BinaryOp::Ge) pred = "uge";
            emit_inst(f, tmp + " = icmp " + pred + " i1 " + lhs.v + ", " + rhs.v);
            return LlvmValue{.ty = "i1", .v = tmp};
          }
          break;
        }
        case BinaryOp::And:
          emit_inst(f, tmp + " = and i1 " + lhs.v + ", " + rhs.v);
          return LlvmValue{.ty = "i1", .v = tmp};
        case BinaryOp::Or:
          emit_inst(f, tmp + " = or i1 " + lhs.v + ", " + rhs.v);
          return LlvmValue{.ty = "i1", .v = tmp};
      }

      session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = e->span, .message = "unsupported binary op in codegen"});
      return LlvmValue{.ty = llvm_type(ty), .v = "0"};
    }
    case AstNodeKind::ExprCast: {
      auto* c = static_cast<const ExprCast*>(e);
      TypeId from_ty = type_of(c->value);
      LlvmValue from = emit_expr(f, c->value);
      // Checker already computed the resulting expression type in `expr_types_`.
      return emit_cast(f, from, from_ty, ty);
    }
    case AstNodeKind::ExprMethodCall: {
      auto* mc = static_cast<const ExprMethodCall*>(e);
      if (!mc->receiver) return LlvmValue{.ty = llvm_type(ty), .v = "0"};
      TypeId recv_ty = type_of(mc->receiver);
      const TypeData& rd = checked_.types.get(recv_ty);

      TypeId nominal = recv_ty;
      if (rd.kind == TypeKind::Ptr) nominal = rd.pointee;

      const TypeData& nd = checked_.types.get(nominal);
      if (nd.kind == TypeKind::DynTrait && nd.trait_def) {
        const ItemTrait* tr = nd.trait_def;
        auto tset_it = checked_.trait_methods.find(tr);
        if (tset_it == checked_.trait_methods.end()) {
          session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = e->span, .message = "unknown trait in dyn call codegen"});
          return LlvmValue{.ty = llvm_type(ty), .v = "0"};
        }
        const TraitMethodSet& set = tset_it->second;
        auto mi = set.methods.find(mc->method);
        if (mi == set.methods.end()) {
          session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = e->span, .message = "unknown dyn method in codegen"});
          return LlvmValue{.ty = llvm_type(ty), .v = "0"};
        }
        const TraitMethodInfo& tm = mi->second;
        if (!tm.object_safe) {
          session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = e->span, .message = "dyn call to non-object-safe method"});
          return LlvmValue{.ty = llvm_type(ty), .v = "0"};
        }

        size_t idx = 0;
        bool found = false;
        for (size_t i = 0; i < set.order.size(); i++) {
          if (set.order[i] == mc->method) {
            idx = i;
            found = true;
            break;
          }
        }
        if (!found) return LlvmValue{.ty = llvm_type(ty), .v = "0"};

        LlvmValue recv = emit_expr(f, mc->receiver);
        std::string data = new_temp(f);
        emit_inst(f, data + " = extractvalue " + recv.ty + " " + recv.v + ", 0");
        std::string vt = new_temp(f);
        emit_inst(f, vt + " = extractvalue " + recv.ty + " " + recv.v + ", 1");

        std::string vt_ty = llvm_vtable_type_name(tr);
        std::string fnptr_ty = llvm_trait_method_fnptr_type(tm);

        std::string gep = new_temp(f);
        emit_inst(f, gep + " = getelementptr inbounds " + vt_ty + ", " + vt_ty + "* " + vt + ", i32 0, i32 " + std::to_string(idx));
        std::string fn = new_temp(f);
        emit_inst(f, fn + " = load " + fnptr_ty + ", " + fnptr_ty + "* " + gep);

        std::ostringstream args;
        args << "i8* " << data;
        for (size_t i = 0; i < mc->args.size(); i++) {
          LlvmValue av = emit_expr(f, mc->args[i]);
          args << ", " << av.ty << " " << av.v;
        }

        std::string ret_ty = llvm_type(tm.sig.ret);
        if (ret_ty == "void") {
          emit_inst(f, "call void " + fn + "(" + args.str() + ")");
          return LlvmValue{.ty = "void", .v = ""};
        }
        std::string tmp = new_temp(f);
        emit_inst(f, tmp + " = call " + ret_ty + " " + fn + "(" + args.str() + ")");
        return LlvmValue{.ty = ret_ty, .v = tmp};
      }

      const Item* def = nullptr;
      if (nd.kind == TypeKind::Struct) def = static_cast<const Item*>(nd.struct_def);
      if (nd.kind == TypeKind::Enum) def = static_cast<const Item*>(nd.enum_def);
      if (!def) {
        session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = e->span, .message = "unsupported receiver type in method call codegen"});
        return LlvmValue{.ty = llvm_type(ty), .v = "0"};
      }

      const ItemFn* method = nullptr;
      if (auto it = crate_.inherent_methods.find(def); it != crate_.inherent_methods.end()) {
        if (auto mi = it->second.find(mc->method); mi != it->second.end()) method = mi->second;
      }

      if (!method) {
        const Module& m = crate_.modules[f.mid];
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
        session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = e->span, .message = "cannot resolve method in codegen"});
        return LlvmValue{.ty = llvm_type(ty), .v = "0"};
      }

      auto sig_it = checked_.fn_info.find(method);
      if (sig_it == checked_.fn_info.end()) return LlvmValue{.ty = llvm_type(ty), .v = "0"};
      const FnInfo& sig = sig_it->second;

      if (sig.params.empty()) {
        session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = e->span, .message = "method has no receiver param"});
        return LlvmValue{.ty = llvm_type(ty), .v = "0"};
      }

      TypeId self_param = sig.params[0];
      const TypeData& sp = checked_.types.get(self_param);

      LlvmValue recv_arg{};
      if (sp.kind == TypeKind::Ptr && rd.kind != TypeKind::Ptr) {
        if (mc->receiver->kind != AstNodeKind::ExprPath) {
          session_.diags.push_back(Diagnostic{
              .severity = Severity::Error,
              .span = mc->receiver->span,
              .message = "codegen v0 only supports implicit borrow from local paths",
          });
          return LlvmValue{.ty = llvm_type(ty), .v = "0"};
        }
        const Path* p = static_cast<const ExprPath*>(mc->receiver)->path;
        if (!p || p->segments.size() != 1) return LlvmValue{.ty = llvm_type(ty), .v = "0"};
        auto local = lookup_local(f, p->segments[0]->text);
        if (!local) return LlvmValue{.ty = llvm_type(ty), .v = "0"};
        recv_arg = LlvmValue{.ty = llvm_type(self_param), .v = local->second};
      } else {
        recv_arg = emit_expr(f, mc->receiver);
      }

      std::ostringstream args;
      args << recv_arg.ty << " " << recv_arg.v;
      for (size_t i = 0; i < mc->args.size(); i++) {
        LlvmValue av = emit_expr(f, mc->args[i]);
        args << ", " << av.ty << " " << av.v;
      }

      std::string ret_ty = llvm_type(sig.ret);
      if (ret_ty == "void") {
        emit_inst(f, "call void @" + mangle_fn(method) + "(" + args.str() + ")");
        return LlvmValue{.ty = "void", .v = ""};
      }
      std::string tmp = new_temp(f);
      emit_inst(f, tmp + " = call " + ret_ty + " @" + mangle_fn(method) + "(" + args.str() + ")");
      return LlvmValue{.ty = ret_ty, .v = tmp};
    }
    case AstNodeKind::ExprCall: {
      auto* call = static_cast<const ExprCall*>(e);
      if (!call->callee || call->callee->kind != AstNodeKind::ExprPath) {
        session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = e->span, .message = "unsupported call target in codegen"});
        return LlvmValue{.ty = llvm_type(ty), .v = "0"};
      }
      const Path* callee = static_cast<const ExprPath*>(call->callee)->path;
      if (!callee) return LlvmValue{.ty = llvm_type(ty), .v = "0"};

      if (callee->segments.size() == 2 && callee->segments[0]->text == "builtin" && callee->segments[1]->text == "size_of") {
        if (call->args.size() != 1) return LlvmValue{.ty = llvm_type(ty), .v = "0"};
        if (call->args[0]->kind != AstNodeKind::ExprPath) return LlvmValue{.ty = llvm_type(ty), .v = "0"};
        auto* tp = static_cast<const ExprPath*>(call->args[0])->path;
        TypeId t = const_cast<TypeStore&>(checked_.types).error();
        if (tp) t = const_cast<TypeStore&>(checked_.types).error();
        (void)t;
        // Prefer comptime evaluator (it already knows how to lower).
        auto v = eval_.eval_expr(f.mid, e);
        if (v && v->kind == ComptimeValue::Kind::Int) return LlvmValue{.ty = llvm_type(ty), .v = std::to_string(v->int_value)};
        return LlvmValue{.ty = llvm_type(ty), .v = "0"};
      }

      if (callee->segments.size() == 2 && callee->segments[0]->text == "builtin" && callee->segments[1]->text == "align_of") {
        auto v = eval_.eval_expr(f.mid, e);
        if (v && v->kind == ComptimeValue::Kind::Int) return LlvmValue{.ty = llvm_type(ty), .v = std::to_string(v->int_value)};
        return LlvmValue{.ty = llvm_type(ty), .v = "0"};
      }

      if (callee->segments.size() == 2 && callee->segments[0]->text == "builtin" && (callee->segments[1]->text == "addr_of" ||
                                                                                       callee->segments[1]->text == "addr_of_mut")) {
        if (call->args.size() != 1) return LlvmValue{.ty = llvm_type(ty), .v = "0"};
        if (call->args[0]->kind == AstNodeKind::ExprPath) {
          auto* ap = static_cast<const ExprPath*>(call->args[0])->path;
          if (ap && ap->segments.size() == 1) {
            auto local = lookup_local(f, ap->segments[0]->text);
            if (local) return LlvmValue{.ty = llvm_type(ty), .v = local->second};
          }
        }
        session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = e->span, .message = "builtin::addr_of requires a local path in codegen v0"});
        return LlvmValue{.ty = llvm_type(ty), .v = "0"};
      }

      if (auto fn = resolve_fn_path(f.mid, callee)) {
        auto sig_it = checked_.fn_info.find(*fn);
        if (sig_it == checked_.fn_info.end()) return LlvmValue{.ty = llvm_type(ty), .v = "0"};
        const FnInfo& sig = sig_it->second;
        std::ostringstream args;
        for (size_t i = 0; i < call->args.size(); i++) {
          LlvmValue av = emit_expr(f, call->args[i]);
          if (i) args << ", ";
          args << av.ty << " " << av.v;
        }
        std::string ret_ty = llvm_type(sig.ret);
        if (ret_ty == "void") {
          emit_inst(f, "call void @" + mangle_fn(*fn) + "(" + args.str() + ")");
          return LlvmValue{.ty = "void", .v = ""};
        }
        std::string tmp = new_temp(f);
        emit_inst(f, tmp + " = call " + ret_ty + " @" + mangle_fn(*fn) + "(" + args.str() + ")");
        return LlvmValue{.ty = ret_ty, .v = tmp};
      }

      // Enum variant constructor `Enum::Variant(...)`.
      const TypeData& td = checked_.types.get(ty);
      if (td.kind == TypeKind::Enum && td.enum_def && callee->segments.size() >= 2) {
        std::string_view vname = callee->segments.back()->text;
        if (const VariantInfo* vi = enum_variant_info(td.enum_def, vname)) {
          if (vi->payload.size() == call->args.size()) return emit_enum_ctor(f, ty, td.enum_def, vname, call->args);
        }
      }

      session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = e->span, .message = "unresolved call in codegen"});
      return LlvmValue{.ty = llvm_type(ty), .v = "0"};
    }
    default:
      session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = e->span, .message = "unsupported expression in codegen"});
      return LlvmValue{.ty = llvm_type(ty), .v = "0"};
  }
}

void LlvmEmitter::emit_stmt(FnCtx& f, const Stmt* s) {
  if (!s) return;
  if (cur_block(f).term) return;
  switch (s->kind) {
    case AstNodeKind::StmtLet: {
      auto* ls = static_cast<const StmtLet*>(s);
      if (!ls->pat || ls->pat->kind != AstNodeKind::PatBinding) {
        session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = s->span, .message = "codegen only supports `let <ident>` patterns for now"});
        return;
      }
      auto* b = static_cast<const PatBinding*>(ls->pat);
      TypeId ty = checked_.types.error();
      if (ls->init) {
        ty = type_of(ls->init);
      } else {
        session_.diags.push_back(Diagnostic{
            .severity = Severity::Error,
            .span = s->span,
            .message = "codegen requires `let` initializers for now",
        });
      }
      std::string slot = "%l" + std::to_string(++f.temp);
      std::string lty = llvm_type(ty);
      f.entry_allocas.push_back(slot + " = alloca " + lty);
      declare_local(f, b->name, ty, slot);
      if (ls->init) {
        LlvmValue init = emit_expr(f, ls->init);
        emit_store(f, ty, slot, init);
      }
      return;
    }
    case AstNodeKind::StmtExpr:
      (void)emit_expr(f, static_cast<const StmtExpr*>(s)->expr);
      return;
    case AstNodeKind::StmtReturn: {
      auto* r = static_cast<const StmtReturn*>(s);
      TypeId ret_ty = checked_.fn_info.at(f.fn).ret;
      if (checked_.types.equal(ret_ty, checked_.types.unit())) {
        emit_term(f, "ret void");
        return;
      }
      if (!r->value) {
        emit_term(f, "ret " + llvm_type(ret_ty) + " 0");
        return;
      }
      LlvmValue v = emit_expr(f, r->value);
      emit_term(f, "ret " + v.ty + " " + v.v);
      return;
    }
    case AstNodeKind::StmtBreak: {
      auto* br = static_cast<const StmtBreak*>(s);
      if (br->value) (void)emit_expr(f, br->value);
      if (f.loops.empty()) {
        session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = s->span, .message = "`break` outside of a loop (codegen)"});
        emit_term(f, "unreachable");
        return;
      }
      emit_term(f, "br label %" + f.loops.back().break_label);
      return;
    }
    case AstNodeKind::StmtContinue: {
      if (f.loops.empty()) {
        session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = s->span, .message = "`continue` outside of a loop (codegen)"});
        emit_term(f, "unreachable");
        return;
      }
      emit_term(f, "br label %" + f.loops.back().continue_label);
      return;
    }
    default:
      session_.diags.push_back(Diagnostic{.severity = Severity::Error, .span = s->span, .message = "unsupported statement in codegen"});
      return;
  }
}

LlvmValue LlvmEmitter::emit_block(FnCtx& f, const Block* b) {
  if (!b) return LlvmValue{.ty = "void", .v = ""};
  f.push_scope();
  for (const Stmt* s : b->stmts) emit_stmt(f, s);
  LlvmValue out{.ty = "void", .v = ""};
  if (b->tail && !cur_block(f).term) out = emit_expr(f, b->tail);
  f.pop_scope();
  return out;
}

}  // namespace

bool emit_llvm_ir(Session& session, const ResolvedCrate& crate, CheckedCrate& checked, const EmitLlvmOptions& opts) {
  LlvmEmitter emitter(session, crate, checked);
  return emitter.emit_to_file(opts.out_ll, opts.emit_main_wrapper);
}

}  // namespace cog
