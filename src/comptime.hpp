#pragma once

#include "ast.hpp"
#include "resolve.hpp"
#include "session.hpp"

#include <cstdint>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

namespace cog {

struct ComptimeValue {
  enum class Kind : std::uint8_t {
    Error,
    Unit,
    Bool,
    Int,
    Ptr,
    Tuple,
    Struct,
    Enum,
    String,
  };

  Kind kind = Kind::Error;

  bool bool_value = false;
  std::int64_t int_value = 0;
  std::uint64_t ptr_value = 0;

  std::string string_value{};

  // Tuple
  std::vector<ComptimeValue> tuple_elems{};

  // Struct
  const ItemStruct* struct_def = nullptr;
  std::unordered_map<std::string, ComptimeValue> struct_fields{};

  // Enum
  const ItemEnum* enum_def = nullptr;
  std::string enum_variant{};
  std::vector<ComptimeValue> enum_payload{};

  static ComptimeValue error() { return ComptimeValue{.kind = Kind::Error}; }
  static ComptimeValue unit() { return ComptimeValue{.kind = Kind::Unit}; }
  static ComptimeValue bool_(bool b) { return ComptimeValue{.kind = Kind::Bool, .bool_value = b}; }
  static ComptimeValue int_(std::int64_t i) { return ComptimeValue{.kind = Kind::Int, .int_value = i}; }
  static ComptimeValue ptr_(std::uint64_t p) { return ComptimeValue{.kind = Kind::Ptr, .ptr_value = p}; }
  static ComptimeValue string(std::string s) {
    ComptimeValue v{};
    v.kind = Kind::String;
    v.string_value = std::move(s);
    return v;
  }
};

class ComptimeEvaluator {
 public:
  explicit ComptimeEvaluator(Session& session, const ResolvedCrate& crate);

  // Evaluate a const/static value. The defining module context is used for name resolution.
  std::optional<ComptimeValue> eval_const(const ItemConst* c);
  std::optional<ComptimeValue> eval_static(const ItemStatic* s);

  // Evaluate an expression in a specific module context.
  std::optional<ComptimeValue> eval_expr(ModuleId mid, const Expr* expr);

  // Convenience for array lengths.
  std::optional<std::uint64_t> eval_usize(ModuleId mid, const Expr* expr);

 private:
  Session& session_;
  const ResolvedCrate& crate_;

  enum class CacheState : std::uint8_t { InProgress, Done };

  struct CachedValue {
    CacheState state = CacheState::InProgress;
    ComptimeValue value{};
  };

  std::unordered_map<const ItemConst*, ModuleId> const_module_{};
  std::unordered_map<const ItemStatic*, ModuleId> static_module_{};

  std::unordered_map<const ItemConst*, CachedValue> const_cache_{};
  std::unordered_map<const ItemStatic*, CachedValue> static_cache_{};

  std::uint64_t step_budget_ = 1'000'000;

  void error(Span span, std::string message);
  bool consume_step(Span span);

  // ---- Name resolution helpers (const-eval only) ----
  const Item* resolve_type_item(ModuleId mid, const Path* path);
  const Item* resolve_value_item(ModuleId mid, const Path* path);

  const ItemStruct* resolve_struct(ModuleId mid, const Path* path);
  const ItemEnum* resolve_enum(ModuleId mid, const Path* path);

  struct ResolvedVariant {
    const ItemEnum* def = nullptr;
    const VariantDecl* variant = nullptr;
  };
  ResolvedVariant resolve_variant(ModuleId mid, const Path* path);

  // ---- Interpreter ----
  struct Binding {
    ComptimeValue value{};
    bool is_mut = false;
  };

  struct Env {
    std::vector<std::unordered_map<std::string, Binding>> scopes{};
    Env() { scopes.emplace_back(); }
    void push_scope() { scopes.emplace_back(); }
    void pop_scope() {
      if (scopes.size() > 1) scopes.pop_back();
    }
    Binding* lookup(std::string_view name);
    void declare(std::string name, Binding b);
  };

  enum class Control : std::uint8_t { None, Return, Break, Continue };

  struct Flow {
    Control control = Control::None;
    ComptimeValue value{};  // valid for Return/Break, ignored otherwise
  };

  std::optional<Flow> eval_expr_inner(ModuleId mid, const Expr* expr, Env& env, int loop_depth);
  std::optional<Flow> eval_block(ModuleId mid, const Block* block, Env& env, int loop_depth);
  std::optional<Flow> eval_stmt(ModuleId mid, const Stmt* stmt, Env& env, int loop_depth);

  bool bind_pattern(ModuleId mid, const Pattern* pat, const ComptimeValue& v, Env& env);
  bool match_pattern(ModuleId mid, const Pattern* pat, const ComptimeValue& v, Env& env);
};

}  // namespace cog

