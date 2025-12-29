#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "ast.hpp"
#include "sem.hpp"
#include "resolve.hpp"
#include "session.hpp"
#include "types.hpp"

namespace cog {

class LayoutEngine;
class TypeStore;

struct TypeConstructContext {
    // Shared cache: key -> constructed type.
    // The key is a compiler-internal, stable-within-compilation string derived
    // from the full descriptor values.
    std::unordered_map<std::string, TypeId>* cache = nullptr;

    // Storage for synthetic nominal items (allocated into `ResolvedCrate::builtins_arena`).
    AstArena* arena = nullptr;

    // Side tables used by layout/codegen for nominal types.
    std::unordered_map<const ItemStruct*, StructInfo>* struct_info = nullptr;
    std::unordered_map<const ItemEnum*, EnumInfo>* enum_info = nullptr;
};

struct ComptimeValue {
    enum class Kind : std::uint8_t {
        Error,
        Unit,
        Bool,
        Int,
        Float,
        Type,
        // Raw pointer value (e.g. from integer↔pointer casts).
        Ptr,
        // Reference into the comptime heap (e.g. from `&place`).
        Ref,
        Array,
        Tuple,
        Struct,
        Enum,
        String,
    };

    Kind kind = Kind::Error;

    bool bool_value = false;
    std::int64_t int_value = 0;
    double float_value = 0.0;
    TypeId type_value = 0;
    std::uint64_t ptr_value = 0;

    std::string string_value{};

    // Array
    std::vector<ComptimeValue> array_elems{};

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
    static ComptimeValue bool_(bool b) {
        return ComptimeValue{.kind = Kind::Bool, .bool_value = b};
    }
    static ComptimeValue int_(std::int64_t i) {
        return ComptimeValue{.kind = Kind::Int, .int_value = i};
    }
    static ComptimeValue float_(double f) {
        return ComptimeValue{.kind = Kind::Float, .float_value = f};
    }
    static ComptimeValue type_(TypeId t) {
        return ComptimeValue{.kind = Kind::Type, .type_value = t};
    }
    static ComptimeValue ptr_(std::uint64_t p) {
        return ComptimeValue{.kind = Kind::Ptr, .ptr_value = p};
    }
    static ComptimeValue ref_(std::uint64_t p) {
        return ComptimeValue{.kind = Kind::Ref, .ptr_value = p};
    }
    static ComptimeValue array(std::vector<ComptimeValue> elems) {
        ComptimeValue v{};
        v.kind = Kind::Array;
        v.array_elems = std::move(elems);
        return v;
    }
    static ComptimeValue string(std::string s) {
        ComptimeValue v{};
        v.kind = Kind::String;
        v.string_value = std::move(s);
        return v;
    }
};

class ComptimeEvaluator {
   public:
    explicit ComptimeEvaluator(Session& session, const ResolvedCrate& crate,
                               TypeStore* types = nullptr,
                               LayoutEngine* layout = nullptr,
                               TypeConstructContext type_ctx = {});

    // Evaluate a const/static value. The defining module context is used for
    // name resolution.
    std::optional<ComptimeValue> eval_const(const ItemConst* c);
    std::optional<ComptimeValue> eval_static(const ItemStatic* s);

    // Evaluate an expression in a specific module context.
    std::optional<ComptimeValue> eval_expr(ModuleId mid, const Expr* expr);

    // Convenience for array lengths.
    std::optional<std::uint64_t> eval_usize(ModuleId mid, const Expr* expr);

    // Evaluate a function call at comptime with already-evaluated arguments.
    std::optional<ComptimeValue> eval_fn(const ItemFn* fn,
                                         std::vector<ComptimeValue> args,
                                         Span use_site);

   private:
    Session& session_;
    const ResolvedCrate& crate_;
    TypeStore* types_ = nullptr;
    LayoutEngine* layout_ = nullptr;
    TypeConstructContext type_ctx_{};
    const ItemStruct* typeinfo_struct_ = nullptr;
    const ItemEnum* typekind_enum_ = nullptr;

    enum class CacheState : std::uint8_t { InProgress, Done };

    struct CachedValue {
        CacheState state = CacheState::InProgress;
        ComptimeValue value{};
    };

    std::unordered_map<const ItemConst*, ModuleId> const_module_{};
    std::unordered_map<const ItemStatic*, ModuleId> static_module_{};
    std::unordered_map<const ItemFn*, ModuleId> fn_module_{};

    std::unordered_map<const ItemConst*, CachedValue> const_cache_{};
    std::unordered_map<const ItemStatic*, CachedValue> static_cache_{};

    std::uint64_t step_budget_ = 1'000'000;
    std::uint32_t recursion_depth_ = 0;
    std::uint32_t recursion_limit_ = 256;
    std::uint64_t heap_budget_units_ = 1'000'000;

    void error(Span span, std::string message);
    bool consume_step(Span span);
    bool consume_heap(Span span, std::uint64_t units);

    // ---- Comptime heap / refs ----
    struct HeapObject {
        ComptimeValue value{};
    };
    std::vector<HeapObject> heap_{};
    std::optional<std::uint64_t> heap_alloc(Span span, ComptimeValue v);
    const ComptimeValue* heap_deref(Span span, const ComptimeValue& ref);

    // ---- Canonicalization helpers ----
    std::string type_key(TypeId t);
    std::string value_key(const ComptimeValue& v,
                          std::unordered_set<std::uint64_t>& visiting);

    // ---- Name resolution helpers (const-eval only) ----
    const Item* resolve_type_item(ModuleId mid, const Path* path);
    const Item* resolve_value_item(ModuleId mid, const Path* path);

    const ItemStruct* resolve_struct(ModuleId mid, const Path* path);
    const ItemEnum* resolve_enum(ModuleId mid, const Path* path);

    const ItemStruct* find_struct_global(std::string_view name);
    const ItemEnum* find_enum_in_builtin_module(std::string_view name);

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

    std::optional<Flow> eval_expr_inner(ModuleId mid, const Expr* expr,
                                        Env& env, int loop_depth);
    std::optional<Flow> eval_block(ModuleId mid, const Block* block, Env& env,
                                   int loop_depth);
    std::optional<Flow> eval_stmt(ModuleId mid, const Stmt* stmt, Env& env,
                                  int loop_depth);

    bool bind_pattern(ModuleId mid, const Pattern* pat, const ComptimeValue& v,
                      Env& env);
    bool match_pattern(ModuleId mid, const Pattern* pat, const ComptimeValue& v,
                       Env& env);

    // ---- Type lowering helpers (needed for size/align builtins) ----
    TypeId lower_type(ModuleId mid, const Type* ty, bool allow_unsized);
    TypeId lower_type_path(ModuleId mid, const Path* path, bool allow_unsized);
    std::optional<TypeId> lower_type_value_expr(ModuleId mid, const Expr* expr);
};

}  // namespace cog
