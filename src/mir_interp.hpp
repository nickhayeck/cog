#pragma once

#include <cstdint>
#include <functional>
#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "comptime.hpp"
#include "layout.hpp"
#include "mir.hpp"
#include "session.hpp"
#include "target.hpp"

namespace cog {

// Executes MIR at compile time to produce ComptimeValue results.
//
// v0.0.20 goals:
// - evaluate const/static initializers on MIR
// - evaluate comptime-only code paths (e.g. compile_error) consistently
// - provide a single semantics engine shared by comptime and (eventually)
//   runtime codegen
class MirInterpreter {
   public:
    explicit MirInterpreter(Session& session, const MirProgram& program,
                            TargetLayout target_layout);

    std::optional<ComptimeValue> eval_const(DefId def);
    std::optional<ComptimeValue> eval_static(DefId def);

    std::optional<ComptimeValue> eval_fn(DefId def,
                                         const std::vector<ComptimeValue>& args,
                                         Span use_site);

   private:
    Session& session_;
    const MirProgram& program_;
    const HirCrate& hir_;
    const ResolvedCrate& crate_;
    CheckedCrate& checked_;
    TypeStore& types_;

    LayoutEngine layout_;

    TypeConstructContext type_ctx_{};
    const ItemStruct* typeinfo_struct_ = nullptr;
    const ItemEnum* typekind_enum_ = nullptr;

    enum class CacheState : std::uint8_t { InProgress, Done };
    struct CachedValue {
        CacheState state = CacheState::InProgress;
        ComptimeValue value{};
    };
    std::unordered_map<DefId, CachedValue> const_cache_{};
    std::unordered_map<DefId, CachedValue> static_cache_{};

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

    std::optional<ComptimeValue> eval_body(
        const MirBody& body, const std::vector<ComptimeValue>& args,
        Span use_site);

    std::optional<ComptimeValue> eval_operand(
        const MirBody& body, std::vector<ComptimeValue>& locals,
        const MirOperand& op);
    std::optional<ComptimeValue> eval_rvalue(const MirBody& body,
                                             std::vector<ComptimeValue>& locals,
                                             const MirRvalue& rv);

    std::optional<ComptimeValue> read_place(const MirBody& body,
                                            std::vector<ComptimeValue>& locals,
                                            const MirPlace& place);
    ComptimeValue* place_ref(const MirBody& body,
                             std::vector<ComptimeValue>& locals,
                             const MirPlace& place);

    std::optional<std::int64_t> as_int(Span span, const ComptimeValue& v);
    std::optional<bool> as_bool(Span span, const ComptimeValue& v);

    std::optional<TypeId> as_type(Span span, const ComptimeValue& v);
    const std::vector<ComptimeValue>* as_array_elems(Span span,
                                                     const ComptimeValue& v);

    const ItemStruct* find_struct_global(std::string_view name) const;
    const ItemEnum* find_enum_in_builtin_module(std::string_view name) const;
};

}  // namespace cog
