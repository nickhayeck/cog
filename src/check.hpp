#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <unordered_map>

#include "resolve.hpp"
#include "sem.hpp"
#include "session.hpp"
#include "types.hpp"

namespace cog {

struct TargetLayout;

// A monomorphic specialization of an `auto`-polymorphic function (v0.0.24).
//
// `auto`-polymorphic functions do not have a single monomorphic signature.
// Instead, each call site instantiates (and caches) a concrete signature,
// which is then lowered through MIR → LLVM like an ordinary function.
struct AutoFnInstance {
    enum class State : std::uint8_t { InProgress, Done };

    State state = State::InProgress;
    std::uint32_t def = 0;  // compiler-internal DefId (not present in HIR)
    ModuleId module = 0;
    const ItemFn* base = nullptr;  // AST item being specialized

    FnInfo sig{};

    // Per-instance type tables. The AST nodes are shared across all
    // instantiations, so expression/binding types must be stored per instance.
    std::unordered_map<const PatBinding*, TypeId> binding_types{};
    std::unordered_map<const Expr*, TypeId> expr_types{};

    // Per-instance call-target tables for calls inside polymorphic bodies.
    std::unordered_map<const ExprCall*, std::uint32_t> call_targets{};
    std::unordered_map<const ExprMethodCall*, std::uint32_t> method_call_targets{};
};

struct CheckedCrate {
    TypeStore types{};

    std::unordered_map<const ItemStruct*, StructInfo> struct_info{};
    std::unordered_map<const ItemEnum*, EnumInfo> enum_info{};

    // Shared canonicalization cache for `builtin::type_*` constructors.
    // Key is a compiler-internal, stable-within-compilation string derived from
    // full descriptor values.
    std::unordered_map<std::string, TypeId> type_construct_cache{};

    std::unordered_map<const ItemConst*, TypeId> const_types{};
    std::unordered_map<const ItemStatic*, TypeId> static_types{};
    std::unordered_map<const ItemFn*, FnInfo> fn_info{};

    std::unordered_map<const PatBinding*, TypeId> binding_types{};
    std::unordered_map<const Expr*, TypeId> expr_types{};

    // ---- `auto` polymorphism (v0.0.24) ----

    // Allocator for compiler-internal DefIds that are not present in HIR
    // (auto specializations, internal const bodies, etc).
    std::uint32_t next_internal_def = 0;

    // Cache: key → specialization def.
    std::unordered_map<std::string, std::uint32_t> auto_fn_instances_by_key{};
    // Specialization bodies and per-instance type tables.
    std::unordered_map<std::uint32_t, AutoFnInstance> auto_fn_instances{};

    // Call-target tables for calls that occur in monomorphic bodies / global
    // initializers. Calls inside `auto` bodies live on `AutoFnInstance`.
    std::unordered_map<const ExprCall*, std::uint32_t> auto_call_targets{};
    std::unordered_map<const ExprMethodCall*, std::uint32_t> auto_method_call_targets{};
};

std::optional<CheckedCrate> check_crate(Session& session,
                                        const ResolvedCrate& crate,
                                        const TargetLayout& target_layout);

}  // namespace cog
