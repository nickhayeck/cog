#pragma once

#include <cstdint>
#include <optional>
#include <unordered_map>

#include "resolve.hpp"
#include "sem.hpp"
#include "session.hpp"
#include "types.hpp"

namespace cog {

struct TargetLayout;

struct CheckedCrate {
    TypeStore types{};

    std::unordered_map<const ItemStruct*, StructInfo> struct_info{};
    std::unordered_map<const ItemEnum*, EnumInfo> enum_info{};

    std::unordered_map<const ItemConst*, TypeId> const_types{};
    std::unordered_map<const ItemStatic*, TypeId> static_types{};
    std::unordered_map<const ItemFn*, FnInfo> fn_info{};

    std::unordered_map<const PatBinding*, TypeId> binding_types{};
    std::unordered_map<const Expr*, TypeId> expr_types{};
};

std::optional<CheckedCrate> check_crate(Session& session,
                                        const ResolvedCrate& crate,
                                        const TargetLayout& target_layout);

}  // namespace cog
