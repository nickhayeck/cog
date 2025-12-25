#pragma once

#include "resolve.hpp"
#include "session.hpp"
#include "sem.hpp"
#include "types.hpp"

#include <cstdint>
#include <optional>
#include <unordered_map>

namespace cog {

struct TargetLayout;

struct CheckedCrate {
  TypeStore types{};

  std::unordered_map<const ItemStruct*, StructInfo> struct_info{};
  std::unordered_map<const ItemEnum*, EnumInfo> enum_info{};

  std::unordered_map<const ItemConst*, TypeId> const_types{};
  std::unordered_map<const ItemStatic*, TypeId> static_types{};
  std::unordered_map<const ItemFn*, FnInfo> fn_info{};

  std::unordered_map<const Expr*, TypeId> expr_types{};
  std::unordered_map<const Expr*, std::uint64_t> array_lens{};
};

std::optional<CheckedCrate> check_crate(Session& session, const ResolvedCrate& crate, const TargetLayout& target_layout);

}  // namespace cog
