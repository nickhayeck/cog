#pragma once

#include "ast.hpp"
#include "types.hpp"

#include <cstdint>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

namespace cog {

struct StructInfo {
  struct Field {
    std::string name{};
    TypeId type = 0;
  };

  std::vector<Field> fields_in_order{};
  std::unordered_map<std::string, TypeId> fields{};
};

struct VariantInfo {
  const VariantDecl* ast = nullptr;
  std::vector<TypeId> payload{};
};

struct EnumInfo {
  std::vector<std::string> variants_in_order{};
  std::unordered_map<std::string, VariantInfo> variants{};

  // For fieldless enums, Cog assigns each variant a concrete integer discriminant.
  // If `enum[tag(<int>)]` is present, discriminants must fit in that integer type.
  std::optional<IntKind> tag_int{};
  std::unordered_map<std::string, std::int64_t> discriminants{};
};

struct FnInfo {
  std::vector<TypeId> params{};
  TypeId ret = 0;
  bool is_variadic = false;
};

}  // namespace cog
