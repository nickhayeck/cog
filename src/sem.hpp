#pragma once

#include "ast.hpp"
#include "types.hpp"

#include <cstdint>
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
};

struct FnInfo {
  std::vector<TypeId> params{};
  TypeId ret = 0;
  bool is_variadic = false;
};

struct TraitMethodInfo {
  FnInfo sig{};
  bool object_safe = false;
};

struct TraitMethodSet {
  std::vector<std::string> order{};
  std::unordered_map<std::string, TraitMethodInfo> methods{};
};

}  // namespace cog
