#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

#include "ast.hpp"
#include "types.hpp"

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

    // For fieldless enums, Cog assigns each variant a concrete integer
    // discriminant. If `enum[tag(<int>)]` is present, discriminants must fit in
    // that integer type.
    std::optional<IntKind> tag_int{};
    std::unordered_map<std::string, std::int64_t> discriminants{};
};

struct FnInfo {
    std::vector<TypeId> params{};
    std::vector<bool> comptime_params{};  // parallel to `params`
    TypeId ret = 0;
    bool is_variadic = false;

    // v0.0.22: `auto` placeholder in signatures.
    // If `has_auto` is true, the signature is not fully known until
    // `auto_instantiated` becomes true.
    bool has_auto = false;
    bool auto_instantiated = true;
    std::vector<bool> param_has_auto{};  // parallel to `params`
    bool ret_has_auto = false;
};

}  // namespace cog
