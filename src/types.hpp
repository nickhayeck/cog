#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "ast.hpp"
#include "diag.hpp"
#include "resolve.hpp"

namespace cog {

using TypeId = std::uint32_t;

enum class IntKind : std::uint8_t {
    I8,
    I16,
    I32,
    I64,
    I128,
    Isize,
    U8,
    U16,
    U32,
    U64,
    U128,
    Usize,
};

enum class TypeKind : std::uint8_t {
    Error,
    Unit,
    Bool,
    Int,
    Never,
    TypeType,
    Ptr,
    Slice,
    Array,
    Tuple,
    Fn,
    Struct,
    Enum,
    Self,
};

struct TypeData {
    TypeKind kind = TypeKind::Error;

    // Int
    IntKind int_kind{};

    // Ptr
    Mutability mutability{};
    TypeId pointee = 0;

    // Slice/Array
    TypeId elem = 0;
    const Expr* array_len_expr = nullptr;

    // Tuple
    std::vector<TypeId> tuple_elems{};

    // Function type (unsized; only valid behind pointers)
    std::vector<TypeId> fn_params{};
    TypeId fn_ret = 0;

    // Nominal
    const ItemStruct* struct_def = nullptr;
    const ItemEnum* enum_def = nullptr;
};

class TypeStore {
   public:
    TypeStore() = default;

    TypeId error();
    TypeId unit();
    TypeId bool_();
    TypeId never();
    TypeId type_type();
    TypeId self();

    TypeId int_(IntKind k);
    TypeId ptr(Mutability mut, TypeId pointee);
    TypeId slice(TypeId elem);
    TypeId array(TypeId elem, const Expr* len_expr);
    TypeId tuple(std::vector<TypeId> elems);
    TypeId fn(std::vector<TypeId> params, TypeId ret);
    TypeId struct_(const ItemStruct* def);
    TypeId enum_(const ItemEnum* def);

    const TypeData& get(TypeId id) const {
        return types_.at(static_cast<size_t>(id));
    }

    bool equal(TypeId a, TypeId b) const;
    bool can_coerce(TypeId from, TypeId to) const;
    bool is_sized(TypeId t) const;
    bool is_copy(TypeId t) const;
    std::string to_string(TypeId t) const;

    std::optional<IntKind> parse_int_kind(std::string_view name) const;

   private:
    std::vector<TypeData> types_{};

    std::optional<TypeId> cached_error_{};
    std::optional<TypeId> cached_unit_{};
    std::optional<TypeId> cached_bool_{};
    std::optional<TypeId> cached_never_{};
    std::optional<TypeId> cached_type_type_{};
    std::optional<TypeId> cached_self_{};

    std::unordered_map<IntKind, TypeId> cached_ints_{};
    std::unordered_map<const ItemStruct*, TypeId> cached_structs_{};
    std::unordered_map<const ItemEnum*, TypeId> cached_enums_{};

    TypeId make(TypeData d);
};

}  // namespace cog
