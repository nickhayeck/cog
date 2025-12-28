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

enum class FloatKind : std::uint8_t {
    F32,
    F64,
};

enum class TypeKind : std::uint8_t {
    Error,
    Unit,
    Bool,
    Int,
    Float,
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

    // Float
    FloatKind float_kind{};

    // Ptr
    Mutability mutability{};
    TypeId pointee = 0;

    // Slice/Array
    TypeId elem = 0;
    const Expr* array_len_expr = nullptr;
    std::optional<std::uint64_t> array_len_value{};

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

    TypeId error() const;
    TypeId unit() const;
    TypeId bool_() const;
    TypeId never() const;
    TypeId type_type() const;
    TypeId self() const;

    TypeId int_(IntKind k) const;
    TypeId float_(FloatKind k) const;
    TypeId ptr(Mutability mut, TypeId pointee) const;
    TypeId slice(TypeId elem) const;
    TypeId array(TypeId elem, const Expr* len_expr) const;
    TypeId tuple(std::vector<TypeId> elems) const;
    TypeId fn(std::vector<TypeId> params, TypeId ret) const;
    TypeId struct_(const ItemStruct* def) const;
    TypeId enum_(const ItemEnum* def) const;

    const TypeData& get(TypeId id) const {
        return types_.at(static_cast<size_t>(id));
    }

    bool equal(TypeId a, TypeId b) const;
    bool can_coerce(TypeId from, TypeId to) const;
    bool is_sized(TypeId t) const;
    bool is_copy(TypeId t) const;
    std::string to_string(TypeId t) const;

    std::optional<IntKind> parse_int_kind(std::string_view name) const;
    std::optional<FloatKind> parse_float_kind(std::string_view name) const;

    void set_array_len_value(const Expr* expr, std::uint64_t len);

   private:
    mutable std::vector<TypeData> types_{};

    mutable std::optional<TypeId> cached_error_{};
    mutable std::optional<TypeId> cached_unit_{};
    mutable std::optional<TypeId> cached_bool_{};
    mutable std::optional<TypeId> cached_never_{};
    mutable std::optional<TypeId> cached_type_type_{};
    mutable std::optional<TypeId> cached_self_{};

    mutable std::unordered_map<IntKind, TypeId> cached_ints_{};
    mutable std::unordered_map<FloatKind, TypeId> cached_floats_{};
    mutable std::unordered_map<const ItemStruct*, TypeId> cached_structs_{};
    mutable std::unordered_map<const ItemEnum*, TypeId> cached_enums_{};

    TypeId make(TypeData d) const;
};

}  // namespace cog
