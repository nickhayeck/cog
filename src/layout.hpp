#pragma once

#include <cstdint>
#include <optional>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "ast.hpp"
#include "sem.hpp"
#include "session.hpp"
#include "target.hpp"
#include "types.hpp"

namespace cog {

struct Layout {
    std::uint64_t size = 0;
    std::uint64_t align = 1;
    bool sized = true;
};

struct FieldLayout {
    std::string_view name{};
    TypeId type = 0;
    std::uint64_t offset = 0;
};

struct StructLayout {
    Layout layout{};
    std::vector<FieldLayout> fields{};
};

struct EnumLayout {
    Layout layout{};
    std::uint64_t tag_size = 0;
    std::uint64_t tag_align = 0;
    std::uint64_t payload_offset = 0;
    std::uint64_t payload_size = 0;
    std::uint64_t payload_align = 1;
};

class LayoutEngine {
   public:
    LayoutEngine(
        Session& session, TypeStore& types,
        const std::unordered_map<const ItemStruct*, StructInfo>& struct_info,
        const std::unordered_map<const ItemEnum*, EnumInfo>& enum_info,
        const std::unordered_map<const Expr*, std::uint64_t>& array_lens,
        TargetLayout target);

    std::optional<Layout> layout_of(TypeId ty, Span use_site);
    std::optional<std::uint64_t> size_of(TypeId ty, Span use_site);
    std::optional<std::uint64_t> align_of(TypeId ty, Span use_site);

    std::optional<StructLayout> struct_layout(const ItemStruct* def,
                                              Span use_site);
    std::optional<EnumLayout> enum_layout(const ItemEnum* def, Span use_site);
    std::optional<std::uint64_t> struct_field_offset(const ItemStruct* def,
                                                     std::string_view field,
                                                     Span use_site);

   private:
    Session& session_;
    TypeStore& types_;
    const std::unordered_map<const ItemStruct*, StructInfo>& struct_info_;
    const std::unordered_map<const ItemEnum*, EnumInfo>& enum_info_;
    const std::unordered_map<const Expr*, std::uint64_t>& array_lens_;
    // Target-specific ABI knobs (pointer size/alignment, integer ABI
    // alignment).
    TargetLayout target_{};

    std::unordered_map<TypeId, Layout> cache_{};
    std::unordered_set<TypeId> in_progress_{};
    std::unordered_map<const ItemStruct*, StructLayout> struct_cache_{};
    std::unordered_map<const ItemEnum*, EnumLayout> enum_cache_{};

    void error(Span span, std::string message);

    static std::uint64_t align_up(std::uint64_t x, std::uint64_t a);

    std::optional<Layout> compute_layout(TypeId ty, Span use_site);
    std::optional<StructLayout> compute_struct_layout(const ItemStruct* def,
                                                      Span use_site);
    std::optional<EnumLayout> compute_enum_layout(const ItemEnum* def,
                                                  Span use_site);

    std::uint64_t pointer_size() const;
    std::uint64_t pointer_align() const;
    std::uint64_t int_align(IntKind k) const;
    std::optional<std::uint64_t> array_len_value(const Expr* expr) const;

    static bool is_packed(const std::vector<Attr*>& attrs);
};

}  // namespace cog
