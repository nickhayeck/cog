#include "layout.hpp"

#include <algorithm>
#include <cstddef>
#include <string>

namespace cog {
namespace {

static bool path_is_ident(const Path* p, std::string_view name) {
    if (!p || p->segments.size() != 1) return false;
    return p->segments[0]->text == name;
}

}  // namespace

LayoutEngine::LayoutEngine(
    Session& session, TypeStore& types,
    const std::unordered_map<const ItemStruct*, StructInfo>& struct_info,
    const std::unordered_map<const ItemEnum*, EnumInfo>& enum_info,
    const std::unordered_map<const Expr*, std::uint64_t>& array_lens,
    TargetLayout target)
    : session_(session),
      types_(types),
      struct_info_(struct_info),
      enum_info_(enum_info),
      array_lens_(array_lens),
      target_(target) {}

void LayoutEngine::error(Span span, std::string message) {
    session_.diags.push_back(Diagnostic{.severity = Severity::Error,
                                        .span = span,
                                        .message = std::move(message)});
}

std::uint64_t LayoutEngine::align_up(std::uint64_t x, std::uint64_t a) {
    if (a == 0) return x;
    std::uint64_t r = x % a;
    if (r == 0) return x;
    return x + (a - r);
}

std::uint64_t LayoutEngine::pointer_size() const {
    return target_.pointer_size;
}

std::uint64_t LayoutEngine::pointer_align() const {
    return target_.pointer_align;
}

std::uint64_t LayoutEngine::int_align(IntKind k) const {
    switch (k) {
        case IntKind::I8:
        case IntKind::U8:
            return target_.i8_align;
        case IntKind::I16:
        case IntKind::U16:
            return target_.i16_align;
        case IntKind::I32:
        case IntKind::U32:
            return target_.i32_align;
        case IntKind::I64:
        case IntKind::U64:
            return target_.i64_align;
        case IntKind::I128:
        case IntKind::U128:
            return target_.i128_align;
        case IntKind::Isize:
        case IntKind::Usize:
            return target_.pointer_align;
    }
    return 1;
}

std::optional<std::uint64_t> LayoutEngine::array_len_value(
    const Expr* expr) const {
    if (!expr) return std::nullopt;
    if (auto it = array_lens_.find(expr); it != array_lens_.end())
        return it->second;
    return std::nullopt;
}

bool LayoutEngine::is_packed(const std::vector<Attr*>& attrs) {
    for (const Attr* a : attrs) {
        if (!a || !a->name || !path_is_ident(a->name, "repr") || !a->arg_path)
            continue;
        if (path_is_ident(a->arg_path, "packed")) return true;
    }
    return false;
}

std::optional<Layout> LayoutEngine::layout_of(TypeId ty, Span use_site) {
    if (auto it = cache_.find(ty); it != cache_.end()) return it->second;
    if (in_progress_.contains(ty)) {
        error(use_site, "recursive type layout");
        return std::nullopt;
    }

    in_progress_.insert(ty);
    auto out = compute_layout(ty, use_site);
    in_progress_.erase(ty);

    if (!out) return std::nullopt;
    cache_.insert({ty, *out});
    return out;
}

std::optional<std::uint64_t> LayoutEngine::size_of(TypeId ty, Span use_site) {
    auto l = layout_of(ty, use_site);
    if (!l) return std::nullopt;
    if (!l->sized) {
        error(use_site, "cannot take size_of an unsized type");
        return std::nullopt;
    }
    return l->size;
}

std::optional<std::uint64_t> LayoutEngine::align_of(TypeId ty, Span use_site) {
    auto l = layout_of(ty, use_site);
    if (!l) return std::nullopt;
    if (!l->sized) {
        error(use_site, "cannot take align_of an unsized type");
        return std::nullopt;
    }
    return l->align;
}

std::optional<StructLayout> LayoutEngine::struct_layout(const ItemStruct* def,
                                                        Span use_site) {
    if (!def) return std::nullopt;
    if (auto it = struct_cache_.find(def); it != struct_cache_.end())
        return it->second;
    auto out = compute_struct_layout(def, use_site);
    if (!out) return std::nullopt;
    struct_cache_.insert({def, *out});
    return out;
}

std::optional<EnumLayout> LayoutEngine::enum_layout(const ItemEnum* def,
                                                    Span use_site) {
    if (!def) return std::nullopt;
    if (auto it = enum_cache_.find(def); it != enum_cache_.end())
        return it->second;
    auto out = compute_enum_layout(def, use_site);
    if (!out) return std::nullopt;
    enum_cache_.insert({def, *out});
    return out;
}

std::optional<std::uint64_t> LayoutEngine::struct_field_offset(
    const ItemStruct* def, std::string_view field, Span use_site) {
    auto sl = struct_layout(def, use_site);
    if (!sl) return std::nullopt;
    for (const FieldLayout& f : sl->fields) {
        if (f.name == field) return f.offset;
    }
    error(use_site, "unknown field `" + std::string(field) + "`");
    return std::nullopt;
}

std::optional<Layout> LayoutEngine::compute_layout(TypeId ty, Span use_site) {
    const TypeData& d = types_.get(ty);
    switch (d.kind) {
        case TypeKind::Error:
            return Layout{.size = 0, .align = 1, .sized = true};
        case TypeKind::Unit:
            return Layout{.size = 0, .align = 1, .sized = true};
        case TypeKind::Bool:
            return Layout{.size = 1, .align = 1, .sized = true};
        case TypeKind::Int: {
            std::uint64_t size = 0;
            switch (d.int_kind) {
                case IntKind::I8:
                case IntKind::U8:
                    size = 1;
                    break;
                case IntKind::I16:
                case IntKind::U16:
                    size = 2;
                    break;
                case IntKind::I32:
                case IntKind::U32:
                    size = 4;
                    break;
                case IntKind::I64:
                case IntKind::U64:
                    size = 8;
                    break;
                case IntKind::I128:
                case IntKind::U128:
                    size = 16;
                    break;
                case IntKind::Isize:
                case IntKind::Usize:
                    size = pointer_size();
                    break;
            }
            std::uint64_t align =
                std::max<std::uint64_t>(int_align(d.int_kind), 1);
            return Layout{.size = size, .align = align, .sized = true};
        }
        case TypeKind::Float: {
            std::uint64_t size = 0;
            switch (d.float_kind) {
                case FloatKind::F32:
                    size = 4;
                    break;
                case FloatKind::F64:
                    size = 8;
                    break;
            }
            // For now, use the natural alignment for the float size.
            std::uint64_t align = std::max<std::uint64_t>(size, 1);
            return Layout{.size = size, .align = align, .sized = true};
        }
        case TypeKind::Never:
            return Layout{.size = 0, .align = 1, .sized = true};
        case TypeKind::TypeType:
            error(use_site, "`type` has no runtime layout");
            return std::nullopt;
        case TypeKind::Self:
            error(use_site, "`Self` has no standalone layout");
            return std::nullopt;
        case TypeKind::Ptr: {
            const TypeData& pd = types_.get(d.pointee);
            std::uint64_t sz = pointer_size();
            std::uint64_t al = pointer_align();
            if (pd.kind == TypeKind::Slice) {
                sz = pointer_size() * 2;
            }
            return Layout{.size = sz, .align = al, .sized = true};
        }
        case TypeKind::Slice:
            return Layout{.size = 0, .align = 1, .sized = false};
        case TypeKind::Fn:
            return Layout{.size = 0, .align = 1, .sized = false};
        case TypeKind::Array: {
            auto elem_l = layout_of(d.elem, use_site);
            if (!elem_l) return std::nullopt;
            if (!elem_l->sized) {
                error(use_site, "array element type must be sized");
                return std::nullopt;
            }
            std::optional<std::uint64_t> len = d.array_len_value;
            if (!len) len = array_len_value(d.array_len_expr);
            if (!len) {
                error(use_site,
                      "array length is not a known comptime constant");
                return std::nullopt;
            }
            std::uint64_t size = elem_l->size * (*len);
            return Layout{.size = size, .align = elem_l->align, .sized = true};
        }
        case TypeKind::Tuple: {
            bool packed = false;
            std::uint64_t offset = 0;
            std::uint64_t max_align = 1;
            for (TypeId e : d.tuple_elems) {
                auto el = layout_of(e, use_site);
                if (!el) return std::nullopt;
                if (!el->sized) {
                    error(use_site, "tuple element type must be sized");
                    return std::nullopt;
                }
                std::uint64_t fa = packed ? 1 : el->align;
                offset = align_up(offset, fa);
                offset += el->size;
                max_align = std::max(max_align, fa);
            }
            std::uint64_t size = align_up(offset, max_align);
            return Layout{.size = size, .align = max_align, .sized = true};
        }
        case TypeKind::Struct: {
            if (!d.struct_def)
                return Layout{.size = 0, .align = 1, .sized = true};
            auto sl = struct_layout(d.struct_def, use_site);
            if (!sl) return std::nullopt;
            return sl->layout;
        }
        case TypeKind::Enum: {
            if (!d.enum_def)
                return Layout{.size = 0, .align = 1, .sized = true};
            auto el = enum_layout(d.enum_def, use_site);
            if (!el) return std::nullopt;
            return el->layout;
        }
    }
    return std::nullopt;
}

std::optional<StructLayout> LayoutEngine::compute_struct_layout(
    const ItemStruct* def, Span use_site) {
    auto it = struct_info_.find(def);
    if (it == struct_info_.end()) {
        error(use_site, "missing struct info for `" + def->name + "`");
        return std::nullopt;
    }

    bool packed = is_packed(def->attrs);
    std::uint64_t offset = 0;
    std::uint64_t max_align = 1;
    StructLayout out{};

    for (const auto& f : it->second.fields_in_order) {
        auto fl = layout_of(f.type, use_site);
        if (!fl) return std::nullopt;
        if (!fl->sized) {
            error(use_site, "struct field `" + f.name + "` has unsized type");
            return std::nullopt;
        }
        std::uint64_t fa = packed ? 1 : fl->align;
        offset = align_up(offset, fa);
        out.fields.push_back(
            FieldLayout{.name = f.name, .type = f.type, .offset = offset});
        offset += fl->size;
        max_align = std::max(max_align, fa);
    }

    out.layout.align = std::max<std::uint64_t>(max_align, 1);
    out.layout.size = align_up(offset, out.layout.align);
    out.layout.sized = true;
    return out;
}

std::optional<EnumLayout> LayoutEngine::compute_enum_layout(const ItemEnum* def,
                                                            Span use_site) {
    auto it = enum_info_.find(def);
    if (it == enum_info_.end()) {
        error(use_site, "missing enum info for `" + def->name + "`");
        return std::nullopt;
    }

    size_t n = it->second.variants_in_order.size();
    std::uint64_t tag_size = 4;
    if (n <= 0x100)
        tag_size = 1;
    else if (n <= 0x10000)
        tag_size = 2;
    else if (n <= 0x1'0000'0000ULL)
        tag_size = 4;
    else
        tag_size = 8;
    std::uint64_t tag_align = tag_size;

    if (it->second.tag_int) {
        switch (*it->second.tag_int) {
            case IntKind::I8:
            case IntKind::U8:
                tag_size = 1;
                break;
            case IntKind::I16:
            case IntKind::U16:
                tag_size = 2;
                break;
            case IntKind::I32:
            case IntKind::U32:
                tag_size = 4;
                break;
            case IntKind::I64:
            case IntKind::U64:
                tag_size = 8;
                break;
            case IntKind::I128:
            case IntKind::U128:
                tag_size = 16;
                break;
            case IntKind::Isize:
            case IntKind::Usize:
                tag_size = pointer_size();
                break;
        }
        tag_align = std::max<std::uint64_t>(int_align(*it->second.tag_int), 1);
    }

    std::uint64_t payload_size = 0;
    std::uint64_t payload_align = 1;

    for (const auto& [_, v] : it->second.variants) {
        std::uint64_t off = 0;
        std::uint64_t max_a = 1;
        for (TypeId pt : v.payload) {
            auto pl = layout_of(pt, use_site);
            if (!pl) return std::nullopt;
            if (!pl->sized) {
                error(use_site, "enum payload type must be sized");
                return std::nullopt;
            }
            off = align_up(off, pl->align);
            off += pl->size;
            max_a = std::max(max_a, pl->align);
        }
        std::uint64_t ps = align_up(off, max_a);
        payload_size = std::max(payload_size, ps);
        payload_align = std::max(payload_align, max_a);
    }

    std::uint64_t payload_offset = align_up(tag_size, payload_align);
    std::uint64_t align = std::max(tag_align, payload_align);
    std::uint64_t size = align_up(payload_offset + payload_size, align);

    EnumLayout out{};
    out.layout = Layout{.size = size, .align = align, .sized = true};
    out.tag_size = tag_size;
    out.tag_align = tag_align;
    out.payload_offset = payload_offset;
    out.payload_size = payload_size;
    out.payload_align = payload_align;
    return out;
}

}  // namespace cog
