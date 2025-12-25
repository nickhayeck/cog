#include "types.hpp"

#include <sstream>

namespace cog {

TypeId TypeStore::make(TypeData d) {
  TypeId id = static_cast<TypeId>(types_.size());
  types_.push_back(std::move(d));
  return id;
}

TypeId TypeStore::error() {
  if (cached_error_) return *cached_error_;
  cached_error_ = make(TypeData{.kind = TypeKind::Error});
  return *cached_error_;
}

TypeId TypeStore::unit() {
  if (cached_unit_) return *cached_unit_;
  cached_unit_ = make(TypeData{.kind = TypeKind::Unit});
  return *cached_unit_;
}

TypeId TypeStore::bool_() {
  if (cached_bool_) return *cached_bool_;
  cached_bool_ = make(TypeData{.kind = TypeKind::Bool});
  return *cached_bool_;
}

TypeId TypeStore::never() {
  if (cached_never_) return *cached_never_;
  cached_never_ = make(TypeData{.kind = TypeKind::Never});
  return *cached_never_;
}

TypeId TypeStore::type_type() {
  if (cached_type_type_) return *cached_type_type_;
  cached_type_type_ = make(TypeData{.kind = TypeKind::TypeType});
  return *cached_type_type_;
}

TypeId TypeStore::self() {
  if (cached_self_) return *cached_self_;
  cached_self_ = make(TypeData{.kind = TypeKind::Self});
  return *cached_self_;
}

TypeId TypeStore::int_(IntKind k) {
  if (auto it = cached_ints_.find(k); it != cached_ints_.end()) return it->second;
  TypeId id = make(TypeData{.kind = TypeKind::Int, .int_kind = k});
  cached_ints_.insert({k, id});
  return id;
}

TypeId TypeStore::ptr(Mutability mut, TypeId pointee) {
  return make(TypeData{.kind = TypeKind::Ptr, .mutability = mut, .pointee = pointee});
}

TypeId TypeStore::slice(TypeId elem) {
  return make(TypeData{.kind = TypeKind::Slice, .elem = elem});
}

TypeId TypeStore::array(TypeId elem, const Expr* len_expr) {
  return make(TypeData{.kind = TypeKind::Array, .elem = elem, .array_len_expr = len_expr});
}

TypeId TypeStore::tuple(std::vector<TypeId> elems) {
  TypeData d{.kind = TypeKind::Tuple};
  d.tuple_elems = std::move(elems);
  return make(std::move(d));
}

TypeId TypeStore::struct_(const ItemStruct* def) {
  if (auto it = cached_structs_.find(def); it != cached_structs_.end()) return it->second;
  TypeId id = make(TypeData{.kind = TypeKind::Struct, .struct_def = def});
  cached_structs_.insert({def, id});
  return id;
}

TypeId TypeStore::enum_(const ItemEnum* def) {
  if (auto it = cached_enums_.find(def); it != cached_enums_.end()) return it->second;
  TypeId id = make(TypeData{.kind = TypeKind::Enum, .enum_def = def});
  cached_enums_.insert({def, id});
  return id;
}

static bool vec_equal(const TypeStore& ts, const std::vector<TypeId>& a, const std::vector<TypeId>& b) {
  if (a.size() != b.size()) return false;
  for (size_t i = 0; i < a.size(); i++) {
    if (!ts.equal(a[i], b[i])) return false;
  }
  return true;
}

bool TypeStore::equal(TypeId a, TypeId b) const {
  if (a == b) return true;
  const TypeData& ta = get(a);
  const TypeData& tb = get(b);
  if (ta.kind != tb.kind) return false;

  switch (ta.kind) {
    case TypeKind::Error:
    case TypeKind::Unit:
    case TypeKind::Bool:
    case TypeKind::Never:
    case TypeKind::TypeType:
    case TypeKind::Self:
      return true;
    case TypeKind::Int:
      return ta.int_kind == tb.int_kind;
    case TypeKind::Ptr:
      return ta.mutability == tb.mutability && equal(ta.pointee, tb.pointee);
    case TypeKind::Slice:
      return equal(ta.elem, tb.elem);
    case TypeKind::Array:
      return equal(ta.elem, tb.elem) && ta.array_len_expr == tb.array_len_expr;
    case TypeKind::Tuple:
      return vec_equal(*this, ta.tuple_elems, tb.tuple_elems);
    case TypeKind::Struct:
      return ta.struct_def == tb.struct_def;
    case TypeKind::Enum:
      return ta.enum_def == tb.enum_def;
  }
  return false;
}

bool TypeStore::can_coerce(TypeId from, TypeId to) const {
  if (equal(from, to)) return true;
  const TypeData& f = get(from);
  const TypeData& t = get(to);
  if (f.kind == TypeKind::Ptr && t.kind == TypeKind::Ptr && f.mutability == Mutability::Mut &&
      t.mutability == Mutability::Const) {
    return equal(f.pointee, t.pointee);
  }
  return false;
}

bool TypeStore::is_sized(TypeId t) const {
  const TypeData& d = get(t);
  switch (d.kind) {
    case TypeKind::Error:
    case TypeKind::Unit:
    case TypeKind::Bool:
    case TypeKind::Int:
    case TypeKind::Never:
    case TypeKind::TypeType:
    case TypeKind::Ptr:
    case TypeKind::Array:
    case TypeKind::Tuple:
    case TypeKind::Struct:
    case TypeKind::Enum:
    case TypeKind::Self:
      return true;
    case TypeKind::Slice:
      return false;
  }
  return true;
}

bool TypeStore::is_copy(TypeId t) const {
  const TypeData& d = get(t);
  switch (d.kind) {
    case TypeKind::Error:
      return true;
    case TypeKind::Unit:
    case TypeKind::Bool:
    case TypeKind::Int:
    case TypeKind::Never:
    case TypeKind::TypeType:
    case TypeKind::Ptr:
      return true;
    case TypeKind::Slice:
      return false;
    case TypeKind::Array:
      return is_copy(d.elem);
    case TypeKind::Tuple:
      for (TypeId e : d.tuple_elems) {
        if (!is_copy(e)) return false;
      }
      return true;
    case TypeKind::Struct:
    case TypeKind::Enum:
    case TypeKind::Self:
      return false;
  }
  return false;
}

static std::string_view int_name(IntKind k) {
  switch (k) {
    case IntKind::I8:
      return "i8";
    case IntKind::I16:
      return "i16";
    case IntKind::I32:
      return "i32";
    case IntKind::I64:
      return "i64";
    case IntKind::I128:
      return "i128";
    case IntKind::Isize:
      return "isize";
    case IntKind::U8:
      return "u8";
    case IntKind::U16:
      return "u16";
    case IntKind::U32:
      return "u32";
    case IntKind::U64:
      return "u64";
    case IntKind::U128:
      return "u128";
    case IntKind::Usize:
      return "usize";
  }
  return "i32";
}

std::string TypeStore::to_string(TypeId t) const {
  const TypeData& d = get(t);
  switch (d.kind) {
    case TypeKind::Error:
      return "<error>";
    case TypeKind::Unit:
      return "()";
    case TypeKind::Bool:
      return "bool";
    case TypeKind::Int:
      return std::string(int_name(d.int_kind));
    case TypeKind::Never:
      return "!";
    case TypeKind::TypeType:
      return "type";
    case TypeKind::Self:
      return "Self";
    case TypeKind::Ptr: {
      std::ostringstream out;
      out << (d.mutability == Mutability::Mut ? "mut* " : "const* ");
      out << to_string(d.pointee);
      return out.str();
    }
    case TypeKind::Slice: {
      std::ostringstream out;
      out << "[" << to_string(d.elem) << "]";
      return out.str();
    }
    case TypeKind::Array: {
      std::ostringstream out;
      out << "[" << to_string(d.elem) << "; _]";
      return out.str();
    }
    case TypeKind::Tuple: {
      std::ostringstream out;
      out << "(";
      for (size_t i = 0; i < d.tuple_elems.size(); i++) {
        if (i) out << ", ";
        out << to_string(d.tuple_elems[i]);
      }
      if (d.tuple_elems.size() == 1) out << ",";
      out << ")";
      return out.str();
    }
    case TypeKind::Struct:
      return d.struct_def ? d.struct_def->name : "<struct>";
    case TypeKind::Enum:
      return d.enum_def ? d.enum_def->name : "<enum>";
  }
  return "<type>";
}

std::optional<IntKind> TypeStore::parse_int_kind(std::string_view name) const {
  if (name == "i8") return IntKind::I8;
  if (name == "i16") return IntKind::I16;
  if (name == "i32") return IntKind::I32;
  if (name == "i64") return IntKind::I64;
  if (name == "i128") return IntKind::I128;
  if (name == "isize") return IntKind::Isize;
  if (name == "u8") return IntKind::U8;
  if (name == "u16") return IntKind::U16;
  if (name == "u32") return IntKind::U32;
  if (name == "u64") return IntKind::U64;
  if (name == "u128") return IntKind::U128;
  if (name == "usize") return IntKind::Usize;
  return std::nullopt;
}

}  // namespace cog
