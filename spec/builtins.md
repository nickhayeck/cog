---
title: Builtins and Intrinsics
version: 0.1.0-draft
---

# Builtins and intrinsics

Cog provides a small set of **compiler-defined builtins** for:
- layout queries (`size_of`, `align_of`)
- comptime reflection (`type_info`)
- compile-time errors (`compile_error`)
- casts (`cast`)
- comptime type construction

Builtins are referenced using the path prefix `builtin::` and are not ordinary items.

## Name resolution

`builtin` is a compiler-provided module at crate root.

- `builtin::name` always resolves to a builtin, not a user item.
- User code must not define a crate-root item named `builtin`.

Note: the long-term plan is to move most “builtins” behind a `core::` surface, but the
crate-root `builtin::` namespace remains reserved.

## Availability

Some builtins are **comptime-only**:
- they may only be used in comptime contexts (see `spec/comptime.md`)
- they have no runtime code generation

Some builtins are “compile-time constant”:
- they may appear in runtime code
- but they are always evaluated at compile time and embedded as constants

In v0.1:
- comptime-only: `builtin::compile_error`, all `builtin::type_*` constructors
- compile-time constant: `builtin::size_of`, `builtin::align_of`, `builtin::type_info`
- runtime+comptime: `builtin::cast`

## v0.1 required builtins

### `builtin::size_of`

Signature:

```cog
builtin::size_of(comptime T: type) -> usize
```

Behavior:
- `T` must be a sized runtime type; otherwise compilation fails.
- The result is the size of `T` in bytes for the selected target.
- `builtin::size_of` is always evaluated at compile time (even in runtime code).

### `builtin::align_of`

Signature:

```cog
builtin::align_of(comptime T: type) -> usize
```

Behavior:
- `T` must be a sized runtime type; otherwise compilation fails.
- The result is the alignment of `T` in bytes for the selected target.
- `builtin::align_of` is always evaluated at compile time (even in runtime code).

### `builtin::type_info`

Signature:

```cog
builtin::type_info(comptime T: type) -> TypeInfo
```

Behavior:
- `T` may be any runtime type, including unsized types (`[T]` and `fn(...) -> R`).
- The result is a `TypeInfo` value describing `T`.
- `builtin::type_info` is always evaluated at compile time (even in runtime code).

`TypeInfo` is a compiler-provided struct type. Its exact layout is **unstable** in v0.1
(see `spec/comptime.md`).

For v0.1, `TypeInfo` and its supporting kind enum are defined as:

```cog
enum builtin::TypeKind {
    Int,
    Float,
    Bool,
    Unit,
    Never,
    Ptr,
    Slice,
    Array,
    Tuple,
    Struct,
    Enum,
    Fn,
}

struct TypeInfo {
    kind: builtin::TypeKind,
    size: usize,
    align: usize,
}
```

Notes:
- `TypeInfo` is injected by the compiler into the crate root.
- For unsized types (`[T]` and `fn(...) -> R`), `size` and `align` are `0`.

### `builtin::compile_error`

Signature:

```cog
builtin::compile_error(msg: const* [u8]) -> !
```

Behavior:
- `builtin::compile_error` is **comptime-only**: it may only appear in comptime contexts.
- When evaluated, compilation fails with `msg`.
- `msg` must be serializable as a comptime value.

### `builtin::cast`

Signature (conceptual):

```cog
builtin::cast(x: any, comptime T: type) -> T
```

Notes:
- `any` is not a real Cog type; it indicates the builtin is polymorphic and has special typing rules.
- `builtin::cast(x, T)` is exactly the operation performed by `x as T` (see below).
- Cast validity rules are specified in `spec/types.md`.
- If `builtin::cast` is used in a comptime context, it is evaluated during comptime execution.

## `as` is syntax sugar for `builtin::cast`

`x as T` is defined as `builtin::cast(x, T)`.

## Type construction (v0.1)

Cog intends to support constructing arbitrary types at comptime. The builtin surface is
intentionally marked **unstable** in v0.1 (it may change before 1.0), but it is part of
the v0.1 conformance target.

Type construction builtins must be:
- **comptime-only**
- **pure/deterministic**
- **canonicalized**: repeated construction with identical inputs yields the same resulting type identity within a compilation

### Descriptors

Type construction uses descriptor values (compiler-provided types) to describe the shape
of the type being created.

Descriptor values may freely allocate and reference comptime-heap memory, because they are
consumed during compilation and are not serialized into the runtime program.

The compiler provides the following descriptor types under `builtin::`:

```cog
// Visibility used in type-construction descriptors.
enum builtin::Vis { Private, Pub, PubCrate }

// Struct representation controls.
enum builtin::StructRepr { Cog, C, Packed }

// Optional helpers used by descriptors.
enum builtin::MaybeType { None, Some(type) }
enum builtin::MaybeComptimeInt { None, Some(comptime_int) }

struct builtin::StructField {
    name: const* [u8],
    ty: type,
    vis: builtin::Vis,
}

struct builtin::StructDesc {
    name: const* [u8],
    repr: builtin::StructRepr,
    fields: const* [builtin::StructField],
}

struct builtin::EnumVariant {
    name: const* [u8],
    payload: const* [type],
    discriminant: builtin::MaybeComptimeInt,
}

struct builtin::EnumDesc {
    name: const* [u8],
    variants: const* [builtin::EnumVariant],
    tag_type: builtin::MaybeType,
}
```

### `builtin::type_struct`

Signature (conceptual):

```cog
builtin::type_struct(desc: builtin::StructDesc) -> type
```

Semantics:
- Produces a nominal struct type.
- Field access uses `name` (layout order is determined by `repr` rules in `spec/layout_abi.md`).

### `builtin::type_enum`

Signature (conceptual):

```cog
builtin::type_enum(desc: builtin::EnumDesc) -> type
```

Semantics:
- Produces a nominal enum type.
- If `desc.tag_type` is `Some(IntType)`, the enum must be fieldless and follows the rules of `enum[tag(IntType)]`.
  - `IntType` must be an integer type.
  - Variant discriminants must fit in `IntType`.

### Other type constructors

Cog also supports constructing non-nominal composite types at comptime, primarily for
metaprogramming:

- `builtin::type_unit() -> type` (yields `()`)
- `builtin::type_never() -> type` (yields `!`)
- `builtin::type_ptr_const(child: type) -> type` (yields `const* Child`)
- `builtin::type_ptr_mut(child: type) -> type` (yields `mut* Child`)
- `builtin::type_slice(child: type) -> type` (yields `[Child]`)
- `builtin::type_array(child: type, comptime n: usize) -> type` (yields `[Child; n]`)
- `builtin::type_tuple(elems: const* [type]) -> type` (yields a tuple type)
- `builtin::type_fn(params: const* [type], ret: type) -> type` (yields `fn(params...) -> ret`)

These are pure canonical constructors:
- identical inputs yield the same resulting type identity
- invalid combinations (e.g. `type_slice([T])`) are compile-time errors

## Canonicalization and type identity

Within a single compilation:
- Constructed types are canonicalized by their full descriptor inputs.
- The compiler must behave as if it maintains an internal cache keyed by the descriptor values.

This guarantee enables patterns like:
- `type A = Option(i32);`
- `type B = Option(i32);`

where `A` and `B` are the same type for all practical purposes (layout, equality, matching).
