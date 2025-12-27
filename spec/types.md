---
title: Types
version: 0.1.0-draft
---

# Types

Cog has:

- **Runtime value types**: types that exist at runtime (ints, pointers, structs, enums, etc.).
- **Comptime-only types**: types whose values exist only during compilation and are erased (e.g. `type`, `comptime_int`, `comptime_float`).

## Primitive types

### Integers

Signed fixed-width (v0.1):
- `i8`, `i16`, `i32`, `i64`, `i128`

Unsigned fixed-width (v0.1):
- `u8`, `u16`, `u32`, `u64`, `u128`

Pointer-sized:
- `isize`, `usize`

Integer semantics:
- Two’s complement for signed integers.
- Endianness is target-defined.

### Floats

- `f32`, `f64`

Float NaN semantics are target/IEEE-754-like; exact NaN payload behavior is not specified in v0.1.

### `bool`

- `bool` has values `true` and `false`.

### `char` (planned for 0.2.0)

- `char` is a Unicode scalar value.
- Char literals are planned but not required for early v0.1 implementations.

### Unit

- `()` is the unit type; it has exactly one value: `()`.

### Never type

- `!` is the never type.
- Expressions that never return (e.g. `return`, `break` out of a diverging loop, `builtin::compile_error`) have type `!`.
- `!` implicitly coerces to any type.

## Comptime-only primitive types

### `type`

- `type` is the type of **type values**.
- Values of type `type` exist only at compile time.
- A type value represents a runtime type (sized or unsized).

Example:

```cog
fn Option(comptime T: type) -> type { ... }
```

#### Type values in expressions

In contexts where a value of type `type` is required (e.g. calls to `builtin::size_of`,
`builtin::type_info`, or `builtin::cast`), a type name path denotes the corresponding type
value:

```cog
let n: usize = builtin::size_of(i32);
let t: type = builtin::type_ptr_const(builtin::type_slice(u8));
let ti: TypeInfo = builtin::type_info(t);
let x: i32 = builtin::cast(1, i32);
```

#### Type-level calls in type positions

In a type position, `Name(T0, T1, ...)` is a **type-level call** (see `spec/syntax.md`).

Semantics (v0.1):
- The callee `Name` must resolve to a function that returns `type`.
- All parameters of the callee must be `comptime` parameters of type `type` (v0.1 restriction).
- The call is evaluated at compile time, and the returned type value is used as the type.

### `comptime_int` and `comptime_float`

These are the “default” types of numeric literals:
- Integer literals are `comptime_int`.
- Float literals are `comptime_float`.

They are compile-time only and must be coerced/cast to runtime numeric types before code generation.

For v0.1, an implementation may represent `comptime_int` as arbitrary precision and `comptime_float` as a high-precision float, but the observable behavior must match the rules in this spec (fit checks, etc.).

## Compound types

### Tuples

- Tuple type: `(T0, T1, ...)`
- Unit is the empty tuple: `()`.

Tuple field access uses numeric indices:

```cog
let t: (i32, bool) = (1, true);
let x: i32 = t.0;
let y: bool = t.1;
```

### Arrays

- Array type: `[T; N]` where `N` is a comptime `usize`.
- Arrays are sized.

### Slices

- Slice type: `[T]` is an **unsized** type.
- Slices may only appear behind pointers: `const* [T]` and `mut* [T]`.

ABI for slice pointers is specified in `spec/layout_abi.md`.

#### Array-to-slice pointer coercion

In v0.1, the following implicit coercions exist:

- `const* [T; N]` → `const* [T]`
- `mut* [T; N]` → `mut* [T]`

This coercion constructs a slice fat pointer `{ ptr, len }` where:
- `ptr` points to the first element of the array, and
- `len` is `N` as a `usize`.

### Structs

Structs are nominal product types.

Examples:

```cog
struct Point { x: i32, y: i32 }
struct Pair(i32, i32); // tuple struct
```

### Enums

Enums are nominal tagged unions.

- Fieldless enums are used for discriminants and C interop (via `enum[tag(T)]`).
- Payload enums are used for sum types.

Examples:

```cog
enum Color { Red, Green, Blue }
enum OptionI32 { None, Some(i32) }
```

## Pointers (raw pointers only)

Cog has only raw pointers:

- `const* T`: pointer to `T` through which writes are not permitted.
- `mut* T`: pointer to `T` through which writes are permitted.

Properties:
- A pointer may be null, dangling, or aliasing.
- `mut*` does not imply uniqueness. The optimizer must assume `mut*` pointers may alias other pointers.

Coercions:
- `mut* T` implicitly coerces to `const* T`.

Mutability meaning:
- `const*` restricts writes *through that pointer*; it does not make the pointed-to storage immutable globally.

## Function types and function pointers

- `fn(P0, P1, ...) -> R` is a function type.
- In v0.1, function types are **unsized** and may only appear behind pointers:
  - `const* fn(...) -> R` is a function pointer.

Calling a function pointer uses the same call syntax:

```cog
fn add(a: i32, b: i32) -> i32 { a + b }

fn main() -> i32 {
    let p: const* fn(i32, i32) -> i32 = add as const* fn(i32, i32) -> i32;
    p(1, 2)
}
```

The calling convention of function pointers is Cog ABI (see `spec/layout_abi.md`).

## Type aliases

`type Name = T;` defines a transparent synonym.

Type aliases do not create new nominal types and do not affect layout.

## Casts (`as`) and conversion rules

`x as T` is sugar for a cast intrinsic. In v0.1, it is defined as:

```cog
builtin::cast(x, T)
```

See `spec/builtins.md` for how intrinsics and builtins are specified in v0.1.

For v0.1, the following casts are required:

- Integer ↔ integer (with range checks as described in `spec/build_modes.md` when narrowing).
- Integer ↔ pointer (permitted for low-level code; implementation-defined mapping; allowed in all build modes).
- `mut* T` → `const* T` (also exists as an implicit coercion).
- Enum[tag(T)] ↔ `T` for fieldless enums (subject to validity checks).

Implementations may support additional casts, but should keep the set small and explicit.
