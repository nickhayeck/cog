---
title: Items and Declarations
version: 0.1.0-draft
---

# Items and declarations

This chapter defines the semantics of core items beyond their syntax:
- functions (`fn`)
- inherent implementations (`impl`)
- constants (`const`)
- statics (`static`)
- type aliases (`type`)

## Functions (`fn`)

### Definitions vs declarations

In v0.1:
- A function **definition** has a body: `fn name(...) -> T { ... }`.
- A function **declaration** has no body: `fn name(...) -> T;`.

Only `fn[extern(C)]` functions may be declared without a body.

It is a compile-time error to write a bodyless declaration without `extern(C)`.

### Parameters

Parameters are passed by value unless their type is a pointer.

`comptime` parameters:
- are evaluated at compile time at each call site
- do not exist at runtime (see `spec/comptime.md`)

`auto`:
- `auto` may appear in parameter types for type inference / polymorphism (see `spec/auto.md`).
- `auto` must not be used in `fn[extern(C)]` / `fn[export(C)]` signatures.

Varargs:
- `...` is only permitted on `fn[extern(C)]` declarations (see `spec/layout_abi.md`).

### Return type

- If `-> T` is omitted, the return type is `()`.
- `-> auto` is permitted for inference; see `spec/auto.md`.
- `return` exits a function early and has type `!` (see `spec/types.md`).

## Inherent impl blocks (`impl`)

### Form

Cog supports inherent implementations only:

```cog
struct Vec2 { x: f32, y: f32 }

impl Vec2 {
    fn len(self: const* Self) -> f32 { ... }
    fn origin() -> Vec2 { Vec2 { x: 0.0, y: 0.0 } }
}
```

Within `impl T { ... }`:
- `Self` resolves to `T`.
- Method items are ordinary functions scoped under `T`.

### Receiver parameter

Cog does not have implicit `self`.

A method is identified by having an explicit first parameter named `self`:

```cog
fn foo(self: mut* Self, x: i32) -> i32 { ... }
```

The receiver type is the type of the `self` parameter. In v0.1, the following receiver types are permitted:
- `Self` (by-value receiver; moves the receiver into the call)
- `const* Self`
- `mut* Self`

Other receiver forms may be added later.

### Method call resolution (`x.m(...)`)

A method call expression:

```cog
receiver.method(arg0, arg1)
```

is resolved as follows:

1) Compute the receiver type `R`.
2) Determine the nominal impl type `T`:
   - If `R` is `T`, then `T` is that nominal type.
   - If `R` is `const* T` or `mut* T`, then `T` is the pointed-to nominal type.
   - Otherwise, method lookup fails.
3) Search inherent methods declared in `impl T { ... }` for a method named `method` with a compatible receiver type:
   - If `receiver` is `mut* T`, it may call methods requiring `const* Self` or `mut* Self`.
   - If `receiver` is `const* T`, it may call only methods requiring `const* Self`.
   - If `receiver` is `T`, it may call methods requiring `Self` (by value), or pointer receivers via implicit address-taking (below).

There is no overloading by argument types.

### Method call desugaring

After resolution, `receiver.method(args...)` is desugared to a direct call:

```cog
T::method(self_arg, args...)
```

where `self_arg` is computed based on the receiver type required by the selected method:

- If the method takes `self: Self`:
  - evaluate `receiver` to a value and pass it by value (move/copy rules apply).

- If the method takes `self: const* Self`:
  - if `receiver` already has type `const* T` or `mut* T`, pass it (with `mut*`→`const*` coercion if needed)
  - else take the address of `receiver`:
    - if `receiver` is a place expression, use its address
    - otherwise, materialize a temporary and take its address

- If the method takes `self: mut* Self`:
  - if `receiver` already has type `mut* T`, pass it
  - else take the mutable address of `receiver`:
    - `receiver` must be a mutable place expression, or compilation fails

## Constants (`const`)

### Meaning

A `const` item defines a **named constant value** evaluated at compile time.

```cog
const ANSWER: i32 = 42;
```

Properties:
- A `const` has no stable address; it behaves as an inline value.
- Each use of a `const` may be duplicated or inlined by the compiler.

### Initialization

The initializer expression is evaluated at compile time.

The value must be serializable (see `spec/comptime.md`). Otherwise compilation fails.

### Address-of a const

`&ANSWER` is permitted, but the resulting address is not required to be stable:
- the compiler may materialize a private static for the constant
- different uses may or may not yield the same address

Programs must not rely on pointer identity for `const` values.

## Statics (`static`)

### Meaning

A `static` item defines addressable global storage:

```cog
static BUF: [u8; 16] = comptime { ... };
```

Properties:
- A `static` has a stable address for the lifetime of the program.
- Taking the address of a static yields a pointer to that storage.

### Initialization

All `static` initializers must be comptime.

The initializer result must be serializable; otherwise compilation fails.

Mutation:
- v0.1 does not specify a `static mut` form.
- Mutating global state is still possible via pointers stored in statics.

## Type aliases (`type`)

`type Name = T;` introduces a transparent type synonym.

- The RHS `T` is resolved in a comptime context.
- `type` aliases do not create new nominal types and do not affect layout.
