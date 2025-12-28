---
title: auto (Type Placeholder)
version: 0.1.0-draft
---

# `auto` (type placeholder)

`auto` is a **type placeholder** used for **type inference** and **compile-time polymorphism** (similar to Zig’s `anytype`, but named `auto`).

`auto` is **not** a runtime type:
- it does not exist at runtime
- it cannot be named, constructed, or reflected on as a value
- by the end of compilation, no runtime type may contain unresolved `auto`

## Where `auto` is allowed (v0.1)

`auto` may appear in type positions that describe **values**, specifically:
- function parameter types (including `comptime` parameters)
- function return types
- `let` type annotations
- `const` and `static` type annotations

`auto` must not appear in:
- struct field types / tuple struct field types
- enum payload types
- `impl` headers (`impl T { ... }`)
- `type` aliases (`type Name = T;`)
- cast target types (`x as T`)
- function types (`fn(P...) -> R` in a type position)
- type-level call arguments (`Name(...)` in a type position)
- tag metadata (e.g. `repr(...)`, `extern_name(...)`, etc.)

Rationale: in v0.1, `auto` is a placeholder for types inferred from **expressions** and/or **call sites**; nominal type declarations do not provide such an inference source.

## `auto` in local declarations

### `let`

`let x: auto = expr;` means: infer the type of `x` from `expr`.

This is equivalent to `let x = expr;` (except it is an error to write `let x: auto;` without an initializer).

### `const` / `static`

`const NAME: auto = expr;` and `static NAME: auto = expr;` infer the declared type from the initializer.

Notes:
- the initializer is still evaluated in a comptime context (see `spec/items.md` and `spec/comptime.md`)
- for `static`, the inferred type must be a valid runtime storage type (e.g. not `type`, `comptime_int`, or other comptime-only types)

## `auto` in function signatures

When `auto` appears in a function signature, the function is **polymorphic**: it can be type-checked and compiled for different argument types.

### Implicit type parameters

Each `auto` in the parameter list introduces an **implicit comptime type parameter** of type `type`.

Conceptually:

```cog
fn id(x: auto) -> auto { x }
```

behaves like:

```cog
fn id(comptime T0: type, x: T0) -> T0 { x }
```

except that `T0` is **not** written by the user and is **implicitly passed** at call sites.

Each occurrence of `auto` introduces a fresh implicit type parameter; equalities between them arise only from typing constraints in the function body.

### Naming the deduced type

`auto` does not introduce a user-visible name for the deduced type.

If a function needs to refer to the deduced type as a `type` value (e.g. to call `builtin::size_of` or to construct another type), write an explicit `comptime T: type` parameter instead:

```cog
fn bytes_of(comptime T: type, x: T) -> usize {
    builtin::size_of(T)
}
```

### Instantiation and residualization (no “templates”)

At a call site:
1) each `auto` parameter’s implicit type argument is deduced from the corresponding argument’s static type
2) the function body is type-checked (and comptime-evaluated where required) with those type arguments
3) compilation emits only the **residual runtime operations**

This may produce distinct runtime code for different deduced type arguments, but this is specified in terms of comptime interpretation + residualization (see `spec/comptime.md`), not user-visible “template specialization”.

Implementations may cache and reuse previously-compiled instantiations as an optimization.

### Restrictions

- `auto` must not be used in `fn[extern(C)]` or `fn[export(C)]` signatures.
- A function with any `auto` in its signature does not have a single monomorphic function type and therefore:
  - it cannot be used as a function value
  - it cannot be coerced to a function pointer
  - it cannot be passed to `builtin::cast` as a function pointer value

## `auto` return type

`fn f(...) -> auto { ... }` means the return type is inferred from the function body.

Rules:
- The function body must determine a single return type after applying standard coercions (notably: `!` coerces to any type).
- `return;` contributes `()` as a return type candidate.
- The trailing expression of a block (if present) contributes its type as a return type candidate.
- If all return paths diverge (e.g. the function always `return`s `!` or loops forever), the return type is `!`.

For polymorphic functions (those with `auto` parameters), the return type may be different for different instantiations.

## Interaction with comptime-only types

Because numeric literals default to `comptime_int` / `comptime_float` (see `spec/types.md`), `auto` can infer comptime-only types.

Example:

```cog
fn f(x: auto) -> auto { x }

const A: comptime_int = f(1); // ok (comptime context)
```

But:
- a runtime parameter/return type must be a runtime type; using an inferred comptime-only type in runtime code is an error
- if you intend a runtime integer/float type, use an explicit type or cast:

```cog
fn f(x: auto) -> auto { x }

fn main() -> i32 {
    f(1 as i32)
}
```

## Examples

### Polymorphic identity

```cog
fn id(x: auto) -> auto { x }

fn main() -> i32 { id(123 as i32) }
```

### Polymorphic swap via pointers

```cog
fn swap(a: mut* auto, b: mut* auto) -> () {
    let tmp: auto = *a;
    *a = *b;
    *b = tmp;
}
```

This is type-checked per call site; `swap` cannot be used as a function pointer.

### Local `auto` is just inference

```cog
fn main() -> i32 {
    let x: auto = 1 as i32;
    let p: const* auto = &x;
    *p
}
```
