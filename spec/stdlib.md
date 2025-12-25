---
title: Core Library Surface
version: 0.1.0-draft
---

# Core library surface (v0.1)

This chapter describes what the compiler assumes exists (or provides implicitly) for basic programs.

## `core` module

Cog intends to ship a minimal `core` module that is dependency-free.

For v0.1, `core` should provide:

- integer and pointer-sized types (`u8`, `i32`, `usize`, etc.)
- `core::Option` and `core::Result` type constructors (see below)
- basic memory intrinsics declarations (see “required runtime symbols”)

## `Option` and `Result` without generics syntax

Cog does not have angle-bracket generics syntax in v0.1.

Instead, generic data structures are expressed as comptime type functions:

```cog
fn Option(comptime T: type) -> type;
fn Result(comptime T: type, comptime E: type) -> type;
```

Semantically:
- `Option(T)` is an enum with variants `None` and `Some(T)`.
- `Result(T, E)` is an enum with variants `Ok(T)` and `Err(E)`.

The compiler may treat these as compiler-known shapes for `?` (below).

## `?` operator

The `?` operator is sugar for early-return on `Option(T)` / `Result(T, E)`.

For `Result(T, E)`:

```cog
let x: T = f()?; // if f() is Err(e), return Err(e) from the current function
```

For `Option(T)`:

```cog
let x: T = g()?; // if g() is None, return None from the current function
```

Constraints in v0.1:
- `?` applies only to values whose type is exactly `core::Result(T, E)` or `core::Option(T)` for some `T`/`E`.
- The containing function’s return type must be compatible:
  - using `?` on a `Result(T,E)` requires the function return type to be `Result(U, E)` for some `U`
  - using `?` on an `Option(T)` requires the function return type to be `Option(U)` for some `U`

There is no generalized “Try” mechanism in v0.1.

## Required runtime symbols

To support basic codegen and future `core` implementations, a Cog program may depend on the following C runtime symbols (exact set may evolve, but should remain small):

- `memcpy`
- `memmove`
- `memset`
- `memcmp`
- `strlen` (for C string interop)

An implementation may provide these by:
- linking against the platform C runtime, or
- providing compiler-rt equivalents.
