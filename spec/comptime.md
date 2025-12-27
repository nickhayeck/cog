---
title: Comptime
version: 0.1.0-draft
---

# Comptime (compile-time execution)

Cog’s comptime model is inspired by Zig:
- comptime code is **interpreted**
- compilation emits a program with only **runtime** operations remaining (“residualization”)
- memoization is permitted as an optimization, but there is no language-level “specialization”

This spec intentionally defines a powerful end-state, even if early compilers implement a smaller subset.

## Comptime contexts

An expression is evaluated at compile time (“in a comptime context”) when it appears in:

- `const` initializers
- `static` initializers (required to be comptime)
- array lengths `[T; N]`
- `type` aliases (`type Name = ...;`) where the RHS may depend on comptime evaluation
- `comptime { ... }` blocks
- arguments passed to `comptime` parameters

## `comptime { ... }` blocks

`comptime { block }` evaluates `block` at compile time and yields a compile-time value.

Example:

```cog
const N: usize = comptime { 40 + 2 };
```

If a `comptime { ... }` block appears in a runtime context, it is still executed at compile time and its result is embedded into the runtime program as a constant value, subject to the “serialization boundary” rules below.

## Comptime parameters

Functions may declare comptime parameters:

```cog
fn repeat(comptime N: usize, x: i32) -> i32 { ... }
fn Option(comptime T: type) -> type { ... }
```

Semantics:
- At a call site, arguments corresponding to `comptime` parameters are evaluated at compile time.
- The function body is then interpreted with those comptime values available.
- Any runtime-dependent computation that remains is lowered/emitted as residual runtime operations.
- The resulting runtime code contains no `comptime` parameters.

## Determinism and side effects

Comptime evaluation must be **pure and deterministic**:
- No I/O.
- No access to wall-clock time.
- No randomness.
- No thread creation.
- No reading unspecified memory.

In practice, comptime code is limited to:
- arithmetic, comparisons, and control flow
- constructing values (arrays/tuples/structs/enums)
- calling comptime-callable functions
- using comptime builtins (reflection, size/alignment, errors, type construction)

An implementation should enforce determinism by design (no syscalls in the interpreter).

## Comptime resource limits

To ensure compilation terminates, implementations should impose limits such as:
- a step/instruction budget
- a recursion depth limit
- a comptime heap size limit

If a limit is exceeded, compilation fails with a diagnostic.

The exact numeric limits are implementation-defined in v0.1.

## Safety checks at comptime

Comptime evaluation must not “silently invoke UB”.

At minimum:
- operations that would trap in `Debug`/`ReleaseSafe` at runtime must produce a compile-time error when executed at comptime.

## Comptime heap and the serialization boundary

Cog comptime evaluation has access to a conceptual “comptime heap” for building composite values.

When a comptime value is embedded into runtime code (e.g. a `const`, a `static`, an array length, or a residual constant), it must be **serializable**:

Serializable values:
- integers, floats, bool
- enums/structs/tuples/arrays whose components are serializable
- pointers *only* when they point to:
  - a static object emitted in the runtime program (including string literal storage)

Non-serializable values:
- pointers into the comptime heap
- interpreter-internal handles/resources

Using a non-serializable comptime value at runtime is a compile-time error.

## Comptime-callable functions

In the end-state model, any function may be executed at comptime when called from a comptime context, subject to restrictions:
- no interaction with runtime-only features
- must terminate within resource limits

Early v0.1 implementations may restrict this (e.g. only allow a small interpreted subset).

## `type` values and type construction

### Type values (`type`)

`type` is comptime-only. A value of type `type` represents a runtime type.

### Constructing types at comptime

Cog intends to support constructing arbitrary types at comptime via builtins.

This enables “generics without generics syntax”:

```cog
fn Option(comptime T: type) -> type {
    // conceptual end-state:
    // return an enum type { None, Some(T) }
    return builtin::type_enum(...);
}
```

The type-construction builtin surface is specified in `spec/builtins.md`.

It is considered unstable across pre-1.0 releases (it may evolve), but v0.1 compilers
aiming for conformance should implement the set described there.

## Reflection

Cog provides reflection via:

```cog
builtin::type_info(T: type) -> TypeInfo
```

See `spec/builtins.md` for the builtin definition and v0.1 restrictions.

In v0.1, `builtin::type_info` is defined for all runtime types, including unsized types
like `[T]` and `fn(...) -> R`.

`TypeInfo` is a struct type provided by the compiler. Its v0.1 definition is specified in
`spec/builtins.md` and includes at least:
- a kind discriminator
- size/alignment (with `0`/`0` used for unsized types)

Future versions may extend `TypeInfo` with richer reflection data.

## Compile-time errors

`builtin::compile_error(msg)` is a comptime-only builtin that aborts compilation with an
error message.

- It may only appear in comptime contexts.
- `msg` has type `const* [u8]`.
- The expression has type `!`.

See `spec/builtins.md` for the canonical builtin definition.
