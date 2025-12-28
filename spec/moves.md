---
title: Moves and Copy Types
version: 0.1.0-draft
---

# Moves and Copy types

Cog is move-by-default. This is a *static* property checked for locals; it is not a borrow/lifetime system.

## Moves

A value is **moved** when it is:
- assigned into a different place
- passed to a function parameter (unless the parameter is a pointer)
- returned from a function
- bound in certain pattern contexts (see `match`)

After a move, using the moved-from local is a compile-time error.

Implementations must reject obvious use-after-move for locals using a conservative dataflow analysis.

## Copy types

Some types are **Copy**: moving them is equivalent to copying their bits.

In v0.1, `Copy` is a compiler-known property (it is not user-extensible).

A type is `Copy` if:
- it is a primitive integer/float/bool
- it is a raw pointer (`const* T` / `mut* T`)
- it is a function pointer (`const* fn(...) -> R`)
- it is a fieldless enum with `enum[tag(IntType)]`
- it is a tuple/array/struct where all fields/elements are `Copy`

All other types are move-only by default.

## Assignment and initialization

- Assigning to a local initializes it if it was uninitialized.
- Re-assigning to an initialized local overwrites it; the old value is considered moved/dropped (drop semantics are out of scope for v0.1).

## `match` move behavior

`match` moves the scrutinee by default:

```cog
let y = match x { ... };
// x is moved unless x is Copy
```

If the scrutinee’s type is `Copy`, the match uses a copy instead of a move.

This rule applies even if patterns only read fields; the v0.1 core language does not have borrowing patterns.

## Pattern bindings

A binding pattern like `x` or `mut x` binds the matched value:
- If the bound value is move-only, binding moves it into the new local.
- If it is `Copy`, binding copies it.

## Guards

A guard `if expr` is evaluated after the pattern matches. Any bindings from the pattern are in scope in the guard and the arm body.

The guard expression follows the normal left-to-right evaluation order (see `spec/expressions.md`).
