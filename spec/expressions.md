---
title: Expressions and Control Flow
version: 0.1.0-draft
---

# Expressions and control flow

## Expression-oriented blocks

Blocks are expressions:

```cog
let x: i32 = {
    let y: i32 = 1;
    y + 1
};
```

A block’s type is:
- the type of its trailing expression, if present
- otherwise `()`

## `let`

`let` introduces locals:

```cog
let x: i32 = 1;
let mut y = x + 1;
```

Rules:
- If no initializer is present, the local is uninitialized and must be assigned before use.
- `mut` on a binding means the local can be assigned after initialization.

## Control flow

### `if`

`if` is an expression:

```cog
let x: i32 = if cond { 1 } else { 2 };
```

The condition must be `bool`.

### Loops

Supported loops:
- `while cond { ... }`
- `loop { ... }`

`break` and `continue` behave like Rust.

#### `break expr`

`break expr;` is permitted.

`loop { ... }` is an expression:
- If every `break` in the loop provides an expression, the loop expression has a type (the unified type of those break values).
- If any `break` omits an expression, the loop expression type is `()`.
- A `loop` that never breaks has type `!`.

`while` is always of type `()` (it is not required to yield a value in v0.1).

### `return`

`return expr;` exits the current function. A `return;` without expression is only permitted in functions returning `()`.

`return` has type `!`.

## Match

`match` selects an arm based on pattern matching:

```cog
let y: i32 = match x {
    0 => 10,
    1 | 2 => 20,
    _ if x < 0 => 30,
    _ => 40,
};
```

Properties:
- Arms are considered in source order.
- A guard `if expr` is evaluated only after the pattern matches.
- Guards must have type `bool`.

Typing:
- All arms must unify to a single type `T` (using `!` coercions as needed).
- The match expression has type `T`.

Exhaustiveness:
- Match must be exhaustive; `_` can be used as a catch-all (see `spec/moves.md` and planned exhaustiveness details).

## Evaluation order (unspecified)

Cog intentionally leaves evaluation order unspecified for most subexpressions.

Examples where you must not rely on order:

```cog
// The order of f() and g() is unspecified.
let x = f() + g();

// The order of argument evaluation is unspecified.
foo(f(), g());
```

The compiler may evaluate subexpressions in any order, including interleaving, as long as the behavior corresponds to some valid order consistent with control-flow constructs.

More concretely:
- For any expression with multiple subexpressions, the implementation may choose an evaluation order.
- The observable behavior of the program must be consistent with evaluating subexpressions in *some* such order.
- If different orders would produce different observable results, the result is **unspecified** (the compiler may pick any).

### Guaranteed ordering (minimal set)

The following ordering is guaranteed:

- `a && b`: `a` is evaluated first; `b` is evaluated only if needed.
- `a || b`: `a` is evaluated first; `b` is evaluated only if needed.
- `if cond { ... } else { ... }`: `cond` is evaluated before either branch; only the selected branch is evaluated.
- `while cond { body }`: each iteration evaluates `cond` before `body`.
- `match scrutinee { ... }`: `scrutinee` is evaluated before trying arms; arms are tried in source order.

All other ordering is unspecified.

### Side effects and “conflicting accesses”

Cog does not attempt to define C-style “unsequenced” UB rules for expressions in v0.1.

Instead:
- If your program’s meaning depends on evaluation order, rewrite it using temporaries (`let`) to force an order.
- If an expression performs multiple reads/writes to the same location and the outcome depends on order, the outcome is **unspecified** (it may vary across builds and compiler versions).

An implementation may diagnose obvious cases of “conflicting accesses” (e.g. modifying the same local multiple times in one expression), but v0.1 does not require these diagnoses.

## Operators

### Unary `*` (dereference)

`*p` dereferences a pointer.

- If `p` is `const* T`, then `*p` is a place expression of type `T` that is read-only.
- If `p` is `mut* T`, then `*p` is a mutable place expression of type `T`.

Runtime checks for null/misalignment follow `spec/build_modes.md`.

### `as` casts

See `spec/types.md`.

### `?` operator (planned in v0.1 surface)

The `?` operator is specified in `spec/stdlib.md`.
