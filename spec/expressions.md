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

## Array literals

Cog supports two array literal forms:

```cog
let a: [i32; 3] = [1, 2, 3];
let z: [u8; 16] = [0; 16];
```

### `[e0, e1, ...]`

- `[e0, e1, ...]` constructs an array value with type `[T; N]` where:
  - `N` is the number of elements written, and
  - `T` is determined by unifying the element types (or by an expected type).
- Each element expression is evaluated and moved into the array.
- Element expressions are evaluated **left-to-right**.
- `[]` (the empty array literal) is only permitted when the expected type is `[T; 0]` for some `T`.

### `[x; N]` (repeat)

- `N` must be a comptime `usize`.
- The element expression `x` is evaluated **once**.
- The resulting array contains `N` copies of that value.
- If `N >= 2`, the element type must be `Copy` (see `spec/moves.md`).

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

### `?` (try)

`expr?` is a postfix operator that performs early return on `core::Option(T)` / `core::Result(T, E)` values (see `spec/stdlib.md`).

Semantics (conceptual desugaring; pseudo-code):

- For `core::Result(T, E)`:

```cog
match expr {
    Ok(v) => v,
    Err(e) => return Err(e),
}
```

- For `core::Option(T)`:

```cog
match expr {
    Some(v) => v,
    None => return None,
}
```

Typing constraints in v0.1:
- `expr` must have type `core::Result(T, E)` or `core::Option(T)` for some `T`/`E`.
- The surrounding function’s return type must be compatible (as specified in `spec/stdlib.md`).

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

## Evaluation order (left-to-right)

Cog guarantees **left-to-right evaluation order** for subexpressions.

Examples:

```cog
// Guaranteed: f() is evaluated before g().
let x = f() + g();

// Guaranteed: arguments are evaluated left-to-right.
foo(f(), g());
```

The left-to-right order is defined syntactically by the surface syntax.

More concretely, evaluation proceeds as follows (all left-to-right):
- Unary operators: evaluate the operand, then apply the operator.
- Binary operators (except `&&`/`||`): evaluate LHS, then RHS, then apply the operator.
- `a && b`: evaluate `a`; if false, do not evaluate `b`.
- `a || b`: evaluate `a`; if true, do not evaluate `b`.
- Assignment `place = expr`: evaluate `place` (including its subexpressions), then evaluate `expr`, then perform the store.
- Calls `callee(args...)`: evaluate `callee`, then evaluate arguments left-to-right, then perform the call.
- Method calls `recv.m(args...)`: evaluate `recv`, then evaluate arguments left-to-right, then perform the call.
- Field access `base.field`: evaluate `base`, then select the field.
- Indexing `base[index]`: evaluate `base`, then evaluate `index`, then select the element.
- Tuple/array/struct literals: evaluate elements/fields left-to-right.
- Blocks: evaluate statements in source order; the trailing expression (if any) is evaluated last.

Control-flow constructs:
- `if cond { ... } else { ... }`: evaluate `cond`; only the selected branch is evaluated.
- `while cond { body }`: per iteration, evaluate `cond`; if true evaluate `body`.
- `match scrutinee { ... }`: evaluate `scrutinee`; arms are tested in source order; only the selected arm body is evaluated.

### Side effects and “conflicting accesses”

Because evaluation order is defined, side effects are sequenced in that left-to-right order.

This does **not** make unsafe memory accesses safe:
- dereferencing dangling pointers is still UB,
- in `ReleaseFast`, many failures are still UB (see `spec/build_modes.md`),
- threading/data races remain out of scope for v0.1.

## Operators

### Unary `*` (dereference)

`*p` dereferences a pointer.

- If `p` is `const* T`, then `*p` is a place expression of type `T` that is read-only.
- If `p` is `mut* T`, then `*p` is a mutable place expression of type `T`.

Runtime checks for null/misalignment follow `spec/build_modes.md`.

### Unary `&` / `&mut` (address-of)

`&place` takes the address of a place expression.

- `&place` has type `const* T` where `place` has type `T`.
- `&mut place` has type `mut* T` and requires `place` be mutable.

Because Cog has no lifetime checking, the resulting pointer may dangle if the pointed-to storage goes away (e.g. after moving/dropping); this is permitted but may cause UB when dereferenced.

### Unary `~` (bitwise NOT)

`~x` performs bitwise NOT on an integer.

It is an error to apply `~` to a non-integer type.

### Binary arithmetic

For v0.1:
- `+`, `-`, `*`, `/` are defined for integers and floats.
- `%` is defined for integers (float remainder is planned).

Integer overflow and other traps/UB are specified in `spec/build_modes.md`.

### Bitwise operators and shifts

For v0.1, the following operators are defined for integer types only:

- `x & y` (bitwise AND)
- `x ^ y` (bitwise XOR)
- `x | y` (bitwise OR)
- `x << y` (shift left)
- `x >> y` (shift right; arithmetic for signed, logical for unsigned)

It is an error to apply these operators to non-integer types.

Shift range traps/UB are specified in `spec/build_modes.md`.

### Indexing `a[i]`

`a[i]` is a place expression selecting an element.

Supported bases:
- Arrays: if `a` has type `[T; N]`, then `a[i]` selects an element of type `T`.
- Slice pointers: if `a` has type `const* [T]` or `mut* [T]`, then `a[i]` selects an element of type `T`.
- Raw pointers: if `a` has type `const* T` or `mut* T`, then `a[i]` is pointer indexing and is equivalent to dereferencing the element pointer at offset `i` from `a` (like C `a[i]`).

Safety checks:
- Array and slice bounds checks follow `spec/build_modes.md`.
- Raw pointer indexing performs **no** bounds check (there is no length).
- Null/misalignment traps follow the same rules as dereferencing `*p` (see `spec/build_modes.md`).

### `as` casts

See `spec/types.md`.

### `?` operator (planned in v0.1 surface)

The `?` operator is specified in `spec/stdlib.md`.
