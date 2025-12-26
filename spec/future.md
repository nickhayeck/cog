---
title: Planned Extensions (Non-normative)
version: 0.1.0-draft
---

# Planned extensions (non-normative)

This chapter lists features that are explicitly *not* fully specified for v0.1, but are part of Cog’s intended trajectory.

## Additional literals

- Char literals (and the `char` type) with full Unicode escape support.
- Additional string literal forms:
  - byte/hex escapes (`\xNN`)
  - Unicode scalar escapes (`\u{...}`)
  - raw strings (no escapes)
- Numeric literal suffixes (e.g. `123_u8`) as an alternative to `as` casts.

## Control-flow sugar (planned for 0.2.0)

### `if let`

Planned syntax:

```cog
if let pat = expr { ... } else { ... }
```

Intended meaning (desugaring model):

```cog
match expr {
    pat => { ... }
    _ => { ... }
}
```

### `while let`

Planned syntax:

```cog
while let pat = expr { body }
```

Intended meaning (desugaring model):

```cog
loop {
    match expr {
        pat => { body }
        _ => break
    }
}
```

### Range expressions

Planned syntax (Rust-like):

- `a..b` (half-open, excludes `b`)
- `a..=b` (inclusive)

These are primarily intended for use with a planned `for` loop and other
library-defined iteration patterns; the concrete runtime representation is not
specified yet.

## Struct literal shorthands (planned for 0.2.0)

Planned field init shorthand:

```cog
// equivalent to `Point { x: x, y: y }`
Point { x, y }
```

## Enum struct variants (planned for 0.2.0)

Planned enum variant declaration forms:

```cog
enum E {
    Tuple(i32, i32),
    Struct { x: i32, y: i32 },
}
```

Planned construction and patterns:

```cog
let v = E::Struct { x: 1, y: 2 };
match v {
    E::Struct { x, y } => x + y,
    _ => 0,
}
```

## Closures (syntax reserved; semantics incomplete)

Cog intends to adopt Rust-like closure literals:

```cog
let add1 = |x: i32| x + 1;
let y = add1(41);
```

Intended representation model (subject to revision):
- a closure value behaves like `{ env_ptr, fn_ptr }`
- the call operator loads `fn_ptr` and passes `env_ptr` as an implicit first argument

Open design question (must be resolved before stabilizing closures):
- capture semantics (by value vs by pointer) in a language without borrow checking.

## Arbitrary bit-width integers (planned)

Cog intends to support integer types like `u1`, `u2`, … and `i1`, `i2`, … up to
some maximum (likely `128`) for both computation and layout control.

## Additional ABIs and interop

- Additional ABIs beyond C for `extern(...)` / `export(...)`.
- Importing C headers (out of scope for v0.1).

## Wrapping/saturating arithmetic

- Explicit wrapping and saturating operations in `core` (instead of relying on `ReleaseFast` UB).

## A multi-crate build/package system

- Zig-style `build.cg` as the primary build description.
- A `gear` tool that generates `build.cg` and provides a Cargo-like UX.
