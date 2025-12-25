---
title: Planned Extensions (Non-normative)
version: 0.1.0-draft
---

# Planned extensions (non-normative)

This chapter lists features that are explicitly *not* fully specified for v0.1, but are part of Cog’s intended trajectory.

## Additional literals

- Hex/binary/octal integer literals.
- Unicode escapes for strings/chars.

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

## Additional ABIs and interop

- Additional ABIs beyond C for `extern(...)` / `export(...)`.
- Importing C headers (out of scope for v0.1).

## Wrapping/saturating arithmetic

- Explicit wrapping and saturating operations in `core` (instead of relying on `ReleaseFast` UB).

## A multi-crate build/package system

- Zig-style `build.cg` as the primary build description.
- A `gear` tool that generates `build.cg` and provides a Cargo-like UX.
