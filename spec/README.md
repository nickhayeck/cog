---
title: Cog Core Language Specification (Draft)
version: 0.1.0-draft
status: draft
---

# Cog core language specification (draft)

This directory contains the **normative** specification for Cog’s **core language** as targeted for **v0.1.0**.

The reference compiler in this repo may accept additional syntax (or be missing some features); in conflicts between implementation and these docs, **these docs define the intended language**.

## Design principles

- **Rust-like surface syntax**: items (`mod`, `use`, `pub`, `struct`, `enum`, `impl`, `fn`, `const`, `static`, `type`), expression blocks, pattern matching.
- **No borrow checking / no lifetimes**: pointers are just pointers; they may be null/dangling/aliasing and may freely escape scope.
- **Move-by-default** with a **local use-after-move** rejection pass.
- **Zig-like build modes**:
  - `Debug`, `ReleaseSafe`: runtime traps for many safety checks.
  - `ReleaseFast`: many safety failures are **undefined behavior** (UB).
- **Zig-like comptime**:
  - explicit `comptime { ... }` blocks and `comptime` parameters
  - comptime is interpreted; compilation emits only **residual runtime operations**
  - no “template specialization” semantics as a language feature (memoization is an optimization only)
- **C interop** through **item tags** (no `extern fn` keyword forms):
  - `fn[extern(C)] ...;` for imported declarations
  - `fn[export(C)] ... { ... }` for exported definitions

## What is explicitly *not* in the v0.1 core spec

- Borrow checker, lifetimes, and ownership-based aliasing guarantees.
- User-defined parametric polymorphism syntax (angle-bracket generics).

## Document map

- `spec/lexical.md`: tokens, keywords, literals, comments, and whitespace.
- `spec/syntax.md`: grammar for the core language.
- `spec/builtins.md`: compiler-provided builtins and intrinsics.
- `spec/build_modes.md`: UB model, traps, and runtime checks.
- `spec/modules.md`: crates, modules, `use`, and visibility.
- `spec/types.md`: types (including `type`, `comptime_int`, pointers, slices, enums/structs, `!`).
- `spec/layout_abi.md`: data layout (`repr(cog)`, `repr(C)`, `repr(packed)`), calling conventions, symbol naming, varargs.
- `spec/items.md`: item semantics (`fn`, `impl`, `const`, `static`, `type`) and method calls.
- `spec/expressions.md`: expression semantics (including unspecified evaluation order) and control flow.
- `spec/patterns.md`: pattern semantics and match exhaustiveness.
- `spec/moves.md`: moves, `Copy` types, and match move behavior.
- `spec/comptime.md`: comptime contexts, interpreter semantics, determinism, heap + serialization, reflection, and type construction.
- `spec/stdlib.md`: required core surface (`core::Option`, `core::Result`, `?`, and required runtime symbols).
- `spec/future.md`: explicitly planned extensions and open questions.

## Notation and normative language

- **Must / must not**: required for conformance.
- **Should**: strongly recommended; may be deferred with justification.
- **May**: permitted behavior.
- `T`, `U`, `E` are metavariables for types.
- “Comptime-only” means a value cannot exist at runtime and is erased by compilation.
