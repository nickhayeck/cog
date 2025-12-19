# Cog compiler roadmap

This roadmap focuses on reaching a usable compiler pipeline while staying aligned with the core constraints: **no borrow checker**, **no lifetimes**, **no generics**, **no noalias assumptions**.

Status: **v0.0.6 is implemented** (front-end + minimal comptime const-eval). See `examples/v0_0_6/main.cg` and `comptime_design.md`.

## Completed

### v0.0.2 — Parser coverage + typed AST (done)
- Typed AST with source spans for every node (`src/ast.hpp`, `src/span.hpp`).
- Lexer + parser coverage for the v0.0.x subset (no generics): `if/else`, `while`, `loop`, `break/continue/return`, tuple types/expr/pats, struct patterns, match guards, `use` alias/groups, `mod name;`, `static`, `type` aliases, bool literals, precedence-layered binary ops.
- Debug flags: `cogc --dump-tokens <file.cg>` and `cogc --dump-ast <file.cg>`.

### v0.0.3 — Module + name resolution (done)
- Module tree with inline `mod name {}` and out-of-line `mod name;`.
- Out-of-line file mapping for `mod name;`: `D/name.cg` then `D/name/mod.cg`.
- `use` resolution for `use path;`, `use path as alias;`, `use path::{...};`.
- Namespaces for modules/types/values + concrete diagnostics for missing items.
- Method indexing for inherent impls and trait impls.

### v0.0.4 — Type system + local move checking (done)
- Nominal type checking for structs/enums, field access, struct literals, enum variants, calls.
- Method calls: inherent methods first, then in-scope traits with an impl for the receiver type.
- `match` typing (arm unification) + match guards (`if guard` must be `bool`).
- Pointer rules:
  - implicit `mut* T` → `const* T` coercion
  - `dyn Trait` and `[T]` must appear behind pointers
- Conservative local move checker:
  - rejects use-after-move of locals
  - assignment re-initializes locals
  - `Copy` modeled for primitives/pointers and aggregates of `Copy`

### v0.0.5 — Trait objects + vtables (typing) (done)
- Type-check `receiver.method(...)` where the receiver is `const* dyn Trait` / `mut* dyn Trait` using the trait method set.
- Validate trait impls: no extra methods, no missing methods, and method signatures match (with `Self` substitution).
- Object-safety (minimal): dyn-dispatchable methods must take `self: const* Self` or `self: mut* Self` and must not mention `Self` elsewhere.
- Note: actual IR/LLVM lowering of dyn calls is deferred until the IR exists (see v0.0.7).

### v0.0.6 — Comptime interpreter (minimal, useful) (done)
- Implement a minimal comptime interpreter for:
  - integer/bool literals, arithmetic/comparisons, `if`, `while`, `loop`, `match` + guards
  - struct/enum construction, tuple values, field access
  - `builtin::compile_error` (emits an error when executed at comptime)
- Wire comptime into:
  - `const` / `static` initializers (all are evaluated in v0.0.6)
  - array lengths `[T; N]` (length expressions are evaluated as `usize`)
- Current limitations (planned extensions):
  - comptime function calls and dyn/method calls are not supported
  - comptime indexing/array literals are not supported yet

## Next milestones

### v0.0.7 — IR + LLVM skeleton
- Introduce a small explicit IR for locals, calls, branches, and struct/enum layouts (default `repr(C)`).
- Emit LLVM IR for:
  - functions, calls, returns
  - control flow and basic expression lowering
  - nominal type layouts and field access (GEP)

## C interop track (ongoing)
- Parse `use_interop "header.h";` as a placeholder item and plumb it through name resolution.
- Decide mapping of C declarations into Cog items (types, functions, constants).
- Add symbol/link control attributes (e.g. `#[link_name="..."]`, `#[link(lib="...")]`).
