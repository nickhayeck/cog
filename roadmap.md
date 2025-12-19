# Cog compiler roadmap

This roadmap focuses on reaching a usable compiler pipeline while staying aligned with the core constraints: **no borrow checker**, **no lifetimes**, **no generics**, **no noalias assumptions**.

Status: **v0.0.4 is implemented** (front-end only: parse → resolve → type+move check). See `examples/v0_0_4/main.cg` for a representative program.

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

## Next milestones

### v0.0.5 — Trait objects + vtables (typing + lowering)
- Finish type checking for calls on `* dyn Trait` (receiver dispatch through trait method set).
- Define vtable layout and emit a vtable symbol per `(Trait, ConcreteType)` impl.
- Lower `dyn` method calls into indirect calls via `(data_ptr, vtable_ptr)`.

### v0.0.6 — Comptime interpreter (minimal, useful)
- Define comptime-evaluable subset and implement an interpreter for:
  - integer arithmetic + comparisons
  - `if`, `while/loop` (with bounds), `match` on ints/enums
  - struct/enum/array construction and indexing
  - `builtin::compile_error`
- Wire comptime into:
  - `const` / `static` initializers (where permitted)
  - array lengths `[T; N]`
  - (later) comptime parameters and specialization

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
