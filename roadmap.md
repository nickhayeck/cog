# Cog compiler roadmap (concrete next steps)

This roadmap focuses on turning the current “parses and prints a tree” prototype into a usable compiler front-end, while staying aligned with the constraints: **no borrow checker**, **no lifetimes**, **no generics (yet)**, **no noalias assumptions**.

## v0.0.2 — Parser coverage + AST hygiene
- Replace the generic `Node` tree with a typed AST (separate structs for items/types/expr/pat) and add source spans (`file:line:col..`) to every node.
- Expand parsing coverage to match the intended subset:
  - Items: `type` aliases, `static`, `const` (full), `mod name;`, `use` forms (`use a::{b,c};`, `use a as b;`).
  - Exprs: `if/else`, `while`, `loop`, `break`, `continue`, `return expr`, unary `!`, comparisons (`== != < <= > >=`), `&& ||`, `%`.
  - Types: tuples `(T, ...)`, function types (if needed), arrays `[T; N]` where `N` is comptime expr.
  - Patterns: identifier bindings, tuple patterns, struct patterns, `Enum::Variant(p0, ...)`, match guards (`pat if expr`).
- Add a dedicated `parse` test suite:
  - `test/parse/*.cg` + `ctest` that asserts “parses successfully” (and later “prints stable AST”).
- Add a `--dump-ast` flag (tree) and a `--dump-tokens` flag (lexer) for debugging.

## v0.0.3 — Module + name resolution
- Define crate layout rules for `mod name;` (file path mapping) and implement module loading.
- Implement `use` resolution with namespaces (types vs values vs modules) and shadowing rules.
- Build symbol tables for each module; emit concrete “cannot find item `X` in module `Y`” diagnostics.
- Implement method collection:
  - Index inherent `impl Type { ... }` by receiver type name.
  - Index trait `impl Trait for Type { ... }` by (trait, type).

## v0.0.4 — Type system (no generics) + move checking
- Implement nominal type checking for:
  - Struct/enum definitions, field access, constructor literals, enum variants.
  - Function calls and method calls (inherent methods first, then in-scope traits).
  - `match` typing (all arms unify) and exhaustiveness as “future” (start with “no”).
- Implement pointer type rules:
  - `mut* T -> const* T` implicit coercion.
  - `dyn Trait` object types behind pointers only (`const* dyn Trait`, `mut* dyn Trait`).
- Implement a local move checker for functions:
  - Track moves of locals and fields conservatively.
  - Reject use-after-move; allow `Copy` types to be used after “move”.

## v0.0.5 — Trait objects + vtables (typing + lowering)
- Represent trait method sets and build vtables for `dyn Trait`.
- Type-check `receiver.method(...)` where receiver is `* dyn Trait` as dynamic dispatch.
- Lower dyn calls into `(data_ptr, vtable_ptr)` load + indirect call.

## v0.0.6 — Comptime interpreter (minimal, useful)
- Define comptime-evaluable subset and implement an interpreter for:
  - Integer arithmetic, `if`, `while/loop` (bounded), `match` on ints/enums.
  - Struct/enum/array construction and indexing.
  - `builtin::compile_error` to stop compilation with a message.
- Wire comptime into:
  - `const` initializers
  - array lengths `[T; N]`
  - `type` aliases where RHS is comptime `type` (parsing now, semantics later)

## v0.0.7 — IR + LLVM skeleton
- Design a small, explicit IR (SSA optional at first) for:
  - locals, loads/stores, calls, branches, match lowering
  - struct/enum layouts (with default `repr(C)`)
- Emit LLVM IR:
  - function definitions, calls, returns
  - struct layouts and GEP field access
  - basic control flow

## C interop track (ongoing)
- Decide the “no-FFI” surface:
  - `use_interop "header.h";` parsing + placeholder AST node
  - mapping of C declarations into Cog items (types, functions, constants)
- Implement `#[link_name="..."]` and `#[link(lib="...")]` attributes for symbol/link control.

