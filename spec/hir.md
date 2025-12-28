---
title: Compiler HIR Design (Non-normative)
version: 0.0.19-main (design draft)
status: non-normative
---

# Compiler HIR design (non-normative)

This document specifies the intended **HIR** (High-level Intermediate Representation) used inside the reference compiler.

HIR is **not** part of the Cog language spec. It is an internal compiler representation whose purpose is to:
- provide stable, ID-based nodes (instead of “pointer identity + side tables”),
- make name resolution and typing explicit in the IR itself,
- normalize/record desugarings (method calls, builtins, etc.) once,
- and serve as the clean input to MIR lowering (`spec/mir.md`).

## Position in the pipeline

Intended front-end pipeline:

1) **AST**: parser output (`src/ast.hpp`), close to source syntax.
2) **Resolution + typing**: today these are computed as side tables over AST pointers.
3) **HIR**: resolved + typed, with stable IDs.
4) **MIR**: CFG-based mid-level IR (`spec/mir.md`).
5) **LLVM IR**: backend lowering.

HIR is the boundary where the compiler stops caring about syntax trivia and starts caring about program meaning.

## Design principles

HIR should be:
- **Resolved**: every name/path use is resolved to a specific definition or binding.
- **Typed**: every expression and pattern binding has a `TypeId` assigned.
- **Stable**: nodes are referred to by integer IDs (`ExprId`, `DefId`, etc.).
- **Mostly tree-shaped**: unlike MIR, HIR keeps structured control flow (`if`, `match`, etc.) as nested nodes.
- **Low ceremony**: HIR should be the *smallest* refactor that removes pointer-keyed tables and helps MIR lowering.

## IDs and core data model

### Definitions (`DefId`)

Every named item in the crate has a stable `DefId`:
- functions
- structs/enums and their fields/variants
- const/static items
- modules
- type aliases

Suggested representation:

```
DefId := { module: ModuleId, index: u32 }
```

The compiler assigns indices during a deterministic crate traversal.

### Expressions (`ExprId`) and bodies

Function bodies (and const/static initializers) are stored as HIR expression trees.

Each expression has:
- an `ExprId`
- a `Span`
- a `TypeId`
- a variant/tag describing its shape

### Locals and bindings

HIR distinguishes:
- **locals** introduced by `let` and parameters (value bindings)
- **pattern bindings** introduced by patterns in `let` and `match`

Suggested representation:

```
LocalId := u32
BindingId := u32
```

Each binding carries:
- name
- mutability (if applicable)
- `TypeId`
- span

## HIR items (outline)

HIR item nodes keep the surface language structure, but with resolved references:

- `HirItemFn { def: DefId, sig: FnSig, body: Option<BodyId>, tags: FnTags, vis: Visibility }`
- `HirItemStruct { def: DefId, fields: [...], repr: StructRepr, vis: Visibility }`
- `HirItemEnum { def: DefId, variants: [...], tag_type: Option<TypeId>, vis: Visibility }`
- `HirItemConst { def: DefId, ty: TypeId, init: BodyId, vis: Visibility }`
- `HirItemStatic { def: DefId, ty: TypeId, init: BodyId, vis: Visibility }`
- `HirItemTypeAlias { def: DefId, rhs: HirType, vis: Visibility }`
- `HirItemMod { def: DefId, items: [DefId], tags: ModTags, vis: Visibility }`

Notes:
- HIR can store both the original tag syntax and the validated/normalized tag meaning. For example, `fn[extern(C)]` can become `FnAbi::C` with `link_name`.
- `use` items typically disappear from HIR as “items”; instead, their effects are recorded in the resolver’s scope tables. If kept, they should be stored only for diagnostics/debugging.

## HIR types

HIR needs a way to represent:
- runtime types (`i32`, `const* T`, `[T; N]`, `fn(...) -> R`, etc.)
- and type-level calls in type positions (e.g. `Option(i32)`).

Recommended:
- Store “fully lowered” types as `TypeId` *when possible*.
- For type syntax that requires comptime evaluation (type-level calls), store a small HIR type AST:

```
HirType :=
  | Resolved(TypeId)
  | TypeCall { callee: DefId, args: [HirType] }
```

Then, during “type lowering” (which now becomes “HIR type folding”), evaluate `TypeCall` at comptime and replace it with `Resolved(TypeId)`.

This preserves a clean layering:
- parsing creates syntactic type nodes,
- resolution identifies the callee definition,
- comptime evaluates type calls,
- and after that everything is a `TypeId`.

## HIR expressions (outline)

HIR expressions mirror surface syntax, but with resolution and typing baked in.

Representative variants:
- literals (`Int`, `Float`, `Bool`, `String`, `CString`, `Unit`)
- `Local(LocalId)` and `Binding(BindingId)` for value reads
- `Item(DefId)` for value-level item references (`fn` values, consts, statics)
- field/index/deref addressable operations as explicit “place-like” nodes:
  - `Field { base: ExprId, field: FieldId }`
  - `Index { base: ExprId, index: ExprId }`
  - `Deref { ptr: ExprId }`
  - `AddrOf { mutability, place: ExprId }` (after place-checking)
- calls:
  - `Call { callee: ExprId, args: [ExprId] }` (direct and indirect)
  - `DirectCall { fn_def: DefId, args: [ExprId] }` (optional optimization for MIR lowering)
- control flow:
  - `Block { stmts: [...], tail: Option<ExprId> }`
  - `If { cond: ExprId, then: ExprId, else_: Option<ExprId> }`
  - `While { cond: ExprId, body: ExprId }`
  - `Loop { body: ExprId }`
  - `Match { scrut: ExprId, arms: [...] }`
- casts:
  - `Cast { value: ExprId, to: TypeId }`
  - optionally normalize `as` to an explicit builtin call (see below)
- `Comptime { body: ExprId }`

### Builtins in HIR

To avoid “stringly-typed builtin recognition” in later passes, HIR should encode builtin calls explicitly:

```
HirExpr::BuiltinCall { builtin: BuiltinId, args: [ExprId] }
```

Where `BuiltinId` includes:
- `SizeOf`, `AlignOf`, `TypeInfo`, `CompileError`, `Cast`
- type construction constructors (`TypeStruct`, `TypeEnum`, `TypePtrConst`, ...)

This also makes it easy to enforce restrictions like:
- `builtin::compile_error` is comptime-only
- `builtin::type_*` constructors are comptime-only

### Method calls

HIR should record method resolution results so MIR lowering does not redo method lookup. 

`recv.m(a, b)` becomes `DirectCall { fn_def: resolved_method, args: [recv', a, b] }`, where `recv'` is either the receiver value or an `AddrOf` of a temporary/place depending on receiver mode.

## Evaluation order

Cog guarantees left-to-right evaluation order (`spec/expressions.md`).

HIR does not need to “encode” this explicitly, but MIR lowering must lower HIR expressions in a way that preserves this order.

## HIR → MIR lowering boundary

HIR is intended to be the only input to MIR lowering.

At the boundary:
- all expression nodes have known `TypeId`s
- name resolution results are explicit (`DefId`, `FieldId`, `VariantId`, etc.)
- type-level calls in types have been evaluated and folded to `TypeId`
- method calls and builtins are explicit (no repeated resolution logic)

This keeps MIR lowering a mostly-mechanical transformation.

## Debugging and tooling

Recommended compiler flags:
- `cogc --emit-hir <out.hir> <file.cg>`: emit HIR after resolution and type checking.

HIR printing should include:
- the HIR item graph (modules/items and their `DefId`s),
- per-body expression trees with `ExprId`s and `TypeId`s,
- and resolved references (callee `DefId`, field/variant IDs, builtin IDs).

Note: in early v0.1 development, HIR text output does not need to be stable. Before 1.0, we likely want to stabilize it enough to write tests against it (see `spec/future.md`).
