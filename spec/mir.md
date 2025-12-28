---
title: Compiler MIR Design (Non-normative)
version: 0.0.19-main (design draft)
status: non-normative
---

# Compiler MIR design (non-normative)

This document specifies the intended **MIR** (Mid-level Intermediate Representation) for the Cog reference compiler in this repo.

It is **not** part of the Cog language spec: changing this IR is not a breaking change for the language. It exists to:
- make comptime interpretation and residualization *practical* and correct,
- ensure the interpreter and codegen share one semantics engine,
- simplify lowering passes (`?`, match lowering, build-mode traps, etc),
- and keep the compiler maintainable as the language grows.

## High-level pipeline

The intended long-term structure is:

1) **AST**: parser output (`src/ast.hpp`), close to source syntax.
2) **HIR**: resolved + typed tree with stable IDs (`spec/hir.md`).
3) **MIR**: a typed, CFG-based IR with explicit temporaries and control flow (this document).
4) **LLVM IR**: backend lowering.

We do **not** need Zig’s full ZIR→AIR split immediately. For the next stage of the prototype, a single well-designed MIR is enough.

## Design principles (MIR)

MIR is designed to be:

- **Typed**: every value has a `TypeId` from the compiler’s type store.
- **CFG-based**: explicit basic blocks with explicit branches.
- **Not SSA** (initially): use locals + place projections (Rust-MIR-like).
  - LLVM lowering should start with `alloca`-based locals and rely on LLVM’s `mem2reg`/promotion passes as the 80/20 solution.
- **Semantics-preserving lowering**: Cog specifies left-to-right evaluation order (`spec/expressions.md`).
  - MIR lowering must preserve that order by making it explicit with temporaries.
  - The interpreter and LLVM lowering both operate on MIR, so they share the same semantics.
- **Lowering-friendly**: most surface features desugar cleanly:
  - `if/else`, `while`, `loop`, `break expr`, `match`
  - `?` (early return)
  - bounds checks / overflow checks insertion
- **Interpreter-friendly**: instructions are simple, explicit, and side effects are obvious.

## Left-to-right evaluation order

Cog guarantees left-to-right evaluation order for subexpressions (`spec/expressions.md`).

MIR makes this order explicit by lowering expression trees into:
- temporaries (`LocalId`s),
- explicit `Assign` statements,
- and explicit control-flow edges.

This is important for:
- correct comptime interpretation of real code,
- consistent semantics between interpreter and codegen,
- and inserting/removing checks depending on build mode.

## Core entities

### Program

A MIR program contains:
- function bodies (for all `fn` items that have a body),
- function declarations (for `fn[extern(C)]` items),
- constants and statics (as global initializers),
- and string literal storage objects.

### Function body

Each function body contains:
- a list of **locals** (arguments + user locals + compiler temporaries),
- a list of **basic blocks**,
- a designated start block,
- and span information for statements/terminators.

Locals are indexed by `LocalId` and have:
- `TypeId ty`
- mutability (for `let` vs `let mut` if/when that exists; today many locals are assignable)
- debug name (optional; used for `--emit-mir`).

### Basic blocks

Each basic block contains:
- `statements: [Statement]` (linear sequence)
- `terminator: Terminator` (exactly one)

## Values: places, operands, rvalues

MIR distinguishes:

- **Place**: an addressable location (lvalue), e.g. a local, a field, a deref, an index.
- **Operand**: something you can “use” (copy/move a place or a constant).
- **Rvalue**: a computation that produces a value (possibly with reads, but not writes except through explicit `Store`/`Assign`).

This mirrors “expression lowering” to explicit temporaries and keeps side effects obvious.

### Place

```
Place := { base: PlaceBase, projection: [Projection] }

PlaceBase :=
  | Local(LocalId)
  | Static(GlobalId)          // `static NAME`
  | ConstAddr(ConstId)        // internal: materialized `&CONST`
```

Projections:
- `Deref`                      (`*p`)
- `Field(FieldId)`             (`s.field` / tuple index `.0`)
- `Index(LocalId)`             (`a[i]` for arrays/slices/pointers; index value in a local)
- `Downcast(VariantId)`        (select enum variant payload storage)

Notes:
- `Index` uses a `LocalId` (or an `Operand`) so the index expression is evaluated once and named.
- For slices, indexing is lowered as a bounds check + pointer arithmetic + deref.

### Operand

```
Operand :=
  | Copy(Place)
  | Move(Place)
  | Const(ConstValue)
  | Fn(FnId)                   // function item value (for function pointers)
```

### Rvalue

Rvalues are pure computations (reads allowed; writes performed via `Assign`).

Representative set (expand as needed):

```
Rvalue :=
  | Use(Operand)
  | UnaryOp(op, Operand)
  | BinaryOp(op, Operand, Operand)
  | Cast(Operand, TypeId)                    // same semantics as `builtin::cast`
  | AddrOf(mutability, Place)                // `&place` / `&mut place`
  | Aggregate(kind, [Operand])               // tuples, arrays, struct literals, enum variants
  | GetSliceLen(Operand)                     // for `const* [T]` / `mut* [T]`
  | SliceDataPtr(Operand)                    // extract `ptr` from slice fat pointer
  | PtrOffset(Operand /*ptr*/, Operand /*idx*/, TypeId /*elem*/)
  | Load(Place)                              // optional; usually `Use(Copy(place))` is enough
```

Notes:
- Whether `Load` exists is an implementation choice; many MIRs represent loads via `Copy(place)`.
- `Aggregate` covers:
  - tuple construction `(a, b)`
  - array construction `[a, b, c]` and `[x; N]` (after lowering)
  - struct literals `Point { x: a, y: b }`
  - enum constructors `E::Some(x)` (payload enums)

## Statements and terminators

### Statements

Representative set:

```
Statement :=
  | Assign(Place, Rvalue)
  | SetDiscriminant(Place, VariantId)        // payload enums; sets active variant
  | Nop
```

Build-mode safety checking is represented using explicit `Assert` statements:

```
Statement :=
  | Assert { cond: Operand, kind: AssertKind, span: Span }
```

`AssertKind` includes:
- integer overflow
- div-by-zero
- shift-range
- bounds
- null deref
- misalignment
- enum tag validity

Lowering inserts these asserts; later passes can erase them in `ReleaseFast`.

### Terminators

Representative set:

```
Terminator :=
  | Return
  | Goto(BlockId)
  | SwitchInt { scrut: Operand, cases: [(u128, BlockId)], otherwise: BlockId }
  | Call { callee: Operand, args: [Operand], ret: Place, next: BlockId }
  | Unreachable
```

Notes:
- `SwitchInt` is used for `bool`, integers, and fieldless `enum[tag(IntType)]`.
- Payload enum `match` can lower to “read tag; switch; downcast; bind payload”.

## Lowering rules (HIR → MIR)

### General strategy

Lower HIR expressions to MIR by:
- creating temporaries for intermediate values,
- emitting explicit statements in a chosen order,
- and returning an `Operand` (or `Place` when an expression is a place expression).

Lowering produces a CFG; control-flow expressions (`if`, `match`, loops) generate blocks.

### `if`

Lower:
- evaluate condition into a local,
- `SwitchInt` to then/else blocks,
- each branch writes to a join-local when the `if` expression yields a value,
- control flows to a join block.

### `while` and `loop`

Lower loops as CFG with:
- a header block (condition),
- a body block,
- an exit/join block.

For `break expr` (value-yielding loops):
- create a hidden “loop result” local of the loop’s type,
- each `break value` assigns to that local then branches to the exit,
- the loop expression evaluates to `Copy(loop_result_local)`.

### `match`

Lower:
- evaluate scrutinee once into a local,
- build a decision chain:
  - `SwitchInt` for integer/bool/fieldless-enum matches,
  - or tag-switch + downcast for payload enums.
- each arm’s body is lowered into its own block(s),
- arm results are written into a join-local if the match yields a value.

Pattern bindings become locals assigned from projections on the scrutinee.

Guards:
- evaluate guard after binding,
- if false, continue to next arm.

### Calls and method calls

Lower calls by:
- evaluating callee + args to locals in left-to-right order,
- emitting a `Call` terminator which writes to a destination place.

Method calls should already be resolved in HIR (either desugared to a direct call or stored with an explicit resolved `DefId` target). MIR lowering uses that information and does not perform method lookup.

### Indexing

Lower `a[i]` into:
- evaluation of `a` then `i`,
- for arrays/slices: bounds check `Assert`,
- for slices: compute element pointer from `{ptr,len}`,
- for pointers: pointer arithmetic without bounds check,
- dereference result as a place.

### `builtin::compile_error`

`builtin::compile_error` is comptime-only and must not appear in runtime MIR.

In comptime MIR evaluation, it is a hard failure with the provided message.

## Comptime execution on MIR (interpreter)

MIR is the **single semantics source** for both:
- runtime codegen (LLVM lowering),
- and comptime execution (interpretation).

### Value domain

The interpreter evaluates MIR with a value domain that can represent:
- concrete comptime values (ints, floats, bool, aggregates, type values),
- and “residual” runtime values (represented by MIR locals in a residual body).

Conceptually:

```
Value :=
  | CT(ComptimeValue)
  | RT(LocalId)     // residual value computed at runtime
```

### Residualization model

When interpreting a function with comptime parameters:
- If a computation depends only on `CT` values, evaluate it and keep the result as `CT`.
- If a computation depends on any `RT` value, emit the corresponding MIR statement into a residual body and treat the result as `RT(new_local)`.

Control flow:
- If a branch condition becomes `CT(true/false)`, interpret only the selected branch (dead branch erased).
- If a branch condition is `RT`, residualize the branch:
  - create residual blocks,
  - emit a residual `SwitchInt`,
  - and interpret both branches under residual contexts.

This is the heart of Zig-style comptime execution.

### Caching

To avoid re-evaluating comptime functions repeatedly, the compiler may memoize based on:
- callee function ID,
- comptime argument values,
- and (for type construction) descriptor contents.

Memoization is an optimization; the language does not expose “specializations”.

## LLVM lowering (MIR → LLVM)

The LLVM backend should:
- translate locals to `alloca`s initially and run a standard LLVM optimization pipeline (including `mem2reg` where applicable),
- translate `Assert` to runtime checks/traps depending on build mode,
- translate `SwitchInt` to `switch` or compare chains,
- map aggregates to LLVM structs/arrays,
- use layout info (DataLayout/TargetMachine) as the source of truth for offsets.

## Debugging and tooling

Recommended compiler flags:
- `cogc --emit-mir <out.mir> <file.cg>`: emit MIR after lowering.
- `cogc --emit-mir-after <pass> <out.mir> <file.cg>`: emit MIR after a specific pass (optional).

MIR printing should include:
- locals with types,
- blocks with statements/terminators,
- and spans for diagnostics.

Note: in early v0.1 development, MIR text output does not need to be stable. Before 1.0, we likely want to stabilize it enough to write tests against it (see `spec/future.md`).
