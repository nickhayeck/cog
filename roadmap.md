# Cog compiler roadmap
This document is written as a **spec-driven** path to **v0.1.0 conformance**. Each milestone should include:
- a goal
- summary of changes
- spec updates (only when needed to clarify ambiguity),
- examples that exercise the feature(s),
- and at least one targeted compile test (positive or negative).

Status: **v0.0.19-main is implemented** (everyday literals/operators + indexing + float/bitwise surface + LLVM backend + basic comptime). The v0.1 core language spec lives in `spec/README.md`.

Key spec gaps as of v0.0.19-main (blocking v0.1.0):
- IR pipeline refactor: AST → HIR → MIR → LLVM (`spec/hir.md`, `spec/mir.md`)
- Builtins: implement `spec/builtins.md` (notably `builtin::cast` and type construction)
- `type` as a first-class comptime value + type-level calls in type positions (`spec/comptime.md`, `spec/types.md`, `spec/syntax.md`)
- `auto` type placeholder (`spec/auto.md`)
- `core::Option/Result` + `?` (`spec/stdlib.md`)
- `comptime_int`/`comptime_float` semantics (`spec/types.md`, `spec/lexical.md`)
- `loop` as an expression via `break expr` (`spec/expressions.md`)
- Structural `Copy` for structs + fieldless `enum[tag(..)]` (`spec/moves.md`)
- `&CONST` materialization + pointer-receiver rvalue temporaries (`spec/items.md`)
- Visibility enforcement + full tag validation (`spec/modules.md`, `spec/layout_abi.md`)
- `repr(C)` layout + build modes/runtime traps (`spec/layout_abi.md`, `spec/build_modes.md`)

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
- Method indexing for inherent impls.

### v0.0.4 — Type system + local move checking (done)
- Nominal type checking for structs/enums, field access, struct literals, enum variants, calls.
- Method calls: inherent methods (`impl Type { ... }`).
- `match` typing (arm unification) + match guards (`if guard` must be `bool`).
- Pointer rules:
  - implicit `mut* T` → `const* T` coercion
  - `[T]` must appear behind pointers
- Conservative local move checker:
  - rejects use-after-move of locals
  - assignment re-initializes locals
  - `Copy` modeled for primitives/pointers/tuples/arrays (struct/enum structural `Copy` is still pending for v0.1 conformance; see v0.0.24)

### v0.0.5 — Legacy: dynamic dispatch scaffolding (done; not part of v0.1 spec)
- The prototype implemented a trait/dyn object system for experimentation.
- The v0.1 core language spec removes this mechanism; users can hand-roll vtables where needed.
- Removed in v0.0.14.

### v0.0.6 — Comptime interpreter (minimal, useful) (done)
- Implement a minimal comptime interpreter for:
  - integer/bool literals, arithmetic/comparisons, `if`, `while`, `loop`, `match` + guards
  - struct/enum construction, tuple values, field access
  - `builtin::compile_error` (emits an error when executed at comptime)
- Wire comptime into:
  - `const` / `static` initializers (all are evaluated in v0.0.6)
  - array lengths `[T; N]` (length expressions are evaluated as `usize`)
- Limitations in this initial interpreter (lifted in later milestones):
  - comptime function calls (added in v0.0.16)
  - comptime indexing/array literals (added in v0.0.19)

### v0.0.7 — Layout engine + layout builtins (done)
- Layout engine (`src/layout.hpp`, `src/layout.cpp`) for primitives, pointers (incl. fat pointers), tuples, arrays, structs, and enums.
- Default `repr(cog)` model (currently source-order layout); `repr(packed)` is implemented and `repr(C)` is reserved for stable C interop layout.
- `builtin::size_of(T)` and `builtin::align_of(T)` type-check and evaluate at comptime using the layout engine.

### v0.0.9 — LLVM backend + executable driver (done)
- Direct AST→LLVM emission (no IR yet) for a small but runnable subset, using LLVM’s C++ API.
- CLI:
  - `cogc --emit-llvm <out.ll> <file.cg>`
  - `cogc --emit-bc <out.bc> <file.cg>`
  - `cogc --emit-obj <out.o> <file.cg>`
  - `cogc --emit-exe <out> <file.cg>` (links via system `clang`)
- Emits: ints/bools, blocks + tail expr, `if/else`, `let` + assignment, struct literals + field access, pointer deref, direct calls, method calls, and address-of (`&` / `&mut`).

### v0.0.10 — Legacy: dynamic dispatch lowering (done; not part of v0.1 spec)
- The prototype lowered dynamic-dispatch calls through vtables.
- The v0.1 core spec does not include dynamic dispatch.
- Removed in v0.0.14.

### v0.0.11 — Enums + match/loops (LLVM) (done)
- LLVM codegen for `while`/`loop` + `break`/`continue`.
- LLVM codegen for `match` on ints/bools/enums (ordered decision chain) with basic bindings and guards.
- LLVM representation for enums (tag + payload) and runtime constructors (including unit variants as values).

### v0.0.12 — FFI surface (tags + extern/export + varargs) + string literals (done)
- Migrate fully from `#[...]` to keyword tags after item keywords (`struct[...]`, `enum[...]`, `fn[...]`).
- Implement `fn[extern(C)]` imported declarations (no body) + extern-only `...` C varargs.
- Implement `fn[export(C)]` exported definitions (body required; symbol name is unmangled).
- Enforce an integer-tagged enum representation only on fieldless enums (prototype syntax uses `repr(i32)`; core spec uses `tag(i32)`).
- LLVM backend: avoid mangling for extern/export functions; emit vararg function types and apply basic C vararg promotions at call sites.
- Initial string literal support for C interop (refined in v0.0.15 to include both `"..."` and `c"..."`).

### v0.0.13 — Spec-surface alignment (tags, enums, visibility, never type, test modules) (done)
- Accept `fn[extern(C)]` / `fn[export(C)]` plus `extern_name(...)` / `export_name(...)` with conflict validation.
- Accept `enum[tag(<int>)]` for fieldless enums; parse discriminant syntax (full discriminant evaluation/validation + codegen is in v0.0.23).
- Parse `pub(crate)`, `!`, and `mod[test]`.

### v0.0.14 — Clean up tech debt: remove trait/dyn + delete deprecated aliases (done)
- Remove trait/dyn parsing, name resolution, type checking, and LLVM lowering (not in `spec/`).
- Remove deprecated prototype surfaces like `enum[repr(i32)]`.
- Keep historical trait/dyn examples out of smoke tests.

### v0.0.15 — Strings: `"..."` slices + `c"..."` C strings (done)
- Lexer: add `c"..."` token form and `\\0` escape.
- Type checker: `"..."` is `const* [u8]` and `c"..."` is `const* u8`.
- Codegen: emit readonly string globals, lowering `"..."` as `{ptr,len}` and `c"..."` as NUL-terminated.
- Update `examples/hello_world/main.cg` and add `examples/ffi_tags_strings/main.cg` (renamed from the old `v0_0_15` folder).

### v0.0.16 — Comptime (step 1): function calls + resource limits + reflection (done)
Goal: make comptime usable beyond literals/blocks.

- Interpreter: support calling non-extern functions at comptime (bounded recursion).
- Implement deterministic resource limits:
  - step budget
  - recursion depth
  - comptime heap budget (even if coarse at first)
- Add `builtin::type_info(type)` backed by the layout engine (TypeInfo layout unstable).

### v0.0.17 — Comptime (step 2): comptime parameters + residualization (done)
Goal: implement Zig-style staged evaluation: interpret comptime parts, emit only runtime ops.

- Support `comptime` parameters in function signatures and calls.
- Implement partial evaluation so a call with comptime args produces residual runtime code with no comptime parameters.
- Add memoization keyed by canonicalized comptime argument values (compiler optimization only).
- Current limitation: comptime arguments must be directly computable at the call site (forwarding through locals isn’t supported yet).

### v0.0.18 — Core typing: `!`, match exhaustiveness, tuple structs, function pointers (done)
Goal: fill in core type semantics required for v0.1.

- Integer literals: enforce “fits in type” checks for coercions and `as` casts (first step toward full `comptime_int` / `comptime_float` semantics).
- Implement `!` as a real type:
  - diverging expressions (`return`, non-terminating `loop`, `builtin::compile_error`) have type `!`
  - `!` coerces to any type for arm/unification
- Match exhaustiveness:
  - require exhaustiveness for `bool`, fieldless enums, and payload enums
  - require `_` for integer matches (v0.1 rule)
- Tuple structs + tuple indexing:
  - type-check `struct Name(T0, T1, ...);`
  - allow `.0`, `.1` field access
- Function types/pointers:
  - type-check `fn(T0, ...) -> R` as a type
  - allow `const* fn(...) -> R` values and calls through them
- LLVM backend: add tuple expressions + `.0`/`.1` indexing and indirect calls through function pointers.

### Mini milestone: v0.0.18-main — bring the main function into specification (done)
Goal: allow the full range of `main` functions specified by `spec/layout_abi.md`.

- Emit an entrypoint shim only when building an executable (`--emit-exe`).
- Implement and validate entrypoint selection:
  - accept explicit C ABI `fn[export(C)] main(argc: i32, argv: const* const* u8) -> i32`
  - otherwise synthesize a C ABI wrapper for Cog ABI `main` (`fn main() -> ()` or `fn main() -> i32`)
- Emit clear diagnostics when `main` is missing or has an invalid signature.

### v0.0.19 — Core literals + operators + indexing (done)
Goal: cover “everyday” expression surface required for v0.1 examples.

- Array literals:
  - `[e0, e1, ...]` array value literals
  - `[x; N]` repeat literals (`N` comptime)
- Numeric literal ergonomics:
  - underscores in numeric literals
  - `0x` hex, `0o` octal, `0b` binary integers
  - float literals (incl. exponent form)
- Float types + ops:
  - `f32`/`f64` types
  - arithmetic + comparisons
- Bitwise + shifts for integers:
  - `& | ^ ~ << >>` with Rust-like precedence
- Indexing/codegen:
  - codegen for `a[i]` on arrays and slice pointers
  - raw pointer indexing `ptr[i]` (C-like)
  - array-to-slice pointer coercion: `const* [T; N]` → `const* [T]` and `mut* [T; N]` → `mut* [T]`
- Comptime:
  - array literal construction + indexing/assignment in comptime evaluation (enables LUT/table generation)
- Examples:
  - add `examples/numerics_arrays_floats/` and `examples/crc32_tool/`

## Next milestones

### v0.0.20 — IR pipeline: AST → HIR → MIR → LLVM
Goal: get an end-to-end HIR/MIR pipeline working so comptime interpretation and codegen share one semantics engine (`spec/hir.md`, `spec/mir.md`).

- HIR:
  - Introduce HIR IDs (`DefId`, `ExprId`, `TypeId`, etc.) and store resolved references in the IR.
  - Move “pointer-keyed side tables” (types, resolution results) into HIR nodes.
  - Add `cogc --emit-hir <out.hir> <file.cg>`.
- MIR:
  - Introduce MIR as a typed CFG IR with locals + blocks + explicit `Assert` checks (per `spec/mir.md`).
  - Lower HIR → MIR while preserving left-to-right evaluation order (`spec/expressions.md`).
  - Add `cogc --emit-mir <out.mir> <file.cg>` and `cogc --emit-mir-after <pass> <out.mir> <file.cg>`.
- Backend:
  - Replace direct AST→LLVM emission with MIR→LLVM lowering (alloca-based locals + `mem2reg` as the initial strategy).
  - Keep `--emit-llvm`, `--emit-bc`, `--emit-obj`, `--emit-exe` working.
- Comptime:
  - Execute comptime on MIR (and residualize MIR) for existing v0.0.19 functionality:
    - const/static initializers, array lengths, `comptime { ... }`, comptime parameters.
  - Ensure `builtin::compile_error` remains comptime-only and works through the MIR interpreter.
- Tests/examples:
  - Keep the existing compile suite green.
  - Add at least one test that exercises `--emit-hir` and `--emit-mir`.

### v0.0.21 — Builtins inventory + `type` values + type-level calls (foundation)
Goal: make `type`-producing comptime functions possible and clarify the builtin surface (`spec/comptime.md`, `spec/types.md`, `spec/syntax.md`).

- Builtins conformance (non-type-construction):
  - Implement `builtin::cast` and define `x as T` as sugar for `builtin::cast(x, T)` (`spec/builtins.md`, `spec/types.md`).
  - Make `builtin::type_info` work for unsized types (full coverage) (`spec/builtins.md`).
  - Enforce `builtin::compile_error` as comptime-only (error if used outside a comptime context) (`spec/builtins.md`, `spec/comptime.md`).
- Parsing:
  - Add **type-level call syntax in type positions**: `TypeName(T0, T1, ...)` where each argument is a *type*.
    - This is the core mechanism for “generics without generics syntax”.
- `type` values:
  - Extend comptime execution with a `type` value representation (e.g. `ComptimeValue::Type(TypeId)`).
  - Allow `comptime` parameters of type `type` and allow comptime evaluation of functions returning `type`.
- Type lowering:
  - During type lowering, evaluate type-level calls at comptime and lower the returned `type` value to a `TypeId`.
  - Add canonicalization so repeated `Option(i32)` yields the same `TypeId` within a compilation.
- Examples/tests:
  - Add an example that calls a comptime type function which *selects* among existing types (no type construction yet), and uses the result in a `type` alias and in a value signature.
  - Add negative tests for type-level calls that do not return `type` or misuse `type` outside comptime contexts.

### v0.0.22 — `auto` type placeholder (parser + typing)
Goal: enable polymorphic functions without generics syntax (`spec/auto.md`).

- Parsing:
  - Reserve `auto` as a keyword and accept it in type positions.
- Type checking:
  - Implement `auto` in `let`/`const`/`static` annotations.
  - Implement `auto` in function parameter and return types.
  - Enforce v0.1 restrictions: reject `auto` in struct/enum fields, type aliases, cast targets, function types, and type-level call arguments.
  - Reject `auto` in `fn[extern(C)]` / `fn[export(C)]` signatures.
  - Reject taking function pointers to `auto`-polymorphic functions.
- Lowering/comptime:
  - Implement `auto`-polymorphic functions by implicitly passing type arguments and compiling via comptime interpretation + residualization (caching is an optimization).
- Examples/tests:
  - Add a small example with `fn id(x: auto) -> auto` and a negative suite covering the disallowed positions.

### v0.0.23 — Type construction builtins (MVP) + user-defined Option/Result
Goal: unlock user-space “generic” data structures as real Cog code (`spec/comptime.md`, `spec/stdlib.md`).

- Implement type-construction builtins from `spec/builtins.md`:
  - Nominal: `builtin::type_struct`, `builtin::type_enum` (with `repr` controls for structs and `tag_type` for fieldless enums).
  - Composite: `builtin::type_unit`, `builtin::type_never`, `builtin::type_ptr_const`, `builtin::type_ptr_mut`, `builtin::type_slice`, `builtin::type_array`, `builtin::type_tuple`, `builtin::type_fn`.
  - Canonicalization: identical descriptor inputs yield identical type identity within a compilation.
- Ensure the compiler pipeline supports constructed nominal types end-to-end:
  - name resolution for variant/field access and patterns
  - layout/ABI queries (`size_of`/`align_of`/`type_info`)
  - codegen for construction, matching, and field access
- Examples/tests:
  - Add an example that defines `Option`/`Result` as comptime type functions (not `core::` yet), uses `type OptI32 = Option(i32);`, and constructs/matches `OptI32::Some(...)`.
  - Add negative tests for malformed type construction (bad field/variant definitions; non-serializable type values; etc).

### v0.0.24 — Core semantics conformance: `loop` values, `Copy`, `comptime_int/float`
Goal: close the biggest remaining semantic gaps in `spec/expressions.md`, `spec/moves.md`, and `spec/types.md`.

- `loop` as an expression (Rust-like):
  - Implement typing for `break expr;` and `loop { ... }`:
    - unify all `break` values
    - if any `break` omits a value, the loop type is `()`
    - if there is syntactically no `break`, the loop type is `!`
  - Implement LLVM codegen for value-yielding loops (phi at loop exit).
  - Implement comptime evaluation for value-yielding loops.
- `Copy` rules (v0.1 structural Copy; not user-extensible):
  - Implement structural `Copy` for structs (all fields `Copy`) and fieldless `enum[tag(IntType)]`.
  - Ensure match scrutinee move behavior follows `Copy` (copy if `Copy`, move otherwise).
- `comptime_int` / `comptime_float` (v0.1 literal typing):
  - Make numeric literals default to comptime-only numeric types until coerced/cast.
  - Enforce fit checks on coercions/casts (and avoid host UB in comptime arithmetic).
  - Prefer `llvm::APInt` / `llvm::APFloat` (or equivalent) for literal storage and comptime math to remove the current `i64` limitations.
- Method-call rvalue handling:
  - Implement “materialize a temporary and take its address” for pointer-receiver method calls on non-place receivers (`spec/items.md`).
- Address-of `const` items:
  - Implement `&CONST` by materializing a private global (implementation-defined deduplication; no pointer identity guarantees) (`spec/items.md`).

### v0.0.25 — Tags, visibility, and C ABI conformance
Goal: match `spec/modules.md` visibility rules and `spec/layout_abi.md` interop rules.

- Visibility:
  - Enforce private-by-default visibility across modules (“visible to defining module + descendants”).
  - Enforce `pub` / `pub(crate)` across the crate.
  - Ensure `use` obeys visibility; improve diagnostics for visibility failures.
- Tag validation (fixed set; unknown tags are errors):
  - `mod`: accept `test` only (reserve syntax; test runner remains future work in `spec/future.md`).
  - `fn`: validate `inline`, `inline(always)`, `inline(never)`, `extern(C)`, `extern_name(...)`, `export(C)`, `export_name(...)` and their conflict rules.
  - `struct`: validate exactly one of `repr(cog)`/`repr(C)`/`repr(packed)`.
  - `enum`: validate `tag(IntType)` only on fieldless enums.
  - Reject tags on `impl`, `use`, `const`, `static`, and `type` items (per `spec/layout_abi.md`).
  - Optional: map inline tags to LLVM attributes (`alwaysinline`/`noinline`) as hints.
- Tests:
  - Add negative tests for unknown tags, duplicate/conflicting tags, and tags in disallowed positions.
- `repr(C)` structs:
  - Implement `repr(C)` field offsets/size/alignment using LLVM `DataLayout`/`TargetMachine` for the selected target.
  - Ensure `builtin::size_of`/`align_of` match this layout (no divergence between layout and codegen).
- `enum[tag(IntType)]` discriminants:
  - Evaluate and assign discriminants (`= expr` and implicit `+1`), as comptime integers.
  - Validate “fits in IntType” at compile time.
  - Codegen: use the assigned discriminant values (not just variant indices).
- C ABI boundary enforcement:
  - Reject non-FFI-safe types in `extern(C)` / `export(C)` signatures in v0.1 (structs/enums/tuples/arrays/slices/function pointers).
  - Ensure vararg call promotions match `spec/layout_abi.md` (including `f32 -> f64`).
- Examples/tests:
  - Add an end-to-end C interop example using `malloc/free/printf` and (optionally) a `enum[tag(i32)]`.
  - Add negative tests for invalid C ABI signatures and bad enum discriminants.

### v0.0.26 — Build mode plumbing (CLI + lowering hooks)
Goal: introduce build-mode selection without changing semantics yet.

- Add `--build-mode {debug,release_safe,release_fast}` and plumb through parsing/checking/comptime/codegen.
- Ensure the chosen build mode is available everywhere we need it (especially in the LLVM backend).
- Tests:
  - Add at least one compile-smoke test per build mode (same input program; just ensure compilation succeeds).

### v0.0.27 — Runtime traps (Debug/ReleaseSafe) + UB in ReleaseFast
Goal: implement the required safety checks from `spec/build_modes.md`.

- Traps in `Debug`/`ReleaseSafe`:
  - integer overflow for `+ - *` and narrowing integer casts
  - div-by-zero (and signed div overflow)
  - shift-range errors
  - bounds checks for array/slice indexing
  - null/misalignment traps for explicit deref (and deref implied by indexing)
  - enum tag validity where applicable (e.g. int↔enum casts)
- In `ReleaseFast`, these failures are UB; emit UB-friendly LLVM IR (no checks).
- Ensure comptime never “silently invokes UB”: the interpreter must report a compile error for these failures.
- Tests:
  - Add targeted negative tests for each trap class in safe modes (and at least one “compiles in ReleaseFast” case).

### v0.0.28 — Ship `core` + prelude + `?` operator
Goal: implement `spec/stdlib.md` once the compiler surface (CLI/build modes/emits) is stable.

- `core` module:
  - Decide and implement a “shipping mechanism” for `core` (bundled `.cg` file compiled implicitly, or a compiler-injected module).
  - Implement `core::Option` and `core::Result` in Cog using the type construction builtins from v0.0.23.
  - Add the required runtime symbol declarations (`memcpy`, `memmove`, `memset`, `memcmp`, `strlen`) as declarations only.
- Prelude:
  - Decide whether `core` items are auto-imported or require explicit `use core::{...};` in v0.1.
- `?` operator:
  - Parsing: add postfix `?` to expressions.
  - Type-checking: enforce the v0.1 restrictions from `spec/stdlib.md` (only `core::Option(T)` / `core::Result(T,E)`; must match function return type shape).
  - Lowering/codegen: desugar to early-return on `None`/`Err` (no generalized “Try”).
- Examples/tests:
  - Add an example that uses both `core::Option(...)` and `core::Result(..., ...)` and uses `?` in at least one helper function.
  - Add negative tests for `?` misuse (wrong return type, wrong operand type).

### v0.0.29 — Tech debt cleanup (stabilize for v0.1)
Goal: keep the compiler maintainable as we approach v0.1.

- Remove host-UB integer math from comptime evaluation (use checked/wrapping ops or APInt everywhere).
- Centralize build-mode configuration (avoid scattered conditionals).
- Add/refresh key comments in confusing subsystems (parser/type checker/comptime/codegen).
- Clean up any v0.0.x legacy scaffolding that no longer serves the v0.1 spec.

### v0.0.30 — v0.1.0 RC
Goal: reach “practical conformance” with the v0.1 spec and stabilize a release candidate.

- Align docs and implementation around the v0.1 subset; ensure examples cover the v0.1 surface.
- Expand the `ctest` suite with targeted positive/negative coverage of all v0.1-required features.

### First release target: v0.1.0
This should be the first “useful” release conforming to the `spec/`. In addition, we expect the compiler to mature into having:
    - no crashes on malformed programs; clear span-based errors
    - reasonable test coverage
    
## Future work
We have outlined some future changes and additions in `spec/future.md`, we will seek to build these in before 0.2.0
