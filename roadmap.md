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

### v0.0.7 — HIR + layout engine (front-end → lowering boundary)
Goal: establish a stable “lowering boundary” so codegen and comptime can share a single notion of types/layout.

- Introduce a lowered representation (HIR) with:
  - resolved references (e.g. `ModuleId`/`Item*` or stable `ItemId`s) instead of raw `Path*`
  - desugared constructs where useful (e.g. `if` as expression with explicit blocks)
  - explicit “place vs value” nodes to support loads/stores cleanly
- Implement a target-dependent layout engine:
  - sizes/alignments for primitives
  - `repr(C)` struct layout + field offsets
  - enum layout strategy (tag + payload; start simple and deterministic)
  - layout queries usable by both comptime (`size_of`, `align_of`) and codegen
- Define canonical runtime representations (even before LLVM exists):
  - pointers (`const* T` / `mut* T`)
  - slices as fat pointers (`{ data_ptr, len }`)
  - `dyn Trait` as fat pointers (`{ data_ptr, vtable_ptr }`)

### v0.0.8 — IR (MIR) + lowering (HIR → IR)
Goal: convert expression-oriented syntax into explicit control flow suitable for optimization and codegen.

- Build a small explicit IR with:
  - basic blocks + terminators (`br`, `cond_br`, `switch`/`match`, `return`)
  - locals, temporaries, and “places” (`local`, `field`, `index`, `deref`)
  - rvalues (`const`, `load`, `binop`, `call`, `addr_of`, `cast`)
- Lower core constructs:
  - `let`, assignment, field/index access
  - `if/else`, `while`, `loop`, `break`/`continue`
  - `match` lowering to `switch`/decision tree (start with ints + enums)
- Define and enforce expression typing rules that IR needs:
  - `loop` as an expression (type from `break expr`) or unit if no value (decide and document)
  - consistent “never”/divergence handling in the IR

### v0.0.9 — LLVM backend + executable driver (first runnable programs)
Goal: compile and run real programs end-to-end.

- Emit LLVM IR for:
  - functions, calls, returns
  - arithmetic/comparisons for ints/bools
  - control flow blocks + PHI where needed
  - struct field access via GEP using the layout engine offsets
  - enum construction + pattern-match lowering (start minimal)
- Add an output pipeline:
  - emit `.ll` / `.o`
  - link an executable using the system toolchain (e.g. `clang`/`lld`), without requiring network access
- Define the “language ABI” for v0.1:
  - calling convention for Cog functions
  - `repr(C)` interop baseline (struct layout + integer widths)

### v0.0.10 — Trait objects lowering (vtables + dyn coercions)
Goal: make `dyn Trait` usable without “magic casts”.

- Generate vtables for each `(Trait, ConcreteType)` impl:
  - method function pointers ordered by the trait method set
  - (optional) type metadata (size/align/drop fn) later
- Implement coercions:
  - `mut* T` → `mut* dyn Trait` (and const variants) when `T: Trait`
  - store `{ data_ptr, vtable_ptr }` in the dyn fat pointer
- Lower dyn method calls into vtable loads + indirect calls.

### v0.0.11 — Comptime functions + staged evaluation (comptime parameters)
Goal: unlock parametric programming via comptime.

- Allow comptime function calls in the interpreter (bounded recursion).
- Support `comptime` parameters as compile-time inputs to compilation:
  - a call with comptime arguments is interpreted/partially evaluated until only runtime operations remain
  - lowering produces concrete IR (no comptime parameters exist at runtime)
  - memoize results by canonicalized comptime values (compiler optimization, not a language feature)
- Add initial builtins backed by the layout engine:
  - `builtin::size_of(type)`
  - `builtin::align_of(type)`
  - `builtin::type_info(type)`
  - `builtin::type_of(type)`

## Release target: v0.1.0
v0.1.0 should be the first “useful” release: you can compile and run small programs, and the surface subset is stable enough to build examples against.

**v0.1.0 exit criteria (must-have)**
- End-to-end pipeline: `cogc` compiles and links an executable for a small subset.
- Stable subset support:
  - modules (`mod` inline/out-of-line) + `use` trees
  - structs/enums + `match` (at least ints + enums)
  - functions + calls + methods via `impl`
  - traits + static dispatch (in-scope traits)
  - `dyn Trait` objects with working vtables + dyn calls
  - `const`/`static` + array lengths with deterministic comptime evaluation
- A coherent ABI story:
  - `repr(C)` default is enforced by a real layout engine
  - pointer/slice/dyn object representations are specified and implemented
- Diagnostics and stability:
  - no crashes on malformed programs; clear span-based errors
  - basic test coverage (smoke tests + a few negative tests)

**v0.1.0 nice-to-have**
- Privacy/visibility enforcement (`pub` across modules).
- Better `match` checking (exhaustiveness + unreachable arms) for small domains.
- A tiny “prelude” module (core integer ops + a couple of builtins).

## C interop track (ongoing)
- Parse `use_interop "header.h";` as a placeholder item and plumb it through name resolution.
- Decide mapping of C declarations into Cog items (types, functions, constants).
- Add symbol/link control attributes (e.g. `#[link_name="..."]`, `#[link(lib="...")]`).
