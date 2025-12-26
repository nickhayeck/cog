# Cog compiler roadmap

Status: **v0.0.18-main is implemented** (comptime calls/params + core typing additions + executable entrypoint handling). The v0.1 core language spec lives in `spec/README.md`.

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
  - `Copy` modeled for primitives/pointers and aggregates of `Copy`

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
- Current limitations (planned extensions):
  - comptime function calls and comptime method calls are not supported
  - comptime indexing/array literals are not supported yet

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
- Implement `fn[extern]` imported declarations (no body) + extern-only `...` C varargs.
- Implement `fn[export(C)]` exported definitions (body required; symbol name is unmangled).
- Enforce an integer-tagged enum representation only on fieldless enums (prototype syntax uses `repr(i32)`; core spec uses `tag(i32)`).
- LLVM backend: avoid mangling for extern/export functions; emit vararg function types and apply basic C vararg promotions at call sites.
- String literals:
  - Decode basic escapes in the lexer (`\\n`, `\\r`, `\\t`, `\\\"`, `\\\\`).
  - Lower to NUL-terminated global constants and use `const* u8` as the prototype type for C interop.
  - Example: `examples/hello_world/main.cg`.

### v0.0.13 — Spec-surface alignment (tags, enums, visibility, never type, test modules) (done)
- Accept `fn[extern(C)]` / `fn[export(C)]` plus `extern_name(...)` / `export_name(...)` with conflict validation.
- Accept `enum[tag(<int>)]` for fieldless enums; parse discriminants and validate them at comptime.
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

## Next milestones

### Mini milestone: v0.0.18-main — bring the main function into specification (done)
Goal: allow the full range of `main` functions specified by `spec/layout_abi.md`.

- Emit an entrypoint shim only when building an executable (`--emit-exe`).
- Implement and validate entrypoint selection:
  - accept explicit C ABI `fn[export(C)] main(argc: i32, argv: const* const* u8) -> i32`
  - otherwise synthesize a C ABI wrapper for Cog ABI `main` (`fn main() -> ()` or `fn main() -> i32`)
- Emit clear diagnostics when `main` is missing or has an invalid signature.

### v0.0.19 — Core literals + operators + indexing
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

### v0.0.20 — Type expressions + minimal shipped `core` + `?`
Goal: unlock `core::Option(T)` / `core::Result(T, E)` and the `?` operator (`spec/stdlib.md`).

- Extend the type checker to support **type-level calls** in type positions (e.g. `core::Option(i32)`), evaluated at comptime.
- Ship a minimal `core` module with:
  - `core::Option` and `core::Result` as compiler-provided comptime type functions (no general user-defined type construction yet)
  - enough support to type-check and pattern-match the resulting enums
- Implement `?` for `core::Option(T)` and `core::Result(T, E)` only.

### v0.0.21 — Visibility enforcement + module polish
Goal: match `spec/modules.md` visibility rules and make modules ergonomic.

- Enforce `pub` / `pub(crate)` visibility across modules.
- Confirm “child modules can see private ancestor items” behavior (and add diagnostics when visibility fails).
- Tighten `use` diagnostics (fully-qualified suggestions, “did you mean” for missing items).

### v0.0.22 — FFI correctness: C ABI boundaries + repr(C) layout
Goal: make extern/export “actually correct” (per `spec/layout_abi.md`).

- Enforce FFI-safe types for `extern(C)`/`export(C)` signatures (v0.1: primitives + raw pointers only).
- Implement `struct[repr(C)]` layout using LLVM `DataLayout`/`TargetMachine` so size/align/offsets match the target C ABI.
- Implement `enum[tag(IntType)]` as “represented exactly as `IntType`” for fieldless enums (no hidden tag/payload).
- Improve symbol-name control:
  - default extern/export names are unmangled
  - `*_name(...)` overrides names
- Add an end-to-end C interop example using `malloc/free/printf` with `c"..."`.

### v0.0.23 — Build modes (Debug/ReleaseSafe/ReleaseFast) + runtime traps
Goal: begin matching `spec/build_modes.md` behavior.

- Add `--build-mode {debug,release_safe,release_fast}` (default: debug for now).
- Implement trap sites in codegen for:
  - integer overflow (checked ops)
  - div-by-zero, shift-range
  - bounds checks for indexing
  - null/misalignment traps for explicit deref
- In `ReleaseFast`, compile these to UB-friendly IR (no checks).

### v0.0.24 — v0.1.0 release candidate (docs, examples, harness)
Goal: freeze the v0.1 subset and make it pleasant to use.

- Ensure `spec/` and the compiler agree on the v0.1 subset (tag syntax, strings, enums, FFI limits).
- Expand `examples/` to cover the v0.1 core features and C interop. Get rid of per-version examples and instead have examples named by the feature/concept they demonstrate.
- Make `ctest` run a stable smoke suite (`--emit-exe` + run) plus a small negative suite.
- Polish diagnostics (module paths, “did you mean”, span rendering) enough for day-to-day use.

## Release target: v0.1.0
v0.1.0 should be the first “useful” release: you can compile and run small programs, and the surface subset is stable enough to build examples against.

**v0.1.0 exit criteria (must-have)**
- End-to-end pipeline: `cogc` compiles and links an executable for a stable subset.
- Docs: a stable core spec under `spec/` + a practical `README.md`.
- Stable subset support:
  - modules (`mod` inline/out-of-line) + `use` trees
  - visibility: private-by-default, `pub`, `pub(crate)`
  - literals and operators: arrays, ints/floats, bitwise ops, indexing
  - structs/enums + `match` (at least ints + enums)
  - functions + calls + methods via `impl`
  - `fn[extern(C)]` / `fn[export(C)]` + extern-only `...` varargs
  - `const`/`static` + array lengths with deterministic comptime evaluation
  - string literals: `"..."` (`const* [u8]`) and `c"..."` (`const* u8`)
  - function pointers: `const* fn(...) -> R`
  - `!` type and diverging expressions
  - `core::Option(T)` / `core::Result(T, E)` + `?`
- ABI story:
  - default `repr(cog)` is implemented (layout is implementation-defined)
  - `struct[repr(C)]` structs for C interop are supported
  - `enum[tag(IntType)]` (fieldless enums) for C-like enums
  - pointer/slice representations are specified and implemented
- Diagnostics and stability:
  - no crashes on malformed programs; clear span-based errors
  - basic test coverage (smoke tests + a few negative tests)

**v0.1.0 nice-to-have**
- Better `match` checking (exhaustiveness + unreachable arms) for small domains.
- A richer prelude/core surface (more integer ops and helpers).

**Open design questions (non-gating)**
- C enums: representation + casts (`enum[tag(i32)]` and friends).
- Unions: do we want them, and how do they interact with `match`/pattern syntax?
- `repr(cog)` policy boundaries: what is guaranteed vs compiler-defined?
- Meta-typing + builtins for type-parametric programming (reflection and type construction).
- Allocation API direction.
- Variadic arguments beyond extern-only C varargs.
- Testing story (built-in test runner vs library/framework).

## Planned for v0.2.0 (spec only today)
- Control-flow sugar: `if let`, `while let`.
- Range expressions: `a..b`, `a..=b`.
- Struct literal shorthand: `Point { x, y }`.
- Enum struct variants: `E::V { x: 1 }` + matching patterns.

## C interop track (ongoing; subject to revision)
- Define surface syntax for linking control (e.g. link names, calling conventions, libraries).
- Define `repr(C)` coverage (structs first; fieldless enums via `tag(<int>)`).
- Add symbol/link control tags (e.g. `link_name(...)`, `link_lib(...)`).
