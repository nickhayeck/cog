# Cog compiler roadmap

Status: **v0.0.12 is implemented** (front-end + layout + LLVM codegen + dyn trait objects + initial C interop surface). See `examples/v0_0_12/main.cg` and `comptime_design.md`.

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
- Note: v0.0.10 implements LLVM lowering for dyn calls (vtables + indirect dispatch).

### v0.0.6 — Comptime interpreter (minimal, useful) (done)
- Implement a minimal comptime interpreter for:
  - integer/bool literals, arithmetic/comparisons, `if`, `while`, `loop`, `match` + guards
  - struct/enum construction, tuple values, field access
  - `builtin::compile_error` (emits an error when executed at comptime)
- Wire comptime into:
  - `const` / `static` initializers (all are evaluated in v0.0.6)
  - array lengths `[T; N]` (length expressions are evaluated as `usize`)
- Current limitations (planned extensions):
  - comptime function calls and comptime method/dyn calls are not supported
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
- Emits: ints/bools, blocks + tail expr, `if/else`, `let` + assignment, struct literals + field access, pointer deref, direct calls, method calls, and `builtin::addr_of(_mut)`.

### v0.0.10 — Trait objects lowering (vtables + dyn coercions) (done)
- `* dyn Trait` lowered to `{ data_ptr, vtable_ptr }` with typed vtable structs per trait.
- Vtables generated for each `(Trait, ConcreteType)` impl with wrapper thunks (`i8* self` erasure).
- Dyn coercion implemented as explicit cast: `mut* T as mut* dyn Trait` (and const variants).
- Dyn method calls lowered to vtable loads + indirect calls.

### v0.0.11 — Enums + match/loops (LLVM) (done)
- LLVM codegen for `while`/`loop` + `break`/`continue`.
- LLVM codegen for `match` on ints/bools/enums (ordered decision chain) with basic bindings and guards.
- LLVM representation for enums (tag + payload) and runtime constructors (including unit variants as values).

### v0.0.12 — FFI surface (tags + extern/export + varargs) + string literals (done)
- Migrate fully from `#[...]` to keyword tags after item keywords (`struct[...]`, `enum[...]`, `fn[...]`).
- Implement `fn[extern]` imported declarations (no body) + extern-only `...` C varargs.
- Implement `fn[export(C)]` exported definitions (body required; symbol name is unmangled).
- Enforce `enum[repr(<int>)]` only on fieldless enums.
- LLVM backend: avoid mangling for extern/export functions; emit vararg function types and apply basic C vararg promotions at call sites.
- String literals:
  - Decode basic escapes in the lexer (`\\n`, `\\r`, `\\t`, `\\\"`, `\\\\`).
  - Lower to NUL-terminated global constants and use `const* u8` as the prototype type for C interop.
  - Example: `examples/hello_world/main.cg`.

## Next milestones

### v0.0.13 — Comptime functions + staged evaluation (comptime parameters)
Goal: unlock parametric programming via comptime.

- Allow comptime function calls in the interpreter (bounded recursion).
- Support `comptime` parameters as compile-time inputs to compilation:
  - a call with comptime arguments is interpreted/partially evaluated until only runtime operations remain
  - lowering produces concrete IR (no comptime parameters exist at runtime)
  - memoize results by canonicalized comptime values (compiler optimization, not a language feature)
- Add `builtin::type_info(type)` builtin backed by the layout engine.

## Release target: v0.1.0
v0.1.0 should be the first “useful” release: you can compile and run small programs, and the surface subset is stable enough to build examples against.

**v0.1.0 exit criteria (must-have)**
- End-to-end pipeline: `cogc` compiles and links an executable for a stable subset.
- Docs: a stable `SPEC.md` for the subset + a practical `README.md`.
- Stable subset support:
  - modules (`mod` inline/out-of-line) + `use` trees
  - structs/enums + `match` (at least ints + enums)
  - functions + calls + methods via `impl`
  - traits + static dispatch (in-scope traits)
  - `dyn Trait` objects with working vtables + dyn calls
  - `const`/`static` + array lengths with deterministic comptime evaluation
- ABI story:
  - default `repr(cog)` is implemented (layout is implementation-defined)
  - `struct[repr(C)]` structs for C interop are supported
  - pointer/slice/dyn object representations are specified and implemented
- Diagnostics and stability:
  - no crashes on malformed programs; clear span-based errors
  - basic test coverage (smoke tests + a few negative tests)

**v0.1.0 nice-to-have**
- Privacy/visibility enforcement (`pub` across modules).
- Better `match` checking (exhaustiveness + unreachable arms) for small domains.
- A tiny “prelude” module (core integer ops + a couple of builtins).

**Open design questions (non-gating)**
- C enums: representation + casts (`enum[repr(i32)]` and friends).
- Unions: do we want them, and how do they interact with `match`/pattern syntax?
- `repr(cog)` policy boundaries: what is guaranteed vs compiler-defined?
- Traits: role in the long-term design (esp. without “Rust-style” safety features).
- Meta-typing + builtins for type-parametric programming (reflection and type construction).
- Allocation API direction.
- Variadic arguments beyond extern-only C varargs.
- Testing story (built-in test runner vs library/framework).

## C interop track (ongoing; subject to revision)
- Define surface syntax for linking control (e.g. link names, calling conventions, libraries).
- Define `repr(C)` coverage (structs first; fieldless enums via `repr(<int>)`).
- Add symbol/link control tags (e.g. `link_name(...)`, `link_lib(...)`).
