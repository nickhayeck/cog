---
title: Cog Language Specification (Draft)
version: 0.0.11-draft
edition: 2025
status: living
---

# Cog Language (draft spec)

Cog is a Rust-syntax, low-latency systems language with Zig-style compile-time execution. Early editions target a C++ compiler implementation; bootstrapping is a long-term goal.

This spec is intentionally split into:
- **Surface syntax** (what parses)
- **Static semantics** (name resolution, type checking, move checking)
- **Runtime/ABI** (layout, calling convention boundaries)
- **Comptime** (compile-time evaluation and codegen)

## 1. Goals and non-goals

### 1.1 Goals
- Familiar **Rust-2021-like** surface syntax: `mod`, `use`, `pub`, items, ADTs, `match`, `impl`, traits.
- **Low-latency** friendly: predictable performance, explicit control, C interop.
- **No borrow checker** (by design): pointers are always “just pointers”.
- **Move-by-default** with **use-after-move rejection** for locals via dataflow (like Rust, but without lifetime analysis).
- **Traits** and **`dyn` trait objects** for dynamic dispatch.
- **Zig-style comptime**: evaluate code at compile time; erase comptime constructs by interpreting them and emitting only concrete residual operations; treat `type` as a first-class value (future parametric programming).

### 1.2 Non-goals (for early versions)
- No lifetimes / regions.
- No generics (parametric polymorphism) as a surface feature yet (future designs use `type` values + comptime evaluation to produce fully concrete IR).
- No strict-aliasing / `noalias` assumptions: optimizer must assume pointers can alias.
- No safety guarantees for pointer dereference; “unsafe” is required only for specific casts/conversions, not for deref itself.

## 2. Lexical structure

### 2.1 Tokens
- Identifiers: `[_a-zA-Z][_a-zA-Z0-9]*` (Unicode identifier support is out of scope for v0.0.x)
- Integer literals: decimal-only (v0.0.x); hex/binary/octal later
- String literals: `"..."`
- Char literals: `'a'` (later)
- Comments:
  - Line: `// ...`
  - Block: `/* ... */`

### 2.2 Keywords (reserved)
`fn struct enum trait impl type const static mod use pub as let if else while loop match return break continue comptime dyn Self mut use_interop true false`

## 3. Modules, items, and visibility

### 3.1 Module system
- A crate is a compilation unit.
- Source files use the `.cg` extension.
- `mod name { ... }` defines an inline module.
- `mod name;` declares an out-of-line module (file-based resolution):
  - For a module declared in directory `D`, the compiler searches `D/name.cg` then `D/name/mod.cg`.
- `use path;` imports an item into scope (details in name resolution).

### 3.2 Visibility
- `pub` makes an item visible outside its defining module.
- Additional visibilities (e.g. `pub(crate)`) are deferred.

### 3.3 Attributes
- `#[...]` attaches metadata to the following item.
- For v0.0.x, attributes are parsed and carried through the AST but semantics can be ignored unless explicitly implemented.

## 4. Types

Cog has (at least) two “kinds”:
- **Value types**: ordinary runtime values (`i32`, structs, pointers, etc.)
- **Type kind**: `type` values that exist only at compile time and describe a value type.

### 4.1 Primitive value types
- Integers: `i8..i128`, `u8..u128`, `isize`, `usize`
- Floats: `f32`, `f64`
- `bool`, `char` (UTF-32 scalar value; details later)
- Unit: `()`

### 4.2 Compound value types
- Tuples: `(T0, T1, ...)` and unit `()`
- Arrays: `[T; N]` where `N` is a comptime `usize`
- Slices: `[T]` is an unsized slice type; it appears behind pointers
- Structs and enums: nominal ADTs

### 4.3 Pointers
Cog pointer types are explicit and do not carry lifetime/uniqueness information:
- `const* T`: pointer to `T` (read-only through this pointer)
- `mut* T`: pointer to `T` (mutable through this pointer)

Properties:
- May be null, dangling, aliasing, and may freely escape scope.
- `mut*` **does not imply uniqueness**; optimizer must assume aliasing.

Coercions:
- `mut* T` → `const* T` is implicit.
- “Scoped pointers” may exist later; conversions between scoped and raw are restricted (raw → scoped only via `unsafe`).

### 4.4 Trait objects
- `const* dyn Trait`, `mut* dyn Trait` are trait-object fat pointers (layout is ABI-defined):
  - data pointer
  - vtable pointer
- Calling a method on `dyn Trait` uses vtable dispatch.

Constraints (initial):
- No generics, no associated types.
- Methods may take `self` explicitly as `self: const* Self` or `self: mut* Self`.

### 4.5 Layout and `repr`
Supported attributes (syntax-level in v0.0.x, semantics incrementally):
- Default representation is `repr(C)` unless overridden by an explicit `#[repr(...)]`.
- `#[repr(C)]`: C-compatible layout and field ordering
- `#[repr(packed)]`: packed layout (alignment reduced)
- More `repr(...)` options may be added.

## 5. Expressions, statements, and control flow

Cog is expression-oriented with Rust-like blocks:
- Blocks: `{ stmt* expr? }`
- `let` bindings: `let mut? pat ( : Type )? (= expr)?;`
- Control flow: `if/else`, `while`, `loop`, `break`, `continue`, `return`
- Pattern matching: `match expr { pat (if guard)? => expr_or_block, ... }`
- Calls, method calls, field access, indexing, `as` casts

## 6. Patterns

Supported (v0.x roadmap):
- Wildcard `_`
- Literals
- Path patterns: `Enum::Variant(...)`, `Name { field: pat, ... }`
- Tuple patterns: `(p0, p1, ...)`
- Alternatives: `p0 | p1 | p2`
- Guards: `pat if expr`

## 7. Move/Copy semantics (no borrow checking)

### 7.1 Move-by-default
- Assigning or passing a value transfers ownership (moves) unless the type implements `Copy`.
- After a move, the local is considered invalid; using it is a compile-time error (“use after move”).

### 7.2 `Copy` and `Clone`
- `Copy`: bitwise copy, implicit.
- `Clone`: explicit duplication via method call (details later).

### 7.3 What is checked
Initial checker scope:
- Local variables, temporaries, and fields tracked in a conservative dataflow.
- No lifetime tracking; pointers do not constrain moves.

## 8. Traits, `impl`, and method resolution

### 8.1 `impl` blocks
- Inherent impl: `impl Type { fn m(...) { ... } }`
- Trait impl: `impl Trait for Type { fn m(...) { ... } }`

### 8.2 Method lookup
Given a call `receiver.method(args...)`:
1. If `receiver` is of nominal type `T`, search inherent impls of `T`.
2. If not found, search in-scope traits with an impl for `T`.
3. If `receiver` is a `* (dyn Trait)`, resolve through the vtable.

No generics/overloading: name + arity + receiver kind is sufficient for early versions.

## 9. Comptime

Cog supports Zig-style compile-time evaluation.

### 9.1 `comptime { ... }` blocks
`comptime { ... }` is evaluated at compile time and yields a compile-time constant value.

Uses:
- Initializers for `const` and `static` (subject to restrictions)
- Array lengths `[T; N]` where `N` is comptime `usize`
- Type aliases and, later, type-level computation

Note: `const`/`static` initializers are inherently comptime contexts, so `comptime { ... }` is usually redundant there; it’s primarily useful to force compile-time evaluation inside non-comptime contexts (e.g. inside function bodies) once lowering/codegen is implemented.

### 9.2 Comptime functions
- `comptime fn` (or later: `fn ...` callable in comptime contexts) can be executed by the comptime interpreter.
- The interpreter is deterministic and side-effect constrained (I/O, syscalls, threads are disallowed in comptime).

### 9.3 Comptime parameters (staged evaluation)
Functions may declare comptime parameters:
```cog
fn add(comptime T: type, a: T, b: T) -> T { a + b }
fn repeat(comptime N: usize, x: i32) -> [i32; N] { ... }
```
Comptime parameters are compile-time inputs to compilation: the compiler interprets comptime-dependent parts and lowers a fully concrete program/IR where the comptime parameters no longer exist at runtime.

Implementation note: the compiler may memoize the resulting lowered form for a given set of comptime arguments to avoid repeating work, but this is not a language-level “template specialization” feature.

### 9.4 `type` as a value
`type` is a comptime-only kind. Values of kind `type` can be passed, returned, and computed at comptime.

Examples:
```cog
fn Option(comptime T: type) -> type { ... }
```

### 9.5 Builtins (initial set)
Namespace and exact spelling are TBD; the prototype uses `builtin::...`:
- `builtin::size_of(comptime T: type) -> usize`
- `builtin::align_of(comptime T: type) -> usize`
- `builtin::type_info(comptime T: type) -> TypeInfo`
- `builtin::compile_error(msg: const* [u8]) -> !` (never returns)
- `builtin::addr_of(x) -> const* T`
- `builtin::addr_of_mut(x) -> mut* T`

## 10. C interop

Cog’s long-term goal is “no-FFI” interop: C declarations are imported directly and used as if they were Cog declarations.

Cog intentionally does **not** have `extern { ... }` blocks; the intent is to make header import the primary workflow.

### 10.1 ABI and layout
- `#[repr(C)]` types are intended to match C struct layout rules for the target.
- Integers use two’s complement; endianness is target-defined.

### 10.2 Header import (future)
We plan a “no-FFI” workflow where importing C headers is direct:
- `use_interop "path/to/header.h";` (placeholder syntax)

## 11. Build modes

Build modes follow Zig’s intent:
- `Debug`: full runtime checks, minimal optimization
- `ReleaseSafe`: optimized with runtime safety checks where feasible (bounds checks, overflow checks, etc.)
- `ReleaseFast`: maximum optimization, minimal checks

Exact checks are specified per operation (later).

## 12. Compiler roadmap

The prototype compiler roadmap lives in `roadmap.md`. Below is the current implementation status.

### v0.0.11 prototype compiler status
- Front-end: parse → module/load + `use` resolution → type-check + local move-check → comptime const-eval (for `const`/`static` and array lengths).
- Parser:
  - Flex/Bison lexer+parser (no generics) producing a typed AST with source spans.
  - CLI flags: `cogc --dump-tokens <file.cg>`, `cogc --dump-ast <file.cg>`.
- Modules + name resolution:
  - Inline `mod name { ... }` and out-of-line `mod name;` with file mapping (`name.cg` or `name/mod.cg`).
  - `use path;`, `use path as alias;`, and `use path::{a, b as c};`.
  - Separate namespaces for modules/types/values; concrete diagnostics for missing items.
  - Method indexing for inherent impls and trait impls.
- Type + move checking (conservative, v0.0.x scope):
  - Nominal structs/enums, field access, struct literals, enum variants, and calls.
  - Methods: inherent methods first, then in-scope traits with an impl for the receiver; dyn calls on `* dyn Trait` use the trait method set (object-safety is enforced).
  - `match` typing with arm unification + guard type checking (`if guard` is `bool`).
  - Pointer rules: implicit `mut* T` → `const* T`; `dyn Trait` and `[T]` must appear behind pointers.
  - Local move checking rejects use-after-move; `Copy` modeled for primitives/pointers and aggregates of `Copy`.
- Layout (v0.0.7+):
  - Target-dependent layout engine for primitives, pointers (incl fat pointers), tuples, arrays, structs, and enums.
  - Default is `repr(C)`; `#[repr(packed)]` supported for structs.
- Comptime (minimal, but useful):
  - Interpreter for integer/bool const-eval and basic control flow (`if`/`while`/`loop`/`match`) in comptime contexts.
  - `builtin::compile_error("...")` triggers an error when executed at comptime.
  - `builtin::size_of(T)` / `builtin::align_of(T)` are supported via the layout engine (type-value argument must be a path).
- LLVM backend (early, v0.0.9+):
  - `cogc --emit-llvm <out.ll> <file.cg>` and `cogc --emit-exe <out> <file.cg>` (links with system `clang`).
  - Emits a runnable subset: ints/bools, blocks + `if/else`, `while`/`loop` + `break`/`continue`, `match` on ints/bools/enums, `let` + assignment, struct literals + field access, pointer deref, direct calls, methods, `builtin::addr_of(_mut)`, and dyn trait objects (`* dyn Trait`) via vtables.
- Not yet implemented (runtime/codegen and staged evaluation):
  - Arrays/slices/string literals as runtime data (beyond type-checking and comptime).
  - Comptime function calls and staged evaluation via comptime parameters.
  - Reflection (`builtin::type_info`, `type` values as first-class).
  - Exhaustiveness checking for `match`.
