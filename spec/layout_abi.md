---
title: Layout, ABI, and Interop
version: 0.1.0-draft
---

# Layout, ABI, and interop

This chapter specifies:
- layout and representation (`repr(...)`)
- the slice ABI
- the FFI boundary surface (`extern(C)` / `export(C)` and varargs)

## Item tags (fixed set)

Tags are written after the item keyword:

```cog
struct[repr(C)] Foo { ... }
fn[extern(C)] puts(s: const* u8) -> i32;
fn[export(C)] main(argc: i32, argv: const* const* u8) -> i32 { ... }
```

Unknown tags are errors. Duplicate tags are errors.

### `mod` tags

Allowed tags on `mod` items:

- `test`

`mod[test] name { ... }` marks a test-only module (see `spec/tests.md`).

### `fn` tags

Allowed tags on `fn` items:

- `inline`
- `inline(always)`
- `inline(never)`
- `extern(C)`
- `extern_name(name)`
- `export(C)`
- `export_name(name)`

`name` is either a string literal or an identifier (single-segment). If an identifier is used, its source spelling is the symbol name.

Conflict rules:
- Exactly one of `inline`, `inline(always)`, `inline(never)` may appear.
- `export(C)` may not appear with any other `fn` tag except `export_name(...)`.
  - `export_name(...)` requires `export(C)`.
- `extern(C)` may not appear with any other `fn` tag except `extern_name(...)`.
  - `extern_name(...)` requires `extern(C)`.

Semantics:
- `fn[extern(C)] name(...);` declares an imported function:
  - it must have no body (`;`)
  - it uses the C ABI
  - its symbol name is either `name` (unmangled) or overridden by `extern_name(...)`
- `fn[export(C)] name(...) { ... }` defines an exported function:
  - it must have a body
  - it uses the C ABI
  - its symbol name is either `name` (unmangled) or overridden by `export_name(...)`
- `inline` tags are hints/requests to the optimizer; they do not change semantics.

Export is orthogonal to visibility:
- `pub` controls *Cog* module visibility.
- `export(C)` controls *linker* symbol visibility.

### `struct` tags

Allowed tags on `struct` items:

- `repr(cog)` (default)
- `repr(C)`
- `repr(packed)`

Exactly one may appear.

### `enum` tags

Allowed tags on `enum` items:

- `tag(IntType)` (fieldless enums only)

Constraints:
- `tag(IntType)` is only permitted for fieldless enums.
- `IntType` must be an integer type.
- Every variant discriminant value must fit in `IntType` (compile-time error otherwise).

### Tags on other items

In v0.1, `use`, `const`, `static`, and `type` items do not accept tags. Supplying tags on these items is an error.

## `repr` and data layout

### `repr(cog)` (default)

`repr(cog)` is Cog’s default representation.

Guarantees in v0.1:
- Layout is fully **implementation-defined**.
- The compiler may reorder fields, choose alignments, and choose enum representations to optimize size/performance.
- The layout used by a compilation must be self-consistent: all code in the crate must agree on offsets/size/alignment for a given type.

Non-guarantees:
- Source field order is not guaranteed to match memory order.
- Layout is not guaranteed stable across compiler versions or build modes.

### `repr(C)` for structs

`repr(C)` specifies a C-compatible struct layout.

Rules:
- Field order is source order.
- Alignment and padding follow the target C ABI and C layout rules.
- The overall struct alignment is the maximum alignment of its fields (subject to target ABI).

`repr(C)` is intended for FFI and stable binary interfaces.

### `repr(packed)` for structs

`repr(packed)` packs fields with minimal padding.

Rules:
- Field order is source order.
- The struct alignment is 1.
- Fields are placed at the next byte offset (no implicit padding).

Safety:
- Reading a packed field through an aligned load may be invalid.
- In `Debug`/`ReleaseSafe`, dereferencing a misaligned pointer traps (see `spec/build_modes.md`).
- Portable access should use explicit unaligned-load helpers (planned for `core`).

## Enums

### Fieldless enums with `enum[tag(IntType)]`

Representation:
- The enum is represented exactly as `IntType`.

Discriminant assignment:
- If a variant has no explicit `= expr`, it is assigned the previous value + 1, starting at 0.
- The discriminant expression is a comptime integer.

C interop:
- This is the preferred representation for C-like enums.

### Payload enums

Payload enums are part of the core language, but `repr(cog)` layout is implementation-defined in v0.1.

An implementation must still provide a coherent semantic model for:
- construction (`Enum::Variant(payload...)`)
- pattern matching and field extraction
- equality comparison when defined (likely structural for small v0.1 subsets)

## Slice ABI (committed)

`const* [T]` and `mut* [T]` are fat pointers:

```
{ ptr: const* T, len: usize }
{ ptr: mut*   T, len: usize }
```

The ordering is `{ ptr, len }`.

This ABI is committed for v0.1 to enable stable interop patterns.

## Strings ABI

- `"..."` yields `const* [u8]` (a slice fat pointer).
- `c"..."` yields `const* u8` (a thin pointer to NUL-terminated bytes).

String literal storage is in static read-only memory; literals may be deduplicated and pointer identity is not guaranteed.

## Calling conventions

### Cog ABI

The default calling convention for non-`extern`/`export` functions is the “Cog ABI”.

In v0.1:
- Cog ABI is an implementation detail and is not specified beyond “self-consistent within a compilation”.
- Function pointer types `const* fn(...) -> R` use Cog ABI.

### C ABI boundary

`fn[extern(C)]` and `fn[export(C)]` use the target C ABI.

#### FFI-safe types for v0.1

For v0.1, `extern(C)` / `export(C)` signatures are restricted to:
- integers
- floats
- `bool`
- raw pointers (`const* T`, `mut* T`)

Other types (structs, enums, slices, arrays, tuples, function pointers) are not FFI-safe in v0.1 and should be rejected at the C ABI boundary.

#### Varargs (`...`)

Varargs are permitted only for `fn[extern(C)]` declarations:

```cog
fn[extern(C)] printf(fmt: const* u8, ...) -> i32;
```

Calling a vararg function must apply C default promotions:
- integer promotions (e.g. `u8`/`i8`/`u16`/`i16` promoted to `i32`/`u32` as appropriate)
- `f32` promoted to `f64`

## Symbol naming

- Non-`extern`/`export` functions may be mangled; mangling is not specified in v0.1.
- `extern(C)`/`export(C)` default symbol names are **unmangled** (the item identifier).
- `extern_name(...)` / `export_name(...)` override the symbol name.

## Executable entry point (`main`)

For executables, the implementation should:

1) Prefer an explicit C ABI entry point:

```cog
fn[export(C)] main(argc: i32, argv: const* const* u8) -> i32 { ... }
```

2) Otherwise, look for a Cog ABI and synthesize a C ABI shim. The Cog ABI main should be either:
    a) `fn main() -> () {...}`, 
    b) `fn main() -> i32 {...}`,

If no suitable `main` exists, or if multiple suitable `main` functions exist, compilation fails.

Future versions will add in core or std support for acquiring argument values from the Cog ABI entrypoints.
