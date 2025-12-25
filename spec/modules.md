---
title: Modules, Name Resolution, and Visibility
version: 0.1.0-draft
---

# Modules, name resolution, and visibility

## Crates and source files

- A **crate** is a compilation unit (one `cogc` invocation).
- Source files use the `.cg` extension.
- The crate has a **root module** (the “crate module”) corresponding to the input file.

## Module items

Modules can be defined in two ways:

1) Inline:

```cog
mod math {
    fn add(a: i32, b: i32) -> i32 { a + b }
}
```

2) Out-of-line:

```cog
mod math;
```

File mapping for `mod name;` declared in directory `D`:
- First try `D/name.cg`
- Then try `D/name/mod.cg`

This matches the Rust mental model and keeps directory structure predictable.

## `use` items

`use` binds names; it does not “import code” (the build system decides what files/crates participate in the build).

Supported forms:

```cog
use a::b::c;
use a::b::c as d;
use a::b::{c, d as e, f};
```

Resolution:
- A `use` tree is resolved relative to the current module.
- A `use` introduces one or more **aliases** into the module scope.

## Namespaces

Cog uses separate namespaces, Rust-style:

- **Module namespace**: modules
- **Type namespace**: `struct`, `enum`, `type` aliases
- **Value namespace**: `fn`, `const`, `static`, enum variants

This enables patterns like:

```cog
struct Foo { x: i32 }
fn Foo() -> Foo { Foo { x: 0 } } // type Foo and value Foo can coexist
```

## Visibility

Each item has a visibility:

- **Private** (default): visible within the defining module and its descendant modules.
- `pub`: visible from any module (within the crate).
- `pub(crate)`: visible from any module in the crate.

Notes:
- In v0.1, `pub` and `pub(crate)` are equivalent within a single-crate build; `pub(crate)` exists for forward compatibility with multi-crate builds.
- Visibility is orthogonal to `fn[export(C)]` / `fn[extern(C)]` (see `spec/layout_abi.md`):
  - A private item may still be exported for C interop.
  - An exported item may still be inaccessible from other Cog modules if not `pub`.

## Name resolution phases (Rust-like intent)

An implementation should follow a two-phase model:

1) **Item collection**
   - Parse all modules/files reachable from the crate root.
   - Collect item declarations into module scopes (names + kinds + vis + tags).

2) **Path resolution**
   - Resolve `use` items to concrete targets.
   - Resolve all paths in types/expressions/patterns against the module tree and namespaces.

Circular module dependencies are permitted: `mod` items describe a tree, and `use` resolution operates over already-collected item sets.

## `Self`

`Self` is a reserved keyword and is only valid as a path segment within an `impl` block:

```cog
struct Vec2 { x: f32, y: f32 }

impl Vec2 {
    fn len(self: const* Self) -> f32 { ... }
}
```

Within an `impl T { ... }`, `Self` resolves to `T`.
