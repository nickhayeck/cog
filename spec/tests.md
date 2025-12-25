---
title: Tests-in-Source (Planned)
version: 0.1.0-draft
---

# Tests-in-source (planned)

Cog intends to support a Rust-like “tests in source” workflow.

This feature is planned for after the core parsing/type-checking pipeline is stable, but the surface syntax is reserved now to avoid compatibility traps.

## `mod[test]`

A module tagged `test` is a test-only module:

```cog
mod[test] tests {
    fn add_works() -> () {
        if add(1, 2) != 3 { builtin::compile_error("add failed") };
    }
}
```

Semantics (intended):
- Test modules are not compiled in normal builds.
- Test modules are compiled when the compiler is invoked in “test mode” (e.g. `cogc --test`).

## Test function discovery

In test mode, the compiler discovers test functions by:
- enumerating all `fn` items inside `mod[test]` modules
- selecting those with signature `fn() -> ()`

The discovery rules may be refined later (e.g. allow submodules, naming conventions, or additional tags).

## Execution model (options)

One of these models should be chosen before stabilizing v0.1:

1) **Generated test runner (Rust-like)**
   - compiler generates a `main` that calls tests in a deterministic order
   - a failing test aborts the process

2) **Library + runtime runner**
   - compiler emits metadata; a runtime runner enumerates and executes

The v0.1 preference is (1) for simplicity and debuggability.
