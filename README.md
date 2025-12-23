# Cog

Cog is a Rust-syntax systems language with Zig-style compile-time execution, targeting low-latency software.

This repo contains an early C++ prototype compiler (`cogc`).

- Spec: `SPEC.md`
- Roadmap: `roadmap.md`
- Grammar (approx): `grammar.md`
- Comptime design notes: `comptime_design.md`
- Examples: `examples/`

## Status (v0.0.11)
- Front-end: parse → modules/`use` → type check + local move check → comptime const-eval for `const`/`static` and array lengths.
- LLVM backend (early): emits runnable code for ints/bools, `if/else`, `while`/`loop` + `break/continue`, `match` on ints/bools/enums, structs, methods, and `dyn Trait` calls.

## Build and run
Prereqs: CMake, a C++23 compiler, Flex/Bison, and `clang` (for `--emit-exe`).

- Configure: `cmake -S . -B build`
- Build: `cmake --build build -j`
- Smoke tests: `ctest --test-dir build --output-on-failure`

Run (check-only):
- `./build/cogc examples/v0_0_11/main.cg`

Run end-to-end (LLVM + link):
- `./build/cogc --emit-exe build/out examples/v0_0_11/main.cg && ./build/out`

