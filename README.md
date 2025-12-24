# Cog

Cog is a Rust-syntax systems language with Zig-style compile-time execution, targeting low-latency software.

This repo contains an early C++ prototype compiler (`cogc`).

- Spec: `SPEC.md`
- Roadmap: `roadmap.md`
- Grammar (approx): `grammar.md`
- Comptime design notes: `comptime_design.md`
- Examples: `examples/`

## Status (v0.0.12)
- Front-end: parse → modules/`use` → type check + local move check → comptime const-eval for `const`/`static` and array lengths.
- LLVM backend (early): emits runnable code for ints/bools, `if/else`, `while`/`loop` + `break/continue`, `match` on ints/bools/enums, structs, methods, and `dyn Trait` calls.
- C interop surface (early): keyword tags on items, `fn[extern]` declarations, `fn[export(C)]` definitions, and extern-only `...` varargs.

## Build and run
Prereqs: CMake, a C++23 compiler, Flex/Bison, LLVM (C++ libraries), and `clang` (currently used as the linker driver for `--emit-exe`).

- Configure: `cmake -S . -B build`
- Build: `cmake --build build -j`
- Smoke tests: `ctest --test-dir build --output-on-failure`

LLVM notes:
- On macOS with Homebrew LLVM, CMake should auto-detect `/opt/homebrew/opt/llvm`. If it doesn’t, set `LLVM_DIR=/opt/homebrew/opt/llvm/lib/cmake/llvm`.

Run (check-only):
- `./build/cogc examples/v0_0_12/main.cg`

Run end-to-end (LLVM + link):
- `./build/cogc --emit-exe build/out examples/v0_0_12/main.cg && ./build/out`

Other emits:
- `./build/cogc --emit-llvm build/out.ll examples/v0_0_12/main.cg`
- `./build/cogc --emit-bc build/out.bc examples/v0_0_12/main.cg`
- `./build/cogc --emit-obj build/out.o examples/v0_0_12/main.cg`
