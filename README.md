# Cog

Cog is a Rust-syntax systems language with Zig-style compile-time execution, targeting low-latency software.

This repo contains an early C++ prototype compiler (`cogc`).

- Core language spec (v0.1 draft): `spec/README.md`
- Roadmap: `roadmap.md`
- Grammar: `spec/syntax.md` (core) and `grammar.md` (prototype parser)
- Comptime design notes: `comptime_design.md`
- Examples: `examples/`

## Status (v0.0.18-main)
- Front-end: parse → modules/`use` → type check + local move check → comptime const-eval for `const`/`static` and array lengths.
- Comptime: function calls (with resource limits), `comptime` parameters with residualization, and `builtin::type_info(type)`.
- Core typing: `!` (never) type + match exhaustiveness (bool/enums; `_` required for int matches), tuple structs + `.0/.1`, and function pointers (`const* fn(...) -> R`).
- LLVM backend (early): adds tuple expressions/indexing and indirect calls through function pointers.
- Executables: implements `main` entrypoint selection per `spec/layout_abi.md`.
- C interop surface (early): keyword tags on items, `fn[extern(C)]` declarations, `fn[export(C)]` definitions, `extern_name(...)`/`export_name(...)`, and extern-only `...` varargs.
- String literals:
  - `"..."` is `const* [u8]` (fat pointer `{ptr,len}`)
  - `c"..."` is `const* u8` (NUL-terminated)

## Build and run
Prereqs: CMake, a C++23 compiler, Flex/Bison, LLVM (C++ libraries), and `clang` (currently used as the linker driver for `--emit-exe`).

- Configure: `cmake -S . -B build`
- Build: `cmake --build build -j`
- Smoke tests: `ctest --test-dir build --output-on-failure`

LLVM notes:
- On macOS with Homebrew LLVM, CMake should auto-detect `/opt/homebrew/opt/llvm`. If it doesn’t, set `LLVM_DIR=/opt/homebrew/opt/llvm/lib/cmake/llvm`.

Run (check-only):
- `./build/cogc examples/ffi_tags_strings/main.cg`

Run end-to-end (LLVM + link):
- `./build/cogc --emit-exe build/out examples/hello_world/main.cg && ./build/out`

Other emits:
- `./build/cogc --emit-llvm build/out.ll examples/ffi_tags_strings/main.cg`
- `./build/cogc --emit-bc build/out.bc examples/ffi_tags_strings/main.cg`
- `./build/cogc --emit-obj build/out.o examples/ffi_tags_strings/main.cg`

Format:
- `find . -name "*.cpp" -o -name "*.hpp" | xargs -I {} /opt/homebrew/opt/llvm/bin/clang-format -i {}`