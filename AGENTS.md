# Agent notes (cog_lang)

These are project-local instructions for working in this repo.

## Scope and intent
- Keep changes small and incremental; aim for a working end-to-end prototype at each version bump.
- Prefer correctness and debuggability over cleverness.
- Avoid “magic” build steps; everything should run via CMake.

## Current compiler pipeline (v0.0.11)
- `cogc <file.cg>` runs: parse → module loading + `use` resolution → type checking + local move checking → comptime const-eval (for `const`/`static` and array lengths).
- Optional backends:
  - `cogc --emit-llvm <out.ll> <file.cg>` emits LLVM IR.
  - `cogc --emit-bc <out.bc> <file.cg>` emits LLVM bitcode.
  - `cogc --emit-obj <out.o> <file.cg>` emits an object file.
  - `cogc --emit-exe <out> <file.cg>` emits an object file and links via system `clang`.
- Debug aids:
  - `cogc --dump-tokens <file.cg>`
  - `cogc --dump-ast <file.cg>` (after successful checking)

## Code style (C++)
- Language: C++23.
- Prefer simple `struct` data types, `std::unique_ptr`, and clear ownership.
- Avoid exceptions for normal control flow; return `std::optional`/`std::expected` style where helpful.
- Names:
  - Types: `PascalCase`
  - Functions/vars: `snake_case`
  - Constants: `kPascalCase` or `k_snake_case` (follow local patterns once established)

## Layout
- Source: `src/`
- Tests: `ctest` currently runs `examples/*` as smoke tests; `test/` is reserved for a future harness.
- Examples: `examples/`
- Generated flex/bison outputs should go into the CMake build directory, not checked into git.

## Building
- Configure: `cmake -S . -B build`
- If CMake can’t find LLVM: set `LLVM_DIR=/opt/homebrew/opt/llvm/lib/cmake/llvm`
- Build: `cmake --build build -j`
- Run (check-only): `./build/cogc examples/v0_0_11/main.cg`
- Run (end-to-end): `./build/cogc --emit-exe build/out examples/v0_0_11/main.cg && ./build/out`
- Test: `ctest --test-dir build --output-on-failure`

## Design notes
- Long-form comptime plan: `comptime_design.md`
- Language spec: `SPEC.md`

## Flex/Bison constraints
- The macOS system `bison` may be old; keep the grammar compatible with GNU Bison 2.3.
- Keep parsing and semantics split: parsing should build AST only; resolution/type checking should live in separate passes.
- Error messages should include file + line + column where possible.
