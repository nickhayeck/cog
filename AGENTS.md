# Agent notes (cog_lang)

These are project-local instructions for working in this repo.

## Scope and intent
- Keep changes small and incremental; aim for a working end-to-end prototype at each version bump.
- Prefer correctness and debuggability over cleverness.
- Avoid “magic” build steps; everything should run via CMake.

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
- Tests: `test/` (only add if we create a test runner)
- Examples: `examples/`
- Generated flex/bison outputs should go into the CMake build directory, not checked into git.

## Building
- Configure: `cmake -S . -B build`
- Build: `cmake --build build -j`
- Run: `./build/cogc examples/v0_0_1.cg`

## Flex/Bison constraints
- The macOS system `bison` may be old; keep the grammar compatible with GNU Bison 2.3.
- Keep the lexer and parser “syntax-only” in early versions: parse into an AST and defer semantics.
- Error messages should include at least line numbers (column support can come later).
