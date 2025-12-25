# Examples

These examples are intended to compile with the current prototype compiler.

- `hello_world/`: `printf` + `c"..."` C strings.
- `ffi_tags_strings/`: `fn[extern(C)]`/`fn[export(C)]`, `*_name(...)`, `enum[tag(..)]`, and both string literal forms.
- `ffi_minimal/`: minimal C-interop surface + extern-only `...` varargs.
- `modules_use/`: `mod` loading + `use` trees.
- `basic_codegen/`: basic LLVM codegen smoke test.
- `inherent_methods/`: `impl` methods + method calls.
- `control_flow/`: `while`/`loop`, `break`/`continue`, and `match` on enums (including payload variants).
- `comptime_consts/`: comptime const/static initializers + array lengths.
- `match_or_patterns/`: `match` with `|` pattern alternatives and guards.
- `pointers_and_deref/`: `&mut`, deref, and assignment through pointers.
- `size_align_builtins/`: `builtin::size_of`/`builtin::align_of` + type aliases.

`legacy/` contains older examples for removed features (traits/dyn).
