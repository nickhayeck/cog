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
- `comptime_function_calls/`: comptime function calls + `builtin::type_info(type)`.
- `comptime_parameters/`: `comptime` parameters + residualization.
- `match_or_patterns/`: `match` with `|` pattern alternatives and guards.
- `match_exhaustiveness/`: `match` exhaustiveness (positive cases).
- `never_type/`: `!` (never) type unification through control flow.
- `pointers_and_deref/`: `&mut`, deref, and assignment through pointers.
- `size_align_builtins/`: `builtin::size_of`/`builtin::align_of` + type aliases.
- `tuple_structs/`: tuple structs + `.0`/`.1` field access.
- `tuples/`: tuple expressions + `.0`/`.1` indexing.
- `function_pointers/`: function pointers (`const* fn(...) -> R`) + indirect calls.
- `main_unit/`: entry point `fn main() -> ()`.
- `main_args_i32/`: entry point `fn main(argc: i32, argv: const* const* u8) -> i32`.
- `main_args_unit/`: entry point `fn main(argc: i32, argv: const* const* u8) -> ()`.
- `main_export_c/`: entry point `fn[export(C)] main(argc: i32, argv: const* const* u8) -> i32`.

`legacy/` contains older examples for removed features (traits/dyn).
