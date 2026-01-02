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
- `crc32_tool/`: a small “real program” using `printf` + comptime-generated data (CRC32 table).
- `type_level_calls/`: `type` values + type-level calls in type positions.
- `auto_placeholder/`: `auto` placeholder for inference + call-site polymorphism.
- `user_option_result/`: user-defined `Option`/`Result` via comptime type construction (`builtin::type_enum`).
- `perfect_hash/`: minimal perfect hashing computed at comptime + a generic `PhfMap(K, V) -> type`.
- `match_or_patterns/`: `match` with `|` pattern alternatives and guards.
- `match_exhaustiveness/`: `match` exhaustiveness (positive cases).
- `never_type/`: `!` (never) type unification through control flow.
- `numerics_arrays_floats/`: array literals + repeat, bitwise/shifts, and float literals/ops.
- `pointers_and_deref/`: `&mut`, deref, and assignment through pointers.
- `size_align_builtins/`: `builtin::size_of`/`builtin::align_of` + type aliases.
- `tuple_structs/`: tuple structs + `.0`/`.1` field access.
- `tuples/`: tuple expressions + `.0`/`.1` indexing.
- `function_pointers/`: function pointers (`const* fn(...) -> R`) + indirect calls.
- `main_unit/`: entry point `fn main() -> ()`.
- `main_export_c/`: entry point `fn[export(C)] main(argc: i32, argv: const* const* u8) -> i32`.
