# Cog

Cog is an opinionated systems programming language with compile-time metaprogramming, reflection, and metatyping.

This repo contains an early C++ prototype compiler (`cogc`).

Status (v0.0.23): the prototype compiles most of the syntax through HIR → MIR → LLVM, and embeds MIR-interpreted comptime results into runtime constants. It also supports `type`-level programming (`fn ... -> type`) and an early C interop surface (`fn[extern(C)]`/`fn[export(C)]`, string literals, varargs).

- Core language spec (v0.1 draft): `spec/*.md`
- Roadmap: `roadmap.md`
- Examples: `examples/`

## Small examples

Full versions live in `examples/crc32_tool/` and `examples/user_option_result/`.

### CRC32 tool (comptime-generated data + C interop)

```cog
fn[extern(C)] printf(fmt: const* u8, ...) -> i32;

static CRC32_TABLE: [u32; 256] = comptime {
    let mut table: [u32; 256] = [0 as u32; 256];
    let mut i: usize = 0;
    while i < 256 {
        let mut crc: u32 = i as u32;
        let mut j: usize = 0;
        while j < 8 {
            if (crc & (1 as u32)) != (0 as u32) {
                crc = (crc >> (1 as u32)) ^ (0xEDB88320 as u32);
            } else {
                crc = crc >> (1 as u32);
            };
            j = j + 1;
        };
        table[i] = crc;
        i = i + 1;
    };
    table
};

fn calculate_crc32(s: const* u8) -> u32 {
    let mut crc: u32 = 0xFFFF_FFFF as u32;
    let mut i: usize = 0;
    while s[i] != (0 as u8) {
        let b: u32 = s[i] as u32;
        let idx: usize = ((crc ^ b) & (0xFF as u32)) as usize;
        crc = (crc >> (8 as u32)) ^ CRC32_TABLE[idx];
        i = i + 1;
    };
    ~crc
}
```

### User-defined `Option(T)` via `type` values (no generics syntax)

```cog
fn Option(comptime T: type) -> type {
    let none_payload: [type; 0] = [];
    let some_payload = [T];

    let variants = [
        builtin::EnumVariant {
            name: "None",
            payload: &none_payload,
            discriminant: builtin::MaybeComptimeInt::None,
        },
        builtin::EnumVariant {
            name: "Some",
            payload: &some_payload,
            discriminant: builtin::MaybeComptimeInt::None,
        },
    ];

    let desc = builtin::EnumDesc {
        name: "Option",
        variants: &variants,
        tag_type: builtin::MaybeType::None,
    };

    builtin::type_enum(desc)
}

fn unwrap_or(x: Option(i32), fallback: i32) -> i32 {
    match x {
        Option(i32)::Some(v) => v,
        _ => fallback,
    }
}
```

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
- `./build/cogc --emit-hir build/out.hir examples/basic_codegen/main.cg`
- `./build/cogc --emit-mir build/out.mir examples/basic_codegen/main.cg`
- `./build/cogc --emit-mir-after lower build/out.mir examples/basic_codegen/main.cg`

Format:
- `find . -name "*.cpp" -o -name "*.hpp" | xargs clang-format -i`
