# Comptime Design (Cog)

This document is a forward-looking design for Cog’s compile-time execution system (“comptime”). It is intentionally more detailed than the current implementation status and is meant to guide early compiler architecture to avoid dead ends.

Status notes:
- v0.0.6 implements a **small** subset (integer/bool const-eval, basic control flow, `builtin::compile_error`, wiring for `const`/`static` initializers and array lengths). See `roadmap.md`.
- Everything else in this document is **planned** and may evolve, but the *interfaces* and *staging model* should remain stable.

---

## 1. Why comptime exists

Cog wants:
- Rust-like surface syntax and item model.
- Zig-like compile-time execution and staged evaluation (partial evaluation).
- No borrow checker/lifetimes and no generics (initially), but still high performance and good ergonomics.

Comptime provides the missing piece: “generics without generics” (initially) by interpreting compile-time parameters and `type` values, then lowering a fully concrete residual program/IR with the comptime constructs erased.

---

## 2. Core model: two stages, one language

Cog code runs in one of two stages:
- **Runtime stage**: normal program execution.
- **Comptime stage**: the compiler runs (a restricted subset of) Cog code during compilation.

### 2.1 Comptime contexts
Expressions must be compile-time-evaluable (CTE) in these contexts:
- `const` initializers
- `static` initializers (subject to restrictions)
- array lengths `[T; N]`
- `comptime { ... }` blocks (in any context)
- `comptime` parameters (staged evaluation inputs)

### 2.2 “CTE subset” philosophy
The compiler should:
- Accept a broad surface syntax.
- Precisely define *which operations* are legal at comptime.
- Produce concrete diagnostics for illegal operations (“not comptime evaluable: …”).

Over time, the CTE subset expands; the staging boundary remains.

---

## 3. `type` as a first-class comptime value

`type` is a comptime-only kind (a type-of-types). Values of kind `type` can be:
- passed as parameters
- returned from functions
- stored in `const`s
- used to compute other types

### 3.1 Examples: type-as-value

#### 3.1.1 Generic function via `type` parameter
```cog
fn add(comptime T: type, a: T, b: T) -> T {
    a + b
}

fn demo() -> i32 {
    // `T` is known at comptime; lowering produces concrete operations for `i32`.
    add(i32, 40, 2)
}
```

#### 3.1.2 Type-returning function (type constructor)
```cog
fn Option(comptime T: type) -> type {
    enum OptionT {
        None,
        Some(T),
    }
    OptionT
}

type OptI32 = comptime { Option(i32) };
```

Notes:
- This implies type expressions exist at comptime. The current prototype’s `type` aliases are syntactic (`type X = SomeType;`) and will be extended to allow comptime type computation.

---

## 4. Staged evaluation and residual IR

Cog’s comptime model is not “template specialization” in the C++ sense. The semantics are:
- The compiler interprets comptime-evaluable code (including control flow driven by comptime values).
- The output of compilation is a *residual* lowered program/IR that contains only concrete runtime operations.
- Comptime-only values/branches are erased; they do not exist at runtime.

### 4.1 What triggers staged evaluation
Staged evaluation occurs whenever a compile-time-only value is required:
- `const`/`static` initializers
- array lengths `[T; N]`
- `comptime { ... }` blocks
- calls with `comptime` parameters

### 4.2 Example: comptime parameter drives lowering
```cog
fn repeat(comptime N: usize, x: i32) -> [i32; N] {
    // Conceptually, lowering sees `N` as a compile-time constant and emits IR
    // where the array length is concrete (e.g. `N=4`).
    builtin::compile_error("array construction not implemented yet");
}

fn demo() -> i32 {
    // The call forces `N` to be known at compile time.
    // After staged evaluation, the program contains only concrete ops for N=4.
    0
}
```

### 4.3 Memoization (compiler optimization)
To avoid repeating work, the compiler may memoize the result of staged evaluation by:
- fully qualified item identity + canonicalized comptime arguments

Pitfall to avoid: using raw AST pointers as cache keys leads to unstable caching and redundant work across modules/files.

### 4.4 Limits
To keep compilation predictable:
- step/instruction budget for comptime execution
- recursion depth limit for comptime calls
- diagnostics that print the call/evaluation stack on overflow

---

## 5. Comptime execution environment (“the interpreter”)

### 5.1 Determinism and purity
Comptime must be deterministic:
- no I/O
- no wall clock
- no threads
- no system calls
- no randomness (unless explicitly modeled and deterministic)

### 5.2 Memory model
At comptime, we need a model for:
- local values
- temporary allocations
- aggregate construction (struct/enum/tuple/array)

Two practical options:
1. **Pure value interpreter** for most things + a small “comptime heap” for addressable objects (needed for `addr_of`, slices, and eventually C interop).
2. A more complete VM with memory + pointers (heavier).

Recommendation:
- Start value-only (v0.0.6 style).
- Introduce a comptime heap once reflection/slices/more complex builtins need stable addresses.

### 5.3 Control flow
The interpreter should support:
- blocks, `let`, assignment
- `if/else`
- `while` / `loop` + `break` / `continue`
- `match` + guards
- `return` (inside comptime functions)

Loops must be bounded by a step budget to prevent hangs.

---

## 6. Builtins (eventual set)

The builtin namespace is spelled `builtin::...` and is always in scope (it is not a user module).

### 6.1 Layout and reflection
```cog
builtin::size_of(comptime T: type) -> usize
builtin::align_of(comptime T: type) -> usize
builtin::type_info(comptime T: type) -> TypeInfo
```

### 6.2 Compile-time errors
```cog
builtin::compile_error(msg: const* [u8]) -> !
```

### 6.3 Address builtins
```cog
builtin::addr_of(x: T) -> const* T
builtin::addr_of_mut(x: mut T) -> mut* T
```

Notes:
- `addr_of`/`addr_of_mut` require a comptime memory model with stable addresses.
- `compile_error` should be allowed only in comptime contexts (or should force evaluation).

---

## 7. Reflection: `TypeInfo`

Reflection is a comptime facility; it returns structured data describing types.

Sketch:
```cog
pub enum TypeTag {
    Unit, Bool, Int,
    Ptr, Slice, Array, Tuple,
    Struct, Enum, TraitObject,
}

pub struct FieldInfo {
    name: const* [u8],
    ty: type,
    offset: usize,        // requires layout computation
}

pub struct StructInfo {
    name: const* [u8],
    fields: const* [FieldInfo],
    repr: ReprInfo,
}

pub struct TypeInfo {
    tag: TypeTag,
    size: usize,
    align: usize,
    // tagged payload, e.g. StructInfo, EnumInfo, ...
}
```

Constraints:
- Since `repr(C)` is the default in Cog, `offset`/layout results should be stable and deterministic given a target ABI.
- Reflection should operate on *fully resolved, concrete* types (no “unknown generics” in early versions).

---

## 8. Type-level programming patterns (examples)

### 8.1 `Result` and `Option` in “std prelude”
```cog
fn Option(comptime T: type) -> type {
    enum OptionT { None, Some(T) }
    OptionT
}

fn Result(comptime T: type, comptime E: type) -> type {
    enum ResultT { Ok(T), Err(E) }
    ResultT
}

type OptU8 = comptime { Option(u8) };
type ResI32 = comptime { Result(i32, i32) };
```

### 8.2 Compile-time array lengths and table generation
```cog
const N: usize = 256;

const TABLE: [u8; N] = comptime {
    // Future: array literal + indexed assignment.
    // For now, conceptually:
    // var t: [u8; N] = ...
    // var i: usize = 0;
    // while i < N { t[i] = (i as u8); i = i + 1; }
    // t
    builtin::compile_error("array literals not implemented yet");
};
```

### 8.3 Conditional compilation via comptime
```cog
const FAST: bool = comptime {
    // Future: read build mode / target features.
    true
};

fn f(x: i32) -> i32 {
    if FAST { x * 2 } else { x + 1 }
}
```

---

## 9. Interaction with traits and `dyn`

Comptime is not limited to runtime values; it will be used to:
- generate concrete implementations (vtables, reflection-driven codegen)
- generate vtables/registries
- validate ABI/layout constraints

Potential future pattern:
```cog
fn assert_object_safe(comptime Tr: type) -> () {
    // builtin::type_info(Tr) and check method signatures.
}
```

---

## 10. Major pitfalls and guardrails

### 10.1 Cycles between typing and comptime
Array lengths and type aliases can cause cycles:
- type checking needs constant values
- constant evaluation needs types (layout, field access)

Guardrail:
- separate phases with explicit dependency edges
- cycle detection with diagnostic “dependency loop: …”

### 10.2 “Type as value” canonicalization
If `type` values are compared structurally (rather than by canonical identity), you can accidentally make caching and equality O(n²) and unstable.

Guardrail:
- intern all types (TypeStore) and use stable `TypeId` identity as the canonical representation in comptime values.

### 10.3 Comptime resource usage
Comptime must never be able to hang the compiler indefinitely.

Guardrail:
- step budget and recursion depth limits
- diagnostic showing the last few executed nodes/spans

### 10.4 Object safety and dyn dispatch
If methods mention `Self` outside the receiver position, calling them on `dyn Trait` becomes ill-defined without generics/associated types.

Guardrail:
- object-safety checks for `dyn` usage and dyn calls

### 10.5 “CTE subset” drift
If you allow too much at comptime too early (I/O, raw pointers, etc.), you will lock in a huge interpreter surface.

Guardrail:
- grow from value-only → add memory model only when needed
- keep builtins small and explicit
