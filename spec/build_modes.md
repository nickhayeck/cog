---
title: Build Modes, Traps, and Undefined Behavior
version: 0.1.0-draft
---

# Build modes, traps, and UB (Zig-like)

Cog defines three build modes:

- `Debug`: maximum runtime safety checking; traps abort the program.
- `ReleaseSafe`: safety checking enabled; optimizations enabled; traps abort the program.
- `ReleaseFast`: safety checks may be removed; many safety failures are **undefined behavior** (UB).

Cog may add `ReleaseSmall` in the future.

## Panics and traps

Cog has no exceptions. “Panic” behavior (including safety-check failures) **always aborts** the program.

An implementation may format diagnostics before aborting in `Debug` and `ReleaseSafe`.

## Safety checks required in Debug/ReleaseSafe

In `Debug` and `ReleaseSafe`, the implementation **must** trap (abort) on the following classes of errors:

- **Integer overflow** for checked arithmetic (see below).
- **Division by zero**.
- **Shift out of range**: shifting by a value not in `[0, bitwidth)`.
- **Array/slice bounds** errors.
- **Invalid enum tag** (when converting from integers or otherwise constructing an enum from a raw tag).
- **Null pointer dereference** when the implementation can check (at minimum, for explicit `*p` dereference).
- **Misaligned pointer dereference** (if the target type requires alignment > 1).

## ReleaseFast UB boundary

In `ReleaseFast`, the above errors are **undefined behavior** unless explicitly stated otherwise.

“Undefined behavior” means the program is not required to do anything sensible; the compiler may assume the UB does not occur for optimization purposes.

## Integer overflow semantics

### Integer types

Integer types include:
- fixed-width signed: `i8`, `i16`, `i32`, `i64`, `i128`
- fixed-width unsigned: `u8`, `u16`, `u32`, `u64`, `u128`
- pointer-sized: `isize`, `usize`

### Checked vs wrapping operations

For v0.1:
- The ordinary arithmetic operators `+ - *` on integer types are **checked**:
  - `Debug`/`ReleaseSafe`: overflow traps.
  - `ReleaseFast`: overflow is UB.

Wrapping and saturating arithmetic are planned as explicit operations (likely in `core`), out of scope for early v0.1 implementations.

## Division and remainder

- Division by zero traps in `Debug`/`ReleaseSafe`, UB in `ReleaseFast`.
- Signed division overflow (e.g. `i32::MIN / -1`) follows the same rule: trap in safe modes, UB in `ReleaseFast`.

## Shifts

For `x << y` and `x >> y`:
- If `y` is negative or `y >= bitwidth(x)`: trap in `Debug`/`ReleaseSafe`, UB in `ReleaseFast`.

## Bounds checks

For indexing:
- `arr[i]` and `slice[i]` trap in `Debug`/`ReleaseSafe` when out of bounds, UB in `ReleaseFast`.
- Raw pointer indexing `ptr[i]` does not perform a bounds check (there is no length); it follows the pointer dereference rules below (null/misalignment traps in safe modes, UB in `ReleaseFast`).

## Pointer dereference checks

For explicit dereference `*p`:
- If `p` is null: trap in `Debug`/`ReleaseSafe`, UB in `ReleaseFast`.
- If `p` is misaligned for `T`: trap in `Debug`/`ReleaseSafe`, UB in `ReleaseFast`.

Note: Cog does not and will not have a general “dangling pointer” check; dereferencing a dangling pointer is UB in all modes.

## Evaluation order and side effects

The **order of evaluation** of subexpressions is **unspecified** (see `spec/expressions.md`).

Cog does not define a general “sequenced-before” relation like Rust. Instead:
- If multiple evaluation orders are possible, the program may behave as if *any* one of them was chosen.
- You must not rely on a particular order unless the spec explicitly guarantees one.
- Differences due solely to evaluation order are **unspecified behavior**, not UB.

## Data races (threads)

Threading is out of scope for early v0.1 implementations. A future spec will define a memory model; until then, data races should be treated as UB.
