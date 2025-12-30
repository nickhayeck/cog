## perfect_hash

This example demonstrates a **minimal perfect hash** table whose **seed + lookup
tables are computed at comptime**.

What it shows:
- Building a minimal perfect hash for a fixed keyset using `comptime { ... }`.
- Using the resulting tables at runtime for O(1) lookup.
- Key “kinds” shown:
  - `i32` keys
  - `f64` keys (via a stable integer fingerprint)
  - C-string keys (`const* u8`) using a stable polynomial hash mod `P`

Important current limitation (compiler, not the concept):
- Cog’s MIR comptime interpreter does **not** support dereferencing pointers yet,
  so we can’t hash string bytes at comptime. For the string-keyed map we
  therefore **precompute the string fingerprints** as integer constants, and
  compute the same hash at runtime for the query string.
- Function pointer values are not supported in comptime evaluation yet, so the
  string-keyed map stores a small opcode and dispatches via `match`.

Once comptime pointer reads exist, the string fingerprints can be computed
directly from the string literals in the comptime block.
