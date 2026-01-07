---
title: Patterns and Match Exhaustiveness
version: 0.1.0-draft
---

# Patterns and match exhaustiveness

Patterns are used in:
- `let` bindings
- `match` arms

Pattern matching is structural; patterns do not evaluate arbitrary expressions (except match guards).

## Pattern forms

### Wildcard

`_` matches any value and binds nothing.

### Literals

Integer and boolean literals match values equal to the literal.

### Bindings

`x` binds the matched value to a new local named `x`.

`mut x` binds a mutable local (the binding may be assigned after initialization).

Bindings introduce new locals whose move/copy behavior follows `spec/moves.md`.

### Tuple patterns

`(p0, p1, ...)` matches tuple values by matching each element positionally.

### Struct patterns

`TypeName { field0: p0, field1: p1, ... }` matches a struct value and matches each named field.

In v0.1, struct patterns must mention all fields (there is no `..` rest pattern).

Tuple structs use the call-like syntax: `TypeName(p0, p1, ...)`.

### Enum patterns

Fieldless enum variants can be matched as paths:

```cog
match c {
    Color::Red => ...
    _ => ...
}
```

Payload enum variants use call-like syntax:

```cog
match opt {
    OptionI32::None => ...
    OptionI32::Some(x) => ...
}
```

If the enum type is a type expression (for example a type-level call), use the
type-qualified form:

```cog
match opt {
    <Option(i32)>::Some(x) => ...
    _ => ...
}
```

### Alternatives

`p0 | p1 | ...` matches if any alternative matches.

All alternatives must bind the same set of names with the same types; otherwise it is a compile-time error.

### Guards

An arm may include a guard:

```cog
pat if cond => expr
```

Rules:
- The pattern is matched first.
- If it matches, bindings are introduced.
- The guard expression is evaluated with those bindings in scope.
- If the guard is false, the arm is treated as non-matching and matching continues with the next arm.

The guard expression must have type `bool`.

## Exhaustiveness

`match` must be exhaustive. A match is exhaustive if, for every possible scrutinee value, at least one arm matches.

### `_` escape hatch

The wildcard pattern `_` matches any value and can always be used to make a match exhaustive.

### Exhaustiveness rules by type (v0.1)

The compiler must at minimum implement these exhaustiveness rules:

- `bool`: exhaustive if arms cover both `true` and `false`, or include `_`.
- Fieldless `enum[tag(IntType)]`: exhaustive if arms cover all variants, or include `_`.
- Payload enums: exhaustive if arms cover all variants (with any subpatterns), or include `_`.

For integer types, exhaustive checking over all values is not required in v0.1; a `match` on an integer type should be required to include `_`.

### Unreachable arms (lint)

If an earlier arm matches all values matched by a later arm, the later arm is unreachable.

Producing a warning for unreachable arms is recommended but not required in v0.1.
