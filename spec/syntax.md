---
title: Syntax (Grammar)
version: 0.1.0-draft
---

# Syntax (core grammar)

This is a deliberately small, Rust-shaped grammar for v0.1. It is not intended to be a full Rust grammar and does not attempt to be whitespace-sensitive.

Notation:
- `?` optional
- `*` repetition (zero or more)
- `+` repetition (one or more)
- Literal tokens are in quotes.

## Program

```
program := item*
```

## Visibility

```
vis := "pub"
     | "pub" "(" "crate" ")"
     | ε
```

Notes:
- The default visibility is module-private (see `spec/modules.md`).
- `pub(crate)` is the only scoped visibility in v0.1.

## Tags

Tags attach metadata to items. Tags are **items-only**.

```
tags      := "[" tag_list? "]"
tag_list  := tag ("," tag)* ","?
tag       := IDENT
          | IDENT "(" tag_arg ")"
tag_arg   := path | STRING
```

Notes:
- The allowed tag sets and their semantics are specified in `spec/layout_abi.md`.
- The reserved `mod[test]` syntax and intended test runner model is described in `spec/future.md`.
- Unknown tags are errors.

## Items

```
item := vis? item_core

item_core :=
    "use" use_tree ";"
  | "mod" tags? IDENT "{" item* "}"
  | "mod" tags? IDENT ";"
  | "struct" tags? IDENT struct_body
  | "enum" tags? IDENT enum_body
  | "impl" type_path "{" impl_item* "}"
  | "fn" tags? IDENT "(" params? ")" ret? (block | ";")
  | "const" IDENT ":" type "=" expr ";"
  | "static" IDENT ":" type "=" expr ";"
  | "type" IDENT "=" type ";"

impl_item := vis? "fn" tags? IDENT "(" params? ")" ret? block
ret       := "->" type
```

Notes:
- `inline` tags may be used on `impl` methods.
- `extern(C)` / `export(C)` tags are module-scope only (see `spec/layout_abi.md`).

### `use` trees

```
use_tree  := path use_alias?
          | path "::{" use_trees? "}"
use_alias := "as" IDENT
use_trees := use_tree ("," use_tree)* ","?
```

### Structs

```
struct_body :=
    "{" field_decl* "}"
  | "(" tuple_fields? ")" ";"?   // tuple struct

field_decl   := vis? IDENT ":" type ","?
tuple_fields := tuple_field ("," tuple_field)* ","?
tuple_field  := vis? type
```

Notes:
- Tuple structs are part of the core language surface, but may be implemented after other struct features.
- `auto` is not permitted in struct field types; see `spec/auto.md`.

### Enums

```
enum_body     := "{" variant_decl* "}"
variant_decl  := IDENT variant_payload? variant_discriminant? ","?
variant_payload := "(" types? ")"
variant_discriminant := "=" expr
```

Notes:
- Fieldless enums are enums where every variant has no payload (no `(...)`).
- `auto` is not permitted in enum payload types; see `spec/auto.md`.

### Parameters

```
params := param ("," param)* ("," "...")? ","?
param  := IDENT ":" type
       | "comptime" IDENT ":" type
```

Notes:
- `...` (varargs) is only permitted on `fn[extern(C)]` declarations.

## Paths

```
path      := path_seg ("::" path_seg)*
path_seg  := IDENT | "Self"
type_path := path
```

## Types

```
type :=
    type_call
  | type_path
  | "auto"                 // type placeholder (see `spec/auto.md`)
  | "type"                 // the type-of-types (comptime-only)
  | "!"                    // never type
  | pointer_type
  | slice_type
  | array_type
  | tuple_type
  | fn_type

// Type-level call (type functions).
// This is how v0.1 expresses “generics without generics syntax”.
type_call := type_path "(" type_list? ")"

Notes:
- `auto` must not appear as a type-call argument (see `spec/auto.md`).

pointer_type := ("const" | "mut") "*" type
slice_type   := "[" type "]"             // unsized slice
array_type   := "[" type ";" expr "]"    // N must be comptime usize

tuple_type :=
    "(" ")"                          // unit
  | "(" type ")"                     // parentheses
  | "(" type "," types? ")"          // tuple type (trailing comma)

fn_type := "fn" "(" type_list? ")" ret

types     := type ("," type)* ","?
type_list := types
```

Notes:
- `fn(...) -> R` is a function type. In v0.1, function types may only appear behind pointers (`const* fn(...) -> R`).
- Although `auto` is parsed in type positions, it is only permitted in specific contexts; see `spec/auto.md`.

## Blocks and statements

```
block := "{" stmt* expr? "}"

stmt :=
    let_stmt ";"
  | expr ";"
  | "return" expr? ";"
  | "break" expr? ";"
  | "continue" ";"

let_stmt := "let" pattern (":" type)? ("=" expr)?
```

## Expressions (precedence-shaped)

This grammar sketches precedence; implementations should use a precedence parser.

```
expr := assign_expr

assign_expr := unary_expr "=" assign_expr
            | logical_or_expr

logical_or_expr  := logical_or_expr "||" logical_and_expr
                 | logical_and_expr
logical_and_expr := logical_and_expr "&&" equality_expr
                 | equality_expr

equality_expr := equality_expr ("==" | "!=") relational_expr
              | relational_expr

relational_expr := relational_expr ("<" | "<=" | ">" | ">=") bit_or_expr
                | bit_or_expr

bit_or_expr := bit_or_expr "|" bit_xor_expr
            | bit_xor_expr
bit_xor_expr := bit_xor_expr "^" bit_and_expr
             | bit_and_expr
bit_and_expr := bit_and_expr "&" shift_expr
             | shift_expr
shift_expr := shift_expr ("<<" | ">>") additive_expr
           | additive_expr

additive_expr := additive_expr ("+" | "-") multiplicative_expr
              | multiplicative_expr
multiplicative_expr := multiplicative_expr ("*" | "/" | "%") cast_expr
                    | cast_expr
cast_expr := cast_expr "as" type
          | unary_expr

unary_expr := ("-" | "!" | "*" | "&" | "~") unary_expr
            | "&" "mut" unary_expr
            | postfix_expr

postfix_expr := primary_expr
             | postfix_expr "(" args? ")"            // call
             | postfix_expr "." IDENT                // field / tuple index
             | postfix_expr "." IDENT "(" args? ")"  // method call
             | postfix_expr "[" expr "]"             // index
             | postfix_expr "?"                      // try (`spec/stdlib.md`)

args := expr ("," expr)* ","?
```

Primary expressions:

```
primary_expr :=
    INT
  | FLOAT
  | STRING
  | array_lit
  | "true" | "false"
  | "(" ")"                          // unit
  | "(" expr ")"                     // parens
  | "(" expr "," args? ")"           // tuple expr (trailing comma)
  | path
  | path "{" field_inits? "}"        // struct literal
  | block
  | "comptime" block
  | "if" expr block ("else" expr_or_block)?
  | "while" expr block
  | "loop" block
  | "match" expr "{" match_arm* "}"

expr_or_block := expr | block
field_inits   := (IDENT ":" expr) ("," IDENT ":" expr)* ","?

array_lit :=
    "[" array_elems? "]"
  | "[" expr ";" expr "]"

array_elems := expr ("," expr)* ","?
```

Notes:
- A v0.1 implementation may use a restricted `if`/`match` condition form to disambiguate struct literals; the surface language intends the Rust behavior.

## Match and patterns

```
match_arm := pattern ("if" expr)? "=>" expr_or_block ","?

pattern := pattern "|" pattern_primary
         | pattern_primary

pattern_primary :=
    "_"
  | INT | "true" | "false"
  | "mut" IDENT | IDENT
  | "(" ")"                             // empty tuple
  | "(" pattern ")"                     // parens
  | "(" pattern "," pat_list? ")"       // tuple pattern
  | path "(" pat_list? ")"              // enum variant pattern
  | path "{" pat_fields? "}"            // struct pattern
  | path                                // path pattern (fieldless enum variant)

pat_list   := pattern ("," pattern)* ","?
pat_fields := (IDENT ":" pattern) ("," IDENT ":" pattern)* ","?
```

## Planned (not required in early v0.1 implementations)

Closure literals (syntax reserved):

```
closure_expr := "|" closure_params? "|" expr_or_block
closure_params := closure_param ("," closure_param)* ","?
closure_param := IDENT (":" type)?
```

`if let` / `while let` (planned for 0.2.0):

```
if_expr :=
    "if" "let" pattern "=" expr block ("else" expr_or_block)?
  | "if" expr block ("else" expr_or_block)?

while_expr :=
    "while" "let" pattern "=" expr block
  | "while" expr block
```

Range expressions (planned for 0.2.0):

```
range_expr := expr ".." expr
           | expr "..=" expr
```

Struct literal field init shorthand (planned for 0.2.0):

```
field_inits := field_init ("," field_init)* ","?
field_init  := IDENT ":" expr
            | IDENT
```

Enum struct variants (planned for 0.2.0):

```
variant_payload := "(" types? ")"
               | "{" (IDENT ":" type ","?)* "}"
```
