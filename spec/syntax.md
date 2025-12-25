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
- The allowed tag sets and their semantics are specified in `spec/layout_abi.md` and `spec/tests.md`.
- Unknown tags are errors.

## Items

```
item := vis? item_core

item_core :=
    "use" tags? use_tree ";"
  | "mod" tags? IDENT "{" item* "}"
  | "mod" tags? IDENT ";"
  | "struct" tags? IDENT struct_body
  | "enum" tags? IDENT enum_body
  | "impl" tags? type_path "{" impl_item* "}"
  | "fn" tags? IDENT "(" params? ")" ret? (block | ";")
  | "const" tags? IDENT ":" type "=" expr ";"
  | "static" tags? IDENT ":" type "=" expr ";"
  | "type" tags? IDENT "=" type ";"

impl_item := vis? "fn" tags? IDENT "(" params? ")" ret? block
ret       := "->" type
```

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

### Enums

```
enum_body     := "{" variant_decl* "}"
variant_decl  := IDENT variant_payload? variant_discriminant? ","?
variant_payload := "(" types? ")"
variant_discriminant := "=" expr
```

Notes:
- Fieldless enums are enums where every variant has no payload (no `(...)`).

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
    type_path
  | "type"                 // the type-of-types (comptime-only)
  | "!"                    // never type
  | pointer_type
  | slice_type
  | array_type
  | tuple_type
  | fn_type

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

assign_expr := postfix_expr "=" assign_expr
            | logical_or_expr

logical_or_expr  := logical_or_expr "||" logical_and_expr
                 | logical_and_expr
logical_and_expr := logical_and_expr "&&" equality_expr
                 | equality_expr

equality_expr := equality_expr ("==" | "!=") relational_expr
              | relational_expr
relational_expr := relational_expr ("<" | "<=" | ">" | ">=") additive_expr
                | additive_expr
additive_expr := additive_expr ("+" | "-") multiplicative_expr
              | multiplicative_expr
multiplicative_expr := multiplicative_expr ("*" | "/" | "%") cast_expr
                    | cast_expr
cast_expr := cast_expr "as" type
          | unary_expr

unary_expr := ("-" | "!" | "*") unary_expr
            | postfix_expr

postfix_expr := primary_expr
             | postfix_expr "(" args? ")"            // call
             | postfix_expr "." IDENT                // field / tuple index
             | postfix_expr "." IDENT "(" args? ")"  // method call
             | postfix_expr "[" expr "]"             // index

args := expr ("," expr)* ","?
```

Primary expressions:

```
primary_expr :=
    INT
  | FLOAT
  | STRING
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
