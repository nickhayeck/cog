# Cog v0.0.1 grammar (draft)

This is a **v0.0.1 parser-focused** grammar aligned with `src/parser.y` + `src/lexer.l`.

Notes:
- No generics in types or items.
- Many Rust constructs are not yet covered (e.g. `if/while/loop`, tuple/struct patterns, match guards).
- The current parser builds a lightweight tree for inspection, not a fully typed AST.

## Lexical notes

- Identifiers: `[A-Za-z_][A-Za-z0-9_]*`
- Integers: `[0-9]+` (decimal only)
- Strings: `"..."` (escapes are parsed but not interpreted yet)
- Comments: `// ...` and `/* ... */`

## EBNF (approximate)

### Program
```
program       := item*
```

### Items
```
item          := attr* vis item_core
vis           := "pub" | ε
attr          := "#[" path ( "(" path ")" )? "]"

item_core     := "use" path ";"
              | "mod" IDENT "{" item* "}"
              | "struct" IDENT "{" field* "}"
              | "enum" IDENT "{" variant* "}"
              | "trait" IDENT "{" trait_decl* "}"
              | "impl" path "{" impl_item* "}"
              | "impl" path "for" path "{" impl_item* "}"
              | "fn" IDENT "(" param_list? ")" ret? block
              | "const" IDENT ":" type "=" expr ";"

trait_decl    := fn_sig ";"

impl_item     := vis "fn" IDENT "(" param_list? ")" ret? block

fn_sig        := "fn" IDENT "(" param_list? ")" ret?
ret           := "->" type
```

### Fields and variants
```
field         := attr* vis IDENT ":" type ","?

variant       := IDENT ","?
              | IDENT "(" type_list? ")" ","?
```

### Parameters
```
param_list    := param ("," param)* ","?
param         := IDENT ":" type
              | "comptime" IDENT ":" type
```

### Paths
```
path          := path_seg ("::" path_seg)*
path_seg      := IDENT | "Self"
```

### Types
```
type          := path
              | "const" "*" type
              | "mut" "*" type
              | "dyn" path
              | "[" type "]"              // slice
              | "[" type ";" expr "]"     // array
              | "(" ")"                   // unit
```

### Blocks and statements
```
block         := "{" block_elems? "}"
block_elems   := stmts expr
              | stmts
              | expr

stmts         := stmt+
stmt          := let_stmt ";"
              | expr ";"
              | "return" ";"
              | "return" expr ";"

let_stmt      := "let" IDENT ":" type "=" expr
              | "let" "mut" IDENT ":" type "=" expr
```

### Expressions
```
expr          := INT
              | STRING
              | "(" ")"
              | path
              | block
              | "comptime" block
              | "match" expr "{" match_arm* "}"
              | expr "=" expr
              | expr ("+"|"-"|"*"|"/") expr
              | expr "as" type
              | "-" expr
              | "*" expr
              | expr "(" arg_list? ")"
              | expr "." IDENT
              | expr "." IDENT "(" arg_list? ")"
              | expr "[" expr "]"
              | path "{" field_init_list? "}"
              | "(" expr ")"

arg_list      := expr ("," expr)* ","?
field_init_list := field_init ("," field_init)* ","?
field_init    := IDENT ":" expr
```

### Match
```
match_arm     := pattern "=>" expr_or_block ","?
expr_or_block := expr

pattern       := "_"
              | INT
              | path
              | path "(" pattern_list? ")"
              | pattern "|" pattern

pattern_list  := pattern ("," pattern)* ","?
```
