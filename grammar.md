# Cog v0.0.18 prototype grammar (draft)

This is an **approximate** EBNF aligned with the v0.0.18 prototype parser (`src/parser.y` + `src/lexer.l`). It describes the subset that currently parses; it is not intended to be a complete Rust grammar.

Core language grammar (v0.1 draft) lives in `spec/syntax.md`. The prototype grammar is intended to be close to `spec/`, but remains incomplete and may accept or reject programs differently in edge cases.

Notes:
- No generics.
- No traits/dyn (removed in v0.0.14).
- Item tags are written after the keyword: `struct[repr(C)] Name { ... }`, `fn[extern(C)] foo(...);`.
- To make `use a::{b,c};` parse reliably, the lexer emits a dedicated token for `::{` (`TOK_COLONCOLON_LBRACE` in the implementation).
- Expression parsing is precedence-based: assignment, `||`, `&&`, equality, comparisons, `+/-`, `*//%`, `as`, unary, postfix.
 - `builtin::...` names are ordinary paths syntactically; they are handled specially during type checking / comptime evaluation.

## Lexical notes
- Identifiers: `[A-Za-z_][A-Za-z0-9_]*`
- Integers: `[0-9]+` (decimal only)
- Strings: `"..."` and `c"..."` (v0.0.x supports basic escapes: `\\n`, `\\r`, `\\t`, `\\0`, `\\\"`, `\\\\`)
- Comments: `// ...` and `/* ... */`

## EBNF (approximate)

### Program
```
program       := item*
```

### Items
```
item          := vis? item_core
vis           := "pub" | "pub" "(" "crate" ")" | ε

tags          := "[" tag_list? "]"
tag_list      := tag ("," tag)* ","?
tag           := path
              | path "(" path ")"
              | path "(" STRING ")"

item_core     := "use" tags? use_tree ";"
              | "mod" tags? IDENT "{" item* "}"
              | "mod" tags? IDENT ";"
              | "struct" tags? IDENT "{" field_decl* "}"
              | "struct" tags? IDENT "(" types? ")" ";"
              | "enum" tags? IDENT "{" variant_decl* "}"
              | "impl" tags? path "{" impl_item* "}"
              | "fn" tags? IDENT "(" params? ")" ret? (block | ";")
              | "const" tags? IDENT ":" type "=" expr ";"
              | "static" tags? IDENT ":" type "=" expr ";"
              | "type" tags? IDENT "=" type ";"

impl_item     := vis? "fn" tags? IDENT "(" params? ")" ret? block
ret           := "->" type
```

### `use` trees
```
use_tree      := path use_alias?
              | path "::{" use_trees? "}"
use_alias     := "as" IDENT
use_trees     := use_tree ("," use_tree)* ","?
```

### Fields and variants
```
field_decl    := vis? IDENT ":" type ","?

variant_decl  := IDENT ","?
              | IDENT "=" expr ","?
              | IDENT "(" types? ")" ","?
```

### Parameters
```
params        := param ("," param)* ("," "...")?
param         := IDENT ":" type
              | "comptime" IDENT ":" type
```

### Paths
```
path          := path_seg ("::" path_seg)*
path_seg      := IDENT | "Self"

qpath         := path_seg "::" path_seg ("::" path_seg)*   // at least 2 segments (used in patterns)
```

### Types
```
type          := path
              | "type"
              | "!"
              | "fn" "(" types? ")" ret?
              | "const" "*" type
              | "mut" "*" type
              | "[" type "]"              // slice type (unsized)
              | "[" type ";" expr "]"     // array type
              | "(" ")"                   // unit
              | "(" type ")"              // parens
              | "(" type "," types? ")"   // tuple type (trailing comma allowed)

types         := type ("," type)* ","?
```

### Blocks and statements
```
block         := "{" block_elems? "}"
block_elems   := stmt* expr?

stmt          := let_stmt ";"
              | expr ";"
              | "return" expr? ";"
              | "break" expr? ";"
              | "continue" ";"

let_stmt      := "let" pattern (":" type)? ("=" expr)?
```

### Expressions (precedence-shaped)
```
expr          := assign_expr
assign_expr   := unary_expr "=" assign_expr
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

cast_expr     := cast_expr "as" type
              | unary_expr

unary_expr    := ("-" | "!" | "*" | "&") unary_expr
              | "&" "mut" unary_expr
              | postfix_expr

postfix_expr  := primary_expr
              | postfix_expr "(" args? ")"          // call
              | postfix_expr "." IDENT              // field
              | postfix_expr "." INT                // tuple index
              | postfix_expr "." IDENT "(" args? ")"// method call
              | postfix_expr "[" expr "]"           // index

args          := expr ("," expr)* ","?
```

Primary expressions:
```
primary_expr  := INT | STRING | "true" | "false"
              | "(" ")"                         // unit
              | "(" expr ")"                    // parens
              | "(" expr "," args? ")"          // tuple expr (trailing comma allowed)
              | path
              | path "{" field_inits? "}"       // struct literal
              | block
              | "comptime" block
              | "if" expr_nostruct block ("else" expr_or_block)?
              | "while" expr_nostruct block
              | "loop" block
              | "match" expr_nostruct "{" match_arm* "}"

field_inits   := (IDENT ":" expr) ("," IDENT ":" expr)* ","?
expr_or_block := expr
expr_nostruct := expr                         // implementation uses a restricted form here
```

### Match and patterns
```
match_arm     := pattern ("if" expr)? "=>" expr_or_block ","?

pattern       := pattern "|" pattern_primary
              | pattern_primary

pattern_primary := "_"
               | INT | "true" | "false"
               | "mut" IDENT | IDENT                // binding
               | "(" ")"                            // empty tuple
               | "(" pattern ")"                    // parens
               | "(" pattern "," pat_list? ")"      // tuple pattern
               | qpath "(" pat_list? ")"            // variant pattern
               | qpath "{" pat_fields? "}"          // struct pattern
               | qpath                              // path pattern

pat_list      := pattern ("," pattern)* ","?
pat_fields    := (IDENT ":" pattern) ("," IDENT ":" pattern)* ","?
```
