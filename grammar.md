# Cog v0.0.11 grammar (draft)

This is an **approximate** EBNF aligned with the v0.0.11 prototype parser (`src/parser.y` + `src/lexer.l`). It describes the subset that currently parses; it is not intended to be a complete Rust grammar.

Notes:
- No generics (types/items/traits).
- Attributes are parsed as `#[path]` or `#[path(path)]` (argument is a path, not a full token tree yet).
- To make `use a::{b,c};` parse reliably, the lexer emits a dedicated token for `::{` (`TOK_COLONCOLON_LBRACE` in the implementation).
- Expression parsing is precedence-based: assignment, `||`, `&&`, equality, comparisons, `+/-`, `*//%`, `as`, unary, postfix.
 - `builtin::...` names are ordinary paths syntactically; they are handled specially during type checking / comptime evaluation.

## Lexical notes
- Identifiers: `[A-Za-z_][A-Za-z0-9_]*`
- Integers: `[0-9]+` (decimal only)
- Strings: `"..."` (escapes are lexed; semantics are future work)
- Comments: `// ...` and `/* ... */`

## EBNF (approximate)

### Program
```
program       := item*
```

### Items
```
item          := attr* vis? item_core
vis           := "pub" | ε
attr          := "#[" path "]"
              | "#[" path "(" path ")" "]"

item_core     := "use" use_tree ";"
              | "mod" IDENT "{" item* "}"
              | "mod" IDENT ";"
              | "struct" IDENT "{" field_decl* "}"
              | "enum" IDENT "{" variant_decl* "}"
              | "trait" IDENT "{" trait_item* "}"
              | "impl" path "{" impl_item* "}"
              | "impl" path "for" path "{" impl_item* "}"
              | "fn" IDENT "(" params? ")" ret? block
              | "const" IDENT ":" type "=" expr ";"
              | "static" IDENT ":" type "=" expr ";"
              | "type" IDENT "=" type ";"

trait_item    := fn_decl ";"
impl_item     := attr* vis? "fn" IDENT "(" params? ")" ret? block
fn_decl       := "fn" IDENT "(" params? ")" ret?
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
field_decl    := attr* vis? IDENT ":" type ","?

variant_decl  := IDENT ","?
              | IDENT "(" types? ")" ","?
```

### Parameters
```
params        := param ("," param)* ","?
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
              | "const" "*" type
              | "mut" "*" type
              | "dyn" path
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
assign_expr   := postfix_expr "=" assign_expr
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

unary_expr    := ("-" | "!" | "*") unary_expr
              | postfix_expr

postfix_expr  := primary_expr
              | postfix_expr "(" args? ")"          // call
              | postfix_expr "." IDENT              // field
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
