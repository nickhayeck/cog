---
title: Lexical Structure
version: 0.1.0-draft
---

# Lexical structure

## Source text

- Source files are UTF-8 text. For v0.1, identifiers are limited to ASCII; other Unicode code points may appear inside comments and string literals.
- Lines end with `\n` (LF). `\r\n` is accepted and normalized.

## Whitespace and comments

Whitespace separates tokens and is otherwise insignificant, except inside string/char literals.

Comments:
- Line comment: `//` to end-of-line.
- Block comment: `/* ... */` (nesting is **not** required in v0.1).

## Identifiers

An identifier matches:

```
IDENT := [_A-Za-z][_A-Za-z0-9]*
```

Identifiers are case-sensitive.

## Keywords (reserved)

The following tokens are reserved words and cannot be used as identifiers:

`as break comptime const continue crate else enum false fn if impl let loop match mod mut pub return Self static struct true type use while`

Notes:
- Tags like `repr`, `extern`, `export`, `inline`, `test` are **not** keywords; they are parsed as identifiers inside item tag lists.
- `builtin` is not a keyword; `builtin::name` is parsed as an ordinary path.

## Punctuators and operators

Cog uses these punctuators/operators (non-exhaustive list):

`(` `)` `{` `}` `[` `]` `,` `;` `:` `.` `::` `->`

`=` `==` `!=` `<` `<=` `>` `>=`

`+` `-` `*` `/` `%`

`!` `&&` `||`

`&` `^` `~` `<<` `>>`

`|` (bitwise OR; also pattern alternation; also used in closure syntax)

`...` (extern-only C varargs marker in parameter lists)

## Integer literals

For v0.1, integer literals support decimal, hex, octal, and binary.

Syntax:

```
INT := DEC_INT | HEX_INT | OCT_INT | BIN_INT

DEC_INT := [0-9] ([0-9] | "_")*
HEX_INT := "0x" HEX_DIGIT (HEX_DIGIT | "_")*
OCT_INT := "0o" [0-7] ([0-7] | "_")*
BIN_INT := "0b" ("0"|"1") ("0"|"1"|"_")*

HEX_DIGIT := [0-9A-Fa-f]
```

Underscores are permitted as visual separators and are ignored for value interpretation.

Typing:
- An integer literal has type `comptime_int` until it is coerced or cast to a concrete integer type.
- If a context requires a specific integer type, the literal must fit; otherwise it is a compile-time error.

## Float literals

Float literal syntax is Rust-like:

```
FLOAT := DIGITS "." DIGITS (EXPONENT)?
       | DIGITS EXPONENT

EXPONENT := ("e"|"E") ("+"|"-")? DIGITS

DIGITS := [0-9] ([0-9] | "_")*
```

Typing:
- A float literal has type `comptime_float` until coerced or cast.
- If a context requires a concrete float type, the value must be representable; otherwise it is a compile-time error.

## Bool literals

`true` and `false` are literals of type `bool`.

## Char literals

Char literals are planned but not required for early v0.1 implementations.

If implemented, a char literal has the form:

```
CHAR := "'" (non-quote char | escape) "'"
```

and has type `char` (a Unicode scalar value).

## String literals

Cog has two string literal forms:

1) **Byte slice** string:

```
STRING := "\"" (string_char | escape)* "\""
```

Type: `const* [u8]` (a fat pointer `{ ptr: const* u8, len: usize }`).

2) **C string** literal:

```
CSTRING := "c" STRING
```

Type: `const* u8` pointing to a NUL-terminated byte sequence.

### Escapes

For v0.1, the following escapes are required:

- `\\n` newline (U+000A)
- `\\r` carriage return (U+000D)
- `\\t` tab (U+0009)
- `\\\"` double quote
- `\\\\` backslash
- `\\0` NUL byte

Additional escapes (hex, unicode) are planned.
