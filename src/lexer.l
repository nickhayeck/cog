%option noyywrap nodefault yylineno

%{
#include "parse_state.hpp"
#include "parser.hpp"

#include <cstdlib>
#include <cstring>
#include <string>

int yycolumn = 1;

extern YYLTYPE yylloc;

#define YY_USER_ACTION                                                                            \
  yylloc.first_line = yylineno;                                                                    \
  yylloc.first_column = yycolumn;                                                                  \
  yylloc.last_line = yylineno;                                                                     \
  yylloc.last_column = yycolumn + static_cast<int>(yyleng) - 1;                                    \
  yycolumn += static_cast<int>(yyleng);

static char* dup_yytext() {
  size_t len = static_cast<size_t>(yyleng);
  char* out = static_cast<char*>(std::malloc(len + 1));
  if (!out) return nullptr;
  std::memcpy(out, yytext, len);
  out[len] = '\0';
  return out;
}

static std::string* dup_string_literal_bytes(size_t quote_start) {
  // yytext includes quotes; decode a small set of escapes for v0.0.x.
  //
  // This returns a byte string (may contain NUL) for v0.0.15+:
  // - STRING: `"..."` -> bytes
  // - CSTRING: `c"..."` -> bytes (codegen adds trailing NUL)
  size_t len = static_cast<size_t>(yyleng);
  if (len < quote_start + 2) return new std::string{};

  size_t open_quote = quote_start;
  size_t close_quote = len - 1;
  if (yytext[open_quote] != '"' || yytext[close_quote] != '"') return new std::string{yytext, yytext + len};

  std::string out{};
  out.reserve(len - (quote_start + 2));

  for (size_t i = open_quote + 1; i < close_quote; i++) {
    char c = yytext[i];
    if (c != '\\') {
      out.push_back(c);
      continue;
    }
    if (i + 1 >= close_quote) {
      out.push_back('\\');
      break;
    }
    char esc = yytext[++i];
    switch (esc) {
      case 'n':
        out.push_back('\n');
        break;
      case 'r':
        out.push_back('\r');
        break;
      case 't':
        out.push_back('\t');
        break;
      case '0':
        out.push_back('\0');
        break;
      case '\\':
        out.push_back('\\');
        break;
      case '"':
        out.push_back('"');
        break;
      default:
        // Keep unknown escapes as-is for now (e.g. "\xNN" will remain "xNN").
        out.push_back(esc);
        break;
    }
  }
  return new std::string(std::move(out));
}

%}

%x COMMENT

%%

[ \t\r]+                ;
\n+                      { yycolumn = 1; }

"//".*                   ;
"/*"                     { BEGIN(COMMENT); }
<COMMENT>"*/"            { BEGIN(INITIAL); }
<COMMENT>\n              { yycolumn = 1; }
<COMMENT>.               ;

"::{"                    { return TOK_COLONCOLON_LBRACE; }
"::"                     { return TOK_COLONCOLON; }
"->"                     { return TOK_ARROW; }
"=>"                     { return TOK_FATARROW; }
"=="                     { return TOK_EQEQ; }
"!="                     { return TOK_NEQ; }
"<="                     { return TOK_LE; }
">="                     { return TOK_GE; }
"&&"                     { return TOK_ANDAND; }
"||"                     { return TOK_OROR; }
"..."                    { return TOK_ELLIPSIS; }

"fn"                     { return KW_FN; }
"struct"                 { return KW_STRUCT; }
"enum"                   { return KW_ENUM; }
"impl"                   { return KW_IMPL; }
"type"                   { return KW_TYPE; }
"const"                  { return KW_CONST; }
"static"                 { return KW_STATIC; }
"mod"                    { return KW_MOD; }
"use"                    { return KW_USE; }
"pub"                    { return KW_PUB; }
"crate"                  { return KW_CRATE; }
"as"                     { return KW_AS; }
"let"                    { return KW_LET; }
"mut"                    { return KW_MUT; }
"if"                     { return KW_IF; }
"else"                   { return KW_ELSE; }
"while"                  { return KW_WHILE; }
"loop"                   { return KW_LOOP; }
"match"                  { return KW_MATCH; }
"return"                 { return KW_RETURN; }
"break"                  { return KW_BREAK; }
"continue"               { return KW_CONTINUE; }
"comptime"               { return KW_COMPTIME; }
"Self"                   { return KW_SELF_TYPE; }
"true"                   { return KW_TRUE; }
"false"                  { return KW_FALSE; }

[0-9]+                   { yylval.int_val = std::atoll(yytext); return INT; }

c\"([^\"\\]|\\.)*\"      { yylval.str_lit = dup_string_literal_bytes(1); return CSTRING; }
\"([^\"\\]|\\.)*\"       { yylval.str_lit = dup_string_literal_bytes(0); return STRING; }

[_a-zA-Z][_a-zA-Z0-9]*   { yylval.cstr = dup_yytext(); return IDENT; }

.                        { return static_cast<unsigned char>(yytext[0]); }

%%
