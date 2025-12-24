%option noyywrap nodefault yylineno

%{
#include "parse_state.hpp"
#include "parser.hpp"

#include <cstdlib>
#include <cstring>

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

static char* dup_string_literal() {
  // yytext includes quotes; decode a small set of escapes for v0.0.x.
  //
  // NOTE: this returns a NUL-terminated C string because the parser consumes it
  // via `take_str(char*)`. As a result, embedded `\0` is not supported yet.
  size_t len = static_cast<size_t>(yyleng);
  if (len < 2) return dup_yytext();

  // Worst case: no escapes, size is (len - 2) + 1 for NUL.
  char* out = static_cast<char*>(std::malloc(len));
  if (!out) return nullptr;

  size_t j = 0;
  for (size_t i = 1; i + 1 < len; i++) {
    char c = yytext[i];
    if (c != '\\') {
      out[j++] = c;
      continue;
    }
    if (i + 1 >= len - 1) {
      out[j++] = '\\';
      break;
    }
    char esc = yytext[++i];
    switch (esc) {
      case 'n':
        out[j++] = '\n';
        break;
      case 'r':
        out[j++] = '\r';
        break;
      case 't':
        out[j++] = '\t';
        break;
      case '\\':
        out[j++] = '\\';
        break;
      case '"':
        out[j++] = '"';
        break;
      default:
        // Keep unknown escapes as-is for now (e.g. "\xNN" will remain "xNN").
        out[j++] = esc;
        break;
    }
  }
  out[j] = '\0';

  // Shrink to fit (best effort).
  char* shrunk = static_cast<char*>(std::realloc(out, j + 1));
  return shrunk ? shrunk : out;
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
"trait"                  { return KW_TRAIT; }
"impl"                   { return KW_IMPL; }
"for"                    { return KW_FOR; }
"type"                   { return KW_TYPE; }
"const"                  { return KW_CONST; }
"static"                 { return KW_STATIC; }
"mod"                    { return KW_MOD; }
"use"                    { return KW_USE; }
"pub"                    { return KW_PUB; }
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
"dyn"                    { return KW_DYN; }
"Self"                   { return KW_SELF_TYPE; }
"true"                   { return KW_TRUE; }
"false"                  { return KW_FALSE; }

[0-9]+                   { yylval.int_val = std::atoll(yytext); return INT; }

\"([^\"\\]|\\.)*\"       { yylval.str = dup_string_literal(); return STRING; }

[_a-zA-Z][_a-zA-Z0-9]*   { yylval.str = dup_yytext(); return IDENT; }

.                        { return static_cast<unsigned char>(yytext[0]); }

%%
