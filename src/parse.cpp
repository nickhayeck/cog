#include "parse.hpp"

#include "parser.hpp"

#include <cstdio>
#include <cctype>

int yyparse(void);
int yylex(void);
extern FILE* yyin;

extern void yyrestart(FILE*);
extern int yylineno;
extern int yycolumn;
extern YYSTYPE yylval;
extern YYLTYPE yylloc;

namespace cog {

ParseState parse_file(FileId file_id, const char* path) {
  ParseState state{};
  state.file = file_id;
  g_parse_state = &state;

  FILE* input = std::fopen(path, "rb");
  if (!input) {
    push_error(Span{.file = state.file}, std::string("could not open file: ") + path);
    g_parse_state = nullptr;
    return state;
  }

  yyin = input;
  yylineno = 1;
  yycolumn = 1;
  yyrestart(input);

  (void)yyparse();

  std::fclose(input);
  g_parse_state = nullptr;
  return state;
}

static const char* token_name(int tok) {
  switch (tok) {
    case 0:
      return "EOF";
    case IDENT:
      return "IDENT";
    case STRING:
      return "STRING";
    case INT:
      return "INT";

    case KW_FN:
      return "fn";
    case KW_STRUCT:
      return "struct";
    case KW_ENUM:
      return "enum";
    case KW_TRAIT:
      return "trait";
    case KW_IMPL:
      return "impl";
    case KW_FOR:
      return "for";
    case KW_TYPE:
      return "type";
    case KW_CONST:
      return "const";
    case KW_STATIC:
      return "static";
    case KW_MOD:
      return "mod";
    case KW_USE:
      return "use";
    case KW_PUB:
      return "pub";
    case KW_AS:
      return "as";
    case KW_LET:
      return "let";
    case KW_MUT:
      return "mut";
    case KW_IF:
      return "if";
    case KW_ELSE:
      return "else";
    case KW_WHILE:
      return "while";
    case KW_LOOP:
      return "loop";
    case KW_MATCH:
      return "match";
    case KW_RETURN:
      return "return";
    case KW_BREAK:
      return "break";
    case KW_CONTINUE:
      return "continue";
    case KW_COMPTIME:
      return "comptime";
    case KW_DYN:
      return "dyn";
    case KW_SELF_TYPE:
      return "Self";
    case KW_TRUE:
      return "true";
    case KW_FALSE:
      return "false";

    case TOK_COLONCOLON:
      return "::";
    case TOK_COLONCOLON_LBRACE:
      return "::{";
    case TOK_ARROW:
      return "->";
    case TOK_FATARROW:
      return "=>";
    case TOK_EQEQ:
      return "==";
    case TOK_NEQ:
      return "!=";
    case TOK_LE:
      return "<=";
    case TOK_GE:
      return ">=";
    case TOK_ANDAND:
      return "&&";
    case TOK_OROR:
      return "||";
  }
  return nullptr;
}

void dump_tokens(FileId file_id, const char* path, std::ostream& os) {
  ParseState state{};
  state.file = file_id;
  g_parse_state = &state;

  FILE* input = std::fopen(path, "rb");
  if (!input) {
    push_error(Span{.file = state.file}, std::string("could not open file: ") + path);
    for (const auto& d : state.diags) os << d.message << "\n";
    g_parse_state = nullptr;
    return;
  }

  yyin = input;
  yylineno = 1;
  yycolumn = 1;
  yyrestart(input);

  while (true) {
    int tok = yylex();
    if (tok == 0) break;

    os << yylloc.first_line << ":" << yylloc.first_column << " ";
    if (const char* name = token_name(tok)) {
      os << name;
    } else if (tok >= 0 && tok < 128 && std::isprint(tok)) {
      os << "'" << static_cast<char>(tok) << "'";
    } else {
      os << "<tok " << tok << ">";
    }

    switch (tok) {
      case IDENT:
      case STRING: {
        std::string s = take_str(yylval.str);
        os << " \"" << s << "\"";
        break;
      }
      case INT:
        os << " " << yylval.int_val;
        break;
      default:
        break;
    }
    os << "\n";
  }

  std::fclose(input);
  g_parse_state = nullptr;
}

}  // namespace cog
