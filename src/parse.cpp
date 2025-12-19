#include "parse.hpp"

#include <cstdio>

int yyparse(void);
extern FILE* yyin;

extern int yylineno;
extern void yyrestart(FILE*);

namespace cog {

ParseState parse_file(const char* path) {
  ParseState state{};
  g_parse_state = &state;

  FILE* file = std::fopen(path, "rb");
  if (!file) {
    push_error(std::string("could not open file: ") + path);
    g_parse_state = nullptr;
    return state;
  }

  yyin = file;
  yylineno = 1;
  yyrestart(file);

  (void)yyparse();

  std::fclose(file);
  g_parse_state = nullptr;
  return state;
}

}  // namespace cog
