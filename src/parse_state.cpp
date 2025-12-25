#include "parse_state.hpp"

#include <cstdlib>

namespace cog {

ParseState* g_parse_state = nullptr;

std::string take_str(char* s) {
  if (!s) return {};
  std::string out{s};
  std::free(s);
  return out;
}

std::string take_string(std::string* s) {
  if (!s) return {};
  std::string out = std::move(*s);
  delete s;
  return out;
}

void push_error(Span span, std::string message) {
  if (!g_parse_state) return;
  g_parse_state->diags.push_back(Diagnostic{.severity = Severity::Error, .span = span, .message = std::move(message)});
}

}  // namespace cog
