#pragma once

#include "ast.hpp"
#include "diag.hpp"

#include <string>
#include <utility>
#include <vector>

namespace cog {

struct ParseState {
  FileId file = 0;
  AstArena arena{};
  FileAst* root = nullptr;
  std::vector<Diagnostic> diags{};
};

extern ParseState* g_parse_state;

std::string take_str(char* s);  // takes ownership and frees
void push_error(Span span, std::string message);

template <typename T, typename... Args>
T* mk(Span span, Args&&... args) {
  if (!g_parse_state) return nullptr;
  return g_parse_state->arena.make<T>(std::move(span), std::forward<Args>(args)...);
}

}  // namespace cog

