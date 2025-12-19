#include "parse_state.hpp"

#include <cstdlib>
#include <utility>

namespace cog {

ParseState* g_parse_state = nullptr;

Node* mk(NodeKind kind) {
  return mk(kind, std::string{}, std::vector<Node*>{});
}

Node* mk(NodeKind kind, std::string text) {
  return mk(kind, std::move(text), std::vector<Node*>{});
}

Node* mk(NodeKind kind, std::vector<Node*> children) {
  return mk(kind, std::string{}, std::move(children));
}

Node* mk(NodeKind kind, std::string text, std::vector<Node*> children) {
  if (!g_parse_state) return nullptr;
  return g_parse_state->arena.make(kind, std::move(text), std::move(children));
}

std::string take_str(char* s) {
  if (!s) return {};
  std::string out{s};
  std::free(s);
  return out;
}

void push_error(std::string message) {
  if (!g_parse_state) return;
  g_parse_state->errors.push_back(std::move(message));
}

}  // namespace cog

