#pragma once

#include "ast.hpp"

#include <string>
#include <vector>

namespace cog {

struct ParseState {
  Arena arena{};
  Node* root = nullptr;
  std::vector<std::string> errors{};
};

extern ParseState* g_parse_state;

Node* mk(NodeKind kind);
Node* mk(NodeKind kind, std::string text);
Node* mk(NodeKind kind, std::vector<Node*> children);
Node* mk(NodeKind kind, std::string text, std::vector<Node*> children);

std::string take_str(char* s);  // takes ownership and frees
void push_error(std::string message);

}  // namespace cog

