#include "parse.hpp"

#include "ast.hpp"

#include <iostream>

int main(int argc, char** argv) {
  if (argc != 2) {
    std::cerr << "usage: cogc <file>\n";
    return 2;
  }

  cog::ParseState result = cog::parse_file(argv[1]);
  if (!result.errors.empty()) {
    for (const auto& e : result.errors) std::cerr << e << "\n";
    return 1;
  }

  cog::print_tree(std::cout, result.root);
  return 0;
}
