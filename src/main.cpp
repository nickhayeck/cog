#include "parse.hpp"

#include "ast.hpp"
#include "check.hpp"
#include "diag.hpp"
#include "resolve.hpp"
#include "session.hpp"

#include <iostream>
#include <string_view>

static void usage(const char* argv0) {
  std::cerr << "usage: " << argv0 << " [--dump-ast] [--dump-tokens] <file.cg>\n";
}

int main(int argc, char** argv) {
  bool dump_ast = false;
  bool dump_tokens = false;
  const char* input_path = nullptr;

  for (int i = 1; i < argc; i++) {
    std::string_view arg = argv[i];
    if (arg == "--dump-ast") {
      dump_ast = true;
      continue;
    }
    if (arg == "--dump-tokens") {
      dump_tokens = true;
      continue;
    }
    if (!arg.empty() && arg[0] == '-') {
      usage(argv[0]);
      return 2;
    }
    input_path = argv[i];
  }

  if (!input_path) {
    usage(argv[0]);
    return 2;
  }

  cog::Session session{};
  cog::FileId file = session.add_file(input_path);

  if (dump_tokens) {
    cog::dump_tokens(file, input_path, std::cout);
    return 0;
  }

  cog::ResolvedCrate crate = cog::resolve_crate(session, file);
  if (!session.has_errors()) (void)cog::check_crate(session, crate);

  if (session.has_errors()) {
    for (const auto& d : session.diags) std::cerr << cog::format_diagnostic(session.sources, d) << "\n";
    return 1;
  }

  if (dump_ast) {
    cog::ParseState* parsed = session.parse(file);
    if (parsed && parsed->root) cog::dump_ast(std::cout, parsed->root);
  }
  return 0;
}
