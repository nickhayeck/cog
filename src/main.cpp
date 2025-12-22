#include "parse.hpp"

#include "ast.hpp"
#include "check.hpp"
#include "diag.hpp"
#include "emit_llvm.hpp"
#include "resolve.hpp"
#include "session.hpp"

#include <iostream>
#include <optional>
#include <cstdlib>
#include <string>
#include <string_view>

static void usage(const char* argv0) {
  std::cerr << "usage: " << argv0
            << " [--dump-ast] [--dump-tokens] [--emit-llvm <out.ll>] [--emit-exe <out>] <file.cg>\n";
}

int main(int argc, char** argv) {
  bool dump_ast = false;
  bool dump_tokens = false;
  std::optional<std::string_view> emit_llvm{};
  std::optional<std::string_view> emit_exe{};
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
    if (arg == "--emit-llvm") {
      if (i + 1 >= argc) {
        usage(argv[0]);
        return 2;
      }
      emit_llvm = std::string_view(argv[++i]);
      continue;
    }
    if (arg == "--emit-exe") {
      if (i + 1 >= argc) {
        usage(argv[0]);
        return 2;
      }
      emit_exe = std::string_view(argv[++i]);
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
  std::optional<cog::CheckedCrate> checked{};
  if (!session.has_errors()) checked = cog::check_crate(session, crate);

  if (!session.has_errors() && emit_llvm) {
    cog::EmitLlvmOptions opts{};
    opts.out_ll = std::filesystem::path(std::string(*emit_llvm));
    (void)cog::emit_llvm_ir(session, crate, *checked, opts);
  }

  if (!session.has_errors() && emit_exe) {
    std::filesystem::path exe_path = std::filesystem::path(std::string(*emit_exe));
    std::filesystem::path ll_path{};
    if (emit_llvm) {
      ll_path = std::filesystem::path(std::string(*emit_llvm));
    } else {
      ll_path = std::filesystem::path(exe_path.string() + ".ll");
    }

    cog::EmitLlvmOptions opts{};
    opts.out_ll = ll_path;
    (void)cog::emit_llvm_ir(session, crate, *checked, opts);

    if (!session.has_errors()) {
      std::string cmd = "clang \"" + ll_path.string() + "\" -o \"" + exe_path.string() + "\"";
      int rc = std::system(cmd.c_str());
      if (rc != 0) {
        session.diags.push_back(cog::Diagnostic{
            .severity = cog::Severity::Error,
            .span = cog::Span{},
            .message = "clang failed: " + cmd,
        });
      }
    }
  }

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
