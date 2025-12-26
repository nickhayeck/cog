#include <cstdlib>
#include <iostream>
#include <optional>
#include <string>
#include <string_view>

#include "ast.hpp"
#include "check.hpp"
#include "diag.hpp"
#include "emit_llvm.hpp"
#include "parse.hpp"
#include "resolve.hpp"
#include "session.hpp"
#include "target.hpp"

static void usage(const char* argv0) {
    std::cerr << "usage: " << argv0
              << " [--dump-ast] [--dump-tokens] [--emit-llvm <out.ll>] "
                 "[--emit-bc <out.bc>] [--emit-obj <out.o>] [--emit-exe <out>] "
                 "[--target <triple>] <file.cg>\n";
}

int main(int argc, char** argv) {
    bool dump_ast = false;
    bool dump_tokens = false;
    std::optional<std::string_view> emit_llvm{};
    std::optional<std::string_view> emit_bc{};
    std::optional<std::string_view> emit_obj{};
    std::optional<std::string_view> emit_exe{};
    std::optional<std::string_view> target_triple{};
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
        if (arg == "--emit-bc") {
            if (i + 1 >= argc) {
                usage(argv[0]);
                return 2;
            }
            emit_bc = std::string_view(argv[++i]);
            continue;
        }
        if (arg == "--emit-obj") {
            if (i + 1 >= argc) {
                usage(argv[0]);
                return 2;
            }
            emit_obj = std::string_view(argv[++i]);
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
        if (arg == "--target") {
            if (i + 1 >= argc) {
                usage(argv[0]);
                return 2;
            }
            target_triple = std::string_view(argv[++i]);
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
    std::optional<cog::TargetSpec> target =
        cog::compute_target_spec(session, target_triple);
    std::optional<cog::CheckedCrate> checked{};
    if (!session.has_errors() && target)
        checked = cog::check_crate(session, crate, target->layout);

    if (!session.has_errors() && checked && target &&
        (emit_llvm || emit_bc || emit_obj || emit_exe)) {
        cog::EmitLlvmOptions opts{};
        opts.target = &*target;
        if (emit_llvm)
            opts.out_ll = std::filesystem::path(std::string(*emit_llvm));
        if (emit_bc) opts.out_bc = std::filesystem::path(std::string(*emit_bc));
        if (emit_obj)
            opts.out_obj = std::filesystem::path(std::string(*emit_obj));
        if (emit_exe)
            opts.out_exe = std::filesystem::path(std::string(*emit_exe));
        (void)cog::emit_llvm(session, crate, *checked, opts);
    }

    if (session.has_errors()) {
        for (const auto& d : session.diags)
            std::cerr << cog::format_diagnostic(session.sources, d) << "\n";
        return 1;
    }

    if (dump_ast) {
        cog::ParseState* parsed = session.parse(file);
        if (parsed && parsed->root) cog::dump_ast(std::cout, parsed->root);
    }
    return 0;
}
