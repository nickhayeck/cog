#include <cstdlib>
#include <fstream>
#include <iostream>
#include <optional>
#include <string>
#include <string_view>

#include "ast.hpp"
#include "check.hpp"
#include "diag.hpp"
#include "hir.hpp"
#include "llvm_backend.hpp"
#include "lower_mir.hpp"
#include "mir.hpp"
#include "mir_interp.hpp"
#include "parse.hpp"
#include "resolve.hpp"
#include "session.hpp"
#include "target.hpp"

static void usage(const char* argv0) {
    std::cerr << "usage: " << argv0
              << " [--dump-ast] [--dump-tokens] [--emit-llvm <out.ll>] "
                 "[--emit-bc <out.bc>] [--emit-obj <out.o>] [--emit-exe <out>] "
                 "[--emit-hir <out.hir>] [--emit-mir <out.mir>] "
                 "[--emit-mir-after <pass> <out.mir>] "
                 "[--target <triple>] <file.cg>\n";
}

int main(int argc, char** argv) {
    bool dump_ast = false;
    bool dump_tokens = false;
    std::optional<std::string_view> emit_llvm{};
    std::optional<std::string_view> emit_bc{};
    std::optional<std::string_view> emit_obj{};
    std::optional<std::string_view> emit_exe{};
    std::optional<std::string_view> emit_hir{};
    std::optional<std::string_view> emit_mir{};
    std::optional<std::string_view> emit_mir_after_pass{};
    std::optional<std::string_view> emit_mir_after_out{};
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
        if (arg == "--emit-hir") {
            if (i + 1 >= argc) {
                usage(argv[0]);
                return 2;
            }
            emit_hir = std::string_view(argv[++i]);
            continue;
        }
        if (arg == "--emit-mir") {
            if (i + 1 >= argc) {
                usage(argv[0]);
                return 2;
            }
            emit_mir = std::string_view(argv[++i]);
            continue;
        }
        if (arg == "--emit-mir-after") {
            if (i + 2 >= argc) {
                usage(argv[0]);
                return 2;
            }
            emit_mir_after_pass = std::string_view(argv[++i]);
            emit_mir_after_out = std::string_view(argv[++i]);
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

    std::optional<cog::HirCrate> hir{};
    if (!session.has_errors() && checked) {
        hir = cog::build_hir(crate, *checked);
    }

    if (!session.has_errors() && hir && emit_hir) {
        std::ofstream os{std::string(*emit_hir)};
        if (!os) {
            session.diags.push_back(cog::Diagnostic{
                .severity = cog::Severity::Error,
                .span = cog::Span{},
                .message = "failed to open output file for --emit-hir",
            });
        } else {
            cog::dump_hir(os, *hir);
        }
    }

    std::optional<cog::MirProgram> mir{};
    if (!session.has_errors() && hir) {
        mir = cog::lower_mir(session, *hir);
    }

    if (!session.has_errors() && mir && emit_mir) {
        std::ofstream os{std::string(*emit_mir)};
        if (!os) {
            session.diags.push_back(cog::Diagnostic{
                .severity = cog::Severity::Error,
                .span = cog::Span{},
                .message = "failed to open output file for --emit-mir",
            });
        } else {
            cog::dump_mir(os, *mir);
        }
    }

    if (!session.has_errors() && mir && emit_mir_after_pass &&
        emit_mir_after_out) {
        if (*emit_mir_after_pass != "lower") {
            session.diags.push_back(cog::Diagnostic{
                .severity = cog::Severity::Error,
                .span = cog::Span{},
                .message = "unsupported --emit-mir-after pass `" +
                           std::string(*emit_mir_after_pass) +
                           "` (supported: lower)",
            });
        } else {
            std::ofstream os{std::string(*emit_mir_after_out)};
            if (!os) {
                session.diags.push_back(cog::Diagnostic{
                    .severity = cog::Severity::Error,
                    .span = cog::Span{},
                    .message =
                        "failed to open output file for --emit-mir-after",
                });
            } else {
                cog::dump_mir(os, *mir);
            }
        }
    }

    std::optional<cog::MirInterpreter> mir_eval{};
    if (!session.has_errors() && hir && mir) {
        mir_eval.emplace(session, *mir);
        for (const cog::HirDef& d : hir->defs) {
            if (d.kind == cog::HirDefKind::Const)
                (void)mir_eval->eval_const(d.id);
            else if (d.kind == cog::HirDefKind::Static)
                (void)mir_eval->eval_static(d.id);
        }
    }

    if (!session.has_errors() && hir && mir && target &&
        (emit_llvm || emit_bc || emit_obj || emit_exe)) {
        cog::LlvmEmitOptions opts{};
        if (emit_llvm)
            opts.out_ll = std::filesystem::path(std::string(*emit_llvm));
        if (emit_bc) opts.out_bc = std::filesystem::path(std::string(*emit_bc));
        if (emit_obj)
            opts.out_obj = std::filesystem::path(std::string(*emit_obj));
        if (emit_exe)
            opts.out_exe = std::filesystem::path(std::string(*emit_exe));
        opts.emit_main_wrapper = emit_exe.has_value();
        if (mir_eval) opts.mir_eval = &*mir_eval;
        (void)cog::llvm_backend_emit(session, *hir, *mir, *target, opts);
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
