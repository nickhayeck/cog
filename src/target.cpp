#include "target.hpp"

#include <llvm/ADT/StringMap.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/TargetParser/SubtargetFeature.h>
#include <llvm/TargetParser/Triple.h>

#include <optional>
#include <string>
#include <string_view>

#include "session.hpp"

namespace cog {
namespace {

// Target detection + layout.
//
// We use LLVM as the source of truth for pointer size/alignment and integer ABI
// alignment so that comptime `size_of` / `align_of` and codegen agree with the
// selected target triple.
static void ensure_llvm_target_init() {
    static bool done = false;
    if (done) return;
    done = true;
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();
}

static std::string host_features_string() {
    const auto features = llvm::sys::getHostCPUFeatures();
    llvm::SubtargetFeatures out{};
    for (const auto& f : features) out.AddFeature(f.getKey(), f.getValue());
    return out.getString();
}

static std::optional<TargetLayout> compute_layout_from_datalayout(
    const llvm::DataLayout& dl) {
    TargetLayout out{};
    out.pointer_bits = dl.getPointerSizeInBits(0);
    out.pointer_size = dl.getPointerSize(0);
    out.pointer_align = dl.getPointerABIAlignment(0).value();

    llvm::LLVMContext ctx{};
    out.i8_align = dl.getABITypeAlign(llvm::IntegerType::get(ctx, 8)).value();
    out.i16_align = dl.getABITypeAlign(llvm::IntegerType::get(ctx, 16)).value();
    out.i32_align = dl.getABITypeAlign(llvm::IntegerType::get(ctx, 32)).value();
    out.i64_align = dl.getABITypeAlign(llvm::IntegerType::get(ctx, 64)).value();
    out.i128_align =
        dl.getABITypeAlign(llvm::IntegerType::get(ctx, 128)).value();
    return out;
}

}  // namespace

std::optional<TargetSpec> compute_target_spec(
    Session& session, std::optional<std::string_view> triple_arg) {
    ensure_llvm_target_init();

    // Prefer the process triple (what can run on this machine) over LLVM's
    // configured default triple (which may reflect the LLVM build host).
    std::string triple =
        triple_arg ? std::string(*triple_arg) : llvm::sys::getProcessTriple();

    std::string cpu = "generic";
    std::string features{};
    const std::string host_triple = llvm::sys::getProcessTriple();
    if (!triple_arg || triple == host_triple) {
        cpu = std::string(llvm::sys::getHostCPUName());
        features = host_features_string();
    }

    std::string error{};
    const llvm::Target* target =
        llvm::TargetRegistry::lookupTarget(triple, error);
    if (!target) {
        session.diags.push_back(Diagnostic{
            .severity = Severity::Error,
            .span = Span{},
            .message =
                "LLVM target lookup failed for `" + triple + "`: " + error,
        });
        return std::nullopt;
    }

    llvm::TargetOptions opt{};
    auto reloc = std::optional<llvm::Reloc::Model>{};
    llvm::Triple tt(triple);
    auto tm = std::unique_ptr<llvm::TargetMachine>(
        target->createTargetMachine(tt, cpu, features, opt, reloc));
    if (!tm) {
        session.diags.push_back(Diagnostic{
            .severity = Severity::Error,
            .span = Span{},
            .message =
                "failed to create LLVM TargetMachine for `" + triple + "`",
        });
        return std::nullopt;
    }

    llvm::DataLayout dl = tm->createDataLayout();
    auto layout = compute_layout_from_datalayout(dl);
    if (!layout) return std::nullopt;

    TargetSpec out{};
    out.triple = std::move(triple);
    out.cpu = std::move(cpu);
    out.features = std::move(features);
    out.data_layout = dl.getStringRepresentation();
    out.layout = *layout;
    return out;
}

}  // namespace cog
