#pragma once

#include <filesystem>
#include <optional>

namespace cog {

struct HirCrate;
class MirInterpreter;
struct MirProgram;
struct Session;
struct TargetSpec;

struct LlvmEmitOptions {
    std::optional<std::filesystem::path> out_ll{};
    std::optional<std::filesystem::path> out_bc{};
    std::optional<std::filesystem::path> out_obj{};
    std::optional<std::filesystem::path> out_exe{};
    bool emit_main_wrapper = false;
    MirInterpreter* mir_eval = nullptr;  // optional externally-owned interpreter
};

bool llvm_backend_emit(Session& session, const HirCrate& hir,
                       const MirProgram& mir, const TargetSpec& target,
                       const LlvmEmitOptions& opts);

}  // namespace cog
