#pragma once

#include <filesystem>
#include <optional>

#include "check.hpp"
#include "resolve.hpp"
#include "session.hpp"

namespace cog {

struct TargetSpec;

struct EmitLlvmOptions {
    const TargetSpec* target = nullptr;

    std::optional<std::filesystem::path> out_ll{};
    std::optional<std::filesystem::path> out_bc{};
    std::optional<std::filesystem::path> out_obj{};
    std::optional<std::filesystem::path> out_exe{};
    bool emit_main_wrapper = true;
};

bool emit_llvm(Session& session, const ResolvedCrate& crate,
               CheckedCrate& checked, const EmitLlvmOptions& opts);

}  // namespace cog
