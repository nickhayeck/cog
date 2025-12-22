#pragma once

#include "check.hpp"
#include "resolve.hpp"
#include "session.hpp"

#include <filesystem>
#include <optional>

namespace cog {

struct EmitLlvmOptions {
  std::filesystem::path out_ll{};
  bool emit_main_wrapper = true;
};

bool emit_llvm_ir(Session& session, const ResolvedCrate& crate, CheckedCrate& checked, const EmitLlvmOptions& opts);

}  // namespace cog
