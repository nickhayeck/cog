#pragma once

#include <optional>

#include "hir.hpp"
#include "mir.hpp"
#include "session.hpp"

namespace cog {

// Lowers a checked crate to MIR.
//
// v0.0.20: MIR is the shared semantics representation for:
// - comptime interpretation (const/static + comptime blocks/params)
// - runtime codegen (lowered to LLVM)
std::optional<MirProgram> lower_mir(Session& session, const HirCrate& hir);

}  // namespace cog
