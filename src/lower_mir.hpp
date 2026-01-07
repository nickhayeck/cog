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

// Lowers a single function body to MIR, producing a tiny program containing
// only that body (plus any internal const bodies created during lowering).
//
// This is used by the checker for MIR-interpreted comptime evaluation in
// contexts where we do not yet have a full crate MIR program.
std::optional<MirProgram> lower_mir_for_fn(Session& session, const HirCrate& hir,
                                           DefId fn_def);

}  // namespace cog
