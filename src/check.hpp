#pragma once

#include "resolve.hpp"
#include "session.hpp"

namespace cog {

bool check_crate(Session& session, const ResolvedCrate& crate);

}  // namespace cog

