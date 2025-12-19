#pragma once

#include "parse_state.hpp"

#include <iosfwd>

namespace cog {

ParseState parse_file(FileId file, const char* path);
void dump_tokens(FileId file, const char* path, std::ostream& os);

}  // namespace cog
