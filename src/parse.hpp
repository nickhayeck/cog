#pragma once

#include <iosfwd>

#include "parse_state.hpp"

namespace cog {

ParseState parse_file(FileId file, const char* path);
void dump_tokens(FileId file, const char* path, std::ostream& os);

}  // namespace cog
