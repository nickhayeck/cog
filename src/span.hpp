#pragma once

#include "source.hpp"

#include <cstdint>

namespace cog {

struct SourceLoc {
  std::uint32_t line = 1;
  std::uint32_t column = 1;
};

struct Span {
  FileId file = 0;
  SourceLoc begin{};
  SourceLoc end{};
};

}  // namespace cog

