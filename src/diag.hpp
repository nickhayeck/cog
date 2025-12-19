#pragma once

#include "source.hpp"
#include "span.hpp"

#include <string>

namespace cog {

enum class Severity : std::uint8_t { Error, Warning, Note };

struct Diagnostic {
  Severity severity = Severity::Error;
  Span span{};
  std::string message{};
};

std::string format_diagnostic(const SourceManager& sm, const Diagnostic& d);

}  // namespace cog

