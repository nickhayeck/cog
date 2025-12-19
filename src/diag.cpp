#include "diag.hpp"

#include <sstream>

namespace cog {

static const char* severity_name(Severity s) {
  switch (s) {
    case Severity::Error:
      return "error";
    case Severity::Warning:
      return "warning";
    case Severity::Note:
      return "note";
  }
  return "error";
}

std::string format_diagnostic(const SourceManager& sm, const Diagnostic& d) {
  std::ostringstream out;
  out << sm.path(d.span.file) << ":" << d.span.begin.line << ":" << d.span.begin.column << ": "
      << severity_name(d.severity) << ": " << d.message;
  return out.str();
}

}  // namespace cog

