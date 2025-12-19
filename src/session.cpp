#include "session.hpp"

#include <utility>

namespace cog {

std::filesystem::path normalize_path(std::filesystem::path path) {
  std::error_code ec{};
  std::filesystem::path abs = std::filesystem::absolute(path, ec);
  if (ec) abs = std::move(path);
  return abs.lexically_normal();
}

FileId Session::add_file(std::filesystem::path path) {
  std::filesystem::path normalized = normalize_path(std::move(path));
  return sources.add_file(normalized.string());
}

ParseState* Session::parse(FileId file) {
  if (file >= parsed_.size()) parsed_.resize(static_cast<size_t>(file) + 1);
  if (parsed_[file]) return parsed_[file].get();

  auto state = std::make_unique<ParseState>(parse_file(file, sources.path(file).c_str()));
  for (auto& d : state->diags) diags.push_back(std::move(d));
  state->diags.clear();
  parsed_[file] = std::move(state);
  return parsed_[file].get();
}

bool Session::has_errors() const {
  for (const auto& d : diags) {
    if (d.severity == Severity::Error) return true;
  }
  return false;
}

}  // namespace cog

