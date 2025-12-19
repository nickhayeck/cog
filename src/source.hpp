#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

namespace cog {

using FileId = std::uint32_t;

struct SourceFile {
  FileId id = 0;
  std::string path{};
};

class SourceManager {
 public:
  FileId add_file(std::string path);
  std::optional<FileId> find_file(std::string_view path) const;

  const SourceFile& file(FileId id) const;
  const std::string& path(FileId id) const;

 private:
  std::vector<SourceFile> files_{};
  std::unordered_map<std::string, FileId> by_path_{};
};

}  // namespace cog

