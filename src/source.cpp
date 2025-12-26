#include "source.hpp"

namespace cog {

FileId SourceManager::add_file(std::string path) {
    if (auto it = by_path_.find(path); it != by_path_.end()) return it->second;

    FileId id = static_cast<FileId>(files_.size());
    files_.push_back(SourceFile{.id = id, .path = std::move(path)});
    by_path_.insert({files_.back().path, id});
    return id;
}

std::optional<FileId> SourceManager::find_file(std::string_view path) const {
    if (auto it = by_path_.find(std::string(path)); it != by_path_.end())
        return it->second;
    return std::nullopt;
}

const SourceFile& SourceManager::file(FileId id) const {
    return files_.at(static_cast<size_t>(id));
}

const std::string& SourceManager::path(FileId id) const {
    return file(id).path;
}

}  // namespace cog
