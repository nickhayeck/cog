#pragma once

#include <filesystem>
#include <memory>
#include <vector>

#include "diag.hpp"
#include "parse.hpp"
#include "source.hpp"

namespace cog {

std::filesystem::path normalize_path(std::filesystem::path path);

struct Session {
    SourceManager sources{};
    std::vector<Diagnostic> diags{};

    FileId add_file(std::filesystem::path path);
    ParseState* parse(FileId file);

    bool has_errors() const;

   private:
    std::vector<std::unique_ptr<ParseState>> parsed_{};
};

}  // namespace cog
