#pragma once

#include <cstdint>
#include <filesystem>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include "ast.hpp"
#include "session.hpp"

namespace cog {

using ModuleId = std::uint32_t;

struct Module {
    ModuleId id = 0;
    ModuleId parent = 0;
    std::string name{};
    FileId file = 0;
    std::filesystem::path dir{};

    std::vector<Item*> items{};

    std::unordered_map<std::string, ModuleId> submodules{};
    std::unordered_map<std::string, Item*> types{};
    std::unordered_map<std::string, Item*> values{};
    std::vector<ItemUse*> uses{};
};

struct ResolvedCrate {
    ModuleId root = 0;
    std::vector<Module> modules{};

    std::unordered_map<const Item*, std::unordered_map<std::string, ItemFn*>>
        inherent_methods{};

    // Storage for compiler-injected AST nodes (builtins, synthetic items).
    // These nodes are treated like normal AST items by later passes.
    mutable AstArena builtins_arena{};
};

ResolvedCrate resolve_crate(Session& session, FileId root_file);

}  // namespace cog
