#pragma once

#include "ast.hpp"
#include "session.hpp"

#include <cstdint>
#include <filesystem>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace cog {

using ModuleId = std::uint32_t;

struct TraitImplKey {
  const ItemTrait* trait = nullptr;
  const Item* type = nullptr;
};

inline bool operator==(const TraitImplKey& a, const TraitImplKey& b) {
  return a.trait == b.trait && a.type == b.type;
}

struct TraitImplKeyHash {
  size_t operator()(const TraitImplKey& k) const noexcept {
    size_t h1 = std::hash<const void*>{}(k.trait);
    size_t h2 = std::hash<const void*>{}(k.type);
    return h1 ^ (h2 + 0x9e3779b97f4a7c15ULL + (h1 << 6) + (h1 >> 2));
  }
};

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

  std::unordered_map<const Item*, std::unordered_map<std::string, ItemFn*>> inherent_methods{};
  std::unordered_map<TraitImplKey, std::unordered_map<std::string, ItemFn*>, TraitImplKeyHash> trait_impl_methods{};
};

ResolvedCrate resolve_crate(Session& session, FileId root_file);

}  // namespace cog

