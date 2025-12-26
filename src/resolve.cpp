#include "resolve.hpp"

#include <filesystem>
#include <limits>
#include <optional>
#include <string_view>

#include "diag.hpp"

namespace cog {
namespace {

enum class ResolvedKind : std::uint8_t { Module, Type, Value };

struct Resolved {
    ResolvedKind kind{};
    ModuleId module = 0;
    Item* item = nullptr;
};

class Resolver {
   public:
    explicit Resolver(Session& session) : session_(session) {}

    ResolvedCrate run(FileId root_file) {
        ParseState* root_parsed = session_.parse(root_file);
        if (!root_parsed || !root_parsed->root) return std::move(crate_);

        std::filesystem::path root_path =
            std::filesystem::path(session_.sources.path(root_file));
        std::filesystem::path root_dir = root_path.parent_path();

        crate_.root = add_module("<crate>", kNoParent, root_file, root_dir,
                                 root_parsed->root->items);
        load_submodules(crate_.root);
        inject_builtin_typeinfo(root_file);

        for (ModuleId id = 0; id < crate_.modules.size(); id++)
            collect_defs(id);
        resolve_all_uses();
        collect_all_impls();
        return std::move(crate_);
    }

   private:
    static constexpr ModuleId kNoParent = std::numeric_limits<ModuleId>::max();

    Session& session_;
    ResolvedCrate crate_{};

    void inject_builtin_typeinfo(FileId root_file) {
        // v0.0.16: provide `TypeInfo` for `builtin::type_info(type)`.
        // This is intentionally small and unstable; it exists primarily for
        // comptime reflection.
        //
        // We inject it into the crate root so it participates in normal name
        // resolution, layout, and field access without needing bespoke typing
        // rules.
        Span sp{};
        sp.file = root_file;

        auto mk_ident = [&](std::string text) {
            return crate_.builtins_arena.make<Ident>(sp, std::move(text));
        };
        auto mk_path1 = [&](std::string text) {
            std::vector<Ident*> segs{};
            segs.push_back(mk_ident(std::move(text)));
            return crate_.builtins_arena.make<Path>(sp, std::move(segs));
        };
        auto mk_ty_path1 = [&](std::string text) {
            return crate_.builtins_arena.make<TypePath>(
                sp, mk_path1(std::move(text)));
        };

        Type* u32_ty = mk_ty_path1("u32");
        Type* usize_ty = mk_ty_path1("usize");

        std::vector<FieldDecl*> fields{};
        fields.push_back(crate_.builtins_arena.make<FieldDecl>(
            sp, std::vector<Attr*>{}, Visibility::Pub, "kind", u32_ty));
        fields.push_back(crate_.builtins_arena.make<FieldDecl>(
            sp, std::vector<Attr*>{}, Visibility::Pub, "size", usize_ty));
        fields.push_back(crate_.builtins_arena.make<FieldDecl>(
            sp, std::vector<Attr*>{}, Visibility::Pub, "align", usize_ty));

        ItemStruct* typeinfo = crate_.builtins_arena.make<ItemStruct>(
            sp, std::vector<Attr*>{}, Visibility::Pub, "TypeInfo",
            std::move(fields));

        crate_.modules[crate_.root].items.push_back(typeinfo);
    }

    void error(Span span, std::string message) {
        session_.diags.push_back(Diagnostic{.severity = Severity::Error,
                                            .span = span,
                                            .message = std::move(message)});
    }

    ModuleId add_module(std::string name, ModuleId parent, FileId file,
                        std::filesystem::path dir, std::vector<Item*> items) {
        ModuleId id = static_cast<ModuleId>(crate_.modules.size());
        crate_.modules.push_back(Module{
            .id = id,
            .parent = parent,
            .name = std::move(name),
            .file = file,
            .dir = std::move(dir),
            .items = std::move(items),
        });
        return id;
    }

    std::optional<std::filesystem::path> find_mod_file(const Module& parent,
                                                       std::string_view name,
                                                       Span span) {
        std::filesystem::path a = parent.dir / (std::string(name) + ".cg");
        std::filesystem::path b = parent.dir / std::string(name) / "mod.cg";

        bool a_exists = std::filesystem::exists(a);
        bool b_exists = std::filesystem::exists(b);
        if (a_exists && b_exists) {
            error(span, "ambiguous module file for `mod " + std::string(name) +
                            ";` (found both `" + a.string() + "` and `" +
                            b.string() + "`)");
            return std::nullopt;
        }
        if (a_exists) return a;
        if (b_exists) return b;

        error(span, "could not find module file for `mod " + std::string(name) +
                        ";` (tried `" + a.string() + "` and `" + b.string() +
                        "`)");
        return std::nullopt;
    }

    void load_submodules(ModuleId parent_id) {
        std::filesystem::path parent_dir = crate_.modules[parent_id].dir;
        std::vector<Item*> parent_items = crate_.modules[parent_id].items;

        for (Item* item : parent_items) {
            if (!item) continue;
            switch (item->kind) {
                case AstNodeKind::ItemModInline: {
                    auto* mod = static_cast<ItemModInline*>(item);
                    if (crate_.modules[parent_id].submodules.contains(
                            mod->name)) {
                        error(mod->span,
                              "duplicate module `" + mod->name + "`");
                        break;
                    }
                    std::filesystem::path child_dir = parent_dir / mod->name;
                    ModuleId child_id =
                        add_module(mod->name, parent_id, mod->span.file,
                                   child_dir, mod->items);
                    crate_.modules[parent_id].submodules.insert(
                        {mod->name, child_id});
                    load_submodules(child_id);
                    break;
                }
                case AstNodeKind::ItemModDecl: {
                    auto* mod = static_cast<ItemModDecl*>(item);
                    if (crate_.modules[parent_id].submodules.contains(
                            mod->name)) {
                        error(mod->span,
                              "duplicate module `" + mod->name + "`");
                        break;
                    }

                    auto file_path_opt = find_mod_file(
                        crate_.modules[parent_id], mod->name, mod->span);
                    if (!file_path_opt) break;

                    FileId file = session_.add_file(*file_path_opt);
                    ParseState* parsed = session_.parse(file);
                    if (!parsed || !parsed->root) break;

                    std::filesystem::path child_dir = parent_dir / mod->name;
                    ModuleId child_id =
                        add_module(mod->name, parent_id, file, child_dir,
                                   parsed->root->items);
                    crate_.modules[parent_id].submodules.insert(
                        {mod->name, child_id});
                    load_submodules(child_id);
                    break;
                }
                default:
                    break;
            }
        }
    }

    static bool is_type_item(const Item* item) {
        if (!item) return false;
        switch (item->kind) {
            case AstNodeKind::ItemStruct:
            case AstNodeKind::ItemEnum:
            case AstNodeKind::ItemTypeAlias:
                return true;
            default:
                return false;
        }
    }

    static bool is_value_item(const Item* item) {
        if (!item) return false;
        switch (item->kind) {
            case AstNodeKind::ItemFn:
            case AstNodeKind::ItemConst:
            case AstNodeKind::ItemStatic:
                return true;
            default:
                return false;
        }
    }

    static std::string item_name(const Item* item) {
        switch (item->kind) {
            case AstNodeKind::ItemStruct:
                return static_cast<const ItemStruct*>(item)->name;
            case AstNodeKind::ItemEnum:
                return static_cast<const ItemEnum*>(item)->name;
            case AstNodeKind::ItemTypeAlias:
                return static_cast<const ItemTypeAlias*>(item)->name;
            case AstNodeKind::ItemConst:
                return static_cast<const ItemConst*>(item)->name;
            case AstNodeKind::ItemStatic:
                return static_cast<const ItemStatic*>(item)->name;
            case AstNodeKind::ItemFn:
                return static_cast<const ItemFn*>(item)->decl->name;
            default:
                return {};
        }
    }

    void collect_defs(ModuleId module_id) {
        Module& m = crate_.modules[module_id];
        m.uses.clear();
        m.types.clear();
        m.values.clear();

        for (Item* item : m.items) {
            if (!item) continue;
            if (item->kind == AstNodeKind::ItemUse) {
                m.uses.push_back(static_cast<ItemUse*>(item));
                continue;
            }

            if (is_type_item(item)) {
                std::string name = item_name(item);
                if (name.empty()) continue;
                if (m.types.contains(name)) {
                    error(item->span, "duplicate type name `" + name +
                                          "` in module `" + m.name + "`");
                    continue;
                }
                m.types.insert({name, item});
                continue;
            }

            if (is_value_item(item)) {
                std::string name = item_name(item);
                if (name.empty()) continue;
                if (m.values.contains(name)) {
                    error(item->span, "duplicate value name `" + name +
                                          "` in module `" + m.name + "`");
                    continue;
                }
                m.values.insert({name, item});
                continue;
            }
        }
    }

    std::optional<ModuleId> resolve_module_in(ModuleId module_id,
                                              std::string_view name) {
        const Module& m = crate_.modules[module_id];
        auto it = m.submodules.find(std::string(name));
        if (it == m.submodules.end()) return std::nullopt;
        return it->second;
    }

    Item* resolve_type_in(ModuleId module_id, std::string_view name) {
        const Module& m = crate_.modules[module_id];
        if (auto it = m.types.find(std::string(name)); it != m.types.end())
            return it->second;
        return nullptr;
    }

    Item* resolve_value_in(ModuleId module_id, std::string_view name) {
        const Module& m = crate_.modules[module_id];
        if (auto it = m.values.find(std::string(name)); it != m.values.end())
            return it->second;
        return nullptr;
    }

    std::optional<ModuleId> resolve_module_path(ModuleId from,
                                                const Path* path) {
        if (!path || path->segments.empty()) return std::nullopt;

        ModuleId cur = from;
        for (const Ident* seg : path->segments) {
            std::string_view name = seg->text;
            auto next = resolve_module_in(cur, name);
            if (!next) {
                error(seg->span, "cannot find module `" + std::string(name) +
                                     "` in module `" +
                                     crate_.modules[cur].name + "`");
                return std::nullopt;
            }
            cur = *next;
        }
        return cur;
    }

    std::optional<Resolved> resolve_path_any(ModuleId from, const Path* path) {
        if (!path || path->segments.empty()) return std::nullopt;

        ModuleId cur = from;
        for (size_t i = 0; i + 1 < path->segments.size(); i++) {
            const Ident* seg = path->segments[i];
            auto next = resolve_module_in(cur, seg->text);
            if (!next) {
                error(seg->span, "cannot find module `" + seg->text +
                                     "` in module `" +
                                     crate_.modules[cur].name + "`");
                return std::nullopt;
            }
            cur = *next;
        }

        const Ident* last = path->segments.back();
        std::string_view name = last->text;

        if (auto mod = resolve_module_in(cur, name))
            return Resolved{.kind = ResolvedKind::Module, .module = *mod};
        if (Item* ty = resolve_type_in(cur, name))
            return Resolved{.kind = ResolvedKind::Type, .item = ty};
        if (Item* val = resolve_value_in(cur, name))
            return Resolved{.kind = ResolvedKind::Value, .item = val};

        error(last->span, "cannot find item `" + std::string(name) +
                              "` in module `" + crate_.modules[cur].name + "`");
        return std::nullopt;
    }

    bool add_import(ModuleId into, std::string name, const Resolved& r,
                    Span span) {
        Module& m = crate_.modules[into];
        switch (r.kind) {
            case ResolvedKind::Module:
                if (auto it = m.submodules.find(name);
                    it != m.submodules.end()) {
                    if (it->second == r.module) return true;
                    error(span, "duplicate module binding `" + name +
                                    "` in module `" + m.name + "`");
                    return false;
                }
                m.submodules.insert({std::move(name), r.module});
                return true;
            case ResolvedKind::Type:
                if (auto it = m.types.find(name); it != m.types.end()) {
                    if (it->second == r.item) return true;
                    error(span, "duplicate type binding `" + name +
                                    "` in module `" + m.name + "`");
                    return false;
                }
                m.types.insert({std::move(name), r.item});
                return true;
            case ResolvedKind::Value:
                if (auto it = m.values.find(name); it != m.values.end()) {
                    if (it->second == r.item) return true;
                    error(span, "duplicate value binding `" + name +
                                    "` in module `" + m.name + "`");
                    return false;
                }
                m.values.insert({std::move(name), r.item});
                return true;
        }
        return false;
    }

    void resolve_use_tree(ModuleId into, ModuleId base, const UseTree* tree) {
        if (!tree || !tree->path) return;

        if (!tree->group.empty()) {
            auto mod_id = resolve_module_path(base, tree->path);
            if (!mod_id) return;
            for (const UseTree* child : tree->group)
                resolve_use_tree(into, *mod_id, child);
            return;
        }

        auto resolved = resolve_path_any(base, tree->path);
        if (!resolved) return;

        std::string import_name = tree->alias.empty()
                                      ? tree->path->segments.back()->text
                                      : tree->alias;
        add_import(into, std::move(import_name), *resolved, tree->span);
    }

    void resolve_all_uses() {
        for (ModuleId mid = 0; mid < crate_.modules.size(); mid++) {
            Module& m = crate_.modules[mid];
            for (const ItemUse* u : m.uses) {
                if (!u || !u->tree) continue;
                resolve_use_tree(mid, mid, u->tree);
            }
        }
    }

    Item* resolve_type_path(ModuleId from, const Path* path) {
        if (!path || path->segments.empty()) return nullptr;
        ModuleId cur = from;
        for (size_t i = 0; i + 1 < path->segments.size(); i++) {
            const Ident* seg = path->segments[i];
            auto next = resolve_module_in(cur, seg->text);
            if (!next) {
                error(seg->span, "cannot find module `" + seg->text +
                                     "` in module `" +
                                     crate_.modules[cur].name + "`");
                return nullptr;
            }
            cur = *next;
        }
        const Ident* last = path->segments.back();
        Item* ty = resolve_type_in(cur, last->text);
        if (!ty) {
            error(last->span, "cannot find type `" + last->text +
                                  "` in module `" + crate_.modules[cur].name +
                                  "`");
            return nullptr;
        }
        return ty;
    }

    void index_inherent_impl(ModuleId module_id, ItemImplInherent* impl) {
        if (!impl) return;
        Item* ty = resolve_type_path(module_id, impl->type_name);
        if (!ty) return;

        auto& table = crate_.inherent_methods[ty];
        for (ItemFn* m : impl->methods) {
            if (!m || !m->decl) continue;
            std::string name = m->decl->name;
            if (table.contains(name)) {
                error(m->span, "duplicate inherent method `" + name + "`");
                continue;
            }
            table.insert({name, m});
        }
    }

    void collect_all_impls() {
        for (ModuleId mid = 0; mid < crate_.modules.size(); mid++) {
            Module& m = crate_.modules[mid];
            for (Item* item : m.items) {
                if (!item) continue;
                switch (item->kind) {
                    case AstNodeKind::ItemImplInherent:
                        index_inherent_impl(
                            mid, static_cast<ItemImplInherent*>(item));
                        break;
                    default:
                        break;
                }
            }
        }
    }
};

}  // namespace

ResolvedCrate resolve_crate(Session& session, FileId root_file) {
    return Resolver(session).run(root_file);
}

}  // namespace cog
