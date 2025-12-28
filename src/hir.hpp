#pragma once

#include <cstdint>
#include <iosfwd>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

#include "ast.hpp"
#include "check.hpp"
#include "resolve.hpp"

namespace cog {

using DefId = std::uint32_t;
using BodyId = std::uint32_t;
using ExprId = std::uint32_t;

enum class HirDefKind : std::uint8_t {
    Fn,
    Struct,
    Enum,
    Const,
    Static,
    TypeAlias,
};

struct HirDef {
    HirDefKind kind{};
    DefId id = 0;
    ModuleId module = 0;
    const Item* ast = nullptr;
    std::string name{};

    std::optional<BodyId> body{};
};

struct HirBody {
    BodyId id = 0;
    ModuleId module = 0;
    const Item* owner = nullptr;  // function/const/static that owns this body

    // For `fn`, this is `fn->body`. For `const`/`static`, this is the
    // initializer expression (any expression, not necessarily a block).
    const Block* fn_block = nullptr;
    const Expr* expr = nullptr;
};

struct HirCrate {
    const ResolvedCrate* crate = nullptr;
    const CheckedCrate* checked = nullptr;

    std::vector<HirDef> defs{};
    std::vector<HirBody> bodies{};

    std::unordered_map<const Item*, DefId> def_ids{};

    // Non-normative debug info: stable IDs for expressions across the crate.
    std::unordered_map<const Expr*, ExprId> expr_ids{};
    std::vector<const Expr*> exprs_by_id{};

    const HirDef* def(DefId id) const;
    const HirBody* body(BodyId id) const;
};

// Builds a HIR view of the crate. This is a lightweight wrapper around the
// existing resolved + checked AST, primarily to provide stable IDs and a clean
// handoff point to MIR lowering.
HirCrate build_hir(const ResolvedCrate& crate, const CheckedCrate& checked);

// Debug output.
void dump_hir(std::ostream& os, const HirCrate& hir);

}  // namespace cog
