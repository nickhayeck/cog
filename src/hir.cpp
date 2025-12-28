#include "hir.hpp"

#include <ostream>
#include <string_view>
#include <utility>

namespace cog {
namespace {

static std::string item_name(const Item* item) {
    if (!item) return "<null>";
    switch (item->kind) {
        case AstNodeKind::ItemFn: {
            auto* fn = static_cast<const ItemFn*>(item);
            return fn->decl ? fn->decl->name : "<fn>";
        }
        case AstNodeKind::ItemStruct:
            return static_cast<const ItemStruct*>(item)->name;
        case AstNodeKind::ItemEnum:
            return static_cast<const ItemEnum*>(item)->name;
        case AstNodeKind::ItemConst:
            return static_cast<const ItemConst*>(item)->name;
        case AstNodeKind::ItemStatic:
            return static_cast<const ItemStatic*>(item)->name;
        case AstNodeKind::ItemTypeAlias:
            return static_cast<const ItemTypeAlias*>(item)->name;
        default:
            break;
    }
    return "<item>";
}

static void collect_expr_ids(HirCrate& out, const Expr* e) {
    if (!e) return;
    if (!out.expr_ids.contains(e)) {
        ExprId id = static_cast<ExprId>(out.exprs_by_id.size());
        out.expr_ids.insert({e, id});
        out.exprs_by_id.push_back(e);
    }

    switch (e->kind) {
        case AstNodeKind::ExprBlock: {
            auto* b = static_cast<const ExprBlock*>(e);
            if (!b->block) break;
            for (const Stmt* s : b->block->stmts) {
                if (!s) continue;
                switch (s->kind) {
                    case AstNodeKind::StmtLet: {
                        auto* ls = static_cast<const StmtLet*>(s);
                        collect_expr_ids(out, ls->init);
                        break;
                    }
                    case AstNodeKind::StmtExpr: {
                        auto* es = static_cast<const StmtExpr*>(s);
                        collect_expr_ids(out, es->expr);
                        break;
                    }
                    case AstNodeKind::StmtReturn: {
                        auto* rs = static_cast<const StmtReturn*>(s);
                        collect_expr_ids(out, rs->value);
                        break;
                    }
                    case AstNodeKind::StmtBreak: {
                        auto* bs = static_cast<const StmtBreak*>(s);
                        collect_expr_ids(out, bs->value);
                        break;
                    }
                    default:
                        break;
                }
            }
            collect_expr_ids(out, b->block->tail);
            break;
        }
        case AstNodeKind::ExprComptime: {
            auto* c = static_cast<const ExprComptime*>(e);
            if (!c->block) break;
            for (const Stmt* s : c->block->stmts) {
                if (!s) continue;
                if (s->kind == AstNodeKind::StmtExpr)
                    collect_expr_ids(out,
                                     static_cast<const StmtExpr*>(s)->expr);
                if (s->kind == AstNodeKind::StmtLet)
                    collect_expr_ids(out, static_cast<const StmtLet*>(s)->init);
            }
            collect_expr_ids(out, c->block->tail);
            break;
        }
        case AstNodeKind::ExprIf: {
            auto* i = static_cast<const ExprIf*>(e);
            collect_expr_ids(out, i->cond);
            if (i->then_block) collect_expr_ids(out, i->then_block->tail);
            collect_expr_ids(out, i->else_expr);
            break;
        }
        case AstNodeKind::ExprWhile: {
            auto* w = static_cast<const ExprWhile*>(e);
            collect_expr_ids(out, w->cond);
            if (w->body) collect_expr_ids(out, w->body->tail);
            break;
        }
        case AstNodeKind::ExprLoop: {
            auto* l = static_cast<const ExprLoop*>(e);
            if (l->body) collect_expr_ids(out, l->body->tail);
            break;
        }
        case AstNodeKind::ExprMatch: {
            auto* m = static_cast<const ExprMatch*>(e);
            collect_expr_ids(out, m->scrutinee);
            for (const MatchArm* a : m->arms) {
                if (!a) continue;
                collect_expr_ids(out, a->guard);
                collect_expr_ids(out, a->body);
            }
            break;
        }
        case AstNodeKind::ExprCall: {
            auto* c = static_cast<const ExprCall*>(e);
            collect_expr_ids(out, c->callee);
            for (const Expr* a : c->args) collect_expr_ids(out, a);
            break;
        }
        case AstNodeKind::ExprMethodCall: {
            auto* mc = static_cast<const ExprMethodCall*>(e);
            collect_expr_ids(out, mc->receiver);
            for (const Expr* a : mc->args) collect_expr_ids(out, a);
            break;
        }
        case AstNodeKind::ExprField:
            collect_expr_ids(out, static_cast<const ExprField*>(e)->base);
            break;
        case AstNodeKind::ExprIndex: {
            auto* ix = static_cast<const ExprIndex*>(e);
            collect_expr_ids(out, ix->base);
            collect_expr_ids(out, ix->index);
            break;
        }
        case AstNodeKind::ExprCast:
            collect_expr_ids(out, static_cast<const ExprCast*>(e)->value);
            break;
        case AstNodeKind::ExprUnary:
            collect_expr_ids(out, static_cast<const ExprUnary*>(e)->expr);
            break;
        case AstNodeKind::ExprBinary: {
            auto* b = static_cast<const ExprBinary*>(e);
            collect_expr_ids(out, b->lhs);
            collect_expr_ids(out, b->rhs);
            break;
        }
        case AstNodeKind::ExprAssign: {
            auto* a = static_cast<const ExprAssign*>(e);
            collect_expr_ids(out, a->lhs);
            collect_expr_ids(out, a->rhs);
            break;
        }
        case AstNodeKind::ExprStructLit: {
            auto* s = static_cast<const ExprStructLit*>(e);
            for (const FieldInit* f : s->inits) {
                if (f) collect_expr_ids(out, f->value);
            }
            break;
        }
        case AstNodeKind::ExprTuple: {
            auto* t = static_cast<const ExprTuple*>(e);
            for (const Expr* el : t->elems) collect_expr_ids(out, el);
            break;
        }
        case AstNodeKind::ExprArrayLit: {
            auto* a = static_cast<const ExprArrayLit*>(e);
            for (const Expr* el : a->elems) collect_expr_ids(out, el);
            break;
        }
        case AstNodeKind::ExprArrayRepeat: {
            auto* r = static_cast<const ExprArrayRepeat*>(e);
            collect_expr_ids(out, r->elem);
            collect_expr_ids(out, r->count);
            break;
        }
        default:
            break;
    }
}

}  // namespace

const HirDef* HirCrate::def(DefId id) const {
    if (id >= defs.size()) return nullptr;
    return &defs[static_cast<size_t>(id)];
}

const HirBody* HirCrate::body(BodyId id) const {
    if (id >= bodies.size()) return nullptr;
    return &bodies[static_cast<size_t>(id)];
}

HirCrate build_hir(const ResolvedCrate& crate, CheckedCrate& checked) {
    HirCrate out{};
    out.crate = &crate;
    out.checked = &checked;

    auto add_def = [&](HirDefKind kind, ModuleId mid,
                       const Item* item) -> DefId {
        DefId id = static_cast<DefId>(out.defs.size());
        HirDef d{};
        d.kind = kind;
        d.id = id;
        d.module = mid;
        d.ast = item;
        d.name = item_name(item);
        out.def_ids.insert({item, id});
        out.defs.push_back(std::move(d));
        return id;
    };

    auto add_body = [&](ModuleId mid, const Item* owner, const Block* fn_block,
                        const Expr* expr) -> BodyId {
        BodyId id = static_cast<BodyId>(out.bodies.size());
        out.bodies.push_back(HirBody{.id = id,
                                     .module = mid,
                                     .owner = owner,
                                     .fn_block = fn_block,
                                     .expr = expr});
        return id;
    };

    for (ModuleId mid = 0; mid < crate.modules.size(); mid++) {
        for (const Item* item : crate.modules[mid].items) {
            if (!item) continue;
            switch (item->kind) {
                case AstNodeKind::ItemFn: {
                    auto* fn = static_cast<const ItemFn*>(item);
                    DefId id = add_def(HirDefKind::Fn, mid, item);
                    if (fn->body) {
                        BodyId bid = add_body(mid, item, fn->body, nullptr);
                        out.defs[id].body = bid;
                        if (fn->body->tail)
                            collect_expr_ids(out, fn->body->tail);
                        for (const Stmt* s : fn->body->stmts) {
                            if (!s) continue;
                            if (s->kind == AstNodeKind::StmtExpr)
                                collect_expr_ids(
                                    out, static_cast<const StmtExpr*>(s)->expr);
                            if (s->kind == AstNodeKind::StmtLet)
                                collect_expr_ids(
                                    out, static_cast<const StmtLet*>(s)->init);
                        }
                    }
                    break;
                }
                case AstNodeKind::ItemStruct:
                    (void)add_def(HirDefKind::Struct, mid, item);
                    break;
                case AstNodeKind::ItemEnum:
                    (void)add_def(HirDefKind::Enum, mid, item);
                    break;
                case AstNodeKind::ItemConst: {
                    auto* c = static_cast<const ItemConst*>(item);
                    DefId id = add_def(HirDefKind::Const, mid, item);
                    BodyId bid = add_body(mid, item, nullptr, c->value);
                    out.defs[id].body = bid;
                    collect_expr_ids(out, c->value);
                    break;
                }
                case AstNodeKind::ItemStatic: {
                    auto* s = static_cast<const ItemStatic*>(item);
                    DefId id = add_def(HirDefKind::Static, mid, item);
                    BodyId bid = add_body(mid, item, nullptr, s->value);
                    out.defs[id].body = bid;
                    collect_expr_ids(out, s->value);
                    break;
                }
                case AstNodeKind::ItemTypeAlias:
                    (void)add_def(HirDefKind::TypeAlias, mid, item);
                    break;
                case AstNodeKind::ItemImplInherent: {
                    auto* impl = static_cast<const ItemImplInherent*>(item);
                    for (const ItemFn* m : impl->methods) {
                        if (!m) continue;
                        DefId id = add_def(HirDefKind::Fn, mid,
                                           static_cast<const Item*>(m));
                        if (m->body) {
                            BodyId bid =
                                add_body(mid, static_cast<const Item*>(m),
                                         m->body, nullptr);
                            out.defs[id].body = bid;
                            if (m->body->tail)
                                collect_expr_ids(out, m->body->tail);
                            for (const Stmt* s : m->body->stmts) {
                                if (!s) continue;
                                if (s->kind == AstNodeKind::StmtExpr)
                                    collect_expr_ids(
                                        out,
                                        static_cast<const StmtExpr*>(s)->expr);
                                if (s->kind == AstNodeKind::StmtLet)
                                    collect_expr_ids(
                                        out,
                                        static_cast<const StmtLet*>(s)->init);
                            }
                        }
                    }
                    break;
                }
                default:
                    break;
            }
        }
    }

    return out;
}

void dump_hir(std::ostream& os, const HirCrate& hir) {
    os << "hir.crate\n";
    os << "  defs: " << hir.defs.size() << "\n";
    for (const HirDef& d : hir.defs) {
        os << "  def#" << d.id << " ";
        switch (d.kind) {
            case HirDefKind::Fn:
                os << "fn ";
                break;
            case HirDefKind::Struct:
                os << "struct ";
                break;
            case HirDefKind::Enum:
                os << "enum ";
                break;
            case HirDefKind::Const:
                os << "const ";
                break;
            case HirDefKind::Static:
                os << "static ";
                break;
            case HirDefKind::TypeAlias:
                os << "type ";
                break;
        }
        os << d.name << " (mod#" << d.module << ")";
        if (d.body) os << " body#" << *d.body;
        os << "\n";
    }

    os << "  bodies: " << hir.bodies.size() << "\n";
    os << "  exprs: " << hir.exprs_by_id.size() << "\n";
}

}  // namespace cog
