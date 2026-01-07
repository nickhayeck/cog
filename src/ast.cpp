#include "ast.hpp"

#include <utility>

namespace cog {

std::string_view ast_kind_name(AstNodeKind kind) {
    switch (kind) {
        case AstNodeKind::File:
            return "File";
        case AstNodeKind::Ident:
            return "Ident";
        case AstNodeKind::Path:
            return "Path";
        case AstNodeKind::Attr:
            return "Attr";
        case AstNodeKind::TypePath:
            return "TypePath";
        case AstNodeKind::TypeCall:
            return "TypeCall";
        case AstNodeKind::TypeAuto:
            return "TypeAuto";
        case AstNodeKind::TypePtr:
            return "TypePtr";
        case AstNodeKind::TypeSlice:
            return "TypeSlice";
        case AstNodeKind::TypeArray:
            return "TypeArray";
        case AstNodeKind::TypeTuple:
            return "TypeTuple";
        case AstNodeKind::TypeFn:
            return "TypeFn";
        case AstNodeKind::TypeUnit:
            return "TypeUnit";
        case AstNodeKind::TypeType:
            return "TypeType";
        case AstNodeKind::TypeNever:
            return "TypeNever";
        case AstNodeKind::UseTree:
            return "UseTree";
        case AstNodeKind::ItemUse:
            return "ItemUse";
        case AstNodeKind::ItemModInline:
            return "ItemModInline";
        case AstNodeKind::ItemModDecl:
            return "ItemModDecl";
        case AstNodeKind::ItemStruct:
            return "ItemStruct";
        case AstNodeKind::ItemEnum:
            return "ItemEnum";
        case AstNodeKind::ItemImplInherent:
            return "ItemImplInherent";
        case AstNodeKind::ItemFn:
            return "ItemFn";
        case AstNodeKind::ItemConst:
            return "ItemConst";
        case AstNodeKind::ItemStatic:
            return "ItemStatic";
        case AstNodeKind::ItemTypeAlias:
            return "ItemTypeAlias";
        case AstNodeKind::FieldDecl:
            return "FieldDecl";
        case AstNodeKind::VariantDecl:
            return "VariantDecl";
        case AstNodeKind::Param:
            return "Param";
        case AstNodeKind::FnSig:
            return "FnSig";
        case AstNodeKind::FnDecl:
            return "FnDecl";
        case AstNodeKind::StmtLet:
            return "StmtLet";
        case AstNodeKind::StmtExpr:
            return "StmtExpr";
        case AstNodeKind::StmtReturn:
            return "StmtReturn";
        case AstNodeKind::StmtBreak:
            return "StmtBreak";
        case AstNodeKind::StmtContinue:
            return "StmtContinue";
        case AstNodeKind::PatWildcard:
            return "PatWildcard";
        case AstNodeKind::PatInt:
            return "PatInt";
        case AstNodeKind::PatBool:
            return "PatBool";
        case AstNodeKind::PatBinding:
            return "PatBinding";
        case AstNodeKind::PatTuple:
            return "PatTuple";
        case AstNodeKind::PatStruct:
            return "PatStruct";
        case AstNodeKind::PatVariant:
            return "PatVariant";
        case AstNodeKind::PatTypeVariant:
            return "PatTypeVariant";
        case AstNodeKind::PatPath:
            return "PatPath";
        case AstNodeKind::PatOr:
            return "PatOr";
        case AstNodeKind::PatField:
            return "PatField";
        case AstNodeKind::Block:
            return "Block";
        case AstNodeKind::MatchArm:
            return "MatchArm";
        case AstNodeKind::FieldInit:
            return "FieldInit";
        case AstNodeKind::ExprInt:
            return "ExprInt";
        case AstNodeKind::ExprFloat:
            return "ExprFloat";
        case AstNodeKind::ExprBool:
            return "ExprBool";
        case AstNodeKind::ExprString:
            return "ExprString";
        case AstNodeKind::ExprUnit:
            return "ExprUnit";
        case AstNodeKind::ExprPath:
            return "ExprPath";
        case AstNodeKind::ExprTypeMember:
            return "ExprTypeMember";
        case AstNodeKind::ExprBlock:
            return "ExprBlock";
        case AstNodeKind::ExprComptime:
            return "ExprComptime";
        case AstNodeKind::ExprIf:
            return "ExprIf";
        case AstNodeKind::ExprWhile:
            return "ExprWhile";
        case AstNodeKind::ExprLoop:
            return "ExprLoop";
        case AstNodeKind::ExprMatch:
            return "ExprMatch";
        case AstNodeKind::ExprCall:
            return "ExprCall";
        case AstNodeKind::ExprMethodCall:
            return "ExprMethodCall";
        case AstNodeKind::ExprField:
            return "ExprField";
        case AstNodeKind::ExprIndex:
            return "ExprIndex";
        case AstNodeKind::ExprCast:
            return "ExprCast";
        case AstNodeKind::ExprUnary:
            return "ExprUnary";
        case AstNodeKind::ExprBinary:
            return "ExprBinary";
        case AstNodeKind::ExprAssign:
            return "ExprAssign";
        case AstNodeKind::ExprStructLit:
            return "ExprStructLit";
        case AstNodeKind::ExprTuple:
            return "ExprTuple";
        case AstNodeKind::ExprArrayLit:
            return "ExprArrayLit";
        case AstNodeKind::ExprArrayRepeat:
            return "ExprArrayRepeat";
    }
    return "Unknown";
}

static void indent_to(std::ostream& os, int indent) {
    for (int i = 0; i < indent; i++) os << "  ";
}

static void dump_text(std::ostream& os, std::string_view text) {
    os << " \"" << text << '"';
}

std::string ast_unary_op_str(UnaryOp op) {
    switch (op) {
        case UnaryOp::Neg:
            return "-";
        case UnaryOp::Not:
            return "!";
        case UnaryOp::BitNot:
            return "~";
        case UnaryOp::Deref:
            return "*";
        case UnaryOp::AddrOf:
            return "&";
        case UnaryOp::AddrOfMut:
            return "&mut";
    }
}

std::string ast_binary_op_str(BinaryOp op) {
    switch (op) {
        case BinaryOp::Add:
            return "+";
        case BinaryOp::Sub:
            return "-";
        case BinaryOp::Mul:
            return "*";
        case BinaryOp::Div:
            return "/";
        case BinaryOp::Mod:
            return "%";
        case BinaryOp::Eq:
            return "==";
        case BinaryOp::Ne:
            return "!=";
        case BinaryOp::Lt:
            return "<";
        case BinaryOp::Le:
            return "<=";
        case BinaryOp::Gt:
            return ">";
        case BinaryOp::Ge:
            return ">=";
        case BinaryOp::And:
            return "&&";
        case BinaryOp::Or:
            return "||";
        case BinaryOp::BitAnd:
            return "&";
        case BinaryOp::BitOr:
            return "|";
        case BinaryOp::BitXor:
            return "^";
        case BinaryOp::Shl:
            return "<<";
        case BinaryOp::Shr:
            return ">>";
        break;
    }
}


void dump_ast(std::ostream& os, const AstNode* node, int indent) {
    if (!node) {
        indent_to(os, indent);
        os << "<null>\n";
        return;
    }

    indent_to(os, indent);
    os << ast_kind_name(node->kind);

    switch (node->kind) {
        case AstNodeKind::Ident:
            dump_text(os, static_cast<const Ident*>(node)->text);
            break;
        case AstNodeKind::ItemModInline:
            dump_text(os, static_cast<const ItemModInline*>(node)->name);
            break;
        case AstNodeKind::ItemModDecl:
            dump_text(os, static_cast<const ItemModDecl*>(node)->name);
            break;
        case AstNodeKind::ItemStruct:
            dump_text(os, static_cast<const ItemStruct*>(node)->name);
            break;
        case AstNodeKind::ItemEnum:
            dump_text(os, static_cast<const ItemEnum*>(node)->name);
            break;
        case AstNodeKind::ItemConst:
            dump_text(os, static_cast<const ItemConst*>(node)->name);
            break;
        case AstNodeKind::ItemStatic:
            dump_text(os, static_cast<const ItemStatic*>(node)->name);
            break;
        case AstNodeKind::ItemTypeAlias:
            dump_text(os, static_cast<const ItemTypeAlias*>(node)->name);
            break;
        case AstNodeKind::FnDecl:
            dump_text(os, static_cast<const FnDecl*>(node)->name);
            break;
        case AstNodeKind::FieldDecl:
            dump_text(os, static_cast<const FieldDecl*>(node)->name);
            break;
        case AstNodeKind::VariantDecl:
            dump_text(os, static_cast<const VariantDecl*>(node)->name);
            break;
        case AstNodeKind::FieldInit:
            dump_text(os, static_cast<const FieldInit*>(node)->name);
            break;
        case AstNodeKind::PatBinding:
            dump_text(os, static_cast<const PatBinding*>(node)->name);
            break;
        case AstNodeKind::PatTypeVariant:
            dump_text(os, static_cast<const PatTypeVariant*>(node)->variant);
            break;
        case AstNodeKind::ExprTypeMember:
            dump_text(os, static_cast<const ExprTypeMember*>(node)->member);
            break;
        default:
            break;
    }

    os << '\n';

    // Children
    switch (node->kind) {
        case AstNodeKind::File: {
            auto* n = static_cast<const FileAst*>(node);
            for (const Item* it : n->items) dump_ast(os, it, indent + 1);
            return;
        }
        case AstNodeKind::Path: {
            auto* n = static_cast<const Path*>(node);
            for (const Ident* seg : n->segments) dump_ast(os, seg, indent + 1);
            return;
        }
        case AstNodeKind::Attr: {
            auto* n = static_cast<const Attr*>(node);
            dump_ast(os, n->name, indent + 1);
            if (n->arg_path) {
                dump_ast(os, n->arg_path, indent + 1);
            } else if (n->arg_string) {
                indent_to(os, indent + 1);
                os << "AttrArgString";
                dump_text(os, *n->arg_string);
                os << '\n';
            }
            return;
        }
        case AstNodeKind::TypePath: {
            dump_ast(os, static_cast<const TypePath*>(node)->path, indent + 1);
            return;
        }
        case AstNodeKind::TypeCall: {
            auto* n = static_cast<const TypeCall*>(node);
            dump_ast(os, n->callee, indent + 1);
            for (const Type* a : n->args) dump_ast(os, a, indent + 1);
            return;
        }
        case AstNodeKind::TypePtr: {
            dump_ast(os, static_cast<const TypePtr*>(node)->pointee,
                     indent + 1);
            return;
        }
        case AstNodeKind::TypeSlice: {
            dump_ast(os, static_cast<const TypeSlice*>(node)->elem, indent + 1);
            return;
        }
        case AstNodeKind::TypeArray: {
            auto* n = static_cast<const TypeArray*>(node);
            dump_ast(os, n->elem, indent + 1);
            dump_ast(os, n->len, indent + 1);
            return;
        }
        case AstNodeKind::TypeTuple: {
            auto* n = static_cast<const TypeTuple*>(node);
            for (const Type* t : n->elems) dump_ast(os, t, indent + 1);
            return;
        }
        case AstNodeKind::TypeFn: {
            auto* n = static_cast<const TypeFn*>(node);
            for (const Type* p : n->params) dump_ast(os, p, indent + 1);
            if (n->ret) dump_ast(os, n->ret, indent + 1);
            return;
        }
        case AstNodeKind::ItemUse: {
            dump_ast(os, static_cast<const ItemUse*>(node)->tree, indent + 1);
            return;
        }
        case AstNodeKind::UseTree: {
            auto* n = static_cast<const UseTree*>(node);
            dump_ast(os, n->path, indent + 1);
            for (const UseTree* child : n->group)
                dump_ast(os, child, indent + 1);
            return;
        }
        case AstNodeKind::ItemModInline: {
            auto* n = static_cast<const ItemModInline*>(node);
            for (const Item* it : n->items) dump_ast(os, it, indent + 1);
            return;
        }
        case AstNodeKind::ItemStruct: {
            auto* n = static_cast<const ItemStruct*>(node);
            for (const Attr* a : n->attrs) dump_ast(os, a, indent + 1);
            for (const FieldDecl* f : n->fields) dump_ast(os, f, indent + 1);
            return;
        }
        case AstNodeKind::ItemEnum: {
            auto* n = static_cast<const ItemEnum*>(node);
            for (const Attr* a : n->attrs) dump_ast(os, a, indent + 1);
            for (const VariantDecl* v : n->variants)
                dump_ast(os, v, indent + 1);
            return;
        }
        case AstNodeKind::ItemImplInherent: {
            auto* n = static_cast<const ItemImplInherent*>(node);
            dump_ast(os, n->type_name, indent + 1);
            for (const ItemFn* m : n->methods) dump_ast(os, m, indent + 1);
            return;
        }
        case AstNodeKind::ItemFn: {
            auto* n = static_cast<const ItemFn*>(node);
            dump_ast(os, n->decl, indent + 1);
            dump_ast(os, n->body, indent + 1);
            return;
        }
        case AstNodeKind::FnDecl: {
            auto* n = static_cast<const FnDecl*>(node);
            dump_ast(os, n->sig, indent + 1);
            return;
        }
        case AstNodeKind::FnSig: {
            auto* n = static_cast<const FnSig*>(node);
            for (const Param* p : n->params) dump_ast(os, p, indent + 1);
            if (n->ret) dump_ast(os, n->ret, indent + 1);
            return;
        }
        case AstNodeKind::Param: {
            auto* n = static_cast<const Param*>(node);
            dump_ast(os, n->type, indent + 1);
            return;
        }
        case AstNodeKind::FieldDecl: {
            auto* n = static_cast<const FieldDecl*>(node);
            for (const Attr* a : n->attrs) dump_ast(os, a, indent + 1);
            dump_ast(os, n->type, indent + 1);
            return;
        }
        case AstNodeKind::VariantDecl: {
            auto* n = static_cast<const VariantDecl*>(node);
            for (const Type* t : n->payload) dump_ast(os, t, indent + 1);
            if (n->discriminant) dump_ast(os, n->discriminant, indent + 1);
            return;
        }
        case AstNodeKind::ItemConst: {
            auto* n = static_cast<const ItemConst*>(node);
            dump_ast(os, n->type, indent + 1);
            dump_ast(os, n->value, indent + 1);
            return;
        }
        case AstNodeKind::ItemStatic: {
            auto* n = static_cast<const ItemStatic*>(node);
            dump_ast(os, n->type, indent + 1);
            dump_ast(os, n->value, indent + 1);
            return;
        }
        case AstNodeKind::ItemTypeAlias: {
            auto* n = static_cast<const ItemTypeAlias*>(node);
            dump_ast(os, n->aliased, indent + 1);
            return;
        }
        case AstNodeKind::Block: {
            auto* n = static_cast<const Block*>(node);
            for (const Stmt* s : n->stmts) dump_ast(os, s, indent + 1);
            if (n->tail) dump_ast(os, n->tail, indent + 1);
            return;
        }
        case AstNodeKind::StmtLet: {
            auto* n = static_cast<const StmtLet*>(node);
            dump_ast(os, n->pat, indent + 1);
            if (n->type_ann) dump_ast(os, n->type_ann, indent + 1);
            if (n->init) dump_ast(os, n->init, indent + 1);
            return;
        }
        case AstNodeKind::StmtExpr: {
            dump_ast(os, static_cast<const StmtExpr*>(node)->expr, indent + 1);
            return;
        }
        case AstNodeKind::StmtReturn: {
            if (auto* v = static_cast<const StmtReturn*>(node)->value)
                dump_ast(os, v, indent + 1);
            return;
        }
        case AstNodeKind::StmtBreak: {
            if (auto* v = static_cast<const StmtBreak*>(node)->value)
                dump_ast(os, v, indent + 1);
            return;
        }
        case AstNodeKind::PatTuple: {
            auto* n = static_cast<const PatTuple*>(node);
            for (const Pattern* p : n->elems) dump_ast(os, p, indent + 1);
            return;
        }
        case AstNodeKind::PatStruct: {
            auto* n = static_cast<const PatStruct*>(node);
            dump_ast(os, n->type_name, indent + 1);
            for (const PatField* f : n->fields) dump_ast(os, f, indent + 1);
            return;
        }
        case AstNodeKind::PatVariant: {
            auto* n = static_cast<const PatVariant*>(node);
            dump_ast(os, n->path, indent + 1);
            for (const Pattern* p : n->args) dump_ast(os, p, indent + 1);
            return;
        }
        case AstNodeKind::PatTypeVariant: {
            auto* n = static_cast<const PatTypeVariant*>(node);
            dump_ast(os, n->type, indent + 1);
            for (const Pattern* p : n->args) dump_ast(os, p, indent + 1);
            return;
        }
        case AstNodeKind::PatPath: {
            dump_ast(os, static_cast<const PatPath*>(node)->path, indent + 1);
            return;
        }
        case AstNodeKind::PatOr: {
            auto* n = static_cast<const PatOr*>(node);
            dump_ast(os, n->lhs, indent + 1);
            dump_ast(os, n->rhs, indent + 1);
            return;
        }
        case AstNodeKind::PatField: {
            dump_ast(os, static_cast<const PatField*>(node)->pat, indent + 1);
            return;
        }
        case AstNodeKind::ExprPath: {
            dump_ast(os, static_cast<const ExprPath*>(node)->path, indent + 1);
            return;
        }
        case AstNodeKind::ExprTypeMember: {
            dump_ast(os, static_cast<const ExprTypeMember*>(node)->type,
                     indent + 1);
            return;
        }
        case AstNodeKind::ExprBlock: {
            dump_ast(os, static_cast<const ExprBlock*>(node)->block,
                     indent + 1);
            return;
        }
        case AstNodeKind::ExprComptime: {
            dump_ast(os, static_cast<const ExprComptime*>(node)->block,
                     indent + 1);
            return;
        }
        case AstNodeKind::ExprIf: {
            auto* n = static_cast<const ExprIf*>(node);
            dump_ast(os, n->cond, indent + 1);
            dump_ast(os, n->then_block, indent + 1);
            if (n->else_expr) dump_ast(os, n->else_expr, indent + 1);
            return;
        }
        case AstNodeKind::ExprWhile: {
            auto* n = static_cast<const ExprWhile*>(node);
            dump_ast(os, n->cond, indent + 1);
            dump_ast(os, n->body, indent + 1);
            return;
        }
        case AstNodeKind::ExprLoop: {
            dump_ast(os, static_cast<const ExprLoop*>(node)->body, indent + 1);
            return;
        }
        case AstNodeKind::ExprMatch: {
            auto* n = static_cast<const ExprMatch*>(node);
            dump_ast(os, n->scrutinee, indent + 1);
            for (const MatchArm* a : n->arms) dump_ast(os, a, indent + 1);
            return;
        }
        case AstNodeKind::MatchArm: {
            auto* n = static_cast<const MatchArm*>(node);
            dump_ast(os, n->pat, indent + 1);
            if (n->guard) dump_ast(os, n->guard, indent + 1);
            dump_ast(os, n->body, indent + 1);
            return;
        }
        case AstNodeKind::ExprCall: {
            auto* n = static_cast<const ExprCall*>(node);
            dump_ast(os, n->callee, indent + 1);
            for (const Expr* a : n->args) dump_ast(os, a, indent + 1);
            return;
        }
        case AstNodeKind::ExprMethodCall: {
            auto* n = static_cast<const ExprMethodCall*>(node);
            dump_ast(os, n->receiver, indent + 1);
            for (const Expr* a : n->args) dump_ast(os, a, indent + 1);
            return;
        }
        case AstNodeKind::ExprField: {
            dump_ast(os, static_cast<const ExprField*>(node)->base, indent + 1);
            return;
        }
        case AstNodeKind::ExprIndex: {
            auto* n = static_cast<const ExprIndex*>(node);
            dump_ast(os, n->base, indent + 1);
            dump_ast(os, n->index, indent + 1);
            return;
        }
        case AstNodeKind::ExprCast: {
            auto* n = static_cast<const ExprCast*>(node);
            dump_ast(os, n->value, indent + 1);
            dump_ast(os, n->to, indent + 1);
            return;
        }
        case AstNodeKind::ExprUnary: {
            dump_ast(os, static_cast<const ExprUnary*>(node)->expr, indent + 1);
            return;
        }
        case AstNodeKind::ExprBinary: {
            auto* n = static_cast<const ExprBinary*>(node);
            dump_ast(os, n->lhs, indent + 1);
            dump_ast(os, n->rhs, indent + 1);
            return;
        }
        case AstNodeKind::ExprAssign: {
            auto* n = static_cast<const ExprAssign*>(node);
            dump_ast(os, n->lhs, indent + 1);
            dump_ast(os, n->rhs, indent + 1);
            return;
        }
        case AstNodeKind::ExprStructLit: {
            auto* n = static_cast<const ExprStructLit*>(node);
            dump_ast(os, n->type_name, indent + 1);
            for (const FieldInit* f : n->inits) dump_ast(os, f, indent + 1);
            return;
        }
        case AstNodeKind::FieldInit: {
            dump_ast(os, static_cast<const FieldInit*>(node)->value,
                     indent + 1);
            return;
        }
        case AstNodeKind::ExprTuple: {
            auto* n = static_cast<const ExprTuple*>(node);
            for (const Expr* e : n->elems) dump_ast(os, e, indent + 1);
            return;
        }
        case AstNodeKind::ExprArrayLit: {
            auto* n = static_cast<const ExprArrayLit*>(node);
            for (const Expr* e : n->elems) dump_ast(os, e, indent + 1);
            return;
        }
        case AstNodeKind::ExprArrayRepeat: {
            auto* n = static_cast<const ExprArrayRepeat*>(node);
            dump_ast(os, n->elem, indent + 1);
            dump_ast(os, n->count, indent + 1);
            return;
        }
        default:
            return;
    }
}

}  // namespace cog
