#include "ast.hpp"

#include <utility>

namespace cog {

Node* Arena::make(NodeKind kind, std::string text, std::vector<Node*> children) {
  auto node = std::make_unique<Node>();
  node->kind = kind;
  node->text = std::move(text);
  node->children = std::move(children);
  Node* raw = node.get();
  nodes_.push_back(std::move(node));
  return raw;
}

std::string_view node_kind_name(NodeKind kind) {
  switch (kind) {
    case NodeKind::Program:
      return "Program";
    case NodeKind::List:
      return "List";
    case NodeKind::Ident:
      return "Ident";
    case NodeKind::Item:
      return "Item";
    case NodeKind::Attr:
      return "Attr";
    case NodeKind::Vis:
      return "Vis";
    case NodeKind::Path:
      return "Path";
    case NodeKind::ItemExternBlock:
      return "ItemExternBlock";
    case NodeKind::ItemMod:
      return "ItemMod";
    case NodeKind::ItemUse:
      return "ItemUse";
    case NodeKind::ItemStruct:
      return "ItemStruct";
    case NodeKind::ItemEnum:
      return "ItemEnum";
    case NodeKind::ItemTrait:
      return "ItemTrait";
    case NodeKind::ItemImpl:
      return "ItemImpl";
    case NodeKind::ItemFn:
      return "ItemFn";
    case NodeKind::ItemConst:
      return "ItemConst";
    case NodeKind::ItemStatic:
      return "ItemStatic";
    case NodeKind::ItemTypeAlias:
      return "ItemTypeAlias";
    case NodeKind::Field:
      return "Field";
    case NodeKind::Variant:
      return "Variant";
    case NodeKind::Param:
      return "Param";
    case NodeKind::Block:
      return "Block";
    case NodeKind::MatchArm:
      return "MatchArm";
    case NodeKind::StmtLet:
      return "StmtLet";
    case NodeKind::StmtExpr:
      return "StmtExpr";
    case NodeKind::StmtReturn:
      return "StmtReturn";
    case NodeKind::PatWildcard:
      return "PatWildcard";
    case NodeKind::PatInt:
      return "PatInt";
    case NodeKind::PatPath:
      return "PatPath";
    case NodeKind::PatVariant:
      return "PatVariant";
    case NodeKind::PatOr:
      return "PatOr";
    case NodeKind::TypePath:
      return "TypePath";
    case NodeKind::TypePtr:
      return "TypePtr";
    case NodeKind::TypeDyn:
      return "TypeDyn";
    case NodeKind::TypeSlice:
      return "TypeSlice";
    case NodeKind::TypeArray:
      return "TypeArray";
    case NodeKind::TypeTuple:
      return "TypeTuple";
    case NodeKind::TypeUnit:
      return "TypeUnit";
    case NodeKind::ExprInt:
      return "ExprInt";
    case NodeKind::ExprString:
      return "ExprString";
    case NodeKind::ExprUnit:
      return "ExprUnit";
    case NodeKind::ExprPath:
      return "ExprPath";
    case NodeKind::ExprBlock:
      return "ExprBlock";
    case NodeKind::ExprComptime:
      return "ExprComptime";
    case NodeKind::ExprMatch:
      return "ExprMatch";
    case NodeKind::ExprCall:
      return "ExprCall";
    case NodeKind::ExprMethodCall:
      return "ExprMethodCall";
    case NodeKind::ExprField:
      return "ExprField";
    case NodeKind::ExprIndex:
      return "ExprIndex";
    case NodeKind::ExprCast:
      return "ExprCast";
    case NodeKind::ExprUnary:
      return "ExprUnary";
    case NodeKind::ExprBinary:
      return "ExprBinary";
    case NodeKind::ExprAssign:
      return "ExprAssign";
    case NodeKind::ExprStructLit:
      return "ExprStructLit";
  }
  return "Unknown";
}

static void indent_to(std::ostream& os, int indent) {
  for (int i = 0; i < indent; i++) os << "  ";
}

void print_tree(std::ostream& os, const Node* node, int indent) {
  if (!node) {
    indent_to(os, indent);
    os << "<null>\n";
    return;
  }

  indent_to(os, indent);
  os << node_kind_name(node->kind);
  if (!node->text.empty()) os << " \"" << node->text << '"';
  os << '\n';
  for (const Node* child : node->children) print_tree(os, child, indent + 1);
}

}  // namespace cog
