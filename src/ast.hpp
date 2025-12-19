#pragma once

#include <cstdint>
#include <memory>
#include <ostream>
#include <string>
#include <string_view>
#include <vector>

namespace cog {

enum class NodeKind : std::uint16_t {
  Program,
  List,
  Ident,
  Item,

  Attr,
  Vis,
  Path,

  ItemExternBlock,
  ItemMod,
  ItemUse,
  ItemStruct,
  ItemEnum,
  ItemTrait,
  ItemImpl,
  ItemFn,
  ItemConst,
  ItemStatic,
  ItemTypeAlias,

  Field,
  Variant,
  Param,

  Block,
  MatchArm,

  StmtLet,
  StmtExpr,
  StmtReturn,

  PatWildcard,
  PatInt,
  PatPath,
  PatVariant,
  PatOr,

  TypePath,
  TypePtr,
  TypeDyn,
  TypeSlice,
  TypeArray,
  TypeTuple,
  TypeUnit,

  ExprInt,
  ExprString,
  ExprUnit,
  ExprPath,
  ExprBlock,
  ExprComptime,
  ExprMatch,
  ExprCall,
  ExprMethodCall,
  ExprField,
  ExprIndex,
  ExprCast,
  ExprUnary,
  ExprBinary,
  ExprAssign,
  ExprStructLit,
};

struct Node {
  NodeKind kind{};
  std::string text{};
  std::vector<Node*> children{};
};

class Arena {
 public:
  Node* make(NodeKind kind, std::string text = {}, std::vector<Node*> children = {});

 private:
  std::vector<std::unique_ptr<Node>> nodes_{};
};

std::string_view node_kind_name(NodeKind kind);
void print_tree(std::ostream& os, const Node* node, int indent = 0);

}  // namespace cog
