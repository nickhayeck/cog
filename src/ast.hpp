#pragma once

#include "span.hpp"

#include <cstdint>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace cog {

enum class Visibility : std::uint8_t { Private, Pub, PubCrate };
enum class Mutability : std::uint8_t { Const, Mut };

enum class UnaryOp : std::uint8_t { Neg, Not, Deref, AddrOf, AddrOfMut };
enum class BinaryOp : std::uint8_t {
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  Eq,
  Ne,
  Lt,
  Le,
  Gt,
  Ge,
  And,
  Or,
};

enum class AstNodeKind : std::uint16_t {
  File,

  Ident,
  Path,
  Attr,

  // Types
  TypePath,
  TypePtr,
  TypeSlice,
  TypeArray,
  TypeTuple,
  TypeUnit,
  TypeType,
  TypeNever,

  // Items
  UseTree,
  ItemUse,
  ItemModInline,
  ItemModDecl,
  ItemStruct,
  ItemEnum,
  ItemImplInherent,
  ItemFn,
  ItemConst,
  ItemStatic,
  ItemTypeAlias,

  // Decls
  FieldDecl,
  VariantDecl,
  Param,
  FnSig,
  FnDecl,

  // Statements
  StmtLet,
  StmtExpr,
  StmtReturn,
  StmtBreak,
  StmtContinue,

  // Patterns
  PatWildcard,
  PatInt,
  PatBool,
  PatBinding,
  PatTuple,
  PatStruct,
  PatVariant,
  PatPath,
  PatOr,
  PatField,

  // Expressions
  Block,
  MatchArm,
  FieldInit,

  ExprInt,
  ExprBool,
  ExprString,
  ExprUnit,
  ExprPath,
  ExprBlock,
  ExprComptime,
  ExprIf,
  ExprWhile,
  ExprLoop,
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
  ExprTuple,
};

struct AstNode {
  AstNodeKind kind{};
  Span span{};

  AstNode(AstNodeKind kind, Span span) : kind(kind), span(span) {}
  virtual ~AstNode() = default;
};

class AstArena {
 public:
  template <typename T, typename... Args>
  T* make(Args&&... args) {
    auto node = std::make_unique<T>(std::forward<Args>(args)...);
    T* raw = node.get();
    nodes_.push_back(std::move(node));
    return raw;
  }

 private:
  std::vector<std::unique_ptr<AstNode>> nodes_{};
};

struct Ident final : AstNode {
  std::string text{};
  explicit Ident(Span span, std::string text)
      : AstNode(AstNodeKind::Ident, span), text(std::move(text)) {}
};

struct Path final : AstNode {
  std::vector<Ident*> segments{};
  explicit Path(Span span, std::vector<Ident*> segments)
      : AstNode(AstNodeKind::Path, span), segments(std::move(segments)) {}
};

struct Attr final : AstNode {
  Path* name = nullptr;
  Path* arg_path = nullptr;  // optional; null means none
  std::optional<std::string> arg_string{};
  explicit Attr(Span span, Path* name) : AstNode(AstNodeKind::Attr, span), name(name) {}
  explicit Attr(Span span, Path* name, Path* arg_path)
      : AstNode(AstNodeKind::Attr, span), name(name), arg_path(arg_path) {}
  explicit Attr(Span span, Path* name, std::string arg_string)
      : AstNode(AstNodeKind::Attr, span), name(name), arg_string(std::move(arg_string)) {}
};

// ---- Types ----

struct Type : AstNode {
  explicit Type(AstNodeKind kind, Span span) : AstNode(kind, span) {}
};

struct TypePath final : Type {
  Path* path = nullptr;
  explicit TypePath(Span span, Path* path) : Type(AstNodeKind::TypePath, span), path(path) {}
};

struct TypePtr final : Type {
  Mutability mutability{};
  Type* pointee = nullptr;
  explicit TypePtr(Span span, Mutability mutability, Type* pointee)
      : Type(AstNodeKind::TypePtr, span), mutability(mutability), pointee(pointee) {}
};

struct TypeSlice final : Type {
  Type* elem = nullptr;
  explicit TypeSlice(Span span, Type* elem) : Type(AstNodeKind::TypeSlice, span), elem(elem) {}
};

struct Expr;  // forward

struct TypeArray final : Type {
  Type* elem = nullptr;
  Expr* len = nullptr;
  explicit TypeArray(Span span, Type* elem, Expr* len)
      : Type(AstNodeKind::TypeArray, span), elem(elem), len(len) {}
};

struct TypeTuple final : Type {
  std::vector<Type*> elems{};
  explicit TypeTuple(Span span, std::vector<Type*> elems)
      : Type(AstNodeKind::TypeTuple, span), elems(std::move(elems)) {}
};

struct TypeUnit final : Type {
  explicit TypeUnit(Span span) : Type(AstNodeKind::TypeUnit, span) {}
};

struct TypeType final : Type {
  explicit TypeType(Span span) : Type(AstNodeKind::TypeType, span) {}
};

struct TypeNever final : Type {
  explicit TypeNever(Span span) : Type(AstNodeKind::TypeNever, span) {}
};

// ---- Expr / Stmt / Pattern ----

struct Stmt : AstNode {
  explicit Stmt(AstNodeKind kind, Span span) : AstNode(kind, span) {}
};

struct Pattern : AstNode {
  explicit Pattern(AstNodeKind kind, Span span) : AstNode(kind, span) {}
};

struct Block final : AstNode {
  std::vector<Stmt*> stmts{};
  Expr* tail = nullptr;  // optional
  explicit Block(Span span, std::vector<Stmt*> stmts, Expr* tail)
      : AstNode(AstNodeKind::Block, span), stmts(std::move(stmts)), tail(tail) {}
};

struct Expr : AstNode {
  explicit Expr(AstNodeKind kind, Span span) : AstNode(kind, span) {}
};

struct MatchArm final : AstNode {
  Pattern* pat = nullptr;
  Expr* guard = nullptr;  // optional
  Expr* body = nullptr;
  explicit MatchArm(Span span, Pattern* pat, Expr* guard, Expr* body)
      : AstNode(AstNodeKind::MatchArm, span), pat(pat), guard(guard), body(body) {}
};

struct FieldInit final : AstNode {
  std::string name{};
  Expr* value = nullptr;
  explicit FieldInit(Span span, std::string name, Expr* value)
      : AstNode(AstNodeKind::FieldInit, span), name(std::move(name)), value(value) {}
};

// ---- Patterns ----

struct PatWildcard final : Pattern {
  explicit PatWildcard(Span span) : Pattern(AstNodeKind::PatWildcard, span) {}
};

struct PatInt final : Pattern {
  std::int64_t value = 0;
  explicit PatInt(Span span, std::int64_t value) : Pattern(AstNodeKind::PatInt, span), value(value) {}
};

struct PatBool final : Pattern {
  bool value = false;
  explicit PatBool(Span span, bool value) : Pattern(AstNodeKind::PatBool, span), value(value) {}
};

struct PatBinding final : Pattern {
  bool is_mut = false;
  std::string name{};
  explicit PatBinding(Span span, bool is_mut, std::string name)
      : Pattern(AstNodeKind::PatBinding, span), is_mut(is_mut), name(std::move(name)) {}
};

struct PatTuple final : Pattern {
  std::vector<Pattern*> elems{};
  explicit PatTuple(Span span, std::vector<Pattern*> elems)
      : Pattern(AstNodeKind::PatTuple, span), elems(std::move(elems)) {}
};

struct PatField final : AstNode {
  std::string name{};
  Pattern* pat = nullptr;
  explicit PatField(Span span, std::string name, Pattern* pat)
      : AstNode(AstNodeKind::PatField, span), name(std::move(name)), pat(pat) {}
};

struct PatStruct final : Pattern {
  Path* type_name = nullptr;
  std::vector<PatField*> fields{};
  explicit PatStruct(Span span, Path* type_name, std::vector<PatField*> fields)
      : Pattern(AstNodeKind::PatStruct, span), type_name(type_name), fields(std::move(fields)) {}
};

struct PatVariant final : Pattern {
  Path* path = nullptr;
  std::vector<Pattern*> args{};
  explicit PatVariant(Span span, Path* path, std::vector<Pattern*> args)
      : Pattern(AstNodeKind::PatVariant, span), path(path), args(std::move(args)) {}
};

struct PatPath final : Pattern {
  Path* path = nullptr;
  explicit PatPath(Span span, Path* path) : Pattern(AstNodeKind::PatPath, span), path(path) {}
};

struct PatOr final : Pattern {
  Pattern* lhs = nullptr;
  Pattern* rhs = nullptr;
  explicit PatOr(Span span, Pattern* lhs, Pattern* rhs)
      : Pattern(AstNodeKind::PatOr, span), lhs(lhs), rhs(rhs) {}
};

// ---- Expressions ----

struct ExprInt final : Expr {
  std::int64_t value = 0;
  explicit ExprInt(Span span, std::int64_t value) : Expr(AstNodeKind::ExprInt, span), value(value) {}
};

struct ExprBool final : Expr {
  bool value = false;
  explicit ExprBool(Span span, bool value) : Expr(AstNodeKind::ExprBool, span), value(value) {}
};

struct ExprString final : Expr {
  std::string value{};
  bool is_c_string = false;
  explicit ExprString(Span span, std::string value, bool is_c_string = false)
      : Expr(AstNodeKind::ExprString, span), value(std::move(value)), is_c_string(is_c_string) {}
};

struct ExprUnit final : Expr {
  explicit ExprUnit(Span span) : Expr(AstNodeKind::ExprUnit, span) {}
};

struct ExprPath final : Expr {
  Path* path = nullptr;
  explicit ExprPath(Span span, Path* path) : Expr(AstNodeKind::ExprPath, span), path(path) {}
};

struct ExprBlock final : Expr {
  Block* block = nullptr;
  explicit ExprBlock(Span span, Block* block) : Expr(AstNodeKind::ExprBlock, span), block(block) {}
};

struct ExprComptime final : Expr {
  Block* block = nullptr;
  explicit ExprComptime(Span span, Block* block) : Expr(AstNodeKind::ExprComptime, span), block(block) {}
};

struct ExprIf final : Expr {
  Expr* cond = nullptr;
  Block* then_block = nullptr;
  Expr* else_expr = nullptr;  // optional
  explicit ExprIf(Span span, Expr* cond, Block* then_block, Expr* else_expr)
      : Expr(AstNodeKind::ExprIf, span), cond(cond), then_block(then_block), else_expr(else_expr) {}
};

struct ExprWhile final : Expr {
  Expr* cond = nullptr;
  Block* body = nullptr;
  explicit ExprWhile(Span span, Expr* cond, Block* body)
      : Expr(AstNodeKind::ExprWhile, span), cond(cond), body(body) {}
};

struct ExprLoop final : Expr {
  Block* body = nullptr;
  explicit ExprLoop(Span span, Block* body) : Expr(AstNodeKind::ExprLoop, span), body(body) {}
};

struct ExprMatch final : Expr {
  Expr* scrutinee = nullptr;
  std::vector<MatchArm*> arms{};
  explicit ExprMatch(Span span, Expr* scrutinee, std::vector<MatchArm*> arms)
      : Expr(AstNodeKind::ExprMatch, span), scrutinee(scrutinee), arms(std::move(arms)) {}
};

struct ExprCall final : Expr {
  Expr* callee = nullptr;
  std::vector<Expr*> args{};
  explicit ExprCall(Span span, Expr* callee, std::vector<Expr*> args)
      : Expr(AstNodeKind::ExprCall, span), callee(callee), args(std::move(args)) {}
};

struct ExprMethodCall final : Expr {
  Expr* receiver = nullptr;
  std::string method{};
  std::vector<Expr*> args{};
  explicit ExprMethodCall(Span span, Expr* receiver, std::string method, std::vector<Expr*> args)
      : Expr(AstNodeKind::ExprMethodCall, span),
        receiver(receiver),
        method(std::move(method)),
        args(std::move(args)) {}
};

struct ExprField final : Expr {
  Expr* base = nullptr;
  std::string field{};
  explicit ExprField(Span span, Expr* base, std::string field)
      : Expr(AstNodeKind::ExprField, span), base(base), field(std::move(field)) {}
};

struct ExprIndex final : Expr {
  Expr* base = nullptr;
  Expr* index = nullptr;
  explicit ExprIndex(Span span, Expr* base, Expr* index)
      : Expr(AstNodeKind::ExprIndex, span), base(base), index(index) {}
};

struct ExprCast final : Expr {
  Expr* value = nullptr;
  Type* to = nullptr;
  explicit ExprCast(Span span, Expr* value, Type* to)
      : Expr(AstNodeKind::ExprCast, span), value(value), to(to) {}
};

struct ExprUnary final : Expr {
  UnaryOp op{};
  Expr* expr = nullptr;
  explicit ExprUnary(Span span, UnaryOp op, Expr* expr)
      : Expr(AstNodeKind::ExprUnary, span), op(op), expr(expr) {}
};

struct ExprBinary final : Expr {
  BinaryOp op{};
  Expr* lhs = nullptr;
  Expr* rhs = nullptr;
  explicit ExprBinary(Span span, BinaryOp op, Expr* lhs, Expr* rhs)
      : Expr(AstNodeKind::ExprBinary, span), op(op), lhs(lhs), rhs(rhs) {}
};

struct ExprAssign final : Expr {
  Expr* lhs = nullptr;
  Expr* rhs = nullptr;
  explicit ExprAssign(Span span, Expr* lhs, Expr* rhs)
      : Expr(AstNodeKind::ExprAssign, span), lhs(lhs), rhs(rhs) {}
};

struct ExprStructLit final : Expr {
  Path* type_name = nullptr;
  std::vector<FieldInit*> inits{};
  explicit ExprStructLit(Span span, Path* type_name, std::vector<FieldInit*> inits)
      : Expr(AstNodeKind::ExprStructLit, span), type_name(type_name), inits(std::move(inits)) {}
};

struct ExprTuple final : Expr {
  std::vector<Expr*> elems{};
  explicit ExprTuple(Span span, std::vector<Expr*> elems)
      : Expr(AstNodeKind::ExprTuple, span), elems(std::move(elems)) {}
};

// ---- Statements ----

struct StmtLet final : Stmt {
  Pattern* pat = nullptr;
  Type* type_ann = nullptr;  // optional
  Expr* init = nullptr;      // optional
  explicit StmtLet(Span span, Pattern* pat, Type* type_ann, Expr* init)
      : Stmt(AstNodeKind::StmtLet, span), pat(pat), type_ann(type_ann), init(init) {}
};

struct StmtExpr final : Stmt {
  Expr* expr = nullptr;
  explicit StmtExpr(Span span, Expr* expr) : Stmt(AstNodeKind::StmtExpr, span), expr(expr) {}
};

struct StmtReturn final : Stmt {
  Expr* value = nullptr;  // optional
  explicit StmtReturn(Span span, Expr* value) : Stmt(AstNodeKind::StmtReturn, span), value(value) {}
};

struct StmtBreak final : Stmt {
  Expr* value = nullptr;  // optional
  explicit StmtBreak(Span span, Expr* value) : Stmt(AstNodeKind::StmtBreak, span), value(value) {}
};

struct StmtContinue final : Stmt {
  explicit StmtContinue(Span span) : Stmt(AstNodeKind::StmtContinue, span) {}
};

// ---- Items and decls ----

struct Param final : AstNode {
  bool is_comptime = false;
  std::string name{};
  Type* type = nullptr;
  explicit Param(Span span, bool is_comptime, std::string name, Type* type)
      : AstNode(AstNodeKind::Param, span), is_comptime(is_comptime), name(std::move(name)), type(type) {}
};

struct FnSig final : AstNode {
  std::vector<Param*> params{};
  Type* ret = nullptr;  // optional; null means ()
  bool is_variadic = false;
  explicit FnSig(Span span, std::vector<Param*> params, Type* ret, bool is_variadic = false)
      : AstNode(AstNodeKind::FnSig, span), params(std::move(params)), ret(ret), is_variadic(is_variadic) {}
};

struct FnDecl final : AstNode {
  std::string name{};
  FnSig* sig = nullptr;
  explicit FnDecl(Span span, std::string name, FnSig* sig)
      : AstNode(AstNodeKind::FnDecl, span), name(std::move(name)), sig(sig) {}
};

struct FieldDecl final : AstNode {
  std::vector<Attr*> attrs{};
  Visibility vis = Visibility::Private;
  std::string name{};
  Type* type = nullptr;
  explicit FieldDecl(Span span, std::vector<Attr*> attrs, Visibility vis, std::string name, Type* type)
      : AstNode(AstNodeKind::FieldDecl, span),
        attrs(std::move(attrs)),
        vis(vis),
        name(std::move(name)),
        type(type) {}
};

struct VariantDecl final : AstNode {
  std::string name{};
  std::vector<Type*> payload{};
  Expr* discriminant = nullptr;  // optional; null means implicit
  explicit VariantDecl(Span span, std::string name, std::vector<Type*> payload, Expr* discriminant = nullptr)
      : AstNode(AstNodeKind::VariantDecl, span),
        name(std::move(name)),
        payload(std::move(payload)),
        discriminant(discriminant) {}
};

struct UseTree final : AstNode {
  Path* path = nullptr;
  std::string alias{};  // empty means none
  std::vector<UseTree*> group{};  // non-empty means `path::{...}`
  explicit UseTree(Span span, Path* path, std::string alias, std::vector<UseTree*> group)
      : AstNode(AstNodeKind::UseTree, span),
        path(path),
        alias(std::move(alias)),
        group(std::move(group)) {}
};

struct Item : AstNode {
  std::vector<Attr*> attrs{};
  Visibility vis = Visibility::Private;
  explicit Item(AstNodeKind kind, Span span, std::vector<Attr*> attrs, Visibility vis)
      : AstNode(kind, span), attrs(std::move(attrs)), vis(vis) {}
};

struct ItemUse final : Item {
  UseTree* tree = nullptr;
  explicit ItemUse(Span span, std::vector<Attr*> attrs, Visibility vis, UseTree* tree)
      : Item(AstNodeKind::ItemUse, span, std::move(attrs), vis), tree(tree) {}
};

struct ItemModInline final : Item {
  std::string name{};
  std::vector<Item*> items{};
  explicit ItemModInline(Span span, std::vector<Attr*> attrs, Visibility vis, std::string name, std::vector<Item*> items)
      : Item(AstNodeKind::ItemModInline, span, std::move(attrs), vis),
        name(std::move(name)),
        items(std::move(items)) {}
};

struct ItemModDecl final : Item {
  std::string name{};
  explicit ItemModDecl(Span span, std::vector<Attr*> attrs, Visibility vis, std::string name)
      : Item(AstNodeKind::ItemModDecl, span, std::move(attrs), vis), name(std::move(name)) {}
};

struct ItemStruct final : Item {
  std::string name{};
  std::vector<FieldDecl*> fields{};
  explicit ItemStruct(Span span, std::vector<Attr*> attrs, Visibility vis, std::string name, std::vector<FieldDecl*> fields)
      : Item(AstNodeKind::ItemStruct, span, std::move(attrs), vis),
        name(std::move(name)),
        fields(std::move(fields)) {}
};

struct ItemEnum final : Item {
  std::string name{};
  std::vector<VariantDecl*> variants{};
  explicit ItemEnum(Span span, std::vector<Attr*> attrs, Visibility vis, std::string name, std::vector<VariantDecl*> variants)
      : Item(AstNodeKind::ItemEnum, span, std::move(attrs), vis),
        name(std::move(name)),
        variants(std::move(variants)) {}
};

struct ItemFn final : Item {
  FnDecl* decl = nullptr;
  Block* body = nullptr;
  explicit ItemFn(Span span, std::vector<Attr*> attrs, Visibility vis, FnDecl* decl, Block* body)
      : Item(AstNodeKind::ItemFn, span, std::move(attrs), vis), decl(decl), body(body) {}
};

struct ItemConst final : Item {
  std::string name{};
  Type* type = nullptr;
  Expr* value = nullptr;
  explicit ItemConst(Span span, std::vector<Attr*> attrs, Visibility vis, std::string name, Type* type, Expr* value)
      : Item(AstNodeKind::ItemConst, span, std::move(attrs), vis),
        name(std::move(name)),
        type(type),
        value(value) {}
};

struct ItemStatic final : Item {
  std::string name{};
  Type* type = nullptr;
  Expr* value = nullptr;
  explicit ItemStatic(Span span, std::vector<Attr*> attrs, Visibility vis, std::string name, Type* type, Expr* value)
      : Item(AstNodeKind::ItemStatic, span, std::move(attrs), vis),
        name(std::move(name)),
        type(type),
        value(value) {}
};

struct ItemTypeAlias final : Item {
  std::string name{};
  Type* aliased = nullptr;
  explicit ItemTypeAlias(Span span, std::vector<Attr*> attrs, Visibility vis, std::string name, Type* aliased)
      : Item(AstNodeKind::ItemTypeAlias, span, std::move(attrs), vis),
        name(std::move(name)),
        aliased(aliased) {}
};

struct ItemImplInherent final : Item {
  Path* type_name = nullptr;
  std::vector<ItemFn*> methods{};
  explicit ItemImplInherent(Span span, std::vector<Attr*> attrs, Visibility vis, Path* type_name, std::vector<ItemFn*> methods)
      : Item(AstNodeKind::ItemImplInherent, span, std::move(attrs), vis),
        type_name(type_name),
        methods(std::move(methods)) {}
};

struct FileAst final : AstNode {
  std::vector<Item*> items{};
  explicit FileAst(Span span, std::vector<Item*> items)
      : AstNode(AstNodeKind::File, span), items(std::move(items)) {}
};

std::string_view ast_kind_name(AstNodeKind kind);
void dump_ast(std::ostream& os, const AstNode* node, int indent = 0);

}  // namespace cog
