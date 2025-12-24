%{
#include "parse_state.hpp"

#include <cstdint>
#include <string>
#include <utility>
#include <vector>

extern int yylex(void);
int yyerror(const char* msg);

template <class T>
static std::vector<T*>* vec_new() {
  return new std::vector<T*>();
}
template <class T>
static std::vector<T*>* vec1(T* n) {
  auto* v = new std::vector<T*>();
  v->push_back(n);
  return v;
}
template <class T>
static std::vector<T*>* vec_push(std::vector<T*>* v, T* n) {
  v->push_back(n);
  return v;
}
template <class T>
static std::vector<T*> take_vec(std::vector<T*>* v) {
  std::vector<T*> out = std::move(*v);
  delete v;
  return out;
}

#define SPAN(loc)                                                                                   \
  cog::Span {                                                                                       \
    .file = cog::g_parse_state ? cog::g_parse_state->file : 0,                                      \
    .begin = cog::SourceLoc{                                                                        \
        static_cast<std::uint32_t>((loc).first_line), static_cast<std::uint32_t>((loc).first_column)}, \
    .end = cog::SourceLoc{                                                                          \
        static_cast<std::uint32_t>((loc).last_line), static_cast<std::uint32_t>((loc).last_column)}     \
  }

#define MK(T, loc, ...) cog::mk<T>(SPAN(loc) __VA_OPT__(,) __VA_ARGS__)

%}

%locations
%error-verbose

%union {
  long long int_val;
  char* str;
  cog::Visibility vis;

  cog::FileAst* file;
  cog::Item* item;
  cog::ItemFn* item_fn;

  cog::Attr* attr;
  cog::Ident* ident;
  cog::Path* path;
  cog::UseTree* use_tree;

  cog::FieldDecl* field_decl;
  cog::VariantDecl* variant_decl;

  cog::Param* param;
  cog::FnDecl* fn_decl;
  cog::FnSig* sig;

  cog::Type* type;
  cog::Block* block;
  cog::Stmt* stmt;
  cog::Expr* expr;
  cog::Pattern* pat;
  cog::MatchArm* arm;
  cog::FieldInit* field_init;
  cog::PatField* pat_field;

  std::vector<cog::Item*>* items;
  std::vector<cog::ItemFn*>* item_fns;
  std::vector<cog::Attr*>* attrs;
  std::vector<cog::Ident*>* idents;
  std::vector<cog::UseTree*>* use_trees;
  std::vector<cog::FieldDecl*>* field_decls;
  std::vector<cog::VariantDecl*>* variant_decls;
  std::vector<cog::Param*>* params;
  std::vector<cog::FnDecl*>* fn_decls;
  std::vector<cog::Type*>* types;
  std::vector<cog::Stmt*>* stmts;
  std::vector<cog::Expr*>* exprs;
  std::vector<cog::Pattern*>* pats;
  std::vector<cog::MatchArm*>* arms;
  std::vector<cog::FieldInit*>* field_inits;
  std::vector<cog::PatField*>* pat_fields;
}

%token <str> IDENT
%token <str> STRING
%token <int_val> INT

%token KW_FN KW_STRUCT KW_ENUM KW_TRAIT KW_IMPL KW_FOR KW_TYPE KW_CONST KW_STATIC
%token KW_MOD KW_USE KW_PUB KW_AS KW_LET KW_MUT KW_IF KW_ELSE KW_WHILE
%token KW_LOOP KW_MATCH KW_RETURN KW_BREAK KW_CONTINUE KW_COMPTIME KW_DYN KW_SELF_TYPE
%token KW_TRUE KW_FALSE

%token TOK_COLONCOLON TOK_COLONCOLON_LBRACE TOK_ARROW TOK_FATARROW TOK_EQEQ TOK_NEQ TOK_LE TOK_GE TOK_ANDAND TOK_OROR TOK_ELLIPSIS

%type <file> program
%type <items> items
%type <item> item item_core
%type <vis> vis_opt
%type <attrs> tags tags_list_opt tags_list
%type <attr> tag
%type <path> path
%type <idents> path_segments
%type <ident> path_seg
%type <path> qpath
%type <idents> qpath_segments

%type <use_tree> use_tree
%type <use_trees> use_trees_opt use_trees
%type <str> use_alias_opt

%type <type> type ret_opt
%type <types> types_opt types

%type <param> param
%type <params> params
%type <sig> fn_sig
%type <fn_decl> fn_decl

%type <field_decl> field_decl
%type <field_decls> fields_opt fields

%type <variant_decl> variant_decl
%type <variant_decls> variants_opt variants

%type <block> block
%type <block> block_elems_opt block_elems
%type <stmts> stmts
%type <stmt> stmt let_stmt

%type <expr> expr expr_nostruct expr_or_block guard_opt else_opt init_opt
%type <expr> assign_expr logical_or_expr logical_and_expr equality_expr relational_expr additive_expr multiplicative_expr
%type <expr> cast_expr unary_expr postfix_expr primary_expr
%type <type> type_ann_opt
%type <exprs> args_opt args
%type <field_inits> field_inits_opt field_inits
%type <field_init> field_init

%type <arms> match_arms_opt match_arms
%type <arm> match_arm

%type <pat> pattern pattern_primary
%type <pats> pat_list_opt pat_list
%type <pat_fields> pat_fields_opt pat_fields
%type <pat_field> pat_field

%type <item_fns> impl_items_opt impl_items
%type <item_fn> impl_item
%type <fn_decls> trait_items_opt trait_items

%start program

%right '='
%left TOK_OROR
%left TOK_ANDAND
%left TOK_EQEQ TOK_NEQ
%left '<' '>' TOK_LE TOK_GE
%left '+' '-'
%left '*' '/' '%'
%left KW_AS
%right UNARY

%destructor { free($$); } IDENT STRING use_alias_opt
%destructor { delete $$; } items tags tags_list_opt tags_list path_segments qpath_segments use_trees_opt use_trees fields_opt fields variants_opt variants params trait_items_opt trait_items impl_items_opt impl_items stmts args_opt args field_inits_opt field_inits match_arms_opt match_arms pat_list_opt pat_list pat_fields_opt pat_fields

%%

program
  : items {
      std::vector<cog::Item*> its = take_vec($1);
      $$ = MK(cog::FileAst, @$, its);
      cog::g_parse_state->root = $$;
    }
  ;

items
  : /* empty */ { $$ = vec_new<cog::Item>(); }
  | items item { $$ = vec_push($1, $2); }
  ;

item
  : vis_opt item_core {
      $2->vis = $1;
      $2->span = SPAN(@$);
      $$ = $2;
    }
  ;

vis_opt
  : /* empty */ { $$ = cog::Visibility::Private; }
  | KW_PUB { $$ = cog::Visibility::Pub; }
  ;

tags
  : '[' tags_list_opt ']' { $$ = $2; }
  ;

tags_list_opt
  : /* empty */ { $$ = vec_new<cog::Attr>(); }
  | tags_list { $$ = $1; }
  ;

tags_list
  : tags_list ',' tag { $$ = vec_push($1, $3); }
  | tags_list ',' { $$ = $1; }
  | tag { $$ = vec1($1); }
  ;

tag
  : path { $$ = MK(cog::Attr, @$, $1, nullptr); }
  | path '(' path ')' { $$ = MK(cog::Attr, @$, $1, $3); }
  ;

item_core
  : KW_USE use_tree ';' {
      $$ = MK(cog::ItemUse, @$, std::vector<cog::Attr*>{}, cog::Visibility::Private, $2);
    }
  | KW_USE tags use_tree ';' {
      $$ = MK(cog::ItemUse, @$, take_vec($2), cog::Visibility::Private, $3);
    }
  | KW_MOD IDENT '{' items '}' {
      std::vector<cog::Item*> body = take_vec($4);
      $$ = MK(
          cog::ItemModInline, @$, std::vector<cog::Attr*>{}, cog::Visibility::Private, cog::take_str($2), std::move(body));
    }
  | KW_MOD tags IDENT '{' items '}' {
      std::vector<cog::Item*> body = take_vec($5);
      $$ = MK(cog::ItemModInline, @$, take_vec($2), cog::Visibility::Private, cog::take_str($3), std::move(body));
    }
  | KW_MOD IDENT ';' {
      $$ = MK(cog::ItemModDecl, @$, std::vector<cog::Attr*>{}, cog::Visibility::Private, cog::take_str($2));
    }
  | KW_MOD tags IDENT ';' {
      $$ = MK(cog::ItemModDecl, @$, take_vec($2), cog::Visibility::Private, cog::take_str($3));
    }
  | KW_STRUCT IDENT '{' fields_opt '}' {
      std::vector<cog::FieldDecl*> fs = take_vec($4);
      $$ = MK(
          cog::ItemStruct, @$, std::vector<cog::Attr*>{}, cog::Visibility::Private, cog::take_str($2), std::move(fs));
    }
  | KW_STRUCT tags IDENT '{' fields_opt '}' {
      std::vector<cog::FieldDecl*> fs = take_vec($5);
      $$ = MK(cog::ItemStruct, @$, take_vec($2), cog::Visibility::Private, cog::take_str($3), std::move(fs));
    }
  | KW_ENUM IDENT '{' variants_opt '}' {
      std::vector<cog::VariantDecl*> vs = take_vec($4);
      $$ = MK(
          cog::ItemEnum, @$, std::vector<cog::Attr*>{}, cog::Visibility::Private, cog::take_str($2), std::move(vs));
    }
  | KW_ENUM tags IDENT '{' variants_opt '}' {
      std::vector<cog::VariantDecl*> vs = take_vec($5);
      $$ = MK(cog::ItemEnum, @$, take_vec($2), cog::Visibility::Private, cog::take_str($3), std::move(vs));
    }
  | KW_TRAIT IDENT '{' trait_items_opt '}' {
      std::vector<cog::FnDecl*> ms = take_vec($4);
      $$ = MK(
          cog::ItemTrait, @$, std::vector<cog::Attr*>{}, cog::Visibility::Private, cog::take_str($2), std::move(ms));
    }
  | KW_TRAIT tags IDENT '{' trait_items_opt '}' {
      std::vector<cog::FnDecl*> ms = take_vec($5);
      $$ = MK(cog::ItemTrait, @$, take_vec($2), cog::Visibility::Private, cog::take_str($3), std::move(ms));
    }
  | KW_IMPL path '{' impl_items_opt '}' {
      std::vector<cog::ItemFn*> ms = take_vec($4);
      $$ = MK(cog::ItemImplInherent, @$, std::vector<cog::Attr*>{}, cog::Visibility::Private, $2, std::move(ms));
    }
  | KW_IMPL tags path '{' impl_items_opt '}' {
      std::vector<cog::ItemFn*> ms = take_vec($5);
      $$ = MK(cog::ItemImplInherent, @$, take_vec($2), cog::Visibility::Private, $3, std::move(ms));
    }
  | KW_IMPL path KW_FOR path '{' impl_items_opt '}' {
      std::vector<cog::ItemFn*> ms = take_vec($6);
      $$ = MK(cog::ItemImplTrait, @$, std::vector<cog::Attr*>{}, cog::Visibility::Private, $2, $4, std::move(ms));
    }
  | KW_IMPL tags path KW_FOR path '{' impl_items_opt '}' {
      std::vector<cog::ItemFn*> ms = take_vec($7);
      $$ = MK(cog::ItemImplTrait, @$, take_vec($2), cog::Visibility::Private, $3, $5, std::move(ms));
    }
  | KW_FN IDENT fn_sig block {
      cog::FnDecl* decl = MK(cog::FnDecl, @$, cog::take_str($2), $3);
      $$ = MK(cog::ItemFn, @$, std::vector<cog::Attr*>{}, cog::Visibility::Private, decl, $4);
    }
  | KW_FN tags IDENT fn_sig block {
      cog::FnDecl* decl = MK(cog::FnDecl, @$, cog::take_str($3), $4);
      $$ = MK(cog::ItemFn, @$, take_vec($2), cog::Visibility::Private, decl, $5);
    }
  | KW_FN IDENT fn_sig ';' {
      cog::FnDecl* decl = MK(cog::FnDecl, @$, cog::take_str($2), $3);
      $$ = MK(cog::ItemFn, @$, std::vector<cog::Attr*>{}, cog::Visibility::Private, decl, nullptr);
    }
  | KW_FN tags IDENT fn_sig ';' {
      cog::FnDecl* decl = MK(cog::FnDecl, @$, cog::take_str($3), $4);
      $$ = MK(cog::ItemFn, @$, take_vec($2), cog::Visibility::Private, decl, nullptr);
    }
  | KW_CONST IDENT ':' type '=' expr ';' {
      $$ = MK(cog::ItemConst, @$, std::vector<cog::Attr*>{}, cog::Visibility::Private, cog::take_str($2), $4, $6);
    }
  | KW_CONST tags IDENT ':' type '=' expr ';' {
      $$ = MK(
          cog::ItemConst, @$, take_vec($2), cog::Visibility::Private, cog::take_str($3), $5, $7);
    }
  | KW_STATIC IDENT ':' type '=' expr ';' {
      $$ = MK(cog::ItemStatic, @$, std::vector<cog::Attr*>{}, cog::Visibility::Private, cog::take_str($2), $4, $6);
    }
  | KW_STATIC tags IDENT ':' type '=' expr ';' {
      $$ = MK(
          cog::ItemStatic, @$, take_vec($2), cog::Visibility::Private, cog::take_str($3), $5, $7);
    }
  | KW_TYPE IDENT '=' type ';' {
      $$ = MK(cog::ItemTypeAlias, @$, std::vector<cog::Attr*>{}, cog::Visibility::Private, cog::take_str($2), $4);
    }
  | KW_TYPE tags IDENT '=' type ';' {
      $$ = MK(
          cog::ItemTypeAlias, @$, take_vec($2), cog::Visibility::Private, cog::take_str($3), $5);
    }
  ;

use_tree
  : path use_alias_opt {
      std::string alias = $2 ? cog::take_str($2) : std::string{};
      $$ = MK(cog::UseTree, @$, $1, std::move(alias), std::vector<cog::UseTree*>{});
    }
  | path TOK_COLONCOLON_LBRACE use_trees_opt '}' {
      std::vector<cog::UseTree*> group = take_vec($3);
      $$ = MK(cog::UseTree, @$, $1, std::string{}, std::move(group));
    }
  ;

use_alias_opt
  : /* empty */ { $$ = nullptr; }
  | KW_AS IDENT { $$ = $2; }
  ;

use_trees_opt
  : /* empty */ { $$ = vec_new<cog::UseTree>(); }
  | use_trees { $$ = $1; }
  ;

use_trees
  : use_trees ',' use_tree { $$ = vec_push($1, $3); }
  | use_trees ',' { $$ = $1; }
  | use_tree { $$ = vec1($1); }
  ;

trait_items_opt
  : /* empty */ { $$ = vec_new<cog::FnDecl>(); }
  | trait_items { $$ = $1; }
  ;

trait_items
  : trait_items fn_decl ';' { $$ = vec_push($1, $2); }
  | fn_decl ';' { $$ = vec1($1); }
  ;

impl_items_opt
  : /* empty */ { $$ = vec_new<cog::ItemFn>(); }
  | impl_items { $$ = $1; }
  ;

impl_items
  : impl_items impl_item { $$ = vec_push($1, $2); }
  | impl_item { $$ = vec1($1); }
  ;

impl_item
  : vis_opt KW_FN IDENT fn_sig block {
      cog::FnDecl* decl = MK(cog::FnDecl, @$, cog::take_str($3), $4);
      $$ = MK(cog::ItemFn, @$, std::vector<cog::Attr*>{}, $1, decl, $5);
    }
  | vis_opt KW_FN tags IDENT fn_sig block {
      std::vector<cog::Attr*> tags = take_vec($3);
      cog::FnDecl* decl = MK(cog::FnDecl, @$, cog::take_str($4), $5);
      $$ = MK(cog::ItemFn, @$, std::move(tags), $1, decl, $6);
    }
  ;

fn_decl
  : KW_FN IDENT fn_sig {
      $$ = MK(cog::FnDecl, @$, cog::take_str($2), $3);
    }
  ;

fn_sig
  : '(' ')' ret_opt { $$ = MK(cog::FnSig, @$, std::vector<cog::Param*>{}, $3, /*is_variadic=*/false); }
  | '(' params ')' ret_opt {
      $$ = MK(cog::FnSig, @$, take_vec($2), $4, /*is_variadic=*/false);
    }
  | '(' params ',' TOK_ELLIPSIS ')' ret_opt {
      $$ = MK(cog::FnSig, @$, take_vec($2), $6, /*is_variadic=*/true);
    }
  | '(' TOK_ELLIPSIS ')' ret_opt { $$ = MK(cog::FnSig, @$, std::vector<cog::Param*>{}, $4, /*is_variadic=*/true); }
  ;

params
  : params ',' param { $$ = vec_push($1, $3); }
  | param { $$ = vec1($1); }
  ;

param
  : IDENT ':' type { $$ = MK(cog::Param, @$, false, cog::take_str($1), $3); }
  | KW_COMPTIME IDENT ':' type { $$ = MK(cog::Param, @$, true, cog::take_str($2), $4); }
  ;

path
  : path_segments { $$ = MK(cog::Path, @$, take_vec($1)); }
  ;

path_segments
  : path_seg { $$ = vec1($1); }
  | path_segments TOK_COLONCOLON path_seg { $$ = vec_push($1, $3); }
  ;

path_seg
  : IDENT { $$ = MK(cog::Ident, @$, cog::take_str($1)); }
  | KW_SELF_TYPE { $$ = MK(cog::Ident, @$, std::string("Self")); }
  ;

qpath
  : qpath_segments { $$ = MK(cog::Path, @$, take_vec($1)); }
  ;

qpath_segments
  : path_seg TOK_COLONCOLON path_seg {
      auto* v = vec1($1);
      $$ = vec_push(v, $3);
    }
  | qpath_segments TOK_COLONCOLON path_seg { $$ = vec_push($1, $3); }
  ;

type
  : path { $$ = MK(cog::TypePath, @$, $1); }
  | KW_TYPE { $$ = MK(cog::TypeType, @$); }
  | KW_CONST '*' type { $$ = MK(cog::TypePtr, @$, cog::Mutability::Const, $3); }
  | KW_MUT '*' type { $$ = MK(cog::TypePtr, @$, cog::Mutability::Mut, $3); }
  | KW_DYN path { $$ = MK(cog::TypeDyn, @$, $2); }
  | '[' type ']' { $$ = MK(cog::TypeSlice, @$, $2); }
  | '[' type ';' expr ']' { $$ = MK(cog::TypeArray, @$, $2, $4); }
  | '(' ')' { $$ = MK(cog::TypeUnit, @$); }
  | '(' type ')' { $$ = $2; }
  | '(' type ',' types_opt ')' {
      std::vector<cog::Type*> elems;
      elems.push_back($2);
      for (cog::Type* t : take_vec($4)) elems.push_back(t);
      $$ = MK(cog::TypeTuple, @$, std::move(elems));
    }
  ;

types_opt
  : /* empty */ { $$ = vec_new<cog::Type>(); }
  | types { $$ = $1; }
  ;

types
  : types ',' type { $$ = vec_push($1, $3); }
  | types ',' { $$ = $1; }
  | type { $$ = vec1($1); }
  ;

ret_opt
  : /* empty */ { $$ = nullptr; }
  | TOK_ARROW type { $$ = $2; }
  ;

fields_opt
  : /* empty */ { $$ = vec_new<cog::FieldDecl>(); }
  | fields { $$ = $1; }
  ;

fields
  : fields field_decl { $$ = vec_push($1, $2); }
  | field_decl { $$ = vec1($1); }
  ;

field_decl
  : vis_opt IDENT ':' type ',' {
      $$ = MK(cog::FieldDecl, @$, std::vector<cog::Attr*>{}, $1, cog::take_str($2), $4);
    }
  | vis_opt IDENT ':' type {
      $$ = MK(cog::FieldDecl, @$, std::vector<cog::Attr*>{}, $1, cog::take_str($2), $4);
    }
  ;

variants_opt
  : /* empty */ { $$ = vec_new<cog::VariantDecl>(); }
  | variants { $$ = $1; }
  ;

variants
  : variants variant_decl { $$ = vec_push($1, $2); }
  | variant_decl { $$ = vec1($1); }
  ;

variant_decl
  : IDENT ',' { $$ = MK(cog::VariantDecl, @$, cog::take_str($1), std::vector<cog::Type*>{}); }
  | IDENT { $$ = MK(cog::VariantDecl, @$, cog::take_str($1), std::vector<cog::Type*>{}); }
  | IDENT '(' types_opt ')' ',' {
      $$ = MK(cog::VariantDecl, @$, cog::take_str($1), take_vec($3));
    }
  | IDENT '(' types_opt ')' {
      $$ = MK(cog::VariantDecl, @$, cog::take_str($1), take_vec($3));
    }
  ;

block
  : '{' block_elems_opt '}' {
      if ($2) {
        $2->span = SPAN(@$);
        $$ = $2;
      } else {
        $$ = MK(cog::Block, @$, std::vector<cog::Stmt*>{}, nullptr);
      }
    }
  ;

block_elems_opt
  : /* empty */ { $$ = nullptr; }
  | block_elems { $$ = $1; }
  ;

stmts
  : stmts stmt { $$ = vec_push($1, $2); }
  | stmt { $$ = vec1($1); }
  ;

block_elems
  : stmts expr { $$ = MK(cog::Block, @$, take_vec($1), $2); }
  | stmts { $$ = MK(cog::Block, @$, take_vec($1), nullptr); }
  | expr { $$ = MK(cog::Block, @$, std::vector<cog::Stmt*>{}, $1); }
  ;

stmt
  : let_stmt ';' { $$ = $1; }
  | expr ';' { $$ = MK(cog::StmtExpr, @$, $1); }
  | KW_RETURN ';' { $$ = MK(cog::StmtReturn, @$, nullptr); }
  | KW_RETURN expr ';' { $$ = MK(cog::StmtReturn, @$, $2); }
  | KW_BREAK ';' { $$ = MK(cog::StmtBreak, @$, nullptr); }
  | KW_BREAK expr ';' { $$ = MK(cog::StmtBreak, @$, $2); }
  | KW_CONTINUE ';' { $$ = MK(cog::StmtContinue, @$); }
  ;

let_stmt
  : KW_LET pattern type_ann_opt init_opt {
      $$ = MK(cog::StmtLet, @$, $2, $3, $4);
    }
  ;

type_ann_opt
  : /* empty */ { $$ = nullptr; }
  | ':' type { $$ = $2; }
  ;

init_opt
  : /* empty */ { $$ = nullptr; }
  | '=' expr { $$ = $2; }
  ;

expr_or_block
  : expr { $$ = $1; }
  ;

else_opt
  : /* empty */ { $$ = nullptr; }
  | KW_ELSE expr_or_block { $$ = $2; }
  ;

guard_opt
  : /* empty */ { $$ = nullptr; }
  | KW_IF expr { $$ = $2; }
  ;

expr
  : expr_nostruct { $$ = $1; }
  | path '{' field_inits_opt '}' {
      $$ = MK(cog::ExprStructLit, @$, $1, take_vec($3));
    }
  ;

expr_nostruct
  : assign_expr { $$ = $1; }
  ;

assign_expr
  : logical_or_expr { $$ = $1; }
  | postfix_expr '=' assign_expr { $$ = MK(cog::ExprAssign, @$, $1, $3); }
  ;

logical_or_expr
  : logical_or_expr TOK_OROR logical_and_expr { $$ = MK(cog::ExprBinary, @$, cog::BinaryOp::Or, $1, $3); }
  | logical_and_expr { $$ = $1; }
  ;

logical_and_expr
  : logical_and_expr TOK_ANDAND equality_expr { $$ = MK(cog::ExprBinary, @$, cog::BinaryOp::And, $1, $3); }
  | equality_expr { $$ = $1; }
  ;

equality_expr
  : equality_expr TOK_EQEQ relational_expr { $$ = MK(cog::ExprBinary, @$, cog::BinaryOp::Eq, $1, $3); }
  | equality_expr TOK_NEQ relational_expr { $$ = MK(cog::ExprBinary, @$, cog::BinaryOp::Ne, $1, $3); }
  | relational_expr { $$ = $1; }
  ;

relational_expr
  : relational_expr '<' additive_expr { $$ = MK(cog::ExprBinary, @$, cog::BinaryOp::Lt, $1, $3); }
  | relational_expr '>' additive_expr { $$ = MK(cog::ExprBinary, @$, cog::BinaryOp::Gt, $1, $3); }
  | relational_expr TOK_LE additive_expr { $$ = MK(cog::ExprBinary, @$, cog::BinaryOp::Le, $1, $3); }
  | relational_expr TOK_GE additive_expr { $$ = MK(cog::ExprBinary, @$, cog::BinaryOp::Ge, $1, $3); }
  | additive_expr { $$ = $1; }
  ;

additive_expr
  : additive_expr '+' multiplicative_expr { $$ = MK(cog::ExprBinary, @$, cog::BinaryOp::Add, $1, $3); }
  | additive_expr '-' multiplicative_expr { $$ = MK(cog::ExprBinary, @$, cog::BinaryOp::Sub, $1, $3); }
  | multiplicative_expr { $$ = $1; }
  ;

multiplicative_expr
  : multiplicative_expr '*' cast_expr { $$ = MK(cog::ExprBinary, @$, cog::BinaryOp::Mul, $1, $3); }
  | multiplicative_expr '/' cast_expr { $$ = MK(cog::ExprBinary, @$, cog::BinaryOp::Div, $1, $3); }
  | multiplicative_expr '%' cast_expr { $$ = MK(cog::ExprBinary, @$, cog::BinaryOp::Mod, $1, $3); }
  | cast_expr { $$ = $1; }
  ;

cast_expr
  : cast_expr KW_AS type { $$ = MK(cog::ExprCast, @$, $1, $3); }
  | unary_expr { $$ = $1; }
  ;

unary_expr
  : '-' unary_expr { $$ = MK(cog::ExprUnary, @$, cog::UnaryOp::Neg, $2); }
  | '*' unary_expr { $$ = MK(cog::ExprUnary, @$, cog::UnaryOp::Deref, $2); }
  | '!' unary_expr { $$ = MK(cog::ExprUnary, @$, cog::UnaryOp::Not, $2); }
  | postfix_expr { $$ = $1; }
  ;

postfix_expr
  : primary_expr { $$ = $1; }
  | postfix_expr '(' args_opt ')' { $$ = MK(cog::ExprCall, @$, $1, take_vec($3)); }
  | postfix_expr '.' IDENT { $$ = MK(cog::ExprField, @$, $1, cog::take_str($3)); }
  | postfix_expr '.' IDENT '(' args_opt ')' { $$ = MK(cog::ExprMethodCall, @$, $1, cog::take_str($3), take_vec($5)); }
  | postfix_expr '[' expr ']' { $$ = MK(cog::ExprIndex, @$, $1, $3); }
  ;

primary_expr
  : INT { $$ = MK(cog::ExprInt, @$, static_cast<std::int64_t>($1)); }
  | STRING { $$ = MK(cog::ExprString, @$, cog::take_str($1)); }
  | KW_TRUE { $$ = MK(cog::ExprBool, @$, true); }
  | KW_FALSE { $$ = MK(cog::ExprBool, @$, false); }
  | '(' ')' { $$ = MK(cog::ExprUnit, @$); }
  | '(' expr ')' { $$ = $2; }
  | '(' expr ',' args_opt ')' {
      std::vector<cog::Expr*> elems;
      elems.push_back($2);
      for (cog::Expr* e : take_vec($4)) elems.push_back(e);
      $$ = MK(cog::ExprTuple, @$, std::move(elems));
    }
  | path { $$ = MK(cog::ExprPath, @$, $1); }
  | block { $$ = MK(cog::ExprBlock, @$, $1); }
  | KW_COMPTIME block { $$ = MK(cog::ExprComptime, @$, $2); }
  | KW_IF expr_nostruct block else_opt { $$ = MK(cog::ExprIf, @$, $2, $3, $4); }
  | KW_WHILE expr_nostruct block { $$ = MK(cog::ExprWhile, @$, $2, $3); }
  | KW_LOOP block { $$ = MK(cog::ExprLoop, @$, $2); }
  | KW_MATCH expr_nostruct '{' match_arms_opt '}' { $$ = MK(cog::ExprMatch, @$, $2, take_vec($4)); }
  ;

args_opt
  : /* empty */ { $$ = vec_new<cog::Expr>(); }
  | args { $$ = $1; }
  ;

args
  : args ',' expr { $$ = vec_push($1, $3); }
  | args ',' { $$ = $1; }
  | expr { $$ = vec1($1); }
  ;

field_inits_opt
  : /* empty */ { $$ = vec_new<cog::FieldInit>(); }
  | field_inits { $$ = $1; }
  ;

field_inits
  : field_inits ',' field_init { $$ = vec_push($1, $3); }
  | field_inits ',' { $$ = $1; }
  | field_init { $$ = vec1($1); }
  ;

field_init
  : IDENT ':' expr { $$ = MK(cog::FieldInit, @$, cog::take_str($1), $3); }
  ;

match_arms_opt
  : /* empty */ { $$ = vec_new<cog::MatchArm>(); }
  | match_arms { $$ = $1; }
  ;

match_arms
  : match_arms match_arm { $$ = vec_push($1, $2); }
  | match_arm { $$ = vec1($1); }
  ;

match_arm
  : pattern guard_opt TOK_FATARROW expr_or_block ',' {
      $$ = MK(cog::MatchArm, @$, $1, $2, $4);
    }
  | pattern guard_opt TOK_FATARROW expr_or_block {
      $$ = MK(cog::MatchArm, @$, $1, $2, $4);
    }
  ;

pattern
  : pattern_primary { $$ = $1; }
  | pattern '|' pattern_primary { $$ = MK(cog::PatOr, @$, $1, $3); }
  ;

pattern_primary
  : '_' { $$ = MK(cog::PatWildcard, @$); }
  | INT { $$ = MK(cog::PatInt, @$, static_cast<std::int64_t>($1)); }
  | KW_TRUE { $$ = MK(cog::PatBool, @$, true); }
  | KW_FALSE { $$ = MK(cog::PatBool, @$, false); }
  | KW_MUT IDENT { $$ = MK(cog::PatBinding, @$, true, cog::take_str($2)); }
  | IDENT { $$ = MK(cog::PatBinding, @$, false, cog::take_str($1)); }
  | '(' ')' { $$ = MK(cog::PatTuple, @$, std::vector<cog::Pattern*>{}); }
  | '(' pattern ')' { $$ = $2; }
  | '(' pattern ',' pat_list_opt ')' {
      std::vector<cog::Pattern*> elems;
      elems.push_back($2);
      for (cog::Pattern* p : take_vec($4)) elems.push_back(p);
      $$ = MK(cog::PatTuple, @$, std::move(elems));
    }
  | qpath '(' pat_list_opt ')' {
      $$ = MK(cog::PatVariant, @$, $1, take_vec($3));
    }
  | qpath '{' pat_fields_opt '}' {
      $$ = MK(cog::PatStruct, @$, $1, take_vec($3));
    }
  | path '(' pat_list_opt ')' {
      $$ = MK(cog::PatVariant, @$, $1, take_vec($3));
    }
  | path '{' pat_fields_opt '}' {
      $$ = MK(cog::PatStruct, @$, $1, take_vec($3));
    }
  | qpath { $$ = MK(cog::PatPath, @$, $1); }
  ;

pat_list_opt
  : /* empty */ { $$ = vec_new<cog::Pattern>(); }
  | pat_list { $$ = $1; }
  ;

pat_list
  : pat_list ',' pattern { $$ = vec_push($1, $3); }
  | pat_list ',' { $$ = $1; }
  | pattern { $$ = vec1($1); }
  ;

pat_fields_opt
  : /* empty */ { $$ = vec_new<cog::PatField>(); }
  | pat_fields { $$ = $1; }
  ;

pat_fields
  : pat_fields ',' pat_field { $$ = vec_push($1, $3); }
  | pat_fields ',' { $$ = $1; }
  | pat_field { $$ = vec1($1); }
  ;

pat_field
  : IDENT ':' pattern { $$ = MK(cog::PatField, @$, cog::take_str($1), $3); }
  ;

%%

int yyerror(const char* msg) {
  extern YYLTYPE yylloc;
  if (!cog::g_parse_state) return 0;
  cog::push_error(SPAN(yylloc), msg);
  return 0;
}
