%{
#include "parse_state.hpp"

#include <cstdio>
#include <string>
#include <utility>

extern int yylex(void);
extern int yylineno;
int yyerror(const char* msg);

static std::vector<cog::Node*>* vec_new() { return new std::vector<cog::Node*>(); }
static std::vector<cog::Node*>* vec1(cog::Node* n) {
  auto* v = new std::vector<cog::Node*>();
  v->push_back(n);
  return v;
}
static std::vector<cog::Node*>* vec_push(std::vector<cog::Node*>* v, cog::Node* n) {
  v->push_back(n);
  return v;
}
static cog::Node* node_from_vec(cog::NodeKind kind, std::vector<cog::Node*>* v) {
  std::vector<cog::Node*> children = std::move(*v);
  delete v;
  return cog::mk(kind, std::move(children));
}
static cog::Node* node_from_vec(cog::NodeKind kind, std::string text, std::vector<cog::Node*>* v) {
  std::vector<cog::Node*> children = std::move(*v);
  delete v;
  return cog::mk(kind, std::move(text), std::move(children));
}

static void parse_error(const char* msg) {
  cog::push_error(std::to_string(yylineno) + ": " + msg);
}
%}

%union {
  long long int_val;
  char* str;
  cog::Node* node;
  std::vector<cog::Node*>* nodes;
}

%token <str> IDENT
%token <str> STRING
%token <int_val> INT

%token KW_FN KW_STRUCT KW_ENUM KW_TRAIT KW_IMPL KW_FOR KW_TYPE KW_CONST KW_STATIC
%token KW_MOD KW_USE KW_PUB KW_AS KW_LET KW_MUT KW_IF KW_ELSE KW_WHILE
%token KW_LOOP KW_MATCH KW_RETURN KW_BREAK KW_CONTINUE KW_COMPTIME KW_DYN KW_SELF_TYPE

%token TOK_COLONCOLON TOK_ARROW TOK_FATARROW TOK_EQEQ TOK_NEQ TOK_LE TOK_GE TOK_ANDAND TOK_OROR

%type <node> program item item_core vis_opt attr path path_seg type ret_opt fn_sig
%type <node> block stmt let_stmt expr expr_nostruct expr_or_block pattern match_arm
%type <node> field variant param field_init
%type <nodes> items attrs_opt fields_opt fields variants_opt variants
%type <nodes> params_opt params args_opt args field_inits_opt field_inits
%type <nodes> impl_items_opt impl_items trait_items_opt trait_items
%type <nodes> match_arms_opt match_arms block_elems_opt block_elems stmts types_opt types
%type <nodes> pat_list_opt pat_list

%start program
%error-verbose

%right '='
%left TOK_OROR
%left TOK_ANDAND
%left TOK_EQEQ TOK_NEQ
%left '<' '>' TOK_LE TOK_GE
%left '+' '-'
%left '*' '/' '%'
%left KW_AS
%right UMINUS

%destructor { free($$); } IDENT STRING
%destructor { delete $$; } items attrs_opt fields_opt fields variants_opt variants params_opt params args_opt args field_inits_opt field_inits impl_items_opt impl_items trait_items_opt trait_items match_arms_opt match_arms block_elems_opt block_elems stmts types_opt types pat_list_opt pat_list

%%

program
  : items {
      cog::g_parse_state->root = node_from_vec(cog::NodeKind::Program, $1);
      $$ = cog::g_parse_state->root;
    }
  ;

items
  : /* empty */ { $$ = vec_new(); }
  | items item { $$ = vec_push($1, $2); }
  ;

item
  : attrs_opt vis_opt item_core {
      cog::Node* attrs = node_from_vec(cog::NodeKind::List, $1);
      $$ = cog::mk(cog::NodeKind::Item, std::vector<cog::Node*>{attrs, $2, $3});
    }
  ;

vis_opt
  : /* empty */ { $$ = cog::mk(cog::NodeKind::Vis, "private"); }
  | KW_PUB { $$ = cog::mk(cog::NodeKind::Vis, "pub"); }
  ;

attrs_opt
  : /* empty */ { $$ = vec_new(); }
  | attrs_opt attr { $$ = vec_push($1, $2); }
  ;

attr
  : '#' '[' path ']' { $$ = cog::mk(cog::NodeKind::Attr, std::vector<cog::Node*>{$3}); }
  | '#' '[' path '(' path ')' ']' { $$ = cog::mk(cog::NodeKind::Attr, std::vector<cog::Node*>{$3, $5}); }
  ;

item_core
  : KW_USE path ';' { $$ = cog::mk(cog::NodeKind::ItemUse, std::vector<cog::Node*>{$2}); }
  | KW_MOD IDENT '{' items '}' {
      cog::Node* body = node_from_vec(cog::NodeKind::List, $4);
      $$ = cog::mk(cog::NodeKind::ItemMod, cog::take_str($2), std::vector<cog::Node*>{body});
    }
  | KW_STRUCT IDENT '{' fields_opt '}' {
      cog::Node* fs = node_from_vec(cog::NodeKind::List, $4);
      $$ = cog::mk(cog::NodeKind::ItemStruct, cog::take_str($2), std::vector<cog::Node*>{fs});
    }
  | KW_ENUM IDENT '{' variants_opt '}' {
      cog::Node* vs = node_from_vec(cog::NodeKind::List, $4);
      $$ = cog::mk(cog::NodeKind::ItemEnum, cog::take_str($2), std::vector<cog::Node*>{vs});
    }
  | KW_TRAIT IDENT '{' trait_items_opt '}' {
      cog::Node* ms = node_from_vec(cog::NodeKind::List, $4);
      $$ = cog::mk(cog::NodeKind::ItemTrait, cog::take_str($2), std::vector<cog::Node*>{ms});
    }
  | KW_IMPL path '{' impl_items_opt '}' {
      cog::Node* ms = node_from_vec(cog::NodeKind::List, $4);
      $$ = cog::mk(cog::NodeKind::ItemImpl, std::vector<cog::Node*>{$2, ms});
    }
  | KW_IMPL path KW_FOR path '{' impl_items_opt '}' {
      cog::Node* ms = node_from_vec(cog::NodeKind::List, $6);
      $$ = cog::mk(cog::NodeKind::ItemImpl, std::vector<cog::Node*>{$2, $4, ms});
    }
  | KW_FN IDENT '(' params_opt ')' ret_opt block {
      cog::Node* ps = node_from_vec(cog::NodeKind::List, $4);
      std::vector<cog::Node*> children;
      children.push_back(ps);
      if ($6) children.push_back($6);
      children.push_back($7);
      $$ = cog::mk(cog::NodeKind::ItemFn, cog::take_str($2), std::move(children));
    }
  | KW_CONST IDENT ':' type '=' expr ';' {
      $$ = cog::mk(cog::NodeKind::ItemConst, cog::take_str($2), std::vector<cog::Node*>{$4, $6});
    }
  ;

ret_opt
  : /* empty */ { $$ = nullptr; }
  | TOK_ARROW type { $$ = $2; }
  ;

trait_items_opt
  : /* empty */ { $$ = vec_new(); }
  | trait_items { $$ = $1; }
  ;

trait_items
  : trait_items fn_sig ';' { $$ = vec_push($1, $2); }
  | fn_sig ';' { $$ = vec1($1); }
  ;

impl_items_opt
  : /* empty */ { $$ = vec_new(); }
  | impl_items { $$ = $1; }
  ;

impl_items
  : impl_items vis_opt KW_FN IDENT '(' params_opt ')' ret_opt block {
      cog::Node* ps = node_from_vec(cog::NodeKind::List, $6);
      std::vector<cog::Node*> children;
      children.push_back($2);
      children.push_back(ps);
      if ($8) children.push_back($8);
      children.push_back($9);
      $$ = vec_push($1, cog::mk(cog::NodeKind::ItemFn, cog::take_str($4), std::move(children)));
    }
  | vis_opt KW_FN IDENT '(' params_opt ')' ret_opt block {
      cog::Node* ps = node_from_vec(cog::NodeKind::List, $5);
      std::vector<cog::Node*> children;
      children.push_back($1);
      children.push_back(ps);
      if ($7) children.push_back($7);
      children.push_back($8);
      $$ = vec1(cog::mk(cog::NodeKind::ItemFn, cog::take_str($3), std::move(children)));
    }
  ;

fn_sig
  : KW_FN IDENT '(' params_opt ')' ret_opt {
      cog::Node* ps = node_from_vec(cog::NodeKind::List, $4);
      std::vector<cog::Node*> children;
      children.push_back(ps);
      if ($6) children.push_back($6);
      $$ = cog::mk(cog::NodeKind::ItemFn, cog::take_str($2), std::move(children));
    }
  ;

fields_opt
  : /* empty */ { $$ = vec_new(); }
  | fields { $$ = $1; }
  ;

fields
  : fields field { $$ = vec_push($1, $2); }
  | field { $$ = vec1($1); }
  ;

field
  : attrs_opt vis_opt IDENT ':' type ',' {
      cog::Node* attrs = node_from_vec(cog::NodeKind::List, $1);
      $$ = cog::mk(cog::NodeKind::Field, cog::take_str($3), std::vector<cog::Node*>{attrs, $2, $5});
    }
  | attrs_opt vis_opt IDENT ':' type {
      cog::Node* attrs = node_from_vec(cog::NodeKind::List, $1);
      $$ = cog::mk(cog::NodeKind::Field, cog::take_str($3), std::vector<cog::Node*>{attrs, $2, $5});
    }
  ;

variants_opt
  : /* empty */ { $$ = vec_new(); }
  | variants { $$ = $1; }
  ;

variants
  : variants variant { $$ = vec_push($1, $2); }
  | variant { $$ = vec1($1); }
  ;

types_opt
  : /* empty */ { $$ = vec_new(); }
  | types { $$ = $1; }
  ;

types
  : types ',' type { $$ = vec_push($1, $3); }
  | types ',' { $$ = $1; }
  | type { $$ = vec1($1); }
  ;

variant
  : IDENT ',' { $$ = cog::mk(cog::NodeKind::Variant, cog::take_str($1)); }
  | IDENT { $$ = cog::mk(cog::NodeKind::Variant, cog::take_str($1)); }
  | IDENT '(' types_opt ')' ',' {
      cog::Node* ts = node_from_vec(cog::NodeKind::List, $3);
      $$ = cog::mk(cog::NodeKind::Variant, cog::take_str($1), std::vector<cog::Node*>{ts});
    }
  | IDENT '(' types_opt ')' {
      cog::Node* ts = node_from_vec(cog::NodeKind::List, $3);
      $$ = cog::mk(cog::NodeKind::Variant, cog::take_str($1), std::vector<cog::Node*>{ts});
    }
  ;

params_opt
  : /* empty */ { $$ = vec_new(); }
  | params { $$ = $1; }
  ;

params
  : params ',' param { $$ = vec_push($1, $3); }
  | params ',' { $$ = $1; }
  | param { $$ = vec1($1); }
  ;

param
  : IDENT ':' type { $$ = cog::mk(cog::NodeKind::Param, cog::take_str($1), std::vector<cog::Node*>{$3}); }
  | KW_COMPTIME IDENT ':' type {
      $$ = cog::mk(
          cog::NodeKind::Param,
          cog::take_str($2),
          std::vector<cog::Node*>{cog::mk(cog::NodeKind::ExprComptime, "comptime"), $4});
    }
  ;

path
  : path_seg { $$ = cog::mk(cog::NodeKind::Path, std::vector<cog::Node*>{$1}); }
  | path TOK_COLONCOLON path_seg {
      $1->children.push_back($3);
      $$ = $1;
    }
  ;

path_seg
  : IDENT { $$ = cog::mk(cog::NodeKind::Ident, cog::take_str($1)); }
  | KW_SELF_TYPE { $$ = cog::mk(cog::NodeKind::Ident, "Self"); }
  ;

type
  : path { $$ = cog::mk(cog::NodeKind::TypePath, std::vector<cog::Node*>{$1}); }
  | KW_CONST '*' type { $$ = cog::mk(cog::NodeKind::TypePtr, "const", std::vector<cog::Node*>{$3}); }
  | KW_MUT '*' type { $$ = cog::mk(cog::NodeKind::TypePtr, "mut", std::vector<cog::Node*>{$3}); }
  | KW_DYN path { $$ = cog::mk(cog::NodeKind::TypeDyn, std::vector<cog::Node*>{$2}); }
  | '[' type ']' { $$ = cog::mk(cog::NodeKind::TypeSlice, std::vector<cog::Node*>{$2}); }
  | '[' type ';' expr ']' { $$ = cog::mk(cog::NodeKind::TypeArray, std::vector<cog::Node*>{$2, $4}); }
  | '(' ')' { $$ = cog::mk(cog::NodeKind::TypeUnit); }
  ;

block
  : '{' block_elems_opt '}' {
      std::vector<cog::Node*> children = std::move(*$2);
      delete $2;
      $$ = cog::mk(cog::NodeKind::Block, std::move(children));
    }
  ;

block_elems_opt
  : /* empty */ { $$ = vec_new(); }
  | block_elems { $$ = $1; }
  ;

block_elems
  : stmts expr { $$ = vec_push($1, $2); }
  | stmts { $$ = $1; }
  | expr { $$ = vec1($1); }
  ;

stmts
  : stmts stmt { $$ = vec_push($1, $2); }
  | stmt { $$ = vec1($1); }
  ;

stmt
  : let_stmt ';' { $$ = $1; }
  | expr ';' { $$ = cog::mk(cog::NodeKind::StmtExpr, std::vector<cog::Node*>{$1}); }
  | KW_RETURN ';' { $$ = cog::mk(cog::NodeKind::StmtReturn); }
  | KW_RETURN expr ';' { $$ = cog::mk(cog::NodeKind::StmtReturn, std::vector<cog::Node*>{$2}); }
  ;

let_stmt
  : KW_LET IDENT ':' type '=' expr {
      $$ = cog::mk(cog::NodeKind::StmtLet, cog::take_str($2), std::vector<cog::Node*>{$4, $6});
    }
  | KW_LET KW_MUT IDENT ':' type '=' expr {
      $$ = cog::mk(
          cog::NodeKind::StmtLet,
          cog::take_str($3),
          std::vector<cog::Node*>{cog::mk(cog::NodeKind::Ident, "mut"), $5, $7});
    }
  ;

expr_or_block
  : expr { $$ = $1; }
  ;

expr
  : expr_nostruct { $$ = $1; }
  | path '{' field_inits_opt '}' {
      cog::Node* inits = node_from_vec(cog::NodeKind::List, $3);
      $$ = cog::mk(cog::NodeKind::ExprStructLit, std::vector<cog::Node*>{$1, inits});
    }
  ;

expr_nostruct
  : INT { $$ = cog::mk(cog::NodeKind::ExprInt, std::to_string($1)); }
  | STRING { $$ = cog::mk(cog::NodeKind::ExprString, cog::take_str($1)); }
  | '(' ')' { $$ = cog::mk(cog::NodeKind::ExprUnit); }
  | path { $$ = cog::mk(cog::NodeKind::ExprPath, std::vector<cog::Node*>{$1}); }
  | block { $$ = cog::mk(cog::NodeKind::ExprBlock, std::vector<cog::Node*>{$1}); }
  | KW_COMPTIME block { $$ = cog::mk(cog::NodeKind::ExprComptime, std::vector<cog::Node*>{$2}); }
  | KW_MATCH expr_nostruct '{' match_arms_opt '}' {
      cog::Node* arms = node_from_vec(cog::NodeKind::List, $4);
      $$ = cog::mk(cog::NodeKind::ExprMatch, std::vector<cog::Node*>{$2, arms});
    }
  | expr_nostruct '=' expr { $$ = cog::mk(cog::NodeKind::ExprAssign, std::vector<cog::Node*>{$1, $3}); }
  | expr_nostruct '+' expr { $$ = cog::mk(cog::NodeKind::ExprBinary, "+", std::vector<cog::Node*>{$1, $3}); }
  | expr_nostruct '-' expr { $$ = cog::mk(cog::NodeKind::ExprBinary, "-", std::vector<cog::Node*>{$1, $3}); }
  | expr_nostruct '*' expr { $$ = cog::mk(cog::NodeKind::ExprBinary, "*", std::vector<cog::Node*>{$1, $3}); }
  | expr_nostruct '/' expr { $$ = cog::mk(cog::NodeKind::ExprBinary, "/", std::vector<cog::Node*>{$1, $3}); }
  | expr_nostruct KW_AS type { $$ = cog::mk(cog::NodeKind::ExprCast, std::vector<cog::Node*>{$1, $3}); }
  | '-' expr %prec UMINUS { $$ = cog::mk(cog::NodeKind::ExprUnary, "-", std::vector<cog::Node*>{$2}); }
  | '*' expr %prec UMINUS { $$ = cog::mk(cog::NodeKind::ExprUnary, "*", std::vector<cog::Node*>{$2}); }
  | expr_nostruct '(' args_opt ')' {
      cog::Node* as = node_from_vec(cog::NodeKind::List, $3);
      $$ = cog::mk(cog::NodeKind::ExprCall, std::vector<cog::Node*>{$1, as});
    }
  | expr_nostruct '.' IDENT {
      $$ = cog::mk(cog::NodeKind::ExprField, cog::take_str($3), std::vector<cog::Node*>{$1});
    }
  | expr_nostruct '.' IDENT '(' args_opt ')' {
      cog::Node* as = node_from_vec(cog::NodeKind::List, $5);
      $$ = cog::mk(cog::NodeKind::ExprMethodCall, cog::take_str($3), std::vector<cog::Node*>{$1, as});
    }
  | expr_nostruct '[' expr ']' { $$ = cog::mk(cog::NodeKind::ExprIndex, std::vector<cog::Node*>{$1, $3}); }
  | '(' expr ')' { $$ = $2; }
  ;

args_opt
  : /* empty */ { $$ = vec_new(); }
  | args { $$ = $1; }
  ;

args
  : args ',' expr { $$ = vec_push($1, $3); }
  | args ',' { $$ = $1; }
  | expr { $$ = vec1($1); }
  ;

field_inits_opt
  : /* empty */ { $$ = vec_new(); }
  | field_inits { $$ = $1; }
  ;

field_inits
  : field_inits ',' field_init { $$ = vec_push($1, $3); }
  | field_inits ',' { $$ = $1; }
  | field_init { $$ = vec1($1); }
  ;

field_init
  : IDENT ':' expr { $$ = cog::mk(cog::NodeKind::Field, cog::take_str($1), std::vector<cog::Node*>{$3}); }
  ;

match_arms_opt
  : /* empty */ { $$ = vec_new(); }
  | match_arms { $$ = $1; }
  ;

match_arms
  : match_arms match_arm { $$ = vec_push($1, $2); }
  | match_arm { $$ = vec1($1); }
  ;

match_arm
  : pattern TOK_FATARROW expr_or_block ',' { $$ = cog::mk(cog::NodeKind::MatchArm, std::vector<cog::Node*>{$1, $3}); }
  | pattern TOK_FATARROW expr_or_block { $$ = cog::mk(cog::NodeKind::MatchArm, std::vector<cog::Node*>{$1, $3}); }
  ;

pattern
  : '_' { $$ = cog::mk(cog::NodeKind::PatWildcard); }
  | INT { $$ = cog::mk(cog::NodeKind::PatInt, std::to_string($1)); }
  | path { $$ = cog::mk(cog::NodeKind::PatPath, std::vector<cog::Node*>{$1}); }
  | path '(' pat_list_opt ')' {
      cog::Node* ps = node_from_vec(cog::NodeKind::List, $3);
      $$ = cog::mk(cog::NodeKind::PatVariant, std::vector<cog::Node*>{$1, ps});
    }
  | pattern '|' pattern { $$ = cog::mk(cog::NodeKind::PatOr, std::vector<cog::Node*>{$1, $3}); }
  ;

pat_list_opt
  : /* empty */ { $$ = vec_new(); }
  | pat_list { $$ = $1; }
  ;

pat_list
  : pat_list ',' pattern { $$ = vec_push($1, $3); }
  | pat_list ',' { $$ = $1; }
  | pattern { $$ = vec1($1); }
  ;

%%

int yyerror(const char* msg) {
  parse_error(msg);
  return 0;
}
