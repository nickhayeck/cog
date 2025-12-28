#include "lower_mir.hpp"

#include <cstdint>
#include <limits>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

#include "diag.hpp"
#include "layout.hpp"
#include "resolve.hpp"

namespace cog {
namespace {

struct LoopCtx {
    MirBlockId break_bb = 0;
    MirBlockId continue_bb = 0;
};

struct Scope {
    std::unordered_map<std::string, MirLocalId> locals{};
};

class Lowerer {
   public:
    Lowerer(Session& session, const HirCrate& hir)
        : session_(session),
          hir_(hir),
          crate_(*hir.crate),
          checked_(*hir.checked),
          types_(const_cast<TypeStore&>(checked_.types)),
          layout_(session, types_, checked_.struct_info, checked_.enum_info,
                  checked_.array_lens, TargetLayout{}) {}

    std::optional<MirProgram> run() {
        MirProgram program{};
        program.hir = &hir_;
        program.types = &types_;
        program_ = &program;
        next_internal_def_ = static_cast<DefId>(hir_.defs.size());

        for (const HirDef& d : hir_.defs) {
            switch (d.kind) {
                case HirDefKind::Fn: {
                    auto* fn = static_cast<const ItemFn*>(d.ast);
                    if (!fn || !fn->decl || !fn->decl->sig) break;
                    if (!fn->body) break;  // extern decl

                    auto it = checked_.fn_info.find(fn);
                    if (it == checked_.fn_info.end()) break;

                    MirBody body = lower_fn_body(d.id, d.module, fn, it->second,
                                                 /*self_ty=*/std::nullopt);
                    MirBodyId bid =
                        static_cast<MirBodyId>(program.bodies.size());
                    body.id = bid;
                    program.fn_bodies.insert({d.id, bid});
                    program.bodies.push_back(std::move(body));
                    break;
                }
                case HirDefKind::Const: {
                    auto* c = static_cast<const ItemConst*>(d.ast);
                    if (!c || !c->value) break;
                    auto it = checked_.const_types.find(c);
                    if (it == checked_.const_types.end()) break;
                    TypeId ty = it->second;

                    MirBody body = lower_const_body(d.id, d.module, c, ty);
                    MirBodyId bid =
                        static_cast<MirBodyId>(program.bodies.size());
                    body.id = bid;
                    program.const_bodies.insert({d.id, bid});
                    program.bodies.push_back(std::move(body));
                    break;
                }
                case HirDefKind::Static: {
                    auto* s = static_cast<const ItemStatic*>(d.ast);
                    if (!s || !s->value) break;
                    auto it = checked_.static_types.find(s);
                    if (it == checked_.static_types.end()) break;
                    TypeId ty = it->second;

                    MirBody body = lower_static_body(d.id, d.module, s, ty);
                    MirBodyId bid =
                        static_cast<MirBodyId>(program.bodies.size());
                    body.id = bid;
                    program.static_bodies.insert({d.id, bid});
                    program.bodies.push_back(std::move(body));
                    break;
                }
                default:
                    break;
            }
        }

        // Methods (impl items) are represented as `HirDefKind::Fn` too; they
        // are already included above because `build_hir` registers them as
        // defs.

        program_ = nullptr;
        if (session_.has_errors()) return std::nullopt;
        return program;
    }

   private:
    Session& session_;
    const HirCrate& hir_;
    const ResolvedCrate& crate_;
    const CheckedCrate& checked_;
    TypeStore& types_;

    // Used for `builtin::type_info` evaluation during lowering of comptime-only
    // expressions (e.g. const initializers).
    LayoutEngine layout_;

    MirProgram* program_ = nullptr;
    DefId next_internal_def_ = 0;
    bool in_comptime_context_ = false;

    // Per-body lowering state.
    MirBody out_{};
    MirBlockId cur_bb_ = 0;
    MirBlockId return_bb_ = 0;
    std::vector<Scope> scopes_{};
    std::vector<LoopCtx> loops_{};

    void error(Span sp, std::string msg) {
        session_.diags.push_back(Diagnostic{.severity = Severity::Error,
                                            .span = sp,
                                            .message = std::move(msg)});
    }

    TypeId type_of(const Expr* e) const {
        if (!e) return types_.error();
        auto it = checked_.expr_types.find(e);
        if (it == checked_.expr_types.end()) return types_.error();
        return it->second;
    }

    TypeId type_of_binding(const Pattern* p) const {
        if (!p) return types_.error();
        if (p->kind != AstNodeKind::PatBinding) return types_.error();
        auto* b = static_cast<const PatBinding*>(p);
        auto it = checked_.binding_types.find(b);
        if (it == checked_.binding_types.end()) return types_.error();
        return it->second;
    }

    struct SavedBodyState {
        MirBody out{};
        MirBlockId cur_bb = 0;
        MirBlockId return_bb = 0;
        std::vector<Scope> scopes{};
        std::vector<LoopCtx> loops{};
        bool in_comptime_context = false;
    };

    SavedBodyState save_body_state() {
        SavedBodyState s{};
        s.out = std::move(out_);
        s.cur_bb = cur_bb_;
        s.return_bb = return_bb_;
        s.scopes = std::move(scopes_);
        s.loops = std::move(loops_);
        s.in_comptime_context = in_comptime_context_;
        return s;
    }

    void restore_body_state(SavedBodyState s) {
        out_ = std::move(s.out);
        cur_bb_ = s.cur_bb;
        return_bb_ = s.return_bb;
        scopes_ = std::move(s.scopes);
        loops_ = std::move(s.loops);
        in_comptime_context_ = s.in_comptime_context;
    }

    MirBody lower_expr_body(DefId owner_def, ModuleId mid, const Expr* expr,
                            TypeId ty) {
        SavedBodyState saved = save_body_state();
        in_comptime_context_ = true;

        out_ = MirBody{};
        out_.owner = owner_def;
        out_.span = expr ? expr->span : Span{};
        out_.ret_ty = ty;
        out_.comptime_params.clear();

        scopes_.clear();
        loops_.clear();
        push_scope();

        add_local(ty, "_ret");

        add_block();  // bb0
        return_bb_ = add_block();
        out_.start = 0;
        cur_bb_ = 0;

        MirOperand result = lower_expr(mid, expr);
        if (!has_terminator(cur_bb_)) {
            if (ty != types_.unit()) {
                emit_stmt(MirStatement{MirStatement::Assign{
                    .dst = MirPlace::local(0),
                    .src = MirRvalue{MirRvalue::Use{std::move(result)}},
                }});
            }
            terminate(MirTerminator{MirTerminator::Goto{return_bb_}});
        }

        start_new_block(return_bb_);
        terminate(MirTerminator{MirTerminator::Return{}});

        MirBody out = std::move(out_);
        restore_body_state(std::move(saved));
        return out;
    }

    MirBody lower_block_body(DefId owner_def, ModuleId mid, const Block* block,
                             TypeId ty, Span span) {
        SavedBodyState saved = save_body_state();
        in_comptime_context_ = true;

        out_ = MirBody{};
        out_.owner = owner_def;
        out_.span = span;
        out_.ret_ty = ty;
        out_.comptime_params.clear();

        scopes_.clear();
        loops_.clear();
        push_scope();

        add_local(ty, "_ret");

        add_block();  // bb0
        return_bb_ = add_block();
        out_.start = 0;
        cur_bb_ = 0;

        MirOperand result = lower_block_expr(mid, block);
        if (!has_terminator(cur_bb_)) {
            if (ty != types_.unit()) {
                emit_stmt(MirStatement{MirStatement::Assign{
                    .dst = MirPlace::local(0),
                    .src = MirRvalue{MirRvalue::Use{std::move(result)}},
                }});
            }
            terminate(MirTerminator{MirTerminator::Goto{return_bb_}});
        }

        start_new_block(return_bb_);
        terminate(MirTerminator{MirTerminator::Return{}});

        MirBody out = std::move(out_);
        restore_body_state(std::move(saved));
        return out;
    }

    MirOperand lower_as_internal_const_item(ModuleId mid, const Expr* expr) {
        if (!program_) return unit_operand();
        TypeId ty = type_of(expr);
        DefId def = next_internal_def_++;
        MirBody body = lower_expr_body(def, mid, expr, ty);
        MirBodyId bid = static_cast<MirBodyId>(program_->bodies.size());
        body.id = bid;
        program_->const_bodies.insert({def, bid});
        program_->bodies.push_back(std::move(body));
        return MirOperand{MirOperand::ConstItem{def}};
    }

    MirOperand lower_block_as_internal_const_item(ModuleId mid, const Block* b,
                                                  TypeId ty, Span span) {
        if (!program_) return unit_operand();
        DefId def = next_internal_def_++;
        MirBody body = lower_block_body(def, mid, b, ty, span);
        MirBodyId bid = static_cast<MirBodyId>(program_->bodies.size());
        body.id = bid;
        program_->const_bodies.insert({def, bid});
        program_->bodies.push_back(std::move(body));
        return MirOperand{MirOperand::ConstItem{def}};
    }

    MirLocalId add_local(TypeId ty, std::string name) {
        MirLocalId id = static_cast<MirLocalId>(out_.locals.size());
        out_.locals.push_back(MirLocal{.ty = ty, .name = std::move(name)});
        return id;
    }

    MirLocalId add_temp(TypeId ty) {
        return add_local(ty, "_t" + std::to_string(out_.locals.size()));
    }

    MirBlockId add_block() {
        MirBlockId id = static_cast<MirBlockId>(out_.blocks.size());
        out_.blocks.push_back(MirBlock{});
        return id;
    }

    MirBlock& cur_block() {
        return out_.blocks.at(static_cast<size_t>(cur_bb_));
    }

    void terminate(MirTerminator term) { cur_block().term = std::move(term); }

    void start_new_block(MirBlockId bb) { cur_bb_ = bb; }

    void emit_stmt(MirStatement st) {
        cur_block().stmts.push_back(std::move(st));
    }

    void push_scope() { scopes_.push_back(Scope{}); }
    void pop_scope() {
        if (!scopes_.empty()) scopes_.pop_back();
    }

    MirLocalId* lookup_local(std::string_view name) {
        for (auto it = scopes_.rbegin(); it != scopes_.rend(); ++it) {
            auto found = it->locals.find(std::string(name));
            if (found != it->locals.end()) return &found->second;
        }
        return nullptr;
    }

    void declare_local(std::string name, MirLocalId id) {
        if (scopes_.empty()) push_scope();
        scopes_.back().locals.insert({std::move(name), id});
    }

    // ---- Name resolution helpers ----
    const Item* resolve_type_item(ModuleId mid, const Path* path) const {
        if (!path || path->segments.empty()) return nullptr;
        ModuleId cur = mid;
        for (size_t i = 0; i + 1 < path->segments.size(); i++) {
            const Ident* seg = path->segments[i];
            if (!seg) return nullptr;
            auto it = crate_.modules[cur].submodules.find(seg->text);
            if (it == crate_.modules[cur].submodules.end()) return nullptr;
            cur = it->second;
        }
        const Ident* last = path->segments.back();
        if (!last) return nullptr;
        auto it = crate_.modules[cur].types.find(last->text);
        if (it == crate_.modules[cur].types.end()) return nullptr;
        return it->second;
    }

    const Item* resolve_value_item(ModuleId mid, const Path* path) const {
        if (!path || path->segments.empty()) return nullptr;
        ModuleId cur = mid;
        for (size_t i = 0; i + 1 < path->segments.size(); i++) {
            const Ident* seg = path->segments[i];
            if (!seg) return nullptr;
            auto it = crate_.modules[cur].submodules.find(seg->text);
            if (it == crate_.modules[cur].submodules.end()) return nullptr;
            cur = it->second;
        }
        const Ident* last = path->segments.back();
        if (!last) return nullptr;
        auto it = crate_.modules[cur].values.find(last->text);
        if (it == crate_.modules[cur].values.end()) return nullptr;
        return it->second;
    }

    const ItemFn* resolve_fn_path(ModuleId mid, const Path* path) const {
        const Item* item = resolve_value_item(mid, path);
        if (!item || item->kind != AstNodeKind::ItemFn) return nullptr;
        return static_cast<const ItemFn*>(item);
    }

    const ItemConst* resolve_const_path(ModuleId mid, const Path* path) const {
        const Item* item = resolve_value_item(mid, path);
        if (!item || item->kind != AstNodeKind::ItemConst) return nullptr;
        return static_cast<const ItemConst*>(item);
    }

    const ItemStatic* resolve_static_path(ModuleId mid,
                                          const Path* path) const {
        const Item* item = resolve_value_item(mid, path);
        if (!item || item->kind != AstNodeKind::ItemStatic) return nullptr;
        return static_cast<const ItemStatic*>(item);
    }

    struct ResolvedVariant {
        const ItemEnum* def = nullptr;
        const VariantDecl* decl = nullptr;
        std::uint32_t index = 0;
    };

    ResolvedVariant resolve_variant_path(ModuleId mid, const Path* path) const {
        if (!path || path->segments.size() < 2) return {};
        std::vector<Ident*> prefix_segs(path->segments.begin(),
                                        path->segments.end() - 1);
        Path prefix{path->span, std::move(prefix_segs)};
        const Item* type_item = resolve_type_item(mid, &prefix);
        if (!type_item || type_item->kind != AstNodeKind::ItemEnum) return {};
        const ItemEnum* def = static_cast<const ItemEnum*>(type_item);
        std::string_view vname = path->segments.back()->text;

        auto info_it = checked_.enum_info.find(def);
        if (info_it == checked_.enum_info.end()) return {};
        const EnumInfo& ei = info_it->second;
        for (size_t i = 0; i < ei.variants_in_order.size(); i++) {
            if (ei.variants_in_order[i] == vname) {
                auto vit = ei.variants.find(std::string(vname));
                const VariantDecl* vd =
                    vit != ei.variants.end() ? vit->second.ast : nullptr;
                return {.def = def,
                        .decl = vd,
                        .index = static_cast<std::uint32_t>(i)};
            }
        }
        return {.def = def, .decl = nullptr, .index = 0};
    }

    // ---- Type value lowering (for builtins) ----
    TypeId lower_type_value_expr(ModuleId mid, const Expr* e) {
        if (!e || e->kind != AstNodeKind::ExprPath) {
            error(e ? e->span : Span{}, "expected a type value (path)");
            return types_.error();
        }
        const Path* p = static_cast<const ExprPath*>(e)->path;
        return lower_type_path(mid, p);
    }

    TypeId lower_type_path(ModuleId mid, const Path* path) {
        if (!path || path->segments.empty()) return types_.error();
        if (path->segments.size() == 1 && path->segments[0]) {
            std::string_view name = path->segments[0]->text;
            if (name == "bool") return types_.bool_();
            if (auto ik = types_.parse_int_kind(name)) return types_.int_(*ik);
            if (auto fk = types_.parse_float_kind(name))
                return types_.float_(*fk);
        }

        const Item* item = resolve_type_item(mid, path);
        if (!item) {
            error(path->span, "cannot resolve type path");
            return types_.error();
        }
        if (item->kind == AstNodeKind::ItemStruct)
            return types_.struct_(static_cast<const ItemStruct*>(item));
        if (item->kind == AstNodeKind::ItemEnum)
            return types_.enum_(static_cast<const ItemEnum*>(item));
        if (item->kind == AstNodeKind::ItemTypeAlias) {
            auto* ta = static_cast<const ItemTypeAlias*>(item);
            if (!ta->aliased) return types_.error();
            return lower_type_syntax(mid, ta->aliased);
        }
        return types_.error();
    }

    TypeId lower_type_syntax(ModuleId mid, const Type* ty) {
        if (!ty) return types_.error();
        switch (ty->kind) {
            case AstNodeKind::TypePath:
                return lower_type_path(mid,
                                       static_cast<const TypePath*>(ty)->path);
            case AstNodeKind::TypePtr: {
                auto* p = static_cast<const TypePtr*>(ty);
                TypeId pointee = lower_type_syntax(mid, p->pointee);
                return types_.ptr(p->mutability, pointee);
            }
            case AstNodeKind::TypeSlice: {
                auto* s = static_cast<const TypeSlice*>(ty);
                TypeId elem = lower_type_syntax(mid, s->elem);
                return types_.slice(elem);
            }
            case AstNodeKind::TypeArray: {
                auto* a = static_cast<const TypeArray*>(ty);
                TypeId elem = lower_type_syntax(mid, a->elem);
                return types_.array(elem, a->len);
            }
            case AstNodeKind::TypeTuple: {
                auto* t = static_cast<const TypeTuple*>(ty);
                std::vector<TypeId> elems{};
                elems.reserve(t->elems.size());
                for (const Type* e : t->elems)
                    elems.push_back(lower_type_syntax(mid, e));
                return types_.tuple(std::move(elems));
            }
            case AstNodeKind::TypeFn: {
                auto* ft = static_cast<const TypeFn*>(ty);
                std::vector<TypeId> params{};
                params.reserve(ft->params.size());
                for (const Type* p : ft->params)
                    params.push_back(lower_type_syntax(mid, p));
                TypeId ret =
                    ft->ret ? lower_type_syntax(mid, ft->ret) : types_.unit();
                return types_.fn(std::move(params), ret);
            }
            case AstNodeKind::TypeUnit:
                return types_.unit();
            case AstNodeKind::TypeType:
                return types_.type_type();
            case AstNodeKind::TypeNever:
                return types_.never();
            default:
                error(ty->span, "unsupported type syntax in MIR lowering");
                return types_.error();
        }
    }

    // ---- Function lowering ----
    MirBody lower_fn_body(DefId owner_def, ModuleId mid, const ItemFn* fn,
                          const FnInfo& sig, std::optional<TypeId> self_ty) {
        (void)self_ty;
        in_comptime_context_ = false;
        out_ = MirBody{};
        out_.owner = owner_def;
        out_.span = fn ? fn->span : Span{};
        out_.ret_ty = sig.ret;

        scopes_.clear();
        loops_.clear();
        push_scope();

        // local#0 is the return place.
        add_local(sig.ret, "_ret");

        // Params.
        out_.comptime_params = sig.comptime_params;
        if (fn && fn->decl && fn->decl->sig) {
            for (size_t i = 0; i < fn->decl->sig->params.size(); i++) {
                const Param* p = fn->decl->sig->params[i];
                std::string name = p ? p->name : ("_arg" + std::to_string(i));
                MirLocalId id = add_local(sig.params[i], name);
                declare_local(std::move(name), id);
            }
        }

        // Blocks.
        add_block();  // bb0
        return_bb_ = add_block();
        out_.start = 0;
        cur_bb_ = 0;

        // Lower body block.
        MirOperand result = lower_block_expr(mid, fn->body);
        if (!has_terminator(cur_bb_)) {
            if (sig.ret != types_.unit()) {
                emit_stmt(MirStatement{MirStatement::Assign{
                    .dst = MirPlace::local(0),
                    .src = MirRvalue{MirRvalue::Use{result}},
                }});
            }
            terminate(MirTerminator{MirTerminator::Goto{return_bb_}});
        }

        // Return block.
        start_new_block(return_bb_);
        terminate(MirTerminator{MirTerminator::Return{}});

        return out_;
    }

    MirBody lower_const_body(DefId owner_def, ModuleId mid, const ItemConst* c,
                             TypeId ty) {
        in_comptime_context_ = true;
        out_ = MirBody{};
        out_.owner = owner_def;
        out_.span = c ? c->span : Span{};
        out_.ret_ty = ty;

        scopes_.clear();
        loops_.clear();
        push_scope();

        add_local(ty, "_ret");

        add_block();  // bb0
        return_bb_ = add_block();
        out_.start = 0;
        cur_bb_ = 0;

        MirOperand result = lower_expr(mid, c ? c->value : nullptr);
        if (!has_terminator(cur_bb_)) {
            if (ty != types_.unit()) {
                emit_stmt(MirStatement{MirStatement::Assign{
                    .dst = MirPlace::local(0),
                    .src = MirRvalue{MirRvalue::Use{std::move(result)}},
                }});
            }
            terminate(MirTerminator{MirTerminator::Goto{return_bb_}});
        }

        start_new_block(return_bb_);
        terminate(MirTerminator{MirTerminator::Return{}});
        return out_;
    }

    MirBody lower_static_body(DefId owner_def, ModuleId mid,
                              const ItemStatic* s, TypeId ty) {
        in_comptime_context_ = true;
        out_ = MirBody{};
        out_.owner = owner_def;
        out_.span = s ? s->span : Span{};
        out_.ret_ty = ty;

        scopes_.clear();
        loops_.clear();
        push_scope();

        add_local(ty, "_ret");

        add_block();  // bb0
        return_bb_ = add_block();
        out_.start = 0;
        cur_bb_ = 0;

        MirOperand result = lower_expr(mid, s ? s->value : nullptr);
        if (!has_terminator(cur_bb_)) {
            if (ty != types_.unit()) {
                emit_stmt(MirStatement{MirStatement::Assign{
                    .dst = MirPlace::local(0),
                    .src = MirRvalue{MirRvalue::Use{std::move(result)}},
                }});
            }
            terminate(MirTerminator{MirTerminator::Goto{return_bb_}});
        }

        start_new_block(return_bb_);
        terminate(MirTerminator{MirTerminator::Return{}});
        return out_;
    }

    bool has_terminator(MirBlockId bb) const {
        const MirBlock& b = out_.blocks.at(static_cast<size_t>(bb));
        return !std::holds_alternative<MirTerminator::Unreachable>(b.term.data);
    }

    // ---- Statements / blocks ----
    void lower_stmt(ModuleId mid, const Stmt* s) {
        if (!s) return;
        switch (s->kind) {
            case AstNodeKind::StmtLet: {
                auto* ls = static_cast<const StmtLet*>(s);
                lower_let(mid, ls);
                break;
            }
            case AstNodeKind::StmtExpr: {
                auto* es = static_cast<const StmtExpr*>(s);
                (void)lower_expr(mid, es->expr);
                break;
            }
            case AstNodeKind::StmtReturn: {
                auto* rs = static_cast<const StmtReturn*>(s);
                if (out_.ret_ty != types_.unit()) {
                    MirOperand v = lower_expr(mid, rs->value);
                    emit_stmt(MirStatement{MirStatement::Assign{
                        .dst = MirPlace::local(0),
                        .src = MirRvalue{MirRvalue::Use{v}},
                    }});
                }
                terminate(MirTerminator{MirTerminator::Goto{return_bb_}});
                break;
            }
            case AstNodeKind::StmtBreak: {
                if (loops_.empty()) {
                    error(s->span, "`break` outside of a loop");
                    break;
                }
                terminate(
                    MirTerminator{MirTerminator::Goto{loops_.back().break_bb}});
                break;
            }
            case AstNodeKind::StmtContinue: {
                if (loops_.empty()) {
                    error(s->span, "`continue` outside of a loop");
                    break;
                }
                terminate(MirTerminator{
                    MirTerminator::Goto{loops_.back().continue_bb}});
                break;
            }
            default:
                break;
        }
    }

    void lower_let(ModuleId mid, const StmtLet* ls) {
        if (!ls || !ls->pat) return;
        if (ls->pat->kind == AstNodeKind::PatWildcard) {
            // Evaluate initializer for side effects and discard.
            if (ls->init) (void)lower_expr(mid, ls->init);
            return;
        }
        if (ls->pat->kind != AstNodeKind::PatBinding) {
            error(ls->span, "unsupported `let` pattern in MIR lowering");
            return;
        }

        auto* b = static_cast<const PatBinding*>(ls->pat);
        TypeId bty = type_of_binding(ls->pat);
        MirLocalId id = add_local(bty, b->name);
        declare_local(b->name, id);
        if (ls->init) {
            MirOperand init = lower_expr(mid, ls->init);
            emit_stmt(MirStatement{MirStatement::Assign{
                .dst = MirPlace::local(id),
                .src = MirRvalue{MirRvalue::Use{init}},
            }});
        }
    }

    MirOperand lower_block_expr(ModuleId mid, const Block* b) {
        if (!b) return unit_operand();
        push_scope();
        for (const Stmt* s : b->stmts) {
            lower_stmt(mid, s);
            if (has_terminator(cur_bb_)) {
                pop_scope();
                return unit_operand();
            }
        }
        MirOperand out = b->tail ? lower_expr(mid, b->tail) : unit_operand();
        pop_scope();
        return out;
    }

    // ---- Expressions ----
    MirOperand unit_operand() {
        return MirOperand{MirOperand::Const{
            .ty = types_.unit(), .value = MirConst{MirConst::Unit{}}}};
    }

    MirOperand bool_operand(bool v) {
        return MirOperand{MirOperand::Const{
            .ty = types_.bool_(), .value = MirConst{MirConst::Bool{v}}}};
    }

    MirOperand int_operand(TypeId ty, std::int64_t v) {
        return MirOperand{
            MirOperand::Const{.ty = ty, .value = MirConst{MirConst::Int{v}}}};
    }

    MirOperand lower_expr(ModuleId mid, const Expr* e) {
        if (!e) return unit_operand();
        switch (e->kind) {
            case AstNodeKind::ExprUnit:
                return unit_operand();
            case AstNodeKind::ExprBool:
                return bool_operand(static_cast<const ExprBool*>(e)->value);
            case AstNodeKind::ExprInt: {
                TypeId ty = type_of(e);
                return int_operand(ty, static_cast<const ExprInt*>(e)->value);
            }
            case AstNodeKind::ExprFloat: {
                TypeId ty = type_of(e);
                auto* f = static_cast<const ExprFloat*>(e);
                MirOperand c{};
                c.data = MirOperand::Const{
                    .ty = ty, .value = MirConst{MirConst::Float{f->value}}};
                return c;
            }
            case AstNodeKind::ExprString: {
                TypeId ty = type_of(e);
                auto* s = static_cast<const ExprString*>(e);
                MirOperand c{};
                c.data = MirOperand::Const{
                    .ty = ty,
                    .value = MirConst{MirConst::String{
                        .bytes = s->value, .is_c_string = s->is_c_string}},
                };
                return c;
            }
            case AstNodeKind::ExprPath:
                return lower_path_expr(mid, static_cast<const ExprPath*>(e));
            case AstNodeKind::ExprBlock:
                return lower_block_expr(
                    mid, static_cast<const ExprBlock*>(e)->block);
            case AstNodeKind::ExprIf:
                return lower_if_expr(mid, static_cast<const ExprIf*>(e));
            case AstNodeKind::ExprWhile:
                return lower_while_expr(mid, static_cast<const ExprWhile*>(e));
            case AstNodeKind::ExprLoop:
                return lower_loop_expr(mid, static_cast<const ExprLoop*>(e));
            case AstNodeKind::ExprMatch:
                return lower_match_expr(mid, static_cast<const ExprMatch*>(e));
            case AstNodeKind::ExprUnary:
                return lower_unary_expr(mid, static_cast<const ExprUnary*>(e));
            case AstNodeKind::ExprBinary:
                return lower_binary_expr(mid,
                                         static_cast<const ExprBinary*>(e));
            case AstNodeKind::ExprCast:
                return lower_cast_expr(mid, static_cast<const ExprCast*>(e));
            case AstNodeKind::ExprAssign:
                return lower_assign_expr(mid,
                                         static_cast<const ExprAssign*>(e));
            case AstNodeKind::ExprCall:
                return lower_call_expr(mid, static_cast<const ExprCall*>(e));
            case AstNodeKind::ExprMethodCall:
                return lower_method_call_expr(
                    mid, static_cast<const ExprMethodCall*>(e));
            case AstNodeKind::ExprField:
                return lower_field_rvalue(mid,
                                          static_cast<const ExprField*>(e));
            case AstNodeKind::ExprIndex:
                return lower_index_rvalue(mid,
                                          static_cast<const ExprIndex*>(e));
            case AstNodeKind::ExprStructLit:
                return lower_struct_lit(mid,
                                        static_cast<const ExprStructLit*>(e));
            case AstNodeKind::ExprTuple:
                return lower_tuple_expr(mid, static_cast<const ExprTuple*>(e));
            case AstNodeKind::ExprArrayLit:
                return lower_array_lit(mid,
                                       static_cast<const ExprArrayLit*>(e));
            case AstNodeKind::ExprArrayRepeat:
                return lower_array_repeat(
                    mid, static_cast<const ExprArrayRepeat*>(e));
            case AstNodeKind::ExprComptime: {
                auto* c = static_cast<const ExprComptime*>(e);
                if (in_comptime_context_)
                    return lower_block_expr(mid, c->block);
                TypeId ty = type_of(e);
                return lower_block_as_internal_const_item(mid, c->block, ty,
                                                          e->span);
            }
            default:
                error(e->span, "unsupported expression in MIR lowering: " +
                                   std::string(ast_kind_name(e->kind)));
                return unit_operand();
        }
    }

    MirOperand lower_path_expr(ModuleId mid, const ExprPath* p) {
        if (!p || !p->path || p->path->segments.empty()) return unit_operand();

        // Local lookup only for single-segment paths.
        if (p->path->segments.size() == 1 && p->path->segments[0]) {
            std::string_view name = p->path->segments[0]->text;
            if (MirLocalId* id = lookup_local(name)) {
                return MirOperand{MirOperand::Copy{MirPlace::local(*id)}};
            }
        }

        // Const item.
        if (const ItemConst* c = resolve_const_path(mid, p->path)) {
            auto it = hir_.def_ids.find(static_cast<const Item*>(c));
            if (it != hir_.def_ids.end())
                return MirOperand{MirOperand::ConstItem{it->second}};
        }

        // Static item (read by value).
        if (const ItemStatic* s = resolve_static_path(mid, p->path)) {
            auto it = hir_.def_ids.find(static_cast<const Item*>(s));
            if (it != hir_.def_ids.end()) {
                MirPlace place{};
                place.base = MirPlace::Static{it->second};
                return MirOperand{MirOperand::Copy{std::move(place)}};
            }
        }

        // Enum unit variant.
        ResolvedVariant rv = resolve_variant_path(mid, p->path);
        if (rv.def && rv.decl && rv.decl->payload.empty()) {
            TypeId enum_ty = type_of(static_cast<const Expr*>(p));
            MirLocalId tmp = add_temp(enum_ty);
            MirRvalue::Aggregate agg{};
            agg.kind = MirRvalue::AggregateKind::EnumVariant;
            agg.ty = enum_ty;
            agg.variant_index = rv.index;
            emit_stmt(MirStatement{MirStatement::Assign{
                .dst = MirPlace::local(tmp),
                .src = MirRvalue{std::move(agg)},
            }});
            return MirOperand{MirOperand::Copy{MirPlace::local(tmp)}};
        }

        // Function item value (for function pointers).
        if (const ItemFn* fn = resolve_fn_path(mid, p->path)) {
            auto it = hir_.def_ids.find(static_cast<const Item*>(fn));
            if (it != hir_.def_ids.end())
                return MirOperand{MirOperand::Fn{it->second}};
        }

        error(p->span, "unresolved path in MIR lowering");
        return unit_operand();
    }

    MirOperand lower_unary_expr(ModuleId mid, const ExprUnary* u) {
        if (!u) return unit_operand();
        if (u->op == UnaryOp::AddrOf || u->op == UnaryOp::AddrOfMut) {
            MirPlace place = lower_place(mid, u->expr);
            TypeId ty = type_of(static_cast<const Expr*>(u));
            MirLocalId tmp = add_temp(ty);
            MirRvalue rv{};
            rv.data = MirRvalue::AddrOf{
                .mutability = u->op == UnaryOp::AddrOfMut ? Mutability::Mut
                                                          : Mutability::Const,
                .place = std::move(place),
            };
            emit_stmt(MirStatement{MirStatement::Assign{
                .dst = MirPlace::local(tmp), .src = std::move(rv)}});
            return MirOperand{MirOperand::Copy{MirPlace::local(tmp)}};
        }
        MirOperand operand = lower_expr(mid, u->expr);
        TypeId ty = type_of(static_cast<const Expr*>(u));
        MirLocalId tmp = add_temp(ty);
        MirRvalue rv{};
        rv.data = MirRvalue::Unary{.op = u->op, .operand = std::move(operand)};
        emit_stmt(MirStatement{MirStatement::Assign{.dst = MirPlace::local(tmp),
                                                    .src = std::move(rv)}});
        return MirOperand{MirOperand::Copy{MirPlace::local(tmp)}};
    }

    MirOperand lower_binary_expr(ModuleId mid, const ExprBinary* b) {
        if (!b) return unit_operand();
        if (b->op == BinaryOp::And || b->op == BinaryOp::Or) {
            // Short-circuit semantics.
            MirOperand lhs = lower_expr(mid, b->lhs);
            TypeId ty = type_of(static_cast<const Expr*>(b));
            MirLocalId out_local = add_temp(ty);

            MirBlockId rhs_bb = add_block();
            MirBlockId short_bb = add_block();
            MirBlockId end_bb = add_block();

            MirTerminator::SwitchInt sw{};
            sw.scrut = std::move(lhs);
            if (b->op == BinaryOp::And) {
                sw.cases.push_back({1, rhs_bb});
                sw.otherwise = short_bb;
            } else {
                sw.cases.push_back({1, short_bb});
                sw.otherwise = rhs_bb;
            }
            terminate(MirTerminator{std::move(sw)});

            // Short-circuit path: store constant and jump to end.
            start_new_block(short_bb);
            emit_stmt(MirStatement{MirStatement::Assign{
                .dst = MirPlace::local(out_local),
                .src = MirRvalue{MirRvalue::Use{
                    bool_operand(b->op == BinaryOp::Or)}},
            }});
            terminate(MirTerminator{MirTerminator::Goto{end_bb}});

            // RHS path: evaluate rhs, store result, jump to end.
            start_new_block(rhs_bb);
            MirOperand rhs = lower_expr(mid, b->rhs);
            emit_stmt(MirStatement{MirStatement::Assign{
                .dst = MirPlace::local(out_local),
                .src = MirRvalue{MirRvalue::Use{std::move(rhs)}},
            }});
            if (!has_terminator(cur_bb_))
                terminate(MirTerminator{MirTerminator::Goto{end_bb}});

            start_new_block(end_bb);
            return MirOperand{MirOperand::Copy{MirPlace::local(out_local)}};
        }

        MirOperand lhs = lower_expr(mid, b->lhs);
        MirOperand rhs = lower_expr(mid, b->rhs);
        TypeId ty = type_of(static_cast<const Expr*>(b));
        MirLocalId tmp = add_temp(ty);
        MirRvalue rv{};
        rv.data = MirRvalue::Binary{
            .op = b->op, .lhs = std::move(lhs), .rhs = std::move(rhs)};
        emit_stmt(MirStatement{MirStatement::Assign{.dst = MirPlace::local(tmp),
                                                    .src = std::move(rv)}});
        return MirOperand{MirOperand::Copy{MirPlace::local(tmp)}};
    }

    MirOperand lower_cast_expr(ModuleId mid, const ExprCast* c) {
        if (!c) return unit_operand();
        MirOperand v = lower_expr(mid, c->value);
        TypeId ty = type_of(static_cast<const Expr*>(c));
        MirLocalId tmp = add_temp(ty);
        MirRvalue rv{};
        rv.data = MirRvalue::Cast{.operand = std::move(v), .to = ty};
        emit_stmt(MirStatement{MirStatement::Assign{.dst = MirPlace::local(tmp),
                                                    .src = std::move(rv)}});
        return MirOperand{MirOperand::Copy{MirPlace::local(tmp)}};
    }

    MirOperand lower_assign_expr(ModuleId mid, const ExprAssign* a) {
        if (!a) return unit_operand();
        MirPlace dst = lower_place(mid, a->lhs);
        MirOperand rhs = lower_expr(mid, a->rhs);
        emit_stmt(MirStatement{MirStatement::Assign{
            .dst = std::move(dst),
            .src = MirRvalue{MirRvalue::Use{std::move(rhs)}},
        }});
        return unit_operand();
    }

    MirPlace lower_place(ModuleId mid, const Expr* e) {
        if (!e) return MirPlace::local(0);
        switch (e->kind) {
            case AstNodeKind::ExprPath: {
                auto* p = static_cast<const ExprPath*>(e);
                if (p->path && p->path->segments.size() == 1 &&
                    p->path->segments[0]) {
                    if (MirLocalId* id =
                            lookup_local(p->path->segments[0]->text))
                        return MirPlace::local(*id);
                }
                if (const ItemStatic* s = resolve_static_path(mid, p->path)) {
                    auto it = hir_.def_ids.find(static_cast<const Item*>(s));
                    if (it != hir_.def_ids.end()) {
                        MirPlace place{};
                        place.base = MirPlace::Static{it->second};
                        return place;
                    }
                }
                if (const ItemConst* c = resolve_const_path(mid, p->path)) {
                    auto it = hir_.def_ids.find(static_cast<const Item*>(c));
                    if (it != hir_.def_ids.end()) {
                        TypeId ty = type_of(e);
                        MirLocalId tmp = add_temp(ty);
                        emit_stmt(MirStatement{MirStatement::Assign{
                            .dst = MirPlace::local(tmp),
                            .src = MirRvalue{MirRvalue::Use{
                                MirOperand{MirOperand::ConstItem{it->second}}}},
                        }});
                        return MirPlace::local(tmp);
                    }
                }
                error(e->span, "unsupported place expression");
                return MirPlace::local(0);
            }
            case AstNodeKind::ExprUnary: {
                auto* u = static_cast<const ExprUnary*>(e);
                if (u->op != UnaryOp::Deref) break;
                MirOperand ptr = lower_expr(mid, u->expr);
                // Materialize pointer value into a temp so place projections
                // can deref it.
                TypeId ptr_ty = type_of(u->expr);
                MirLocalId tmp = add_temp(ptr_ty);
                emit_stmt(MirStatement{MirStatement::Assign{
                    .dst = MirPlace::local(tmp),
                    .src = MirRvalue{MirRvalue::Use{std::move(ptr)}},
                }});
                MirPlace place = MirPlace::local(tmp);
                place.projection.push_back(
                    MirProjection{MirProjection::Deref{}});
                return place;
            }
            case AstNodeKind::ExprField: {
                auto* f = static_cast<const ExprField*>(e);
                MirPlace base = lower_place(mid, f->base);
                TypeId base_ty = type_of(f->base);
                const TypeData& bd = types_.get(base_ty);
                TypeId agg_ty = base_ty;
                if (bd.kind == TypeKind::Ptr) {
                    agg_ty = bd.pointee;
                    base.projection.push_back(
                        MirProjection{MirProjection::Deref{}});
                }
                const TypeData& ad = types_.get(agg_ty);
                std::optional<std::uint32_t> field_index{};
                if (ad.kind == TypeKind::Struct && ad.struct_def) {
                    auto it = checked_.struct_info.find(ad.struct_def);
                    if (it != checked_.struct_info.end()) {
                        for (size_t i = 0;
                             i < it->second.fields_in_order.size(); i++) {
                            if (it->second.fields_in_order[i].name == f->field)
                                field_index = static_cast<std::uint32_t>(i);
                        }
                    }
                } else if (ad.kind == TypeKind::Tuple) {
                    // tuple index is stored as decimal in `field`.
                    std::uint32_t idx = 0;
                    for (char c : f->field) {
                        if (c < '0' || c > '9') {
                            field_index = std::nullopt;
                            break;
                        }
                        idx = idx * 10 + static_cast<std::uint32_t>(c - '0');
                    }
                    field_index = idx;
                }
                if (!field_index) {
                    error(f->span, "unknown field in place expression");
                    return MirPlace::local(0);
                }
                base.projection.push_back(
                    MirProjection{MirProjection::Field{*field_index}});
                return base;
            }
            case AstNodeKind::ExprIndex: {
                auto* ix = static_cast<const ExprIndex*>(e);
                MirPlace base = lower_place(mid, ix->base);
                MirOperand idx = lower_expr(mid, ix->index);
                MirLocalId idx_local = add_temp(type_of(ix->index));
                emit_stmt(MirStatement{MirStatement::Assign{
                    .dst = MirPlace::local(idx_local),
                    .src = MirRvalue{MirRvalue::Use{std::move(idx)}},
                }});
                base.projection.push_back(
                    MirProjection{MirProjection::Index{idx_local}});
                return base;
            }
            default:
                break;
        }
        error(e->span, "unsupported place expression");
        return MirPlace::local(0);
    }

    MirOperand lower_field_rvalue(ModuleId mid, const ExprField* f) {
        if (!f) return unit_operand();
        auto base_place_like = [](const Expr* e) -> bool {
            if (!e) return false;
            switch (e->kind) {
                case AstNodeKind::ExprPath:
                case AstNodeKind::ExprField:
                case AstNodeKind::ExprIndex:
                    return true;
                case AstNodeKind::ExprUnary: {
                    auto* u = static_cast<const ExprUnary*>(e);
                    return u->op == UnaryOp::Deref;
                }
                default:
                    return false;
            }
        };

        if (base_place_like(f->base)) {
            MirPlace p = lower_place(mid, static_cast<const Expr*>(f));
            return MirOperand{MirOperand::Copy{std::move(p)}};
        }

        // Allow reading fields from rvalues (e.g. `foo().x`) by materializing
        // the base into a temp local, then projecting.
        MirOperand base_v = lower_expr(mid, f->base);
        TypeId base_ty = type_of(f->base);
        MirLocalId tmp_base = add_temp(base_ty);
        emit_stmt(MirStatement{MirStatement::Assign{
            .dst = MirPlace::local(tmp_base),
            .src = MirRvalue{MirRvalue::Use{std::move(base_v)}},
        }});

        MirPlace place = MirPlace::local(tmp_base);
        TypeId agg_ty = base_ty;
        const TypeData& bd = types_.get(base_ty);
        if (bd.kind == TypeKind::Ptr) {
            agg_ty = bd.pointee;
            place.projection.push_back(MirProjection{MirProjection::Deref{}});
        }

        const TypeData& ad = types_.get(agg_ty);
        std::optional<std::uint32_t> field_index{};
        if (ad.kind == TypeKind::Struct && ad.struct_def) {
            auto it = checked_.struct_info.find(ad.struct_def);
            if (it != checked_.struct_info.end()) {
                for (size_t i = 0; i < it->second.fields_in_order.size(); i++) {
                    if (it->second.fields_in_order[i].name == f->field)
                        field_index = static_cast<std::uint32_t>(i);
                }
            }
        } else if (ad.kind == TypeKind::Tuple) {
            std::uint32_t idx = 0;
            for (char c : f->field) {
                if (c < '0' || c > '9') {
                    field_index = std::nullopt;
                    break;
                }
                idx = idx * 10 + static_cast<std::uint32_t>(c - '0');
            }
            field_index = idx;
        }

        if (!field_index) {
            error(f->span, "unknown field in rvalue field access");
            return unit_operand();
        }
        place.projection.push_back(
            MirProjection{MirProjection::Field{*field_index}});
        return MirOperand{MirOperand::Copy{std::move(place)}};
    }

    MirOperand lower_index_rvalue(ModuleId mid, const ExprIndex* ix) {
        if (!ix) return unit_operand();
        auto base_place_like = [](const Expr* e) -> bool {
            if (!e) return false;
            switch (e->kind) {
                case AstNodeKind::ExprPath:
                case AstNodeKind::ExprField:
                case AstNodeKind::ExprIndex:
                    return true;
                case AstNodeKind::ExprUnary: {
                    auto* u = static_cast<const ExprUnary*>(e);
                    return u->op == UnaryOp::Deref;
                }
                default:
                    return false;
            }
        };

        if (base_place_like(ix->base)) {
            MirPlace p = lower_place(mid, static_cast<const Expr*>(ix));
            return MirOperand{MirOperand::Copy{std::move(p)}};
        }

        MirOperand base_v = lower_expr(mid, ix->base);
        TypeId base_ty = type_of(ix->base);
        MirLocalId tmp_base = add_temp(base_ty);
        emit_stmt(MirStatement{MirStatement::Assign{
            .dst = MirPlace::local(tmp_base),
            .src = MirRvalue{MirRvalue::Use{std::move(base_v)}},
        }});

        MirPlace place = MirPlace::local(tmp_base);
        MirOperand idx = lower_expr(mid, ix->index);
        MirLocalId idx_local = add_temp(type_of(ix->index));
        emit_stmt(MirStatement{MirStatement::Assign{
            .dst = MirPlace::local(idx_local),
            .src = MirRvalue{MirRvalue::Use{std::move(idx)}},
        }});
        place.projection.push_back(
            MirProjection{MirProjection::Index{idx_local}});
        return MirOperand{MirOperand::Copy{std::move(place)}};
    }

    MirOperand lower_tuple_expr(ModuleId mid, const ExprTuple* t) {
        TypeId ty = type_of(static_cast<const Expr*>(t));
        MirLocalId tmp = add_temp(ty);
        MirRvalue::Aggregate agg{};
        agg.kind = MirRvalue::AggregateKind::Tuple;
        agg.ty = ty;
        for (const Expr* e : t->elems) agg.elems.push_back(lower_expr(mid, e));
        emit_stmt(MirStatement{MirStatement::Assign{.dst = MirPlace::local(tmp),
                                                    .src = MirRvalue{agg}}});
        return MirOperand{MirOperand::Copy{MirPlace::local(tmp)}};
    }

    MirOperand lower_struct_lit(ModuleId mid, const ExprStructLit* s) {
        TypeId ty = type_of(static_cast<const Expr*>(s));
        const TypeData& td = types_.get(ty);
        if (td.kind != TypeKind::Struct || !td.struct_def) {
            error(s->span, "struct literal on non-struct type");
            return unit_operand();
        }
        auto it = checked_.struct_info.find(td.struct_def);
        if (it == checked_.struct_info.end()) return unit_operand();
        const StructInfo& si = it->second;

        std::unordered_map<std::string, MirOperand> init_map{};
        for (const FieldInit* f : s->inits) {
            if (!f) continue;
            init_map.insert({f->name, lower_expr(mid, f->value)});
        }

        MirRvalue::Aggregate agg{};
        agg.kind = MirRvalue::AggregateKind::Struct;
        agg.ty = ty;
        agg.elems.reserve(si.fields_in_order.size());
        for (const auto& fld : si.fields_in_order) {
            auto fit = init_map.find(fld.name);
            if (fit != init_map.end())
                agg.elems.push_back(std::move(fit->second));
            else
                agg.elems.push_back(int_operand(fld.type, 0));
        }

        MirLocalId tmp = add_temp(ty);
        emit_stmt(MirStatement{MirStatement::Assign{.dst = MirPlace::local(tmp),
                                                    .src = MirRvalue{agg}}});
        return MirOperand{MirOperand::Copy{MirPlace::local(tmp)}};
    }

    MirOperand lower_array_lit(ModuleId mid, const ExprArrayLit* a) {
        TypeId ty = type_of(static_cast<const Expr*>(a));
        const TypeData& td = types_.get(ty);
        if (td.kind != TypeKind::Array) {
            error(a->span, "array literal on non-array type");
            return unit_operand();
        }
        MirRvalue::Aggregate agg{};
        agg.kind = MirRvalue::AggregateKind::Array;
        agg.ty = ty;
        for (const Expr* e : a->elems) agg.elems.push_back(lower_expr(mid, e));
        MirLocalId tmp = add_temp(ty);
        emit_stmt(MirStatement{MirStatement::Assign{.dst = MirPlace::local(tmp),
                                                    .src = MirRvalue{agg}}});
        return MirOperand{MirOperand::Copy{MirPlace::local(tmp)}};
    }

    std::optional<std::uint64_t> array_len_value(TypeId array_ty) const {
        const TypeData& td = types_.get(array_ty);
        if (td.kind != TypeKind::Array) return std::nullopt;
        if (td.array_len_value) return td.array_len_value;
        if (td.array_len_expr) {
            auto it = checked_.array_lens.find(td.array_len_expr);
            if (it != checked_.array_lens.end()) return it->second;
        }
        return std::nullopt;
    }

    MirOperand lower_array_repeat(ModuleId mid, const ExprArrayRepeat* r) {
        TypeId ty = type_of(static_cast<const Expr*>(r));
        auto n = array_len_value(ty);
        if (!n) {
            error(r->span, "array repeat count is not a comptime constant");
            return unit_operand();
        }
        MirOperand elem = lower_expr(mid, r->elem);
        TypeId elem_ty = types_.get(ty).elem;
        MirLocalId tmp_elem = add_temp(elem_ty);
        emit_stmt(MirStatement{MirStatement::Assign{
            .dst = MirPlace::local(tmp_elem),
            .src = MirRvalue{MirRvalue::Use{std::move(elem)}},
        }});
        MirRvalue::Aggregate agg{};
        agg.kind = MirRvalue::AggregateKind::Array;
        agg.ty = ty;
        agg.elems.reserve(static_cast<size_t>(*n));
        for (std::uint64_t i = 0; i < *n; i++)
            agg.elems.push_back(
                MirOperand{MirOperand::Copy{MirPlace::local(tmp_elem)}});
        MirLocalId tmp = add_temp(ty);
        emit_stmt(MirStatement{MirStatement::Assign{.dst = MirPlace::local(tmp),
                                                    .src = MirRvalue{agg}}});
        return MirOperand{MirOperand::Copy{MirPlace::local(tmp)}};
    }

    MirOperand lower_if_expr(ModuleId mid, const ExprIf* i) {
        MirOperand cond = lower_expr(mid, i->cond);

        MirBlockId then_bb = add_block();
        MirBlockId else_bb = add_block();
        MirBlockId join_bb = add_block();

        MirTerminator::SwitchInt sw{};
        sw.scrut = std::move(cond);
        sw.cases.push_back({1, then_bb});
        sw.otherwise = else_bb;
        terminate(MirTerminator{std::move(sw)});

        TypeId ty = type_of(static_cast<const Expr*>(i));
        std::optional<MirLocalId> out_local{};
        if (ty != types_.unit()) out_local = add_temp(ty);

        // then
        start_new_block(then_bb);
        MirOperand then_v = lower_block_expr(mid, i->then_block);
        if (out_local && !has_terminator(cur_bb_)) {
            emit_stmt(MirStatement{MirStatement::Assign{
                .dst = MirPlace::local(*out_local),
                .src = MirRvalue{MirRvalue::Use{then_v}},
            }});
        }
        if (!has_terminator(cur_bb_))
            terminate(MirTerminator{MirTerminator::Goto{join_bb}});

        // else
        start_new_block(else_bb);
        MirOperand else_v =
            i->else_expr ? lower_expr(mid, i->else_expr) : unit_operand();
        if (out_local && !has_terminator(cur_bb_)) {
            emit_stmt(MirStatement{MirStatement::Assign{
                .dst = MirPlace::local(*out_local),
                .src = MirRvalue{MirRvalue::Use{else_v}},
            }});
        }
        if (!has_terminator(cur_bb_))
            terminate(MirTerminator{MirTerminator::Goto{join_bb}});

        // join
        start_new_block(join_bb);
        if (!out_local) return unit_operand();
        return MirOperand{MirOperand::Copy{MirPlace::local(*out_local)}};
    }

    MirOperand lower_while_expr(ModuleId mid, const ExprWhile* w) {
        MirBlockId header = add_block();
        MirBlockId body = add_block();
        MirBlockId exit = add_block();

        terminate(MirTerminator{MirTerminator::Goto{header}});

        start_new_block(header);
        MirOperand cond = lower_expr(mid, w->cond);
        MirTerminator::SwitchInt sw{};
        sw.scrut = std::move(cond);
        sw.cases.push_back({1, body});
        sw.otherwise = exit;
        terminate(MirTerminator{std::move(sw)});

        loops_.push_back(LoopCtx{.break_bb = exit, .continue_bb = header});

        start_new_block(body);
        (void)lower_block_expr(mid, w->body);
        if (!has_terminator(cur_bb_))
            terminate(MirTerminator{MirTerminator::Goto{header}});

        loops_.pop_back();

        start_new_block(exit);
        return unit_operand();
    }

    MirOperand lower_loop_expr(ModuleId mid, const ExprLoop* l) {
        MirBlockId body = add_block();
        MirBlockId exit = add_block();

        terminate(MirTerminator{MirTerminator::Goto{body}});

        loops_.push_back(LoopCtx{.break_bb = exit, .continue_bb = body});

        start_new_block(body);
        (void)lower_block_expr(mid, l->body);
        if (!has_terminator(cur_bb_))
            terminate(MirTerminator{MirTerminator::Goto{body}});

        loops_.pop_back();

        start_new_block(exit);
        return unit_operand();
    }

    MirOperand lower_match_expr(ModuleId mid, const ExprMatch* m) {
        if (!m) return unit_operand();

        // Lower match as a chain of pattern tests (like the existing AST
        // backend). This preserves semantics and keeps guards simple.
        MirOperand scrut = lower_expr(mid, m->scrutinee);
        TypeId scrut_ty = type_of(m->scrutinee);
        MirLocalId scrut_local = add_temp(scrut_ty);
        emit_stmt(MirStatement{MirStatement::Assign{
            .dst = MirPlace::local(scrut_local),
            .src = MirRvalue{MirRvalue::Use{std::move(scrut)}},
        }});

        TypeId out_ty = type_of(static_cast<const Expr*>(m));
        std::optional<MirLocalId> out_local{};
        if (out_ty != types_.unit()) out_local = add_temp(out_ty);

        MirBlockId join_bb = add_block();
        MirBlockId next_test_bb = add_block();
        terminate(MirTerminator{MirTerminator::Goto{next_test_bb}});

        for (const MatchArm* arm : m->arms) {
            if (!arm) continue;

            MirBlockId arm_bb = add_block();
            MirBlockId fail_bb = add_block();

            start_new_block(next_test_bb);
            next_test_bb = fail_bb;

            MirOperand cond =
                lower_pat_cond(mid, arm->pat, scrut_ty, scrut_local);
            MirTerminator::SwitchInt sw{};
            sw.scrut = std::move(cond);
            sw.cases.push_back({1, arm_bb});
            sw.otherwise = fail_bb;
            terminate(MirTerminator{std::move(sw)});

            // Arm body.
            start_new_block(arm_bb);
            push_scope();
            lower_pat_bindings(mid, arm->pat, scrut_ty, scrut_local);

            if (arm->guard) {
                MirOperand g = lower_expr(mid, arm->guard);
                MirBlockId guard_ok = add_block();
                MirTerminator::SwitchInt gsw{};
                gsw.scrut = std::move(g);
                gsw.cases.push_back({1, guard_ok});
                gsw.otherwise = fail_bb;
                terminate(MirTerminator{std::move(gsw)});
                start_new_block(guard_ok);
            }

            MirOperand body_v = lower_expr(mid, arm->body);
            if (out_local && !has_terminator(cur_bb_)) {
                emit_stmt(MirStatement{MirStatement::Assign{
                    .dst = MirPlace::local(*out_local),
                    .src = MirRvalue{MirRvalue::Use{std::move(body_v)}},
                }});
            }
            if (!has_terminator(cur_bb_))
                terminate(MirTerminator{MirTerminator::Goto{join_bb}});
            pop_scope();
        }

        // Exhaustiveness checking should prevent reaching here, but keep a
        // well-formed CFG.
        start_new_block(next_test_bb);
        terminate(MirTerminator{MirTerminator::Goto{join_bb}});

        start_new_block(join_bb);
        if (!out_local) return unit_operand();
        return MirOperand{MirOperand::Copy{MirPlace::local(*out_local)}};
    }

    MirOperand emit_bool_binary(BinaryOp op, MirOperand lhs, MirOperand rhs) {
        MirLocalId tmp = add_temp(types_.bool_());
        MirRvalue rv{};
        rv.data = MirRvalue::Binary{
            .op = op, .lhs = std::move(lhs), .rhs = std::move(rhs)};
        emit_stmt(MirStatement{MirStatement::Assign{
            .dst = MirPlace::local(tmp),
            .src = std::move(rv),
        }});
        return MirOperand{MirOperand::Copy{MirPlace::local(tmp)}};
    }

    MirOperand lower_pat_cond(ModuleId mid, const Pattern* pat, TypeId scrut_ty,
                              MirLocalId scrut_local) {
        if (!pat) return bool_operand(true);
        switch (pat->kind) {
            case AstNodeKind::PatWildcard:
            case AstNodeKind::PatBinding:
                return bool_operand(true);
            case AstNodeKind::PatInt: {
                auto* ip = static_cast<const PatInt*>(pat);
                MirOperand lhs =
                    MirOperand{MirOperand::Copy{MirPlace::local(scrut_local)}};
                MirOperand rhs = int_operand(scrut_ty, ip->value);
                return emit_bool_binary(BinaryOp::Eq, std::move(lhs),
                                        std::move(rhs));
            }
            case AstNodeKind::PatBool: {
                auto* bp = static_cast<const PatBool*>(pat);
                MirOperand lhs =
                    MirOperand{MirOperand::Copy{MirPlace::local(scrut_local)}};
                MirOperand rhs = bool_operand(bp->value);
                return emit_bool_binary(BinaryOp::Eq, std::move(lhs),
                                        std::move(rhs));
            }
            case AstNodeKind::PatOr: {
                auto* o = static_cast<const PatOr*>(pat);
                MirOperand lhs =
                    lower_pat_cond(mid, o->lhs, scrut_ty, scrut_local);
                MirOperand rhs =
                    lower_pat_cond(mid, o->rhs, scrut_ty, scrut_local);
                return emit_bool_binary(BinaryOp::BitOr, std::move(lhs),
                                        std::move(rhs));
            }
            case AstNodeKind::PatPath:
            case AstNodeKind::PatVariant: {
                const TypeData& td = types_.get(scrut_ty);
                if (td.kind != TypeKind::Enum || !td.enum_def) {
                    error(pat->span, "enum pattern on non-enum");
                    return bool_operand(false);
                }

                const Path* path = nullptr;
                const std::vector<Pattern*>* args = nullptr;
                if (pat->kind == AstNodeKind::PatPath) {
                    path = static_cast<const PatPath*>(pat)->path;
                } else {
                    auto* vp = static_cast<const PatVariant*>(pat);
                    path = vp->path;
                    args = &vp->args;
                }

                ResolvedVariant rv = resolve_variant_path(mid, path);
                if (!rv.def || !rv.decl) {
                    error(pat->span, "unresolved enum variant pattern");
                    return bool_operand(false);
                }
                auto info_it = checked_.enum_info.find(rv.def);
                if (info_it == checked_.enum_info.end()) {
                    error(pat->span, "missing enum info for pattern");
                    return bool_operand(false);
                }
                const EnumInfo& ei = info_it->second;
                std::int64_t disc_value = static_cast<std::int64_t>(rv.index);
                if (auto disc_it = ei.discriminants.find(rv.decl->name);
                    disc_it != ei.discriminants.end()) {
                    disc_value = disc_it->second;
                }

                TypeId usize_ty = types_.int_(IntKind::Usize);
                MirLocalId tag_local = add_temp(usize_ty);
                MirRvalue tag_rv{};
                tag_rv.data =
                    MirRvalue::EnumTag{.operand = MirOperand{MirOperand::Copy{
                                           MirPlace::local(scrut_local)}}};
                emit_stmt(MirStatement{MirStatement::Assign{
                    .dst = MirPlace::local(tag_local),
                    .src = std::move(tag_rv),
                }});

                MirOperand tag_op =
                    MirOperand{MirOperand::Copy{MirPlace::local(tag_local)}};
                MirOperand disc_op = int_operand(usize_ty, disc_value);
                MirOperand cond = emit_bool_binary(
                    BinaryOp::Eq, std::move(tag_op), std::move(disc_op));

                if (!args || args->empty()) return cond;

                auto vit = ei.variants.find(rv.decl->name);
                if (vit == ei.variants.end()) return cond;
                const VariantInfo& vi = vit->second;
                if (args->size() != vi.payload.size()) {
                    error(pat->span, "variant pattern arity mismatch");
                    return cond;
                }

                for (size_t i = 0; i < args->size(); i++) {
                    const Pattern* ap = (*args)[i];
                    if (!ap) continue;
                    if (ap->kind == AstNodeKind::PatWildcard ||
                        ap->kind == AstNodeKind::PatBinding)
                        continue;

                    if (ap->kind != AstNodeKind::PatInt &&
                        ap->kind != AstNodeKind::PatBool) {
                        error(ap->span,
                              "unsupported nested pattern in enum match");
                        continue;
                    }

                    TypeId payload_ty = vi.payload[i];
                    MirPlace field = MirPlace::local(scrut_local);
                    field.projection.push_back(
                        MirProjection{MirProjection::Downcast{rv.index}});
                    field.projection.push_back(MirProjection{
                        MirProjection::Field{static_cast<std::uint32_t>(i)}});
                    MirOperand field_op =
                        MirOperand{MirOperand::Copy{std::move(field)}};

                    MirOperand test_rhs{};
                    if (ap->kind == AstNodeKind::PatInt) {
                        test_rhs = int_operand(
                            payload_ty, static_cast<const PatInt*>(ap)->value);
                    } else {
                        test_rhs = bool_operand(
                            static_cast<const PatBool*>(ap)->value);
                    }
                    MirOperand test = emit_bool_binary(
                        BinaryOp::Eq, std::move(field_op), std::move(test_rhs));
                    cond = emit_bool_binary(BinaryOp::BitAnd, std::move(cond),
                                            std::move(test));
                }

                return cond;
            }
            default:
                error(pat->span, "unsupported pattern in MIR lowering");
                return bool_operand(false);
        }
    }

    void lower_pat_bindings(ModuleId mid, const Pattern* pat, TypeId scrut_ty,
                            MirLocalId scrut_local) {
        (void)mid;
        if (!pat) return;
        switch (pat->kind) {
            case AstNodeKind::PatWildcard:
            case AstNodeKind::PatInt:
            case AstNodeKind::PatBool:
            case AstNodeKind::PatPath:
            case AstNodeKind::PatOr:
                return;
            case AstNodeKind::PatBinding: {
                auto* b = static_cast<const PatBinding*>(pat);
                TypeId bty = type_of_binding(pat);
                MirLocalId id = add_local(bty, b->name);
                declare_local(b->name, id);
                emit_stmt(MirStatement{MirStatement::Assign{
                    .dst = MirPlace::local(id),
                    .src = MirRvalue{MirRvalue::Use{MirOperand{
                        MirOperand::Copy{MirPlace::local(scrut_local)}}}},
                }});
                return;
            }
            case AstNodeKind::PatVariant: {
                auto* vp = static_cast<const PatVariant*>(pat);
                const TypeData& td = types_.get(scrut_ty);
                if (td.kind != TypeKind::Enum || !td.enum_def) return;

                ResolvedVariant rv = resolve_variant_path(mid, vp->path);
                if (!rv.def || !rv.decl) return;
                auto info_it = checked_.enum_info.find(rv.def);
                if (info_it == checked_.enum_info.end()) return;
                const EnumInfo& ei = info_it->second;
                auto vit = ei.variants.find(rv.decl->name);
                if (vit == ei.variants.end()) return;
                const VariantInfo& vi = vit->second;
                if (vp->args.size() != vi.payload.size()) return;

                for (size_t i = 0; i < vp->args.size(); i++) {
                    const Pattern* ap = vp->args[i];
                    if (!ap || ap->kind != AstNodeKind::PatBinding) continue;
                    auto* b = static_cast<const PatBinding*>(ap);
                    TypeId payload_ty = vi.payload[i];
                    MirLocalId id = add_local(payload_ty, b->name);
                    declare_local(b->name, id);

                    MirPlace field = MirPlace::local(scrut_local);
                    field.projection.push_back(
                        MirProjection{MirProjection::Downcast{rv.index}});
                    field.projection.push_back(MirProjection{
                        MirProjection::Field{static_cast<std::uint32_t>(i)}});
                    emit_stmt(MirStatement{MirStatement::Assign{
                        .dst = MirPlace::local(id),
                        .src = MirRvalue{MirRvalue::Use{
                            MirOperand{MirOperand::Copy{std::move(field)}}}},
                    }});
                }
                return;
            }
            default:
                error(pat->span, "unsupported binding pattern in MIR lowering");
                return;
        }
    }

    MirOperand lower_call_expr(ModuleId mid, const ExprCall* call) {
        if (!call) return unit_operand();

        // Builtins: lower to constants/aggregates.
        const Path* callee_path = nullptr;
        if (call->callee && call->callee->kind == AstNodeKind::ExprPath)
            callee_path = static_cast<const ExprPath*>(call->callee)->path;

        if (callee_path && callee_path->segments.size() == 2 &&
            callee_path->segments[0]->text == "builtin") {
            std::string_view name = callee_path->segments[1]->text;
            if (name == "size_of" || name == "align_of") {
                if (call->args.size() != 1) return unit_operand();
                TypeId ty = lower_type_value_expr(mid, call->args[0]);
                std::optional<std::uint64_t> v{};
                if (name == "size_of") v = layout_.size_of(ty, call->span);
                if (name == "align_of") v = layout_.align_of(ty, call->span);
                TypeId usize_ty = types_.int_(IntKind::Usize);
                if (!v) return int_operand(usize_ty, 0);
                if (*v > static_cast<std::uint64_t>(
                             std::numeric_limits<std::int64_t>::max())) {
                    error(call->span, "comptime integer overflow");
                    return int_operand(usize_ty, 0);
                }
                return int_operand(usize_ty, static_cast<std::int64_t>(*v));
            }
            if (name == "type_info") {
                if (call->args.size() != 1) return unit_operand();
                TypeId arg_ty = lower_type_value_expr(mid, call->args[0]);
                if (!types_.is_sized(arg_ty)) {
                    error(call->args[0] ? call->args[0]->span : call->span,
                          "builtin::type_info currently requires a sized type");
                    return unit_operand();
                }

                auto sz = layout_.size_of(arg_ty, call->span);
                auto al = layout_.align_of(arg_ty, call->span);
                if (!sz || !al) return unit_operand();

                const TypeData& out_td =
                    types_.get(type_of(static_cast<const Expr*>(call)));
                if (out_td.kind != TypeKind::Struct || !out_td.struct_def)
                    return unit_operand();
                auto si_it = checked_.struct_info.find(out_td.struct_def);
                if (si_it == checked_.struct_info.end()) return unit_operand();
                const StructInfo& si = si_it->second;

                std::unordered_map<std::string, MirOperand> field_vals{};
                for (const auto& f : si.fields_in_order) {
                    if (f.name == "kind") {
                        std::int64_t kind_code =
                            static_cast<std::int64_t>(types_.get(arg_ty).kind);
                        field_vals.insert({std::string(f.name),
                                           int_operand(f.type, kind_code)});
                    } else if (f.name == "size") {
                        if (*sz >
                            static_cast<std::uint64_t>(
                                std::numeric_limits<std::int64_t>::max())) {
                            error(call->span, "comptime integer overflow");
                            field_vals.insert(
                                {std::string(f.name), int_operand(f.type, 0)});
                        } else {
                            field_vals.insert(
                                {std::string(f.name),
                                 int_operand(f.type,
                                             static_cast<std::int64_t>(*sz))});
                        }
                    } else if (f.name == "align") {
                        if (*al >
                            static_cast<std::uint64_t>(
                                std::numeric_limits<std::int64_t>::max())) {
                            error(call->span, "comptime integer overflow");
                            field_vals.insert(
                                {std::string(f.name), int_operand(f.type, 0)});
                        } else {
                            field_vals.insert(
                                {std::string(f.name),
                                 int_operand(f.type,
                                             static_cast<std::int64_t>(*al))});
                        }
                    }
                }

                TypeId out_ty = type_of(static_cast<const Expr*>(call));
                MirRvalue::Aggregate agg{};
                agg.kind = MirRvalue::AggregateKind::Struct;
                agg.ty = out_ty;
                agg.elems.reserve(si.fields_in_order.size());
                for (const auto& f : si.fields_in_order) {
                    auto it = field_vals.find(std::string(f.name));
                    if (it != field_vals.end())
                        agg.elems.push_back(it->second);
                    else
                        agg.elems.push_back(int_operand(f.type, 0));
                }
                MirLocalId tmp = add_temp(out_ty);
                emit_stmt(MirStatement{MirStatement::Assign{
                    .dst = MirPlace::local(tmp),
                    .src = MirRvalue{std::move(agg)},
                }});
                return MirOperand{MirOperand::Copy{MirPlace::local(tmp)}};
            }
            if (name == "compile_error") {
                std::string msg = "builtin::compile_error";
                if (call->args.size() == 1) {
                    if (call->args[0] &&
                        call->args[0]->kind == AstNodeKind::ExprString) {
                        msg = static_cast<const ExprString*>(call->args[0])
                                  ->value;
                    }
                }
                emit_stmt(MirStatement{MirStatement::Assert{
                    .cond = bool_operand(false),
                    .message = std::move(msg),
                    .span = call->span,
                }});
                terminate(MirTerminator{MirTerminator::Unreachable{}});
                start_new_block(add_block());
                return unit_operand();
            }
        }

        // Direct function call?
        if (callee_path) {
            if (const ItemFn* fn = resolve_fn_path(mid, callee_path)) {
                auto dit = hir_.def_ids.find(static_cast<const Item*>(fn));
                if (dit == hir_.def_ids.end()) return unit_operand();
                auto sig_it = checked_.fn_info.find(fn);
                if (sig_it == checked_.fn_info.end()) return unit_operand();
                const FnInfo& sig = sig_it->second;

                TypeId ret_ty = type_of(static_cast<const Expr*>(call));
                std::optional<MirLocalId> out_local{};
                if (ret_ty != types_.unit()) out_local = add_temp(ret_ty);
                MirBlockId next_bb = add_block();

                MirTerminator::Call t{};
                t.callee = MirOperand{MirOperand::Fn{dit->second}};
                t.args.reserve(call->args.size());
                for (size_t i = 0; i < call->args.size(); i++) {
                    const Expr* a = call->args[i];
                    const bool is_ct = i < sig.comptime_params.size() &&
                                       sig.comptime_params[i];
                    if (is_ct)
                        t.args.push_back(lower_as_internal_const_item(mid, a));
                    else
                        t.args.push_back(lower_expr(mid, a));
                }
                if (out_local) t.ret = MirPlace::local(*out_local);
                t.next = next_bb;
                terminate(MirTerminator{std::move(t)});
                start_new_block(next_bb);
                if (!out_local) return unit_operand();
                return MirOperand{
                    MirOperand::Copy{MirPlace::local(*out_local)}};
            }

            // Enum variant constructor.
            ResolvedVariant rv = resolve_variant_path(mid, callee_path);
            if (rv.def && rv.decl) {
                TypeId enum_ty = type_of(static_cast<const Expr*>(call));
                MirRvalue::Aggregate agg{};
                agg.kind = MirRvalue::AggregateKind::EnumVariant;
                agg.ty = enum_ty;
                agg.variant_index = rv.index;
                for (const Expr* a : call->args)
                    agg.elems.push_back(lower_expr(mid, a));
                MirLocalId tmp = add_temp(enum_ty);
                emit_stmt(MirStatement{MirStatement::Assign{
                    .dst = MirPlace::local(tmp),
                    .src = MirRvalue{std::move(agg)},
                }});
                return MirOperand{MirOperand::Copy{MirPlace::local(tmp)}};
            }

            // Tuple struct constructor.
            if (const Item* ty_item = resolve_type_item(mid, callee_path)) {
                if (ty_item->kind == AstNodeKind::ItemStruct) {
                    TypeId struct_ty = type_of(static_cast<const Expr*>(call));
                    const TypeData& td = types_.get(struct_ty);
                    if (td.kind == TypeKind::Struct && td.struct_def) {
                        auto si_it = checked_.struct_info.find(td.struct_def);
                        if (si_it != checked_.struct_info.end()) {
                            bool is_tuple = true;
                            for (size_t i = 0;
                                 i < si_it->second.fields_in_order.size();
                                 i++) {
                                if (si_it->second.fields_in_order[i].name !=
                                    std::to_string(i)) {
                                    is_tuple = false;
                                    break;
                                }
                            }
                            if (is_tuple) {
                                MirRvalue::Aggregate agg{};
                                agg.kind = MirRvalue::AggregateKind::Struct;
                                agg.ty = struct_ty;
                                for (const Expr* a : call->args)
                                    agg.elems.push_back(lower_expr(mid, a));
                                MirLocalId tmp = add_temp(struct_ty);
                                emit_stmt(MirStatement{MirStatement::Assign{
                                    .dst = MirPlace::local(tmp),
                                    .src = MirRvalue{std::move(agg)},
                                }});
                                return MirOperand{
                                    MirOperand::Copy{MirPlace::local(tmp)}};
                            }
                        }
                    }
                }
            }
        }

        // Indirect call (callee expression).
        MirOperand callee = lower_expr(mid, call->callee);
        return emit_call(mid, call, std::move(callee));
    }

    MirOperand emit_call(ModuleId mid, const ExprCall* call,
                         MirOperand callee) {
        (void)mid;
        TypeId ret_ty = type_of(static_cast<const Expr*>(call));
        std::optional<MirLocalId> out_local{};
        if (ret_ty != types_.unit()) out_local = add_temp(ret_ty);
        MirBlockId next_bb = add_block();

        MirTerminator::Call t{};
        t.callee = std::move(callee);
        for (const Expr* a : call->args) t.args.push_back(lower_expr(mid, a));
        if (out_local) t.ret = MirPlace::local(*out_local);
        t.next = next_bb;
        terminate(MirTerminator{std::move(t)});
        start_new_block(next_bb);
        if (!out_local) return unit_operand();
        return MirOperand{MirOperand::Copy{MirPlace::local(*out_local)}};
    }

    MirOperand lower_method_call_expr(ModuleId mid, const ExprMethodCall* mc) {
        if (!mc) return unit_operand();
        TypeId recv_ty = type_of(mc->receiver);
        const TypeData& rd = types_.get(recv_ty);
        const Item* nominal = nullptr;
        if (rd.kind == TypeKind::Struct) nominal = rd.struct_def;
        if (rd.kind == TypeKind::Enum) nominal = rd.enum_def;
        if (rd.kind == TypeKind::Ptr) {
            const TypeData& pd = types_.get(rd.pointee);
            if (pd.kind == TypeKind::Struct) nominal = pd.struct_def;
            if (pd.kind == TypeKind::Enum) nominal = pd.enum_def;
        }
        if (!nominal) {
            error(mc->span, "method call on non-nominal type");
            return unit_operand();
        }

        auto it = crate_.inherent_methods.find(nominal);
        if (it == crate_.inherent_methods.end()) {
            error(mc->span, "no inherent methods for receiver type");
            return unit_operand();
        }
        auto mit = it->second.find(mc->method);
        if (mit == it->second.end()) {
            error(mc->span, "unknown method `" + mc->method + "`");
            return unit_operand();
        }
        const ItemFn* method = mit->second;
        if (!method) return unit_operand();
        auto dit = hir_.def_ids.find(static_cast<const Item*>(method));
        if (dit == hir_.def_ids.end()) return unit_operand();

        auto sig_it = checked_.fn_info.find(method);
        if (sig_it == checked_.fn_info.end()) return unit_operand();
        const FnInfo& sig = sig_it->second;
        if (sig.params.empty()) return unit_operand();

        // Compute self argument.
        TypeId self_param_ty = sig.params[0];
        MirOperand self_arg{};
        const TypeData& sd = types_.get(self_param_ty);
        if (sd.kind == TypeKind::Ptr) {
            // Need a pointer. If receiver is already a pointer value, use it.
            if (types_.get(recv_ty).kind == TypeKind::Ptr) {
                self_arg = lower_expr(mid, mc->receiver);
            } else {
                MirPlace recv_place = lower_place(mid, mc->receiver);
                MirLocalId tmp = add_temp(self_param_ty);
                MirRvalue rv{};
                rv.data = MirRvalue::AddrOf{.mutability = sd.mutability,
                                            .place = std::move(recv_place)};
                emit_stmt(MirStatement{MirStatement::Assign{
                    .dst = MirPlace::local(tmp),
                    .src = std::move(rv),
                }});
                self_arg = MirOperand{MirOperand::Copy{MirPlace::local(tmp)}};
            }
        } else {
            self_arg = lower_expr(mid, mc->receiver);
        }

        // Direct call to the method function.
        TypeId ret_ty = type_of(static_cast<const Expr*>(mc));
        std::optional<MirLocalId> out_local{};
        if (ret_ty != types_.unit()) out_local = add_temp(ret_ty);
        MirBlockId next_bb = add_block();
        MirTerminator::Call t{};
        t.callee = MirOperand{MirOperand::Fn{dit->second}};
        t.args.push_back(std::move(self_arg));
        for (size_t i = 0; i < mc->args.size(); i++) {
            const Expr* a = mc->args[i];
            const size_t param_index = 1 + i;
            const bool is_ct = param_index < sig.comptime_params.size() &&
                               sig.comptime_params[param_index];
            if (is_ct)
                t.args.push_back(lower_as_internal_const_item(mid, a));
            else
                t.args.push_back(lower_expr(mid, a));
        }
        if (out_local) t.ret = MirPlace::local(*out_local);
        t.next = next_bb;
        terminate(MirTerminator{std::move(t)});
        start_new_block(next_bb);
        if (!out_local) return unit_operand();
        return MirOperand{MirOperand::Copy{MirPlace::local(*out_local)}};
    }
};

}  // namespace

std::optional<MirProgram> lower_mir(Session& session, const HirCrate& hir) {
    Lowerer l{session, hir};
    return l.run();
}

}  // namespace cog
