#include "mir_interp.hpp"

#include <cstdint>
#include <limits>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "diag.hpp"

namespace cog {
namespace {

static std::optional<size_t> enum_variant_index(const EnumInfo& ei,
                                                std::string_view name) {
    for (size_t i = 0; i < ei.variants_in_order.size(); i++) {
        if (ei.variants_in_order[i] == name) return i;
    }
    return std::nullopt;
}

}  // namespace

MirInterpreter::MirInterpreter(Session& session, const MirProgram& program)
    : session_(session),
      program_(program),
      hir_(*program.hir),
      checked_(*program.hir->checked),
      types_(*program.types) {}

void MirInterpreter::error(Span span, std::string message) {
    session_.diags.push_back(Diagnostic{.severity = Severity::Error,
                                        .span = span,
                                        .message = std::move(message)});
}

bool MirInterpreter::consume_step(Span span) {
    if (step_budget_ == 0) {
        error(span, "comptime step budget exceeded");
        return false;
    }
    step_budget_--;
    return true;
}

bool MirInterpreter::consume_heap(Span span, std::uint64_t units) {
    if (heap_budget_units_ < units) {
        error(span, "comptime heap budget exceeded");
        return false;
    }
    heap_budget_units_ -= units;
    return true;
}

std::optional<std::int64_t> MirInterpreter::as_int(Span span,
                                                   const ComptimeValue& v) {
    if (v.kind != ComptimeValue::Kind::Int) {
        error(span, "expected comptime integer");
        return std::nullopt;
    }
    return v.int_value;
}

std::optional<bool> MirInterpreter::as_bool(Span span, const ComptimeValue& v) {
    if (v.kind != ComptimeValue::Kind::Bool) {
        error(span, "expected comptime bool");
        return std::nullopt;
    }
    return v.bool_value;
}

std::optional<ComptimeValue> MirInterpreter::eval_const(DefId def) {
    if (auto it = const_cache_.find(def); it != const_cache_.end()) {
        if (it->second.state == CacheState::InProgress) {
            error(Span{}, "cycle detected while evaluating const");
            return std::nullopt;
        }
        return it->second.value;
    }

    const_cache_.insert({def, CachedValue{.state = CacheState::InProgress}});
    const MirBody* body = program_.body_for_const(def);
    if (!body) {
        error(Span{}, "missing MIR body for const");
        return std::nullopt;
    }

    auto v = eval_body(*body, /*args=*/{}, body->span);
    if (!v) return std::nullopt;

    const_cache_[def] = CachedValue{.state = CacheState::Done, .value = *v};
    return *v;
}

std::optional<ComptimeValue> MirInterpreter::eval_static(DefId def) {
    if (auto it = static_cache_.find(def); it != static_cache_.end()) {
        if (it->second.state == CacheState::InProgress) {
            error(Span{}, "cycle detected while evaluating static");
            return std::nullopt;
        }
        return it->second.value;
    }

    static_cache_.insert({def, CachedValue{.state = CacheState::InProgress}});
    const MirBody* body = program_.body_for_static(def);
    if (!body) {
        error(Span{}, "missing MIR body for static");
        return std::nullopt;
    }

    auto v = eval_body(*body, /*args=*/{}, body->span);
    if (!v) return std::nullopt;

    static_cache_[def] = CachedValue{.state = CacheState::Done, .value = *v};
    return *v;
}

std::optional<ComptimeValue> MirInterpreter::eval_fn(
    DefId def, const std::vector<ComptimeValue>& args, Span use_site) {
    const MirBody* body = program_.body_for_fn(def);
    if (!body) {
        const HirDef* hd = hir_.def(def);
        if (hd && hd->ast) {
            error(use_site, "cannot call extern function at comptime");
            return std::nullopt;
        }
        error(use_site, "missing MIR body for function call");
        return std::nullopt;
    }
    return eval_body(*body, args, use_site);
}

std::optional<ComptimeValue> MirInterpreter::eval_body(
    const MirBody& body, const std::vector<ComptimeValue>& args,
    Span use_site) {
    if (recursion_depth_ >= recursion_limit_) {
        error(use_site, "comptime recursion limit exceeded");
        return std::nullopt;
    }
    recursion_depth_++;

    const size_t param_count = body.comptime_params.size();
    if (args.size() != param_count) {
        error(use_site, "comptime call arity mismatch");
        recursion_depth_--;
        return std::nullopt;
    }

    std::vector<ComptimeValue> locals{};
    locals.resize(body.locals.size(), ComptimeValue::error());
    for (size_t i = 0; i < args.size(); i++) {
        const size_t local_index = 1 + i;
        if (local_index < locals.size()) locals[local_index] = args[i];
    }

    MirBlockId bb = body.start;
    while (true) {
        if (bb >= body.blocks.size()) {
            error(use_site, "internal error: invalid MIR control flow");
            recursion_depth_--;
            return std::nullopt;
        }
        const MirBlock& block = body.blocks[static_cast<size_t>(bb)];

        for (const MirStatement& st : block.stmts) {
            if (!consume_step(body.span)) {
                recursion_depth_--;
                return std::nullopt;
            }

            if (std::holds_alternative<MirStatement::Assign>(st.data)) {
                const auto& a = std::get<MirStatement::Assign>(st.data);
                auto v = eval_rvalue(body, locals, a.src);
                if (!v) {
                    recursion_depth_--;
                    return std::nullopt;
                }
                if (ComptimeValue* dst = place_ref(body, locals, a.dst)) {
                    *dst = std::move(*v);
                } else {
                    recursion_depth_--;
                    return std::nullopt;
                }
            } else if (std::holds_alternative<MirStatement::Assert>(st.data)) {
                const auto& a = std::get<MirStatement::Assert>(st.data);
                auto cv = eval_operand(body, locals, a.cond);
                if (!cv) {
                    recursion_depth_--;
                    return std::nullopt;
                }
                auto b = as_bool(a.span, *cv);
                if (!b) {
                    recursion_depth_--;
                    return std::nullopt;
                }
                if (!*b) {
                    if (!a.message.empty()) {
                        error(a.span, a.message);
                    } else {
                        error(a.span, "comptime assertion failed");
                    }
                    recursion_depth_--;
                    return std::nullopt;
                }
            }
        }

        if (!consume_step(body.span)) {
            recursion_depth_--;
            return std::nullopt;
        }

        const MirTerminator& term = block.term;
        if (std::holds_alternative<MirTerminator::Return>(term.data)) {
            recursion_depth_--;
            if (body.ret_ty == types_.unit() || body.ret_ty == types_.never())
                return ComptimeValue::unit();
            if (locals.empty()) return ComptimeValue::unit();
            return locals[0];
        }
        if (std::holds_alternative<MirTerminator::Goto>(term.data)) {
            bb = std::get<MirTerminator::Goto>(term.data).target;
            continue;
        }
        if (std::holds_alternative<MirTerminator::SwitchInt>(term.data)) {
            const auto& sw = std::get<MirTerminator::SwitchInt>(term.data);
            auto sv = eval_operand(body, locals, sw.scrut);
            if (!sv) {
                recursion_depth_--;
                return std::nullopt;
            }

            std::uint64_t key = 0;
            if (sv->kind == ComptimeValue::Kind::Bool) {
                key = sv->bool_value ? 1u : 0u;
            } else if (sv->kind == ComptimeValue::Kind::Int) {
                key = static_cast<std::uint64_t>(sv->int_value);
            } else {
                error(body.span, "switch scrutinee is not an integer/bool");
                recursion_depth_--;
                return std::nullopt;
            }

            MirBlockId dest = sw.otherwise;
            for (const auto& [val, target] : sw.cases) {
                if (val == key) {
                    dest = target;
                    break;
                }
            }
            bb = dest;
            continue;
        }
        if (std::holds_alternative<MirTerminator::Call>(term.data)) {
            const auto& c = std::get<MirTerminator::Call>(term.data);
            if (!c.ret && body.ret_ty == types_.never()) {
                error(body.span, "internal error: call in never-typed body");
                recursion_depth_--;
                return std::nullopt;
            }

            // Evaluate the callee.
            DefId callee_def = 0;
            if (std::holds_alternative<MirOperand::Fn>(c.callee.data)) {
                callee_def = std::get<MirOperand::Fn>(c.callee.data).def;
            } else {
                error(body.span,
                      "indirect calls are not supported at comptime yet");
                recursion_depth_--;
                return std::nullopt;
            }

            std::vector<ComptimeValue> call_args{};
            call_args.reserve(c.args.size());
            for (const MirOperand& a : c.args) {
                auto av = eval_operand(body, locals, a);
                if (!av) {
                    recursion_depth_--;
                    return std::nullopt;
                }
                call_args.push_back(std::move(*av));
            }

            auto rv = eval_fn(callee_def, call_args, body.span);
            if (!rv) {
                recursion_depth_--;
                return std::nullopt;
            }

            if (c.ret) {
                if (ComptimeValue* dst = place_ref(body, locals, *c.ret)) {
                    *dst = *rv;
                } else {
                    recursion_depth_--;
                    return std::nullopt;
                }
            }

            bb = c.next;
            continue;
        }
        if (std::holds_alternative<MirTerminator::Unreachable>(term.data)) {
            error(body.span, "reached unreachable during comptime evaluation");
            recursion_depth_--;
            return std::nullopt;
        }

        error(use_site, "internal error: unknown MIR terminator");
        recursion_depth_--;
        return std::nullopt;
    }
}

std::optional<ComptimeValue> MirInterpreter::eval_operand(
    const MirBody& body, std::vector<ComptimeValue>& locals,
    const MirOperand& op) {
    (void)body;
    if (std::holds_alternative<MirOperand::Copy>(op.data)) {
        const auto& c = std::get<MirOperand::Copy>(op.data);
        return read_place(body, locals, c.place);
    }
    if (std::holds_alternative<MirOperand::Move>(op.data)) {
        const auto& c = std::get<MirOperand::Move>(op.data);
        return read_place(body, locals, c.place);
    }
    if (std::holds_alternative<MirOperand::Const>(op.data)) {
        const auto& c = std::get<MirOperand::Const>(op.data);
        (void)c.ty;
        return std::visit(
            [&](const auto& v) -> std::optional<ComptimeValue> {
                using T = std::decay_t<decltype(v)>;
                if constexpr (std::is_same_v<T, MirConst::Unit>) {
                    return ComptimeValue::unit();
                } else if constexpr (std::is_same_v<T, MirConst::Bool>) {
                    return ComptimeValue::bool_(v.v);
                } else if constexpr (std::is_same_v<T, MirConst::Int>) {
                    return ComptimeValue::int_(v.v);
                } else if constexpr (std::is_same_v<T, MirConst::Float>) {
                    return ComptimeValue::float_(v.v);
                } else if constexpr (std::is_same_v<T, MirConst::String>) {
                    if (!consume_heap(body.span,
                                      /*units=*/8 + static_cast<std::uint64_t>(
                                                        v.bytes.size())))
                        return std::nullopt;
                    return ComptimeValue::string(v.bytes);
                }
                return ComptimeValue::error();
            },
            c.value.data);
    }
    if (std::holds_alternative<MirOperand::ConstItem>(op.data)) {
        DefId def = std::get<MirOperand::ConstItem>(op.data).def;
        return eval_const(def);
    }
    if (std::holds_alternative<MirOperand::Fn>(op.data)) {
        error(body.span, "function values are not supported at comptime yet");
        return std::nullopt;
    }
    error(body.span, "unsupported MIR operand at comptime");
    return std::nullopt;
}

std::optional<ComptimeValue> MirInterpreter::eval_rvalue(
    const MirBody& body, std::vector<ComptimeValue>& locals,
    const MirRvalue& rv) {
    if (std::holds_alternative<MirRvalue::Use>(rv.data)) {
        return eval_operand(body, locals, std::get<MirRvalue::Use>(rv.data).op);
    }
    if (std::holds_alternative<MirRvalue::Unary>(rv.data)) {
        const auto& u = std::get<MirRvalue::Unary>(rv.data);
        auto ov = eval_operand(body, locals, u.operand);
        if (!ov) return std::nullopt;
        switch (u.op) {
            case UnaryOp::Neg:
                if (ov->kind == ComptimeValue::Kind::Int)
                    return ComptimeValue::int_(-ov->int_value);
                if (ov->kind == ComptimeValue::Kind::Float)
                    return ComptimeValue::float_(-ov->float_value);
                error(body.span, "unary `-` requires int/float at comptime");
                return std::nullopt;
            case UnaryOp::Not:
                if (ov->kind != ComptimeValue::Kind::Bool) {
                    error(body.span, "unary `!` requires bool at comptime");
                    return std::nullopt;
                }
                return ComptimeValue::bool_(!ov->bool_value);
            case UnaryOp::BitNot: {
                if (ov->kind != ComptimeValue::Kind::Int) {
                    error(body.span, "unary `~` requires integer at comptime");
                    return std::nullopt;
                }
                std::uint64_t x = static_cast<std::uint64_t>(ov->int_value);
                return ComptimeValue::int_(static_cast<std::int64_t>(~x));
            }
            case UnaryOp::Deref:
                error(body.span,
                      "pointer dereference is not supported at comptime yet");
                return std::nullopt;
            case UnaryOp::AddrOf:
            case UnaryOp::AddrOfMut:
                error(body.span, "address-of is not supported at comptime yet");
                return std::nullopt;
        }
    }
    if (std::holds_alternative<MirRvalue::Binary>(rv.data)) {
        const auto& b = std::get<MirRvalue::Binary>(rv.data);
        auto lv = eval_operand(body, locals, b.lhs);
        auto rvv = eval_operand(body, locals, b.rhs);
        if (!lv || !rvv) return std::nullopt;

        if (lv->kind == ComptimeValue::Kind::Int &&
            rvv->kind == ComptimeValue::Kind::Int) {
            std::int64_t a = lv->int_value;
            std::int64_t c = rvv->int_value;
            const std::uint64_t au = static_cast<std::uint64_t>(a);
            const std::uint64_t cu = static_cast<std::uint64_t>(c);

            auto shift_amount = [&]() -> std::optional<unsigned> {
                if (c < 0) return std::nullopt;
                if (c >= 64) return std::nullopt;
                return static_cast<unsigned>(c);
            };

            switch (b.op) {
                case BinaryOp::Add:
                    return ComptimeValue::int_(a + c);
                case BinaryOp::Sub:
                    return ComptimeValue::int_(a - c);
                case BinaryOp::Mul:
                    return ComptimeValue::int_(a * c);
                case BinaryOp::Div:
                    if (c == 0) {
                        error(body.span, "division by zero at comptime");
                        return std::nullopt;
                    }
                    return ComptimeValue::int_(a / c);
                case BinaryOp::Mod:
                    if (c == 0) {
                        error(body.span, "modulo by zero at comptime");
                        return std::nullopt;
                    }
                    return ComptimeValue::int_(a % c);
                case BinaryOp::BitAnd:
                    return ComptimeValue::int_(
                        static_cast<std::int64_t>(au & cu));
                case BinaryOp::BitOr:
                    return ComptimeValue::int_(
                        static_cast<std::int64_t>(au | cu));
                case BinaryOp::BitXor:
                    return ComptimeValue::int_(
                        static_cast<std::int64_t>(au ^ cu));
                case BinaryOp::Shl: {
                    auto sh = shift_amount();
                    if (!sh) {
                        error(body.span, "invalid shift amount at comptime");
                        return std::nullopt;
                    }
                    return ComptimeValue::int_(
                        static_cast<std::int64_t>(au << *sh));
                }
                case BinaryOp::Shr: {
                    auto sh = shift_amount();
                    if (!sh) {
                        error(body.span, "invalid shift amount at comptime");
                        return std::nullopt;
                    }
                    return ComptimeValue::int_(
                        static_cast<std::int64_t>(au >> *sh));
                }
                case BinaryOp::Eq:
                    return ComptimeValue::bool_(a == c);
                case BinaryOp::Ne:
                    return ComptimeValue::bool_(a != c);
                case BinaryOp::Lt:
                    return ComptimeValue::bool_(a < c);
                case BinaryOp::Le:
                    return ComptimeValue::bool_(a <= c);
                case BinaryOp::Gt:
                    return ComptimeValue::bool_(a > c);
                case BinaryOp::Ge:
                    return ComptimeValue::bool_(a >= c);
                case BinaryOp::And:
                    return ComptimeValue::bool_((a != 0) && (c != 0));
                case BinaryOp::Or:
                    return ComptimeValue::bool_((a != 0) || (c != 0));
            }
        }

        if (lv->kind == ComptimeValue::Kind::Float &&
            rvv->kind == ComptimeValue::Kind::Float) {
            double a = lv->float_value;
            double c = rvv->float_value;
            switch (b.op) {
                case BinaryOp::Add:
                    return ComptimeValue::float_(a + c);
                case BinaryOp::Sub:
                    return ComptimeValue::float_(a - c);
                case BinaryOp::Mul:
                    return ComptimeValue::float_(a * c);
                case BinaryOp::Div:
                    return ComptimeValue::float_(a / c);
                case BinaryOp::Eq:
                    return ComptimeValue::bool_(a == c);
                case BinaryOp::Ne:
                    return ComptimeValue::bool_(a != c);
                case BinaryOp::Lt:
                    return ComptimeValue::bool_(a < c);
                case BinaryOp::Le:
                    return ComptimeValue::bool_(a <= c);
                case BinaryOp::Gt:
                    return ComptimeValue::bool_(a > c);
                case BinaryOp::Ge:
                    return ComptimeValue::bool_(a >= c);
                default:
                    break;
            }
        }

        if (lv->kind == ComptimeValue::Kind::Bool &&
            rvv->kind == ComptimeValue::Kind::Bool) {
            bool a = lv->bool_value;
            bool c = rvv->bool_value;
            switch (b.op) {
                case BinaryOp::Eq:
                    return ComptimeValue::bool_(a == c);
                case BinaryOp::Ne:
                    return ComptimeValue::bool_(a != c);
                case BinaryOp::BitAnd:
                    return ComptimeValue::bool_(a & c);
                case BinaryOp::BitOr:
                    return ComptimeValue::bool_(a | c);
                default:
                    break;
            }
        }

        error(body.span, "unsupported operands for binary op at comptime");
        return std::nullopt;
    }
    if (std::holds_alternative<MirRvalue::Cast>(rv.data)) {
        const auto& c = std::get<MirRvalue::Cast>(rv.data);
        auto v = eval_operand(body, locals, c.operand);
        if (!v) return std::nullopt;

        const TypeData& dst = types_.get(c.to);
        if (dst.kind == TypeKind::Ptr) {
            if (v->kind == ComptimeValue::Kind::Ptr) return v;
            if (v->kind != ComptimeValue::Kind::Int) {
                error(body.span,
                      "cast to pointer requires integer at comptime");
                return std::nullopt;
            }
            return ComptimeValue::ptr_(
                static_cast<std::uint64_t>(v->int_value));
        }
        if (dst.kind == TypeKind::Int) {
            if (v->kind == ComptimeValue::Kind::Ptr)
                return ComptimeValue::int_(
                    static_cast<std::int64_t>(v->ptr_value));
            if (v->kind == ComptimeValue::Kind::Int) return v;
            if (v->kind == ComptimeValue::Kind::Float)
                return ComptimeValue::int_(
                    static_cast<std::int64_t>(v->float_value));
            return v;
        }
        if (dst.kind == TypeKind::Float) {
            if (v->kind == ComptimeValue::Kind::Float) return v;
            if (v->kind == ComptimeValue::Kind::Int)
                return ComptimeValue::float_(static_cast<double>(v->int_value));
            return v;
        }
        if (dst.kind == TypeKind::Unit) return ComptimeValue::unit();
        return v;
    }
    if (std::holds_alternative<MirRvalue::AddrOf>(rv.data)) {
        error(body.span, "address-of is not supported at comptime yet");
        return std::nullopt;
    }
    if (std::holds_alternative<MirRvalue::EnumTag>(rv.data)) {
        const auto& t = std::get<MirRvalue::EnumTag>(rv.data);
        auto ev = eval_operand(body, locals, t.operand);
        if (!ev) return std::nullopt;
        if (ev->kind != ComptimeValue::Kind::Enum || !ev->enum_def) {
            error(body.span, "enum_tag requires an enum value at comptime");
            return std::nullopt;
        }
        auto it = checked_.enum_info.find(ev->enum_def);
        if (it == checked_.enum_info.end()) {
            error(body.span, "missing enum info in enum_tag");
            return std::nullopt;
        }
        const EnumInfo& ei = it->second;
        if (auto di = ei.discriminants.find(ev->enum_variant);
            di != ei.discriminants.end())
            return ComptimeValue::int_(di->second);
        if (auto idx = enum_variant_index(ei, ev->enum_variant))
            return ComptimeValue::int_(static_cast<std::int64_t>(*idx));
        error(body.span, "unknown enum variant in enum_tag");
        return std::nullopt;
    }
    if (std::holds_alternative<MirRvalue::Aggregate>(rv.data)) {
        const auto& a = std::get<MirRvalue::Aggregate>(rv.data);
        std::vector<ComptimeValue> elems{};
        elems.reserve(a.elems.size());
        for (const MirOperand& e : a.elems) {
            auto v = eval_operand(body, locals, e);
            if (!v) return std::nullopt;
            elems.push_back(std::move(*v));
        }

        if (!consume_heap(
                body.span,
                /*units=*/4 + static_cast<std::uint64_t>(elems.size()) * 2))
            return std::nullopt;

        switch (a.kind) {
            case MirRvalue::AggregateKind::Tuple: {
                ComptimeValue out{};
                out.kind = ComptimeValue::Kind::Tuple;
                out.tuple_elems = std::move(elems);
                return out;
            }
            case MirRvalue::AggregateKind::Array:
                return ComptimeValue::array(std::move(elems));
            case MirRvalue::AggregateKind::Struct: {
                const TypeData& td = types_.get(a.ty);
                if (td.kind != TypeKind::Struct || !td.struct_def)
                    return ComptimeValue::error();
                auto si_it = checked_.struct_info.find(td.struct_def);
                if (si_it == checked_.struct_info.end())
                    return ComptimeValue::error();
                const StructInfo& si = si_it->second;
                ComptimeValue out{};
                out.kind = ComptimeValue::Kind::Struct;
                out.struct_def = td.struct_def;
                for (size_t i = 0;
                     i < si.fields_in_order.size() && i < elems.size(); i++) {
                    out.struct_fields.insert(
                        {si.fields_in_order[i].name, elems[i]});
                }
                return out;
            }
            case MirRvalue::AggregateKind::EnumVariant: {
                const TypeData& td = types_.get(a.ty);
                if (td.kind != TypeKind::Enum || !td.enum_def)
                    return ComptimeValue::error();
                auto ei_it = checked_.enum_info.find(td.enum_def);
                if (ei_it == checked_.enum_info.end())
                    return ComptimeValue::error();
                const EnumInfo& ei = ei_it->second;
                if (a.variant_index >= ei.variants_in_order.size())
                    return ComptimeValue::error();

                ComptimeValue out{};
                out.kind = ComptimeValue::Kind::Enum;
                out.enum_def = td.enum_def;
                out.enum_variant = ei.variants_in_order[a.variant_index];
                out.enum_payload = std::move(elems);
                return out;
            }
        }
    }

    error(body.span, "unsupported MIR rvalue at comptime");
    return std::nullopt;
}

std::optional<ComptimeValue> MirInterpreter::read_place(
    const MirBody& body, std::vector<ComptimeValue>& locals,
    const MirPlace& place) {
    if (std::holds_alternative<MirPlace::Local>(place.base)) {
        if (ComptimeValue* v = place_ref(body, locals, place)) return *v;
        return std::nullopt;
    }

    if (std::holds_alternative<MirPlace::Static>(place.base)) {
        DefId def = std::get<MirPlace::Static>(place.base).def;
        auto v = eval_static(def);
        if (!v) return std::nullopt;

        ComptimeValue cur = *v;
        bool in_enum_payload = false;
        for (const MirProjection& proj : place.projection) {
            if (std::holds_alternative<MirProjection::Deref>(proj.data)) {
                error(body.span,
                      "deref is not supported for comptime static reads");
                return std::nullopt;
            }
            if (std::holds_alternative<MirProjection::Downcast>(proj.data)) {
                if (cur.kind != ComptimeValue::Kind::Enum) {
                    error(body.span, "downcast on non-enum at comptime");
                    return std::nullopt;
                }
                in_enum_payload = true;
                continue;
            }
            if (std::holds_alternative<MirProjection::Field>(proj.data)) {
                std::uint32_t idx =
                    std::get<MirProjection::Field>(proj.data).index;
                if (in_enum_payload && cur.kind == ComptimeValue::Kind::Enum) {
                    if (idx >= cur.enum_payload.size()) return std::nullopt;
                    cur = cur.enum_payload[idx];
                    in_enum_payload = false;
                    continue;
                }
                if (cur.kind == ComptimeValue::Kind::Tuple) {
                    if (idx >= cur.tuple_elems.size()) return std::nullopt;
                    cur = cur.tuple_elems[idx];
                    continue;
                }
                if (cur.kind == ComptimeValue::Kind::Struct && cur.struct_def) {
                    auto si_it = checked_.struct_info.find(cur.struct_def);
                    if (si_it == checked_.struct_info.end())
                        return std::nullopt;
                    const StructInfo& si = si_it->second;
                    if (idx >= si.fields_in_order.size()) return std::nullopt;
                    const std::string& name = si.fields_in_order[idx].name;
                    auto it = cur.struct_fields.find(name);
                    if (it == cur.struct_fields.end()) return std::nullopt;
                    cur = it->second;
                    continue;
                }
                return std::nullopt;
            }
            if (std::holds_alternative<MirProjection::Index>(proj.data)) {
                MirLocalId idx_local =
                    std::get<MirProjection::Index>(proj.data).index_local;
                if (idx_local >= locals.size()) return std::nullopt;
                auto idx = as_int(body.span, locals[idx_local]);
                if (!idx) return std::nullopt;
                if (cur.kind != ComptimeValue::Kind::Array) return std::nullopt;
                if (*idx < 0) return std::nullopt;
                if (static_cast<size_t>(*idx) >= cur.array_elems.size())
                    return std::nullopt;
                cur = cur.array_elems[static_cast<size_t>(*idx)];
                continue;
            }
        }
        return cur;
    }

    error(body.span, "unsupported MIR place base at comptime");
    return std::nullopt;
}

ComptimeValue* MirInterpreter::place_ref(const MirBody& body,
                                         std::vector<ComptimeValue>& locals,
                                         const MirPlace& place) {
    if (!std::holds_alternative<MirPlace::Local>(place.base)) {
        error(body.span, "assignment to non-local place at comptime");
        return nullptr;
    }
    MirLocalId base = std::get<MirPlace::Local>(place.base).local;
    if (base >= locals.size()) {
        error(body.span, "internal error: local id out of range at comptime");
        return nullptr;
    }

    ComptimeValue* cur = &locals[base];
    bool in_enum_payload = false;

    for (const MirProjection& proj : place.projection) {
        if (std::holds_alternative<MirProjection::Deref>(proj.data)) {
            error(body.span, "deref is not supported at comptime yet");
            return nullptr;
        }
        if (std::holds_alternative<MirProjection::Downcast>(proj.data)) {
            if (cur->kind != ComptimeValue::Kind::Enum) {
                error(body.span, "downcast on non-enum at comptime");
                return nullptr;
            }
            in_enum_payload = true;
            continue;
        }
        if (std::holds_alternative<MirProjection::Field>(proj.data)) {
            std::uint32_t idx = std::get<MirProjection::Field>(proj.data).index;
            if (in_enum_payload && cur->kind == ComptimeValue::Kind::Enum) {
                if (idx >= cur->enum_payload.size()) {
                    error(body.span,
                          "enum payload field out of range at comptime");
                    return nullptr;
                }
                cur = &cur->enum_payload[idx];
                in_enum_payload = false;
                continue;
            }
            if (cur->kind == ComptimeValue::Kind::Tuple) {
                if (idx >= cur->tuple_elems.size()) {
                    error(body.span, "tuple field out of range at comptime");
                    return nullptr;
                }
                cur = &cur->tuple_elems[idx];
                continue;
            }
            if (cur->kind == ComptimeValue::Kind::Struct && cur->struct_def) {
                auto si_it = checked_.struct_info.find(cur->struct_def);
                if (si_it == checked_.struct_info.end()) return nullptr;
                const StructInfo& si = si_it->second;
                if (idx >= si.fields_in_order.size()) return nullptr;
                const std::string& name = si.fields_in_order[idx].name;
                auto it = cur->struct_fields.find(name);
                if (it == cur->struct_fields.end()) {
                    error(body.span, "missing struct field value at comptime");
                    return nullptr;
                }
                cur = &it->second;
                continue;
            }
            error(body.span, "field projection on non-aggregate at comptime");
            return nullptr;
        }
        if (std::holds_alternative<MirProjection::Index>(proj.data)) {
            MirLocalId idx_local =
                std::get<MirProjection::Index>(proj.data).index_local;
            if (idx_local >= locals.size()) return nullptr;
            auto idx = as_int(body.span, locals[idx_local]);
            if (!idx) return nullptr;
            if (*idx < 0) return nullptr;
            if (cur->kind != ComptimeValue::Kind::Array) {
                error(body.span, "indexing non-array at comptime");
                return nullptr;
            }
            size_t i = static_cast<size_t>(*idx);
            if (i >= cur->array_elems.size()) {
                error(body.span, "comptime array index out of range");
                return nullptr;
            }
            cur = &cur->array_elems[i];
            continue;
        }
    }
    return cur;
}

}  // namespace cog
