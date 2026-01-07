#include "mir.hpp"
#include "ast.hpp"

#include <ostream>
#include <string>
#include <string_view>

namespace cog {
namespace {

static void dump_place(std::ostream& os, const MirPlace& p) {
    std::visit(
        [&](const auto& base) {
            using T = std::decay_t<decltype(base)>;
            if constexpr (std::is_same_v<T, MirPlace::Local>) {
                os << "local#" << base.local;
            } else if constexpr (std::is_same_v<T, MirPlace::Static>) {
                os << "static(def#" << base.def << ")";
            }
        },
        p.base);
    for (const MirProjection& proj : p.projection) {
        std::visit(
            [&](const auto& pr) {
                using T = std::decay_t<decltype(pr)>;
                if constexpr (std::is_same_v<T, MirProjection::Deref>) {
                    os << ".*";
                } else if constexpr (std::is_same_v<T, MirProjection::Field>) {
                    os << ".field#" << pr.index;
                } else if constexpr (std::is_same_v<T, MirProjection::Index>) {
                    os << "[local#" << pr.index_local << "]";
                } else if constexpr (std::is_same_v<T,
                                                    MirProjection::Downcast>) {
                    os << ".downcast#" << pr.variant_index;
                }
            },
            proj.data);
    }
}

static void dump_const(std::ostream& os, const MirConst& c) {
    std::visit(
        [&](const auto& v) {
            using T = std::decay_t<decltype(v)>;
            if constexpr (std::is_same_v<T, MirConst::Unit>) {
                os << "()";
            } else if constexpr (std::is_same_v<T, MirConst::Bool>) {
                os << (v.v ? "true" : "false");
            } else if constexpr (std::is_same_v<T, MirConst::Int>) {
                os << v.v;
            } else if constexpr (std::is_same_v<T, MirConst::Float>) {
                os << v.v;
            } else if constexpr (std::is_same_v<T, MirConst::Type>) {
                os << "type(" << v.ty << ")";
            } else if constexpr (std::is_same_v<T, MirConst::String>) {
                os << (v.is_c_string ? "c\"" : "\"") << v.bytes << "\"";
            }
        },
        c.data);
}

static void dump_operand(std::ostream& os, const TypeStore& ts,
                         const MirOperand& op) {
    std::visit(
        [&](const auto& v) {
            using T = std::decay_t<decltype(v)>;
            if constexpr (std::is_same_v<T, MirOperand::Copy>) {
                os << "copy(";
                dump_place(os, v.place);
                os << ")";
            } else if constexpr (std::is_same_v<T, MirOperand::Move>) {
                os << "move(";
                dump_place(os, v.place);
                os << ")";
            } else if constexpr (std::is_same_v<T, MirOperand::Const>) {
                os << "const(" << ts.to_string(v.ty) << ", ";
                dump_const(os, v.value);
                os << ")";
            } else if constexpr (std::is_same_v<T, MirOperand::Fn>) {
                os << "fn(def#" << v.def << ")";
            } else if constexpr (std::is_same_v<T, MirOperand::ConstItem>) {
                os << "const_item(def#" << v.def << ")";
            }
        },
        op.data);
}

static void dump_rvalue(std::ostream& os, const TypeStore& ts,
                        const MirRvalue& rv) {
    std::visit(
        [&](const auto& v) {
            using T = std::decay_t<decltype(v)>;
            if constexpr (std::is_same_v<T, MirRvalue::Use>) {
                dump_operand(os, ts, v.op);
            } else if constexpr (std::is_same_v<T, MirRvalue::Unary>) {
                os << "unary(" << ast_unary_op_str(v.op) << ", ";
                dump_operand(os, ts, v.operand);
                os << ")";
            } else if constexpr (std::is_same_v<T, MirRvalue::Binary>) {
                os << "binary(" << ast_binary_op_str(v.op) << ", ";
                dump_operand(os, ts, v.lhs);
                os << ", ";
                dump_operand(os, ts, v.rhs);
                os << ")";
            } else if constexpr (std::is_same_v<T, MirRvalue::Cast>) {
                os << "cast(";
                dump_operand(os, ts, v.operand);
                os << " as " << ts.to_string(v.to) << ")";
            } else if constexpr (std::is_same_v<T, MirRvalue::AddrOf>) {
                os << (v.mutability == Mutability::Mut ? "&mut " : "&");
                dump_place(os, v.place);
            } else if constexpr (std::is_same_v<T, MirRvalue::EnumTag>) {
                os << "enum_tag(";
                dump_operand(os, ts, v.operand);
                os << ")";
            } else if constexpr (std::is_same_v<T, MirRvalue::Intrinsic>) {
                auto kind_name = [&]() -> std::string_view {
                    switch (v.kind) {
                        case MirRvalue::IntrinsicKind::SizeOf:
                            return "size_of";
                        case MirRvalue::IntrinsicKind::AlignOf:
                            return "align_of";
                        case MirRvalue::IntrinsicKind::TypeInfo:
                            return "type_info";
                        case MirRvalue::IntrinsicKind::TypeUnit:
                            return "type_unit";
                        case MirRvalue::IntrinsicKind::TypeNever:
                            return "type_never";
                        case MirRvalue::IntrinsicKind::TypePtrConst:
                            return "type_ptr_const";
                        case MirRvalue::IntrinsicKind::TypePtrMut:
                            return "type_ptr_mut";
                        case MirRvalue::IntrinsicKind::TypeSlice:
                            return "type_slice";
                        case MirRvalue::IntrinsicKind::TypeArray:
                            return "type_array";
                        case MirRvalue::IntrinsicKind::TypeTuple:
                            return "type_tuple";
                        case MirRvalue::IntrinsicKind::TypeFn:
                            return "type_fn";
                        case MirRvalue::IntrinsicKind::TypeStruct:
                            return "type_struct";
                        case MirRvalue::IntrinsicKind::TypeEnum:
                            return "type_enum";
                    }
                    return "<intrinsic>";
                };
                os << "intrinsic(" << kind_name() << ", [";
                for (size_t i = 0; i < v.args.size(); i++) {
                    if (i) os << ", ";
                    dump_operand(os, ts, v.args[i]);
                }
                os << "])";
            } else if constexpr (std::is_same_v<T, MirRvalue::Aggregate>) {
                os << "agg(";
                switch (v.kind) {
                    case MirRvalue::AggregateKind::Tuple:
                        os << "tuple";
                        break;
                    case MirRvalue::AggregateKind::Array:
                        os << "array";
                        break;
                    case MirRvalue::AggregateKind::Struct:
                        os << "struct";
                        break;
                    case MirRvalue::AggregateKind::EnumVariant:
                        os << "enum_variant#" << v.variant_index;
                        break;
                }
                os << " : " << ts.to_string(v.ty);
                os << " [";
                for (size_t i = 0; i < v.elems.size(); i++) {
                    if (i) os << ", ";
                    dump_operand(os, ts, v.elems[i]);
                }
                os << "])";
            }
        },
        rv.data);
}

}  // namespace

const MirBody* MirProgram::body_for_fn(DefId def) const {
    auto it = fn_bodies.find(def);
    if (it == fn_bodies.end()) return nullptr;
    if (it->second >= bodies.size()) return nullptr;
    return &bodies[static_cast<size_t>(it->second)];
}

const MirBody* MirProgram::body_for_const(DefId def) const {
    auto it = const_bodies.find(def);
    if (it == const_bodies.end()) return nullptr;
    if (it->second >= bodies.size()) return nullptr;
    return &bodies[static_cast<size_t>(it->second)];
}

const MirBody* MirProgram::body_for_static(DefId def) const {
    auto it = static_bodies.find(def);
    if (it == static_bodies.end()) return nullptr;
    if (it->second >= bodies.size()) return nullptr;
    return &bodies[static_cast<size_t>(it->second)];
}

void dump_mir(std::ostream& os, const MirProgram& program) {
    os << "mir.program\n";
    os << "  bodies: " << program.bodies.size() << "\n";
    for (const MirBody& b : program.bodies) {
        dump_mir_body(os, program, b);
    }
}

void dump_mir_body(std::ostream& os, const MirProgram& program,
                   const MirBody& body) {
    const TypeStore& ts = *program.types;

    os << "mir.body#" << body.id << " owner=def#" << body.owner << "\n";
    os << "  ret: " << ts.to_string(body.ret_ty) << "\n";
    os << "  locals:\n";
    for (size_t i = 0; i < body.locals.size(); i++) {
        os << "    local#" << i << " " << body.locals[i].name << ": "
           << ts.to_string(body.locals[i].ty);
        if (i > 0 && i - 1 < body.comptime_params.size() &&
            body.comptime_params[i - 1])
            os << " (comptime)";
        os << "\n";
    }
    os << "  blocks:\n";
    for (size_t bid = 0; bid < body.blocks.size(); bid++) {
        os << "    bb" << bid << ":\n";
        for (const MirStatement& st : body.blocks[bid].stmts) {
            os << "      ";
            std::visit(
                [&](const auto& s) {
                    using T = std::decay_t<decltype(s)>;
                    if constexpr (std::is_same_v<T, MirStatement::Assign>) {
                        dump_place(os, s.dst);
                        os << " = ";
                        dump_rvalue(os, ts, s.src);
                    } else if constexpr (std::is_same_v<T,
                                                        MirStatement::Assert>) {
                        os << "assert(";
                        dump_operand(os, ts, s.cond);
                        os << ")";
                    }
                },
                st.data);
            os << "\n";
        }
        os << "      ";
        std::visit(
            [&](const auto& t) {
                using T = std::decay_t<decltype(t)>;
                if constexpr (std::is_same_v<T, MirTerminator::Return>) {
                    os << "return";
                } else if constexpr (std::is_same_v<T, MirTerminator::Goto>) {
                    os << "goto bb" << t.target;
                } else if constexpr (std::is_same_v<T,
                                                    MirTerminator::SwitchInt>) {
                    os << "switch ";
                    dump_operand(os, ts, t.scrut);
                    os << " {";
                    for (const auto& [val, dst] : t.cases) {
                        os << " " << val << "=>bb" << dst;
                    }
                    os << " _=>bb" << t.otherwise << " }";
                } else if constexpr (std::is_same_v<T, MirTerminator::Call>) {
                    os << "call ";
                    dump_operand(os, ts, t.callee);
                    os << "(";
                    for (size_t i = 0; i < t.args.size(); i++) {
                        if (i) os << ", ";
                        dump_operand(os, ts, t.args[i]);
                    }
                    os << ")";
                    if (t.ret) {
                        os << " -> ";
                        dump_place(os, *t.ret);
                    }
                    os << "; goto bb" << t.next;
                } else if constexpr (std::is_same_v<
                                         T, MirTerminator::Unreachable>) {
                    os << "unreachable";
                }
            },
            body.blocks[bid].term.data);
        os << "\n";
    }
}

}  // namespace cog
