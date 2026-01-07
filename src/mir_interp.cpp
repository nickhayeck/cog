#include "mir_interp.hpp"

#include <algorithm>
#include <cstdint>
#include <limits>
#include <optional>
#include <sstream>
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

MirInterpreter::MirInterpreter(Session& session, const MirProgram& program,
                               TargetLayout target_layout)
    : session_(session),
      program_(program),
      hir_(*program.hir),
      crate_(*program.hir->crate),
      checked_(*program.hir->checked),
      types_(const_cast<TypeStore&>(*program.types)),
      layout_(session, types_, checked_.struct_info, checked_.enum_info,
              target_layout) {
    type_ctx_.cache = &checked_.type_construct_cache;
    type_ctx_.arena = &crate_.builtins_arena;
    type_ctx_.struct_info = &checked_.struct_info;
    type_ctx_.enum_info = &checked_.enum_info;

    typeinfo_struct_ = find_struct_global("TypeInfo");
    typekind_enum_ = find_enum_in_builtin_module("TypeKind");
}

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

std::optional<std::uint64_t> MirInterpreter::heap_alloc(Span span,
                                                        ComptimeValue v) {
    // Coarse heap accounting: charge a small header, plus a linear cost for
    // nested aggregates.
    std::uint64_t units = 8;
    switch (v.kind) {
        case ComptimeValue::Kind::String:
            units += static_cast<std::uint64_t>(v.string_value.size());
            break;
        case ComptimeValue::Kind::Array:
            units += static_cast<std::uint64_t>(v.array_elems.size()) * 2;
            break;
        case ComptimeValue::Kind::Tuple:
            units += static_cast<std::uint64_t>(v.tuple_elems.size()) * 2;
            break;
        case ComptimeValue::Kind::Struct:
            units += static_cast<std::uint64_t>(v.struct_fields.size()) * 2;
            break;
        case ComptimeValue::Kind::Enum:
            units += static_cast<std::uint64_t>(v.enum_payload.size()) * 2;
            break;
        default:
            break;
    }
    if (!consume_heap(span, units)) return std::nullopt;
    heap_.push_back(HeapObject{.value = std::move(v)});
    return heap_.size();  // 1-based IDs
}

const ComptimeValue* MirInterpreter::heap_deref(Span span,
                                                const ComptimeValue& ref) {
    if (ref.kind != ComptimeValue::Kind::Ref) {
        error(span, "expected a comptime reference (`&place` result)");
        return nullptr;
    }
    if (ref.ptr_value == 0 || ref.ptr_value > heap_.size()) {
        error(span, "invalid comptime reference");
        return nullptr;
    }
    return &heap_[static_cast<size_t>(ref.ptr_value - 1)].value;
}

std::string MirInterpreter::type_key(TypeId t) {
    const TypeData& d = types_.get(t);
    switch (d.kind) {
        case TypeKind::Error:
            return "err";
        case TypeKind::Unit:
            return "unit";
        case TypeKind::Bool:
            return "bool";
        case TypeKind::Int:
        case TypeKind::Float:
        case TypeKind::Never:
        case TypeKind::TypeType:
        case TypeKind::Self:
            // `to_string` is stable for primitives.
            return types_.to_string(t);
        case TypeKind::Ptr: {
            std::ostringstream out;
            out << (d.mutability == Mutability::Mut ? "mut*" : "const*");
            out << "<" << type_key(d.pointee) << ">";
            return out.str();
        }
        case TypeKind::Slice: {
            std::ostringstream out;
            out << "slice<" << type_key(d.elem) << ">";
            return out.str();
        }
        case TypeKind::Array: {
            std::ostringstream out;
            out << "array<" << type_key(d.elem) << ">[";
            if (d.array_len_value) {
                out << *d.array_len_value;
            } else {
                out << "expr@"
                    << reinterpret_cast<std::uintptr_t>(d.array_len_expr);
            }
            out << "]";
            return out.str();
        }
        case TypeKind::Tuple: {
            std::ostringstream out;
            out << "tuple(";
            for (size_t i = 0; i < d.tuple_elems.size(); i++) {
                if (i) out << ",";
                out << type_key(d.tuple_elems[i]);
            }
            out << ")";
            return out.str();
        }
        case TypeKind::Fn: {
            std::ostringstream out;
            out << "fn(";
            for (size_t i = 0; i < d.fn_params.size(); i++) {
                if (i) out << ",";
                out << type_key(d.fn_params[i]);
            }
            out << ")->" << type_key(d.fn_ret);
            return out.str();
        }
        case TypeKind::Struct: {
            std::ostringstream out;
            out << "struct@"
                << reinterpret_cast<std::uintptr_t>(d.struct_def);
            return out.str();
        }
        case TypeKind::Enum: {
            std::ostringstream out;
            out << "enum@" << reinterpret_cast<std::uintptr_t>(d.enum_def);
            return out.str();
        }
    }
    return "<type>";
}

std::string MirInterpreter::value_key(const ComptimeValue& v,
                                      std::unordered_set<std::uint64_t>& visiting) {
    switch (v.kind) {
        case ComptimeValue::Kind::Error:
            return "err";
        case ComptimeValue::Kind::Unit:
            return "unit";
        case ComptimeValue::Kind::Bool:
            return v.bool_value ? "bool:true" : "bool:false";
        case ComptimeValue::Kind::Int:
            return "int:" + std::to_string(v.int_value);
        case ComptimeValue::Kind::Float:
            return "float:" + std::to_string(v.float_value);
        case ComptimeValue::Kind::Type:
            return "type:" + type_key(v.type_value);
        case ComptimeValue::Kind::Ptr:
            return "ptr:" + std::to_string(v.ptr_value);
        case ComptimeValue::Kind::Ref: {
            if (v.ptr_value == 0) return "ref:null";
            if (visiting.contains(v.ptr_value)) return "ref:<cycle>";
            visiting.insert(v.ptr_value);
            const ComptimeValue* pointee = heap_deref(Span{}, v);
            std::string inner =
                pointee ? value_key(*pointee, visiting) : std::string("<invalid>");
            visiting.erase(v.ptr_value);
            return "ref<" + inner + ">";
        }
        case ComptimeValue::Kind::String:
            return "str:" + v.string_value;
        case ComptimeValue::Kind::Array: {
            std::ostringstream out;
            out << "arr[";
            for (size_t i = 0; i < v.array_elems.size(); i++) {
                if (i) out << ",";
                out << value_key(v.array_elems[i], visiting);
            }
            out << "]";
            return out.str();
        }
        case ComptimeValue::Kind::Tuple: {
            std::ostringstream out;
            out << "tup(";
            for (size_t i = 0; i < v.tuple_elems.size(); i++) {
                if (i) out << ",";
                out << value_key(v.tuple_elems[i], visiting);
            }
            out << ")";
            return out.str();
        }
        case ComptimeValue::Kind::Struct: {
            std::ostringstream out;
            out << "struct@" << reinterpret_cast<std::uintptr_t>(v.struct_def)
                << "{";
            if (v.struct_def) {
                bool first = true;
                for (const FieldDecl* f : v.struct_def->fields) {
                    if (!f) continue;
                    if (!first) out << ",";
                    first = false;
                    out << f->name << "=";
                    auto it = v.struct_fields.find(f->name);
                    if (it == v.struct_fields.end()) {
                        out << "<missing>";
                    } else {
                        out << value_key(it->second, visiting);
                    }
                }
            } else {
                // Fall back to sorted keys if we don't have a field list.
                std::vector<std::string> keys{};
                keys.reserve(v.struct_fields.size());
                for (const auto& [k, _] : v.struct_fields) keys.push_back(k);
                std::sort(keys.begin(), keys.end());
                bool first = true;
                for (const std::string& k : keys) {
                    if (!first) out << ",";
                    first = false;
                    out << k << "=" << value_key(v.struct_fields.at(k), visiting);
                }
            }
            out << "}";
            return out.str();
        }
        case ComptimeValue::Kind::Enum: {
            std::ostringstream out;
            out << "enum@" << reinterpret_cast<std::uintptr_t>(v.enum_def) << "::"
                << v.enum_variant << "(";
            for (size_t i = 0; i < v.enum_payload.size(); i++) {
                if (i) out << ",";
                out << value_key(v.enum_payload[i], visiting);
            }
            out << ")";
            return out.str();
        }
    }
    return "<value>";
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

std::optional<TypeId> MirInterpreter::as_type(Span span, const ComptimeValue& v) {
    if (v.kind != ComptimeValue::Kind::Type) {
        error(span, "expected a `type` value");
        return std::nullopt;
    }
    return v.type_value;
}

const std::vector<ComptimeValue>* MirInterpreter::as_array_elems(
    Span span, const ComptimeValue& v) {
    if (v.kind == ComptimeValue::Kind::Array) return &v.array_elems;
    if (v.kind == ComptimeValue::Kind::Ref) {
        const ComptimeValue* p = heap_deref(span, v);
        if (!p) return nullptr;
        if (p->kind != ComptimeValue::Kind::Array) {
            error(span, "expected a reference to an array value");
            return nullptr;
        }
        return &p->array_elems;
    }
    error(span, "expected an array value or `&` reference to one");
    return nullptr;
}

const ItemStruct* MirInterpreter::find_struct_global(std::string_view name) const {
    for (const Module& m : crate_.modules) {
        auto it = m.types.find(std::string(name));
        if (it == m.types.end()) continue;
        if (it->second && it->second->kind == AstNodeKind::ItemStruct)
            return static_cast<const ItemStruct*>(it->second);
    }
    return nullptr;
}

const ItemEnum* MirInterpreter::find_enum_in_builtin_module(
    std::string_view name) const {
    if (crate_.root >= crate_.modules.size()) return nullptr;
    auto it = crate_.modules[crate_.root].submodules.find("builtin");
    if (it == crate_.modules[crate_.root].submodules.end()) return nullptr;
    ModuleId bid = it->second;
    if (bid >= crate_.modules.size()) return nullptr;
    auto tit = crate_.modules[bid].types.find(std::string(name));
    if (tit == crate_.modules[bid].types.end()) return nullptr;
    if (!tit->second || tit->second->kind != AstNodeKind::ItemEnum)
        return nullptr;
    return static_cast<const ItemEnum*>(tit->second);
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
                } else if constexpr (std::is_same_v<T, MirConst::Type>) {
                    return ComptimeValue::type_(v.ty);
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
        const auto& a = std::get<MirRvalue::AddrOf>(rv.data);
        auto pv = read_place(body, locals, a.place);
        if (!pv) return std::nullopt;
        auto id = heap_alloc(body.span, *pv);
        if (!id) return std::nullopt;
        return ComptimeValue::ref_(*id);
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
    if (std::holds_alternative<MirRvalue::Intrinsic>(rv.data)) {
        const auto& in = std::get<MirRvalue::Intrinsic>(rv.data);

        auto eval_arg = [&](size_t i) -> std::optional<ComptimeValue> {
            if (i >= in.args.size()) return std::nullopt;
            return eval_operand(body, locals, in.args[i]);
        };

        auto cache_lookup = [&](const std::string& key) -> std::optional<TypeId> {
            if (!type_ctx_.cache) return std::nullopt;
            auto it = type_ctx_.cache->find(key);
            if (it == type_ctx_.cache->end()) return std::nullopt;
            return it->second;
        };
        auto cache_store = [&](std::string key, TypeId t) {
            if (!type_ctx_.cache) return;
            type_ctx_.cache->insert({std::move(key), t});
        };

        switch (in.kind) {
            case MirRvalue::IntrinsicKind::SizeOf:
            case MirRvalue::IntrinsicKind::AlignOf: {
                if (in.args.size() != 1) {
                    error(body.span,
                          "internal error: size_of/align_of intrinsic arity mismatch");
                    return std::nullopt;
                }
                auto tv = eval_arg(0);
                if (!tv) return std::nullopt;
                auto ty = as_type(body.span, *tv);
                if (!ty) return std::nullopt;
                const TypeData& td = types_.get(*ty);
                if (td.kind == TypeKind::TypeType) {
                    error(body.span, "builtin::size_of/align_of require a runtime type");
                    return std::nullopt;
                }
                std::optional<std::uint64_t> v{};
                if (in.kind == MirRvalue::IntrinsicKind::SizeOf)
                    v = layout_.size_of(*ty, body.span);
                else
                    v = layout_.align_of(*ty, body.span);
                if (!v) return std::nullopt;
                if (*v > static_cast<std::uint64_t>(
                             std::numeric_limits<std::int64_t>::max())) {
                    error(body.span, "comptime integer overflow");
                    return std::nullopt;
                }
                return ComptimeValue::int_(static_cast<std::int64_t>(*v));
            }
            case MirRvalue::IntrinsicKind::TypeInfo: {
                if (in.args.size() != 1) {
                    error(body.span,
                          "internal error: type_info intrinsic arity mismatch");
                    return std::nullopt;
                }
                if (!typeinfo_struct_ || !typekind_enum_) {
                    error(body.span,
                          "internal error: missing builtin `TypeInfo`/`TypeKind`");
                    return std::nullopt;
                }
                auto tv = eval_arg(0);
                if (!tv) return std::nullopt;
                auto ty = as_type(body.span, *tv);
                if (!ty) return std::nullopt;

                const TypeData& td = types_.get(*ty);
                if (td.kind == TypeKind::TypeType) {
                    error(body.span, "builtin::type_info requires a runtime type");
                    return std::nullopt;
                }

                auto kind_name = [&]() -> std::optional<std::string> {
                    switch (td.kind) {
                        case TypeKind::Int:
                            return "Int";
                        case TypeKind::Float:
                            return "Float";
                        case TypeKind::Bool:
                            return "Bool";
                        case TypeKind::Unit:
                            return "Unit";
                        case TypeKind::Never:
                            return "Never";
                        case TypeKind::Ptr:
                            return "Ptr";
                        case TypeKind::Slice:
                            return "Slice";
                        case TypeKind::Array:
                            return "Array";
                        case TypeKind::Tuple:
                            return "Tuple";
                        case TypeKind::Struct:
                            return "Struct";
                        case TypeKind::Enum:
                            return "Enum";
                        case TypeKind::Fn:
                            return "Fn";
                        case TypeKind::Error:
                        case TypeKind::TypeType:
                        case TypeKind::Self:
                            return std::nullopt;
                    }
                    return std::nullopt;
                }();
                if (!kind_name) {
                    error(body.span,
                          "builtin::type_info does not support this type kind");
                    return std::nullopt;
                }

                std::uint64_t sz = 0;
                std::uint64_t al = 0;
                if (types_.is_sized(*ty)) {
                    auto sz_opt = layout_.size_of(*ty, body.span);
                    auto al_opt = layout_.align_of(*ty, body.span);
                    if (!sz_opt || !al_opt) return std::nullopt;
                    sz = *sz_opt;
                    al = *al_opt;
                }
                if (sz > static_cast<std::uint64_t>(
                             std::numeric_limits<std::int64_t>::max()) ||
                    al > static_cast<std::uint64_t>(
                             std::numeric_limits<std::int64_t>::max())) {
                    error(body.span,
                          "comptime integer overflow in builtin::type_info");
                    return std::nullopt;
                }

                if (!consume_heap(body.span, /*units=*/4)) return std::nullopt;

                ComptimeValue kind{};
                kind.kind = ComptimeValue::Kind::Enum;
                kind.enum_def = typekind_enum_;
                kind.enum_variant = *kind_name;

                ComptimeValue out{};
                out.kind = ComptimeValue::Kind::Struct;
                out.struct_def = typeinfo_struct_;
                out.struct_fields.insert({"kind", std::move(kind)});
                out.struct_fields.insert(
                    {"size",
                     ComptimeValue::int_(static_cast<std::int64_t>(sz))});
                out.struct_fields.insert(
                    {"align",
                     ComptimeValue::int_(static_cast<std::int64_t>(al))});
                return out;
            }

            // ---- Type construction intrinsics (comptime-only) ----
            case MirRvalue::IntrinsicKind::TypeUnit:
                return ComptimeValue::type_(types_.unit());
            case MirRvalue::IntrinsicKind::TypeNever:
                return ComptimeValue::type_(types_.never());

            case MirRvalue::IntrinsicKind::TypePtrConst:
            case MirRvalue::IntrinsicKind::TypePtrMut: {
                if (in.args.size() != 1) return std::nullopt;
                auto tv = eval_arg(0);
                if (!tv) return std::nullopt;
                auto child = as_type(body.span, *tv);
                if (!child) return std::nullopt;
                if (types_.get(*child).kind == TypeKind::TypeType) {
                    error(body.span,
                          "cannot form a pointer to `type` (runtime-erased)");
                    return std::nullopt;
                }
                Mutability mut = in.kind == MirRvalue::IntrinsicKind::TypePtrMut
                                     ? Mutability::Mut
                                     : Mutability::Const;
                std::string key = "type_ptr:" +
                                  std::string(mut == Mutability::Mut ? "mut:" : "const:") +
                                  type_key(*child);
                if (auto hit = cache_lookup(key))
                    return ComptimeValue::type_(*hit);
                TypeId out = types_.ptr(mut, *child);
                cache_store(std::move(key), out);
                return ComptimeValue::type_(out);
            }

            case MirRvalue::IntrinsicKind::TypeSlice: {
                if (in.args.size() != 1) return std::nullopt;
                auto tv = eval_arg(0);
                if (!tv) return std::nullopt;
                auto child = as_type(body.span, *tv);
                if (!child) return std::nullopt;
                const TypeData& cd = types_.get(*child);
                if (cd.kind == TypeKind::TypeType) {
                    error(body.span, "cannot form a slice of `type`");
                    return std::nullopt;
                }
                if (!types_.is_sized(*child)) {
                    error(body.span, "slice element type must be sized");
                    return std::nullopt;
                }
                std::string key = "type_slice:" + type_key(*child);
                if (auto hit = cache_lookup(key))
                    return ComptimeValue::type_(*hit);
                TypeId out = types_.slice(*child);
                cache_store(std::move(key), out);
                return ComptimeValue::type_(out);
            }

            case MirRvalue::IntrinsicKind::TypeArray: {
                if (in.args.size() != 2) return std::nullopt;
                auto tv = eval_arg(0);
                auto nv = eval_arg(1);
                if (!tv || !nv) return std::nullopt;
                auto child = as_type(body.span, *tv);
                if (!child) return std::nullopt;
                if (types_.get(*child).kind == TypeKind::TypeType) {
                    error(body.span, "cannot form an array of `type`");
                    return std::nullopt;
                }
                if (!types_.is_sized(*child)) {
                    error(body.span, "array element type must be sized");
                    return std::nullopt;
                }
                auto n_i = as_int(body.span, *nv);
                if (!n_i) return std::nullopt;
                if (*n_i < 0) {
                    error(body.span, "array length must be non-negative");
                    return std::nullopt;
                }
                std::uint64_t n = static_cast<std::uint64_t>(*n_i);
                std::string key = "type_array:" + type_key(*child) + ":" +
                                  std::to_string(n);
                if (auto hit = cache_lookup(key))
                    return ComptimeValue::type_(*hit);
                TypeId out = types_.array_const(*child, n);
                cache_store(std::move(key), out);
                return ComptimeValue::type_(out);
            }

            case MirRvalue::IntrinsicKind::TypeTuple: {
                if (in.args.size() != 1) return std::nullopt;
                auto av = eval_arg(0);
                if (!av) return std::nullopt;
                const auto* elems = as_array_elems(body.span, *av);
                if (!elems) return std::nullopt;
                std::vector<TypeId> tys{};
                tys.reserve(elems->size());
                for (const ComptimeValue& tv : *elems) {
                    if (tv.kind != ComptimeValue::Kind::Type) {
                        error(body.span,
                              "builtin::type_tuple expects an array of `type` values");
                        return std::nullopt;
                    }
                    if (types_.get(tv.type_value).kind == TypeKind::TypeType) {
                        error(body.span, "tuple elements must be runtime types");
                        return std::nullopt;
                    }
                    if (!types_.is_sized(tv.type_value)) {
                        error(body.span,
                              "tuple element types must be sized (v0.1)");
                        return std::nullopt;
                    }
                    tys.push_back(tv.type_value);
                }
                std::ostringstream key_out;
                key_out << "type_tuple:";
                for (size_t i = 0; i < tys.size(); i++) {
                    if (i) key_out << ",";
                    key_out << type_key(tys[i]);
                }
                std::string key = key_out.str();
                if (auto hit = cache_lookup(key))
                    return ComptimeValue::type_(*hit);
                TypeId out = types_.tuple(std::move(tys));
                cache_store(std::move(key), out);
                return ComptimeValue::type_(out);
            }

            case MirRvalue::IntrinsicKind::TypeFn: {
                if (in.args.size() != 2) return std::nullopt;
                auto params_v = eval_arg(0);
                auto ret_v = eval_arg(1);
                if (!params_v || !ret_v) return std::nullopt;
                const auto* params = as_array_elems(body.span, *params_v);
                if (!params) return std::nullopt;
                auto ret = as_type(body.span, *ret_v);
                if (!ret) return std::nullopt;
                if (types_.get(*ret).kind == TypeKind::TypeType) {
                    error(body.span, "function return type must be a runtime type");
                    return std::nullopt;
                }
                if (!types_.is_sized(*ret)) {
                    error(body.span, "function return type must be sized in v0.1");
                    return std::nullopt;
                }
                std::vector<TypeId> pts{};
                pts.reserve(params->size());
                for (const ComptimeValue& tv : *params) {
                    if (tv.kind != ComptimeValue::Kind::Type) {
                        error(body.span,
                              "builtin::type_fn expects an array of `type` values for params");
                        return std::nullopt;
                    }
                    if (types_.get(tv.type_value).kind == TypeKind::TypeType) {
                        error(body.span, "function parameter types must be runtime types");
                        return std::nullopt;
                    }
                    if (!types_.is_sized(tv.type_value)) {
                        error(body.span,
                              "function parameter types must be sized in v0.1");
                        return std::nullopt;
                    }
                    pts.push_back(tv.type_value);
                }
                std::ostringstream key_out;
                key_out << "type_fn(";
                for (size_t i = 0; i < pts.size(); i++) {
                    if (i) key_out << ",";
                    key_out << type_key(pts[i]);
                }
                key_out << ")->" << type_key(*ret);
                std::string key = key_out.str();
                if (auto hit = cache_lookup(key))
                    return ComptimeValue::type_(*hit);
                TypeId out = types_.fn(std::move(pts), *ret);
                cache_store(std::move(key), out);
                return ComptimeValue::type_(out);
            }

            case MirRvalue::IntrinsicKind::TypeStruct: {
                if (in.args.size() != 1) return std::nullopt;
                if (!type_ctx_.arena || !type_ctx_.struct_info || !type_ctx_.cache) {
                    error(body.span,
                          "internal error: missing type construction context");
                    return std::nullopt;
                }
                auto dv = eval_arg(0);
                if (!dv) return std::nullopt;
                if (dv->kind != ComptimeValue::Kind::Struct || !dv->struct_def) {
                    error(body.span, "builtin::type_struct expects a StructDesc value");
                    return std::nullopt;
                }

                std::unordered_set<std::uint64_t> visiting{};
                std::string key = "type_struct:" + value_key(*dv, visiting);
                if (auto hit = cache_lookup(key))
                    return ComptimeValue::type_(*hit);

                const ComptimeValue& desc = *dv;
                auto it_name = desc.struct_fields.find("name");
                auto it_repr = desc.struct_fields.find("repr");
                auto it_fields = desc.struct_fields.find("fields");
                if (it_name == desc.struct_fields.end() ||
                    it_repr == desc.struct_fields.end() ||
                    it_fields == desc.struct_fields.end()) {
                    error(body.span, "malformed StructDesc value");
                    return std::nullopt;
                }
                if (it_name->second.kind != ComptimeValue::Kind::String) {
                    error(body.span, "StructDesc.name must be a string");
                    return std::nullopt;
                }
                if (it_repr->second.kind != ComptimeValue::Kind::Enum) {
                    error(body.span, "StructDesc.repr must be an enum value");
                    return std::nullopt;
                }

                std::vector<Attr*> attrs{};
                if (it_repr->second.enum_variant == "Packed" ||
                    it_repr->second.enum_variant == "C") {
                    auto* repr_id =
                        type_ctx_.arena->make<Ident>(body.span, "repr");
                    auto* arg_id =
                        type_ctx_.arena->make<Ident>(
                            body.span,
                            it_repr->second.enum_variant == "Packed" ? "packed"
                                                                     : "C");
                    auto* repr_path = type_ctx_.arena->make<Path>(
                        body.span, std::vector<Ident*>{repr_id});
                    auto* arg_path = type_ctx_.arena->make<Path>(
                        body.span, std::vector<Ident*>{arg_id});
                    attrs.push_back(type_ctx_.arena->make<Attr>(body.span, repr_path,
                                                               arg_path));
                }

                std::vector<FieldDecl*> field_asts{};
                StructInfo si{};
                const auto* field_vals = as_array_elems(body.span, it_fields->second);
                if (!field_vals) return std::nullopt;
                for (const ComptimeValue& fv : *field_vals) {
                    if (fv.kind != ComptimeValue::Kind::Struct || !fv.struct_def) {
                        error(body.span,
                              "StructDesc.fields must contain StructField values");
                        return std::nullopt;
                    }
                    auto fn_it = fv.struct_fields.find("name");
                    auto ft_it = fv.struct_fields.find("ty");
                    auto fv_it = fv.struct_fields.find("vis");
                    if (fn_it == fv.struct_fields.end() ||
                        ft_it == fv.struct_fields.end() ||
                        fv_it == fv.struct_fields.end()) {
                        error(body.span, "malformed StructField value");
                        return std::nullopt;
                    }
                    if (fn_it->second.kind != ComptimeValue::Kind::String) {
                        error(body.span, "StructField.name must be a string");
                        return std::nullopt;
                    }
                    if (ft_it->second.kind != ComptimeValue::Kind::Type) {
                        error(body.span, "StructField.ty must be a `type` value");
                        return std::nullopt;
                    }
                    if (types_.get(ft_it->second.type_value).kind == TypeKind::TypeType) {
                        error(body.span, "StructField.ty must be a runtime type");
                        return std::nullopt;
                    }
                    if (!types_.is_sized(ft_it->second.type_value)) {
                        error(body.span, "StructField.ty must be sized");
                        return std::nullopt;
                    }
                    if (fv_it->second.kind != ComptimeValue::Kind::Enum) {
                        error(body.span, "StructField.vis must be an enum value");
                        return std::nullopt;
                    }

                    Visibility fvis = Visibility::Private;
                    if (fv_it->second.enum_variant == "Pub") fvis = Visibility::Pub;
                    if (fv_it->second.enum_variant == "PubCrate")
                        fvis = Visibility::PubCrate;

                    std::string fname = fn_it->second.string_value;
                    if (si.fields.contains(fname)) {
                        error(body.span,
                              "duplicate field `" + fname + "` in constructed struct");
                        return std::nullopt;
                    }

                    field_asts.push_back(type_ctx_.arena->make<FieldDecl>(
                        body.span, std::vector<Attr*>{}, fvis, fname,
                        /*type=*/nullptr));
                    si.fields_in_order.push_back(StructInfo::Field{
                        .name = fname,
                        .type = ft_it->second.type_value,
                    });
                    si.fields.insert({fname, ft_it->second.type_value});
                }

                std::string sname = it_name->second.string_value;
                auto* def = type_ctx_.arena->make<ItemStruct>(
                    body.span, std::move(attrs), Visibility::Private, sname,
                    std::move(field_asts));
                type_ctx_.struct_info->insert({def, std::move(si)});

                TypeId out = types_.struct_(def);
                cache_store(std::move(key), out);
                return ComptimeValue::type_(out);
            }

            case MirRvalue::IntrinsicKind::TypeEnum: {
                if (in.args.size() != 1) return std::nullopt;
                if (!type_ctx_.arena || !type_ctx_.enum_info || !type_ctx_.cache) {
                    error(body.span,
                          "internal error: missing type construction context");
                    return std::nullopt;
                }
                auto dv = eval_arg(0);
                if (!dv) return std::nullopt;
                if (dv->kind != ComptimeValue::Kind::Struct || !dv->struct_def) {
                    error(body.span, "builtin::type_enum expects an EnumDesc value");
                    return std::nullopt;
                }

                std::unordered_set<std::uint64_t> visiting{};
                std::string key = "type_enum:" + value_key(*dv, visiting);
                if (auto hit = cache_lookup(key))
                    return ComptimeValue::type_(*hit);

                const ComptimeValue& desc = *dv;
                auto it_name = desc.struct_fields.find("name");
                auto it_variants = desc.struct_fields.find("variants");
                auto it_tag = desc.struct_fields.find("tag_type");
                if (it_name == desc.struct_fields.end() ||
                    it_variants == desc.struct_fields.end() ||
                    it_tag == desc.struct_fields.end()) {
                    error(body.span, "malformed EnumDesc value");
                    return std::nullopt;
                }
                if (it_name->second.kind != ComptimeValue::Kind::String) {
                    error(body.span, "EnumDesc.name must be a string");
                    return std::nullopt;
                }

                std::optional<IntKind> tag_int{};
                std::unordered_map<std::string, std::int64_t> discriminants{};

                if (it_tag->second.kind != ComptimeValue::Kind::Enum ||
                    !it_tag->second.enum_def) {
                    error(body.span, "EnumDesc.tag_type must be MaybeType");
                    return std::nullopt;
                }
                if (it_tag->second.enum_variant == "Some") {
                    if (it_tag->second.enum_payload.size() != 1 ||
                        it_tag->second.enum_payload[0].kind !=
                            ComptimeValue::Kind::Type) {
                        error(body.span,
                              "EnumDesc.tag_type=Some expects a single `type` payload");
                        return std::nullopt;
                    }
                    TypeId tt = it_tag->second.enum_payload[0].type_value;
                    const TypeData& td = types_.get(tt);
                    if (td.kind != TypeKind::Int) {
                        error(body.span, "EnumDesc.tag_type must be an integer type");
                        return std::nullopt;
                    }
                    tag_int = td.int_kind;
                }

                std::string ename = it_name->second.string_value;
                auto* def = type_ctx_.arena->make<ItemEnum>(
                    body.span, std::vector<Attr*>{}, Visibility::Private, ename,
                    std::vector<VariantDecl*>{});

                EnumInfo ei{};
                ei.tag_int = tag_int;

                const auto* vars = as_array_elems(body.span, it_variants->second);
                if (!vars) return std::nullopt;

                const std::uint32_t ptr_bits = 64;
                auto is_signed = [&](IntKind k) -> bool {
                    switch (k) {
                        case IntKind::I8:
                        case IntKind::I16:
                        case IntKind::I32:
                        case IntKind::I64:
                        case IntKind::I128:
                        case IntKind::Isize:
                            return true;
                        case IntKind::U8:
                        case IntKind::U16:
                        case IntKind::U32:
                        case IntKind::U64:
                        case IntKind::U128:
                        case IntKind::Usize:
                            return false;
                    }
                    return true;
                };
                auto int_bits = [&](IntKind k) -> std::uint32_t {
                    switch (k) {
                        case IntKind::I8:
                        case IntKind::U8:
                            return 8;
                        case IntKind::I16:
                        case IntKind::U16:
                            return 16;
                        case IntKind::I32:
                        case IntKind::U32:
                            return 32;
                        case IntKind::I64:
                        case IntKind::U64:
                            return 64;
                        case IntKind::I128:
                        case IntKind::U128:
                            return 128;
                        case IntKind::Isize:
                        case IntKind::Usize:
                            return ptr_bits;
                    }
                    return 64;
                };
                auto fits = [&](std::int64_t v, IntKind k) -> bool {
                    std::uint32_t bits = int_bits(k);
                    if (bits >= 64) {
                        if (!is_signed(k)) return v >= 0;
                        return true;
                    }
                    if (is_signed(k)) {
                        const std::int64_t min = -(std::int64_t(1) << (bits - 1));
                        const std::int64_t max =
                            (std::int64_t(1) << (bits - 1)) - 1;
                        return v >= min && v <= max;
                    }
                    if (v < 0) return false;
                    const std::uint64_t max = (std::uint64_t(1) << bits) - 1;
                    return static_cast<std::uint64_t>(v) <= max;
                };

                std::unordered_map<std::int64_t, std::string> seen_disc{};
                std::int64_t next_disc = 0;

                for (const ComptimeValue& vv : *vars) {
                    if (vv.kind != ComptimeValue::Kind::Struct || !vv.struct_def) {
                        error(body.span,
                              "EnumDesc.variants must contain EnumVariant values");
                        return std::nullopt;
                    }
                    auto vn_it = vv.struct_fields.find("name");
                    auto vp_it = vv.struct_fields.find("payload");
                    auto vd_it = vv.struct_fields.find("discriminant");
                    if (vn_it == vv.struct_fields.end() ||
                        vp_it == vv.struct_fields.end() ||
                        vd_it == vv.struct_fields.end()) {
                        error(body.span, "malformed EnumVariant value");
                        return std::nullopt;
                    }
                    if (vn_it->second.kind != ComptimeValue::Kind::String) {
                        error(body.span, "EnumVariant.name must be a string");
                        return std::nullopt;
                    }
                    std::string vname = vn_it->second.string_value;
                    if (ei.variants.contains(vname)) {
                        error(body.span,
                              "duplicate variant `" + vname + "` in constructed enum");
                        return std::nullopt;
                    }
                    const auto* payload_vals = as_array_elems(body.span, vp_it->second);
                    if (!payload_vals) return std::nullopt;

                    VariantInfo vi{};
                    std::vector<TypeId> payload_tys{};
                    payload_tys.reserve(payload_vals->size());
                    for (const ComptimeValue& tv : *payload_vals) {
                        if (tv.kind != ComptimeValue::Kind::Type) {
                            error(body.span,
                                  "EnumVariant.payload must be an array of `type` values");
                            return std::nullopt;
                        }
                        if (types_.get(tv.type_value).kind == TypeKind::TypeType) {
                            error(body.span,
                                  "enum payload types must be runtime types");
                            return std::nullopt;
                        }
                        if (!types_.is_sized(tv.type_value)) {
                            error(body.span, "enum payload types must be sized");
                            return std::nullopt;
                        }
                        payload_tys.push_back(tv.type_value);
                    }

                    if (tag_int) {
                        if (!payload_tys.empty()) {
                            error(body.span,
                                  "tagged enums (tag_type=Some) must be fieldless");
                            return std::nullopt;
                        }
                        std::int64_t disc = next_disc;
                        if (vd_it->second.kind == ComptimeValue::Kind::Enum &&
                            vd_it->second.enum_variant == "Some") {
                            if (vd_it->second.enum_payload.size() != 1 ||
                                vd_it->second.enum_payload[0].kind !=
                                    ComptimeValue::Kind::Int) {
                                error(body.span,
                                      "EnumVariant.discriminant=Some expects an integer payload");
                                return std::nullopt;
                            }
                            disc = vd_it->second.enum_payload[0].int_value;
                            next_disc = disc;
                        }
                        if (!fits(disc, *tag_int)) {
                            error(body.span,
                                  "enum discriminant does not fit in the tag type");
                            return std::nullopt;
                        }
                        if (auto it = seen_disc.find(disc); it != seen_disc.end()) {
                            error(body.span,
                                  "duplicate enum discriminant value (also used by `" +
                                      it->second + "`)");
                            return std::nullopt;
                        }
                        seen_disc.insert({disc, vname});
                        discriminants.insert({vname, disc});
                        if (next_disc ==
                            std::numeric_limits<std::int64_t>::max()) {
                            error(body.span, "enum discriminant overflow");
                            return std::nullopt;
                        }
                        next_disc++;
                    }

                    std::vector<Type*> payload_syntax(payload_tys.size(), nullptr);
                    auto* vdecl = type_ctx_.arena->make<VariantDecl>(
                        body.span, vname, std::move(payload_syntax),
                        /*discriminant=*/nullptr);

                    vi.ast = vdecl;
                    vi.payload = std::move(payload_tys);

                    def->variants.push_back(vdecl);
                    ei.variants_in_order.push_back(vname);
                    ei.variants.insert({vname, std::move(vi)});
                }

                ei.discriminants = std::move(discriminants);
                type_ctx_.enum_info->insert({def, std::move(ei)});

                TypeId out = types_.enum_(def);
                cache_store(std::move(key), out);
                return ComptimeValue::type_(out);
            }
        }
        error(body.span, "unsupported comptime intrinsic");
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
