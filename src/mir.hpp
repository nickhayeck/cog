#pragma once

#include <cstdint>
#include <iosfwd>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "ast.hpp"
#include "hir.hpp"
#include "span.hpp"
#include "types.hpp"

namespace cog {

using MirBodyId = std::uint32_t;
using MirBlockId = std::uint32_t;
using MirLocalId = std::uint32_t;

struct MirPlace;
struct MirOperand;

struct MirProjection {
    struct Deref {};
    struct Field {
        std::uint32_t index = 0;
    };
    struct Index {
        MirLocalId index_local = 0;
    };
    struct Downcast {
        std::uint32_t variant_index = 0;
    };

    std::variant<Deref, Field, Index, Downcast> data{};
};

struct MirPlace {
    struct Local {
        MirLocalId local = 0;
    };
    struct Static {
        DefId def = 0;
    };

    std::variant<Local, Static> base{};
    std::vector<MirProjection> projection{};

    static MirPlace local(MirLocalId l) { return MirPlace{.base = Local{l}}; }
};

struct MirConst {
    struct Unit {};
    struct Bool {
        bool v = false;
    };
    struct Int {
        std::int64_t v = 0;
    };
    struct Float {
        double v = 0.0;
    };
    struct Type {
        TypeId ty = 0;
    };
    struct String {
        std::string bytes{};
        bool is_c_string = false;
    };

    std::variant<Unit, Bool, Int, Float, Type, String> data{};
};

struct MirOperand {
    struct Copy {
        MirPlace place{};
    };
    struct Move {
        MirPlace place{};
    };
    struct Const {
        TypeId ty = 0;
        MirConst value{};
    };
    struct Fn {
        DefId def = 0;
    };
    struct ConstItem {
        DefId def = 0;
    };

    std::variant<Copy, Move, Const, Fn, ConstItem> data{};
};

struct MirRvalue {
    struct Use {
        MirOperand op{};
    };
    struct Unary {
        UnaryOp op{};
        MirOperand operand{};
    };
    struct Binary {
        BinaryOp op{};
        MirOperand lhs{};
        MirOperand rhs{};
    };
    struct Cast {
        MirOperand operand{};
        TypeId to = 0;
    };
    struct AddrOf {
        Mutability mutability{};
        MirPlace place{};
    };
    struct EnumTag {
        MirOperand operand{};
    };

    enum class IntrinsicKind : std::uint8_t {
        // Layout/reflect (compile-time constant)
        SizeOf,
        AlignOf,
        TypeInfo,

        // Type construction (comptime-only)
        TypeUnit,
        TypeNever,
        TypePtrConst,
        TypePtrMut,
        TypeSlice,
        TypeArray,
        TypeTuple,
        TypeFn,
        TypeStruct,
        TypeEnum,
    };

    struct Intrinsic {
        IntrinsicKind kind{};
        std::vector<MirOperand> args{};
    };

    enum class AggregateKind : std::uint8_t {
        Tuple,
        Array,
        Struct,
        EnumVariant,
    };
    struct Aggregate {
        AggregateKind kind{};
        TypeId ty = 0;
        std::uint32_t variant_index = 0;  // for EnumVariant
        std::vector<MirOperand> elems{};
    };

    std::variant<Use, Unary, Binary, Cast, AddrOf, EnumTag, Intrinsic, Aggregate>
        data{};
};

struct MirStatement {
    struct Assign {
        MirPlace dst{};
        MirRvalue src{};
    };

    struct Assert {
        MirOperand cond{};
        std::string message{};
        Span span{};
    };

    std::variant<Assign, Assert> data{};
};

struct MirTerminator {
    struct Return {};
    struct Goto {
        MirBlockId target = 0;
    };
    struct SwitchInt {
        MirOperand scrut{};
        std::vector<std::pair<std::uint64_t, MirBlockId>> cases{};
        MirBlockId otherwise = 0;
    };
    struct Call {
        MirOperand callee{};
        std::vector<MirOperand> args{};
        std::optional<MirPlace> ret{};
        MirBlockId next = 0;
    };
    struct Unreachable {};

    std::variant<Return, Goto, SwitchInt, Call, Unreachable> data{};
};

struct MirBlock {
    std::vector<MirStatement> stmts{};
    MirTerminator term{MirTerminator::Unreachable{}};
};

struct MirLocal {
    TypeId ty = 0;
    std::string name{};
};

struct MirBody {
    MirBodyId id = 0;
    DefId owner = 0;  // function def this body belongs to
    Span span{};

    // The return place is always local#0.
    TypeId ret_ty = 0;
    std::vector<MirLocal> locals{};

    MirBlockId start = 0;
    std::vector<MirBlock> blocks{};

    // Parallel to function signature params (excluding the return local).
    std::vector<bool> comptime_params{};
};

struct MirProgram {
    const HirCrate* hir = nullptr;
    const TypeStore* types = nullptr;

    std::vector<MirBody> bodies{};
    std::unordered_map<DefId, MirBodyId> fn_bodies{};
    std::unordered_map<DefId, MirBodyId> const_bodies{};
    std::unordered_map<DefId, MirBodyId> static_bodies{};

    const MirBody* body_for_fn(DefId def) const;
    const MirBody* body_for_const(DefId def) const;
    const MirBody* body_for_static(DefId def) const;
};

void dump_mir(std::ostream& os, const MirProgram& program);
void dump_mir_body(std::ostream& os, const MirProgram& program,
                   const MirBody& body);

}  // namespace cog
