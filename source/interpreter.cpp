#include "interpreter.h"

#include "builtin.h"
#include "os.h"
#include "string_builder.h"
#include "temp_allocator.h"

#include <stdio.h>

#include <tracy/Tracy.hpp>

namespace Zodiac
{
    char callback_handler(DCCallback *cb, DCArgs *args, DCValue *result, void *_userdata)
    {
        ZoneScopedN("callback_handler");
        assert(cb);

        assert(_userdata);
        auto userdata = (uint8_t *)_userdata;

        auto bc_func = (BC_Function *)(userdata - offsetof(BC_Function, ffi_data));
        assert(bc_func);

        ZoneText(bc_func->name.data, bc_func->name.length);

        auto interp = (Interpreter *)bc_func->ffi_data.interpreter;
        assert(interp);

        auto func_type = bc_func->type;

        auto first_temp_index = stack_count(&interp->temp_stack);

        for (int64_t i = 0; i < func_type->function.param_types.count; i++) {
            auto param_type = func_type->function.param_types[i];

            Interpreter_Value arg_val;

            switch (param_type->kind) {

                case AST_Type_Kind::INVALID: assert(false);
                case AST_Type_Kind::VOID: assert(false);

                case AST_Type_Kind::INTEGER: {

                    AST_Type *t = nullptr;
                    Integer_Literal l = {};

                    if (param_type->integer.sign) {
                        switch (param_type->bit_size) {
                            default: assert(false);
                            case 8:  t = Builtin::type_s8;  l.s8 = dcbArgChar(args); break;
                            case 16: t = Builtin::type_s16; l.s16 = dcbArgShort(args); break;
                            case 32: t = Builtin::type_s32; l.s32 = dcbArgInt(args); break;
                            case 64: t = Builtin::type_s64; l.s64 = dcbArgLongLong(args); break;
                        }
                    } else {
                        switch (param_type->bit_size) {
                            default: assert(false);
                            case 8:  t = Builtin::type_s8;  l.u8 = dcbArgUChar(args); break;
                            case 16: t = Builtin::type_s16; l.u16 = dcbArgUShort(args); break;
                            case 32: t = Builtin::type_s32; l.u32 = dcbArgUInt(args); break;
                            case 64: t = Builtin::type_s64; l.u64 = dcbArgULongLong(args); break;
                        }
                    }
                    assert(t);
                    arg_val = { .type = t, .integer_literal = l };
                    break;
                }

                case AST_Type_Kind::FLOAT: {

                    AST_Type *t = nullptr;
                    Float_Literal fl = {};

                    switch (param_type->bit_size) {
                        default: assert(false);
                        case 32: t = Builtin::type_float; fl.r32 = dcbArgFloat(args); break;
                        case 64: t = Builtin::type_double; fl.r64 = dcbArgDouble(args); break;
                    }

                    assert(t);
                    arg_val = { .type = t, .float_literal = fl };
                    break;
                }

                case AST_Type_Kind::BOOL: {
                    arg_val = { .type = Builtin::type_bool,
                                .boolean_literal = static_cast<bool>(dcbArgBool(args)) };
                    break;
                }

                case AST_Type_Kind::POINTER: {
                    auto arg = dcbArgPointer(args);
                    arg_val = { .type = param_type, .pointer = arg };
                    break;
                }

                case AST_Type_Kind::FUNCTION: assert(false);
                case AST_Type_Kind::STRUCTURE: assert(false);
                case AST_Type_Kind::UNION: assert(false);
                case AST_Type_Kind::ENUM: assert(false);
                case AST_Type_Kind::ARRAY: assert(false);
            }

            stack_push(&interp->arg_stack, arg_val);
        }

        int64_t return_value_index = -1;
        auto return_type = bc_func->type->function.return_type;

        if (return_type->kind != AST_Type_Kind::VOID) {

            assert(return_type->kind == AST_Type_Kind::INTEGER ||
                   return_type->kind == AST_Type_Kind::FLOAT ||
                   return_type->kind == AST_Type_Kind::POINTER ||
                   return_type->kind == AST_Type_Kind::BOOL);

            return_value_index = stack_count(&interp->temp_stack);

            Interpreter_Value return_val = { .type = return_type, .integer_literal = {} };
            stack_push(&interp->temp_stack, return_val);
        }

        interpreter_start(interp, bc_func, first_temp_index, return_value_index);

        if (return_value_index != -1) {
            // We have just poppped this of at the end of interpreter_start,
            //  but it should not be possible for this memory to have changed
            //  inbetween.
            Interpreter_Value bc_return_val = interp->temp_stack.buffer[return_value_index];

            switch (return_type->kind) {
                default: assert(false);

                case AST_Type_Kind::INTEGER: {
                    auto il = bc_return_val.integer_literal;
                    if (return_type->integer.sign) {
                        switch (return_type->bit_size) {
                            default: assert(false);
                            case 8: result->c = il.s8; break;
                            case 16: result->s = il.s16; break;
                            case 32: result->i = il.s32; break;
                            case 64: result->l = il.s64; break;
                        }
                    } else {
                        switch (return_type->bit_size) {
                            default: assert(false);
                            case 8: result->C = il.u8; break;
                            case 16: result->S = il.u16; break;
                            case 32: result->I = il.u32; break;
                            case 64: result->L = il.u64; break;
                        }
                    }
                    break;
                }

                case AST_Type_Kind::FLOAT: {
                    auto fl = bc_return_val.float_literal;
                    switch (return_type->bit_size) {
                        default: assert(false);
                        case 32: result->f = fl.r32; break;
                        case 64: result->d = fl.r64; break;
                    }
                    break;
                }

                case AST_Type_Kind::POINTER: {
                    if (return_type == Builtin::type_ptr_u8) {
                        result->Z = bc_return_val.string_literal;
                    } else {
                        result->p = bc_return_val.pointer;
                    }
                    break;
                }

                case AST_Type_Kind::BOOL: {
                    result->B = bc_return_val.boolean_literal;
                    break;
                }
            }
        }

        assert(interp->running == false);
        assert(interp->aborted == false);
        interp->running = true;

        return ffi_dcb_type_sig_char(bc_func->type->function.return_type);
    }

    Interpreter interpreter_create(Allocator *allocator, Build_Data *build_data)
    {
        Interpreter result = {
            .allocator = allocator,
            .build_data = build_data,
            .running = false,
            .aborted = false,
            .globals_initialized = false,
            .foreigns_initialized = false,
            .exit_code = 0,
        };

        stack_init(allocator, &result.temp_stack);
        stack_init(allocator, &result.arg_stack);
        stack_init(allocator, &result.frames);

        const auto mb = 1024 * 1024;
        auto stack_size = 1 * mb;
        result.alloc_stack = alloc_array<uint8_t>(allocator, stack_size);
        result.alloc_sp = result.alloc_stack;
        result.alloc_stack_end = result.alloc_stack + stack_size;

        result.ffi = ffi_create(allocator, build_data, callback_handler);

        hash_table_init(allocator, &result.callbacks, &hash_table_pointers_equal);

        result.functions = {};
        result.foreign_functions = {};

        return result;
    }

    void interpreter_free(Interpreter *interp)
    {
        stack_free(&interp->temp_stack);
        stack_free(&interp->arg_stack);
        stack_free(&interp->frames);

        free(interp->allocator, interp->alloc_stack);

        if (interp->global_mem) {
            free(interp->allocator, interp->global_mem);
        }

        hash_table_free(&interp->callbacks);

        *interp = {};
    }

    void interpreter_start(Interpreter *interp, BC_Function *entry_func,
                           int64_t first_temp_index /*= 1*/,
                           int64_t return_value_index /*= 0*/)
    {
        assert(interp->functions.count);
        assert(interp->globals_initialized);
        assert(interp->foreigns_initialized);

        auto frame_count_on_entry = stack_count(&interp->frames);

        AST_Type *return_type = entry_func->type->function.return_type;
        bool returns_void = return_type->kind == AST_Type_Kind::VOID;
        assert(return_type->kind == AST_Type_Kind::INTEGER || returns_void);

        if (returns_void) {
            assert(return_value_index == -1);
        }

        Interp_Stack_Frame first_frame = {
            .function = entry_func,
            .ip = {
                .index = 0,
                .block = entry_func->blocks[0],
            },

            .first_temp_index = first_temp_index,
            .result_index = return_value_index,
            .previous_alloc_sp = interp->alloc_sp,
        };

        assert(entry_func->parameters.count <= stack_count(&interp->arg_stack));

        auto entry_arg_count = entry_func->parameters.count;
        if (entry_arg_count) {
            first_frame.arg_count = entry_arg_count;

            auto arg_size = sizeof(Interpreter_Value) * entry_arg_count;
            assert(interp->alloc_sp + arg_size < interp->alloc_stack_end);
            first_frame.args = (Interpreter_Value *)interp->alloc_sp;
            interp->alloc_sp += arg_size;
        }

        for (int64_t i = 0; i < entry_arg_count; i++) {
            auto bc_arg = entry_func->parameters[i];
            assert(bc_arg->type->kind == AST_Type_Kind::POINTER);
            auto arg_type = bc_arg->type->pointer.base;

            Interpreter_Value arg_value = stack_peek(&interp->arg_stack,
                                                     (entry_arg_count - 1) - i);
            assert(arg_value.type == arg_type);

            if (arg_type->kind == AST_Type_Kind::ARRAY ||
                arg_type->kind == AST_Type_Kind::STRUCTURE ||
                arg_type->kind == AST_Type_Kind::UNION) {

                auto old_ptr = arg_value.pointer;
                assert(old_ptr);

                assert(arg_type->bit_size % 8 == 0);
                auto byte_size = arg_type->bit_size / 8;
                assert(interp->alloc_sp + byte_size < interp->alloc_stack_end);
                arg_value.pointer = interp->alloc_sp;
                interp->alloc_sp += byte_size;

                memcpy(arg_value.pointer, old_ptr, byte_size);
            }

            auto index = bc_arg->parameter.index;
            assert(index < first_frame.arg_count);
            first_frame.args[index] = arg_value;
        }

        if (entry_arg_count) stack_pop(&interp->arg_stack, entry_arg_count);

        stack_push(&interp->frames, first_frame);

        assert(entry_func->locals.count == 0);

        for (int64_t i = 0; i < entry_func->temps.count; i++) {
            Interpreter_Value temp { .type = entry_func->temps[i]->type };
            stack_push(&interp->temp_stack, temp);
        }

        interp->running = true;

        while (interp->running) {

            bool advance_ip = true;

            auto frame = stack_top_ptr(&interp->frames);
            auto inst = frame->ip.block->instructions[frame->ip.index];

            switch (inst.op) {
                case NOP: assert(false);

                case ALLOCL: {
                    break;
                }

                case STOREL: {
                    assert(inst.a->kind == BC_Value_Kind::ALLOCL);

                    Interpreter_LValue dest = interp_load_lvalue(interp, inst.a);
                    Interpreter_Value source = interp_load_value(interp, inst.b);

                    assert(dest.type == source.type);

                    if (dest.type->kind == AST_Type_Kind::ARRAY ||
                        dest.type->kind == AST_Type_Kind::STRUCTURE ||
                        dest.type->kind == AST_Type_Kind::UNION) {

                        assert(dest.kind == Interp_LValue_Kind::ALLOCL);
                        assert(dest.index < frame->local_count);
                        auto dest_ptr = frame->locals[dest.index];
                        assert(dest_ptr.pointer);
                        assert(dest.type->bit_size % 8 == 0);
                        auto byte_size = dest.type->bit_size / 8;
                        memcpy(dest_ptr.pointer, source.pointer, byte_size);
                    } else {
                        interp_store(interp, source, dest);
                    }

                    break;
                }

                case STORE_ARG: {
                    assert(inst.a->kind == BC_Value_Kind::PARAM);

                    Interpreter_LValue dest = interp_load_lvalue(interp, inst.a);
                    Interpreter_Value source = interp_load_value(interp, inst.b);

                    assert(dest.type == source.type);
                    interp_store(interp, source, dest);
                    break;
                }

                case STORE_GLOBAL: {
                    Interpreter_LValue dest = interp_load_lvalue(interp, inst.a);
                    Interpreter_Value source = interp_load_value(interp, inst.b);

                    assert(dest.type == source.type);
                    interp_store(interp, source, dest);
                    break;
                }

                case STORE_PTR: {
                    assert(inst.a->kind == BC_Value_Kind::TEMP);
                    Interpreter_Value dest_pointer_val = interp_load_value(interp, inst.a);
                    Interpreter_Value source_val = interp_load_value(interp, inst.b);

                    assert(dest_pointer_val.type->kind == AST_Type_Kind::POINTER);
                    assert(source_val.type == dest_pointer_val.type->pointer.base);

                    interp_store(interp, source_val, dest_pointer_val.pointer,
                                 dest_pointer_val.type);
                    break;
                }

                case LOADL: {
                    Interpreter_LValue source_lval = interp_load_lvalue(interp, inst.a);
                    Interpreter_LValue dest = interp_load_lvalue(interp, inst.result);

                    assert(source_lval.type == dest.type);
                    assert(source_lval.kind == Interp_LValue_Kind::ALLOCL);
                    assert(source_lval.index < frame->local_count);
                    Interpreter_Value value = frame->locals[source_lval.index];

                    assert(dest.type == value.type);
                    interp_store(interp, value, dest);
                    break;
                }

                case LOAD_PARAM: {
                    Interpreter_LValue source_lval = interp_load_lvalue(interp, inst.a);
                    Interpreter_LValue dest = interp_load_lvalue(interp, inst.result);

                    assert(source_lval.type == dest.type);
                    assert(source_lval.kind == Interp_LValue_Kind::PARAM);
                    assert(source_lval.index < frame->arg_count);
                    Interpreter_Value value = frame->args[source_lval.index];

                    assert(dest.type == value.type);
                    interp_store(interp, value, dest);
                    break;
                }

                case LOAD_GLOBAL: {
                    Interpreter_Value value = interp_load_value(interp, inst.a);
                    Interpreter_LValue dest = interp_load_lvalue(interp, inst.result);

                    assert(value.type == dest.type);
                    interp_store(interp, value, dest);
                    break;
                }

                case LOAD_PTR: {
                    assert(inst.a->kind == BC_Value_Kind::TEMP);
                    auto ptr_val = interp_load_value(interp, inst.a);
                    assert(ptr_val.type->kind == AST_Type_Kind::POINTER);

                    auto dest_val = interp_load_lvalue(interp, inst.result);
                    assert(dest_val.type == ptr_val.type->pointer.base);

                    assert(ptr_val.pointer);
                    interp_store(interp, ptr_val.pointer, ptr_val.type, dest_val);
                    break;
                }

#define MAKE_INT_NAME(sign, size) sign##size

#define BINOP_INT_CASE(sign, size, op) \
    case size: {  (r.integer_literal. MAKE_INT_NAME(sign, size))  = \
                   (lhs.integer_literal. MAKE_INT_NAME(sign, size)) op \
                   (rhs.integer_literal. MAKE_INT_NAME(sign, size)); \
               break; }

#define IS_CMP_OP(op) \
    ((#op[0] == '=' && #op[1] == '=') || \
     (#op[0] == '!' && #op[1] == '=') || \
     (#op[0] == '<' && #op[1] != '<') || \
     (#op[0] == '>' && #op[1] != '>'))

#define BINOP_INT_(sign_, op) { \
    Interpreter_Value lhs = interp_load_value(interp, inst.a); \
    Interpreter_Value rhs = interp_load_value(interp, inst.b); \
    Interpreter_LValue dest = interp_load_lvalue(interp, inst.result); \
    assert(lhs.type == rhs.type); \
    assert(!IS_CMP_OP(op)); \
    assert(dest.type == lhs.type); \
    auto type = dest.type; \
    if (type->kind == AST_Type_Kind::ENUM) { \
        type = type->enum_type.base_type; \
        dest.type = type; \
    } \
    assert(type->kind == AST_Type_Kind::INTEGER); \
    if (#sign_[0] == 's') { assert(type->integer.sign); } \
    Interpreter_Value r = { \
        .type = type, \
    }; \
    switch (type->bit_size) { \
        BINOP_INT_CASE(sign_, 8, op) \
        BINOP_INT_CASE(sign_, 16, op) \
        BINOP_INT_CASE(sign_, 32, op) \
        BINOP_INT_CASE(sign_, 64, op) \
        default: assert(false); \
    } \
    interp_store(interp, r, dest); \
    break; \
}

#define BINOP_INT(op) BINOP_INT_(s, op)
#define BINOP_UINT(op) BINOP_INT_(u, op)

#define BINOP_CMP_INT_CASE(sign, size, op) \
    case size: {  (r.boolean_literal)  = \
                   (lhs.integer_literal. MAKE_INT_NAME(sign, size)) op \
                   (rhs.integer_literal. MAKE_INT_NAME(sign, size)); \
               break; }

#define BINOP_CMP_INT_(sign_, op) { \
    Interpreter_Value lhs = interp_load_value(interp, inst.a); \
    Interpreter_Value rhs = interp_load_value(interp, inst.b); \
    Interpreter_LValue dest = interp_load_lvalue(interp, inst.result); \
    assert(lhs.type == rhs.type); \
    assert(IS_CMP_OP(op)); \
    assert(dest.type == Builtin::type_bool); \
    auto type = lhs.type; \
    if (type->kind == AST_Type_Kind::ENUM) { \
        type = type->enum_type.base_type; \
        assert(type->kind == AST_Type_Kind::INTEGER); \
    } else { \
        assert(type->kind == AST_Type_Kind::INTEGER || type->kind == AST_Type_Kind::POINTER); \
    } \
    if (#sign_[0] == 's') { assert(type->integer.sign); } \
    Interpreter_Value r = { \
        .type = dest.type, \
    }; \
    switch (type->bit_size) { \
        BINOP_CMP_INT_CASE(sign_, 8, op) \
        BINOP_CMP_INT_CASE(sign_, 16, op) \
        BINOP_CMP_INT_CASE(sign_, 32, op) \
        BINOP_CMP_INT_CASE(sign_, 64, op) \
        default: assert(false); \
    } \
    interp_store(interp, r, dest); \
    break; \
}

#define BINOP_CMP_INT(op) BINOP_CMP_INT_(s, op)
#define BINOP_CMP_UINT(op) BINOP_CMP_INT_(u, op)

                case LSHIFT: {
                    assert(inst.a->type->kind == AST_Type_Kind::INTEGER);
                    if (inst.a->type->integer.sign) {
                        BINOP_INT(<<);
                    } else {
                        BINOP_UINT(<<);
                    }
                }

                case ADD_S:    BINOP_INT(+);
                case SUB_S:    BINOP_INT(-);
                case REM_S:    BINOP_INT(%);
                case MUL_S:    BINOP_INT(*);
                case DIV_S:    BINOP_INT(/);
                case OR_S:     BINOP_INT(|);
                case AND_S:    BINOP_INT(&);
                case RSHIFT_S: BINOP_INT(>>);

                case ADD_U:    BINOP_UINT(+);
                case SUB_U:    BINOP_UINT(-);
                case REM_U:    BINOP_UINT(%);
                case MUL_U:    BINOP_UINT(*);
                case DIV_U:    BINOP_UINT(/);
                case OR_U:     BINOP_UINT(|);
                case AND_U:    BINOP_UINT(&);
                case RSHIFT_U: BINOP_UINT(>>);

                case EQ_S:   BINOP_CMP_INT(==);
                case NEQ_S:  BINOP_CMP_INT(!=);

                case LT_S:   BINOP_CMP_INT(<);
                case LTEQ_S: BINOP_CMP_INT(<=);
                case GT_S:   BINOP_CMP_INT(>);
                case GTEQ_S: BINOP_CMP_INT(>=);

                case EQ_U:   BINOP_CMP_UINT(==);
                case NEQ_U: {
                    Interpreter_Value lhs = interp_load_value(interp, inst.a);
                    Interpreter_Value rhs = interp_load_value(interp, inst.b);
                    Interpreter_LValue dest = interp_load_lvalue(interp, inst.result);
                    assert(lhs.type == rhs.type);
                    assert(IS_CMP_OP(!=));
                    assert(dest.type == Builtin::type_bool);
                    auto type = lhs.type;
                    if (type->kind == AST_Type_Kind::ENUM) type = type->enum_type.base_type;
                    Interpreter_Value r = {
                        .type = dest.type,
                    };
                    switch (type->bit_size) {
                        BINOP_CMP_INT_CASE(u, 8, !=)
                        BINOP_CMP_INT_CASE(u, 16, !=)
                        BINOP_CMP_INT_CASE(u, 32, !=)
                        BINOP_CMP_INT_CASE(u, 64, !=)
                        default: assert(false);
                    }
                    interp_store(interp, r, dest);
                    break;
                }

                case LT_U:   BINOP_CMP_UINT(<);
                case LTEQ_U: BINOP_CMP_UINT(<=);
                case GT_U:   BINOP_CMP_UINT(>);
                case GTEQ_U: BINOP_CMP_UINT(>=);

#undef MAKE_INT_NAME
#undef BINOP_INT_CASE
#undef BINOP_INT_CASES
#undef BINOP_INT_
#undef BINOP_INT
#undef BINOP_UINT
#undef BINOP_CMP_INT_
#undef BINOP_CMP_INT
#undef BINOP_CMP_UINT

#define BINOP_FLOAT_CASE(op, size) case size: \
    r.float_literal.r##size = lhs.float_literal.r##size op rhs.float_literal.r##size; break;

#define BINOP_FLOAT(op) { \
    Interpreter_Value lhs = interp_load_value(interp, inst.a); \
    Interpreter_Value rhs = interp_load_value(interp, inst.b); \
    Interpreter_LValue dest = interp_load_lvalue(interp, inst.result); \
    assert(!IS_CMP_OP(op)); \
    assert(lhs.type == rhs.type); \
    assert(lhs.type == dest.type); \
    assert(lhs.type->kind == AST_Type_Kind::FLOAT); \
    Interpreter_Value r = { .type = lhs.type }; \
    switch (lhs.type->bit_size) { \
        default: assert(false); \
        BINOP_FLOAT_CASE(op, 32); \
        BINOP_FLOAT_CASE(op, 64); \
    } \
    interp_store(interp, r, dest); \
    break; \
}

#define BINOP_CMP_FLOAT_CASE(op, size) case size: \
    r.boolean_literal = lhs.float_literal.r##size op rhs.float_literal.r##size; break;

#define BINOP_CMP_FLOAT(op) { \
    Interpreter_Value lhs = interp_load_value(interp, inst.a); \
    Interpreter_Value rhs = interp_load_value(interp, inst.b); \
    Interpreter_LValue dest = interp_load_lvalue(interp, inst.result); \
    assert(IS_CMP_OP(op)); \
    assert(lhs.type == rhs.type); \
    assert(lhs.type->kind == AST_Type_Kind::FLOAT); \
    assert(dest.type == Builtin::type_bool); \
    Interpreter_Value r = { .type = dest.type }; \
    switch (lhs.type->bit_size) { \
        default: assert(false); \
        BINOP_CMP_FLOAT_CASE(op, 32) \
        BINOP_CMP_FLOAT_CASE(op, 64) \
    } \
    interp_store(interp, r, dest); \
    break; \
}

                case ADD_F: BINOP_FLOAT(+);
                case SUB_F: BINOP_FLOAT(-);
                case MUL_F: BINOP_FLOAT(*);
                case DIV_F: BINOP_FLOAT(/);

                case EQ_F:   BINOP_CMP_FLOAT(==);
                case NEQ_F:  BINOP_CMP_FLOAT(!=);
                case LT_F:   BINOP_CMP_FLOAT(<);
                case LTEQ_F: BINOP_CMP_FLOAT(<=);
                case GT_F:   BINOP_CMP_FLOAT(>);
                case GTEQ_F: BINOP_CMP_FLOAT(>=);

#undef IS_CMP_OP
#undef BINOP_FLOAT_CASE
#undef BINOP_FLOAT
#undef BINOP_CMP_FLOAT_CASE
#undef BINOP_CMP_FLOAT

                case NEG_LOG: {
                    Interpreter_Value operand = interp_load_value(interp, inst.a);
                    Interpreter_LValue dest_lval = interp_load_lvalue(interp, inst.result);

                    assert(operand.type->kind == AST_Type_Kind::BOOL ||
                           operand.type->kind == AST_Type_Kind::INTEGER ||
                           operand.type->kind == AST_Type_Kind::POINTER);
                    assert(dest_lval.type->kind == AST_Type_Kind::BOOL);

                    bool result_value;

                    if (operand.type->kind == AST_Type_Kind::BOOL ||
                        operand.type->kind == AST_Type_Kind::INTEGER) {
                        // @TODO: @Cleanup: I think we should check all sizes and the sign here?
                        result_value = operand.integer_literal.s64 == 0;
                    } else if (operand.type->kind == AST_Type_Kind::POINTER) {
                        // @Cleanup: this could be merged with the case above?
                        result_value = operand.pointer == nullptr;
                    }

                    AST_Type *source_type = dest_lval.type->pointer_to;
                    assert(source_type);
                    interp_store(interp, &result_value, source_type, dest_lval);

                    break;
                }

                case PUSH_ARG: {
                    Interpreter_Value arg_val = interp_load_value(interp, inst.a);
                    stack_push(&interp->arg_stack, arg_val);
                    break;
                }

                case CALL_PTR: {
                    ZoneScopedN("CALL_PTR");

                    Interpreter_Value ptr_val = interp_load_value(interp, inst.a);
                    ZoneValue((uint64_t)ptr_val.pointer);

                    Interpreter_Value arg_count_val = interp_load_value(interp, inst.b);
                    assert(arg_count_val.type == Builtin::type_s64);
                    auto arg_count = arg_count_val.integer_literal.s64;

                    BC_Function *bc_func = nullptr;
                    bool found = false;
                    {
                        ZoneScopedN("Hash check");
                        found = hash_table_find(&interp->callbacks, ptr_val.pointer, &bc_func);
                    }

                    if (found) {

                        ZoneScopedN("Calling BC callback");

                        auto first_temp_index_ = stack_count(&interp->temp_stack);

                        int64_t return_value_index_ = -1;
                        auto return_type_ = bc_func->type->function.return_type;

                        if (return_type_->kind != AST_Type_Kind::VOID) {
                            assert(return_type_->kind == AST_Type_Kind::INTEGER ||
                                   return_type_->kind == AST_Type_Kind::FLOAT ||
                                   return_type_->kind == AST_Type_Kind::POINTER ||
                                   return_type_->kind == AST_Type_Kind::BOOL);

                            return_value_index_ = stack_count(&interp->temp_stack);
                            Interpreter_Value return_val = { .type = return_type,
                                                             .integer_literal = {} };
                            stack_push(&interp->temp_stack, return_val);
                        }

                        interpreter_start(interp, bc_func, first_temp_index_, return_value_index_);

                        if (return_value_index_ != -1) {
                            assert(inst.result);
                            assert(inst.result->type == return_type_);

                            auto return_val = interp->temp_stack.buffer[return_value_index_];
                            auto rv_lval = interp_load_lvalue(interp, inst.result);

                            interp_store(interp, return_val, rv_lval);
                        }

                        assert(interp->running == false);
                        if (!(interp->aborted)) {
                            interp->running = true;
                        }

                    } else {

                        ZoneScopedN("Calling c callback");

                        assert(ptr_val.type->kind == AST_Type_Kind::POINTER);
                        assert(ptr_val.type->pointer.base->kind == AST_Type_Kind::FUNCTION);
                        auto fn_type = ptr_val.type->pointer.base;

                        interpreter_call_function_pointer(interp, ptr_val.pointer, fn_type,
                                                          arg_count, inst.result);
                    }
                    break;
                }

                case CALL: {
                    auto callee_val = inst.a;
                    BC_Function *callee = nullptr;
                    if (callee_val->kind != BC_Value_Kind::FUNCTION) {
                        assert(callee_val->type->kind == AST_Type_Kind::POINTER &&
                               callee_val->type->pointer.base->kind == AST_Type_Kind::FUNCTION);
                        auto _callee = interp_load_value(interp, callee_val);
                        assert(_callee.type);
                        callee = (BC_Function *)_callee.pointer;
                    } else {
                        callee = callee_val->function;
                    }
                    assert(callee);

                    assert((callee->flags & BC_FUNC_FLAG_EMITTED) ||
                           (callee->flags & BC_FUNC_FLAG_FOREIGN));

                    auto arg_count_val = inst.b;
                    assert(arg_count_val->type == Builtin::type_s64);
                    assert(arg_count_val->kind == BC_Value_Kind::INTEGER_LITERAL);
                    auto arg_count = arg_count_val->integer_literal.s64;

                    if (callee->flags & BC_FUNC_FLAG_FOREIGN) {
                        interpreter_execute_foreign_function(interp, callee, arg_count,
                                                             inst.result);
                        break;
                    }

                    if (callee->flags & BC_FUNC_FLAG_COMPILER_FUNC) {
                        interpreter_execute_compiler_function(interp, callee, arg_count);
                        break;
                    }

                    int64_t result_index = -1;

                    if (inst.result) {
                        Interpreter_LValue result_value = interp_load_lvalue(interp, inst.result);
                        result_index = result_value.index;
                    }

                    assert(stack_count(&interp->temp_stack) > result_index);

                    Interp_Stack_Frame new_frame = {
                        .function = callee,
                        .ip = {
                            .index = 0,
                            .block = callee->blocks[0],
                        },
                        .result_index = result_index,
                        .previous_alloc_sp = interp->alloc_sp,
                    };

                    if (arg_count) {
                        new_frame.arg_count = arg_count;

                        auto arg_size = sizeof(Interpreter_Value) * arg_count;
                        assert(interp->alloc_sp + arg_size < interp->alloc_stack_end);
                        new_frame.args = (Interpreter_Value *)interp->alloc_sp;
                        interp->alloc_sp += arg_size;
                    }

                    for (int64_t i = 0; i < arg_count; i++) {
                        auto bc_arg = callee->parameters[i];
                        assert(bc_arg->type->kind == AST_Type_Kind::POINTER);
                        auto arg_type = bc_arg->type->pointer.base;

                        Interpreter_Value arg_value = stack_peek(&interp->arg_stack,
                                                                 (arg_count - 1) - i);
                        assert(arg_value.type == arg_type);

                        if (arg_type->kind == AST_Type_Kind::ARRAY ||
                            arg_type->kind == AST_Type_Kind::STRUCTURE ||
                            arg_type->kind == AST_Type_Kind::UNION) {

                            auto old_ptr = arg_value.pointer;
                            assert(old_ptr);

                            assert(arg_type->bit_size % 8 == 0);
                            auto byte_size = arg_type->bit_size / 8;
                            assert(interp->alloc_sp + byte_size < interp->alloc_stack_end);
                            arg_value.pointer = interp->alloc_sp;
                            interp->alloc_sp += byte_size;

                            memcpy(arg_value.pointer, old_ptr, byte_size);
                        }

                        auto index = bc_arg->parameter.index;
                        assert(index < new_frame.arg_count);
                        new_frame.args[index] = arg_value;
                    }

                    if (arg_count) stack_pop(&interp->arg_stack, arg_count);

                    new_frame.first_temp_index = stack_count(&interp->temp_stack);

                    for (int64_t i = 0; i < callee->temps.count; i++) {
                        Interpreter_Value new_temp = {
                            .type = callee->temps[i]->type,
                        };
                        stack_push(&interp->temp_stack, new_temp);
                    }

                    auto local_size = sizeof(Interpreter_Value) * callee->locals.count;
                    assert(interp->alloc_sp + local_size < interp->alloc_stack_end);
                    new_frame.locals = (Interpreter_Value *)interp->alloc_sp;
                    new_frame.local_count = callee->locals.count;
                    interp->alloc_sp += local_size;

                    for (int64_t i = 0; i < callee->locals.count; i++) {
                        auto bc_allocl = callee->locals[i];
                        auto allocl_type = bc_allocl->type->pointer.base;

                        Interpreter_Value alloc_value = {
                            .type = allocl_type,
                        };

                        if (allocl_type->kind == AST_Type_Kind::ARRAY ||
                            allocl_type->kind == AST_Type_Kind::STRUCTURE ||
                            allocl_type->kind == AST_Type_Kind::UNION) {

                            assert(allocl_type->bit_size % 8 == 0);
                            auto byte_size = allocl_type->bit_size / 8;
                            assert(interp->alloc_sp + byte_size < interp->alloc_stack_end);
                            alloc_value.pointer = interp->alloc_sp;
                            interp->alloc_sp += byte_size;
                        }

                        auto index = bc_allocl->allocl.index;
                        assert(index < new_frame.local_count);
                        new_frame.locals[index] = alloc_value;
                    }

                    advance_ip = false;
                    frame->ip.index += 1;
                    assert(frame->ip.index <= frame->ip.block->instructions.count);

                    stack_push(&interp->frames, new_frame);
                    frame = stack_peek_ptr(&interp->frames, 1);

                    break;
                }

                case RETURN: {
                    advance_ip = false;

                    assert(inst.a);
                    Interpreter_Value return_value = interp_load_value(interp, inst.a);
                    assert(return_value.type);

                    auto current_frame = stack_pop(&interp->frames);

                    if (frame_count_on_entry == stack_count(&interp->frames)) {
                        interp->running = false;
                    }

                    assert(current_frame.result_index >= 0);
                    assert(stack_count(&interp->temp_stack) > current_frame.result_index);

                    if (current_frame.function->temps.count) {
                        stack_pop(&interp->temp_stack, current_frame.function->temps.count);
                    }

                    interp->alloc_sp = current_frame.previous_alloc_sp;

                    Interpreter_LValue dest = {
                        .kind = Interp_LValue_Kind::TEMP,
                        .type = return_value.type,
                        .index = current_frame.result_index,
                    };

                    interp_store(interp, return_value, dest);
                    break;
                }

                case RETURN_VOID: {
                    advance_ip = false;
                    auto old_frame = stack_pop(&interp->frames);
                    assert(old_frame.result_index == -1);

                    if (frame_count_on_entry == stack_count(&interp->frames)) {
                        interp->running = false;
                    }

                    if (old_frame.function->temps.count) {
                        stack_pop(&interp->temp_stack, old_frame.function->temps.count);
                    }

                    interp->alloc_sp = old_frame.previous_alloc_sp;
                    break;
                }

                case JUMP: {
                    auto block_value = inst.a;
                    assert(block_value->kind == BC_Value_Kind::BLOCK);

                    advance_ip = false;

                    frame->ip = {
                        .index = 0,
                        .block = block_value->block,
                    };
                    break;
                }

                case JUMP_IF: {
                    auto cond_val = interp_load_value(interp, inst.a);
                    assert(cond_val.type->kind == AST_Type_Kind::BOOL);

                    assert(inst.b->kind == BC_Value_Kind::BLOCK);
                    assert(inst.result->kind == BC_Value_Kind::BLOCK);

                    BC_Block *then_block = inst.b->block;
                    BC_Block *else_block = inst.result->block;
                    BC_Block *target_block = nullptr;

                    if (cond_val.boolean_literal) {
                        target_block = then_block;
                    } else {
                        target_block = else_block;
                    }

                    advance_ip = false;

                    frame->ip = {
                        .index = 0,
                        .block = target_block,
                    };
                    break;
                }

                case SWITCH: {
                    advance_ip = false;

                    BC_Block *default_block = nullptr;
                    BC_Block *target_block = nullptr;

                    assert(inst.b->kind == BC_Value_Kind::SWITCH_DATA);
                    auto switch_data = inst.b->switch_data;

                    Interpreter_Value operand_val = interp_load_value(interp, inst.a);

                    for (int64_t i = 0; i < switch_data.cases.count; i++) {
                        auto case_info = switch_data.cases[i];

                        if (case_info.case_value) {
                            assert(inst.a->type == case_info.case_value->type);
                            auto type = inst.a->type;

                            if (type->kind == AST_Type_Kind::ENUM) {
                                type = type->enum_type.base_type;
                            }

                            assert(type->kind == AST_Type_Kind::INTEGER);
                            assert(type->bit_size == 64);

                            bool match;
                            if (type->integer.sign) {
                                match = operand_val.integer_literal.s64 ==
                                        case_info.case_value->integer_literal.s64;
                            } else {
                                match = operand_val.integer_literal.u64 ==
                                        case_info.case_value->integer_literal.u64;
                            }

                            if (match) {
                                target_block = case_info.target_block;
                                break;
                            }

                        } else {
                            assert(!default_block);
                            default_block = case_info.target_block;
                        }
                    }

                    assert(target_block || default_block);
                    auto dest = target_block;
                    if (!dest) dest = default_block;
                    assert(dest);

                    advance_ip = false;

                    frame->ip = {
                        .index = 0,
                        .block = dest,
                    };

                    break;
                }

                case PTR_OFFSET: {
                    auto offset_val = interp_load_value(interp, inst.b);

                    assert(offset_val.type->kind == AST_Type_Kind::INTEGER);

                    auto result_val = interp_load_lvalue(interp, inst.result);

                    void *ptr = nullptr;

                    if (inst.a->kind == BC_Value_Kind::ALLOCL) {
                        Interpreter_LValue pointer_lval = interp_load_lvalue(interp, inst.a);
                        if (pointer_lval.type->kind == AST_Type_Kind::ARRAY) {
                            assert(pointer_lval.index < frame->local_count);
                            Interpreter_Value *pointer_val = &frame->locals[pointer_lval.index];
                            assert(pointer_val->pointer);
                            ptr = pointer_val->pointer;
                        } else {
                            assert(false);
                        }
                    } else if (inst.a->kind == BC_Value_Kind::GLOBAL) {
                        Interpreter_LValue pointer_lval = interp_load_lvalue(interp, inst.a);
                        if (pointer_lval.type->kind == AST_Type_Kind::ARRAY) {
                            auto global_val = &interp->globals[pointer_lval.index];
                            assert(global_val->pointer);
                            ptr = global_val->pointer;
                        } else {
                            assert(false);
                        }
                    } else if (inst.a->kind == BC_Value_Kind::PARAM) {
                        assert(false);
                    } else if (inst.a->kind == BC_Value_Kind::TEMP) {
                        auto pointer_val = interp_load_value(interp, inst.a);
                        assert(pointer_val.type->kind == AST_Type_Kind::POINTER);
                        if (pointer_val.type->pointer.base->kind == AST_Type_Kind::ARRAY) {
#ifndef NDEBUG
                            auto array_type = pointer_val.type->pointer.base;
#endif
                            assert(array_type->array.element_type->pointer_to);
                            assert(result_val.type == array_type->array.element_type->pointer_to);
                        } else {
                            assert(result_val.type == pointer_val.type);
                        }
                        ptr = pointer_val.pointer;
                    } else {
                        assert(false);
                    }

                    AST_Type *element_type = result_val.type->pointer.base;
                    assert(element_type);
                    assert(element_type->bit_size % 8 == 0);
                    auto byte_size = element_type->bit_size / 8;

                    assert(offset_val.type == Builtin::type_s64);
                    void *result_ptr = ((uint8_t*)ptr) +
                                       (offset_val.integer_literal.s64 * byte_size);

                    Interpreter_Value result_ptr_val = {
                        .type = result_val.type,
                        .pointer = result_ptr,
                    };

                    interp_store(interp, result_ptr_val, result_val);
                    break;
                }

                case AGG_OFFSET: {
                    auto index_val = interp_load_value(interp, inst.b);
                    assert(index_val.type->kind == AST_Type_Kind::INTEGER);

                    auto result_val = interp_load_lvalue(interp, inst.result);
                    assert(result_val.type);

                    AST_Type *struct_type = nullptr;

                    void *ptr = nullptr;

                    if (inst.a->kind == BC_Value_Kind::ALLOCL) {
                        Interpreter_LValue pointer_lval = interp_load_lvalue(interp, inst.a);
                        if (pointer_lval.type->kind == AST_Type_Kind::STRUCTURE) {
                            assert(pointer_lval.index < frame->local_count);
                            Interpreter_Value *pointer_val = &frame->locals[pointer_lval.index];
                            assert(pointer_val->pointer);
                            ptr = pointer_val->pointer;
                            struct_type = pointer_lval.type;
                        } else {
                            assert(false);
                        }
                    } else if (inst.a->kind == BC_Value_Kind::PARAM) {
                         Interpreter_LValue pointer_lval = interp_load_lvalue(interp, inst.a);
                        if (pointer_lval.type->kind == AST_Type_Kind::STRUCTURE) {
                            assert(pointer_lval.index < frame->arg_count);
                            Interpreter_Value *pointer_val = &frame->args[pointer_lval.index];
                            assert(pointer_val->pointer);
                            ptr = pointer_val->pointer;
                            struct_type = pointer_lval.type;
                        } else {
                            assert(false);
                        }
                    } else if (inst.a->kind == BC_Value_Kind::TEMP) {
                        auto pointer_val = interp_load_value(interp, inst.a);
                        assert(pointer_val.type->kind == AST_Type_Kind::POINTER);
                        assert(pointer_val.type->pointer.base->kind == AST_Type_Kind::STRUCTURE);
                        ptr = pointer_val.pointer;
                        struct_type = pointer_val.type->pointer.base;
                    } else {
                        assert(false);
                        // ptr = *(void**)_ptr_ptr;
                    }

                    assert(ptr);
                    assert(struct_type);

                    assert(result_val.type->kind == AST_Type_Kind::POINTER);
#ifndef NDEBUG
                    AST_Type *member_type = result_val.type->pointer.base;
#endif

                    assert(index_val.type == Builtin::type_u32);
                    auto index = index_val.integer_literal.u32;

                    assert(struct_type->structure.member_types.count > index);
                    assert(struct_type->structure.member_types[index] == member_type);

                    int64_t offset = 0;
                    for (int64_t i = 0; i < index; i++) {
                        auto mem_type = struct_type->structure.member_types[i];
                        assert(mem_type->bit_size % 8 == 0);
                        auto byte_size = mem_type->bit_size / 8;
                        offset += byte_size;
                    }

                    void *result_ptr = ((uint8_t*)ptr) + offset;

                    Interpreter_Value result_ptr_val = {
                        .type = result_val.type,
                        .pointer = result_ptr,
                    };

                    interp_store(interp, result_ptr_val, result_val);
                    break;
                }

                case ADDROF_FUNC: {
                    assert(inst.a->kind == BC_Value_Kind::FUNCTION);

                    BC_Function *func = inst.a->function;
                    auto dest_lval = interp_load_lvalue(interp, inst.result);

                    if (func->flags & BC_FUNC_FLAG_COMPILER_FUNC) {
                        assert(false);
                    }

                    void *fn_ptr = nullptr;

                    if (func->ffi_data.c_fn_ptr) {
                        fn_ptr = func->ffi_data.c_fn_ptr;
                    } else {
                        // Pointers for foreign functions are loaded when the
                        //  interpreter starts.
                        assert(!(func->flags & BC_FUNC_FLAG_FOREIGN));

                        func->ffi_data.interpreter = interp;
                        fn_ptr = ffi_create_callback(&interp->ffi, &func->ffi_data,
                                                          func->type);
                        assert(func->ffi_data.c_fn_ptr);
                        assert(fn_ptr == func->ffi_data.c_fn_ptr);

                        hash_table_add(&interp->callbacks, fn_ptr, func);
                    }

                    assert(fn_ptr);
                    interp_store(interp, &fn_ptr, func->type->pointer_to, dest_lval, true);

                    break;
                }

                case ZEXT: {
                    Interpreter_Value operand = interp_load_value(interp, inst.a);
                    AST_Type *op_type = operand.type;
                    if (op_type->kind == AST_Type_Kind::ENUM) op_type = op_type->enum_type.base_type;
                    assert(op_type->kind == AST_Type_Kind::INTEGER);
                    assert(op_type->integer.sign == false);

                    Interpreter_LValue dest_lval = interp_load_lvalue(interp, inst.result);
                    assert(dest_lval.type->kind == AST_Type_Kind::INTEGER);
                    assert(dest_lval.type->pointer_to);

                    assert(dest_lval.type->bit_size > operand.type->bit_size);

#define ZEXT_CASE(size) case size: { \
    uint##size##_t new_val = 0; \
    switch (operand.type->bit_size) { \
        default: assert(false); \
        case 8: new_val =  (uint##size##_t)operand.integer_literal.u8; break; \
        case 16: new_val = (uint##size##_t)operand.integer_literal.u16; break; \
        case 32: new_val = (uint##size##_t)operand.integer_literal.u32; break; \
        case 64: new_val = (uint##size##_t)operand.integer_literal.u64; break; \
    } \
    interp_store(interp, &new_val, dest_lval.type->pointer_to, dest_lval); \
    break; \
}
                    switch (dest_lval.type->bit_size) {
                        default: assert(false);
                        ZEXT_CASE(8)
                        ZEXT_CASE(16)
                        ZEXT_CASE(32)
                        ZEXT_CASE(64)
                    }

#undef ZEXT_CASE
                    break;
                }

                case SEXT: {
                    Interpreter_Value operand = interp_load_value(interp, inst.a);
                    Interpreter_LValue dest_lvalue = interp_load_lvalue(interp, inst.result);

                    assert(operand.type->kind == AST_Type_Kind::INTEGER);
                    assert(operand.type->integer.sign);
                    assert(dest_lvalue.type->kind == AST_Type_Kind::INTEGER);
                    assert(dest_lvalue.type->integer.sign);

#define SEXT_CASE(size) case size: { \
    int##size##_t new_val; \
    switch (dest_lvalue.type->bit_size) { \
        default: assert(false); \
        case 8: new_val =  (int##size##_t)operand.integer_literal.s8; break;\
        case 16: new_val = (int##size##_t)operand.integer_literal.s16; break;\
        case 32: new_val = (int##size##_t)operand.integer_literal.s32; break;\
        case 64: new_val = (int##size##_t)operand.integer_literal.s64; break;\
    } \
    auto source_type = dest_lvalue.type->pointer_to; \
    assert(source_type); \
    interp_store(interp, &new_val, source_type, dest_lvalue); \
    break; \
}

                    switch (dest_lvalue.type->bit_size) {
                        default: assert(false);
                        SEXT_CASE(8);
                        SEXT_CASE(16);
                        SEXT_CASE(32);
                        SEXT_CASE(64);
                    }
                    break;

#undef SEXT_CASE
                }

                case TRUNC: {
                    Interpreter_Value operand = interp_load_value(interp, inst.a);
                    Interpreter_LValue dest_lvalue = interp_load_lvalue(interp, inst.result);

                    assert(operand.type->kind == AST_Type_Kind::INTEGER);
                    assert(dest_lvalue.type->kind == AST_Type_Kind::INTEGER);

#define TRUNC_CASE(size) case size: { \
    uint##size##_t new_val; \
    switch (operand.type->bit_size) { \
        default: assert(false); \
        case 8: new_val =  (uint##size##_t)operand.integer_literal.u8; break;\
        case 16: new_val = (uint##size##_t)operand.integer_literal.u16; break;\
        case 32: new_val = (uint##size##_t)operand.integer_literal.u32; break;\
        case 64: new_val = (uint##size##_t)operand.integer_literal.u64; break;\
    } \
    auto source_type = dest_lvalue.type->pointer_to; \
    assert(source_type); \
    interp_store(interp, &new_val, source_type, dest_lvalue); \
    break; \
}

                    switch (dest_lvalue.type->bit_size) {
                        default: assert(false);
                        TRUNC_CASE(8)
                        TRUNC_CASE(16)
                        TRUNC_CASE(32)
                        TRUNC_CASE(64)
                    }

#undef TRUNC_CASE

                    break;
                }

                case F_TO_S: {
                    Interpreter_Value operand = interp_load_value(interp, inst.a);
                    Interpreter_LValue dest_lvalue = interp_load_lvalue(interp, inst.result);

                    assert(operand.type->kind == AST_Type_Kind::FLOAT);
                    assert(dest_lvalue.type->kind == AST_Type_Kind::INTEGER);
                    assert(dest_lvalue.type->integer.sign);

#define F_TO_S_CASE_INT_SIZE(size) case size: { \
    auto new_value = (int##size##_t)v; \
    assert(dest_lvalue.type->pointer_to); \
    interp_store(interp, &new_value, dest_lvalue.type->pointer_to, \
                 dest_lvalue); \
    break; \
}

#define F_TO_S_CASE_FLOAT_SIZE(size) case size: { \
    auto v = operand.float_literal.r##size; \
    switch (dest_lvalue.type->bit_size) { \
        default: assert(false); \
        F_TO_S_CASE_INT_SIZE(8) \
        F_TO_S_CASE_INT_SIZE(16) \
        F_TO_S_CASE_INT_SIZE(32) \
        F_TO_S_CASE_INT_SIZE(64) \
    } \
    break; \
}

                    switch (operand.type->bit_size) {
                        default: assert(false);
                        F_TO_S_CASE_FLOAT_SIZE(32);
                        F_TO_S_CASE_FLOAT_SIZE(64);
                    }


#undef F_TO_S_CASE_INT_SIZE
#undef F_TO_S_CASE_FLOAT_SIZE

                    break;
                }

                case S_TO_F: {
                    Interpreter_Value operand = interp_load_value(interp, inst.a);
                    Interpreter_LValue dest_lvalue = interp_load_lvalue(interp, inst.result);

                    assert(operand.type->kind == AST_Type_Kind::INTEGER);
                    assert(operand.type->integer.sign);
                    assert(dest_lvalue.type->kind == AST_Type_Kind::FLOAT);

#define S_TO_F_CASE(size) case size: { \
    Float_Literal fl; \
    switch (operand.type->bit_size) { \
        default: assert(false); \
        case 8: fl.r##size = (r##size)operand.integer_literal.s8; break; \
        case 16: fl.r##size = (r##size)operand.integer_literal.s16; break; \
        case 32: fl.r##size = (r##size)operand.integer_literal.s32; break; \
        case 64: fl.r##size = (r##size)operand.integer_literal.s64; break; \
    } \
    auto source_type = dest_lvalue.type->pointer_to; \
    assert(source_type); \
    interp_store(interp, &fl.r##size, source_type, dest_lvalue); \
    break; \
}

                    switch (dest_lvalue.type->bit_size) {
                        default: assert(false);
                        S_TO_F_CASE(32);
                        S_TO_F_CASE(64);
                    }

                    break;
                }

                case U_TO_F: assert(false);

                case F_TO_F: {
                    Interpreter_Value operand = interp_load_value(interp, inst.a);
                    Interpreter_LValue dest_lvalue = interp_load_lvalue(interp, inst.result);

                    assert(operand.type->kind == AST_Type_Kind::FLOAT);
                    assert(dest_lvalue.type->kind == AST_Type_Kind::FLOAT);
                    assert(operand.type != dest_lvalue.type);

                    Interpreter_Value result_value = {
                        .type = dest_lvalue.type,
                    };

                    switch (dest_lvalue.type->bit_size) {
                        default: assert(false);

                        case 32:
                             result_value.float_literal.r32 = (r32)operand.float_literal.r64;
                             break;

                        case 64: result_value.float_literal.r64 = operand.float_literal.r32; break;
                    }

                    interp_store(interp, result_value, dest_lvalue);
                    break;
                }

                case PTR_TO_INT: {
                    assert(inst.a->type->kind == AST_Type_Kind::POINTER);
                    assert(inst.result->type->kind == AST_Type_Kind::INTEGER);

                    AST_Type *pointer_type = nullptr;
                    void *ptr;

                    // @TODO: @Cleanup: There should be a general way to get a pointer from an lvalue
                    if (inst.a->kind == BC_Value_Kind::ALLOCL) {
                        Interpreter_LValue allocl_lval = interp_load_lvalue(interp, inst.a);
                        assert(allocl_lval.type);
                        pointer_type = allocl_lval.type->pointer_to;
                        assert(pointer_type);
                        assert(inst.a->allocl.index < frame->local_count);
                        Interpreter_Value *val_ptr = &frame->locals[inst.a->allocl.index];
                        ptr = &val_ptr->pointer;

                    } else if (inst.a->kind == BC_Value_Kind::TEMP) {
                        Interpreter_Value pointer_val = interp_load_value(interp, inst.a);
                        assert(pointer_val.type->kind == AST_Type_Kind::POINTER);
                        pointer_type = pointer_val.type;
                        ptr = pointer_val.pointer;

                    } else if (inst.a->kind == BC_Value_Kind::STRING_LITERAL) {
                        Interpreter_Value pointer_val = interp_load_value(interp, inst.a);
                        assert(pointer_val.type->kind == AST_Type_Kind::POINTER);
                        pointer_type = pointer_val.type;
                        ptr = pointer_val.pointer;

                    } else {
                        assert(false);
                    }
                    assert(pointer_type);

                    Interpreter_LValue dest_lval = interp_load_lvalue(interp, inst.result);
                    assert(dest_lval.type->kind == AST_Type_Kind::INTEGER);

                    interp_store(interp, &ptr, pointer_type, dest_lval, true);
                    break;
                }

                case PTR_TO_PTR: assert(false);

                case SIZEOF: {
                    assert(inst.a->kind == BC_Value_Kind::TYPE);
                    auto dest = interp_load_lvalue(interp, inst.result);

                    assert(dest.type == Builtin::type_s64);
                    assert(inst.a->type->bit_size % 8 == 0);
                    int64_t size = inst.a->type->bit_size / 8;

                    assert(dest.type->pointer_to);
                    interp_store(interp, &size, dest.type->pointer_to, dest);
                    break;
                }

                case OFFSETOF: {
                    assert(inst.a->kind == BC_Value_Kind::TYPE);
                    assert(inst.b->kind == BC_Value_Kind::INTEGER_LITERAL);
                    Interpreter_Value index_val = interp_load_value(interp, inst.b);
                    assert(index_val.type == Builtin::type_s64);

                    Interpreter_LValue dest = interp_load_lvalue(interp, inst.result);

                    int64_t index = index_val.integer_literal.s64;
                    int64_t offset = 0;

                    AST_Type *struct_type = inst.a->type;
                    assert(struct_type->kind == AST_Type_Kind::STRUCTURE);

                    for (int64_t i = 0; i < index; i++) {
                        auto bit_size = struct_type->structure.member_types[i]->bit_size;
                        assert(bit_size % 8 == 0);
                        offset += (bit_size / 8);
                    }

                    interp_store(interp, &offset, dest.type->pointer_to, dest);
                    break;
                }

                case EXIT: assert(false);

                case SYSCALL: {
                    assert(inst.a->kind == BC_Value_Kind::INTEGER_LITERAL);
                    Interpreter_Value arg_count_val = interp_load_value(interp, inst.a);
                    assert(arg_count_val.type == Builtin::type_u64);

                    // @TODO: @CLEANUP: We are asserting the type is u64 but using the s64?
                    auto arg_count = arg_count_val.integer_literal.s64;

                    assert(arg_count >= 1);

                    assert(stack_count(&interp->arg_stack) >= arg_count);

                    Array<int64_t> args = {};
                    array_init(interp->allocator, &args, arg_count);

                    for (int64_t i = 0; i < arg_count; i++) {
                        auto offset = arg_count - i - 1;
                        Interpreter_Value param = stack_peek(&interp->arg_stack, offset);
                        assert(param.type == Builtin::type_s64);
                        array_append(&args, param.integer_literal.s64);
                    }

                    Interpreter_LValue result_lvalue = interp_load_lvalue(interp, inst.result);
                    assert(result_lvalue.type == Builtin::type_s64);

                    int64_t result = os_syscall(args);

                    Interpreter_Value result_source_val = {
                        .type = Builtin::type_s64,
                        .integer_literal = { .s64 = result },
                    };

                    interp_store(interp, result_source_val, result_lvalue);

                    array_free(&args);

                    stack_pop(&interp->arg_stack, arg_count);

                    break;
                }
            }

            if (advance_ip) {
                frame->ip.index += 1;
                assert(frame->ip.index <= frame->ip.block->instructions.count);
            }

            if (stack_count(&interp->frames) < 1) {
                interp->running = false;
            }
        }

        if (!interp->aborted) {
            if (!returns_void) {
                assert(stack_count(&interp->temp_stack));
                auto exit_val = stack_pop(&interp->temp_stack);
                assert(exit_val.type->kind == AST_Type_Kind::INTEGER);
                assert(exit_val.type == Builtin::type_s64);

                interp->exit_code = exit_val.integer_literal.s64;
            }
        } else {
            fprintf(stderr, "Bytecode aborted!\n");
        }
    }

    void interpreter_start(Interpreter *interp, BC_Function *entry_func,
                           Array<BC_Global_Info> global_info, int64_t global_size,
                           Array<BC_Function *> functions,
                           Array<BC_Function *> foreign_functions)
    {
        assert(interp->functions.count == 0 && interp->functions.capacity == 0);
        interp->functions = functions;

        assert(interp->foreign_functions.count == 0 && interp->foreign_functions.capacity == 0);
        interp->foreign_functions = foreign_functions;

        interpreter_initialize_globals(interp, global_info, global_size);
        interpreter_initialize_foreigns(interp, foreign_functions);

        auto return_type = entry_func->type->function.return_type;

        if (return_type->kind != AST_Type_Kind::VOID) {
            Interpreter_Value return_val = { .type = return_type, .integer_literal = {} };
            stack_push(&interp->temp_stack, return_val);
        }

        interpreter_start(interp, entry_func);
    }

    Interpreter_Value interp_load_value(Interpreter *interp, BC_Value *bc_val)
    {
        Interpreter_Value result = {
            .type = bc_val->type,
        };

        switch (bc_val->kind) {
            case BC_Value_Kind::INVALID: assert(false);

            case BC_Value_Kind::INTEGER_LITERAL: {
                result.integer_literal = bc_val->integer_literal;
                break;
            }

            case BC_Value_Kind::FLOAT_LITERAL: {
                result.float_literal = bc_val->float_literal;
                break;
            }

            case BC_Value_Kind::STRING_LITERAL: {
                result.string_literal = bc_val->string_literal.data;
                break;
            }

            case BC_Value_Kind::BOOL_LITERAL: {
                result.boolean_literal = bc_val->bool_literal;
                break;
            }

            case BC_Value_Kind::NULL_LITERAL: {
                result.pointer = nullptr;
                break;
            }

            case BC_Value_Kind::TEMP: {
                auto frame = stack_top_ptr(&interp->frames);
                auto temp_index = frame->first_temp_index + bc_val->temp.index;
                assert(stack_count(&interp->temp_stack) > temp_index);
                result = interp->temp_stack.buffer[temp_index];
                break;
            }

            case BC_Value_Kind::ALLOCL: {
                assert(bc_val->type->kind == AST_Type_Kind::POINTER);

                auto frame = stack_top_ptr(&interp->frames);

                assert(bc_val->allocl.index < frame->local_count);
                Interpreter_Value *value = &frame->locals[bc_val->allocl.index];

                AST_Type *base_type = bc_val->type->pointer.base;

                if (base_type->kind == AST_Type_Kind::ARRAY ||
                    base_type->kind == AST_Type_Kind::STRUCTURE ||
                    base_type->kind == AST_Type_Kind::UNION) {

                    assert(value->pointer);
                    result.pointer = value->pointer;
                } else {
                    result.pointer = &value->integer_literal;
                }
                break;
            }

            case BC_Value_Kind::PARAM: {
                assert(bc_val->type->kind == AST_Type_Kind::POINTER);
                auto frame = stack_top_ptr(&interp->frames);

                assert(bc_val->parameter.index < frame->arg_count);
                Interpreter_Value *value = &frame->args[bc_val->parameter.index];

                AST_Type *base_type = bc_val->type->pointer.base;
                assert(base_type == value->type);

                if (base_type->kind == AST_Type_Kind::ARRAY ||
                    base_type->kind == AST_Type_Kind::STRUCTURE ||
                    base_type->kind == AST_Type_Kind::UNION) {

                    assert(value->pointer);
                    result.pointer = value->pointer;
                } else {
                    result.pointer = &value->integer_literal;
                }
                break;
            }

            case BC_Value_Kind::GLOBAL: {
                assert(interp->globals.count > bc_val->global.index);
                result = interp->globals[bc_val->global.index];
                break;
            }

            case BC_Value_Kind::FUNCTION: {
               assert(result.type->kind == AST_Type_Kind::POINTER &&
                      result.type->pointer.base->kind == AST_Type_Kind::FUNCTION);
                result.pointer = bc_val->function;
                break;
            }

            case BC_Value_Kind::BLOCK: assert(false);
            case BC_Value_Kind::TYPE: assert(false);
            case BC_Value_Kind::SWITCH_DATA: assert(false);
        }

        return result;
    }

    Interpreter_LValue interp_load_lvalue(Interpreter *interp, BC_Value *bc_val)
    {
        Interpreter_LValue result = {};

        switch (bc_val->kind) {
            case BC_Value_Kind::INVALID: assert(false);
            case BC_Value_Kind::INTEGER_LITERAL: assert(false);
            case BC_Value_Kind::FLOAT_LITERAL: assert(false);
            case BC_Value_Kind::STRING_LITERAL: assert(false);
            case BC_Value_Kind::BOOL_LITERAL: assert(false);
            case BC_Value_Kind::NULL_LITERAL: assert(false);

            case BC_Value_Kind::TEMP: {
                auto frame = stack_top_ptr(&interp->frames);
                auto index = frame->first_temp_index + bc_val->temp.index;
                assert(stack_count(&interp->temp_stack) > index);

                result.kind = Interp_LValue_Kind::TEMP;
                result.index = index;
                result.type = bc_val->type;
                break;
            }

            case BC_Value_Kind::ALLOCL: {
                assert(bc_val->type->kind == AST_Type_Kind::POINTER);

                auto index = bc_val->allocl.index;

#ifndef NDEBUG
                auto frame = stack_top_ptr(&interp->frames);
#endif
                assert(index < frame->local_count);

                result.kind = Interp_LValue_Kind::ALLOCL;
                result.index = index;
                result.type = bc_val->type->pointer.base;
                break;
            }

            case BC_Value_Kind::PARAM: {
                assert(bc_val->type->kind == AST_Type_Kind::POINTER);

                auto index = bc_val->parameter.index;
#ifndef NDEBUG
                auto frame = stack_top_ptr(&interp->frames);
#endif
                assert(index < frame->arg_count);


                result.kind = Interp_LValue_Kind::PARAM;
                result.index = index;
                result.type = bc_val->type->pointer.base;
                break;
            }

            case BC_Value_Kind::GLOBAL: {
                assert(bc_val->type->kind == AST_Type_Kind::POINTER);
                assert(interp->globals.count > bc_val->global.index);

                result.kind = Interp_LValue_Kind::GLOBAL;
                result.index = bc_val->global.index;
                result.type = bc_val->type->pointer.base;
                break;
            }

            case BC_Value_Kind::FUNCTION: assert(false);
            case BC_Value_Kind::BLOCK: assert(false);
            case BC_Value_Kind::TYPE: assert(false);
            case BC_Value_Kind::SWITCH_DATA: assert(false);
        }

        return result;
    }

    void interp_store(Interpreter *interp, Interpreter_Value source, Interpreter_LValue dest)
    {
        assert(source.type);
        assert(dest.type);
        assert(source.type == dest.type);

        auto type = source.type;

        Interpreter_Value *dest_ptr = nullptr;

        switch (dest.kind) {
            case Interp_LValue_Kind::INVALID: assert(false);

            case Interp_LValue_Kind::TEMP: {
                assert(stack_count(&interp->temp_stack) > dest.index);
                dest_ptr = &interp->temp_stack.buffer[dest.index];
                break;
            }

            case Interp_LValue_Kind::ALLOCL: {
                auto frame = stack_top_ptr(&interp->frames);
                assert(dest.index < frame->local_count);
                dest_ptr = &frame->locals[dest.index];
                break;
            }

            case Interp_LValue_Kind::PARAM: {
                auto frame = stack_top_ptr(&interp->frames);
                assert(dest.index < frame->arg_count);
                dest_ptr = &frame->args[dest.index];
                break;
            }

            case Interp_LValue_Kind::GLOBAL: {
                assert(interp->globals.count > dest.index);
                dest_ptr = &interp->globals[dest.index];
                break;
            }
        }

        assert(dest_ptr);
        if (dest_ptr->type->kind == AST_Type_Kind::ENUM &&
            source.type->kind != AST_Type_Kind::ENUM) {
            assert(dest_ptr->type->enum_type.base_type == source.type);
        } else {
            assert(dest_ptr->type == source.type);
        }

        switch (type->kind) {
            case AST_Type_Kind::INVALID: assert(false);
            case AST_Type_Kind::VOID: assert(false);

            case AST_Type_Kind::INTEGER: {
                dest_ptr->integer_literal = source.integer_literal;
                break;
            }

            case AST_Type_Kind::FLOAT: {
                dest_ptr->float_literal = source.float_literal;
                break;
            }

            case AST_Type_Kind::BOOL: {
                dest_ptr->boolean_literal = source.boolean_literal;
                break;
            }

            case AST_Type_Kind::POINTER: {
                dest_ptr->pointer = source.pointer;
                break;
            }

            case AST_Type_Kind::FUNCTION: assert(false);

            case AST_Type_Kind::STRUCTURE: {
                dest_ptr->pointer = source.pointer;
                break;
            }

            case AST_Type_Kind::UNION: assert(false);

            case AST_Type_Kind::ENUM: {
                assert(type->enum_type.base_type->kind == AST_Type_Kind::INTEGER);
                dest_ptr->integer_literal = source.integer_literal;
                break;
            }

            case AST_Type_Kind::ARRAY: assert(false);
        }
    }

    void interp_store(Interpreter *interp, Interpreter_Value source, void *dest_ptr,
                      AST_Type *dest_type)
    {
        assert(interp);
        assert(dest_ptr);
        assert(dest_type->kind == AST_Type_Kind::POINTER);
        assert(dest_type->pointer.base == source.type);

        switch (source.type->kind) {
            case AST_Type_Kind::INVALID: assert(false);
            case AST_Type_Kind::VOID: assert(false);

            case AST_Type_Kind::INTEGER: {
                switch (source.type->bit_size) {
                    default: assert(false);
                    case 8: *(uint8_t*)dest_ptr = source.integer_literal.u8; break;
                    case 16: *(uint16_t*)dest_ptr = source.integer_literal.u16; break;
                    case 32: *(uint32_t*)dest_ptr = source.integer_literal.u32; break;
                    case 64: *(uint64_t*)dest_ptr = source.integer_literal.u64; break;
                }
                break;
            }

            case AST_Type_Kind::FLOAT: {
                switch (source.type->bit_size) {
                    default: assert(false);
                    case 32: *(r32*)dest_ptr = source.float_literal.r32; break;
                    case 64: *(r64*)dest_ptr = source.float_literal.r64; break;
                }
                break;
            }

            case AST_Type_Kind::BOOL: assert(false);

            case AST_Type_Kind::POINTER: {
                *(void**)dest_ptr = source.pointer;
                break;
            }

            case AST_Type_Kind::FUNCTION: assert(false);

            case AST_Type_Kind::STRUCTURE: {
                assert(source.type->bit_size % 8 == 0);
                auto byte_size = source.type->bit_size / 8;
                assert(source.pointer);
                memcpy(dest_ptr, source.pointer, byte_size);
                break;
            }

            case AST_Type_Kind::UNION: assert(false);
            case AST_Type_Kind::ENUM: assert(false);
            case AST_Type_Kind::ARRAY: assert(false);
        }
    }

    void interp_store(Interpreter *interp, void *source_ptr, AST_Type *source_type,
                      Interpreter_LValue dest, bool allow_type_mismatch /*= false*/)
    {
        assert(source_type->kind == AST_Type_Kind::POINTER);

        if (!allow_type_mismatch) {
            assert(dest.type == source_type->pointer.base);
        }

        Interpreter_Value *dest_ptr = nullptr;

        switch (dest.kind) {
            case Interp_LValue_Kind::INVALID: assert(false);

            case Interp_LValue_Kind::TEMP: {
                dest_ptr = &interp->temp_stack.buffer[dest.index];
                assert(stack_count(&interp->temp_stack) > dest.index);
                break;
            }

            case Interp_LValue_Kind::ALLOCL: {
                auto frame = stack_top_ptr(&interp->frames);
                assert(dest.index < frame->local_count);
                dest_ptr = &frame->locals[dest.index];
                break;
            }

            case Interp_LValue_Kind::PARAM: {
                auto frame = stack_top_ptr(&interp->frames);
                assert(dest.index < frame->arg_count);
                dest_ptr = &frame->args[dest.index];
                break;
            }

            case Interp_LValue_Kind::GLOBAL: {
                assert(false);
                break;
            }
        }

        assert(dest_ptr);
        if (!allow_type_mismatch) {
            assert(dest_ptr->type == source_type->pointer.base);
        }

        switch(dest.type->kind) {
            case AST_Type_Kind::INVALID: assert(false);
            case AST_Type_Kind::VOID: assert(false);

            case AST_Type_Kind::INTEGER: {
                switch (dest.type->bit_size) {
                    default: assert(false);
                    case 8: dest_ptr->integer_literal.u8 = *((uint8_t*)source_ptr); break;
                    case 16: dest_ptr->integer_literal.u16 = *((uint16_t*)source_ptr); break;
                    case 32: dest_ptr->integer_literal.u32 = *((uint32_t*)source_ptr); break;
                    case 64: dest_ptr->integer_literal.u64 = *((uint64_t*)source_ptr); break;
                }
                break;
            }

            case AST_Type_Kind::FLOAT: {
                if (dest.type == Builtin::type_float) {
                    dest_ptr->float_literal.r32 = *((float*)source_ptr); break;
                } else if (dest.type == Builtin::type_double) {
                    dest_ptr->float_literal.r64 = *((double*)source_ptr); break;
                } else {
                    assert(false);
                }
                break;
            }

            case AST_Type_Kind::BOOL: {
                dest_ptr->boolean_literal = *(bool*)source_ptr;
                break;
            }

            case AST_Type_Kind::POINTER:
            {
                dest_ptr->pointer = *(void**)source_ptr;
                break;
            }

            case AST_Type_Kind::FUNCTION: assert(false);
            case AST_Type_Kind::STRUCTURE: assert(false);
            case AST_Type_Kind::UNION: assert(false);
            case AST_Type_Kind::ENUM: assert(false);
            case AST_Type_Kind::ARRAY: assert(false);
        }
    }

    void interpreter_initialize_globals(Interpreter *interp, Array<BC_Global_Info> global_info,
                                        int64_t global_data_size)
    {
        if (global_info.count < 1) {
            interp->globals_initialized = true;
            return;
        }

        array_init(interp->allocator, &interp->globals, global_info.count);

        assert(interp->global_mem == nullptr);
        uint8_t *gm_cursor = nullptr;
        if (global_data_size > 0) {
            interp->global_mem = alloc_array<uint8_t>(interp->allocator, global_data_size);
            gm_cursor = interp->global_mem;
        }

        for (int64_t i = 0; i < global_info.count; i++) {
            auto info = global_info[i];
            auto type = info.declaration->type;
            Interpreter_Value global_value = { };
            global_value.type = type;

            array_append(&interp->globals, global_value);

            Interpreter_LValue dest_lval = {
                .kind = Interp_LValue_Kind::GLOBAL,
                .type = type,
                .index = i,
            };

            if (info.has_initializer) {
                interp_store_constant(interp, info.init_const_val, dest_lval);
            } else if (type->kind == AST_Type_Kind::ARRAY ||
                       type->kind == AST_Type_Kind::STRUCTURE ||
                       type->kind == AST_Type_Kind::UNION) {

                assert(type->bit_size % 8 == 0);
                auto byte_size = type->bit_size / 8;

                auto val_ptr = &interp->globals[dest_lval.index];
                assert(gm_cursor);
                val_ptr->pointer = gm_cursor;
                gm_cursor += byte_size;

                memset(val_ptr->pointer, 0 , byte_size);
            }
        }

        interp->globals_initialized = true;
    }

    void interp_store_constant(Interpreter *interp, Const_Value const_val,
                               Interpreter_LValue dest)
    {
        assert(const_val.type->kind == AST_Type_Kind::INTEGER);

        Interpreter_Value val = { .type = const_val.type, .integer_literal = const_val.integer };
        interp_store(interp, val, dest);
    }

    void interp_store_constant(Interpreter *interp, Const_Value const_val, void *dest_ptr)
    {
        assert(const_val.type->kind == AST_Type_Kind::INTEGER);
        assert(const_val.type->pointer_to);

        Interpreter_Value val = { .type = const_val.type, .integer_literal = const_val.integer };
        interp_store(interp, val, dest_ptr, val.type->pointer_to);
    }

    void interpreter_initialize_foreigns(Interpreter *interp,
                                         Array<BC_Function *> foreign_functions)
    {
        for (int64_t i = 0; i < foreign_functions.count; i++) {
            auto func = foreign_functions[i];
            bool found = ffi_load_function(&interp->ffi, func->name, &func->ffi_data);
            if (!found) {
                fprintf(stderr, "Did not find foreign function: '%s'\n", func->name.data);
            }
            assert(found);
        }

        interp->foreigns_initialized = true;
    }

    void interpreter_execute_foreign_function(Interpreter *interp, BC_Function *bc_func,
                                              int64_t arg_count, BC_Value *result_value)
    {
        assert(bc_func->flags & BC_FUNC_FLAG_FOREIGN);
        assert(bc_func->ffi_data.c_fn_ptr);

        interpreter_call_function_pointer(interp, bc_func->ffi_data.c_fn_ptr,
                                          bc_func->type, arg_count, result_value);
    }

    void interpreter_call_function_pointer(Interpreter *interp, void *fn_ptr, AST_Type *fn_type,
                                           int64_t arg_count, BC_Value *result_value)
    {
        assert(fn_ptr);
        assert(fn_type->kind == AST_Type_Kind::FUNCTION);

        assert(fn_type->function.param_types.count == arg_count);
        assert(stack_count(&interp->arg_stack) >= arg_count);

        if (result_value) {
            assert(result_value->kind == BC_Value_Kind::TEMP);
            assert(fn_type->function.return_type);
        }

        ffi_reset(&interp->ffi);

        for (int64_t i = 0; i < arg_count; i++) {
            auto param_type = fn_type->function.param_types[i];
            // assert(param_type->kind == AST_Type_Kind::POINTER);
            // param_type = param_type->pointer.base;

            void *arg_ptr = nullptr;

            auto param_ptr = stack_peek_ptr(&interp->arg_stack, arg_count - 1 - i);

            if (param_type->kind == AST_Type_Kind::ARRAY ||
                param_type->kind == AST_Type_Kind::STRUCTURE ||
                param_type->kind == AST_Type_Kind::UNION) {

                assert(false);
            } else {
                arg_ptr = &param_ptr->pointer;
            }

            assert(arg_ptr);

            ffi_push_arg(&interp->ffi, arg_ptr, param_type);
        }

        AST_Type *return_type = fn_type->function.return_type;
        void *return_val_ptr = nullptr;

        if (result_value) {
            return_type = result_value->type;
            assert(return_type == result_value->type);

            assert(result_value->kind == BC_Value_Kind::TEMP);
            assert(return_type->kind != AST_Type_Kind::ARRAY ||
                   return_type->kind != AST_Type_Kind::STRUCTURE ||
                   return_type->kind != AST_Type_Kind::UNION);
            auto rv_lval = interp_load_lvalue(interp, result_value);
            auto result_val_ptr = &interp->temp_stack.buffer[rv_lval.index];
            return_val_ptr = &result_val_ptr->pointer;
        }

        assert(return_type);
        ffi_call(&interp->ffi, fn_ptr, return_val_ptr, return_type);

        if (arg_count) stack_pop(&interp->arg_stack, arg_count);
    }

    void interpreter_execute_compiler_function(Interpreter *interp, BC_Function *func,
                                               int64_t arg_count)
    {
        assert(func->flags & BC_FUNC_FLAG_COMPILER_FUNC);

        assert(arg_count == 0);

        if (func->name == Builtin::atom_abort) {
            interp->aborted = true;
            interp->running = false;
            interp->exit_code = 134;
        } else {
            assert(false && "Unimplemented compiler function!");
        }
    }
}
