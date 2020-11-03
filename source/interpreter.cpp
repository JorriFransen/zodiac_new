
#include "interpreter.h"

#include "builtin.h"
#include "os.h"

#include <cstdio>

namespace Zodiac
{

    Interpreter interpreter_create(Allocator *allocator, Build_Data *build_data)
    {
        Interpreter result = {};

        result.allocator = allocator;
        result.build_data = build_data;

        result.running = false;

        result.stack_size = 4096;
        result.stack = alloc_array<uint8_t>(allocator, 4096);
        result.sp = 0;

        result.frame_pointer = 0;
        result.ip = {};

        result.exit_code = 0;

        return result;
    }

    void interpreter_start(Interpreter *interp, Bytecode_Function *entry_func)
    {
        assert(entry_func->blocks.count);

        interp->ip = {
            .function = entry_func,
            .block = entry_func->blocks[0],
            .index = 0,
        };

        interp_stack_push(interp, 0); // fp
        Instruction_Pointer empty_ip = {};
        interp_stack_push(interp, empty_ip);

        for (int64_t i = 0; i < entry_func->locals.count; i++)
        {
            auto allocl = entry_func->locals[i];

            assert(allocl->type->bit_size % 8 == 0);
            auto size = allocl->type->bit_size / 8;
            assert(interp->sp + size <= interp->stack_size);

            allocl->allocl.byte_offset_from_fp = interp->sp;

            interp->sp += size;
        }

        for (int64_t i = 0; i < entry_func->temps.count; i++)
        {
            auto temp = entry_func->temps[i];

            assert(temp->type->bit_size % 8 == 0);
            auto size = temp->type->bit_size / 8;
            assert(interp->sp + size <= interp->stack_size);

            temp->temp.byte_offset_from_fp = interp->sp;

            interp->sp += size;
        }

        interp->running = true;

        while (interp->running)
        {
            Bytecode_Instruction *inst = interpreter_fetch_instruction(interp);

            bool advance_ip = true;

            switch (inst->op)
            {
                case NOP: assert(false);

                case ALLOCL:
                {
                    break;
                }

                case STOREL:
                case STORE_ARG:
                {
                    uint8_t *dest_ptr = interpreter_load_lvalue(interp, inst->a);
                    Bytecode_Value source_val = interpreter_load_value(interp, inst->b);

                    interp_store_value(dest_ptr, source_val);
                    break;
                }

                case STORE_PTR:
                {
                    Bytecode_Value ptr_val = interpreter_load_value(interp, inst->a);
                    Bytecode_Value source_val = interpreter_load_value(interp, inst->b);
                    interp_store_value((uint8_t*)ptr_val.pointer, source_val);
                    break;
                }

                case LOADL: {
                    Bytecode_Value result_value = interpreter_load_value(interp, inst->a);
                    auto result_addr = interpreter_load_lvalue(interp, inst->result);

                    interp_store_value(result_addr, result_value);

                    break;
                }

                case LOAD_PARAM: {
                    Bytecode_Value arg_value = interpreter_load_value(interp, inst->a);
                    auto result_addr = interpreter_load_lvalue(interp, inst->result);

                    interp_store_value(result_addr, arg_value);
                    break;
                }

                case LOAD_PTR: {
                    Bytecode_Value ptr_val = interpreter_load_value(interp, inst->a);
                    auto result_addr = interpreter_load_lvalue(interp, inst->result);

                    assert(ptr_val.type->kind == AST_Type_Kind::POINTER);
                    auto element_type = ptr_val.type->pointer.base;

                    assert(element_type->bit_size % 8 == 0);
                    auto byte_size = element_type->bit_size / 8;

                    memcpy(result_addr, ptr_val.pointer, byte_size);
                    break;
                }

#define _binop_arithmetic_int(op, signed) { \
    auto lhs = interpreter_load_value(interp, inst->a); \
    auto rhs = interpreter_load_value(interp, inst->b); \
    assert(lhs.type == rhs.type); \
    auto result_addr = interpreter_load_lvalue(interp, inst->result); \
    if (signed) {  \
        switch (lhs.type->bit_size) { \
            case 8: { \
                int8_t result_value = lhs.integer_literal.s8 op rhs.integer_literal.s8; \
                interp_store(result_addr, result_value); \
                break; } \
            case 16: { \
                int16_t result_value = lhs.integer_literal.s16 op rhs.integer_literal.s16; \
                interp_store(result_addr, result_value); \
                break; } \
            case 32: { \
                int32_t result_value = lhs.integer_literal.s32 op rhs.integer_literal.s32; \
                interp_store(result_addr, result_value); \
                break; }\
            case 64: {\
                int64_t result_value = lhs.integer_literal.s64 op rhs.integer_literal.s64; \
                interp_store(result_addr, result_value); \
                break; } \
        } \
    } else {\
        uint64_t result_value = lhs.integer_literal.u64 op rhs.integer_literal.u64; \
        interp_store(result_addr, result_value); \
    } \
    break; \
}

                case ADD_S:_binop_arithmetic_int(+, true);
                case SUB_S:_binop_arithmetic_int(-, true);
                case REM_S:_binop_arithmetic_int(%, true);
                case MUL_S:_binop_arithmetic_int(*, true);
                case DIV_S:_binop_arithmetic_int(/, true);

                case ADD_U:_binop_arithmetic_int(+, false);
                case SUB_U:_binop_arithmetic_int(-, false);
                case REM_U:_binop_arithmetic_int(%, false);
                case MUL_U:_binop_arithmetic_int(*, false);
                case DIV_U:_binop_arithmetic_int(/, false);

#undef _binop_arithmetic_int

#define _binop_arithmetic_float(op, signed) { \
    auto lhs = interpreter_load_value(interp, inst->a); \
    auto rhs = interpreter_load_value(interp, inst->b); \
    assert(lhs.type == rhs.type); \
    assert(lhs.type->bit_size == 64 && "float arithmetic only supports 64 bits right now"); \
    auto result_addr = interpreter_load_lvalue(interp, inst->result); \
    int64_t result_value = lhs.r64 op rhs.r64; \
    interp_store(result_addr, result_value); \
    break; \
}

                case ADD_F:_binop_arithmetic_float(+, false);
                case SUB_F:_binop_arithmetic_float(-, false);
                case MUL_F:_binop_arithmetic_float(*, false);
                case DIV_F:_binop_arithmetic_float(/, false);

#undef _binop_arithmetic_float

#define _binop_compare_int(op) { \
    auto lhs = interpreter_load_value(interp, inst->a); \
    auto rhs = interpreter_load_value(interp, inst->b); \
    assert(lhs.type == rhs.type); \
    assert(lhs.type->kind == AST_Type_Kind::INTEGER); \
    assert(lhs.type->integer.sign); \
    assert(lhs.type->bit_size == 64); \
    auto result_addr = interpreter_load_lvalue(interp, inst->result); \
    bool result_value = lhs.integer_literal.s64 op rhs.integer_literal.s64; \
    assert(sizeof(result_value) == (inst->result->type->bit_size / 8)); \
    interp_store(result_addr, result_value); \
    break; \
}

                case EQ_S: _binop_compare_int(==);
                case NEQ_S: _binop_compare_int(!=);
                case LT_S: _binop_compare_int(<);
                case LTEQ_S: _binop_compare_int(<=);
                case GT_S: _binop_compare_int(>);
                case GTEQ_S: _binop_compare_int(>=);

#undef _binop_compare_int

                case EQ_F: assert(false);
                case NEQ_F: assert(false);
                case LT_F: assert(false);
                case LTEQ_F: assert(false);
                case GT_F: assert(false);
                case GTEQ_F: assert(false);

                case PUSH_ARG:
                {
                    Bytecode_Value arg_val = {};
                    if (inst->a->kind == Bytecode_Value_Kind::ALLOCL) {
                        auto ptr = interpreter_load_lvalue(interp, inst->a);
                        arg_val.pointer = ptr;
                        arg_val.type = inst->a->type;
                        assert(arg_val.type->kind == AST_Type_Kind::POINTER);
                    } else {
                        auto kind = inst->a->kind;
                        assert(kind == Bytecode_Value_Kind::TEMP ||
                               kind == Bytecode_Value_Kind::INTEGER_LITERAL ||
                               kind == Bytecode_Value_Kind::FLOAT_LITERAL ||
                               kind == Bytecode_Value_Kind::STRING_LITERAL);
                        arg_val = interpreter_load_value(interp, inst->a);
                    }

                    assert(arg_val.type->bit_size % 8 == 0);
                    auto size = arg_val.type->bit_size / 8;
                    assert(interp->sp + size <= interp->stack_size);

                    uint8_t *arg_ptr = &interp->stack[interp->sp];
                    interp->sp += size;

                    interp_store_value(arg_ptr, arg_val);

                    break;
                }

                case CALL:
                {
                    advance_ip = false;

                    auto func_val = inst->a;
                    assert(func_val->kind == Bytecode_Value_Kind::FUNCTION);
                    auto func = func_val->function;

                    int64_t param_offset = -sizeof(int64_t);
                    int64_t total_arg_size = 0;
                    for (int64_t i = func->parameters.count - 1; i >= 0; i--)
                    {
                        auto param = func->parameters[i];

                        assert(param->type->bit_size % 8 == 0);
                        auto size = param->type->bit_size / 8;

                        param_offset -= size;
                        total_arg_size += size;

                        param->parameter.byte_offset_from_fp = param_offset;
                    }

                    interp_stack_push(interp, (int64_t)total_arg_size);

                    auto new_fp = interp->sp;
                    interp_stack_push(interp, interp->frame_pointer);
                    interp_stack_push(interp, interp->ip);

                    uint8_t *ret_val_ptr = nullptr;
                    if (inst->result)
                    {
                        ret_val_ptr = interpreter_load_lvalue(interp, inst->result);
                    }
                    interp_stack_push(interp, ret_val_ptr);

                    for (int64_t i = 0; i < func->locals.count; i++)
                    {
                        auto allocl = func->locals[i];

                        assert(allocl->type->bit_size % 8 == 0);
                        auto size = allocl->type->bit_size / 8;
                        assert(interp->sp + size <= interp->stack_size);

                        auto offset = interp->sp - new_fp;
                        allocl->allocl.byte_offset_from_fp = offset;

                        interp->sp += size;
                    }

                    for (int64_t i = 0; i < func->temps.count; i++)
                    {
                        auto temp = func->temps[i];

                        assert(temp->type->bit_size % 8 == 0);
                        auto size = temp->type->bit_size / 8;
                        assert(interp->sp + size <= interp->stack_size);

                        auto offset = interp->sp - new_fp;
                        temp->temp.byte_offset_from_fp = offset;

                        interp->sp += size;
                    }

                    assert(inst->a->kind == Bytecode_Value_Kind::FUNCTION);

                    auto arg_count_val = inst->b;
                    assert(arg_count_val->kind == Bytecode_Value_Kind::INTEGER_LITERAL);

                    interp->frame_pointer = new_fp;
                    interp->ip = {
                        .function = inst->a->function,
                        .block = inst->a->function->blocks[0],
                        .index = 0,
                    };

                    break;
                }

                case RETURN:
                {
                    auto ret_val = interpreter_load_value(interp, inst->a);

                    int64_t offset = 0;
                    int64_t old_fp = *(int64_t*)(&interp->stack[interp->frame_pointer]);

                    offset += sizeof(old_fp);
                    interp->ip = *(Instruction_Pointer*)(&interp->stack[interp->frame_pointer + offset]);

                    offset += sizeof(Instruction_Pointer);
                    uint8_t *ret_val_ptr = *(uint8_t**)(&interp->stack[interp->frame_pointer + offset]);

                    interp_store_value(ret_val_ptr, ret_val);

                    interp->sp = interp->frame_pointer;
                    interp->frame_pointer = old_fp;

                    int64_t total_arg_size = interp_stack_pop<int64_t>(interp);
                    interp->sp -= total_arg_size;
                    break;
                }

                case RETURN_VOID: {
                    int64_t offset = 0;
                    int64_t old_fp = *(int64_t*)(&interp->stack[interp->frame_pointer]);

                    offset += sizeof(old_fp);
                    interp->ip = *(Instruction_Pointer*)(&interp->stack[interp->frame_pointer + offset]);

                    interp->sp = interp->frame_pointer;
                    interp->frame_pointer = old_fp;

                    int64_t total_arg_size = interp_stack_pop<int64_t>(interp);
                    interp->sp -= total_arg_size;
                    break;
                }

                case JUMP: {
                    advance_ip = false;
                    Bytecode_Block *target_block = inst->a->block;
                    interp->ip.block = target_block;
                    interp->ip.index = 0;
                    break;
                }

                case JUMP_IF: {
                    advance_ip = false;
                    Bytecode_Value cond_val = interpreter_load_value(interp, inst->a);
                    assert(cond_val.type->kind == AST_Type_Kind::BOOL);

                    assert(inst->b->kind == Bytecode_Value_Kind::BLOCK);
                    assert(inst->result->kind == Bytecode_Value_Kind::BLOCK);

                    Bytecode_Block *then_block = inst->b->block;
                    Bytecode_Block *else_block = inst->result->block;

                    if (cond_val.integer_literal.u8) interp->ip.block = then_block;
                    else interp->ip.block = else_block;

                    interp->ip.index = 0;
                    break;
                }

                case PTR_OFFSET: {
                    auto ptr_val = interpreter_load_value(interp, inst->a);
                    auto offset_val = interpreter_load_value(interp, inst->b);
                    auto result_addr = interpreter_load_lvalue(interp, inst->result);

                    assert(ptr_val.type->kind == AST_Type_Kind::POINTER);
                    assert(offset_val.type->kind == AST_Type_Kind::INTEGER);

                    AST_Type *element_type = ptr_val.type->pointer.base;
                    assert(element_type->bit_size % 8 == 0);
                    auto byte_size = element_type->bit_size / 8;

                    void *result = ((uint8_t*)ptr_val.pointer) +
                                   (offset_val.integer_literal.s64 * byte_size);
                    interp_store(result_addr, result);
                    break;
                }

                case ZEXT: {
                    Bytecode_Value operand_val = interpreter_load_value(interp, inst->a);
                    auto result_addr = interpreter_load_lvalue(interp, inst->result);

                    auto result_type = inst->result->type;
                    assert(result_type->bit_size > operand_val.type->bit_size);

                    switch (result_type->bit_size) {
                        default: assert(false);
                        case 8: assert(false);
                        case 16: assert(false);
                        case 32: assert(false);

                        case 64: {
                            uint64_t new_val;
                            switch (operand_val.type->bit_size) {
                                default: assert(false);
                                case 8: new_val = operand_val.integer_literal.u8; break;
                            }
                            interp_store(result_addr, new_val);
                            break;
                        }
                    }

                    break;
                }

                case SEXT: assert(false);

                case TRUNC: {
                    Bytecode_Value operand_val = interpreter_load_value(interp, inst->a);
                    auto result_addr = interpreter_load_lvalue(interp, inst->result);

                    auto result_type = inst->result->type;
                    assert(result_type->bit_size < operand_val.type->bit_size);

                    switch (result_type->bit_size) {
                        default: assert(false);
                        case 8: {
                            uint8_t new_val;
                            switch (operand_val.type->bit_size) {
                                default: assert(false);
                                case 64: new_val = operand_val.integer_literal.u64; break;
                            }
                            interp_store(result_addr, new_val);
                            break;

                        }
                        case 16: assert(false);
                        case 32: assert(false);
                        case 64: assert(false);
                    }

                    break;
                }

                case F_TO_S: assert(false);

                case S_TO_F: assert(false);
                case U_TO_F: assert(false);
                case F_TO_F: assert(false);

                case EXIT: {
                    assert(inst->a);

                    auto exit_code_val = interpreter_load_value(interp, inst->a);
                    advance_ip = false;
                    interp->running = false;

                    interp->exit_code = exit_code_val.integer_literal.s64;

                    break;
                }

                case SYSCALL: {
                    Bytecode_Value *arg_count_val = inst->a;
                    assert(arg_count_val->kind == Bytecode_Value_Kind::INTEGER_LITERAL);
                    assert(arg_count_val->type == Builtin::type_u64);
                    auto arg_count = arg_count_val->integer_literal.s64;

                    Bytecode_Value *arg_size_val = inst->b;
                    assert(arg_size_val->kind == Bytecode_Value_Kind::INTEGER_LITERAL);
                    assert(arg_size_val->type == Builtin::type_s64);
                    auto arg_size = arg_size_val->integer_literal.s64;
                    assert(arg_size % 8 == 0); // Should all be 64 bit integers

                    Array<int64_t> args = {};
                    array_init(interp->allocator, &args, arg_count);

                    for (int64_t i = 0; i < arg_count; i++)
                    {
                        auto offset = -((arg_count - i) * sizeof(int64_t));
                        int64_t *param_ptr =
                            (int64_t*)&interp->stack[interp->sp + offset];
                        array_append(&args, *param_ptr);
                    }

                    os_syscall(args);
                    array_free(&args);

                    interp->sp -= arg_size;
                    break;
                }
            }

            if (advance_ip) interpreter_advance_ip(interp);
        }
    }

    Bytecode_Value interpreter_load_value(Interpreter *interp, Bytecode_Value *value)
    {

        uint8_t *source_ptr = interpreter_load_lvalue(interp, value);

        Bytecode_Value result = {};
        result.type = value->type;

        switch (value->type->kind) {

            case AST_Type_Kind::BOOL:
            case AST_Type_Kind::INTEGER: {
                switch (value->type->bit_size) {
                    case 8: result.integer_literal.s8 = *((int8_t*)source_ptr); break;
                    case 64: result.integer_literal.s64 = *((int64_t*)source_ptr); break;
                    default: assert(false);
                }
                break;
            }

            case AST_Type_Kind::FLOAT: {
                if (value->type == Builtin::type_float)       result.r32 = *((float *)source_ptr);
                else if (value->type == Builtin::type_double) result.r64 = *((double *)source_ptr);
                else {
                    assert(false);
                }
                break;
            }

            case AST_Type_Kind::POINTER: {
                result.pointer = *(void**)source_ptr;
                break;
            }

            case AST_Type_Kind::ARRAY: {
                if (value->kind == Bytecode_Value_Kind::ALLOCL) {
                    result.pointer = source_ptr;
                } else if (value->kind == Bytecode_Value_Kind::TEMP) {
                    result.pointer = *(void**)source_ptr;
                }
                assert(value->type->array.element_type->pointer_to);
                result.type = value->type->array.element_type->pointer_to;
                break;
            }

            default: assert(false);
        }

        return result;
    }

    uint8_t *interpreter_load_lvalue(Interpreter *interp, Bytecode_Value *value)
    {
        switch (value->kind)
        {
            case Bytecode_Value_Kind::TEMP:
            {
                return &interp->stack[interp->frame_pointer + value->temp.byte_offset_from_fp];
                break;
            }

            case Bytecode_Value_Kind::ALLOCL:
            {
                return &interp->stack[interp->frame_pointer + value->allocl.byte_offset_from_fp];
                break;
            }

            case Bytecode_Value_Kind::INTEGER_LITERAL:
            {
                return (uint8_t*)&value->integer_literal;
                break;
            }

            case Bytecode_Value_Kind::FLOAT_LITERAL:
            {
                if (value->type == Builtin::type_float) {
                    return (uint8_t*)&value->r32;
                } else if (value->type == Builtin::type_double) {
                    return (uint8_t*)&value->r64;
                } else {
                    assert(false);
                }
                break;
            }

            case Bytecode_Value_Kind::STRING_LITERAL:
            {
                return (uint8_t*)&value->string_literal.data;
                break;
            }

            case Bytecode_Value_Kind::PARAM:
            {
                assert(value->parameter.byte_offset_from_fp < 0);
                auto result = &interp->stack[interp->frame_pointer +
                                      value->parameter.byte_offset_from_fp];
                return result;
                break;
            }

            default: assert(false);
        }
    }

    Bytecode_Instruction *interpreter_fetch_instruction(Interpreter *interp)
    {
        Bytecode_Instruction *result = interp->ip.block->instructions[interp->ip.index];
        return result;
    }

    void interpreter_advance_ip(Interpreter *interp)
    {
        auto cb = interp->ip.block;
        auto index = interp->ip.index;

        assert(index + 1 < cb->instructions.count);

        interp->ip.index += 1;
    }

    void interpreter_free(Interpreter *interp)
    {
        free(interp->allocator, interp->stack);
    }

    void interp_store_value(uint8_t *dest, Bytecode_Value val)
    {
        switch (val.type->kind) {

            case AST_Type_Kind::INTEGER: {
                switch (val.type->bit_size)
                {
                    case 8: interp_store(dest, val.integer_literal.s8); break;
                    case 16: interp_store(dest, val.integer_literal.s16); break;
                    case 32: interp_store(dest, val.integer_literal.s32); break;
                    case 64: interp_store(dest, val.integer_literal.s64); break;
                    default: assert(false);
                }
                break;
            }

            case AST_Type_Kind::FLOAT: {
                if (val.type == Builtin::type_float)       interp_store(dest, val.r32); 
                else if (val.type == Builtin::type_double) interp_store(dest, val.r64);
                else {
                    assert(false);
                }
                break;
            }

            case AST_Type_Kind::POINTER: {
                interp_store(dest, val.pointer);
                break;
            }

            case AST_Type_Kind::ARRAY: assert(false);

            default: assert(false);
        }
    }
}
