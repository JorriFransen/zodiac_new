#include "interpreter.h"

#include "builtin.h"

#include <stdio.h>

namespace Zodiac
{
    Interpreter interpreter_create(Allocator *allocator, Build_Data *build_data)
    {
        Interpreter result = {
            .build_data = build_data,
            .exit_code = 0,
        };

        stack_init(allocator, &result.temp_stack);
        stack_init(allocator, &result.alloc_stack);
        stack_init(allocator, &result.arg_stack);
        stack_init(allocator, &result.frames);

        return result;
    }

    void interpreter_start(Interpreter *interp, BC_Function *entry_func)
    {
        Interp_Stack_Frame first_frame = {
            .function = entry_func,
            .ip = {
                .index = 0,
                .block = entry_func->blocks[0],
            },
            .first_temp_index = 1,
            .result_index = 0,
        };

        stack_push(&interp->frames, first_frame);

        AST_Type *return_type = entry_func->type->function.return_type;
        assert(return_type->kind == AST_Type_Kind::INTEGER);
        Interpreter_Value return_val = { .type = return_type, .integer_literal = {} };
        stack_push(&interp->temp_stack, return_val);

        bool running = true;

        Interp_Stack_Frame *frame = nullptr;

        while (running) {

            bool advance_ip = true;

            frame = stack_top_ptr(&interp->frames);
            auto inst = frame->ip.block->instructions[frame->ip.index];

            switch (inst.op) {
                case NOP: assert(false);

                case ALLOCL: {
                    assert(stack_count(&interp->alloc_stack) ==
                           inst.result->allocl.index + frame->first_alloc_index);

                    assert(inst.result->type->kind == AST_Type_Kind::POINTER);
                    Interpreter_Value alloc_value = {
                        .type = inst.result->type->pointer.base,
                    };
                    stack_push(&interp->alloc_stack, alloc_value);
                    break;
                }

                case STOREL: {
                    assert(inst.a->kind == BC_Value_Kind::ALLOCL);
                    Interpreter_LValue dest = interp_load_lvalue(interp, inst.a);
                    Interpreter_Value source = interp_load_value(interp, inst.b);

                    assert(dest.type == source.type);
                    interp_store(interp, source, dest);
                    break;
                }

                case STORE_ARG: assert(false);
                case STORE_GLOBAL: assert(false);
                case STORE_PTR: assert(false);

                case LOADL: {
                    Interpreter_Value value = interp_load_value(interp, inst.a);
                    Interpreter_LValue dest = interp_push_temp(interp, inst.result);

                    assert(dest.type == value.type);
                    interp_store(interp, value, dest);
                    break;
                }

                case LOAD_PARAM: {

                    Interpreter_Value param_val = interp_load_value(interp, inst.a);
                    Interpreter_LValue dest = interp_push_temp(interp, inst.result);

                    assert(dest.type == param_val.type);
                    interp_store(interp, param_val, dest);
                    break;
                }

                case LOAD_GLOBAL: assert(false);

                case LOAD_PTR: {
                    assert(inst.a->kind == BC_Value_Kind::TEMP);
                    auto ptr_val = interp_load_value(interp, inst.a);
                    assert(ptr_val.type->kind == AST_Type_Kind::POINTER);

                    auto dest_val = interp_push_temp(interp, inst.result);
                    assert(dest_val.type == ptr_val.type->pointer.base);

                    interp_store(interp, ptr_val.pointer, ptr_val.type, dest_val);
                    break;
                }

#define MAKE_INT_NAME(sign, size) sign##size

#define BINOP_INT_CASE(sign, size, op) \
    case size: {  (r.integer_literal. MAKE_INT_NAME(sign, size))  = \
                   (lhs.integer_literal. MAKE_INT_NAME(sign, size)) op \
                   (rhs.integer_literal. MAKE_INT_NAME(sign, size)); \
               break; }


#define BINOP_INT_CASES(sign, op) \
    BINOP_INT_CASE(sign, 8, op); \
    BINOP_INT_CASE(sign, 16, op); \
    BINOP_INT_CASE(sign, 32, op); \
    BINOP_INT_CASE(sign, 64, op);


#define BINOP_INT_(sign_, op) { \
    Interpreter_Value lhs = interp_load_value(interp, inst.a); \
    Interpreter_Value rhs = interp_load_value(interp, inst.b); \
    Interpreter_LValue dest = interp_push_temp(interp, inst.result); \
    assert(lhs.type == rhs.type); \
    assert(lhs.type == dest.type); \
    auto type = lhs.type; \
    if (#sign_[0] == 's') { assert(type->integer.sign); } \
    Interpreter_Value r = { \
        .type = type, \
    }; \
    switch (type->bit_size) { \
        BINOP_INT_CASES(sign_, op); \
        default: assert(false); \
    } \
    interp_store(interp, r, dest); \
    break; \
}

#define BINOP_INT(op) BINOP_INT_(s, op)
#define BINOP_UINT(op) BINOP_INT_(u, op)


                case ADD_S: BINOP_INT(+);
                case SUB_S: BINOP_INT(-);
                case REM_S: BINOP_INT(%);
                case MUL_S: BINOP_INT(*);
                case DIV_S: BINOP_INT(/);

                case ADD_U: BINOP_UINT(+);
                case SUB_U: BINOP_UINT(-);
                case REM_U: BINOP_UINT(%);
                case MUL_U: BINOP_UINT(*);
                case DIV_U: BINOP_UINT(/);

                case EQ_S:   BINOP_INT(==);
                case NEQ_S:  BINOP_INT(!=);
                case LT_S:   BINOP_INT(<);
                case LTEQ_S: BINOP_INT(<=);
                case GT_S:   BINOP_INT(>);
                case GTEQ_S: BINOP_INT(>=);

                case EQ_U:   BINOP_UINT(==);
                case NEQ_U:  BINOP_UINT(!=);
                case LT_U:   BINOP_UINT(<);
                case LTEQ_U: BINOP_UINT(<=);
                case GT_U:   BINOP_UINT(>);
                case GTEQ_U: BINOP_UINT(>=);

#undef MAKE_INT_NAME
#undef BINOP_INT_CASE
#undef BINOP_INT_CASES
#undef BINOP_INT_
#undef BINOP_INT
#undef BINOP_UINT

                case ADD_F: assert(false);
                case SUB_F: assert(false);
                case MUL_F: assert(false);
                case DIV_F: assert(false);
                case EQ_F: assert(false);
                case NEQ_F: assert(false);
                case LT_F: assert(false);
                case LTEQ_F: assert(false);
                case GT_F: assert(false);
                case GTEQ_F: assert(false);
                case NEG_LOG: assert(false);

                case PUSH_ARG: {
                    Interpreter_Value arg_val = interp_load_value(interp, inst.a);
                    stack_push(&interp->arg_stack, arg_val);
                    break;
                }

                case CALL: {
                    auto callee_val = inst.a;
                    assert(callee_val->kind == BC_Value_Kind::FUNCTION);
                    auto callee = callee_val->function;

                    assert((callee->flags & BC_FUNC_FLAG_EMITTED) ||
                           (callee->flags & BC_FUNC_FLAG_FOREIGN));

                    auto arg_count_val = inst.b;
                    assert(arg_count_val->type == Builtin::type_s64);
                    assert(arg_count_val->kind == BC_Value_Kind::INTEGER_LITERAL);
                    auto arg_count = arg_count_val->integer_literal.s64;

                    if (callee->flags & BC_FUNC_FLAG_FOREIGN) {
                        assert(false);
                        // interpreter_execute_foreign_function(interp, func, arg_count,
                        //                                      inst->result);
                        break;
                    }

                    if (callee->flags & BC_FUNC_FLAG_COMPILER_FUNC) {
                        assert(false);
                        // interpreter_execute_compiler_function(interp, func, arg_count,
                        //                                       inst->result);
                        break;
                    }

                    int64_t result_index = -1;

                    if (inst.result) {
                        Interpreter_LValue result_value = interp_push_temp(interp, inst.result);
                        result_index = result_value.index;
                    }


                    Interp_Stack_Frame new_frame = {
                        .function = callee,
                        .ip = {
                            .index = 0,
                            .block = callee->blocks[0],
                        },
                        .result_index = result_index,
                    };

                    if (arg_count) {
                        assert(stack_count(&interp->arg_stack) >= arg_count);
                        new_frame.first_arg_index = stack_count(&interp->arg_stack) - arg_count;
                    }

                    new_frame.first_temp_index = stack_count(&interp->temp_stack);
                    new_frame.first_alloc_index = stack_count(&interp->alloc_stack);

                    stack_push(&interp->frames, new_frame);

                    advance_ip = false;
                    frame->ip.index += 1;
                    assert(frame->ip.index <= frame->ip.block->instructions.count);

                    break;
                }

                case RETURN: {
                    advance_ip = false;

                    assert(inst.a);
                    Interpreter_Value return_value = interp_load_value(interp, inst.a);
                    assert(return_value.type);

                    auto old_frame = stack_pop(&interp->frames);

                    if (old_frame.function->parameters.count) {
                        stack_pop(&interp->arg_stack, old_frame.function->parameters.count);
                    }

                    if (old_frame.function->temps.count) {
                        stack_pop(&interp->temp_stack, old_frame.function->temps.count);
                    }

                    if (old_frame.function->locals.count) {
                        stack_pop(&interp->alloc_stack, old_frame.function->locals.count);
                    }

                    assert(old_frame.result_index >= 0);
                    assert(stack_count(&interp->temp_stack) > old_frame.result_index);

                    Interpreter_LValue dest = {
                        .kind = Interp_LValue_Kind::TEMP,
                        .type = return_value.type,
                        .index = old_frame.result_index,
                    };

                    interp_store(interp, return_value, dest);
                    break;
                }

                case RETURN_VOID: {
                    advance_ip = false;
                    auto old_frame = stack_pop(&interp->frames);
                    assert(old_frame.result_index == -1);

                    // printf("Returning from: %s\n", frame->function->name.data);
                    if (old_frame.function->parameters.count) {
                        assert(false); // Pop parameters
                    }

                    if (old_frame.function->temps.count) {
                        assert(false);
                    }

                    if (old_frame.function->locals.count) {
                        assert(false);
                    }
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

                case SWITCH: assert(false);

                case PTR_OFFSET: {
                    auto offset_val = interp_load_value(interp, inst.b);

                    assert(offset_val.type->kind == AST_Type_Kind::INTEGER);

                    auto result_val = interp_push_temp(interp, inst.result);

                    void *ptr = nullptr;

                    if (inst.a->kind == BC_Value_Kind::ALLOCL ||
                        inst.a->kind == BC_Value_Kind::GLOBAL) {
                        assert(false);
                        // ptr = _ptr_ptr;
                    } else if (inst.a->kind == BC_Value_Kind::PARAM) {
                        assert(false);
                    } else if (inst.a->kind == BC_Value_Kind::TEMP) {
                        auto pointer_val = interp_load_value(interp, inst.a);
                        assert(pointer_val.type->kind == AST_Type_Kind::POINTER);
                        assert(result_val.type == pointer_val.type);
                        ptr = pointer_val.pointer;
                    } else {
                        assert(false);
                        // ptr = *(void**)_ptr_ptr;
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

                case AGG_OFFSET: assert(false);
                case ZEXT: assert(false);
                case SEXT: assert(false);
                case TRUNC: assert(false);
                case F_TO_S: assert(false);
                case S_TO_F: assert(false);
                case U_TO_F: assert(false);
                case F_TO_F: assert(false);
                case PTR_TO_INT: assert(false);
                case PTR_TO_PTR: assert(false);
                case SIZEOF: assert(false);
                case OFFSETOF: assert(false);
                case EXIT: assert(false);
                case SYSCALL: assert(false);
            }

            if (advance_ip) {
                frame->ip.index += 1;
                assert(frame->ip.index <= frame->ip.block->instructions.count);
            }

            if (stack_count(&interp->frames) < 1) {
                running = false;
            }
        }

        assert(stack_count(&interp->temp_stack));
        auto exit_val = stack_pop(&interp->temp_stack);
        assert(exit_val.type->kind == AST_Type_Kind::INTEGER);
        assert(exit_val.type == Builtin::type_s64);

        interp->exit_code = exit_val.integer_literal.s64;
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

            case BC_Value_Kind::FLOAT_LITERAL: assert(false);

            case BC_Value_Kind::STRING_LITERAL: {
                result.string_literal = bc_val->string_literal.data;
                break;
            }

            case BC_Value_Kind::BOOL_LITERAL: assert(false);
            case BC_Value_Kind::NULL_LITERAL: assert(false);

            case BC_Value_Kind::TEMP: {
                auto frame = stack_top_ptr(&interp->frames);
                auto temp_index = frame->first_temp_index + bc_val->temp.index;
                assert(stack_count(&interp->temp_stack) > temp_index);
                result = interp->temp_stack.buffer[temp_index];
                break;
            }

            case BC_Value_Kind::ALLOCL: {
                auto frame = stack_top_ptr(&interp->frames);
                auto alloc_index = frame->first_alloc_index + bc_val->allocl.index;
                assert(stack_count(&interp->temp_stack) > alloc_index);
                result = interp->alloc_stack.buffer[alloc_index];
                break;
            }

            case BC_Value_Kind::PARAM: {
                auto frame = stack_top_ptr(&interp->frames);
                auto param_index = frame->first_arg_index + bc_val->parameter.index;
                assert(stack_count(&interp->arg_stack) > param_index);
                result = interp->arg_stack.buffer[param_index];
                break;
            }

            case BC_Value_Kind::GLOBAL: assert(false);
            case BC_Value_Kind::FUNCTION: assert(false);
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

                auto frame = stack_top_ptr(&interp->frames);
                auto index = frame->first_alloc_index + bc_val->allocl.index;
                assert(stack_count(&interp->alloc_stack) > index);


                result.kind = Interp_LValue_Kind::ALLOCL;
                result.index = index;
                result.type = bc_val->type->pointer.base;
                break;
            }

            case BC_Value_Kind::PARAM: assert(false);
            case BC_Value_Kind::GLOBAL: assert(false);
            case BC_Value_Kind::FUNCTION: assert(false);
            case BC_Value_Kind::BLOCK: assert(false);
            case BC_Value_Kind::TYPE: assert(false);
            case BC_Value_Kind::SWITCH_DATA: assert(false);
        }

        return result;
    }

    Interpreter_LValue interp_push_temp(Interpreter *interp, BC_Value *bc_val)
    {
        assert(bc_val->kind == BC_Value_Kind::TEMP);

        auto frame = stack_top_ptr(&interp->frames);

        auto index = bc_val->temp.index + frame->first_temp_index;

        assert(stack_count(&interp->temp_stack) == index);

        Interpreter_Value new_temp_value = { .type = bc_val->type };
        stack_push(&interp->temp_stack, new_temp_value);

        Interpreter_LValue result = {
            .kind = Interp_LValue_Kind::TEMP,
            .type = bc_val->type,
            .index = index,
        };

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
                dest_ptr = &interp->temp_stack.buffer[dest.index];
                assert(stack_count(&interp->temp_stack) > dest.index);
                break;
            }

            case Interp_LValue_Kind::ALLOCL: {
                dest_ptr = &interp->alloc_stack.buffer[dest.index];
                assert(stack_count(&interp->alloc_stack) > dest.index);
                break;
            }
        }

        assert(dest_ptr);
        assert(dest_ptr->type == source.type);

        switch (type->kind) {
            case AST_Type_Kind::INVALID: assert(false);
            case AST_Type_Kind::VOID: assert(false);

            case AST_Type_Kind::INTEGER: {
                dest_ptr->integer_literal = source.integer_literal;
                break;
            }

            case AST_Type_Kind::FLOAT: assert(false);

            case AST_Type_Kind::BOOL: {
                dest_ptr->boolean_literal = source.boolean_literal;
                break;
            }

            case AST_Type_Kind::POINTER: {
                dest_ptr->pointer = source.pointer;
                break;
            }

            case AST_Type_Kind::FUNCTION: assert(false);
            case AST_Type_Kind::STRUCTURE: assert(false);
            case AST_Type_Kind::UNION: assert(false);
            case AST_Type_Kind::ENUM: assert(false);
            case AST_Type_Kind::ARRAY: assert(false);
        }
    }

    void interp_store(Interpreter *interp, void *source_ptr, AST_Type *source_type,
                      Interpreter_LValue dest)
    {
        assert(source_type->kind == AST_Type_Kind::POINTER);
        assert(dest.type == source_type->pointer.base);

        Interpreter_Value *dest_ptr = nullptr;

        switch (dest.kind) {
            case Interp_LValue_Kind::INVALID: assert(false);

            case Interp_LValue_Kind::TEMP: {
                dest_ptr = &interp->temp_stack.buffer[dest.index];
                assert(stack_count(&interp->temp_stack) > dest.index);
                break;
            }

            case Interp_LValue_Kind::ALLOCL: {
                dest_ptr = &interp->alloc_stack.buffer[dest.index];
                assert(stack_count(&interp->alloc_stack) > dest.index);
                break;
            }
        }

        assert(dest_ptr);
        assert(dest_ptr->type == source_type->pointer.base);

        switch(dest.type->kind) {
            case AST_Type_Kind::INVALID: assert(false);
            case AST_Type_Kind::VOID: assert(false);

            case AST_Type_Kind::INTEGER:
            {
                if (dest.type->integer.sign) {
                    switch (dest.type->bit_size) {
                        default: assert(false);
                        case 8: dest_ptr->integer_literal.s8 = *((int8_t*)source_ptr);
                        case 16: dest_ptr->integer_literal.s16 = *((int16_t*)source_ptr);
                        case 32: dest_ptr->integer_literal.s32 = *((int32_t*)source_ptr);
                        case 64: dest_ptr->integer_literal.s64 = *((int64_t*)source_ptr);
                    }
                } else {
                    switch (dest.type->bit_size) {
                        default: assert(false);
                        case 8: dest_ptr->integer_literal.u8 = *((uint8_t*)source_ptr);
                        case 16: dest_ptr->integer_literal.u16 = *((uint16_t*)source_ptr);
                        case 32: dest_ptr->integer_literal.u32 = *((uint32_t*)source_ptr);
                        case 64: dest_ptr->integer_literal.u64 = *((uint64_t*)source_ptr);
                    }
                }
                break;
            }

            case AST_Type_Kind::FLOAT: assert(false);
            case AST_Type_Kind::BOOL: assert(false);
            case AST_Type_Kind::POINTER: assert(false);
            case AST_Type_Kind::FUNCTION: assert(false);
            case AST_Type_Kind::STRUCTURE: assert(false);
            case AST_Type_Kind::UNION: assert(false);
            case AST_Type_Kind::ENUM: assert(false);
            case AST_Type_Kind::ARRAY: assert(false);
        }
    }
}
