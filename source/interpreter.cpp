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
                    break;
                }

                case STOREL: assert(false);
                case STORE_ARG: assert(false);
                case STORE_GLOBAL: assert(false);
                case STORE_PTR: assert(false);
                case LOADL: assert(false);

                case LOAD_PARAM: {

                    Interpreter_Value param_val = interp_load_value(interp, inst.a);
                    // Interpreter_LValue dest = interp_load_lvalue(interp, inst.result);
                    Interpreter_LValue dest = interp_push_temp(interp, inst.result);

                    interp_store(interp, param_val, dest);

                    break;
                }

                case LOAD_GLOBAL: assert(false);
                case LOAD_PTR: assert(false);
                case ADD_S: assert(false);
                case SUB_S: assert(false);
                case REM_S: assert(false);

                case MUL_S: {
                    Interpreter_Value lhs = interp_load_value(interp, inst.a);
                    Interpreter_Value rhs = interp_load_value(interp, inst.b);

                    Interpreter_LValue dest = interp_push_temp(interp, inst.result);

                    assert(lhs.type == rhs.type);
                    assert(lhs.type == dest.type);

                    auto type = lhs.type;

                    Interpreter_Value result = {
                        .type = type,
                    };

                    switch (type->bit_size) {
                        case 8: {
                            result.integer_literal.s8 =
                                lhs.integer_literal.s8 * rhs.integer_literal.s8;
                                break;
                        }
                        case 16: {
                            result.integer_literal.s16 =
                                lhs.integer_literal.s16 * rhs.integer_literal.s16;
                                break;
                        }
                        case 32: {
                            result.integer_literal.s32 =
                                lhs.integer_literal.s32 * rhs.integer_literal.s32;
                                break;
                        }
                        case 64: {
                            result.integer_literal.s64 =
                                lhs.integer_literal.s64 * rhs.integer_literal.s64;
                                break;
                        }
                        default: assert(false);
                    }

                    interp_store(interp, result, dest);
                    break;
                }

                case DIV_S: assert(false);
                case EQ_S: assert(false);
                case NEQ_S: assert(false);
                case LT_S: assert(false);
                case LTEQ_S: assert(false);
                case GT_S: assert(false);
                case GTEQ_S: assert(false);
                case ADD_U: assert(false);
                case SUB_U: assert(false);
                case REM_U: assert(false);
                case MUL_U: assert(false);
                case DIV_U: assert(false);
                case EQ_U: assert(false);
                case NEQ_U: assert(false);
                case LT_U: assert(false);
                case LTEQ_U: assert(false);
                case GT_U: assert(false);
                case GTEQ_U: assert(false);
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
                    break;
                }

                case JUMP: assert(false);
                case JUMP_IF: assert(false);
                case SWITCH: assert(false);
                case PTR_OFFSET: assert(false);
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
            case BC_Value_Kind::STRING_LITERAL: assert(false);
            case BC_Value_Kind::BOOL_LITERAL: assert(false);
            case BC_Value_Kind::NULL_LITERAL: assert(false);

            case BC_Value_Kind::TEMP: {
                auto frame = stack_top_ptr(&interp->frames);
                auto temp_index = frame->first_temp_index + bc_val->temp.index;
                assert(stack_count(&interp->temp_stack) > temp_index);
                result = interp->temp_stack.buffer[temp_index];
                break;
            }

            case BC_Value_Kind::ALLOCL: assert(false);

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
                break;
            }

            case BC_Value_Kind::ALLOCL: assert(false);
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
