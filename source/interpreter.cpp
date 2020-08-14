
#include "interpreter.h"
#include "builtin.h"

namespace Zodiac
{
    void interpreter_init(Allocator *allocator, Interpreter *interp)
    {
        assert(allocator);
        assert(interp);

        interp->allocator = allocator;
        interp->program = nullptr;

        interp->running = false;

        stack_init(allocator, &interp->stack_frames);
        stack_init(allocator, &interp->temp_stack);
        stack_init(allocator, &interp->allocl_stack);
        stack_init(allocator, &interp->arg_stack);
    }

    void interpreter_free(Interpreter *interp)
    {
        assert(interp);

        stack_free(&interp->stack_frames);
        stack_free(&interp->temp_stack);
        stack_free(&interp->allocl_stack);
        stack_free(&interp->arg_stack);
    }

    void interpreter_execute_entry(Interpreter *interp, Bytecode_Program *program)
    {
        assert(interp);
        assert(program);
        assert(program->entry_function);

        assert(interp->program == nullptr);
        assert(interp->running == false);
        interp->running = true;
        interp->program = program;

        interpreter_execute_function(interp, program->entry_function, 0);
    }

    void interpreter_execute_function(Interpreter *interp, Bytecode_Function *func,
                                      int64_t arg_count)
    {
        assert(interp);
        assert(func);
        assert(func->blocks.count);

        Stack_Frame frame = interpreter_create_stack_frame(interp->allocator, func);
        stack_push(&interp->stack_frames, frame);
        auto frame_p = interpreter_current_frame(interp);

        for (int64_t i = 0; i < arg_count; i++)
        {
            array_append(&frame_p->parameters, stack_peek(&interp->arg_stack, (arg_count - 1) - i));
        }

        for (int64_t i = 0; i < arg_count; i++)
        {
            stack_pop(&interp->arg_stack);
        }

        for (int64_t i = 0; i < func->local_allocs.count; i++)
        {
            Bytecode_Value allocl_val = {};
            allocl_val.kind = Bytecode_Value_Kind::ALLOCL;
            allocl_val.type = func->local_allocs[i].value->type;
            assert(allocl_val.type);

            stack_push(&interp->allocl_stack, allocl_val);
        }

        Bytecode_Block *current_block = interpreter_current_block(interp, frame_p);
        while (!frame_p->returned && interp->running && current_block)
        {
            interpreter_execute_block(interp, current_block); 
            frame_p = interpreter_current_frame(interp);
            auto next_block = interpreter_current_block(interp, frame_p);
            if (next_block == current_block) break;
            current_block = next_block;
        }

        for (int64_t i = 0; i < func->local_allocs.count; i++)
        {
            stack_pop(&interp->allocl_stack);
        }

        if (interp->running)
        {
            assert(frame_p->returned);
        }
    }

    void interpreter_execute_block(Interpreter *interp, Bytecode_Block *block)
    {
        assert(interp);
        assert(block);
        assert(block->instructions.count);

        auto frame = interpreter_current_frame(interp);

        frame->instruction_index = 0;

        while (!frame->returned &&
               interp->running && 
               frame->instruction_index < block->instructions.count)
        {
            Bytecode_Instruction inst = interpreter_fetch_instruction(interp);

            switch (inst)
            {
                case Bytecode_Instruction::NOP: assert(false);

                case Bytecode_Instruction::EXIT:
                {
                    uint32_t temp_index = interpreter_fetch<uint32_t>(interp);
                    Bytecode_Value *exit_code = interpreter_load_temporary(interp, temp_index);
                    assert(exit_code);
                    
                    interp->running = false;
                    interp->exit_code_value = *exit_code;
                    break;
                }

                case Bytecode_Instruction::CALL:
                {
                    uint32_t func_index = interpreter_fetch<uint32_t>(interp);
                    uint32_t arg_count = interpreter_fetch<uint32_t>(interp);

                    auto func = interp->program->functions[func_index];
                    assert(func);

                    interpreter_execute_function(interp, func, arg_count);
                    auto return_frame = stack_pop(&interp->stack_frames);

                    if (return_frame.parameters.count)
                    {
                        array_free(&return_frame.parameters);
                    }

                    if (interp->running)
                    {
                        assert(return_frame.return_value.type);

                        Bytecode_Value *return_value =
                            interpreter_push_temporary(interp, return_frame.return_value.type);

                        *return_value = return_frame.return_value;

                    }
                    break;
                }

                case Bytecode_Instruction::RETURN:
                {
                    uint32_t temp_index = interpreter_fetch<uint32_t>(interp);

                    auto ret_val = interpreter_load_temporary(interp, temp_index);
                    assert(ret_val);

                    frame->return_value = *ret_val;

                    frame->returned = true;
                    break;
                }

                case Bytecode_Instruction::ALLOCL:
                {
                    interpreter_fetch<uint32_t>(interp);
                    // Do nothing, handled on function call.
                    break;
                }

                case Bytecode_Instruction::LOAD_IM:
                {
                    auto size_spec = interpreter_fetch<Bytecode_Size_Specifier>(interp);
                    switch (size_spec)
                    {
                        case Bytecode_Size_Specifier::INVALID: assert(false);
                        case Bytecode_Size_Specifier::SIGN_FLAG: assert(false);
                        case Bytecode_Size_Specifier::U8: assert(false);
                        case Bytecode_Size_Specifier::S8: assert(false);
                        case Bytecode_Size_Specifier::U16: assert(false);
                        case Bytecode_Size_Specifier::S16: assert(false);
                        case Bytecode_Size_Specifier::U32: assert(false);
                        case Bytecode_Size_Specifier::S32: assert(false);
                        case Bytecode_Size_Specifier::U64: assert(false);
                        case Bytecode_Size_Specifier::S64:
                        {
                            int64_t val = interpreter_fetch<int64_t>(interp);
                            auto bc_val = interpreter_push_temporary(interp, Builtin::type_s64); 
                            bc_val->integer_literal.value = val;
                            break;
                        }
                        default: assert(false);
                    }

                    break;
                }

                case Bytecode_Instruction::LOADL:
                {
                    auto allocl_index = interpreter_fetch<uint32_t>(interp);
                    assert(allocl_index >= 0);

                    auto offset = (frame->alloc_count - 1) - allocl_index;
                    auto source_allocl = stack_peek_ptr(&interp->allocl_stack, offset);
                    assert(source_allocl);

                    auto dest_val = interpreter_push_temporary(interp, source_allocl->type);
                    *dest_val = *source_allocl;
                    break;
                }

                case Bytecode_Instruction::LOAD_PARAM:
                {
                    auto param_index = interpreter_fetch<uint32_t>(interp);
                    assert(param_index < frame->parameters.count);

                    auto& param = frame->parameters[param_index];
                    assert(param.type);

                    auto result_value = interpreter_push_temporary(interp, param.type);
                    *result_value = param;
                    break;
                }

                case Bytecode_Instruction::STOREL:
                {
                    auto allocl_index = interpreter_fetch<uint32_t>(interp);
                    auto val_index = interpreter_fetch<uint32_t>(interp);

                    auto source_val = interpreter_load_temporary(interp, val_index);

                    auto offset = (frame->alloc_count - 1) - allocl_index;
                    assert(offset < frame->alloc_count);
                    
                    auto dest_allocl = stack_peek_ptr(&interp->allocl_stack, offset);

                    assert(source_val);
                    assert(dest_allocl);
                    *dest_allocl = *source_val;
                    break;
                }

                case Bytecode_Instruction::PUSH_ARG:
                {
                    auto temp_index = interpreter_fetch<int32_t>(interp);
                    auto arg_val = interpreter_load_temporary(interp, temp_index);
                    assert(arg_val->type);
                    stack_push(&interp->arg_stack, *arg_val);
                    break;
                }

                case Bytecode_Instruction::ADD:
                {
                    auto size_spec = interpreter_fetch<Bytecode_Size_Specifier>(interp);
                    auto lhs_index = interpreter_fetch<uint32_t>(interp);
                    auto rhs_index = interpreter_fetch<uint32_t>(interp);

                    auto lhs_val = interpreter_load_temporary(interp, lhs_index);
                    auto rhs_val = interpreter_load_temporary(interp, rhs_index);

                    assert(lhs_val->type == rhs_val->type);

                    auto result_val = interpreter_push_temporary(interp, lhs_val->type);
                    
                    switch (size_spec)
                    {
                        case Bytecode_Size_Specifier::INVALID: assert(false);
                        case Bytecode_Size_Specifier::SIGN_FLAG: assert(false);
                        case Bytecode_Size_Specifier::U8: assert(false);
                        case Bytecode_Size_Specifier::S8: assert(false);
                        case Bytecode_Size_Specifier::U16: assert(false);
                        case Bytecode_Size_Specifier::S16: assert(false);
                        case Bytecode_Size_Specifier::U32: assert(false);
                        case Bytecode_Size_Specifier::S32: assert(false);
                        case Bytecode_Size_Specifier::U64: assert(false);
                        case Bytecode_Size_Specifier::S64:
                        {
                            result_val->integer_literal.value =
                                lhs_val->integer_literal.value + rhs_val->integer_literal.value;
                            break;
                        }
                        default: assert(false);

                    }
                    break;
                }
            }
        }
    }

    Stack_Frame interpreter_create_stack_frame(Allocator *allocator, Bytecode_Function *func)
    {
        assert(func);
        assert(func->blocks.count);

        Stack_Frame result = {};

        result.instruction_index = 0;
        result.block_index = 0;
        result.local_count = 0;
        result.alloc_count = func->local_allocs.count;
        result.returned = false;
        result.func = func;

        array_init(allocator, &result.parameters, func->parameters.count);

        return result;
    }

    Stack_Frame *interpreter_current_frame(Interpreter *interp)
    {
        assert(interp);
        assert(stack_count(&interp->stack_frames));

        return stack_top_ptr(&interp->stack_frames);
    }

    Bytecode_Block *interpreter_current_block(Interpreter *interp)
    {
        assert(interp);

        if (interp->running && stack_count(&interp->stack_frames))
        {
            auto frame = interpreter_current_frame(interp);
            return interpreter_current_block(interp, frame);
        }

        return nullptr;
    }

    Bytecode_Block *interpreter_current_block(Interpreter *interp, Stack_Frame *frame)
    {
        assert(interp);
        assert(frame);

        assert(frame->block_index >= 0);
        assert(frame->block_index < frame->func->blocks.count);

        return frame->func->blocks[frame->block_index];
    }

    Bytecode_Value *interpreter_push_temporary(Interpreter *interp, AST_Type *type)
    {
        assert(type);

        Bytecode_Value value = {};
        value.kind = Bytecode_Value_Kind::TEMPORARY;
        value.type = type;
        
        stack_push(&interp->temp_stack, value);
        auto frame = interpreter_current_frame(interp);
        frame->local_count += 1;

        return stack_top_ptr(&interp->temp_stack);
    }

    Bytecode_Value *interpreter_load_temporary(Interpreter *interp, int32_t local_index)
    {
        assert(interp);
        assert(interp->running);

        auto frame = interpreter_current_frame(interp);
        auto offset = (frame->local_count - 1) - local_index;
        assert(offset >= 0 && offset < frame->local_count);

        return stack_peek_ptr(&interp->temp_stack, offset);
    }

    Bytecode_Instruction interpreter_fetch_instruction(Interpreter *interp)
    {
        assert(interp);
        assert(interp->running);

        return (Bytecode_Instruction)interpreter_fetch<uint8_t>(interp);
    }
}
