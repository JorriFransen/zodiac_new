
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
        interp->temp_offset = 0;

        stack_init(allocator, &interp->stack_frames);
        stack_init(allocator, &interp->temp_stack);
    }

    void interpreter_free(Interpreter *interp)
    {
        assert(interp);

        stack_free(&interp->stack_frames);
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

        interpreter_execute_function(interp, program->entry_function);
    }

    void interpreter_execute_function(Interpreter *interp, Bytecode_Function *func)
    {
        assert(interp);
        assert(func);
        assert(func->blocks.count);

        Stack_Frame frame = interpreter_create_stack_frame(func);
        stack_push(&interp->stack_frames, frame);

        Bytecode_Block *current_block = interpreter_current_block(interp);
        while (interp->running && current_block)
        {
            interpreter_execute_block(interp, current_block); 
            current_block = interpreter_current_block(interp);
        }

        if (interp->running)
        {
            assert(false);
        }
    }

    void interpreter_execute_block(Interpreter *interp, Bytecode_Block *block)
    {
        assert(interp);
        assert(block);
        assert(block->instructions.count);

        auto frame = interpreter_current_frame(interp);

        frame->instruction_index = 0;

        while (interp->running && 
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

                    assert(arg_count == 0);

                    auto func = interp->program->functions[func_index];
                    assert(func);

                    interpreter_execute_function(interp, func);
                    stack_pop(&interp->stack_frames);

                    break;
                }

                case Bytecode_Instruction::RETURN: assert(false);
                case Bytecode_Instruction::ALLOCL: assert(false);

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

                case Bytecode_Instruction::LOADL: assert(false);
                case Bytecode_Instruction::LOAD_PARAM: assert(false);
                case Bytecode_Instruction::STOREL: assert(false);
                case Bytecode_Instruction::PUSH_ARG: assert(false);

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

    Stack_Frame interpreter_create_stack_frame(Bytecode_Function *func)
    {
        assert(func);
        assert(func->blocks.count);

        Stack_Frame result = {};

        result.instruction_index = 0;
        result.block_index = 0;
        result.local_count = 0;
        result.func = func;

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
        assert(interp->running);
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
        frame->local_count++;

        return stack_top_ptr(&interp->temp_stack);
    }

    Bytecode_Value *interpreter_load_temporary(Interpreter *interp, int32_t local_index)
    {
        assert(interp);
        assert(interp->running);

        auto index = local_index + interp->temp_offset;
        assert(index < stack_count(&interp->temp_stack));

        auto frame = interpreter_current_frame(interp);

        return stack_peek_ptr(&interp->temp_stack, (frame->local_count - 1) - index);
    }

    Bytecode_Instruction interpreter_fetch_instruction(Interpreter *interp)
    {
        assert(interp);
        assert(interp->running);

        return (Bytecode_Instruction)interpreter_fetch<uint8_t>(interp);
    }
}
