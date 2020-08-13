#pragma once

#include "allocator.h"
#include "bytecode.h"
#include "stack.h"

namespace Zodiac
{
    struct Stack_Frame
    {
        int64_t instruction_index = -1;
        int64_t block_index = -1;
        int32_t local_count = -1;
        bool returned = false;
        Bytecode_Function *func = nullptr;
        Bytecode_Value return_value = {};

        Array<Bytecode_Value> parameters = {};
    };

    struct Interpreter
    {
        Allocator *allocator = nullptr;

        Bytecode_Program *program = nullptr;

        bool running = false;
        int64_t temp_offset = 0;

        Stack<Stack_Frame> stack_frames = {};
        Stack<Bytecode_Value> temp_stack = {};
        Stack<Bytecode_Value> arg_stack = {};

        Bytecode_Value exit_code_value = {};
    };

    void interpreter_init(Allocator *allocator, Interpreter *interp);
    void interpreter_free(Interpreter *interp);

    void interpreter_execute_entry(Interpreter *interp, Bytecode_Program *program);
    void interpreter_execute_function(Interpreter *interp, Bytecode_Function *func,
                                      int64_t arg_count);
    void interpreter_execute_block(Interpreter *interp, Bytecode_Block *block);

    Stack_Frame interpreter_create_stack_frame(Allocator *allocator, Bytecode_Function *func);

    Stack_Frame *interpreter_current_frame(Interpreter *interp);
    Bytecode_Block *interpreter_current_block(Interpreter *interp);
    Bytecode_Block *interpreter_current_block(Interpreter *interp, Stack_Frame *frame);

    Bytecode_Value *interpreter_push_temporary(Interpreter *interp, AST_Type *type);
    Bytecode_Value *interpreter_load_temporary(Interpreter *interp, int32_t local_index);

    Bytecode_Instruction interpreter_fetch_instruction(Interpreter *interp);

    template <typename T>
    T interpreter_fetch(Interpreter *interp)
    {
        assert(interp);
        assert(interp->running);

        auto frame = interpreter_current_frame(interp);
        auto block = interpreter_current_block(interp, frame);

        T result = *((T*)(&block->instructions[frame->instruction_index]));
        frame->instruction_index += sizeof(T);

        return result;
    }

}
