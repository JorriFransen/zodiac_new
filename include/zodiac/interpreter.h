#pragma once

#include "allocator.h"
#include "build_data.h"
#include "bytecode.h"
#include "stack.h"

namespace Zodiac
{
    struct Instruction_Pointer
    {
        Bytecode_Function *function = nullptr;
        Bytecode_Block *block = nullptr;

        int64_t index = 0;
    };

    struct Interpreter
    {
        Allocator *allocator = nullptr;
        Build_Data *build_data = nullptr;

        bool running = false;

        uint8_t *stack = nullptr;
        int64_t stack_size = 0;
        int64_t sp = 0;

        Stack<Bytecode_Value> arg_stack = {};

        int64_t frame_pointer = 0;
        Instruction_Pointer ip = {};

        int64_t exit_code = 0;
    };

    Interpreter interpreter_create(Allocator *allocator, Build_Data *build_data);

    void interpreter_start(Interpreter *interp, Bytecode_Function *entry_func);

    Bytecode_Value interpreter_load_value(Interpreter *interp, Bytecode_Value *value);
    uint8_t *interpreter_load_lvalue(Interpreter *interp, Bytecode_Value *value);

    Bytecode_Instruction *interpreter_fetch_instruction(Interpreter *interp);

    void interpreter_advance_ip(Interpreter *interp);

    void interpreter_free(Interpreter *interp);

    void interp_store_value(uint8_t *dest, Bytecode_Value val);

    template <typename T>
    void interp_store(uint8_t *dest, T value)
    {
        *((T*)dest) = value;
    }

    template <typename T>
    uint8_t *interp_stack_push(Interpreter *interp, T value)
    {
        auto size = sizeof(T);

        assert(interp->sp + size <= interp->stack_size);

        uint8_t *ptr = &interp->stack[interp->sp];

        memcpy(ptr, &value, size);
        interp->sp += size;


        return ptr;
    }

    template <typename T>
    T interp_stack_pop(Interpreter *interp)
    {
        auto size = sizeof(T);

        assert(interp->sp >= size);

        interp->sp -= size;

        uint8_t *ptr = &interp->stack[interp->sp];
        return *(T *)ptr;
    }
}
