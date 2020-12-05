#pragma once

#include "allocator.h"
#include "build_data.h"
#include "bytecode.h"
#include "ffi.h"
#include "stack.h"

namespace Zodiac
{
    typedef Instruction_Locator Instruction_Pointer;

    struct Interpreter
    {
        Allocator *allocator = nullptr;
        Build_Data *build_data = nullptr;

        bool running = false;

        uint8_t *stack = nullptr;
        int64_t stack_size = 0;
        int64_t sp = 0;

        int64_t frame_pointer = 0;
        Instruction_Pointer ip = {};

        uint8_t *global_data = 0;

        FFI_Context ffi = {};

        void *null_pointer = nullptr;

        int64_t exit_code = 0;
    };

    Interpreter interpreter_create(Allocator *allocator, Build_Data *build_data);

    void interpreter_start(Interpreter *interp, Bytecode_Function *entry_func,
                           int64_t global_data_size, Array<Bytecode_Global_Info> global_info,
                           Array<Bytecode_Function *> foreign_functions);

    void interpreter_initialize_globals(Interpreter *interp, int64_t global_data_size,
                                        Array<Bytecode_Global_Info> global_info);
    void interpreter_initialize_foreigns(Interpreter *interp,
                                         Array<Bytecode_Function *> foreign_functions);

    void interpreter_execute_foreign_function(Interpreter *interp, Bytecode_Function *func,
                                              int64_t arg_count, Bytecode_Value *result_value);
    void interpreter_push_foreign_arg(Interpreter *interp, uint8_t *arg_ptr, AST_Type *type);

    Bytecode_Value interpreter_load_value(Interpreter *interp, Bytecode_Value *value);
    uint8_t *interpreter_load_lvalue(Interpreter *interp, Bytecode_Value *value);

    void interpreter_free(Interpreter *interp);

    void interp_store_value(uint8_t *dest, Bytecode_Value val);
    void interp_store_constant(uint8_t *dest, Const_Value val);

    template <typename T>
    void interp_store(uint8_t *dest, T value)
    {
        *((T*)dest) = value;
    }

    template <typename T>
    uint8_t *interp_stack_push(Interpreter *interp, T value)
    {
        int64_t size = sizeof(T);

        assert(interp->sp + size <= interp->stack_size);

        uint8_t *ptr = &interp->stack[interp->sp];

        memcpy(ptr, &value, size);
        interp->sp += size;


        return ptr;
    }

    template <typename T>
    T interp_stack_pop(Interpreter *interp)
    {
        int64_t size = sizeof(T);

        assert(interp->sp >= size);

        interp->sp -= size;

        uint8_t *ptr = &interp->stack[interp->sp];
        return *(T *)ptr;
    }
}
