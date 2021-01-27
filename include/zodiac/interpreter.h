#pragma once

#include "allocator.h"
#include "build_data.h"
#include "bytecode.h"
#include "ffi.h"
#include "stack.h"

namespace Zodiac
{
    typedef Instruction_Locator Instruction_Pointer;

    enum Interp_Flag : uint64_t
    {
        INTERP_FLAG_NONE    = 0x0,
        INTERP_FLAG_ABORTED = 0x1,
    };

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

        uint64_t flags = INTERP_FLAG_NONE;
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

    void interpreter_execute_compiler_function(Interpreter *interp, Bytecode_Function *func,
                                               int64_t arg_count, Bytecode_Value *result_value);

    void *interpreter_load_lvalue(Interpreter *interp, Bytecode_Value *value);

    void interpreter_free(Interpreter *interp);

    void interp_store(AST_Type *type, void *dest_ptr, void *source_ptr);
    void interp_store_constant(void *dest, Const_Value val);

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

#define INTERPRETER_LOAD_OR_CREATE_LVALUE(interp, val, name) \
    void *name = nullptr; \
    switch (val->kind) { \
        default: assert(false && !"default"); \
        case Bytecode_Value_Kind::ALLOCL: { \
            void *tmp = &interp->stack[interp->frame_pointer + val->allocl.byte_offset_from_fp]; \
            name = &tmp; \
            break; \
        } \
        case Bytecode_Value_Kind::PARAM: { \
            void *tmp = &interp->stack[interp->frame_pointer + \
                                       val->parameter.byte_offset_from_fp]; \
            name = &tmp; \
            break; \
        } \
        case Bytecode_Value_Kind::GLOBAL: { \
            void *tmp = &interp->global_data[val->global.byte_offset]; \
            name = &tmp; \
            break; \
        } \
        case Bytecode_Value_Kind::TEMP: \
        case Bytecode_Value_Kind::INTEGER_LITERAL: \
        case Bytecode_Value_Kind::BOOL_LITERAL: \
        case Bytecode_Value_Kind::FLOAT_LITERAL: \
        case Bytecode_Value_Kind::NULL_LITERAL: \
        case Bytecode_Value_Kind::STRING_LITERAL: { \
            name = interpreter_load_lvalue(interp, val); \
            break;                                     \
        } \
    }

}
