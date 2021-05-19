#pragma once

#include "allocator.h"
#include "bc.h"
#include "build_data.h"

namespace Zodiac
{
    struct Interp_Instruction_Pointer
    {
        int64_t index = 0;
        BC_Block *block = nullptr;
    };

    struct Interp_Stack_Frame
    {
        BC_Function *function = nullptr;
        Interp_Instruction_Pointer ip = {};
        int64_t first_arg_index = 0;
        int64_t first_temp_index = 0;
        int64_t first_alloc_index = 0;
        int64_t result_index = -1;
    };

    struct Interpreter_Value
    {
        AST_Type *type = nullptr;

        union
        {
            Integer_Literal integer_literal = {};
            const char *string_literal;
            void *pointer;
            bool boolean_literal;
        };
    };

    enum class Interp_LValue_Kind
    {
        INVALID,
        TEMP,
        ALLOCL,
    };

    struct Interpreter_LValue
    {
        Interp_LValue_Kind kind = Interp_LValue_Kind::INVALID;
        AST_Type *type = nullptr;
        int64_t index = 0;
    };

    struct Interpreter
    {
        Allocator *allocator = nullptr;
        Build_Data *build_data = nullptr;

        Stack<Interpreter_Value> temp_stack = {};
        Stack<Interpreter_Value> alloc_stack = {};
        Stack<Interpreter_Value> arg_stack = {};
        Stack<Interp_Stack_Frame> frames = {};

        int64_t exit_code = 0;
    };

    Interpreter interpreter_create(Allocator *allocator, Build_Data *build_data);

    void interpreter_start(Interpreter *interp, BC_Function *entry_func);

    Interpreter_Value interp_load_value(Interpreter *interp, BC_Value *bc_val);
    Interpreter_LValue interp_load_lvalue(Interpreter *interp, BC_Value *bc_val);

    Interpreter_LValue interp_push_temp(Interpreter *interp, BC_Value *bc_val);

    void interp_store(Interpreter *interp, Interpreter_Value source, Interpreter_LValue dest);
    void interp_store(Interpreter *interp, void *source_ptr, AST_Type *source_type,
                      Interpreter_LValue dest);

    // void interpreter_start(Interpreter *interp, BC_Function *entry_func,
    //                        int64_t global_data_size, Array<BC_Global_Info> global_info,
    //                        Array<BC_Function *> foreign_functions);

    // void interpreter_initialize_globals(Interpreter *interp, int64_t global_data_size,
    //                                     Array<BC_Global_Info> global_info);
    // void interpreter_initialize_foreigns(Interpreter *interp,
    //                                      Array<BC_Function *> foreign_functions);

    // void interpreter_execute_foreign_function(Interpreter *interp, BC_Function *func,
    //                                           int64_t arg_count, BC_Value *result_value);
    // void interpreter_push_foreign_arg(Interpreter *interp, uint8_t *arg_ptr, AST_Type *type);

    // void interpreter_execute_compiler_function(Interpreter *interp, BC_Function *func,
    //                                            int64_t arg_count, BC_Value *result_value);

    // void *interpreter_load_lvalue(Interpreter *interp, BC_Value *value);

    // void interpreter_free(Interpreter *interp);

    // void interp_store(AST_Type *type, void *dest_ptr, void *source_ptr);
    // void interp_store_constant(void *dest, Const_Value val);

    // void *interp_stack_ptr(Interpreter *interp, int64_t index, int64_t byte_size);
}
