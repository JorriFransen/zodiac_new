#pragma once

#include "allocator.h"
#include "bc.h"
#include "build_data.h"

namespace Zodiac
{
    struct Interpreter
    {
        Build_Data *build_data = nullptr;

        int64_t exit_code = 0;
    };

    Interpreter interpreter_create(Allocator *allocator, Build_Data *build_data);

    void interpreter_start(Interpreter *interp, BC_Function *entry_func);

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
