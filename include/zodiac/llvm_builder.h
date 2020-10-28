#pragma once

#include "build_data.h"
#include "bytecode.h"

namespace Zodiac
{
    enum class Zodiac_Target_Platform
    {
        INVALID,
        LINUX,
        WINDOWS,
    };

    struct LLVM_Builder 
    {
        Build_Data *build_data = nullptr;
        Zodiac_Target_Platform target_platform = Zodiac_Target_Platform::INVALID;
    };

    LLVM_Builder llvm_builder_create(Allocator *allocator, Build_Data *build_data);

    void llvm_register_function(LLVM_Builder *builder, Bytecode_Function *bc_func);
    void llvm_emit_function(LLVM_Builder *builder, Bytecode_Function *bc_func);
    void llvm_emit_global(LLVM_Builder *builder, Bytecode_Value *bc_val);

    bool llvm_emit_binary(LLVM_Builder *builder, const char * out_file_name);
}
