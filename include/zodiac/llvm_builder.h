#pragma once

#include "allocator.h"
#include "bytecode.h"

namespace Zodiac
{
    struct LLVM_Builder
    {

    };

    void llvm_builder_init(Allocator *allocator, LLVM_Builder *llvm_builder);

    void llvm_emit_function(LLVM_Builder *builder, Bytecode_Function *bc_func);
}
