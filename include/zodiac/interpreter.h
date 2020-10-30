#pragma once

#include "allocator.h"
#include "build_data.h"

namespace Zodiac
{
    struct Interpreter
    {
        Allocator *allocator = nullptr;
        Build_Data *build_data = nullptr;
    };

    Interpreter interpreter_create(Allocator *allocator, Build_Data *build_data);

    void interpreter_start(Interpreter *interp, Bytecode_Function *entry_func);

    void interpreter_free(Interpreter *interp);
}
