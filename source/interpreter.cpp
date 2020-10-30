
#include "interpreter.h"

namespace Zodiac
{
    Interpreter interpreter_create(Allocator *allocator, Build_Data *build_data)
    {
        Interpreter result = {};

        result.allocator = allocator;
        result.build_data = build_data;

        return result;
    }

    void interpreter_start(Interpreter *interp, Bytecode_Function *entry_func)
    {
        assert(false);
    }

    void interpreter_free(Interpreter *interp)
    {
        assert(false);
    }
}
