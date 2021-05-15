#include "interpreter.h"

namespace Zodiac
{
    Interpreter interpreter_create(Allocator *allocator, Build_Data *build_data)
    {
        Interpreter result = {
            .build_data = build_data,
            .exit_code = 0,
        };

        return result;
    }

    void interpreter_start(Interpreter *interp, BC_Function *entry_func)
    {
        assert(false);
    }
}
