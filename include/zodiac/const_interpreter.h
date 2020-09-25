#pragma once

#include "ast.h"

namespace Zodiac
{
    struct Const_Value
    {
        AST_Type *type = nullptr;

        int64_t s64 = 0;
    };

    Const_Value const_interpret_expression(AST_Expression *expr);
    Const_Value const_interpret_binary_expression(AST_Expression *expr);
}
