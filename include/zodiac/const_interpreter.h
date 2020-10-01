#pragma once

#include "ast.h"

namespace Zodiac
{
    struct Const_Value
    {
        AST_Type *type = nullptr;

        Integer_Literal integer = {};
    };

    Const_Value const_interpret_expression(AST_Expression *expr);
    Const_Value const_interpret_binary_expression(AST_Expression *expr);
}
