#pragma once

#include "ast.h"

namespace Zodiac
{
    Const_Value const_interpret_expression(AST_Expression *expr);
    Const_Value const_interpret_binary_expression(AST_Expression *expr);
}
