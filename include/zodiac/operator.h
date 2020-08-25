#pragma once

namespace Zodiac
{
    enum Binary_Operator
    {
        BINOP_INVALID,

        BINOP_EQ,
        BINOP_LT,
        BINOP_LTEQ,
        BINOP_GT,
        BINOP_GTEQ,

        BINOP_ADD,
        BINOP_SUB,

        BINOP_REMAINDER,
        BINOP_MUL,
        BINOP_DIV,
    };

    enum Unary_Operator
    {
        UNOP_INVALID,

        UNOP_DEREF,
    };
}
