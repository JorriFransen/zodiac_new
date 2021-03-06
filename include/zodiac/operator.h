#pragma once

namespace Zodiac
{
    enum Binary_Operator
    {
        BINOP_INVALID,

        BINOP_FIRST_CMP,
        BINOP_EQ = BINOP_FIRST_CMP,
        BINOP_NEQ,
        BINOP_LT,
        BINOP_LTEQ,
        BINOP_GT,
        BINOP_GTEQ,
        BINOP_LAST_CMP = BINOP_GTEQ,

        BINOP_ADD,
        BINOP_SUB,

        BINOP_REMAINDER,
        BINOP_MUL,
        BINOP_DIV,
        BINOP_LSHIFT,
        BINOP_RSHIFT,

        BINOP_AND,
        BINOP_OR,
    };

    enum Unary_Operator
    {
        UNOP_INVALID,

        UNOP_DEREF,

        UNOP_MINUS,
        UNOP_NOT,
    };

    bool binop_is_cmp(Binary_Operator op);
}
