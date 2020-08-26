#include "const_interpreter.h"

#include "builtin.h"

namespace Zodiac
{

    Const_Value const_interpret_expression(AST_Expression *expr)
    {
        assert(expr);
        assert(expr->is_const);

        switch (expr->kind)
        {
            case AST_Expression_Kind::INVALID: assert(false);
            case AST_Expression_Kind::IDENTIFIER: assert(false);
            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);
            case AST_Expression_Kind::DOT: assert(false);
            case AST_Expression_Kind::BINARY: assert(false);
            case AST_Expression_Kind::UNARY: assert(false);
            case AST_Expression_Kind::CALL: assert(false);
            case AST_Expression_Kind::ADDROF: assert(false);
            case AST_Expression_Kind::COMPOUND: assert(false);

            case AST_Expression_Kind::SUBSCRIPT: assert(false);

            case AST_Expression_Kind::CAST: assert(false);

            case AST_Expression_Kind::NUMBER_LITERAL:
            {
                assert(expr->type == Builtin::type_s64);
                Const_Value result = {};
                result.type = expr->type;
                result.s64 = expr->number_literal.s64;
                return result;
                break;
            }

            case AST_Expression_Kind::STRING_LITERAL: assert(false);
        }

        assert(false);
        return {};
    }
}
