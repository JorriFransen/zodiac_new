#include "const_interpreter.h"

#include "builtin.h"

namespace Zodiac
{

    Const_Value const_interpret_expression(AST_Expression *expr)
    {
        assert(expr);
        assert(expr->expr_flags & AST_EXPR_FLAG_CONST);

        switch (expr->kind)
        {
            case AST_Expression_Kind::INVALID: assert(false);
            case AST_Expression_Kind::IDENTIFIER: assert(false);
            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);

            case AST_Expression_Kind::DOT:
            {
                assert(expr->dot.child_decl);

                auto decl = expr->dot.child_decl;
                if (decl->kind == AST_Declaration_Kind::CONSTANT)
                {
                    return const_interpret_expression(decl->constant.init_expression);
                }
                else
                { 
                    assert(false);
                }
                break;
            }

            case AST_Expression_Kind::BINARY: 
            {
                return const_interpret_binary_expression(expr);
                break;
            }

            case AST_Expression_Kind::UNARY: assert(false);
            case AST_Expression_Kind::POST_FIX: assert(false);
            case AST_Expression_Kind::PRE_FIX: assert(false);
            case AST_Expression_Kind::CALL: assert(false);
            case AST_Expression_Kind::ADDROF: assert(false);
            case AST_Expression_Kind::COMPOUND: assert(false);

            case AST_Expression_Kind::SUBSCRIPT: assert(false);

            case AST_Expression_Kind::CAST: assert(false);

            case AST_Expression_Kind::INTEGER_LITERAL:
            {
                Const_Value result = {};
                result.type = expr->type;

                if (expr->type->kind == AST_Type_Kind::ENUM)
                {
#ifndef NDEBUG
                    auto base_type = expr->type->enum_type.base_type;
                    assert(base_type->kind == AST_Type_Kind::INTEGER);
#endif
                    result.integer = expr->integer_literal;
                }
                else
                {
                    assert(expr->type == Builtin::type_s64);
                    result.integer = expr->integer_literal;
                }

                return result;
                break;
            }

            case AST_Expression_Kind::FLOAT_LITERAL: assert(false);
            case AST_Expression_Kind::STRING_LITERAL: assert(false);
            case AST_Expression_Kind::CHAR_LITERAL: assert(false);
            case AST_Expression_Kind::BOOL_LITERAL: assert(false);
            case AST_Expression_Kind::RANGE: assert(false);
        }

        assert(false);
        return {};
    }

    Const_Value const_interpret_binary_expression(AST_Expression *expr)
    {
        assert(expr->kind == AST_Expression_Kind::BINARY);

        auto lhs_val = const_interpret_expression(expr->binary.lhs);
        auto rhs_val = const_interpret_expression(expr->binary.rhs);

        assert(lhs_val.type == rhs_val.type);
        auto type = lhs_val.type;

        assert(type->kind == AST_Type_Kind::INTEGER);
        bool sign = type->integer.sign;

        Const_Value result = {};
        result.type = type;

        switch (expr->binary.op)
        {
            case Binary_Operator::BINOP_INVALID: assert(false);
            case Binary_Operator::BINOP_EQ: assert(false);
            case Binary_Operator::BINOP_NEQ: assert(false);
            case Binary_Operator::BINOP_LT: assert(false);
            case Binary_Operator::BINOP_LTEQ: assert(false);
            case Binary_Operator::BINOP_GT: assert(false);
            case Binary_Operator::BINOP_GTEQ: assert(false);

            case Binary_Operator::BINOP_ADD:
            {
                switch (type->bit_size)
                {
                    case 64:
                    {
                        if (sign)
                        {
                            result.integer.s64 = lhs_val.integer.s64 + rhs_val.integer.s64;
                        }
                        else
                        {
                            assert(false);
                        }
                        break;
                    }

                    default: assert(false); 
                }
                break; 
            }

            case Binary_Operator::BINOP_SUB: assert(false);
            case Binary_Operator::BINOP_REMAINDER: assert(false);
            case Binary_Operator::BINOP_MUL: assert(false);
            case Binary_Operator::BINOP_DIV: assert(false);
        }

        return result;
    }

}
