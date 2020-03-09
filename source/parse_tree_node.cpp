#include "parse_tree_node.h"

namespace Zodiac
{

PTN_Kind Identifier_PTN::_kind = PTN_Kind::IDENTIFIER;
PTN_Kind Function_Proto_PTN::_kind = PTN_Kind::FUNCTION_PROTO;
PTN_Kind Statement_PTN::_kind = PTN_Kind::STATEMENT;
PTN_Kind Declaration_PTN::_kind = PTN_Kind::DECLARATION;
PTN_Kind Expression_List_PTN::_kind = PTN_Kind::EXPRESSION_LIST;
PTN_Kind Expression_PTN::_kind = PTN_Kind::EXPRESSION;

void init_ptn(PTN* ptn, PTN_Kind kind)
{
    assert(ptn);
    ptn->kind = kind;
}

Statement_PTN* new_statement(Allocator* allocator, Statement_PTN_Kind kind)
{
    auto result = new_ptn<Statement_PTN>(allocator);
    result->kind = kind;
    return result;
}

Identifier_PTN* new_identifier_ptn(Allocator* allocator, const Atom& atom)
{
    Identifier_PTN* result = new_ptn<Identifier_PTN>(allocator);
    result->atom = atom;
    return result;
}

Statement_PTN* new_block_statement_ptn(Allocator* allocator, Array<Statement_PTN*> statements)
{
    auto result = new_statement(allocator, Statement_PTN_Kind::BLOCK);
    result->block.statements = statements;
    return result;
}

Statement_PTN* new_expression_statement_ptn(Allocator* allocator,
                                            Expression_PTN* expr)
{
    auto result = new_statement(allocator, Statement_PTN_Kind::EXPRESSION);
    result->expression = expr;
    return result;
}

Statement_PTN* new_declaration_statement_ptn(Allocator* allocator, Declaration_PTN* decl)
{
    auto result = new_statement(allocator, Statement_PTN_Kind::DECLARATION);
    result->declaration = decl;
    return result;
}

Statement_PTN* new_return_statement_ptn(Allocator* allocator, Expression_PTN* expr)
{
    auto result = new_statement(allocator, Statement_PTN_Kind::RETURN);
    result->return_stmt.expression = expr;
    return result;
}

Function_Proto_PTN* new_function_prototype_parse_tree_node(
    Allocator* allocator,
    Array<Parameter_PTN*> parameters,
    Expression_PTN* return_type_expr
)
{
    Function_Proto_PTN* result = new_ptn<Function_Proto_PTN>(allocator);
    result->parameters = parameters;
    result->return_type_expression = return_type_expr;
    return result;
}

Declaration_PTN* new_function_declaration_ptn(Allocator* allocator, Identifier_PTN* identifier,
                                              Function_Proto_PTN* prototype, Statement_PTN* body)
{
    assert(prototype);
    if (body) assert(body->kind == Statement_PTN_Kind::BLOCK);

    auto result = new_ptn<Declaration_PTN>(allocator);
    assert(result);

    result->kind = Declaration_PTN_Kind::FUNCTION;
    result->identifier = identifier;
    result->function.prototype = prototype;
    result->function.body = body;

    return result;
}

Declaration_PTN* new_variable_declaration_ptn(Allocator* allocator, Identifier_PTN* identifier,
                                              Expression_PTN* type_expression,
                                              Expression_PTN* init_expression)
{
    auto result = new_ptn<Declaration_PTN>(allocator);
    result->kind = Declaration_PTN_Kind::VARIABLE;
    result->identifier = identifier;
    result->variable.type_expression = type_expression;
    result->variable.init_expression = init_expression;
    return result;
}

Declaration_PTN* new_struct_declaration_ptn(Allocator* allocator, Identifier_PTN* identifier,
                                            Array<Declaration_PTN*> members)
{
    auto result = new_ptn<Declaration_PTN>(allocator);
    result->kind = Declaration_PTN_Kind::STRUCT;
    result->identifier = identifier;
    result->structure.member_declarations = members;
    return result;
}

Expression_List_PTN* new_expression_list_ptn(Allocator* allocator,
                                             Array<Expression_PTN*> expressions
)
{
    auto result = new_ptn<Expression_List_PTN>(allocator);
    result->expressions = expressions;
    return result;
}

Expression_PTN* new_call_expression_ptn(Allocator* allocator, bool is_builtin,
                                        Identifier_PTN* identifier, Expression_List_PTN* arg_list)
{
    auto result = new_ptn<Expression_PTN>(allocator);
    result->kind = Expression_PTN_Kind::CALL;
    result->call.is_builtin = is_builtin;
    result->call.identifier = identifier;
    result->call.arg_list = arg_list;
    return result;
}

Expression_PTN* new_identifier_expression_ptn(Allocator* allocator, Identifier_PTN* identifier)
{
    auto result = new_ptn<Expression_PTN>(allocator);
    result->kind = Expression_PTN_Kind::IDENTIFIER;
    result->identifier = identifier;
    return result;
}

Expression_PTN* new_binary_expression_ptn(Allocator* allocator, Binary_Operator op,
                                          Expression_PTN* lhs, Expression_PTN* rhs)
{
    auto result = new_ptn<Expression_PTN>(allocator);
    result->kind = Expression_PTN_Kind::BINARY;
    result->binary.op = op;
    result->binary.lhs = lhs;
    result->binary.rhs = rhs;

    return result;
}

Expression_PTN* new_number_literal_expression_ptn(Allocator* allocator, Atom atom)
{
    auto result = new_ptn<Expression_PTN>(allocator);
    result->kind = Expression_PTN_Kind::NUMBER_LITERAL;
    if (atom.data[0] == '-')
    {
        result->number_literal.value.s64 = atom_to_s64(atom);
    }
    else
    {
        result->number_literal.value.u64 = atom_to_u64(atom);
    }

    return result;
}

void print_indent(uint64_t indent)
{
    for (uint64_t i = 0; i < indent; i++)
    {
        printf(" ");
    }
}

void print_ptn(PTN* ptn, uint64_t indent)
{
    switch (ptn->kind)
    {
        case PTN_Kind::INVALID:
        {
            assert(false);
            break;
        }

        case PTN_Kind::IDENTIFIER:
        {
            assert(false);
            break;
        }

        case PTN_Kind::FUNCTION_PROTO:
        {
            auto _this = (Function_Proto_PTN*)ptn;

            print_indent(indent);
            printf("FUNC_PROTO:\n");
            if (_this->parameters.count)
            {
                print_indent(indent);
                printf("  PARAMS:\n");
                for (int64_t i = 0; i < _this->parameters.count; i++)
                {
                    print_ptn(&_this->parameters[i]->self, indent + 4);
                }
            }
            break;
        }

        case PTN_Kind::PARAMETER:
        {
            assert(false);
            break;
        }

        case PTN_Kind::EXPRESSION_LIST:
        {
            auto _this = (Expression_List_PTN*)ptn;
            for (int64_t i = 0 ; i < _this->expressions.count; i++)
            {
                print_expression_ptn(_this->expressions[i], indent);
            }
            break;
        }

        case PTN_Kind::DECLARATION:
        {
            print_declaration_ptn((Declaration_PTN*)ptn, indent);
            break;
        }

        case PTN_Kind::STATEMENT:
        {
            print_statement_ptn((Statement_PTN*)ptn, indent);
            break;
        }

        case PTN_Kind::EXPRESSION:
        {
            assert(false);
            break;
        }
    }
}

void print_statement_ptn(Statement_PTN* statement, uint64_t indent)
{
    switch (statement->kind)
    {
        case Statement_PTN_Kind::INVALID: assert(false);

        case Statement_PTN_Kind::BLOCK:
        {
            print_indent(indent);
            printf("{\n");

            for (int64_t i = 0; i < statement->block.statements.count; i++)
            {
                print_statement_ptn(statement->block.statements[i], indent + 2);
            }

            print_indent(indent);
            printf("}\n");
            break;
        }

        case Statement_PTN_Kind::DECLARATION:
        {
            print_declaration_ptn(statement->declaration, indent);
            break;
        }

        case Statement_PTN_Kind::EXPRESSION:
        {
            print_expression_ptn(statement->expression, indent);
            break;
        }

        case Statement_PTN_Kind::RETURN:
        {
            print_indent(indent);
            printf("RETURN:\n");
            if (statement->return_stmt.expression)
            {
                print_expression_ptn(statement->return_stmt.expression, indent + 2);
            }
        }
    }
}

void print_declaration_ptn(Declaration_PTN* decl, uint64_t indent)
{
    switch (decl->kind)
    {
        case Declaration_PTN_Kind::INVALID: assert(false);

        case Declaration_PTN_Kind::FUNCTION:
        {
            print_indent(indent);
            printf("FUNCTION: \"%s\"\n", decl->identifier->atom.data);
            print_ptn(&decl->function.prototype->self, indent + 2);
            if (decl->function.body)
            {
                print_statement_ptn(decl->function.body, indent + 2);
            }
            printf("\n");
            break;
        }

        case Declaration_PTN_Kind::VARIABLE:
        {
            print_indent(indent);
            printf("VARIABLE: %s : ", decl->identifier->atom.data);
            if (decl->variable.type_expression)
            {
                print_expression_ptn(decl->variable.type_expression, 0);
            }
            if (decl->variable.init_expression)
            {
                printf(" = ");
                print_expression_ptn(decl->variable.init_expression, 0);
            }
            break;
        }

        case Declaration_PTN_Kind::STRUCT:
        {
            print_indent(indent);
            printf("STRUCT: \"%s\"\n", decl->identifier->atom.data);
            print_indent(indent);
            printf("{\n");
            for (int64_t i = 0; i < decl->structure.member_declarations.count; i++)
            {
                print_declaration_ptn(decl->structure.member_declarations[i], indent + 2);
            }
            print_indent(indent);
            printf("}\n");
            break;
        }
    }
}

void print_expression_ptn(Expression_PTN* expression, uint64_t indent)
{
    switch (expression->kind)
    {
        case Expression_PTN_Kind::INVALID: assert(false);

        case Expression_PTN_Kind::CALL:
        {
            print_indent(indent);
            printf("CALL: \"%s\"\n", expression->call.identifier->atom.data);
            if (expression->call.arg_list)
            {
                print_indent(indent);
                printf("ARGS:\n");
                print_ptn(&expression->call.arg_list->self, indent + 2);
            }
            break;
        }

        case Expression_PTN_Kind::IDENTIFIER:
        {
            print_indent(indent);
            printf("IDENT_EXPR: \"%s\"\n", expression->identifier->atom.data);
            break;
        }

        case Expression_PTN_Kind::BINARY:
        {
            print_indent(indent);
            printf("BINARY:\n");
            print_indent(indent);
            printf("  LHS: ");
            print_expression_ptn(expression->binary.lhs, 0);
            print_indent(indent + 2);
            switch (expression->binary.op)
            {
                case BINOP_INVALID: assert(false);

                case BINOP_ADD:
                {
                    printf("+");
                    break;
                }

                case BINOP_SUB:
                {
                    printf("-");
                    break;
                }
            }
            printf("\n");
            print_indent(indent);
            printf("  RHS: ");
            print_expression_ptn(expression->binary.rhs, 0);
            break;
        }

        case Expression_PTN_Kind::NUMBER_LITERAL:
        {
            print_indent(indent);
            printf("NUMBER: %ld\n", expression->number_literal.value.s64);
            break;
        }

    }
}

}
