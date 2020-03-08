#include "parse_tree_node.h"

namespace Zodiac
{

PTN_Kind Identifier_PTN::_kind = PTN_Kind::IDENTIFIER;
PTN_Kind Function_Proto_PTN::_kind = PTN_Kind::FUNCTION_PROTO;
PTN_Kind Statement_PTN::_kind = PTN_Kind::STATEMENT;
PTN_Kind Declaration_PTN::_kind = PTN_Kind::DECLARATION;

void init_ptn(PTN* ptn, PTN_Kind kind)
{
    assert(ptn);
    ptn->kind = kind;
}

void init_parse_tree_node(Parse_Tree_Node* ptn)
{
    assert(ptn);
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
                                            Expression_Parse_Tree_Node* expr)
{
    auto result = new_statement(allocator, Statement_PTN_Kind::EXPRESSION);
    result->expression = expr;
    return result;
}

Statement_PTN* new_declaration_statement_ptn(Allocator* allocator,
                                             Declaration_PTN* decl)
{
    auto result = new_statement(allocator, Statement_PTN_Kind::DECLARATION);
    result->declaration = decl;
    return result;
}

Statement_PTN* new_return_statement_ptn(Allocator* allocator, Expression_Parse_Tree_Node* expr)
{
    auto result = new_statement(allocator, Statement_PTN_Kind::RETURN);
    result->return_stmt.expression = expr;
    return result;
}

Function_Proto_PTN* new_function_prototype_parse_tree_node(
    Allocator* allocator,
    Array<Parameter_PTN*> parameters,
    Expression_Parse_Tree_Node* return_type_expr
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
                                              Expression_Parse_Tree_Node* init_expression)
{
    auto result = new_ptn<Declaration_PTN>(allocator);
    result->kind = Declaration_PTN_Kind::VARIABLE;
    result->identifier = identifier;
    result->variable.init_expression = init_expression;
    return result;
}

Declaration_Statement_Parse_Tree_Node* new_declaration_statement_parse_tree_node(
    Allocator* allocator,
    Declaration_PTN* declaration
)
{
    auto result = new_parse_tree_node<Declaration_Statement_Parse_Tree_Node>(allocator);
    result->declaration = declaration;
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

Expression_Statement_Parse_Tree_Node* new_expression_statement_parse_tree_node(
    Allocator* allocator,
    Expression_Parse_Tree_Node* expression
)
{
    auto result = new_parse_tree_node<Expression_Statement_Parse_Tree_Node>(allocator);
    result->expression = expression;
    return result;
}

Return_Statement_Parse_Tree_Node* new_return_statement_parse_tree_node(
    Allocator* allocator,
    Expression_Parse_Tree_Node* return_expression
)
{
    auto result = new_parse_tree_node<Return_Statement_Parse_Tree_Node>(allocator);
    result->expression = return_expression;
    return result;
}

Expression_List_Parse_Tree_Node* new_expression_list_parse_tree_node(
    Allocator* allocator,
    Array<Expression_Parse_Tree_Node*> expressions
)
{
    auto result = new_parse_tree_node<Expression_List_Parse_Tree_Node>(allocator);
    result->expressions = expressions;
    return result;
}

Call_Expression_Parse_Tree_Node* new_call_expression_parse_tree_node(
    Allocator* allocator,
    bool is_builtin,
    Identifier_PTN* identifier,
    Expression_List_Parse_Tree_Node* arg_list
)
{
    auto result = new_parse_tree_node<Call_Expression_Parse_Tree_Node>(allocator);
    result->is_builtin = is_builtin;
    result->identifier = identifier;
    result->arg_list = arg_list;
    return result;
}

Identifier_Expression_Parse_Tree_Node* new_identifier_expression_parse_tree_node(
    Allocator* allocator,
    Identifier_PTN* identifier
)
{
    auto result = new_parse_tree_node<Identifier_Expression_Parse_Tree_Node>(allocator);
    result->identifier = identifier;
    return result;
}

Binary_Expression_Parse_Tree_Node* new_binary_expression_parse_tree_node(
    Allocator* allocator,
    Binary_Operator op,
    Expression_Parse_Tree_Node* lhs,
    Expression_Parse_Tree_Node* rhs
)
{
    auto result = new_parse_tree_node<Binary_Expression_Parse_Tree_Node>(allocator);
    result->op = op;
    result->lhs = lhs;
    result->rhs = rhs;

    return result;
}

Number_Literal_Expression_Parse_Tree_Node* new_number_literal_expression_parse_tree_node(
    Allocator* allocator,
    Atom atom
)
{
    auto result = new_parse_tree_node<Number_Literal_Expression_Parse_Tree_Node>(allocator);
    if (atom.data[0] == '-')
    {
        result->value.s64 = atom_to_s64(atom);
    }
    else
    {
        result->value.u64 = atom_to_u64(atom);
    }

    return result;
}

void Expression_Statement_Parse_Tree_Node::print(uint64_t indent /*= 0*/)
{
    expression->print(indent + 2);
}

// void Declaration_PTN::print(uint64_t indent /*= 0*/)
// {
//     print_indent(indent);
//     printf("MUTABLE: \"%s\"\n", identifier->atom.data);

//     if (init_expression)
//     {
//         print_indent(indent);
//         printf("  INIT_EXPR:\n");
//         init_expression->print(indent + 4);
//     }
// }

void Return_Statement_Parse_Tree_Node::print(uint64_t indent /*= 0*/)
{
    print_indent(indent);
    printf("RETURN: \n");
    expression->print(indent + 2);
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
    print_indent(indent);

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
            statement->expression->print(indent);
            break;
        }

        case Statement_PTN_Kind::RETURN:
        {
            print_indent(indent);
            printf("RETURN:\n");
            if (statement->return_stmt.expression)
            {
                statement->return_stmt.expression->print(indent + 2);
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
            printf("VARIABLE: %s\n", decl->identifier->atom.data);
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

}
