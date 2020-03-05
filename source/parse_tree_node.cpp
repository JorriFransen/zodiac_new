#include "parse_tree_node.h"

namespace Zodiac
{

void init_parse_tree_node(Parse_Tree_Node* ptn)
{
    assert(ptn);
}

Identifier_Parse_Tree_Node* new_identifier_parse_tree_node(Allocator* allocator, const Atom& atom)
{
    Identifier_Parse_Tree_Node* result = new_parse_tree_node<Identifier_Parse_Tree_Node>(allocator);
    result->identifier = atom;
    return result;
}

Parameter_Parse_Tree_Node* new_parameter_parse_tree_node(
    Allocator* allocator
)
{
    assert(allocator);
    assert(false);
}


Function_Prototype_Parse_Tree_Node* new_function_prototype_parse_tree_node(
    Allocator* allocator,
    Array<Parameter_Parse_Tree_Node*> parameters,
    Expression_Parse_Tree_Node* return_type_expr
)
{
    Function_Prototype_Parse_Tree_Node* result =
        new_parse_tree_node<Function_Prototype_Parse_Tree_Node>(allocator);
    result->parameters = parameters;
    result->return_type_expression = return_type_expr;
    return result;
}

Function_Body_Parse_Tree_Node* new_function_body_parse_tree_node(
    Allocator* allocator,
    Array<Statement_Parse_Tree_Node*> statements
)
{
    auto result = new_parse_tree_node<Function_Body_Parse_Tree_Node>(allocator);
    result->statements = statements;
    return result;
}

Function_Declaration_Parse_Tree_Node* new_function_declaration_parse_tree_node(
    Allocator* allocator,
    Identifier_Parse_Tree_Node* identifier,
    Function_Prototype_Parse_Tree_Node* prototype,
    Function_Body_Parse_Tree_Node* body
)
{
    assert(prototype);

    auto result = new_parse_tree_node<Function_Declaration_Parse_Tree_Node>(allocator);
    assert(result);

    result->identifier = identifier;
    result->prototype = prototype;
    result->body = body;

    return result;
}

Mutable_Declaration_Parse_Tree_Node* new_mutable_declaration_parse_tree_node(
    Allocator* allocator,
    Identifier_Parse_Tree_Node* identifier,
    Expression_Parse_Tree_Node* init_expression
)
{
    auto result = new_parse_tree_node<Mutable_Declaration_Parse_Tree_Node>(allocator);
    result->identifier = identifier;
    result->init_expression = init_expression;
    return result;
}

Declaration_Statement_Parse_Tree_Node* new_declaration_statement_parse_tree_node(
    Allocator* allocator,
    Declaration_Parse_Tree_Node* declaration
)
{
    auto result = new_parse_tree_node<Declaration_Statement_Parse_Tree_Node>(allocator);
    result->declaration = declaration;
    return result;
}

Struct_Declaration_Parse_Tree_Node*
new_struct_declaration_parse_tree_node(Allocator* allocator,
                                        Identifier_Parse_Tree_Node* identifier,
                                        Array<Declaration_Parse_Tree_Node*> members)
{
    auto result = new_parse_tree_node<Struct_Declaration_Parse_Tree_Node>(allocator);
    result->identifier = identifier;
    result->member_declarations = members;
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
    Identifier_Parse_Tree_Node* identifier,
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
    Identifier_Parse_Tree_Node* identifier
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

void Function_Prototype_Parse_Tree_Node::print(uint64_t indent /*= 0*/)
{
    print_indent(indent);
    printf("FUNC_PROTO\n");
    print_indent(indent);
    printf("  PARAMS\n");
    for (int64_t i = 0; i < parameters.count; i++)
    {
        parameters[i]->print(indent + 4);
    }

    if (return_type_expression)
    {
        print_indent(indent);
        printf("  RETURN_TYPE\n");
        return_type_expression->print(indent + 4);
    }
}

void Function_Body_Parse_Tree_Node::print(uint64_t indent /*= 0*/)
{
    print_indent(indent);
    printf("FUNC_BODY\n");
    for (int64_t i = 0; i < statements.count; i++)
    {
        statements[i]->print(indent + 2);
    }
}

void Expression_Statement_Parse_Tree_Node::print(uint64_t indent /*= 0*/)
{
    expression->print(indent + 2);
}

void Mutable_Declaration_Parse_Tree_Node::print(uint64_t indent /*= 0*/)
{
    print_indent(indent);
    printf("MUTABLE: \"%s\"\n", identifier->identifier.data);

    if (init_expression)
    {
        print_indent(indent);
        printf("  INIT_EXPR:\n");
        init_expression->print(indent + 4);
    }
}

void Return_Statement_Parse_Tree_Node::print(uint64_t indent /*= 0*/)
{
    print_indent(indent);
    printf("RETURN: \n");
    expression->print(indent + 2);
}

}
