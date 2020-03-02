#pragma once

#include "array.h"
#include "atom.h"
#include "operator.h"

namespace Zodiac
{

struct Parse_Tree_Node
{
    
};

struct Statement_Parse_Tree_Node;
struct Expression_Parse_Tree_Node;

struct Identifier_Parse_Tree_Node : public Parse_Tree_Node
{
    Atom identifier = {};
};

struct Parameter_Parse_Tree_Node : public Parse_Tree_Node
{
    
};

struct Function_Prototype_Parse_Tree_Node : public Parse_Tree_Node
{
    Array<Parameter_Parse_Tree_Node*> parameters = {};
    Expression_Parse_Tree_Node* return_type_expression = nullptr;
};

struct Function_Body_Parse_Tree_Node : public Parse_Tree_Node
{
    Array<Statement_Parse_Tree_Node*> statements = {};
};

struct Declaration_Parse_Tree_Node : public Parse_Tree_Node
{
    Identifier_Parse_Tree_Node* identifier = nullptr;
};

struct Function_Declaration_Parse_Tree_Node : public Declaration_Parse_Tree_Node
{
    Function_Prototype_Parse_Tree_Node* prototype = nullptr;
    Function_Body_Parse_Tree_Node* body = nullptr;
};

struct Mutable_Declaration_Parse_Tree_Node : public Declaration_Parse_Tree_Node
{
    Expression_Parse_Tree_Node* init_expression = nullptr;
};

struct Statement_Parse_Tree_Node : public Parse_Tree_Node
{

};

struct Declaration_Statement_Parse_Tree_Node : public Statement_Parse_Tree_Node
{
    Declaration_Parse_Tree_Node* declaration = nullptr;
};

struct Expression_Statement_Parse_Tree_Node : public Statement_Parse_Tree_Node
{
    Expression_Parse_Tree_Node* expression = nullptr;
};

struct Return_Statement_Parse_Tree_Node : public Statement_Parse_Tree_Node
{
    Expression_Parse_Tree_Node* expression = nullptr;
};

struct Expression_Parse_Tree_Node : public Parse_Tree_Node
{

};

struct Expression_List_Parse_Tree_Node : public Parse_Tree_Node
{
    Array<Expression_Parse_Tree_Node*> expressions = {};
};

struct Call_Expression_Parse_Tree_Node : public Expression_Parse_Tree_Node
{
    bool is_builtin = false;
    Identifier_Parse_Tree_Node* identifier = nullptr;
    Expression_List_Parse_Tree_Node* arg_list = nullptr;
};

struct Identifier_Expression_Parse_Tree_Node : public Expression_Parse_Tree_Node
{
    Identifier_Parse_Tree_Node* identifier = nullptr;
};

struct Binary_Expression_Parse_Tree_Node : public Expression_Parse_Tree_Node
{
    Binary_Operator op = BINOP_INVALID;
    Expression_Parse_Tree_Node* lhs = nullptr;
    Expression_Parse_Tree_Node* rhs = nullptr;
};

void init_parse_tree_node(Parse_Tree_Node* ptn);

template <typename T>
T* new_parse_tree_node(Allocator* allocator)
{
    T* result = alloc_type<T>(allocator);
    init_parse_tree_node(result);
    return result;
}

Identifier_Parse_Tree_Node* new_identifier_parse_tree_node(Allocator* allocator, const Atom& atom);

Parameter_Parse_Tree_Node* new_parameter_parse_tree_node(
    Allocator* allocator
);

Function_Prototype_Parse_Tree_Node* new_function_prototype_parse_tree_node(
    Allocator* allocator,
    Array<Parameter_Parse_Tree_Node*> parameters,
    Expression_Parse_Tree_Node* return_type_expr
);

Function_Body_Parse_Tree_Node* new_function_body_parse_tree_node(
    Allocator* allocator,
    Array<Statement_Parse_Tree_Node*> statements
);

Function_Declaration_Parse_Tree_Node* new_function_declaration_parse_tree_node(
    Allocator* allocator,
    Identifier_Parse_Tree_Node* identifier,
    Function_Prototype_Parse_Tree_Node* prototype,
    Function_Body_Parse_Tree_Node* body
);

Mutable_Declaration_Parse_Tree_Node* new_mutable_declaration_parse_tree_node(
    Allocator* allocator,
    Identifier_Parse_Tree_Node* identifier,
    Expression_Parse_Tree_Node* init_expression
);

Declaration_Statement_Parse_Tree_Node* new_declaration_statement_parse_tree_node(
    Allocator* allocator,
    Declaration_Parse_Tree_Node* declaration
);

Expression_Statement_Parse_Tree_Node* new_expression_statement_parse_tree_node(
    Allocator* allocator,
    Expression_Parse_Tree_Node* expression
);

Return_Statement_Parse_Tree_Node* new_return_statement_parse_tree_node(
    Allocator* allocator,
    Expression_Parse_Tree_Node* return_expression
);

Expression_List_Parse_Tree_Node* new_expression_list_parse_tree_node(
    Allocator* allocator,
    Array<Expression_Parse_Tree_Node*> expressions
);

Call_Expression_Parse_Tree_Node* new_call_expression_parse_tree_node(
    Allocator* allocator,
    bool is_builtin,
    Identifier_Parse_Tree_Node* identifier,
    Expression_List_Parse_Tree_Node* arg_list
);

Identifier_Expression_Parse_Tree_Node* new_identifier_expression_parse_tree_node(
    Allocator* allocator,
    Identifier_Parse_Tree_Node* identifier
);

Binary_Expression_Parse_Tree_Node* new_binary_expression_parse_tree_node(
    Allocator* allocator,
    Binary_Operator op,
    Expression_Parse_Tree_Node* lhs,
    Expression_Parse_Tree_Node* rhs
);

}
