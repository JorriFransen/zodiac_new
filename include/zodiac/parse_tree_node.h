#pragma once

#include "array.h"
#include "atom.h"
#include "operator.h"

#include <cstdio>

namespace Zodiac
{

struct Parse_Tree_Node
{
    virtual void print(uint64_t indent = 0) = 0;
    void print_indent(uint64_t indent)
    {
        for (uint64_t i = 0; i < indent; i++)
        {
            printf(" ");
        }
    }
};

struct Statement_Parse_Tree_Node;
struct Expression_Parse_Tree_Node;

struct Identifier_Parse_Tree_Node : public Parse_Tree_Node
{
    Atom identifier = {};

    void print (uint64_t indent = 0)
    {
        print_indent(indent);
        printf("IDENTIFIER: %s\n", identifier.data);
    }
};

struct Parameter_Parse_Tree_Node : public Parse_Tree_Node
{
    
};

struct Function_Prototype_Parse_Tree_Node : public Parse_Tree_Node
{
    Array<Parameter_Parse_Tree_Node*> parameters = {};
    Expression_Parse_Tree_Node* return_type_expression = nullptr;

    void print(uint64_t indent = 0);

};

struct Function_Body_Parse_Tree_Node : public Parse_Tree_Node
{
    Array<Statement_Parse_Tree_Node*> statements = {};

    void print(uint64_t indent = 0);
};

struct Declaration_Parse_Tree_Node : public Parse_Tree_Node
{
    Identifier_Parse_Tree_Node* identifier = nullptr;
};

struct Function_Declaration_Parse_Tree_Node : public Declaration_Parse_Tree_Node
{
    Function_Prototype_Parse_Tree_Node* prototype = nullptr;
    Function_Body_Parse_Tree_Node* body = nullptr;

    void print(uint64_t indent = 0)
    {
        print_indent(indent);
        printf("FUNCTION: \"%s\"\n", identifier->identifier.data);
        prototype->print(indent + 2);
        body->print(indent + 2);
        printf("\n");
    }
};

struct Mutable_Declaration_Parse_Tree_Node : public Declaration_Parse_Tree_Node
{
    Expression_Parse_Tree_Node* init_expression = nullptr;

    void print(uint64_t indent = 0);
};

struct Statement_Parse_Tree_Node : public Parse_Tree_Node
{
};

struct Declaration_Statement_Parse_Tree_Node : public Statement_Parse_Tree_Node
{
    Declaration_Parse_Tree_Node* declaration = nullptr;

    void print(uint64_t ident = 0)
    {
        declaration->print(ident + 2);
    }
};

struct Expression_Statement_Parse_Tree_Node : public Statement_Parse_Tree_Node
{
    Expression_Parse_Tree_Node* expression = nullptr;

    void print(uint64_t indent = 0);
};

struct Return_Statement_Parse_Tree_Node : public Statement_Parse_Tree_Node
{
    Expression_Parse_Tree_Node* expression = nullptr;

    void print(uint64_t indent = 0);
};

struct Expression_Parse_Tree_Node : public Parse_Tree_Node
{
    virtual void print(uint64_t indent = 0) = 0;
};

struct Expression_List_Parse_Tree_Node : public Parse_Tree_Node
{
    Array<Expression_Parse_Tree_Node*> expressions = {};

    void print(uint64_t indent = 0)
    {
        for (int64_t i = 0; i < expressions.count; i++)
        {
            // print_indent(indent);
            expressions[i]->print(indent);
        }
    }
};

struct Call_Expression_Parse_Tree_Node : public Expression_Parse_Tree_Node
{
    bool is_builtin = false;
    Identifier_Parse_Tree_Node* identifier = nullptr;
    Expression_List_Parse_Tree_Node* arg_list = nullptr;

    void print(uint64_t indent = 0)
    {
        print_indent(indent);
        printf("CALL: \"%s\"\n", identifier->identifier.data);

        if (arg_list)
        {
            print_indent(indent);
            printf("  ARGS:\n");
            arg_list->print(indent + 4);
        }
    }
};

struct Identifier_Expression_Parse_Tree_Node : public Expression_Parse_Tree_Node
{
    Identifier_Parse_Tree_Node* identifier = nullptr;

    void print(uint64_t indent = 0)
    {
        print_indent(indent);
        printf("IDENT_EXPR: \"%s\"\n", identifier->identifier.data);
    }
};

struct Binary_Expression_Parse_Tree_Node : public Expression_Parse_Tree_Node
{
    Binary_Operator op = BINOP_INVALID;
    Expression_Parse_Tree_Node* lhs = nullptr;
    Expression_Parse_Tree_Node* rhs = nullptr;

    void print(uint64_t indent = 0)
    {
        print_indent(indent);

        printf("BINOP: '");
        switch (op)
        {
            case BINOP_INVALID: assert(false);

            case BINOP_ADD: printf("+'\n"); break;
            case BINOP_SUB: printf("-'\n"); break;
        }

        print_indent(indent);
        printf("  LHS:\n");
        lhs->print(indent + 4);

        print_indent(indent);
        printf("  RHS:\n");
        rhs->print(indent + 4);
    }
};

struct Number_Literal_Expression_Parse_Tree_Node : public Expression_Parse_Tree_Node
{
    union
    {
        int64_t s64;
        uint64_t u64;
    } value;

    void print(uint64_t indent = 0)
    {
        print_indent(indent);
        printf("NUMBER_LITERAL: %ld\n", value.u64);
    }
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

Number_Literal_Expression_Parse_Tree_Node* new_number_literal_expression_parse_tree_node(
    Allocator* allocator,
    Atom atom
);

}
