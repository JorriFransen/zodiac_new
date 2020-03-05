#pragma once

#include "array.h"
#include "atom.h"
#include "operator.h"

#include <cstdio>

namespace Zodiac
{

enum class PT_Node_Kind
{
    INVALID,

    IDENTIFIER,
    FUNCTION_PROTO,

    DECLARATION,

    STATEMENT,

    EXPRESSION,

};

typedef PT_Node_Kind PTN_Kind;

struct PT_Node
{
    PT_Node_Kind kind = PT_Node_Kind::INVALID;
};

typedef PT_Node PTN;

struct Identifier_PTN
{
    static PTN_Kind _kind;
    PT_Node self = {};
    Atom atom = {};
};

// @CLEANUP: NOCHECKIN:
struct Parameter_Parse_Tree_Node;
struct Expression_Parse_Tree_Node;

struct Function_Proto_PTN
{
    static PTN_Kind _kind;
    PT_Node self = {};
    Array<Parameter_Parse_Tree_Node*> parameters = {};
    Expression_Parse_Tree_Node* return_type_expression = nullptr;
};

enum class Statement_PTN_Kind
{
    INVALID,
    BLOCK,
};

struct Statement_PTN
{
    static PTN_Kind _kind;
    PT_Node self = {};
    Statement_PTN_Kind kind = Statement_PTN_Kind::INVALID;

    union
    {
        struct
        {
            Array<Statement_PTN*> statements = {};
        } block;
    };

    Statement_PTN() {}
};


void print_ptn(PTN* ptn, uint64_t indent);





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

struct Parameter_Parse_Tree_Node : public Parse_Tree_Node
{

};

struct Declaration_Parse_Tree_Node : public Parse_Tree_Node
{
    Identifier_PTN* identifier = nullptr;
};

struct Function_Declaration_Parse_Tree_Node : public Declaration_Parse_Tree_Node
{
    Function_Proto_PTN* prototype = nullptr;
    Statement_PTN* body = nullptr;

    void print(uint64_t indent = 0)
    {
        print_indent(indent);
        printf("FUNCTION: \"%s\"\n", identifier->atom.data);
        print_ptn(&prototype->self, indent + 2);
        print_ptn(&body->self, indent + 2);
        printf("\n");
    }
};

struct Mutable_Declaration_Parse_Tree_Node : public Declaration_Parse_Tree_Node
{
    Expression_Parse_Tree_Node* init_expression = nullptr;

    void print(uint64_t indent = 0);
};

struct Struct_Declaration_Parse_Tree_Node : public Declaration_Parse_Tree_Node
{
    Array<Declaration_Parse_Tree_Node*> member_declarations = {};

    void print(uint64_t indent = 0)
    {
        print_indent(indent);
        printf("STRUCT: \"%s\"\n", identifier->atom.data);
        for (int64_t i = 0; i < member_declarations.count; i++)
        {
            member_declarations[i]->print(indent + 2);
        }
    }
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
    Identifier_PTN* identifier = nullptr;
    Expression_List_Parse_Tree_Node* arg_list = nullptr;

    void print(uint64_t indent = 0)
    {
        print_indent(indent);
        printf("CALL: \"%s\"\n", identifier->atom.data);

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
    Identifier_PTN* identifier = nullptr;

    void print(uint64_t indent = 0)
    {
        print_indent(indent);
        printf("IDENT_EXPR: \"%s\"\n", identifier->atom.data);
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

void init_ptn(PTN* ptn, PTN_Kind kind);
void init_parse_tree_node(Parse_Tree_Node* ptn);

template <typename T>
T* new_ptn(Allocator* allocator)
{
    T* result = alloc_type<T>(allocator);
    init_ptn(&result->self, T::_kind);
    return result;
}

template <typename T>
T* new_parse_tree_node(Allocator* allocator)
{
    T* result = alloc_type<T>(allocator);
    init_parse_tree_node(result);
    return result;
}

Statement_PTN* new_statement(Allocator* allocator, Statement_PTN_Kind kind);

Identifier_PTN* new_identifier_ptn(Allocator* allocator, const Atom& atom);

Statement_PTN* new_block_statement_ptn(Allocator* allocator, Array<Statement_PTN*> statements);

Parameter_Parse_Tree_Node* new_parameter_parse_tree_node(
    Allocator* allocator
);

Function_Proto_PTN* new_function_prototype_parse_tree_node(
    Allocator* allocator,
    Array<Parameter_Parse_Tree_Node*> parameters,
    Expression_Parse_Tree_Node* return_type_expr
);

Function_Declaration_Parse_Tree_Node* new_function_declaration_parse_tree_node(
    Allocator* allocator,
    Identifier_PTN* identifier,
    Function_Proto_PTN* prototype,
    Statement_PTN* body
);

Mutable_Declaration_Parse_Tree_Node* new_mutable_declaration_parse_tree_node(
    Allocator* allocator,
    Identifier_PTN* identifier,
    Expression_Parse_Tree_Node* init_expression
);

Struct_Declaration_Parse_Tree_Node*
new_struct_declaration_parse_tree_node(Allocator* allocator,
                                       Identifier_PTN* identifier,
                                       Array<Declaration_Parse_Tree_Node*> members);

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
    Identifier_PTN* identifier,
    Expression_List_Parse_Tree_Node* arg_list
);

Identifier_Expression_Parse_Tree_Node* new_identifier_expression_parse_tree_node(
    Allocator* allocator,
    Identifier_PTN* identifier
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
