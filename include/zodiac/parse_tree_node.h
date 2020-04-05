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
    PARAMETER,
    EXPRESSION_LIST,

    DECLARATION,

    STATEMENT,

    EXPRESSION,

};

struct Declaration_PTN;
struct Parameter_PTN;
struct Expression_PTN;

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
struct Function_Proto_PTN
{
    static PTN_Kind _kind;
    PT_Node self = {};
    Array<Parameter_PTN*> parameters = {};
    Expression_PTN* return_type_expression = nullptr;
};

enum class Statement_PTN_Kind
{
    INVALID,
    BLOCK,
    DECLARATION,
    EXPRESSION,
    RETURN,
    ASSIGNMENT,
};

struct Statement_PTN
{
    static PTN_Kind _kind;
    PT_Node self = {};
    Statement_PTN_Kind kind = Statement_PTN_Kind::INVALID;

    union
    {
        Expression_PTN* expression;
        Declaration_PTN* declaration;

        struct
        {
            Array<Statement_PTN*> statements = {};
        } block;

        struct
        {
            Expression_PTN* expression;
        } return_stmt;

        struct
        {
            Expression_PTN* ident_expression;
            Expression_PTN* rhs_expression;
        } assignment;
    };

    Statement_PTN() {}
};

enum class Declaration_PTN_Kind
{
    INVALID,

    IMPORT,

    VARIABLE,
    CONSTANT,

    FUNCTION,
    STRUCT,

};

struct Declaration_PTN
{
    static PTN_Kind _kind;
    PT_Node self = {};
    Declaration_PTN_Kind kind = Declaration_PTN_Kind::INVALID;

    Identifier_PTN* identifier = nullptr;

    union
    {
        struct
        {
            Expression_PTN* module_ident_expr;
        } import;

        struct
        {
            Expression_PTN* init_expression;
            Expression_PTN* type_expression;
        } variable;

        struct
        {
            Function_Proto_PTN* prototype;
            Statement_PTN* body;
        } function;

        struct
        {
            Array<Declaration_PTN*> member_declarations;
        } structure;

        struct
        {
            Expression_PTN* type_expression;
            Expression_PTN* init_expression;
        } constant;
    };

    Declaration_PTN() {}
};

struct Expression_List_PTN
{
    static PT_Node_Kind _kind;
    PT_Node self = {};
    Array<Expression_PTN*> expressions = {};
};

struct Parameter_PTN
{
    static PT_Node_Kind _kind;
    PT_Node self = {};

    Identifier_PTN* identifier = nullptr;
    Expression_PTN* type_expression = nullptr;
};

enum class Expression_PTN_Kind
{
    INVALID,

    CALL,
    IDENTIFIER,
    BINARY,
    DOT,

    NUMBER_LITERAL,
    STRING_LITERAL,

    ARRAY_TYPE,
    POINTER_TYPE,
};

struct Expression_PTN
{
    static PT_Node_Kind _kind;
    PT_Node self = {};

    Expression_PTN_Kind kind = Expression_PTN_Kind::INVALID;

    union
    {
        Identifier_PTN* identifier;

        struct
        {
            bool is_builtin;
            Expression_PTN* ident_expression;
            Expression_List_PTN* arg_list;
        } call;

        struct
        {
            Binary_Operator op = BINOP_INVALID;
            Expression_PTN* lhs;
            Expression_PTN* rhs;
        } binary;

        struct
        {
            union
            {
                int64_t s64;
                uint64_t u64;
            } value;
        } number_literal;

        struct
        {
            Atom atom;
        } string_literal;

        struct
        {
            Expression_PTN* parent_expression;
            Identifier_PTN* child_identifier;
        } dot;

        struct
        {
            Expression_PTN* element_type_expression;
        } array_type;

        struct
        {
            Expression_PTN* pointee_type_expression;
        } pointer_type;
    };

    Expression_PTN() {}
};

void init_ptn(PTN* ptn, PTN_Kind kind);

template <typename T>
T* new_ptn(Allocator* allocator)
{
    T* result = alloc_type<T>(allocator);
    init_ptn(&result->self, T::_kind);
    return result;
}

Statement_PTN* new_statement(Allocator* allocator, Statement_PTN_Kind kind);

Identifier_PTN* new_identifier_ptn(Allocator* allocator, const Atom& atom);

Statement_PTN* new_block_statement_ptn(Allocator* allocator, Array<Statement_PTN*> statements);
Statement_PTN* new_expression_statement_ptn(Allocator* allocator,
                                            Expression_PTN* expr);
Statement_PTN* new_declaration_statement_ptn(Allocator* allocator, Declaration_PTN* decl);
Statement_PTN* new_return_statement_ptn(Allocator* allocator, Expression_PTN* expr);
Statement_PTN* new_assignment_statement_ptn(Allocator* allocator,
                                            Expression_PTN* ident_expression,
                                            Expression_PTN* rhs_expression);

Function_Proto_PTN* new_function_prototype_parse_tree_node(
    Allocator* allocator,
    Array<Parameter_PTN*> parameters,
    Expression_PTN* return_type_expr
);

Declaration_PTN* new_import_declaration_ptn(Allocator* allocator, Identifier_PTN* identifier,
                                            Expression_PTN* module_ident_expr);

Declaration_PTN* new_function_declaration_ptn(Allocator* allocator, Identifier_PTN* identifier,
                                              Function_Proto_PTN* prototype, Statement_PTN* body);

Declaration_PTN* new_variable_declaration_ptn(Allocator* allocator, Identifier_PTN* identifier,
                                              Expression_PTN* type_expression,
                                              Expression_PTN* init_expression);

Declaration_PTN* new_struct_declaration_ptn(Allocator* allocator, Identifier_PTN* identifier,
                                            Array<Declaration_PTN*> members);

Declaration_PTN* new_constant_declaration_ptn(Allocator* allocator, Identifier_PTN* identifier,
                                              Expression_PTN* type_expr,
                                              Expression_PTN* init_expr);

Statement_PTN* new_expression_statement_ptn(Allocator* allocator,
                                            Expression_PTN* expression);

Expression_List_PTN* new_expression_list_ptn(Allocator* allocator,
                                             Array<Expression_PTN*> expressions);

Expression_PTN* new_call_expression_ptn(Allocator* allocator, bool is_builtin,
                                        Expression_PTN* ident_expr, Expression_List_PTN* arg_list);

Expression_PTN* new_identifier_expression_ptn(Allocator* allocator, Identifier_PTN* identifier);

Expression_PTN* new_binary_expression_ptn(Allocator* allocator, Binary_Operator op,
                                          Expression_PTN* lhs, Expression_PTN* rhs);

Expression_PTN* new_number_literal_expression_ptn(Allocator* allocator, Atom atom);
Expression_PTN* new_string_literal_expression_ptn(Allocator* allocator, Atom atom);
Expression_PTN* new_dot_expression_ptn(Allocator* allocator, Expression_PTN* parent,
                                       Identifier_PTN* child_ident);
Expression_PTN* new_array_type_expression_ptn(Allocator* allocator,
                                              Expression_PTN* element_type_expression);
Expression_PTN* new_pointer_type_expression_ptn(Allocator* allocator,
                                                Expression_PTN* pointee_type_expression);

Parameter_PTN* new_parameter_ptn(Allocator* allocator, Identifier_PTN* identifier,
                                 Expression_PTN* type_expression);

void print_ptn(PTN* ptn, uint64_t indent);
void print_statement_ptn(Statement_PTN* statement, uint64_t indent, bool newline = true);
void print_declaration_ptn(Declaration_PTN* declaration, uint64_t indent, bool newline = true);
void print_expression_ptn(Expression_PTN* expression, uint64_t indent);

}
