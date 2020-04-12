#pragma once

#include "array.h"
#include "atom.h"
#include "struct_predecls.h"

namespace Zodiac
{

    enum class AST_Node_Kind
    {
        INVALID,

        MODULE,
        IDENTIFIER,
        DECLARATION,
        STATEMENT,
        EXPRESSION,
        TYPE,
    };

    struct AST_Node
    {
        AST_Node_Kind kind = AST_Node_Kind::INVALID;
    };

    struct AST_Identifier : public AST_Node
    {
        static AST_Node_Kind _kind;

        Atom atom = {};
    };

    struct AST_Module : public AST_Node
    {
        static AST_Node_Kind _kind;

        Array<AST_Declaration*> declarations = {};
    };

    enum class AST_Declaration_Kind
    {
        INVALID,
    };

    struct AST_Declaration : public AST_Node
    {
        static AST_Node_Kind _kind;

        AST_Declaration_Kind kind = AST_Declaration_Kind::INVALID;
    };

    enum class AST_Statement_Kind
    {
        INVALID,

        BLOCK,
    };

    struct AST_Statement : public AST_Node
    {
        AST_Statement() {}
        static AST_Node_Kind _kind;

        AST_Statement_Kind kind = AST_Statement_Kind::INVALID;

        union
        {
            struct
            {
                Array<AST_Statement*> statements;
            } block;
        };
    };

    enum class AST_Type_Kind
    {
        INVALID,

        FUNCTION,
    };

    struct AST_Type : public AST_Node
    {
        AST_Type() {}

        static AST_Node_Kind _kind;

        AST_Type_Kind kind = AST_Type_Kind::INVALID;

        union
        {
            struct
            {
                Array<AST_Type*> parameter_types;
                AST_Type* return_type;
            } function;
        };
    };

    void ast_node_init(AST_Node* ast_node, AST_Node_Kind kind);

    template <typename T>
    T* ast_node_new(Allocator* allocator)
    {
        T* result = alloc_type<T>(allocator);
        ast_node_init(result, T::_kind);
        return result;
    }

    AST_Node* ast_create_from_parsed_file(Allocator* allocator, Parsed_File* parsed_file);
    AST_Declaration* ast_create_declaration_from_ptn(Allocator* allocator, Declaration_PTN* ptn);
    AST_Statement* ast_create_statement_from_ptn(Allocator* allocator, Statement_PTN* ptn);
    AST_Expression* ast_create_expression_from_ptn(Allocator* allocator, Expression_PTN* ptn);
    AST_Type* ast_create_type_from_ptn(Allocator* allocator, PT_Node* ptn);

    AST_Identifier* ast_identifier_new(Allocator* allocator, Atom& atom);
    AST_Module* ast_module_new(Allocator* allocator, Array<AST_Declaration*> decls);

    AST_Declaration* ast_function_declaration_new(Allocator* allocator, AST_Identifier* identifier,
                                                  AST_Type* type, AST_Statement* body);

    AST_Statement* ast_statement_new(Allocator* allocator, AST_Statement_Kind kind);
    AST_Statement* ast_block_statement_new(Allocator* allocator, Array<AST_Statement*> statements);
    AST_Statement* ast_expression_statement_new(Allocator* allocator, AST_Expression* expression);

    AST_Type* ast_type_new(Allocator* allocator, AST_Type_Kind kind);
    AST_Type* ast_function_type_new(Allocator* allocator, Array<AST_Type*> param_types,
                                    AST_Type* return_type);

    void ast_print(AST_Node* ast_node);
}
