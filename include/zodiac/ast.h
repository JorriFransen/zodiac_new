#pragma once

#include "array.h"
#include "atom.h"
#include "struct_predecls.h"
#include "operator.h"

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
        TYPE_SPEC,
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

        IMPORT,

        VARIABLE,
        CONSTANT,

        PARAMETER,
        FUNCTION,
        STRUCTURE,
    };

    typedef uint64_t AST_Declaration_Flag;

    enum AST_Declaration_Flag__ : AST_Declaration_Flag
    {
        AST_DECL_FLAG_NONE     = 0x00,
        AST_DECL_FLAG_IS_NAKED = 0x01,
    };

    struct AST_Declaration : public AST_Node
    {
        AST_Declaration() {}

        static AST_Node_Kind _kind;

        AST_Declaration_Kind kind = AST_Declaration_Kind::INVALID;
        AST_Declaration_Flag flags = AST_DECL_FLAG_NONE;

        AST_Identifier* identifier = nullptr;

        union
        {
            struct
            {
                AST_Expression* ident_expression;                
            } import;

            struct
            {
                AST_Type_Spec* type_spec;
                AST_Expression* init_expression;
            } variable, constant;

            struct
            {
                AST_Type_Spec* type_spec;
            } parameter;

            struct 
            {
                AST_Type_Spec* type_spec;
                Array<AST_Declaration*> parameter_declarations; 
                AST_Statement* body;
            } function;

            struct
            {
                Array<AST_Declaration*> member_declarations;
                Array<AST_Declaration*> parameters;
            } structure;
        };
    };

    enum class AST_Statement_Kind
    {
        INVALID,

        BLOCK,

        ASSIGNMENT,
        RETURN,

        DECLARATION,
        EXPRESSION,
    };

    struct AST_Statement : public AST_Node
    {
        AST_Statement() {}
        static AST_Node_Kind _kind;

        AST_Statement_Kind kind = AST_Statement_Kind::INVALID;

        union
        {
            AST_Declaration* declaration;
            AST_Expression* expression;

            struct
            {
                Array<AST_Statement*> statements;
            } block;

            struct
            {
                AST_Expression* identifier_expression;
                AST_Expression* rhs_expression;
            } assignment;
        };
    };

    enum class AST_Expression_Kind
    {
        INVALID,

        IDENTIFIER,
        POLY_IDENTIFIER,
        DOT,

        BINARY,
        UNARY,

        CALL,
        COMPOUND,

        NUMBER_LITERAL,
        STRING_LITERAL,
    };

    struct AST_Expression : public AST_Node
    {
        AST_Expression() {};
        static AST_Node_Kind _kind;

        AST_Expression_Kind kind;

        union
        {
            AST_Identifier* identifier;

            struct
            {
                AST_Identifier* identifier;
                AST_Identifier* specification_identifier;
            } poly_identifier;

            struct
            {
                AST_Expression* parent_expression;
                AST_Identifier* child_identifier;
            } dot;

            struct
            {
                Binary_Operator op;
                AST_Expression* lhs;
                AST_Expression* rhs;
            } binary;

            struct
            {
                Unary_Operator op;
                AST_Expression* operand_expression;
            } unary;

            struct
            {
                AST_Expression* ident_expression;
                Array<AST_Expression*> arg_expressions;
                bool is_builtin;
            } call;

            struct
            {
                Array<AST_Expression*> expressions;
                AST_Type_Spec* type_spec;
            } compound;

            struct
            {
                int64_t s64;
            } number_literal;

            struct
            {
                Atom atom;
            } string_literal;
        };
    };

    enum class AST_Type_Spec_Kind
    {
        INVALID,

        IDENTIFIER,
        POINTER,
        DOT,
        FUNCTION,
        ARRAY,

        TEMPLATED, // Hash_Table(int, string);
        POLY_IDENTIFIER, // $T or $T/Hash_Table
    };

    struct AST_Type_Spec : public AST_Node
    {
        AST_Type_Spec() {}

        static AST_Node_Kind _kind;

        AST_Type_Spec_Kind kind = AST_Type_Spec_Kind::INVALID;

        union
        {
            AST_Identifier* identifier;
            AST_Type_Spec* base_type_spec;
            AST_Expression* dot_expression;

            struct
            {
                Array<AST_Type_Spec*> parameter_type_specs;
                AST_Type_Spec* return_type_spec;
            } function;

            struct
            {
                AST_Type_Spec* element_type_spec;
            } array;

            struct
            {
                AST_Expression* ident_expression;
                Array<AST_Expression*> argument_expressions;
            } templated;

            struct
            {
                AST_Identifier* identifier;
                AST_Identifier* specification_identifier; 
            } poly_identifier;
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
    AST_Declaration* ast_create_declaration_from_ptn(Allocator* allocator, Parameter_PTN* ptn);
    AST_Statement* ast_create_statement_from_ptn(Allocator* allocator, Statement_PTN* ptn);
    AST_Expression* ast_create_expression_from_ptn(Allocator* allocator, Expression_PTN* ptn);
    AST_Type_Spec* ast_create_type_spec_from_ptn(Allocator* allocator, PT_Node* ptn);
    AST_Type_Spec* ast_create_type_spec_from_expression_ptn(Allocator* allocator,
                                                            Expression_PTN* ptn);

    AST_Identifier* ast_identifier_new(Allocator* allocator, Atom& atom);
    AST_Module* ast_module_new(Allocator* allocator, Array<AST_Declaration*> decls);

    AST_Declaration* ast_declaration_new(Allocator* allocator, AST_Declaration_Kind kind, 
                                         AST_Identifier* identifier);
    AST_Declaration* ast_import_declaration_new(Allocator* allocator, AST_Identifier* identifier,
                                                AST_Expression* ident_expr);
    AST_Declaration* ast_variable_declaration_new(Allocator* allocator, AST_Identifier* identifier,
                                                  AST_Type_Spec* type_spec,
                                                  AST_Expression* init_expr);
    AST_Declaration* ast_constant_declaration_new(Allocator* allocator, AST_Identifier* identifier,
                                                  AST_Type_Spec* type_spec,
                                                  AST_Expression* init_expr);
    AST_Declaration* ast_parameter_declaration_new(Allocator* allocator, AST_Identifier* identifier,
                                                   AST_Type_Spec* type_spec);
    AST_Declaration* ast_function_declaration_new(Allocator* allocator, AST_Identifier* identifier,
                                                  AST_Type_Spec* type_spec, 
                                                  Array<AST_Declaration*> parameter_declarations,
                                                  AST_Statement* body,
                                                  bool is_naked);
    AST_Declaration* ast_structure_declaration_new(Allocator* allocator,
                                                   AST_Identifier* identifier,
                                                   Array<AST_Declaration*> member_decls,
                                                   Array<AST_Declaration*> parameters);

    AST_Statement* ast_statement_new(Allocator* allocator, AST_Statement_Kind kind);
    AST_Statement* ast_block_statement_new(Allocator* allocator, Array<AST_Statement*> statements);
    AST_Statement* ast_assignment_statement_new(Allocator* allocator, AST_Expression* ident_expr,
                                                AST_Expression* rhs_expr);
    AST_Statement* ast_return_statement_new(Allocator* allocator, AST_Expression* return_expr);
    AST_Statement* ast_declaration_statement_new(Allocator* allocator,
                                                 AST_Declaration* declaration);
    AST_Statement* ast_expression_statement_new(Allocator* allocator, AST_Expression* expression);

    AST_Expression* ast_expression_new(Allocator* allocator, AST_Expression_Kind kind);
    AST_Expression* ast_identifier_expression_new(Allocator* allocator, AST_Identifier* identifier);
    AST_Expression* ast_poly_identifier_expression_new(Allocator* allocator, AST_Identifier* ident,
                                                       AST_Identifier* specification_identifier);
    AST_Expression* ast_dot_expression_new(Allocator* allocator, AST_Expression* parent_expr,
                                           AST_Identifier* child_ident);
    AST_Expression* ast_binary_expression_new(Allocator* allocator, Binary_Operator op,
                                              AST_Expression* lhs, AST_Expression* rhs);
    AST_Expression* ast_unary_expression_new(Allocator* allocator, Unary_Operator op,
                                             AST_Expression* operand_expr);
    AST_Expression* ast_call_expression_new(Allocator* allocator, AST_Expression* ident_expr,
                                            Array<AST_Expression*> arg_expressions,
                                            bool is_builtin);
    AST_Expression* ast_compound_expression_new(Allocator* allocator, Array<AST_Expression*> exprs,
                                                AST_Type_Spec* type_spec);
    AST_Expression* ast_number_literal_expression_new(Allocator* allocator, int64_t value);
    AST_Expression* ast_string_literal_expression_new(Allocator* allocator, Atom& atom);

    AST_Type_Spec* ast_type_spec_new(Allocator* allocator, AST_Type_Spec_Kind kind);
    AST_Type_Spec* ast_identifier_type_spec_new(Allocator* allocator, AST_Identifier* identifier);
    AST_Type_Spec* ast_pointer_type_spec_new(Allocator* allocator, AST_Type_Spec* base_ts);
    AST_Type_Spec* ast_dot_type_spec_new(Allocator* allocator, AST_Expression* dot_expr);
    AST_Type_Spec* ast_function_type_spec_new(Allocator* allocator,
                                              Array<AST_Type_Spec*> param_type_specs,
                                              AST_Type_Spec* return_type_spec);
    AST_Type_Spec* ast_array_type_spec_new(Allocator* allocator, AST_Type_Spec* element_ts);
    AST_Type_Spec* ast_templated_type_spec_new(Allocator* allocator, AST_Expression* ident_expr,
                                               Array<AST_Expression*> arg_exprs);
    AST_Type_Spec* ast_poly_identifier_type_spec_new(Allocator* allocator, AST_Identifier* ident,
                                                     AST_Identifier* spec_ident);

    void ast_print_indent(uint64_t indent);
    void ast_print(AST_Node* ast_node);
    void ast_print_declaration(AST_Declaration* ast_decl, uint64_t indent);
    void ast_print_statement(AST_Statement* ast_stmt, uint64_t indent);
    void ast_print_expression(AST_Expression* ast_expr, uint64_t indent);
    void ast_print_type_spec(AST_Type_Spec* type_spec);
}
