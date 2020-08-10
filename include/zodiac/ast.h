#pragma once

#include "array.h"
#include "atom.h"
#include "file_pos.h"
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
        TYPE,
    };

    typedef uint64_t AST_Node_Flags;

    enum AST_Node_Flags_ : AST_Node_Flags
    {
        AST_NODE_FLAG_NONE                     = 0x00,
        AST_NODE_FLAG_RESOLVED_ID              = 0x01,
        AST_NODE_FLAG_TYPED                    = 0x02,
        AST_NODE_FLAG_SIZED                    = 0x04,
        AST_NODE_FLAG_QUEUED_BYTECODE_EMISSION = 0x08
    };

    struct AST_Node
    {
        AST_Node_Kind kind = AST_Node_Kind::INVALID;
        AST_Node_Flags flags = AST_NODE_FLAG_NONE;

        File_Pos begin_file_pos = {};
        File_Pos end_file_pos = {};
    };

    struct AST_Identifier : public AST_Node
    {
        static AST_Node_Kind _kind;

        Atom atom = {};
        AST_Declaration *declaration = nullptr;
    };

    struct AST_Module : public AST_Node
    {
        static AST_Node_Kind _kind;

        Scope *module_scope = nullptr;

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
        TYPE,
        STRUCTURE,

        POLY_TYPE,
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
        AST_Declaration_Flag decl_flags = AST_DECL_FLAG_NONE;

        AST_Identifier *identifier = nullptr;
        AST_Type *type = nullptr;

        union
        {
            struct
            {
                AST_Expression* ident_expression;                
            } import;

            struct
            {
                AST_Type_Spec *type_spec;
                AST_Expression *init_expression;
            } variable, constant;

            struct
            {
                AST_Type_Spec* type_spec;
            } parameter;

            struct 
            {
                AST_Type_Spec *type_spec;
                Array<AST_Declaration*> parameter_declarations; 
                Array<AST_Declaration*> variable_declarations;
                AST_Statement *body;

                Scope *parameter_scope;
            } function;

            struct
            {
                Array<AST_Declaration*> member_declarations;
                Array<AST_Declaration*> parameters;

                Scope *parameter_scope;
                Scope *member_scope;
            } structure;

            struct
            {
                AST_Identifier *specification_identifier;
            } poly_type;
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
            AST_Declaration *declaration;
            AST_Expression *expression;

            struct
            {
                Array<AST_Statement*> statements;
                Scope *scope;
            } block;

            struct
            {
                AST_Expression *identifier_expression;
                AST_Expression *rhs_expression;
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

        AST_Expression_Kind kind = AST_Expression_Kind::INVALID;
        AST_Type *type = nullptr;

        union
        {
            AST_Identifier* identifier;

            struct
            {
                AST_Declaration *poly_type_decl;
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
                AST_Expression *ident_expression;
                Array<AST_Expression*> arg_expressions;
                bool is_builtin;

                AST_Declaration *callee_declaration;
            } call;

            struct
            {
                Array<AST_Expression*> expressions;
                AST_Type_Spec* type_spec;
            } compound;

            Number_Literal number_literal;

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
        AST_Type *type = nullptr;

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
                AST_Declaration *declaration;
                AST_Identifier *specification_identifier;
            } poly_identifier;
        };
    };

    enum class AST_Type_Kind
    {
        INVALID,

        VOID,
        INTEGER,

        FUNCTION,
    };

    struct AST_Type :  public AST_Node
    {
        AST_Type() {}

        static AST_Node_Kind _kind;

        AST_Type_Kind kind = AST_Type_Kind::INVALID;

        uint64_t bit_size = 0;

        union
        {
            struct
            {
                bool sign;
            } integer;

            struct
            {
                Array<AST_Type*> param_types;
                AST_Type* return_type;
            } function;
        };
    };

    void ast_node_init(AST_Node* ast_node, AST_Node_Kind kind, const File_Pos &begin_fp,
                       const File_Pos &end_fp);

    template <typename T>
    T* ast_node_new(Allocator* allocator, const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        T* result = alloc_type<T>(allocator);
        ast_node_init(result, T::_kind, begin_fp, end_fp);
        return result;
    }

    AST_Node* ast_create_from_parsed_file(Allocator* allocator, Parsed_File* parsed_file);
    AST_Declaration* ast_create_declaration_from_ptn(Allocator* allocator, Declaration_PTN* ptn,
                                                     Array<AST_Declaration*> *var_decls);
    AST_Declaration* ast_create_declaration_from_ptn(Allocator* allocator, Parameter_PTN* ptn,
                                                     AST_Type_Spec *type_spec);
    AST_Statement* ast_create_statement_from_ptn(Allocator* allocator, Statement_PTN* ptn, 
                                                 Array<AST_Declaration*> *var_decls);
    AST_Expression* ast_create_expression_from_ptn(Allocator* allocator, Expression_PTN* ptn);
    AST_Type_Spec* ast_create_type_spec_from_ptn(Allocator* allocator, PT_Node* ptn);
    AST_Type_Spec* ast_create_type_spec_from_expression_ptn(Allocator* allocator,
                                                            Expression_PTN* ptn);

    AST_Identifier* ast_identifier_new(Allocator* allocator, Atom& atom,
                                       const File_Pos & begin_fp, const File_Pos &end_fp);
    AST_Module* ast_module_new(Allocator* allocator, Array<AST_Declaration*> decls, 
                               const File_Pos & begin_fp, const File_Pos &end_fp);

    AST_Declaration* ast_declaration_new(Allocator* allocator, AST_Declaration_Kind kind, 
                                         AST_Identifier* identifier, const File_Pos & begin_fp,
                                         const File_Pos &end_fp);
    AST_Declaration* ast_import_declaration_new(Allocator* allocator, AST_Identifier* identifier,
                                                AST_Expression* ident_expr, const File_Pos & begin_fp,
                                                const File_Pos &end_fp);
    AST_Declaration* ast_variable_declaration_new(Allocator* allocator, AST_Identifier* identifier,
                                                  AST_Type_Spec* type_spec,
                                                  AST_Expression* init_expr,
                                                  const File_Pos & begin_fp, const File_Pos &end_fp);
    AST_Declaration* ast_constant_declaration_new(Allocator* allocator, AST_Identifier* identifier,
                                                  AST_Type_Spec* type_spec,
                                                  AST_Expression* init_expr,
                                                  const File_Pos & begin_fp, const File_Pos &end_fp);
    AST_Declaration* ast_parameter_declaration_new(Allocator* allocator, AST_Identifier* identifier,
                                                   AST_Type_Spec* type_spec,
                                                   const File_Pos & begin_fp, const File_Pos &end_fp);
    AST_Declaration* ast_function_declaration_new(Allocator* allocator, AST_Identifier* identifier,
                                                  AST_Type_Spec* type_spec, 
                                                  Array<AST_Declaration*> parameter_declarations,
                                                  Array<AST_Declaration*> variable_declarations,
                                                  AST_Statement* body,
                                                  bool is_naked, const File_Pos & begin_fp,
                                                  const File_Pos &end_fp);
    AST_Declaration* ast_type_declaration_new(Allocator *allocator, AST_Type *type,
                                              AST_Identifier *identifier);
    AST_Declaration* ast_structure_declaration_new(Allocator* allocator,
                                                   AST_Identifier* identifier,
                                                   Array<AST_Declaration*> member_decls,
                                                   Array<AST_Declaration*> parameters,
                                                   const File_Pos & begin_fp, const File_Pos &end_fp);
    AST_Declaration* ast_poly_type_declaration_new(Allocator* allocator,
                                                   AST_Identifier* identifier,
                                                   AST_Identifier* spec_ident,
                                                   const File_Pos & begin_fp,
                                                   const File_Pos &end_fp);

    AST_Statement* ast_statement_new(Allocator* allocator, AST_Statement_Kind kind,
                                     const File_Pos & begin_fp, const File_Pos &end_fp);
    AST_Statement* ast_block_statement_new(Allocator* allocator, Array<AST_Statement*> statements,
                                           const File_Pos & begin_fp, const File_Pos &end_fp);
    AST_Statement* ast_assignment_statement_new(Allocator* allocator, AST_Expression* ident_expr,
                                                AST_Expression* rhs_expr,
                                                const File_Pos & begin_fp, const File_Pos &end_fp);
    AST_Statement* ast_return_statement_new(Allocator* allocator, AST_Expression* return_expr,
                                            const File_Pos & begin_fp, const File_Pos &end_fp);
    AST_Statement* ast_declaration_statement_new(Allocator* allocator,
                                                 AST_Declaration* declaration,
                                                 const File_Pos & begin_fp, const File_Pos &end_fp);
    AST_Statement* ast_expression_statement_new(Allocator* allocator,
                                                AST_Expression* expression,
                                                const File_Pos & begin_fp, const File_Pos &end_fp);

    AST_Expression* ast_expression_new(Allocator* allocator, AST_Expression_Kind kind,
                                       const File_Pos & begin_fp, const File_Pos &end_fp);
    AST_Expression* ast_identifier_expression_new(Allocator* allocator, AST_Identifier* identifier,
                                                  const File_Pos & begin_fp, const File_Pos &end_fp);
    AST_Expression* ast_poly_identifier_expression_new(Allocator* allocator,
                                                       AST_Declaration *poly_type_decl,
                                                       const File_Pos & begin_fp,
                                                       const File_Pos &end_fp);
    AST_Expression* ast_dot_expression_new(Allocator* allocator, AST_Expression* parent_expr,
                                           AST_Identifier* child_ident, const File_Pos & begin_fp,
                                           const File_Pos &end_fp);
    AST_Expression* ast_binary_expression_new(Allocator* allocator, Binary_Operator op,
                                              AST_Expression* lhs, AST_Expression* rhs,
                                              const File_Pos & begin_fp, const File_Pos &end_fp);
    AST_Expression* ast_unary_expression_new(Allocator* allocator, Unary_Operator op,
                                             AST_Expression* operand_expr, const File_Pos & begin_fp,
                                             const File_Pos &end_fp);
    AST_Expression* ast_call_expression_new(Allocator* allocator, AST_Expression* ident_expr,
                                            Array<AST_Expression*> arg_expressions,
                                            bool is_builtin, const File_Pos & begin_fp,
                                            const File_Pos &end_fp);
    AST_Expression* ast_compound_expression_new(Allocator* allocator, Array<AST_Expression*> exprs,
                                                AST_Type_Spec* type_spec, const File_Pos & begin_fp,
                                                const File_Pos &end_fp);
    AST_Expression* ast_number_literal_expression_new(Allocator* allocator, int64_t value,
                                                      const File_Pos & begin_fp,
                                                      const File_Pos &end_fp);
    AST_Expression* ast_string_literal_expression_new(Allocator* allocator, Atom& atom,
                                                      const File_Pos & begin_fp,
                                                      const File_Pos &end_fp);

    AST_Type_Spec* ast_type_spec_new(Allocator* allocator, AST_Type_Spec_Kind kind,
                                     const File_Pos & begin_fp, const File_Pos &end_fp);
    AST_Type_Spec* ast_identifier_type_spec_new(Allocator* allocator, AST_Identifier* identifier,
                                                const File_Pos & begin_fp, const File_Pos &end_fp);
    AST_Type_Spec* ast_pointer_type_spec_new(Allocator* allocator, AST_Type_Spec* base_ts,
                                             const File_Pos & begin_fp, const File_Pos &end_fp);
    AST_Type_Spec* ast_dot_type_spec_new(Allocator* allocator, AST_Expression* dot_expr,
                                         const File_Pos & begin_fp, const File_Pos &end_fp);
    AST_Type_Spec* ast_function_type_spec_new(Allocator* allocator,
                                              Array<AST_Type_Spec*> param_type_specs,
                                              AST_Type_Spec* return_type_spec,
                                              const File_Pos & begin_fp, const File_Pos &end_fp);
    AST_Type_Spec* ast_array_type_spec_new(Allocator* allocator, AST_Type_Spec* element_ts,
                                           const File_Pos & begin_fp, const File_Pos &end_fp);
    AST_Type_Spec* ast_templated_type_spec_new(Allocator* allocator, AST_Expression* ident_expr,
                                               Array<AST_Expression*> arg_exprs,
                                               const File_Pos & begin_fp, const File_Pos &end_fp);
    AST_Type_Spec* ast_poly_identifier_type_spec_new(Allocator* allocator, AST_Declaration *decl,
                                                     AST_Identifier* spec_ident,
                                                     const File_Pos & begin_fp,
                                                     const File_Pos &end_fp);

    AST_Type* ast_type_new(Allocator *allocator, AST_Type_Kind kind, uint64_t bit_size);
    AST_Type* ast_integer_type_new(Allocator *allocator, uint64_t bit_size, bool sign);
    AST_Type* ast_function_type_new(Allocator *allocator, Array<AST_Type*> param_types,
                                    AST_Type *return_type);

    void ast_print_indent(uint64_t indent);
    void ast_print(AST_Node* ast_node);
    void ast_print_declaration(AST_Declaration* ast_decl, uint64_t indent);
    void ast_print_statement(AST_Statement* ast_stmt, uint64_t indent);
    void ast_print_expression(AST_Expression* ast_expr, uint64_t indent);
    void ast_print_type_spec(AST_Type_Spec* type_spec);

    void ast_print_scope(Allocator *allocator, AST_Node *anode);
    void ast_print_scope(String_Builder* sb, AST_Node *anode, int64_t indent = 0);
    void ast_print_declaration_scopes(String_Builder *sb, AST_Declaration *ast_decl, int64_t indent);

    void ast_print_type(String_Builder *sb, AST_Type *type);
}
