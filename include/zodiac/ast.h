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
        SWITCH_CASE,
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
        USING,

        VARIABLE,
        CONSTANT,

        PARAMETER,
        FUNCTION,
        TYPE,
        TYPEDEF,
        STRUCTURE,
        ENUM,

        POLY_TYPE,
    };

    typedef uint64_t AST_Declaration_Flag;

    enum AST_Declaration_Flag__ : AST_Declaration_Flag
    {
        AST_DECL_FLAG_NONE                  = 0x000,
        AST_DECL_FLAG_IS_NAKED              = 0x001,
        AST_DECL_FLAG_IS_ENTRY              = 0x002,
        AST_DECL_FLAG_IS_BYTECODE_ENTRY     = 0x004,
        AST_DECL_FLAG_NORETURN              = 0x008,
        AST_DECL_FLAG_FOREIGN               = 0x010,
        AST_DECL_FLAG_GLOBAL                = 0x020,
        AST_DECL_FLAG_IS_ENUM_MEMBER        = 0x040,
        AST_DECL_FLAG_ENUM_MEMBER_INTINIT   = 0x080,
        AST_DECL_FLAG_ENUM_MEMBER_IDENTINIT = 0x100,
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
                AST_Expression *ident_expression;                
                AST_Module *ast_module;
                bool parse_queued;
            } import;

            struct
            {
                AST_Expression *ident_expr; 
                Scope *import_scope;
            } using_decl;

            struct
            {
                AST_Type_Spec *type_spec;
                AST_Expression *init_expression;
            } variable, constant;

            struct
            {
                AST_Type_Spec *type_spec;
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
                AST_Type_Spec *type_spec;
                Array<AST_Declaration*> member_declarations;
                Scope *member_scope; 
            } enum_decl;

            struct
            {
                AST_Type_Spec *type_spec;
            } typedef_decl;

            struct
            {
                AST_Identifier *specification_identifier;
            } poly_type;
        };
    };

    struct AST_Switch_Case : public AST_Node
    {
        static AST_Node_Kind _kind;

        Array<AST_Expression *> expressions = {};
        bool is_default = false;
        AST_Statement *body = nullptr;
    };

    enum class AST_Statement_Kind
    {
        INVALID,

        BLOCK,

        ASSIGNMENT,
        RETURN,
        BREAK,

        DECLARATION,
        EXPRESSION,

        WHILE,
        FOR,
        IF,
        SWITCH,
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

            struct
            {
                AST_Expression *cond_expr;
                AST_Statement *body;
                Scope *body_scope;
            } while_stmt;

            struct
            {
                Array<AST_Statement *> init_statements;
                AST_Expression *cond_expr;
                AST_Declaration *it_decl;
                Array<AST_Statement *> step_statements;
                AST_Statement *body_stmt;

                Scope *scope;
            } for_stmt;

            struct
            {
                AST_Expression *cond_expr;
                AST_Statement *then_stmt;
                AST_Statement *else_stmt;

                Scope *then_scope;
                Scope *else_scope;
            } if_stmt;

            struct 
            {
                AST_Expression *expression;
                AST_Switch_Case *default_case;
                Array<AST_Switch_Case*> cases;
                uint32_t case_expr_count;
                bool allow_incomplete;
            } switch_stmt;
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
        POST_FIX,
        PRE_FIX,

        CALL,
        BUILTIN_CALL,
        ADDROF,
        COMPOUND,
        SUBSCRIPT,
        CAST,

        INTEGER_LITERAL,
        FLOAT_LITERAL,
        STRING_LITERAL,
        CHAR_LITERAL,
        BOOL_LITERAL,
        NULL_LITERAL,

        RANGE,
    };

    typedef uint64_t AST_Expression_Flags;

    enum AST_Expression_Flags_ : AST_Expression_Flags
    {
        AST_EXPR_FLAG_NONE      = 0x00,
        AST_EXPR_FLAG_CONST     = 0x01,
        AST_EXPR_FLAG_DOT_COUNT = 0x02,
    };

    struct AST_Expression : public AST_Node
    {
        AST_Expression() {};
        static AST_Node_Kind _kind;

        AST_Expression_Kind kind = AST_Expression_Kind::INVALID;
        AST_Type *type = nullptr;

        AST_Expression_Flags expr_flags = AST_EXPR_FLAG_NONE;

        union
        {
            AST_Identifier *identifier;

            struct
            {
                AST_Declaration *poly_type_decl;
            } poly_identifier;

            struct
            {
                AST_Expression *parent_expression;
                AST_Identifier *child_identifier;

                AST_Declaration *child_decl;
                int64_t child_index;
            } dot;

            struct
            {
                Binary_Operator op;
                AST_Expression *lhs;
                AST_Expression *rhs;
            } binary;

            struct
            {
                Unary_Operator op;
                AST_Expression *operand_expression;
            } unary;

            struct
            {
                Binary_Operator op;
                AST_Expression *operand_expression;
            } post_fix, pre_fix;

            struct
            {
                AST_Expression *ident_expression;
                Array<AST_Expression*> arg_expressions;

                AST_Declaration *callee_declaration;
            } call;

            struct
            {
                AST_Identifier *identifier;
                Array<AST_Expression*> arg_expressions;
            } builtin_call;

            struct
            {
                AST_Expression *operand_expr;
            } addrof;

            struct
            {
                Array<AST_Expression*> expressions;
                AST_Type_Spec *type_spec;
            } compound;

            struct 
            {
                AST_Expression *pointer_expression;
                AST_Expression *index_expression;
            } subscript;

            struct
            {
                AST_Expression *operand_expression;
                AST_Type *target_type;
            } cast;

            Integer_Literal integer_literal;

            struct
            {
                float r32;
            } float_literal;

            struct
            {
                Atom atom;
            } string_literal;

            struct
            {
                char c;
            } char_literal;

            struct
            {
                bool value;
            } bool_literal;

            struct
            {
                AST_Expression *begin;
                AST_Expression *end;
            } range;
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
        FROM_TYPE,
    };

    struct AST_Type_Spec : public AST_Node
    {
        AST_Type_Spec() {}

        static AST_Node_Kind _kind;

        AST_Type_Spec_Kind kind = AST_Type_Spec_Kind::INVALID;
        AST_Type *type = nullptr;

        union
        {
            AST_Identifier *identifier;
            AST_Type_Spec *base_type_spec;
            AST_Expression *dot_expression;

            struct
            {
                Array<AST_Type_Spec*> parameter_type_specs;
                AST_Type_Spec *return_type_spec;
            } function;

            struct
            {
                AST_Expression *length_expression;
                AST_Type_Spec *element_type_spec;
            } array;

            struct
            {
                AST_Expression *ident_expression;
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
        FLOAT,
        BOOL,
        POINTER,

        FUNCTION,
        STRUCTURE,
        ENUM,
        ARRAY,
    };

    struct AST_Type :  public AST_Node
    {
        AST_Type() {}

        static AST_Node_Kind _kind;

        AST_Type_Kind kind = AST_Type_Kind::INVALID;

        uint64_t bit_size = 0;
        AST_Type *pointer_to = nullptr;

        union
        {
            struct
            {
                bool sign;
            } integer;

            struct
            {
                AST_Type *base;
            } pointer;

            struct
            {
                Array<AST_Type*> param_types;
                AST_Type *return_type;
            } function;

            struct
            {
                Array<AST_Type*> member_types;
                Scope *member_scope;
                AST_Declaration *declaration;
            } structure;

            struct
            {
                AST_Type *base_type;
                AST_Declaration *declaration;
                Array<Integer_Literal> unique_member_values;
                Scope *member_scope;
            } enum_type;

            struct 
            {
                AST_Type *element_type;
                int64_t element_count;
            } array;
        };
    };

    void ast_node_init(AST_Node *ast_node, AST_Node_Kind kind, const File_Pos &begin_fp,
                       const File_Pos &end_fp);

    template <typename T>
    T *ast_node_new(Allocator *allocator, const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        T *result = alloc_type<T>(allocator);
        ast_node_init(result, T::_kind, begin_fp, end_fp);
        return result;
    }

    AST_Module *ast_create_from_parsed_file(Allocator *allocator,
                                            Parsed_File *parsed_file,
                                            Scope *global_scope);

    AST_Identifier *ast_create_identifier_from_ptn(Allocator *allocator,
                                                   Identifier_PTN *ptn);
    AST_Identifier *ast_create_identifier_from_ptn(Allocator *allocator,
                                                   Expression_PTN *ptn);

    AST_Declaration *ast_create_declaration_from_ptn(Allocator *allocator,
                                                     Declaration_PTN *ptn,
                                                     Array<AST_Declaration*> *var_decls,
                                                     Scope *parent_scope);

    AST_Declaration *ast_create_declaration_from_ptn(Allocator *allocator,
                                                     Parameter_PTN *ptn,
                                                     AST_Type_Spec *type_spec);

    AST_Declaration *ast_create_enum_member_from_ptn(Allocator *allocator, PT_Node *ptn);

    AST_Statement *ast_create_statement_from_ptn(Allocator *allocator, Statement_PTN *ptn, 
                                                 Array<AST_Declaration*> *var_decls,
                                                 Scope *parent_scope);

    AST_Expression *ast_create_expression_from_ptn(Allocator *allocator,
                                                   Expression_PTN *ptn);
    AST_Type_Spec *ast_create_type_spec_from_ptn(Allocator *allocator, PT_Node *ptn);

    AST_Type_Spec *ast_create_type_spec_from_expression_ptn(Allocator *allocator,
                                                            Expression_PTN *ptn);

    AST_Identifier *ast_identifier_new(Allocator *allocator, Atom &atom,
                                       const File_Pos &begin_fp, const File_Pos &end_fp);

    AST_Module *ast_module_new(Allocator *allocator, Array<AST_Declaration*> decls, 
                               Scope *module_scope,
                               const File_Pos &begin_fp, const File_Pos &end_fp);

    AST_Declaration *ast_declaration_new(Allocator *allocator, AST_Declaration_Kind kind, 
                                         AST_Identifier *identifier,
                                         const File_Pos &begin_fp,
                                         const File_Pos &end_fp);

    AST_Declaration *ast_import_declaration_new(Allocator *allocator,
                                                AST_Identifier *identifier,
                                                AST_Expression *ident_expr,
                                                const File_Pos &begin_fp,
                                                const File_Pos &end_fp);

    AST_Declaration *ast_using_declaration_new(Allocator *allocator,
                                               AST_Expression *import_ident_expr,
                                               const File_Pos &begin_fp,
                                               const File_Pos &end_fp);

    AST_Declaration *ast_variable_declaration_new(Allocator *allocator, AST_Identifier *identifier,
                                                  AST_Type_Spec *type_spec,
                                                  AST_Expression *init_expr,
                                                  const File_Pos &begin_fp,
                                                  const File_Pos &end_fp);

    AST_Declaration *ast_constant_declaration_new(Allocator *allocator, AST_Identifier *identifier,
                                                  AST_Type_Spec *type_spec,
                                                  AST_Expression *init_expr,
                                                  const File_Pos &begin_fp,
                                                  const File_Pos &end_fp);

    AST_Declaration *ast_parameter_declaration_new(Allocator *allocator, AST_Identifier *identifier,
                                                   AST_Type_Spec *type_spec,
                                                   const File_Pos &begin_fp,
                                                   const File_Pos &end_fp);

    AST_Declaration *ast_function_declaration_new(Allocator *allocator,
                                                  AST_Identifier *identifier,
                                                  AST_Type_Spec *type_spec, 
                                                  Array<AST_Declaration*> param_decls,
                                                  Array<AST_Declaration*> var_decls,
                                                  AST_Statement *body,
                                                  bool is_naked, bool is_noreturn,
                                                  bool is_foreign,
                                                  Scope *param_scope,
                                                  const File_Pos &begin_fp,
                                                  const File_Pos &end_fp);

    AST_Declaration *ast_type_declaration_new(Allocator *allocator, AST_Type *type,
                                              AST_Identifier *identifier);

    AST_Declaration *ast_structure_declaration_new(Allocator *allocator,
                                                   AST_Identifier *identifier,
                                                   Array<AST_Declaration*> member_decls,
                                                   Array<AST_Declaration*> parameters,
                                                   Scope *scope,
                                                   const File_Pos &begin_fp,
                                                   const File_Pos &end_fp);

    AST_Declaration *ast_enum_declaration_new(Allocator *allocator,
                                              AST_Identifier *identifier,
                                              AST_Type_Spec *ast_ts,
                                              Array<AST_Declaration*> member_decls,
                                              Scope *parent_scope,
                                              const File_Pos &begin_fp,
                                              const File_Pos &end_fp);

    AST_Declaration *ast_typedef_declaration_new(Allocator *allocator,
                                                 AST_Identifier *identifier,
                                                 AST_Type_Spec *type_spec,
                                                 const File_Pos &begin_fp,
                                                 const File_Pos &end_fp);

    AST_Declaration *ast_poly_type_declaration_new(Allocator *allocator,
                                                   AST_Identifier *identifier,
                                                   AST_Identifier *spec_ident,
                                                   const File_Pos &begin_fp,
                                                   const File_Pos &end_fp);

    AST_Switch_Case *ast_switch_case_new(Allocator *allocator,
                                         Array<AST_Expression *> case_exprs,
                                         bool is_default, AST_Statement *body, 
                                         const File_Pos &begin_fp,
                                         const File_Pos &end_fp);

    AST_Statement *ast_statement_new(Allocator *allocator, AST_Statement_Kind kind,
                                     const File_Pos &begin_fp, const File_Pos &end_fp);

    AST_Statement *ast_block_statement_new(Allocator *allocator,
                                           Array<AST_Statement*> statements,
                                           Scope *block_scope,
                                           const File_Pos &begin_fp,
                                           const File_Pos &end_fp);

    AST_Statement *ast_assignment_statement_new(Allocator *allocator,
                                                AST_Expression *ident_expr,
                                                AST_Expression *rhs_expr,
                                                const File_Pos &begin_fp,
                                                const File_Pos &end_fp);

    AST_Statement *ast_return_statement_new(Allocator *allocator,
                                            AST_Expression *return_expr,
                                            const File_Pos &begin_fp,
                                            const File_Pos &end_fp);

    AST_Statement *ast_break_statement_new(Allocator *allocator, const File_Pos &begin_fp,
                                           const File_Pos &end_fp);

    AST_Statement *ast_declaration_statement_new(Allocator *allocator,
                                                 AST_Declaration *declaration,
                                                 const File_Pos &begin_fp,
                                                 const File_Pos &end_fp);

    AST_Statement *ast_expression_statement_new(Allocator *allocator,
                                                AST_Expression *expression,
                                                const File_Pos &begin_fp,
                                                const File_Pos &end_fp);

    AST_Statement *ast_while_statement_new(Allocator *allocator, AST_Expression *cond_expr,
                                           AST_Statement *body, Scope *body_scope,
                                           const File_Pos &begin_fp,
                                           const File_Pos &end_fp);

    AST_Statement *ast_for_statement_new(Allocator *allocator, 
                                         Array<AST_Statement *> init_statements,
                                         AST_Expression *cond_expr,
                                         AST_Declaration *it_decl,
                                         Array<AST_Statement *> step_statements,
                                         AST_Statement *body_stmt, 
                                         const File_Pos &begin_fp,
                                         const File_Pos &end_fp);

    AST_Statement *ast_if_statement_new(Allocator *allocator, AST_Expression *cond_expr,
                                           AST_Statement *then_stmt,
                                           AST_Statement *else_stmt,
                                           Scope *then_scope, Scope *else_scope,
                                           const File_Pos &begin_fp,
                                           const File_Pos &end_fp);

    AST_Statement *ast_switch_statement_new(Allocator *allocator, AST_Expression *expression,
                                            AST_Switch_Case *default_case,
                                            Array<AST_Switch_Case*> cases,
                                            uint32_t case_expr_count,
                                            bool allow_incomplete,
                                            const File_Pos &begin_fp,
                                            const File_Pos &end_fp);

    AST_Expression *ast_expression_new(Allocator *allocator, AST_Expression_Kind kind,
                                       const File_Pos &begin_fp, const File_Pos &end_fp);

    AST_Expression *ast_identifier_expression_new(Allocator *allocator,
                                                  AST_Identifier *identifier,
                                                  const File_Pos &begin_fp,
                                                  const File_Pos &end_fp);

    AST_Expression *ast_poly_identifier_expression_new(Allocator *allocator,
                                                       AST_Declaration *poly_type_decl,
                                                       const File_Pos &begin_fp,
                                                       const File_Pos &end_fp);

    AST_Expression *ast_dot_expression_new(Allocator *allocator,
                                           AST_Expression *parent_expr,
                                           AST_Identifier *child_ident,
                                           const File_Pos &begin_fp,
                                           const File_Pos &end_fp);

    AST_Expression *ast_binary_expression_new(Allocator *allocator, Binary_Operator op,
                                              AST_Expression *lhs, AST_Expression *rhs,
                                              const File_Pos &begin_fp,
                                              const File_Pos &end_fp);

    AST_Expression *ast_unary_expression_new(Allocator *allocator, Unary_Operator op,
                                             AST_Expression *operand_expr,
                                             const File_Pos &begin_fp,
                                             const File_Pos &end_fp);

    AST_Expression *ast_postfix_expression_new(Allocator *allocator, Binary_Operator op,
                                               AST_Expression *operand_expr, 
                                               const File_Pos &begin_fp,
                                               const File_Pos &end_fp);

    AST_Expression *ast_prefix_expression_new(Allocator *allocator, Binary_Operator op,
                                              AST_Expression *operand_expr, 
                                              const File_Pos &begin_fp,
                                              const File_Pos &end_fp);

    AST_Expression *ast_call_expression_new(Allocator *allocator,
                                            AST_Expression *ident_expr,
                                            Array<AST_Expression*> arg_expressions,
                                            const File_Pos &begin_fp,
                                            const File_Pos &end_fp);

    AST_Expression *ast_builtin_call_expression_new(Allocator *allocator,
                                                    AST_Identifier *identifier,
                                                    Array<AST_Expression*> arg_expressions,
                                                    const File_Pos &begin_fp,
                                                    const File_Pos &end_fp);

    AST_Expression *ast_addrof_expression_new(Allocator *allocator,
                                              AST_Expression *operand_expr,
                                              const File_Pos &begin_fp,
                                              const File_Pos &end_fp);

    AST_Expression *ast_compound_expression_new(Allocator *allocator,
                                                Array<AST_Expression*> exprs,
                                                AST_Type_Spec *type_spec,
                                                const File_Pos &begin_fp,
                                                const File_Pos &end_fp);

    AST_Expression *ast_subscript_expression_new(Allocator *allocator,
                                                 AST_Expression *pointer_expr,
                                                 AST_Expression *index_expr,
                                                 const File_Pos &bfp, const File_Pos &efp);

    AST_Expression *ast_cast_expression_new(Allocator *allocator,
                                            AST_Expression *operand_expr,
                                            AST_Type *target_type, const File_Pos &bfp,
                                            const File_Pos &efp);

    AST_Expression *ast_integer_literal_expression_new(Allocator *allocator, int64_t value,
                                                       const File_Pos &begin_fp,
                                                       const File_Pos &end_fp);

    AST_Expression *ast_float_literal_expression_new(Allocator *allocator, float value,
                                                     const File_Pos &begin_fp,
                                                     const File_Pos &end_fp);

    AST_Expression *ast_string_literal_expression_new(Allocator *allocator, Atom& atom,
                                                      const File_Pos &begin_fp,
                                                      const File_Pos &end_fp);

    AST_Expression *ast_char_literal_expression_new(Allocator *allocator, char c,
                                                      const File_Pos &begin_fp,
                                                      const File_Pos &end_fp);

    AST_Expression *ast_boolean_literal_expression_new(Allocator *allocator, bool value,
                                                       const File_Pos &begin_fp,
                                                       const File_Pos &end_fp);

    AST_Expression *ast_null_literal_expression_new(Allocator *allocator,
                                                    const File_Pos &begin_fp,
                                                    const File_Pos &end_fp);

    AST_Expression *ast_range_expression_new(Allocator *allocator, 
                                              AST_Expression *begin_expr,
                                              AST_Expression *end_expr);

    AST_Type_Spec *ast_type_spec_new(Allocator *allocator, AST_Type_Spec_Kind kind,
                                     const File_Pos &begin_fp, const File_Pos &end_fp);

    AST_Type_Spec *ast_identifier_type_spec_new(Allocator *allocator,
                                                AST_Identifier *identifier,
                                                const File_Pos &begin_fp,
                                                const File_Pos &end_fp);

    AST_Type_Spec *ast_pointer_type_spec_new(Allocator *allocator,
                                             AST_Type_Spec *base_ts,
                                             const File_Pos &begin_fp,
                                             const File_Pos &end_fp);

    AST_Type_Spec *ast_dot_type_spec_new(Allocator *allocator, AST_Expression *dot_expr,
                                         const File_Pos &begin_fp,
                                         const File_Pos &end_fp);

    AST_Type_Spec *ast_function_type_spec_new(Allocator *allocator,
                                              Array<AST_Type_Spec*> param_type_specs,
                                              AST_Type_Spec *return_type_spec,
                                              const File_Pos &begin_fp,
                                              const File_Pos &end_fp);

    AST_Type_Spec *ast_array_type_spec_new(Allocator *allocator,
                                           AST_Expression *length_expr,
                                           AST_Type_Spec *element_ts,
                                           const File_Pos &begin_fp,
                                           const File_Pos &end_fp);

    AST_Type_Spec *ast_templated_type_spec_new(Allocator *allocator,
                                               AST_Expression *ident_expr,
                                               Array<AST_Expression*> arg_exprs,
                                               const File_Pos &begin_fp,
                                               const File_Pos &end_fp);

    AST_Type_Spec *ast_poly_identifier_type_spec_new(Allocator *allocator,
                                                     AST_Declaration *decl,
                                                     AST_Identifier *spec_ident,
                                                     const File_Pos &begin_fp,
                                                     const File_Pos &end_fp);

    AST_Type_Spec *ast_type_spec_from_type_new(Allocator *allocator, AST_Type *type);

    AST_Type *ast_type_new(Allocator *allocator, AST_Type_Kind kind, uint64_t bit_size);
    AST_Type *ast_integer_type_new(Allocator *allocator, uint64_t bit_size, bool sign);
    AST_Type *ast_float_type_new(Allocator *allocator, uint64_t bit_size);
    AST_Type *ast_bool_type_new(Allocator *allocator, uint64_t bit_size);
    AST_Type *ast_pointer_type_new(Allocator *allocator, AST_Type *base_type);
    AST_Type *ast_function_type_new(Allocator *allocator, Array<AST_Type*> param_types,
                                    AST_Type *return_type);
    AST_Type *ast_structure_type_new(Allocator *allocator, AST_Declaration *declaration,
                                     Array<AST_Type*> member_types, Scope *member_scope);
    AST_Type *ast_enum_type_new(Allocator *allocator, AST_Declaration *declaration,
                                AST_Type *base_type,
                                Scope *member_scope);

    AST_Type *_ast_find_or_create_pointer_type(Allocator *allocator, AST_Type *base_type);
    AST_Type *_ast_create_array_type(Allocator *allocator, AST_Type *elem_type,
                                     int64_t elem_count);

    AST_Declaration *ast_find_enum_member(AST_Type *enum_type,
                                          Const_Value member_value);

    void ast_print_indent(uint64_t indent);
    void ast_print(AST_Node *ast_node);
    void ast_print_declaration(AST_Declaration *ast_decl, uint64_t indent,
                               bool newline = true);
    void ast_print_statement(AST_Statement *ast_stmt, uint64_t indent,
                             bool newline = false);
    void ast_print_expression(AST_Expression *ast_expr, uint64_t indent);
    void ast_print_type_spec(AST_Type_Spec *type_spec);

    void ast_print_scope(Allocator *allocator, AST_Node *anode);
    void ast_print_scope(String_Builder *sb, AST_Node *anode, int64_t indent = 0);
    void ast_print_declaration_scopes(String_Builder *sb, AST_Declaration *ast_decl,
                                      int64_t indent);

    void ast_print_type(AST_Type *type);
    void ast_print_type(String_Builder *sb, AST_Type *type);

    String ast_type_to_string(Allocator *allocator, AST_Type *type);

}
