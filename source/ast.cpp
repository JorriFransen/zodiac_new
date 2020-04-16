
#include "ast.h"

#include "parse_tree_node.h"

#include <cassert>

namespace Zodiac
{

    AST_Node_Kind AST_Module::_kind = AST_Node_Kind::MODULE;
    AST_Node_Kind AST_Identifier::_kind = AST_Node_Kind::IDENTIFIER;
    AST_Node_Kind AST_Declaration::_kind = AST_Node_Kind::DECLARATION;
    AST_Node_Kind AST_Statement::_kind = AST_Node_Kind::STATEMENT;
    AST_Node_Kind AST_Expression::_kind = AST_Node_Kind::EXPRESSION;
    AST_Node_Kind AST_Type_Spec::_kind = AST_Node_Kind::TYPE_SPEC;

    void ast_node_init(AST_Node* ast_node, AST_Node_Kind kind)
    {
        assert(ast_node);
        ast_node->kind = kind;
    }

    AST_Node* ast_create_from_parsed_file(Allocator* allocator, Parsed_File* parsed_file)
    {
        assert(allocator);
        assert(parsed_file);

        Array<AST_Declaration*> global_decls = {};
        array_init(allocator, &global_decls);

        for (int64_t i = 0; i < parsed_file->declarations.count; i++)
        {
            AST_Declaration* ast_decl =
                ast_create_declaration_from_ptn(allocator, parsed_file->declarations[i]);
            assert(ast_decl);
            array_append(&global_decls, ast_decl);
        }

        assert(global_decls.count);

        AST_Module* ast_module = ast_module_new(allocator, global_decls);
        assert(ast_module);
        return ast_module;
    }

    AST_Declaration* ast_create_declaration_from_ptn(Allocator* allocator, Declaration_PTN* ptn)
    {
        assert(allocator);

        assert(ptn->identifier);
        AST_Identifier* ast_ident = ast_identifier_new(allocator, ptn->identifier->atom);
        assert(ast_ident);

        switch (ptn->kind)
        {
            case Declaration_PTN_Kind::INVALID: assert(false);
            case Declaration_PTN_Kind::IMPORT: assert(false);

            case Declaration_PTN_Kind::VARIABLE:
            {
                AST_Type_Spec* type_expr = nullptr;
                AST_Expression* init_expr = nullptr;

                if (ptn->variable.type_expression)
                {
                    type_expr = ast_create_type_spec_from_ptn(allocator,
                                                              &ptn->variable.type_expression->self);
                    assert(type_expr);
                }

                if (ptn->variable.init_expression)
                {
                    init_expr = ast_create_expression_from_ptn(allocator,
                                                               ptn->variable.init_expression);
                    assert(init_expr);
                }
                
                return ast_variable_declaration_new(allocator, ast_ident, type_expr, init_expr);
                break; 
            }

            case Declaration_PTN_Kind::CONSTANT: assert(false);

            case Declaration_PTN_Kind::FUNCTION:
            {
                AST_Type_Spec* ast_type =
                    ast_create_type_spec_from_ptn(allocator, &ptn->function.prototype->self);
                assert(ast_type);

                AST_Statement* ast_body = ast_create_statement_from_ptn(allocator,
                                                                        ptn->function.body);
                assert(ast_body);

                return ast_function_declaration_new(allocator, ast_ident, ast_type, ast_body);
                break;
            }

            case Declaration_PTN_Kind::STRUCT: assert(false);
        }

        return nullptr;
    }

    AST_Statement* ast_create_statement_from_ptn(Allocator* allocator, Statement_PTN* ptn)
    {
        switch (ptn->kind)
        {
            case Statement_PTN_Kind::INVALID: assert(false); 

            case Statement_PTN_Kind::BLOCK:
            {
                Array<AST_Statement*> ast_block_stmts = {};
                if (ptn->block.statements.count)
                {
                    array_init(allocator, &ast_block_stmts);

                    for (int64_t i = 0; i < ptn->block.statements.count; i++)
                    {
                        AST_Statement* ast_block_stmt =
                            ast_create_statement_from_ptn(allocator, ptn->block.statements[i]);
                        assert(ast_block_stmt);

                        array_append(&ast_block_stmts, ast_block_stmt);
                    }
                }

                return ast_block_statement_new(allocator, ast_block_stmts);
                break; 
            }

            case Statement_PTN_Kind::DECLARATION:
            {
                auto ast_declaration = ast_create_declaration_from_ptn(allocator, ptn->declaration);
                assert(ast_declaration);
                return ast_declaration_statement_new(allocator, ast_declaration);
                break;
            }

            case Statement_PTN_Kind::EXPRESSION:
            {
                auto ast_expression = ast_create_expression_from_ptn(allocator, ptn->expression);
                return ast_expression_statement_new(allocator, ast_expression);
                break;
            }

            case Statement_PTN_Kind::RETURN:
            {
                AST_Expression* return_val_expr = nullptr;
                if (ptn->return_stmt.expression)
                {
                    return_val_expr = ast_create_expression_from_ptn(allocator,
                                                                     ptn->return_stmt.expression);
                    assert(return_val_expr);
                }

                return ast_return_statement_new(allocator, return_val_expr);
                break;
            }

            case Statement_PTN_Kind::ASSIGNMENT: assert(false); 
        }

        assert(false);
    }

    AST_Expression* ast_create_expression_from_ptn(Allocator* allocator, Expression_PTN* ptn)
    {
        assert(allocator);
       
        switch (ptn->kind)
        {
            case Expression_PTN_Kind::INVALID: assert(false);

            case Expression_PTN_Kind::CALL:
            {
                AST_Expression* ident_expr =
                    ast_create_expression_from_ptn(allocator, ptn->call.ident_expression);
                assert(ident_expr);

                Array<AST_Expression*> arg_exprs = {};
                if (ptn->call.arg_list)
                {
                    array_init(allocator, &arg_exprs);

                    for (int64_t i = 0; i < ptn->call.arg_list->expressions.count; i++)
                    {
                        AST_Expression* arg_expr =
                            ast_create_expression_from_ptn(allocator,
                                    ptn->call.arg_list->expressions[i]);
                        assert(arg_expr);

                        array_append(&arg_exprs, arg_expr);
                    }
                }

                return ast_call_expression_new(allocator, ident_expr, arg_exprs,
                                               ptn->call.is_builtin);
                break;
            }

            case Expression_PTN_Kind::IDENTIFIER:
            {
                AST_Identifier* identifier = ast_identifier_new(allocator, ptn->identifier->atom);
                return ast_identifier_expression_new(allocator, identifier);

                break;
            }

            case Expression_PTN_Kind::BINARY: assert(false);
            case Expression_PTN_Kind::UNARY: assert(false);
            case Expression_PTN_Kind::DOT: assert(false);
            case Expression_PTN_Kind::COMPOUND: assert(false);

            case Expression_PTN_Kind::NUMBER_LITERAL:
            {
                assert(((int64_t)ptn->number_literal.value.u64) == ptn->number_literal.value.s64);
                return ast_number_literal_expression_new(allocator, ptn->number_literal.value.s64);
                break;
            }

            case Expression_PTN_Kind::STRING_LITERAL: assert(false);
            case Expression_PTN_Kind::ARRAY_TYPE: assert(false);
            case Expression_PTN_Kind::POINTER_TYPE: assert(false);
            case Expression_PTN_Kind::POLY_TYPE: assert(false);
        }

        assert(false);
    }

    AST_Type_Spec* ast_create_type_spec_from_ptn(Allocator* allocator, PT_Node* ptn)
    {
        switch (ptn->kind)
        {
            case PT_Node_Kind::INVALID: assert(false);
            case PT_Node_Kind::IDENTIFIER: assert(false);

            case PT_Node_Kind::FUNCTION_PROTO:
            {
                auto function_ptn = (Function_Proto_PTN*)ptn;

                Array<AST_Type_Spec*> ast_param_types = {};
                if (function_ptn->parameters.count)
                {
                    array_init(allocator, &ast_param_types);
                    for (int64_t i = 0; i < function_ptn->parameters.count; i++)
                    {
                        AST_Type_Spec* ast_param_type = nullptr;
                            ast_create_type_spec_from_ptn(allocator,
                                                          &function_ptn->parameters[i]->self);
                        assert(ast_param_type);
                        
                        array_append(&ast_param_types, ast_param_type);
                    }

                }

                AST_Type_Spec* ast_return_type = nullptr;
                if (function_ptn->return_type_expression)
                {
                    ast_return_type =
                        ast_create_type_spec_from_ptn(allocator,
                                                      &function_ptn->return_type_expression->self);
                    assert(ast_return_type);
                }

                return ast_function_type_spec_new(allocator, ast_param_types, ast_return_type);
                break;
            }

            case PT_Node_Kind::PARAMETER: assert(false);
            case PT_Node_Kind::EXPRESSION_LIST: assert(false);
            case PT_Node_Kind::DECLARATION: assert(false);
            case PT_Node_Kind::STATEMENT: assert(false);

            case PT_Node_Kind::EXPRESSION:
            {
                return ast_create_type_spec_from_expression_ptn(allocator, (Expression_PTN*)ptn);
                break; 
            }
        }

        assert(false);
    }

    AST_Type_Spec* ast_create_type_spec_from_expression_ptn(Allocator* allocator,
                                                            Expression_PTN* ptn)
    {
        assert(allocator);

        switch (ptn->kind)
        {
            case Expression_PTN_Kind::INVALID: assert(false);
            case Expression_PTN_Kind::CALL: assert(false);

            case Expression_PTN_Kind::IDENTIFIER:
            {
                AST_Identifier* ident = ast_identifier_new(allocator, ptn->identifier->atom);
                return ast_identifier_type_spec_new(allocator, ident);
                break;
            }

            case Expression_PTN_Kind::BINARY: assert(false);
            case Expression_PTN_Kind::UNARY: assert(false);
            case Expression_PTN_Kind::DOT: assert(false);
            case Expression_PTN_Kind::COMPOUND: assert(false);
            case Expression_PTN_Kind::NUMBER_LITERAL: assert(false);
            case Expression_PTN_Kind::STRING_LITERAL: assert(false);
            case Expression_PTN_Kind::ARRAY_TYPE: assert(false);
            case Expression_PTN_Kind::POINTER_TYPE: assert(false);
            case Expression_PTN_Kind::POLY_TYPE: assert(false);
        }

        assert(false);
        return nullptr;
    }

    AST_Identifier* ast_identifier_new(Allocator* allocator, Atom& atom)
    {
        auto result = ast_node_new<AST_Identifier>(allocator);

        result->atom = atom;

        return result;
    }

    AST_Module* ast_module_new(Allocator* allocator, Array<AST_Declaration*> decls)
    {
        auto result = ast_node_new<AST_Module>(allocator);

        result->declarations = decls;
        
        return result;
    }

    AST_Declaration* ast_declaration_new(Allocator* allocator, AST_Declaration_Kind kind, 
                                         AST_Identifier* identifier)
    {
        auto result = ast_node_new<AST_Declaration>(allocator);

        result->kind = kind;
        result->identifier = identifier;

        return result;
    }

    AST_Declaration* ast_variable_declaration_new(Allocator* allocator, AST_Identifier* identifier,
                                                  AST_Type_Spec* type_spec,
                                                  AST_Expression* init_expr)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::VARIABLE, identifier);

        result->variable.type_spec = type_spec;
        result->variable.init_expression = init_expr;

        return  result;
    }

    AST_Declaration* ast_function_declaration_new(Allocator* allocator, AST_Identifier* identifier,
                                                  AST_Type_Spec* type_spec, AST_Statement* body)
    {
        assert(body->kind == AST_Statement_Kind::BLOCK);

        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::FUNCTION, identifier);

        result->function.type_spec = type_spec;
        result->function.body = body;

        return result;
    }

    AST_Statement* ast_statement_new(Allocator* allocator, AST_Statement_Kind kind)
    {
        auto result = ast_node_new<AST_Statement>(allocator);

        result->kind = kind;

        return result;
    }

    AST_Statement* ast_block_statement_new(Allocator* allocator, Array<AST_Statement*> statements)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::BLOCK);

        result->block.statements = statements;

        return result;
    }

    AST_Statement* ast_return_statement_new(Allocator* allocator, AST_Expression* return_expr)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::RETURN);

        result->expression = return_expr;

        return result;
    }

    AST_Statement* ast_declaration_statement_new(Allocator* allocator,
                                                 AST_Declaration* declaration)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::DECLARATION);

        result->declaration = declaration;

        return result;
    }

    AST_Statement* ast_expression_statement_new(Allocator* allocator, AST_Expression* expression)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::EXPRESSION);

        result->expression = expression;

        return result;
    }

    AST_Expression* ast_expression_new(Allocator* allocator, AST_Expression_Kind kind)
    {
        auto result = ast_node_new<AST_Expression>(allocator); 

        result->kind = kind;

        return result;
    }

    AST_Expression* ast_identifier_expression_new(Allocator* allocator, AST_Identifier* identifier)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::IDENTIFIER);

        result->identifier = identifier;

        return result;
    }

    AST_Expression* ast_call_expression_new(Allocator* allocator, AST_Expression* ident_expr,
                                            Array<AST_Expression*> arg_expressions,
                                            bool is_builtin)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::CALL);

        result->call.ident_expression = ident_expr;
        result->call.arg_expressions = arg_expressions;
        result->call.is_builtin = is_builtin;

        return result;
    }

    AST_Expression* ast_number_literal_expression_new(Allocator* allocator, int64_t value)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::NUMBER_LITERAL);
        
        result->number_literal.s64 = value;

        return result;
    }

    AST_Type_Spec* ast_type_spec_new(Allocator* allocator, AST_Type_Spec_Kind kind)
    {
        AST_Type_Spec* result = ast_node_new<AST_Type_Spec>(allocator);

        result->kind = kind;

        return result;
    }

    AST_Type_Spec* ast_identifier_type_spec_new(Allocator* allocator, AST_Identifier* identifier)
    {
        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::IDENTIFIER);

        result->identifier = identifier;

        return result;
    }

    AST_Type_Spec* ast_function_type_spec_new(Allocator* allocator,
                                              Array<AST_Type_Spec*> param_type_specs,
                                              AST_Type_Spec* return_type_spec)
    {
        assert(allocator);

        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::FUNCTION);

        result->function.parameter_type_specs = param_type_specs;
        result->function.return_type_spec = return_type_spec;

        return result;
    }

    void ast_print_indent(uint64_t indent)
    {
        for (uint64_t i = 0; i < indent; i++)
        {
            printf("    ");
        }
    }

    void ast_print(AST_Node* ast_node)
    {
        assert(ast_node);

        switch (ast_node->kind)
        {
            case AST_Node_Kind::INVALID: assert(false);

            case AST_Node_Kind::MODULE:
            {
                auto module = (AST_Module*)ast_node;
                for (int64_t i = 0; i < module->declarations.count; i++)
                {
                    ast_print_declaration(module->declarations[i], 0);
                }
                break;
            }

            case AST_Node_Kind::IDENTIFIER: assert(false);
            case AST_Node_Kind::DECLARATION: assert(false);
            case AST_Node_Kind::STATEMENT: assert(false);
            case AST_Node_Kind::EXPRESSION: assert(false);
            case AST_Node_Kind::TYPE_SPEC: assert(false);
        }
    }

    void ast_print_declaration(AST_Declaration* ast_decl, uint64_t indent)
    {
        ast_print_indent(indent);

        printf("%s :", ast_decl->identifier->atom.data);

        switch (ast_decl->kind)
        {
            case AST_Declaration_Kind::INVALID: assert(false);

            case AST_Declaration_Kind::VARIABLE:
            {
                if (ast_decl->variable.type_spec)
                {
                    printf(" ");
                    ast_print_type_spec(ast_decl->variable.type_spec);
                    printf(" ");
                }
                
                if (ast_decl->variable.init_expression)
                {
                    printf("= ");
                    ast_print_expression(ast_decl->variable.init_expression, 0);
                    printf(";");
                }
                break;
            }

            case AST_Declaration_Kind::FUNCTION:
            {
                printf(": ");
                ast_print_type_spec(ast_decl->function.type_spec);
                ast_print_statement(ast_decl->function.body, 0);
                printf("\n");
                break;
            }
        }
    }

    void ast_print_statement(AST_Statement* ast_stmt, uint64_t indent)
    {
        ast_print_indent(indent);

        switch (ast_stmt->kind)
        {
            case AST_Statement_Kind::INVALID: assert(false);

            case AST_Statement_Kind::BLOCK:
            {
                printf("\n{\n");
                for (int64_t i = 0; i < ast_stmt->block.statements.count; i++)
                {
                    ast_print_statement(ast_stmt->block.statements[i], indent + 1);
                    printf("\n");
                }
                printf("}\n");
                break;  
            }

            case AST_Statement_Kind::RETURN:
            {
                printf("return");
                if (ast_stmt->expression)
                {
                    printf(" ");
                    ast_print_expression(ast_stmt->expression, 0);
                    printf(";");
                }
                break;
            }

            case AST_Statement_Kind::DECLARATION:
            {
                ast_print_declaration(ast_stmt->declaration, 0);
                break;
            }

            case AST_Statement_Kind::EXPRESSION:
            {
                ast_print_expression(ast_stmt->expression, 0);
                printf(";");
            }
        } 
    }

    void ast_print_type_spec(AST_Type_Spec* type_spec)
    {
        switch (type_spec->kind)
        {
            case AST_Type_Spec_Kind::INVALID: assert(false);

            case AST_Type_Spec_Kind::IDENTIFIER:
            {
                printf("%s", type_spec->identifier->atom.data);
                break; 
            }

            case AST_Type_Spec_Kind::FUNCTION:
            {
                printf("func (");
                for (int64_t i = 0; i < type_spec->function.parameter_type_specs.count; i++)
                {
                    if (i > 0) printf(", ");
                    ast_print_type_spec(type_spec->function.parameter_type_specs[i]);
                }
                printf(")");
                if (type_spec->function.return_type_spec)
                {
                    printf(" -> ");
                    ast_print_type_spec(type_spec->function.return_type_spec);
                }
                break;
            }
        }
    }

    void ast_print_expression(AST_Expression* ast_expr, uint64_t indent)
    {
        ast_print_indent(indent);

        switch (ast_expr->kind)
        {
            case AST_Expression_Kind::INVALID: assert(false);

            case AST_Expression_Kind::IDENTIFIER:
            {
                printf("%s", ast_expr->identifier->atom.data);
                break;
            }

            case AST_Expression_Kind::CALL: 
            {
                ast_print_expression(ast_expr->call.ident_expression, 0);
                printf("(");
                for (int64_t i = 0; i < ast_expr->call.arg_expressions.count; i++)
                {
                    if (i > 0) printf(", ");
                    ast_print_expression(ast_expr->call.arg_expressions[i], 0);
                }
                printf(")");
                break;
            }

            case AST_Expression_Kind::NUMBER_LITERAL:
            {
                printf("%ld", ast_expr->number_literal.s64);
            }
        }
    }
}
