
#include "ast.h"

#include "parse_tree_node.h"

#include <cassert>

namespace Zodiac
{

    AST_Node_Kind AST_Module::_kind = AST_Node_Kind::MODULE;
    AST_Node_Kind AST_Identifier::_kind = AST_Node_Kind::IDENTIFIER;
    AST_Node_Kind AST_Statement::_kind = AST_Node_Kind::STATEMENT;
    AST_Node_Kind AST_Type::_kind = AST_Node_Kind::TYPE;

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
            case Declaration_PTN_Kind::VARIABLE: assert(false);
            case Declaration_PTN_Kind::CONSTANT: assert(false);

            case Declaration_PTN_Kind::FUNCTION:
            {
                AST_Type* ast_type = ast_create_type_from_ptn(allocator,
                                                              &ptn->function.prototype->self);
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

            case Statement_PTN_Kind::DECLARATION: assert(false); 

            case Statement_PTN_Kind::EXPRESSION:
            {
                auto ast_expression = ast_create_expression_from_ptn(allocator, ptn->expression);
                return ast_expression_statement_new(allocator, ast_expression);
                break;
            }

            case Statement_PTN_Kind::RETURN: assert(false); 
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

            case Expression_PTN_Kind::CALL: assert(false);

            case Expression_PTN_Kind::IDENTIFIER: assert(false);
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
    }

    AST_Type* ast_create_type_from_ptn(Allocator* allocator, PT_Node* ptn)
    {
        switch (ptn->kind)
        {
            case PT_Node_Kind::INVALID: assert(false);
            case PT_Node_Kind::IDENTIFIER: assert(false);

            case PT_Node_Kind::FUNCTION_PROTO:
            {
                auto function_ptn = (Function_Proto_PTN*)ptn;

                Array<AST_Type*> ast_param_types = {};
                if (function_ptn->parameters.count)
                {
                    array_init(allocator, &ast_param_types);
                    for (int64_t i = 0; i < function_ptn->parameters.count; i++)
                    {
                        AST_Type* ast_param_type = nullptr;
                            ast_create_type_from_ptn(allocator, &function_ptn->parameters[i]->self);
                        assert(ast_param_type);
                        
                        array_append(&ast_param_types, ast_param_type);
                    }

                }

                AST_Type* ast_return_type = nullptr;
                if (function_ptn->return_type_expression)
                {
                    ast_create_type_from_ptn(allocator, &function_ptn->return_type_expression->self);
                    assert(ast_return_type);
                }

                return ast_function_type_new(allocator, ast_param_types, ast_return_type);
                break;
            }

            case PT_Node_Kind::PARAMETER: assert(false);
            case PT_Node_Kind::EXPRESSION_LIST: assert(false);
            case PT_Node_Kind::DECLARATION: assert(false);
            case PT_Node_Kind::STATEMENT: assert(false);
            case PT_Node_Kind::EXPRESSION: assert(false);
        }

        assert(false);
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

    AST_Declaration* ast_function_declaration_new(Allocator* allocator, AST_Identifier* identifier,
                                                  AST_Type* type, AST_Statement* body)
    {
        assert(allocator);
        assert(identifier);
        assert(type);
        assert(body);
        assert(false);
        return nullptr;
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

    AST_Statement* ast_expression_statement_new(Allocator* allocator, AST_Expression* expression)
    {
        assert(allocator);
        assert(expression);
        assert(false);
    }

    AST_Type* ast_type_new(Allocator* allocator, AST_Type_Kind kind)
    {
        AST_Type* result = ast_node_new<AST_Type>(allocator);

        result->kind = kind;

        return result;
    }

    AST_Type* ast_function_type_new(Allocator* allocator, Array<AST_Type*> param_types,
                                    AST_Type* return_type)
    {
        assert(allocator);

        auto result = ast_type_new(allocator, AST_Type_Kind::FUNCTION);

        result->function.parameter_types = param_types;
        result->function.return_type = return_type;

        return result;
    }

    void ast_print(AST_Node* ast_node)
    {
        assert(ast_node);
        assert(false);
    }
}
