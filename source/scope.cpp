
#include "scope.h"

#include "ast.h"
#include "string_builder.h"

namespace Zodiac
{
    void scope_populate_ast(Allocator* allocator, AST_Node* anode, Scope *parent_scope)
    {
        assert(allocator);

        if (parent_scope) assert(anode->kind != AST_Node_Kind::MODULE);
        else assert(anode->kind == AST_Node_Kind::MODULE);

        switch (anode->kind)
        {
            case AST_Node_Kind::INVALID: assert(false);

            case AST_Node_Kind::MODULE:
            {
                auto ast_module = static_cast<AST_Module*>(anode);
                assert(ast_module->module_scope == nullptr);

                ast_module->module_scope = scope_new(allocator, Scope_Kind::MODULE, nullptr,
                                                     ast_module->declarations.count);

                for (int64_t i = 0; i < ast_module->declarations.count; i++)
                {
                    scope_add_declaration(ast_module->module_scope, ast_module->declarations[i]);
                }

                for (int64_t i = 0; i < ast_module->declarations.count; i++)
                {
                    scope_populate_ast(allocator, ast_module->declarations[i], ast_module->module_scope);
                }

                break;
            }

            case AST_Node_Kind::IDENTIFIER: assert(false);

            case AST_Node_Kind::DECLARATION:
            {
                auto ast_decl = static_cast<AST_Declaration*>(anode);
                scope_populate_declaration_ast(allocator, ast_decl, parent_scope);
                break;
            }

            case AST_Node_Kind::STATEMENT: assert(false);
            case AST_Node_Kind::EXPRESSION: assert(false);
            case AST_Node_Kind::TYPE_SPEC: assert(false);
        }
    }

    void scope_populate_declaration_ast(Allocator *allocator, AST_Declaration *ast_decl, Scope *parent_scope)
    {
        assert(allocator);
        switch (ast_decl->kind)
        {
            case AST_Declaration_Kind::INVALID: assert(false);
            case AST_Declaration_Kind::IMPORT: assert(false);

            case AST_Declaration_Kind::VARIABLE:
            {
                scope_add_declaration(parent_scope, ast_decl);
                break;
            }

            case AST_Declaration_Kind::CONSTANT: assert(false);

            case AST_Declaration_Kind::PARAMETER: 
            {
                // Do nothing for now
                break;
            }

            case AST_Declaration_Kind::FUNCTION:
            {
                assert(ast_decl->function.parameter_scope == nullptr);
                assert(parent_scope);

                ast_decl->function.parameter_scope =
                    scope_new(allocator, Scope_Kind::PARAMETER, parent_scope,
                              ast_decl->function.parameter_declarations.count);

                for (int64_t i = 0; i < ast_decl->function.parameter_declarations.count; i++)
                {
                    scope_add_declaration(ast_decl->function.parameter_scope,
                                          ast_decl->function.parameter_declarations[i]);
                }

                for (int64_t i = 0; i < ast_decl->function.parameter_declarations.count; i++)
                {
                    scope_populate_ast(allocator, ast_decl->function.parameter_declarations[i],
                                       ast_decl->function.parameter_scope);
                }

                scope_populate_statement_ast(allocator, ast_decl->function.body,
                                             ast_decl->function.parameter_scope);
                break;
            }

            case AST_Declaration_Kind::STRUCTURE: assert(false);
        }
    }

    void scope_populate_statement_ast(Allocator *allocator, AST_Statement *ast_stmt, Scope *parent_scope)
    {
        assert(parent_scope);

        switch (ast_stmt->kind)
        {
            case AST_Statement_Kind::INVALID: assert(false);

            case AST_Statement_Kind::BLOCK:
            {
                assert(ast_stmt->block.scope == nullptr);
                ast_stmt->block.scope = scope_new(allocator, Scope_Kind::BLOCK, parent_scope);

                for (int64_t i = 0; i < ast_stmt->block.statements.count; i++)
                {
                    scope_populate_statement_ast(allocator, ast_stmt->block.statements[i],
                                                 ast_stmt->block.scope);
                }
                break;
            }

            case AST_Statement_Kind::ASSIGNMENT: assert(false);

            case AST_Statement_Kind::RETURN:
            {
                if (ast_stmt->expression)
                {
                    scope_populate_expression_ast(allocator, ast_stmt->expression);
                }
                break;
            }

            case AST_Statement_Kind::DECLARATION:
            {
                scope_populate_declaration_ast(allocator, ast_stmt->declaration, parent_scope);
                break;
            }

            case AST_Statement_Kind::EXPRESSION:
            {
                scope_populate_expression_ast(allocator, ast_stmt->expression);
                break;
            }

        }
    }

    void scope_populate_expression_ast(Allocator* allocator, AST_Expression *ast_expr)
    {
        assert(allocator);

        switch (ast_expr->kind)
        {
            case AST_Expression_Kind::INVALID: assert(false);

            case AST_Expression_Kind::IDENTIFIER: break;

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);
            case AST_Expression_Kind::DOT: assert(false);
            case AST_Expression_Kind::BINARY: assert(false);
            case AST_Expression_Kind::UNARY: assert(false);

            case AST_Expression_Kind::CALL: break;

            case AST_Expression_Kind::COMPOUND: assert(false);
            case AST_Expression_Kind::NUMBER_LITERAL: assert(false);
            case AST_Expression_Kind::STRING_LITERAL: assert(false);

        }
    }

    void scope_add_declaration(Scope *scope, AST_Declaration *adecl)
    {
        if (scope->decl_count >= scope->decl_cap)
        {
            scope_grow(scope);
        }

        scope->declarations[scope->decl_count] = adecl;
        scope->decl_count++;
    }

    void scope_grow(Scope *scope)
    {
        assert(scope);
        assert(false);
    }

    Scope *scope_new(Allocator* allocator, Scope_Kind kind, Scope *parent, int64_t initial_cap/*=4*/)
    {
        auto mem = alloc(allocator, sizeof(Scope) + (initial_cap * sizeof(AST_Declaration*)));
        assert(mem);

        if (parent == nullptr) assert(kind == Scope_Kind::MODULE);

        Scope *result = (Scope*)mem;
        result->kind = kind;
        result->declarations = (AST_Declaration**)&result[1];
        result->decl_count = 0;
        result->decl_cap = initial_cap;
        result->parent = parent;
        result->allocator = allocator;

        return result;
    }


    void scope_print_indent(String_Builder *sb, int64_t indent)
    {
        for (int64_t i = 0; i < indent; i++)
        {
            string_builder_append(sb, "    ");
        }
    }

    void scope_print(String_Builder *sb, Scope *scope, int64_t indent/*=0*/)
    {
        assert(sb);

        if (scope->decl_count <= 0) return;

        scope_print_indent(sb, indent);

        switch (scope->kind)
        {
            case Scope_Kind::INVALID: assert(false);

            case Scope_Kind::MODULE:
            {
                string_builder_append(sb, "MODULE:\n");
                break;
            }

            case Scope_Kind::PARAMETER:
            {
                string_builder_append(sb, "PARAMETER:\n");
                break;
            }

            case Scope_Kind::BLOCK:
            {
                string_builder_append(sb, "BLOCK:\n");
                break;
            }
        }

        indent += 1;

        for (int64_t i = 0; i < scope->decl_count; i++)
        {
            AST_Declaration *decl = scope->declarations[i];
            assert(decl->identifier);
            auto &atom = decl->identifier->atom;
            scope_print_indent(sb, indent);
            string_builder_append(sb, atom.data, atom.length);
            ast_print_scope(sb, decl, indent + 1);
            string_builder_append(sb, "\n");
        }
    }
}
