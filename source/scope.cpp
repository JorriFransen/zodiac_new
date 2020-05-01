
#include "scope.h"

#include "ast.h"
#include "string_builder.h"

namespace Zodiac
{
    void scope_populate_ast(Allocator* allocator, AST_Node* anode)
    {
        assert(allocator);
        switch (anode->kind)
        {
            case AST_Node_Kind::INVALID: assert(false);

            case AST_Node_Kind::MODULE:
            {
                auto ast_module = static_cast<AST_Module*>(anode);
                assert(ast_module->module_scope == nullptr);

                ast_module->module_scope = scope_new(allocator, Scope_Kind::MODULE,
                                                     ast_module->declarations.count);

                for (int64_t i = 0; i < ast_module->declarations.count; i++)
                {
                    scope_add_declaration(ast_module->module_scope, ast_module->declarations[i]);
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

    Scope *scope_new(Allocator* allocator, Scope_Kind kind, int64_t initial_cap/*=4*/)
    {
        auto mem = alloc(allocator, sizeof(Scope) + (initial_cap * sizeof(AST_Declaration*)));
        assert(mem);

        Scope *result = (Scope*)mem;
        result->kind = kind;
        result->declarations = (AST_Declaration**)&result[1];
        result->decl_count = 0;
        result->decl_cap = initial_cap;
        result->allocator = allocator;

        return result;
    }

    void scope_print(String_Builder *sb, Scope* scope)
    {
        assert(sb);

        switch (scope->kind)
        {
            case Scope_Kind::INVALID: assert(false);

            case Scope_Kind::MODULE:
            {
                string_builder_append(sb, "MODULE:\n");

                for (int64_t i = 0; i < scope->decl_count; i++)
                {
                    AST_Declaration *decl = scope->declarations[i];
                    assert(decl->identifier);
                    auto &atom = decl->identifier->atom;
                    string_builder_append(sb, "    ");
                    string_builder_append(sb, atom.data, atom.length);
                    string_builder_append(sb, "\n");
                }
                break;
            }
        }
    }
}
