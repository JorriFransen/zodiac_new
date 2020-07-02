#include "resolver.h"

#include "ast.h"
#include "builtin.h"
#include "scope.h"

#include <cassert>

namespace Zodiac
{
    void resolver_init(Allocator *allocator, Resolver *resolver)
    {
        assert(allocator);
        assert(resolver);

        resolver->allocator = allocator;

        queue_init(allocator, &resolver->ident_job_queue);
        queue_init(allocator, &resolver->type_job_queue);
        queue_init(allocator, &resolver->size_job_queue);
    }

    void start_resolving(Resolver *resolver, AST_Node *ast_node, bool blocking)
    {
        assert(resolver);
        assert(ast_node);

        assert(blocking);

        assert(resolver->root_node == nullptr);
        resolver->root_node = ast_node;

        assert(ast_node->kind == AST_Node_Kind::MODULE);

        auto ast_module = static_cast<AST_Module*>(ast_node);
        for (int64_t i = 0; i < ast_module->declarations.count; i++)
        {
            AST_Declaration *decl = ast_module->declarations[i];
            queue_ident_job(resolver, decl, ast_module->module_scope);
        }

        if (blocking)
        {
             start_resolve_pump(resolver);
        }
        else
        {
            assert(false);
        }
    }

    Resolve_Result finish_resolving(Resolver *resolver)
    {
        assert(resolver);

        assert(queue_count(&resolver->ident_job_queue) == 0);
        assert(queue_count(&resolver->type_job_queue) == 0);
        assert(queue_count(&resolver->size_job_queue) == 0);

        assert(false);
    }

    void start_resolve_pump(Resolver *resolver)
    {
        bool done = false;

        while (!done)
        {
            while (queue_count(&resolver->ident_job_queue))
            {
                auto job = queue_dequeue(&resolver->ident_job_queue);
                bool job_done = try_resolve_job(resolver, job);

                if (!job_done)
                {
                    queue_enqueue(&resolver->ident_job_queue, job);
                }
            }

            if (queue_count(&resolver->ident_job_queue) == 0)
            {
                done = true;
            }
        }
    }

    bool try_resolve_job(Resolver *resolver, Resolve_Job *job)
    {
        assert(resolver);
        assert(job);

        switch (job->kind)
        {
            case Resolve_Job_Kind::INVALID: assert(false);

            case Resolve_Job_Kind::IDENTIFIER:
            {
                return try_resolve_identifiers(resolver, job->ast_node, job->node_scope);
                break;
            }

            case Resolve_Job_Kind::TYPE: assert(false);
            case Resolve_Job_Kind::SIZE: assert(false);
        }

        assert(false);
        return false;
    }

    bool try_resolve_identifiers(Resolver *resolver, AST_Node *ast_node, Scope *scope)
    {
        assert(resolver);
        assert(ast_node);
        assert(scope);

        if (ast_node->flags & AST_NODE_FLAG_RESOLVED) return true;

        switch (ast_node->kind)
        {
            case AST_Node_Kind::INVALID: assert(false);
            case AST_Node_Kind::MODULE: assert(false);
            case AST_Node_Kind::IDENTIFIER: assert(false);

            case AST_Node_Kind::DECLARATION:
            {
                auto decl = static_cast<AST_Declaration*>(ast_node);
                return try_resolve_identifiers(resolver, decl, scope);
            }

            case AST_Node_Kind::EXPRESSION:
            {
                auto expr = static_cast<AST_Expression*>(ast_node);
                return try_resolve_identifiers(resolver, expr, scope);
            }

            case AST_Node_Kind::STATEMENT: 
            {
                auto stmt = static_cast<AST_Statement*>(ast_node);
                return try_resolve_identifiers(resolver, stmt, scope);
            }

            case AST_Node_Kind::TYPE_SPEC: assert(false);
        }

        assert(false);
        return false;
    }

    bool try_resolve_identifiers(Resolver *resolver, AST_Declaration *ast_decl, Scope *scope)
    {
        assert(resolver);
        assert(ast_decl);

        switch (ast_decl->kind)
        {
            case AST_Declaration_Kind::INVALID: assert(false);
            case AST_Declaration_Kind::IMPORT: assert(false);
            case AST_Declaration_Kind::VARIABLE: assert(false);
            case AST_Declaration_Kind::CONSTANT: assert(false);
            case AST_Declaration_Kind::PARAMETER: assert(false);

            case AST_Declaration_Kind::FUNCTION:
            {
                if (!try_resolve_identifiers(resolver, ast_decl->function.type_spec))
                {
                    assert(false);
                }

                for (int64_t i = 0; i < ast_decl->function.parameter_declarations.count; i++)
                {
                    auto param_decl = ast_decl->function.parameter_declarations[i];
                    if (!try_resolve_identifiers(resolver, param_decl,
                                                 ast_decl->function.parameter_scope))
                    {
                        assert(false);
                    }
                }

                if (!try_resolve_identifiers(resolver, ast_decl->function.body, scope))
                {
                    assert(false);
                }

                return true;
                break;
            }

            case AST_Declaration_Kind::STRUCTURE: assert(false);
            case AST_Declaration_Kind::POLY_TYPE: assert(false);
        }

        assert(false);
        return false;
    }

    bool try_resolve_identifiers(Resolver *resolver, AST_Statement *ast_stmt, Scope *scope)
    {
        switch (ast_stmt->kind)
        {
            case AST_Statement_Kind::INVALID: assert(false);

            case AST_Statement_Kind::BLOCK:
            {
                for (int64_t i = 0; i < ast_stmt->block.statements.count; i++)
                {
                    if (!try_resolve_identifiers(resolver, ast_stmt->block.statements[i],
                                                 ast_stmt->block.scope))
                    {
                        assert(false);
                    }
                }

                return true;
            }

            case AST_Statement_Kind::ASSIGNMENT: assert(false);
            case AST_Statement_Kind::RETURN: assert(false);
            case AST_Statement_Kind::DECLARATION: assert(false);

            case AST_Statement_Kind::EXPRESSION:
            {
                return try_resolve_identifiers(resolver, ast_stmt->expression, scope);
            }
        }

        assert(false);
        return false;
    }

    bool try_resolve_identifiers(Resolver *resolver, AST_Expression* ast_expr, Scope *scope)
    {
        assert(resolver);
        switch (ast_expr->kind)
        {
            case AST_Expression_Kind::INVALID: assert(false);
            case AST_Expression_Kind::DOT: assert(false);

            case AST_Expression_Kind::IDENTIFIER:
            {
                auto decl = scope_find_declaration(scope, ast_expr->identifier);
                assert(decl);
                return true;
                break;
            }

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);
            case AST_Expression_Kind::BINARY: assert(false);
            case AST_Expression_Kind::UNARY: assert(false);

            case AST_Expression_Kind::CALL:
            {
                if (!ast_expr->call.is_builtin)
                {
                    if (!try_resolve_identifiers(resolver, ast_expr->call.ident_expression, scope))
                    {
                        assert(false);
                    }
                }
                else
                {
                    auto atom = ast_expr->call.ident_expression->identifier->atom;
                    if (atom == Builtin::atom_exit)
                    {
                        // okay
                    }
                    else assert(false);
                }

                for (int64_t i = 0; i < ast_expr->call.arg_expressions.count; i++)
                {
                    auto arg_expr = ast_expr->call.arg_expressions[i];
                    if (!try_resolve_identifiers(resolver, arg_expr, scope))
                    {
                        assert(false);
                    }
                }

                return true;
            }

            case AST_Expression_Kind::COMPOUND: assert(false);
                                                
            case AST_Expression_Kind::NUMBER_LITERAL: return true; 

            case AST_Expression_Kind::STRING_LITERAL: assert(false);
        }

        assert(false);
        return false;
    }

    bool try_resolve_identifiers(Resolver *resolver, AST_Type_Spec *ast_ts)
    {
        assert(resolver);
        assert(ast_ts);
        
        switch (ast_ts->kind)
        {
            case AST_Type_Spec_Kind::INVALID: assert(false);
            case AST_Type_Spec_Kind::IDENTIFIER: assert(false);
            case AST_Type_Spec_Kind::POINTER: assert(false);
            case AST_Type_Spec_Kind::DOT: assert(false);

            case AST_Type_Spec_Kind::FUNCTION:
            {
                for (int64_t i = 0; i < ast_ts->function.parameter_type_specs.count; i++)
                {
                    auto param_ts = ast_ts->function.parameter_type_specs[i];
                    if (!try_resolve_identifiers(resolver, param_ts))
                    {
                        assert(false);
                    }
                }

                if (ast_ts->function.return_type_spec)
                {
                    if (!try_resolve_identifiers(resolver,
                                                 ast_ts->function.return_type_spec))
                    {
                        assert(false);
                    }
                }

                return true;
                break;
            }

            case AST_Type_Spec_Kind::ARRAY: assert(false);
            case AST_Type_Spec_Kind::TEMPLATED: assert(false);
            case AST_Type_Spec_Kind::POLY_IDENTIFIER: assert(false);
        }

        assert(false);
        return false;
    }

    void queue_ident_job(Resolver *resolver, AST_Node *ast_node, Scope *scope)
    {
        assert(resolver);
        assert(ast_node);

        auto allocator = resolver->allocator;
        assert(allocator);

        auto job = resolve_job_ident_new(allocator, ast_node, scope);
        queue_enqueue(&resolver->ident_job_queue, job);
        assert(job);
    }

    Resolve_Job *resolve_job_new(Allocator *allocator, Resolve_Job_Kind kind, AST_Node *ast_node, 
                                 Scope *scope)
    {
        auto result = alloc_type<Resolve_Job>(allocator);
        assert(result);
        result->kind = kind;
        result->ast_node = ast_node;
        result->node_scope = scope;
        return result;
    }

    Resolve_Job *resolve_job_ident_new(Allocator *allocator, AST_Node *ast_node, Scope *scope)
    {
        return resolve_job_new(allocator, Resolve_Job_Kind::IDENTIFIER, ast_node, scope);
    }
}
