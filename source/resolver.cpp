#include "resolver.h"

#include "ast.h"
#include "builtin.h"
#include "scope.h"

#include <cassert>

namespace Zodiac
{
    void resolver_init(Allocator *allocator, Resolver *resolver, Build_Data *build_data)
    {
        assert(allocator);
        assert(resolver);

        resolver->allocator = allocator;
        resolver->build_data = build_data;

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

        return Resolve_Result {};
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
                else
                {
                    queue_type_job(resolver, job->declaration, job->node_scope);
                    free_job(resolver, job);
                }
            }

            while (queue_count(&resolver->type_job_queue))
            {
                auto job = queue_dequeue(&resolver->type_job_queue);
                bool job_done = try_resolve_job(resolver, job);

                if (!job_done)
                {
                    queue_enqueue(&resolver->type_job_queue, job);
                }
                else
                {
                    // Size jobs are queued when types are first created
                    free_job(resolver, job);
                }
            }

            while (queue_count(&resolver->size_job_queue))
            {
                auto job = queue_dequeue(&resolver->size_job_queue);
                bool job_done = try_resolve_job(resolver, job);

                if (!job_done)
                {
                    queue_enqueue(&resolver->size_job_queue, job);
                }
                else
                {
                    free_job(resolver, job);
                }
            }

            if (queue_count(&resolver->ident_job_queue) == 0 &&
                queue_count(&resolver->type_job_queue) == 0 &&
                queue_count(&resolver->size_job_queue) == 0)

            {
                done = true;
            }
        }
    }

    bool try_resolve_job(Resolver *resolver, Resolve_Job *job)
    {
        assert(resolver);
        assert(job);

        bool result = false;

        switch (job->kind)
        {
            case Resolve_Job_Kind::INVALID: assert(false);

            case Resolve_Job_Kind::IDENTIFIER:
            {
                result = try_resolve_identifiers(resolver, job->ast_node, job->node_scope);
                if (result)
                {
                    assert(job->ast_node->flags & AST_NODE_FLAG_RESOLVED_ID);
                }
                break;
            }

            case Resolve_Job_Kind::TYPE:
            {
                result = try_resolve_types(resolver, job->ast_node, job->node_scope);
                if (result)
                {
                    assert(job->ast_node->flags & AST_NODE_FLAG_TYPED);
                }
                break;
            }

            case Resolve_Job_Kind::SIZE:
            {
                result = try_resolve_sizes(resolver, job->ast_node, job->node_scope);
                if (result)
                {
                    assert(job->ast_node->flags & AST_NODE_FLAG_SIZED);
                }
                break;
            }
        }

        return result;
    }

    bool try_resolve_identifiers(Resolver *resolver, AST_Node *ast_node, Scope *scope)
    {
        assert(resolver);
        assert(ast_node);
        assert(scope);

        if (ast_node->flags & AST_NODE_FLAG_RESOLVED_ID) assert(false);

        bool result = false;

        switch (ast_node->kind)
        {
            case AST_Node_Kind::INVALID: assert(false);
            case AST_Node_Kind::MODULE: assert(false);
            case AST_Node_Kind::IDENTIFIER: assert(false);

            case AST_Node_Kind::DECLARATION:
            {
                auto decl = static_cast<AST_Declaration*>(ast_node);
                result = try_resolve_identifiers(resolver, decl, scope);
                break;
            }

            case AST_Node_Kind::EXPRESSION:
            {
                auto expr = static_cast<AST_Expression*>(ast_node);
                result = try_resolve_identifiers(resolver, expr, scope);
                break;
            }

            case AST_Node_Kind::STATEMENT: 
            {
                auto stmt = static_cast<AST_Statement*>(ast_node);
                result = try_resolve_identifiers(resolver, stmt, scope);
                break;
            }

            case AST_Node_Kind::TYPE_SPEC: assert(false);
            case AST_Node_Kind::TYPE: assert(false);
        }

        if (result) assert(ast_node->flags & AST_NODE_FLAG_RESOLVED_ID);

        return result;
    }

    bool try_resolve_identifiers(Resolver *resolver, AST_Declaration *ast_decl, Scope *scope)
    {
        assert(resolver);
        assert(ast_decl);

        bool result = false;

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

                result = true;
                break;
            }

            case AST_Declaration_Kind::STRUCTURE: assert(false);
            case AST_Declaration_Kind::POLY_TYPE: assert(false);
        }

        if (result) ast_decl->flags |= AST_NODE_FLAG_RESOLVED_ID;
        return result;
    }

    bool try_resolve_identifiers(Resolver *resolver, AST_Statement *ast_stmt, Scope *scope)
    {
        bool result = false;

        switch (ast_stmt->kind)
        {
            case AST_Statement_Kind::INVALID: assert(false);

            case AST_Statement_Kind::BLOCK:
            {
                auto nodes = ast_stmt->block.statements;
                for (int64_t i = 0; i < nodes.count; i++)
                {
                    if (!try_resolve_identifiers(resolver, nodes[i], ast_stmt->block.scope))
                    {
                        assert(false);
                    }
                }

                result = true;
                break;
            }

            case AST_Statement_Kind::ASSIGNMENT: assert(false);
            case AST_Statement_Kind::RETURN: assert(false);
            case AST_Statement_Kind::DECLARATION: assert(false);

            case AST_Statement_Kind::EXPRESSION:
            {
                result = try_resolve_identifiers(resolver, ast_stmt->expression, scope);
                break;
            }
        }

        if (result)
        {
            ast_stmt->flags |= AST_NODE_FLAG_RESOLVED_ID;
        }
        return result;
    }

    bool try_resolve_identifiers(Resolver *resolver, AST_Expression* ast_expr, Scope *scope)
    {
        bool result = false;

        assert(!(ast_expr->flags & AST_NODE_FLAG_RESOLVED_ID));

        switch (ast_expr->kind)
        {
            case AST_Expression_Kind::INVALID: assert(false);
            case AST_Expression_Kind::DOT: assert(false);

            case AST_Expression_Kind::IDENTIFIER:
            {
                auto decl = scope_find_declaration(scope, ast_expr->identifier);
                assert(decl);
                assert(ast_expr->identifier->declaration);

                result = true;
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

                result = true;
                break;
            }

            case AST_Expression_Kind::COMPOUND: assert(false);
                                                
            case AST_Expression_Kind::NUMBER_LITERAL: 
            {
                result = true;
                break;
            }

            case AST_Expression_Kind::STRING_LITERAL: assert(false);
        }

        if (result)
        {
            ast_expr->flags |= AST_NODE_FLAG_RESOLVED_ID;
        }

        return result;
    }

    bool try_resolve_identifiers(Resolver *resolver, AST_Type_Spec *ast_ts)
    {
        bool result = false;
        
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

                result = true;
                break;
            }

            case AST_Type_Spec_Kind::ARRAY: assert(false);
            case AST_Type_Spec_Kind::TEMPLATED: assert(false);
            case AST_Type_Spec_Kind::POLY_IDENTIFIER: assert(false);
        }

        if (result)
        {
            ast_ts->flags |= AST_NODE_FLAG_RESOLVED_ID;
        }
        return result;
    }

    bool try_resolve_types(Resolver *resolver, AST_Node *ast_node, Scope *scope)
    {
        assert(resolver);
        assert(ast_node);
        assert(scope);

        assert(ast_node->flags & AST_NODE_FLAG_RESOLVED_ID);
        assert(!(ast_node->flags & AST_NODE_FLAG_TYPED));  

        bool result = false;

        switch (ast_node->kind)
        {
            case AST_Node_Kind::INVALID: assert(false);
            case AST_Node_Kind::MODULE: assert(false);
            case AST_Node_Kind::IDENTIFIER: assert(false);

            case AST_Node_Kind::DECLARATION:
            {
                auto decl = static_cast<AST_Declaration*>(ast_node);
                //assert(decl->type == nullptr);
                if (!(decl->flags & AST_NODE_FLAG_TYPED))
                {
                    result = try_resolve_types(resolver, decl, scope);
                    if (result) assert(decl->type);
                }
                break;
            }

            case AST_Node_Kind::EXPRESSION: assert(false);
            case AST_Node_Kind::STATEMENT: assert(false);
            case AST_Node_Kind::TYPE_SPEC: assert(false);
            case AST_Node_Kind::TYPE: assert(false);
        }

        if (result) assert(ast_node->flags & AST_NODE_FLAG_TYPED);

        return result;
    }

    bool try_resolve_types(Resolver *resolver, AST_Declaration* ast_decl, Scope *scope)
    {
        assert(resolver);
        assert(ast_decl);
        assert(scope);

        assert(ast_decl->flags & AST_NODE_FLAG_RESOLVED_ID);
        assert(!(ast_decl->flags & AST_NODE_FLAG_TYPED));
        //assert(ast_decl->type == nullptr);

        bool result = true;

        switch (ast_decl->kind)
        {
            case AST_Declaration_Kind::INVALID: assert(false);
            case AST_Declaration_Kind::IMPORT: assert(false);
            case AST_Declaration_Kind::VARIABLE: assert(false);
            case AST_Declaration_Kind::CONSTANT: assert(false);
            case AST_Declaration_Kind::PARAMETER: assert(false);

            case AST_Declaration_Kind::FUNCTION:
            {
                if (!ast_decl->function.type_spec->type &&
                    !try_resolve_types(resolver, ast_decl->function.type_spec, scope,
                                       &ast_decl->type))
                {
                    result = false;
                    break;
                }
                else
                {
                    assert(ast_decl->type);
                }

                int param_fail_count = 0;

                auto param_decls = ast_decl->function.parameter_declarations;
                for (int64_t i = 0; i < param_decls.count; i++)
                {
                    bool param_res = try_resolve_types(resolver, param_decls[i],
                                                       ast_decl->function.parameter_scope);
                    if (!param_res) param_fail_count++;
                }

                if (param_fail_count > 0) 
                {
                    result = false;
                    break;
                }

                // Passing a null scope since the block holds it's own scope
                if (!try_resolve_types(resolver, ast_decl->function.body, nullptr))
                {
                    result = false;
                    break;
                }

                if (result)
                {
                    assert(ast_decl->type);
                    assert(ast_decl->function.type_spec->type);
                    assert(ast_decl->function.body->flags & AST_NODE_FLAG_TYPED);
                    ast_decl->flags |= AST_NODE_FLAG_TYPED;
                }

                break;
            }

            case AST_Declaration_Kind::STRUCTURE: assert(false);
            case AST_Declaration_Kind::POLY_TYPE: assert(false);
        }

        if (result)
        {
            assert(ast_decl->flags & AST_NODE_FLAG_TYPED);
            assert(ast_decl->type);
        }

        return result;
    }

    bool try_resolve_types(Resolver *resolver, AST_Statement* ast_stmt, Scope *scope)
    {
        assert(resolver);
        assert(ast_stmt);

        bool result = false;

        assert(ast_stmt->flags & AST_NODE_FLAG_RESOLVED_ID);
        assert(!(ast_stmt->flags & AST_NODE_FLAG_TYPED));

        switch (ast_stmt->kind)
        {
            case AST_Statement_Kind::INVALID: assert(false);

            case AST_Statement_Kind::BLOCK: 
            {
                // Blocks hold their own scope
                assert(scope == nullptr);
                assert(ast_stmt->block.scope);

                bool block_res = true;

                auto nodes = ast_stmt->block.statements;
                for (int64_t i = 0; i < nodes.count; i++)
                {
                    if (!try_resolve_types(resolver, nodes[i], ast_stmt->block.scope))
                    {
                        block_res = false;
                        break;
                    }
                }

                if (block_res) ast_stmt->flags |= AST_NODE_FLAG_TYPED;

                result = block_res;
                break;
            }

            case AST_Statement_Kind::ASSIGNMENT: assert(false);
            case AST_Statement_Kind::RETURN: assert(false);
            case AST_Statement_Kind::DECLARATION: assert(false);

            case AST_Statement_Kind::EXPRESSION:
            {
                auto expr = ast_stmt->expression;
                result = try_resolve_types(resolver, expr, scope);
                if (result && (expr->flags & AST_NODE_FLAG_TYPED))
                {
                    assert(expr->type);
                    ast_stmt->flags |= AST_NODE_FLAG_TYPED;
                }
                break;
            }

        }

        if (result)
        {
            assert(ast_stmt->flags & AST_NODE_FLAG_TYPED);
        }

        return result;
    }

    bool try_resolve_types(Resolver *resolver, AST_Expression* ast_expr, Scope *scope)
    {
        assert(resolver);
        assert(ast_expr);
        assert(scope);

        assert(ast_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
        assert(!(ast_expr->flags & AST_NODE_FLAG_TYPED));
        assert(ast_expr->type == nullptr);

        bool result = false;

        switch (ast_expr->kind)
        {
            case AST_Expression_Kind::INVALID: assert(false);

            case AST_Expression_Kind::IDENTIFIER:
            {
                auto ident = ast_expr->identifier;
                assert(ident->declaration);
                if (ident->declaration->flags & AST_NODE_FLAG_TYPED)
                {
                    assert(ident->declaration->type);
                    ast_expr->type = ident->declaration->type;
                    result = true;
                }
                else
                {
                    result = false;
                }
                break;
            }

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);
            case AST_Expression_Kind::DOT: assert(false);
            case AST_Expression_Kind::BINARY: assert(false);
            case AST_Expression_Kind::UNARY: assert(false);

            case AST_Expression_Kind::CALL:
            {
                if (ast_expr->call.is_builtin)
                {
                    result = try_resolve_builtin_call_types(resolver, ast_expr, scope);
                }
                else
                {

                    if (!try_resolve_types(resolver, ast_expr->call.ident_expression, scope))
                    {
                        result = false;
                    }
                    else
                    {
                        auto func_type = ast_expr->call.ident_expression->type;
                        assert(func_type->kind == AST_Type_Kind::FUNCTION);

                        for (int64_t i = 0; i < ast_expr->call.arg_expressions.count; i++)
                        {
                            auto arg_expr = ast_expr->call.arg_expressions[i];
                            if (!try_resolve_types(resolver, arg_expr, scope))
                            {
                                assert(false);
                            }
                        }

                        ast_expr->type = func_type->function.return_type;
                        result = true;
                    }
                }

                if (result)
                {
                    assert(ast_expr->type);
                }
                
                break;
            }

            case AST_Expression_Kind::COMPOUND: assert(false);

            case AST_Expression_Kind::NUMBER_LITERAL:
            {
                ast_expr->type = Builtin::type_s64;
                result = true;
                break;
            }

            case AST_Expression_Kind::STRING_LITERAL: assert(false);
        }

        if (result)
        {
            assert(ast_expr->type);
            ast_expr->flags |= AST_NODE_FLAG_TYPED;
        }

        return result;
    }

    bool try_resolve_builtin_call_types(Resolver *resolver, AST_Expression *call_expr, Scope *scope)
    {
        assert(resolver);
        assert(call_expr);
        assert(call_expr->kind == AST_Expression_Kind::CALL);
        assert(scope);

        auto ident_expr = call_expr->call.ident_expression;
        assert(ident_expr->kind == AST_Expression_Kind::IDENTIFIER);

        auto ident_atom = ident_expr->identifier->atom;

        if (ident_atom == Builtin::atom_exit)
        {
            assert(call_expr->call.arg_expressions.count == 1);
            auto arg = call_expr->call.arg_expressions[0];
            
            bool arg_res = try_resolve_types(resolver, arg, scope);
            if (!arg_res) { assert(false); return false; }

            call_expr->type = Builtin::type_void;

            return true;
        }
        else assert(false);

    }

    bool try_resolve_types(Resolver *resolver, AST_Type_Spec* ts, Scope* scope,
                           AST_Type **type_target)
    {
        assert(resolver);
        assert(ts);
        assert(ts->type == nullptr);
        assert(scope);
        assert(type_target);
        assert(*type_target == nullptr);

        assert(ts->flags & AST_NODE_FLAG_RESOLVED_ID);
        assert(!(ts->flags & AST_NODE_FLAG_TYPED));
        assert(ts->type == nullptr);

        bool result = false;

        switch (ts->kind)
        {
            case AST_Type_Spec_Kind::INVALID: assert(false);
            case AST_Type_Spec_Kind::IDENTIFIER: assert(false);
            case AST_Type_Spec_Kind::POINTER: assert(false);
            case AST_Type_Spec_Kind::DOT: assert(false);

            case AST_Type_Spec_Kind::FUNCTION:
            {
                auto param_count = ts->function.parameter_type_specs.count;
                Array<AST_Type*> param_types = {};

                if (param_count)
                {
                    array_init(resolver->allocator, &param_types, param_count);
                }

                for (int64_t i = 0; i < param_count; i++)
                {
                    auto param_ts = ts->function.parameter_type_specs[i];
                    AST_Type *param_type = nullptr;
                    if (!try_resolve_types(resolver, param_ts, scope, &param_type))
                    {
                        assert(false);
                    }
                    else
                    {
                        assert(param_type);
                        array_append(&param_types, param_type);
                    }
                }

                assert(param_types.count == param_count);
                assert(param_types.count <= param_count);

                if (param_types.count < param_count)
                {
                    assert(false);
                    array_free(&param_types);
                }

                AST_Type *return_type = nullptr;
                if (ts->function.return_type_spec)
                {
                    if (!try_resolve_types(resolver, ts->function.return_type_spec, scope,
                                           &return_type))
                    {
                        assert(false);
                    }

                    assert(return_type);
                } 
                else
                {
                    return_type = Builtin::type_void; 
                }

                auto func_type = find_or_create_function_type(resolver, param_types, return_type,
                                                              scope);
                assert(func_type);
                result = true;
                ts->type = func_type;
                *type_target = func_type;
                break;
            }

            case AST_Type_Spec_Kind::ARRAY: assert(false);
            case AST_Type_Spec_Kind::TEMPLATED: assert(false);
            case AST_Type_Spec_Kind::POLY_IDENTIFIER: assert(false);
        }

        if (result)
        {
            assert(ts->type);
            assert(*type_target);
            ts->flags |= AST_NODE_FLAG_TYPED;
        }

        return result;
    }

    bool try_resolve_sizes(Resolver *resolver, AST_Node *ast_node, Scope *scope)
    {
        assert(resolver);
        assert(ast_node);
        assert(scope);

        if (ast_node->flags & AST_NODE_FLAG_SIZED) return  true;

        bool result = true;

        assert(ast_node->kind == AST_Node_Kind::TYPE);

        switch (ast_node->kind)
        {
            case AST_Node_Kind::INVALID: assert(false);
            case AST_Node_Kind::MODULE: assert(false);
            case AST_Node_Kind::IDENTIFIER: assert(false);
            case AST_Node_Kind::DECLARATION: assert(false);
            case AST_Node_Kind::STATEMENT: assert(false);
            case AST_Node_Kind::EXPRESSION: assert(false);
            case AST_Node_Kind::TYPE_SPEC: assert(false);

            case AST_Node_Kind::TYPE:
            {
                result = try_resolve_sizes(resolver, static_cast<AST_Type*>(ast_node), scope);
                break;
            }
        }

        if (result)
        {
            assert(ast_node->flags & AST_NODE_FLAG_SIZED);
        }

        return result;
    }

    bool try_resolve_sizes(Resolver *resolver, AST_Type *ast_type, Scope *scope)
    {
        assert(resolver);
        assert(ast_type);
        assert(scope);

        if (ast_type->flags & AST_NODE_FLAG_SIZED)
        {
            assert(ast_type->flags & AST_NODE_FLAG_SIZED);
            assert(ast_type->bit_size > 0);
            return true;
        }

        bool result = true;

        switch (ast_type->kind)
        {
            case AST_Type_Kind::INVALID: assert(false);
            case AST_Type_Kind::VOID: assert(false);
            case AST_Type_Kind::INTEGER: assert(false);

            case AST_Type_Kind::FUNCTION:
            {
                assert(ast_type->bit_size == Builtin::pointer_size);
                ast_type->flags |= AST_NODE_FLAG_SIZED;
                result = true;
                break;
            }
        }

        if (result)
        {
            assert(ast_type->flags & AST_NODE_FLAG_SIZED);
            assert(ast_type->bit_size > 0);
        }

        return result;
    }

    AST_Type* find_or_create_function_type(Resolver *resolver, Array<AST_Type*> param_types,
                                           AST_Type *return_type, Scope *scope)
    {
        assert(resolver);
        assert(return_type);
        assert(param_types.count >= 0);
        assert(scope);

        AST_Type *func_type = build_data_find_function_type(resolver->build_data, param_types,
                                                            return_type);

        if (!func_type)
        {
            func_type = ast_function_type_new(resolver->allocator, param_types, return_type);
            array_append(&resolver->build_data->type_table, func_type);
            queue_size_job(resolver, func_type, scope);
        }

        assert(func_type);

        return func_type;
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

    void queue_type_job(Resolver *resolver, AST_Node *ast_node, Scope *scope)
    {
        assert(resolver);
        assert(ast_node);
        assert(scope);

        auto allocator = resolver->allocator;
        assert(allocator);

        auto job = resolve_job_type_new(allocator, ast_node, scope);
        queue_enqueue(&resolver->type_job_queue, job);
        assert(job);
    }

    void queue_size_job(Resolver *resolver, AST_Node *ast_node, Scope *scope)
    {
        assert(resolver);
        assert(ast_node);
        assert(scope);

        auto allocator = resolver->allocator;
        assert(allocator);

        auto job = resolve_job_size_new(allocator, ast_node, scope);
        queue_enqueue(&resolver->size_job_queue, job);
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

    Resolve_Job *resolve_job_type_new(Allocator *allocator, AST_Node *ast_node, Scope *scope)
    {
        return resolve_job_new(allocator, Resolve_Job_Kind::TYPE, ast_node, scope);
    }

    Resolve_Job *resolve_job_size_new(Allocator *allocator, AST_Node *ast_node, Scope *scope)
    {
        return resolve_job_new(allocator, Resolve_Job_Kind::SIZE, ast_node, scope);
    }

    void free_job(Resolver *resolver, Resolve_Job *job)
    {
        assert(resolver->allocator);
        free(resolver->allocator, job);  
    }

}
