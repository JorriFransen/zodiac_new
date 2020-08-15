#include "resolver.h"

#include "ast.h"
#include "builtin.h"
#include "scope.h"

#include <cassert>
#include <stdio.h>
#include <stdarg.h>

#include <inttypes.h>

namespace Zodiac
{
    void resolver_init(Allocator *allocator, Allocator *err_allocator, Resolver *resolver,
                       Build_Data *build_data)
    {
        assert(allocator);
        assert(resolver);

        resolver->allocator = allocator;
        resolver->err_allocator = err_allocator;

        resolver->build_data = build_data;
        bytecode_builder_init(allocator, &resolver->bytecode_builder);
        llvm_builder_init(allocator, &resolver->llvm_builder);

        queue_init(allocator, &resolver->ident_job_queue);
        queue_init(allocator, &resolver->type_job_queue);
        queue_init(allocator, &resolver->size_job_queue);
        queue_init(allocator, &resolver->emit_bytecode_job_queue);
        queue_init(allocator, &resolver->emit_llvm_job_queue);

        array_init(err_allocator, &resolver->errors);
    }

    void start_resolving(Resolver *resolver, AST_Node *ast_node, bool blocking)
    {
        assert(resolver);
        assert(ast_node);

        assert(blocking);

        assert(resolver->root_node == nullptr);
        resolver->root_node = ast_node;

        assert(ast_node->kind == AST_Node_Kind::MODULE);

        AST_Declaration *entry_decl = nullptr;

        auto ast_module = static_cast<AST_Module*>(ast_node);
        for (int64_t i = 0; i < ast_module->declarations.count; i++)
        {
            AST_Declaration *decl = ast_module->declarations[i];
            queue_ident_job(resolver, decl, ast_module->module_scope);

            if (decl->kind == AST_Declaration_Kind::FUNCTION &&
                decl->identifier->atom == Builtin::atom__start &&
                (decl->decl_flags & AST_DECL_FLAG_IS_NAKED))
            {
                assert(!entry_decl);
                entry_decl = decl;
                decl->decl_flags |= AST_DECL_FLAG_IS_ENTRY;
            }
        }

        if (blocking)
        {
             start_resolve_pump(resolver, entry_decl);
        }
        else
        {
            assert(false);
        }
    }

    Resolve_Result finish_resolving(Resolver *resolver)
    {
        assert(resolver);

        if (resolver->errors.count == 0)
        {
            assert(queue_count(&resolver->ident_job_queue) == 0);
            assert(queue_count(&resolver->type_job_queue) == 0);
            assert(queue_count(&resolver->size_job_queue) == 0);
            assert(queue_count(&resolver->emit_bytecode_job_queue) == 0);
            assert(queue_count(&resolver->emit_llvm_job_queue) == 0);

        }

        return Resolve_Result {};
    }

    void start_resolve_pump(Resolver *resolver, AST_Declaration *entry_decl)
    {
        assert(entry_decl->kind == AST_Declaration_Kind::FUNCTION);

        bool done = false;

        while (!done)
        {
            auto ident_job_count = queue_count(&resolver->ident_job_queue);
            while (ident_job_count--)
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

            auto type_job_count = queue_count(&resolver->type_job_queue);
            while (type_job_count--)
            {
                auto job = queue_dequeue(&resolver->type_job_queue);
                bool job_done = try_resolve_job(resolver, job);

                if (!job_done)
                {
                    queue_enqueue(&resolver->type_job_queue, job);
                }
                else
                {
                    if (job->declaration == entry_decl)
                    {
                        queue_emit_bytecode_jobs_from_declaration(resolver, entry_decl,
                                                                  job->node_scope);
                    }

                    // Size jobs are queued when types are first created
                    free_job(resolver, job);
                }
            }

            auto size_job_count = queue_count(&resolver->size_job_queue);
            while (size_job_count--)
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

            auto bytecode_job_count = queue_count(&resolver->emit_bytecode_job_queue);
            while (bytecode_job_count--)
            {
                auto job = queue_dequeue(&resolver->emit_bytecode_job_queue);
                bool job_done = try_resolve_job(resolver, job);

                if (!job_done)
                {
                    queue_enqueue(&resolver->emit_bytecode_job_queue, job);
                }
                else
                {
                    assert(job->result);
                    queue_emit_llvm_job(resolver, job->result);
                    free_job(resolver, job);
                }
            }

            if (queue_count(&resolver->ident_job_queue) == 0 &&
                queue_count(&resolver->type_job_queue) == 0 &&
                queue_count(&resolver->size_job_queue) == 0 &&
                queue_count(&resolver->emit_bytecode_job_queue) == 0)

            {
                done = true;
            }

            auto llvm_job_count = queue_count(&resolver->emit_llvm_job_queue);
            while (llvm_job_count--)
            {
                auto job = queue_dequeue(&resolver->emit_llvm_job_queue);
                bool job_done = try_resolve_job(resolver, job);
                assert(job_done);
                free_job(resolver, job);
            }

            if (resolver->errors.count)
            {
                resolver_report_errors(resolver);
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

            case Resolve_Job_Kind::EMIT_BYTECODE:
            {
                auto node = job->ast_node;
                if (node->kind == AST_Node_Kind::TYPE &&
                    !(node->flags & AST_NODE_FLAG_SIZED))
                {
                    result = false;
                    assert(false);
                }
                else if (!(node->flags & AST_NODE_FLAG_TYPED))
                {
                    result = false;
                }
                else
                {
                    assert(job->ast_node->kind == AST_Node_Kind::DECLARATION);
                    auto decl = static_cast<AST_Declaration*>(job->ast_node);
                    assert(decl->kind == AST_Declaration_Kind::FUNCTION);
                    auto bc_func = bytecode_emit_function_declaration(&resolver->bytecode_builder,
                                                                      decl);
                    job->result = bc_func;
                    result = true;
                }
                break;
            }

            case Resolve_Job_Kind::EMIT_LLVM:
            {
                auto bc_func = job->bc_func;
                llvm_emit_function(&resolver->llvm_builder, bc_func);
                result = true;
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

            case AST_Node_Kind::IDENTIFIER:
            {
                auto ident = static_cast<AST_Identifier*>(ast_node);
                result = try_resolve_identifiers(resolver, ident, scope);
                break;
            }

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

        bool result = true;

        switch (ast_decl->kind)
        {
            case AST_Declaration_Kind::INVALID: assert(false);
            case AST_Declaration_Kind::IMPORT: assert(false);

            case AST_Declaration_Kind::VARIABLE:
            {
                if (ast_decl->variable.type_spec)
                {
                    if (!try_resolve_identifiers(resolver, ast_decl->variable.type_spec, scope))
                    {
                        assert(false);
                    }
                }

                if (ast_decl->variable.init_expression)
                {
                    if (!try_resolve_identifiers(resolver, ast_decl->variable.init_expression,
                                                 scope))
                    {
                        assert(false);
                    }
                }

                break;
            }

            case AST_Declaration_Kind::CONSTANT: assert(false);

            case AST_Declaration_Kind::PARAMETER:
            {
                result = try_resolve_identifiers(resolver, ast_decl->parameter.type_spec, scope);
                break;
            };

            case AST_Declaration_Kind::FUNCTION:
            {
                if (!try_resolve_identifiers(resolver, ast_decl->function.type_spec, scope))
                {
                    result = false;
                }

                if (result)
                {
                    for (int64_t i = 0; i < ast_decl->function.parameter_declarations.count; i++)
                    {
                        auto param_decl = ast_decl->function.parameter_declarations[i];
                        if (!try_resolve_identifiers(resolver, param_decl,
                                                     ast_decl->function.parameter_scope))
                        {
                            result = false;
                        }
                    }
                }

                if (result)
                {
                    if (!try_resolve_identifiers(resolver, ast_decl->function.body, scope))
                    {
                        result = false;
                    }
                }

                break;
            }

            case AST_Declaration_Kind::TYPE: assert(false);
            case AST_Declaration_Kind::STRUCTURE: assert(false);
            case AST_Declaration_Kind::POLY_TYPE: assert(false);
        }

        if (result) ast_decl->flags |= AST_NODE_FLAG_RESOLVED_ID;
        return result;
    }

    bool try_resolve_identifiers(Resolver *resolver, AST_Statement *ast_stmt, Scope *scope)
    {
        bool result = false;

        if (ast_stmt->flags & AST_NODE_FLAG_RESOLVED_ID) return true;

        switch (ast_stmt->kind)
        {
            case AST_Statement_Kind::INVALID: assert(false);

            case AST_Statement_Kind::BLOCK:
            {
                auto nodes = ast_stmt->block.statements;

                bool block_res = true;
                for (int64_t i = 0; i < nodes.count; i++)
                {
                    if (!try_resolve_identifiers(resolver, nodes[i], ast_stmt->block.scope))
                    {
                        block_res = false;
                        break;
                    }
                }

                result = block_res;
                break;
            }

            case AST_Statement_Kind::ASSIGNMENT:
            {
                if (!try_resolve_identifiers(resolver,
                                             ast_stmt->assignment.identifier_expression, scope))
                {
                    assert(false);
                }

                if (!try_resolve_identifiers(resolver, ast_stmt->assignment.rhs_expression, scope))
                {
                    assert(false);
                }
                result = true;
                break;
            }

            case AST_Statement_Kind::RETURN:
            {
                result = try_resolve_identifiers(resolver, ast_stmt->expression, scope);
                break;
            }

            case AST_Statement_Kind::DECLARATION:
            {
                result = try_resolve_identifiers(resolver, ast_stmt->declaration, scope);
                break;
            }

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

        if (ast_expr->flags & AST_NODE_FLAG_RESOLVED_ID)
        {
            return true;
        }

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

            case AST_Expression_Kind::BINARY:
            {
                if (!try_resolve_identifiers(resolver, ast_expr->binary.lhs, scope))
                {
                    assert(false);
                }
                if (!try_resolve_identifiers(resolver, ast_expr->binary.rhs, scope))
                {
                    assert(false);
                }

                result = true;
                break;
            }

            case AST_Expression_Kind::UNARY: assert(false);

            case AST_Expression_Kind::CALL:
            {
                if (!ast_expr->call.is_builtin)
                {
                    if (!try_resolve_identifiers(resolver, ast_expr->call.ident_expression, scope))
                    {
                        assert(false);
                    }

                    auto ident_expr = ast_expr->call.ident_expression;
                    assert(ident_expr->kind == AST_Expression_Kind::IDENTIFIER);
                    assert(ident_expr->identifier->declaration);
                    ast_expr->call.callee_declaration = ident_expr->identifier->declaration;
                    
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

    bool try_resolve_identifiers(Resolver *resolver, AST_Type_Spec *ast_ts, Scope *scope)
    {
        bool result = true;
        
        switch (ast_ts->kind)
        {
            case AST_Type_Spec_Kind::INVALID: assert(false);

            case AST_Type_Spec_Kind::IDENTIFIER:
            {
                auto decl = scope_find_declaration(scope, ast_ts->identifier);
                if (!decl)
                {
                    resolver_report_undeclared_identifier(resolver, ast_ts->identifier);
                    result = false;
                    break;
                }

                assert(decl);
                assert(decl->kind == AST_Declaration_Kind::TYPE);

                break;
            }

            case AST_Type_Spec_Kind::POINTER: assert(false);
            case AST_Type_Spec_Kind::DOT: assert(false);

            case AST_Type_Spec_Kind::FUNCTION:
            {
                for (int64_t i = 0; i < ast_ts->function.parameter_type_specs.count; i++)
                {
                    auto param_ts = ast_ts->function.parameter_type_specs[i];
                    if (!try_resolve_identifiers(resolver, param_ts, scope))
                    {
                        result = false;
                    }
                }

                if (ast_ts->function.return_type_spec)
                {
                    if (!try_resolve_identifiers(resolver,
                                                 ast_ts->function.return_type_spec,
                                                 scope))
                    {
                        result = false;
                    }
                }

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

            case AST_Declaration_Kind::VARIABLE:
            {
                AST_Type *ts_type = nullptr;
                if (ast_decl->variable.type_spec)
                {
                    if (!try_resolve_types(resolver, ast_decl->variable.type_spec, scope,
                                           &ts_type))
                    {
                        assert(false);
                    }
                }

                if (ast_decl->variable.init_expression)
                {
                    if (!try_resolve_types(resolver, ast_decl->variable.init_expression, scope))
                    {
                        result = false;
                    }
                }

                if (result)
                {
                    if (ts_type && ast_decl->variable.init_expression)
                    {
                        assert(ts_type == ast_decl->variable.init_expression->type);
                    }

                    AST_Type *var_type = ts_type;
                    if (var_type == nullptr && ast_decl->variable.init_expression)
                    {
                        var_type = ast_decl->variable.init_expression->type;
                    }

                    assert(var_type);

                    ast_decl->type = var_type;
                    ast_decl->flags |= AST_NODE_FLAG_TYPED;
                }
                break;
            }

            case AST_Declaration_Kind::CONSTANT: assert(false);

            case AST_Declaration_Kind::PARAMETER:
            {
                if (ast_decl->parameter.type_spec->type &&
                    !ast_decl->type)
                {
                    // Was already resolved when resolving the function type spec
                    ast_decl->type = ast_decl->parameter.type_spec->type;
                }
                else if (!ast_decl->parameter.type_spec->type)
                {
                    result = try_resolve_types(resolver, ast_decl->parameter.type_spec, scope,
                                               &ast_decl->type);
                    assert(ast_decl->type);
                } else assert(false);

                if (result)
                {
                    assert(ast_decl->type);
                    ast_decl->flags |= AST_NODE_FLAG_TYPED;
                }
                break;
            }


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
                    if (ast_decl->decl_flags & AST_DECL_FLAG_NORETURN)
                    {
                        assert(ast_decl->type->function.return_type == Builtin::type_void);
                    }
                    assert(ast_decl->type);
                    assert(ast_decl->function.type_spec->type);
                    assert(ast_decl->function.body->flags & AST_NODE_FLAG_TYPED);
                    ast_decl->flags |= AST_NODE_FLAG_TYPED;
                }

                break;
            }

            case AST_Declaration_Kind::TYPE: assert(false);
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

        if ((ast_stmt->flags & AST_NODE_FLAG_TYPED))
        {
            return true;
        }

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

            case AST_Statement_Kind::ASSIGNMENT:
            {
                if (!try_resolve_types(resolver,
                                       ast_stmt->assignment.identifier_expression, scope))
                {
                    assert(false);
                }

                if (!try_resolve_types(resolver, ast_stmt->assignment.rhs_expression, scope))
                {
                    assert(false);
                }

                result = true;
                ast_stmt->flags |= AST_NODE_FLAG_TYPED;
                break;
            }

            case AST_Statement_Kind::RETURN:
            {
                result = try_resolve_types(resolver, ast_stmt->expression, scope);
                if (result && (ast_stmt->expression->flags & AST_NODE_FLAG_TYPED))
                {
                    assert(ast_stmt->expression->type);
                    ast_stmt->flags |= AST_NODE_FLAG_TYPED;
                }
                break;
            }

            case AST_Statement_Kind::DECLARATION:
            {
                auto decl = ast_stmt->declaration;
                result = try_resolve_types(resolver, decl, scope);
                if (result && (decl->flags & AST_NODE_FLAG_TYPED))
                {
                    ast_stmt->flags |= AST_NODE_FLAG_TYPED;
                }
                break;
            }

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

            case AST_Expression_Kind::BINARY:
            {
                auto lhs = ast_expr->binary.lhs;
                auto rhs = ast_expr->binary.rhs;

                if (!try_resolve_types(resolver, lhs, scope))
                {
                    assert(false);
                }
                if (!try_resolve_types(resolver, rhs, scope))
                {
                    assert(false);
                }

                result = true;
                assert(lhs->type == rhs->type);
                ast_expr->type = lhs->type;
                break;
            }

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
            
            if (!try_resolve_types(resolver, arg, scope))
            {
                return false;
            }

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

            case AST_Type_Spec_Kind::IDENTIFIER:
            {
                auto decl = ts->identifier->declaration;
                assert(decl);
                assert(decl->kind == AST_Declaration_Kind::TYPE);
                assert(decl->type);
                *type_target = decl->type;
                ts->type = decl->type;
                result = true;
                break;
            }

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

    void queue_emit_bytecode_job(Resolver *resolver, AST_Node *ast_node, Scope *scope)
    {
        assert(resolver->allocator);

        if (ast_node->flags & AST_NODE_FLAG_QUEUED_BYTECODE_EMISSION)
            return;

        auto job = resolve_job_emit_bytecode_new(resolver->allocator, ast_node, scope);
        queue_enqueue(&resolver->emit_bytecode_job_queue, job);
        assert(job);

        ast_node->flags |= AST_NODE_FLAG_QUEUED_BYTECODE_EMISSION;
    }
    
    void queue_emit_llvm_job(Resolver *resolver, Bytecode_Function *bc_func)
    {
        assert(resolver->allocator);

        auto job = resolve_job_emit_llvm_new(resolver->allocator, bc_func);
        queue_enqueue(&resolver->emit_llvm_job_queue, job);
        assert(job);
    }

    void queue_emit_bytecode_jobs_from_declaration(Resolver *resolver, AST_Declaration *entry_decl,
                                                   Scope *scope)
    {
        switch (entry_decl->kind)
        {
            case AST_Declaration_Kind::INVALID: assert(false);
            case AST_Declaration_Kind::IMPORT: assert(false);

            case AST_Declaration_Kind::VARIABLE:
            {
                //queue_emit_bytecode_job(resolver, entry_decl, scope);                
                if (entry_decl->variable.init_expression)
                {
                    queue_emit_bytecode_jobs_from_expression(resolver,
                                                             entry_decl->variable.init_expression,
                                                             scope);
                }
                break;
            }

            case AST_Declaration_Kind::CONSTANT: assert(false);
            case AST_Declaration_Kind::PARAMETER: assert(false);

            case AST_Declaration_Kind::FUNCTION:
            {
                assert(scope->kind == Scope_Kind::MODULE);

                auto body = entry_decl->function.body;
                assert(body);

                for (int64_t i = 0; i < body->block.statements.count; i++)
                {
                    queue_emit_bytecode_jobs_from_statement(resolver, body->block.statements[i],
                                                            body->block.scope);
                }

                queue_emit_bytecode_job(resolver, entry_decl, scope);
                break;
            }

            case AST_Declaration_Kind::TYPE: assert(false);
            case AST_Declaration_Kind::STRUCTURE: assert(false);
            case AST_Declaration_Kind::POLY_TYPE: assert(false);
        }
    }
    
    void queue_emit_bytecode_jobs_from_statement(Resolver *resolver, AST_Statement *stmt,
                                                 Scope *scope)
    {
        switch (stmt->kind)
        {
            case AST_Statement_Kind::INVALID: assert(false);
            case AST_Statement_Kind::BLOCK: assert(false);

            case AST_Statement_Kind::ASSIGNMENT:
            {
                queue_emit_bytecode_jobs_from_expression(resolver,
                                                         stmt->assignment.identifier_expression,
                                                         scope);
                queue_emit_bytecode_jobs_from_expression(resolver,
                                                         stmt->assignment.rhs_expression, scope);
                break;
            }

            case AST_Statement_Kind::RETURN:
            {
                if (stmt->expression)
                {
                    queue_emit_bytecode_jobs_from_expression(resolver, stmt->expression, scope);
                }
                break;
            }

            case AST_Statement_Kind::DECLARATION:
            {
                queue_emit_bytecode_jobs_from_declaration(resolver, stmt->declaration, scope);
                break;
            }

            case AST_Statement_Kind::EXPRESSION:
            {
                queue_emit_bytecode_jobs_from_expression(resolver, stmt->expression, scope);
                break;
            }

        }
    }

    void queue_emit_bytecode_jobs_from_expression(Resolver *resolver, AST_Expression *expr,
                                                  Scope *scope)
    {
        assert(resolver);
        assert(scope);

        switch (expr->kind)
        {
            case AST_Expression_Kind::INVALID: assert(false);

            case AST_Expression_Kind::IDENTIFIER:
            {
                auto ident = expr->identifier;
                assert(ident->declaration);

                if (ident->declaration->kind != AST_Declaration_Kind::VARIABLE &&
                    ident->declaration->kind != AST_Declaration_Kind::PARAMETER)
                {
                    assert(false); 
                }

                break;
            }

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);
            case AST_Expression_Kind::DOT: assert(false);

            case AST_Expression_Kind::BINARY:
            {
                queue_emit_bytecode_jobs_from_expression(resolver, expr->binary.lhs, scope);
                queue_emit_bytecode_jobs_from_expression(resolver, expr->binary.rhs, scope);
                break;
            }

            case AST_Expression_Kind::UNARY: assert(false);

            case AST_Expression_Kind::CALL:
            {
                for (int64_t i = 0; i < expr->call.arg_expressions.count; i++)
                {
                    auto arg_expr = expr->call.arg_expressions[i];
                    queue_emit_bytecode_jobs_from_expression(resolver, arg_expr, scope);
                }

                if (expr->call.is_builtin)
                {
                    //assert(false);
                }
                else
                {
                    auto callee_decl = expr->call.callee_declaration;
                    assert(callee_decl);
                    assert(callee_decl->kind == AST_Declaration_Kind::FUNCTION);
                    if (!(callee_decl->flags & AST_NODE_FLAG_QUEUED_BYTECODE_EMISSION))
                    {
                        auto func_parent_scope = callee_decl->function.parameter_scope->parent;
                        assert(func_parent_scope);
                        queue_emit_bytecode_jobs_from_declaration(resolver, callee_decl,
                                                                  func_parent_scope);
                        queue_emit_bytecode_job(resolver, callee_decl, scope);
                    }
                }
                break;
            }

            case AST_Expression_Kind::COMPOUND: assert(false);
            case AST_Expression_Kind::NUMBER_LITERAL:
            {
                break;
            }
            case AST_Expression_Kind::STRING_LITERAL: assert(false);
        }
    }

    Resolve_Job *resolve_job_new(Allocator *allocator, Resolve_Job_Kind kind)
    {
        auto result = alloc_type<Resolve_Job>(allocator);
        assert(result);
        result->kind = kind;
        result->result = nullptr;
        return result;
    }

    Resolve_Job *resolve_job_new(Allocator *allocator, Resolve_Job_Kind kind, AST_Node *ast_node, 
                                 Scope *scope)
    {
        auto result = resolve_job_new(allocator, kind);
        result->ast_node = ast_node;
        result->node_scope = scope;
        return result;
    }

    Resolve_Job *resolve_job_new(Allocator *allocator, Bytecode_Function *bc_func)
    {
        auto result = resolve_job_new(allocator, Resolve_Job_Kind::EMIT_LLVM);
        result->bc_func = bc_func;
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

    Resolve_Job *resolve_job_emit_bytecode_new(Allocator *allocator, AST_Node *ast_node,
                                               Scope *scope)
    {
        return resolve_job_new(allocator, Resolve_Job_Kind::EMIT_BYTECODE, ast_node, scope);
    }

    Resolve_Job *resolve_job_emit_llvm_new(Allocator *allocator, Bytecode_Function *bc_func)
    {
        return resolve_job_new(allocator, bc_func);
    }

    void free_job(Resolver *resolver, Resolve_Job *job)
    {
        assert(resolver->allocator);
        free(resolver->allocator, job);  
    }

    void resolver_report_undeclared_identifier(Resolver *resolver, AST_Identifier *identifier)
    {
        assert(resolver);
        assert(identifier);

        auto atom = identifier->atom;

        for (int64_t i = 0; i < resolver->errors.count; i++)
        {
            auto &err = resolver->errors[i];
            if (err.kind == Resolve_Error_Kind::UNDECLARED_IDENTIFIER &&
                    err.ast_node == identifier)
            {
                return;
            }
        }

        resolver_report_error(resolver,
                              Resolve_Error_Kind::UNDECLARED_IDENTIFIER,
                              identifier,
                              "Reference to undeclared identifier: '%.*s'",
                              (int)atom.length, atom.data);
    }

    void resolver_report_error(Resolver *resolver, Resolve_Error_Kind kind, AST_Node *ast_node,
                               const char *fmt, ...)
    {
        va_list args;
        va_start(args, fmt);
        resolver_report_error(resolver, kind, ast_node, fmt, args);
        va_end(args);
    }

    void resolver_report_error(Resolver *resolver, Resolve_Error_Kind kind, AST_Node *ast_node,
                               const char *fmt, va_list args)
    {
        auto allocator = resolver->err_allocator;

        va_list args_copy;
        va_copy(args_copy, args);
        auto size = vsnprintf(nullptr, 0, fmt, args_copy);
        va_end(args_copy);

        char *buf = alloc_array<char>(allocator, size + 1);
        assert(buf);

        auto written_size = vsnprintf(buf, size + 1, fmt, args);
        assert(written_size <= size); 

        Resolve_Error err = resolver_make_error(kind, buf, written_size, ast_node);
        array_append(&resolver->errors, err);
    }

    void resolver_report_errors(Resolver *resolver)
    {
        fprintf(stderr, "\n");
        for (int64_t i = 0; i < resolver->errors.count; i++)
        {
            auto &err = resolver->errors[i];
            auto node = err.ast_node;
            auto &bfp = node->begin_file_pos;

            fprintf(stderr, "Error:%.*s:%" PRIu64 ":%" PRIu64 ": %.*s\n",
                    (int)bfp.file_name.length, bfp.file_name.data,
                    bfp.line, bfp.column,
                    (int)err.message_size, err.message);
        }
        fprintf(stderr, "\n");
    }

    Resolve_Error resolver_make_error(Resolve_Error_Kind kind, const char *message,
                                      int64_t message_size, AST_Node *ast_node)
    {
        return { kind, message, message_size, ast_node };
    }
}
