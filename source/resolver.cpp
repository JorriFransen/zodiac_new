#include "resolver.h"

#include "ast.h"
#include "builtin.h"
#include "const_interpreter.h"
#include "scope.h"
#include "os.h"
#include "temp_allocator.h"

#include <stdio.h>
#include <stdarg.h>

#include <inttypes.h>

namespace Zodiac
{
    void resolver_init(Allocator *allocator, Allocator *err_allocator, Resolver *resolver,
                       Build_Data *build_data, String first_file_path)
    {
        assert(allocator);
        assert(resolver);

        resolver->allocator = allocator;
        resolver->err_allocator = err_allocator;

        if (is_relative_path(first_file_path))
        {
            first_file_path = get_absolute_path(allocator, first_file_path);
        }
        resolver->first_file_path = first_file_path;

        auto ta = temp_allocator_get();

        auto file_name = get_file_name(ta, first_file_path);
        assert(string_ends_with(first_file_path, ".zdc"));
        resolver->first_file_name = string_copy(allocator, file_name, file_name.length - 4);

        resolver->first_file_dir = get_file_dir(allocator, first_file_path);

        resolver->build_data = build_data;
        assert(build_data);
        resolver->lexer = lexer_create(allocator, build_data);
        resolver->parser = parser_create(allocator, build_data);

        bytecode_builder_init(allocator, &resolver->bytecode_builder, build_data);
        llvm_builder_init(allocator, &resolver->llvm_builder,
                          build_data, &resolver->bytecode_builder.program);

        queue_init(allocator, &resolver->parse_job_queue);
        queue_init(allocator, &resolver->ident_job_queue);
        queue_init(allocator, &resolver->type_job_queue);
        queue_init(allocator, &resolver->size_job_queue);
        queue_init(allocator, &resolver->emit_bytecode_job_queue);
        queue_init(allocator, &resolver->emit_llvm_func_job_queue);
        queue_init(allocator, &resolver->emit_llvm_binary_job_queue);

        resolver->progression = {};

        array_init(err_allocator, &resolver->parsed_modules);

        resolver->llvm_error = false;
        array_init(err_allocator, &resolver->errors);

        
        //@TODO: initialize with builtin global count
        resolver->global_scope = scope_new(allocator, Scope_Kind::GLOBAL, nullptr);
        builtin_populate_scope(allocator, resolver->global_scope);

        queue_parse_job(resolver, resolver->first_file_name, resolver->first_file_path, nullptr);

    }

    void start_resolving(Resolver *resolver, bool blocking)
    {
        assert(resolver);

        assert(blocking);

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

        if (resolver->errors.count == 0)
        {
            assert(queue_count(&resolver->parse_job_queue) == 0);
            assert(queue_count(&resolver->ident_job_queue) == 0);
            assert(queue_count(&resolver->type_job_queue) == 0);
            assert(queue_count(&resolver->size_job_queue) == 0);
            assert(queue_count(&resolver->emit_bytecode_job_queue) == 0);
            assert(queue_count(&resolver->emit_llvm_func_job_queue) == 0);
            assert(queue_count(&resolver->emit_llvm_binary_job_queue) == 0);

        }

        Resolve_Result result = {};
        result.error_count = resolver->errors.count;
        result.llvm_error = resolver->llvm_error;
        return result;
    }

    void start_resolve_pump(Resolver *resolver)
    {
        bool done = false;

        auto options = resolver->build_data->options;

        while (!done)
        {
            auto parse_job_count = queue_count(&resolver->parse_job_queue);
            while (parse_job_count--)
            {
                auto job = queue_dequeue(&resolver->parse_job_queue);
                bool job_done = try_resolve_job(resolver, job);

                assert(job_done);
                auto ast_module = job->result.ast_module;
                assert(ast_module);
                queue_ident_job(resolver, ast_module, ast_module->module_scope);
            }

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
                    if (job->ast_node->kind != AST_Node_Kind::MODULE)
                    {
                        queue_type_job(resolver, job->declaration, job->node_scope);
                    }
                    free_job(resolver, job);
                }
            }

            auto type_job_count = queue_count(&resolver->type_job_queue);
            while (type_job_count--)
            {
                auto job = queue_dequeue(&resolver->type_job_queue);
                assert(job);
                bool job_done = try_resolve_job(resolver, job);

                if (!job_done)
                {
                    queue_enqueue(&resolver->type_job_queue, job);
                }
                else
                {
                    auto decl = job->declaration;
                    assert(decl);
                    if (decl->kind == AST_Declaration_Kind::FUNCTION)
                    {
                        if (decl == resolver->entry_decl)
                        {
                            queue_emit_bytecode_jobs_from_declaration(resolver, decl,
                                                                      job->node_scope);
                        }
                        else if (decl->decl_flags & AST_DECL_FLAG_FOREIGN)
                        {
                            queue_emit_bytecode_job(resolver, decl, job->node_scope);
                        }
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
                    assert(job->result.bc_func);
                    if (job->ast_node == resolver->entry_decl)
                    {
                        auto exe_file_name = resolver->first_file_name.data;
                        auto options = resolver->build_data->options;
                        if (options->exe_file_name.data)
                        {
                            exe_file_name = options->exe_file_name.data;
                        }

                        if (!options->dont_emit_llvm)
                            queue_emit_llvm_binary_job(resolver, exe_file_name);
                    }

                    if (!options->dont_emit_llvm)
                        queue_emit_llvm_func_job(resolver, job->result.bc_func);

                    free_job(resolver, job);
                }
            }

            auto llvm_job_count = queue_count(&resolver->emit_llvm_func_job_queue);
            while (llvm_job_count--)
            {
                auto job = queue_dequeue(&resolver->emit_llvm_func_job_queue);
                bool job_done = try_resolve_job(resolver, job);
                if (!job_done)
                {
                    resolver->llvm_error = true;
                    done = true;
                }
                free_job(resolver, job);
            }

            auto llvm_binary_job_count = queue_count(&resolver->emit_llvm_binary_job_queue);
            while (llvm_binary_job_count--)
            {
                auto job = queue_dequeue(&resolver->emit_llvm_binary_job_queue);
                bool job_done = try_resolve_job(resolver, job);
                free_job(resolver, job);

                if (!job_done)
                {
                    resolver->llvm_error = true;
                    done = true;
                }
            }

            if (queue_count(&resolver->parse_job_queue) == 0 &&
                queue_count(&resolver->ident_job_queue) == 0 &&
                queue_count(&resolver->type_job_queue) == 0 &&
                queue_count(&resolver->size_job_queue) == 0 &&
                queue_count(&resolver->emit_bytecode_job_queue) == 0 &&
                queue_count(&resolver->emit_llvm_func_job_queue) == 0 &&
                queue_count(&resolver->emit_llvm_binary_job_queue) == 0)

            {
                done = true;
            }


            if (resolver_has_progressed(resolver))
            {
                resolver_clear_errors(resolver);
            } 
            else if (resolver->errors.count)
            {
                resolver_report_errors(resolver);
                done = true;
            }

            resolver_save_progression(resolver);
        }
    }

    bool resolver_has_progressed(Resolver *resolver)
    {
        auto p = &resolver->progression;

        return (queue_count(&resolver->parse_job_queue) != p->parse_job_count) ||
               (queue_count(&resolver->ident_job_queue) != p->ident_job_count) ||
               (queue_count(&resolver->type_job_queue) != p->type_job_count) ||
               (queue_count(&resolver->size_job_queue) != p->size_job_count) ||
               (queue_count(&resolver->emit_bytecode_job_queue) != p->bytecode_job_count) ||
               (queue_count(&resolver->emit_llvm_func_job_queue) != p->llvm_job_count) ||
               (queue_count(&resolver->emit_llvm_binary_job_queue) != p->binary_job_count);
    }

    void resolver_save_progression(Resolver *resolver)
    {
        auto p = &resolver->progression;

        p->parse_job_count = queue_count(&resolver->parse_job_queue);
        p->ident_job_count = queue_count(&resolver->ident_job_queue);
        p->type_job_count = queue_count(&resolver->type_job_queue);
        p->size_job_count = queue_count(&resolver->size_job_queue);
        p->bytecode_job_count = queue_count(&resolver->emit_bytecode_job_queue);
        p->llvm_job_count = queue_count(&resolver->emit_llvm_func_job_queue);
        p->binary_job_count = queue_count(&resolver->emit_llvm_binary_job_queue);
    }

    bool try_resolve_job(Resolver *resolver, Resolve_Job *job)
    {
        assert(resolver);
        assert(job);

        auto options = resolver->build_data->options;

        bool result = false;

        switch (job->kind)
        {
            case Resolve_Job_Kind::INVALID: assert(false);

            case Resolve_Job_Kind::PARSE:
            {
                Lexed_File lexed_file = lexer_lex_file(&resolver->lexer, job->parse.module_path);
                assert(lexed_file.valid);

                Token_Stream *token_stream = lexer_new_token_stream(resolver->allocator,
                                                                    &lexed_file);
                Parsed_File parsed_file = parser_parse_file(&resolver->parser, token_stream);

                if (options->print_parse_tree) parsed_file_print(&parsed_file);

                auto module_ast = ast_create_from_parsed_file(resolver->allocator, &parsed_file);
                assert(module_ast);

                if (options->print_ast) ast_print(module_ast);


                scope_populate_ast(resolver->allocator, module_ast, resolver->global_scope);

                result = true;

                job->result.ast_module = module_ast;

                for (int64_t i = 0; i < module_ast->declarations.count; i++)
                {
                    auto decl = module_ast->declarations[i];
                    if (decl->kind == AST_Declaration_Kind::FUNCTION)
                    {
                        if (is_entry_decl(resolver, decl))
                        {
                            assert(!resolver->entry_decl);
                            decl->decl_flags |= AST_DECL_FLAG_IS_ENTRY;
                            resolver->entry_decl = decl;
                        }
                        else if (is_bc_entry_decl(resolver, decl))
                        {
                            assert(!resolver->bc_entry_decl);
                            decl->decl_flags |= AST_DECL_FLAG_IS_BYTECODE_ENTRY;
                            resolver->bc_entry_decl = decl;
                        }
                    }
                }

                lexer_free_lexed_file(&resolver->lexer, &lexed_file);
                token_stream->free();
                parser_free_parsed_file(&resolver->parser, &parsed_file);

                array_append(&resolver->parsed_modules, { job->parse.module_path, module_ast });

                break;
            }

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
                result = try_resolve_types(resolver, job->ast_node, job->node_scope, nullptr);
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
                    auto bc_func =
                        bytecode_emit_function_declaration(&resolver->bytecode_builder, decl);
                    job->result.bc_func = bc_func;
                    result = true;
                }
                break;
            }

            case Resolve_Job_Kind::EMIT_LLVM_FUNC:
            {
                auto bc_func = job->bc_func;
                llvm_emit_function(&resolver->llvm_builder, bc_func);
                result = true;
                break;
            }

            case Resolve_Job_Kind::EMIT_LLVM_BINARY:
            {
                if (llvm_emit_binary(&resolver->llvm_builder, job->llvm_bin.output_file_name))
                {
                    result = true;
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

            case AST_Node_Kind::MODULE:
            {
                auto module = static_cast<AST_Module*>(ast_node);
                for (int64_t i = 0; i < module->declarations.count; i++)
                {
                    auto decl = module->declarations[i]; 
                    if (!try_resolve_identifiers(resolver, decl, module->module_scope))
                    {
                        queue_ident_job(resolver, decl, module->module_scope);
                    }
                    else if (decl->flags & AST_NODE_FLAG_RESOLVED_ID)
                    {
                        queue_type_job(resolver, decl, module->module_scope); 
                    }
                }

                // This never fails, if we can't resolve a declaration it is queued seperately.
                result = true;
                ast_node->flags |= AST_NODE_FLAG_RESOLVED_ID;
                break;
            }

            case AST_Node_Kind::IDENTIFIER:
            {
                assert(false);
                // Should be handled in place when encountered
                
                //auto ident = static_cast<AST_Identifier*>(ast_node);
                //result = try_resolve_identifiers(resolver, ident, scope);
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

        if (ast_decl->flags & AST_NODE_FLAG_RESOLVED_ID) return true;

        bool result = true;
        bool apply_flag = true;

        switch (ast_decl->kind)
        {
            case AST_Declaration_Kind::INVALID: assert(false);

            case AST_Declaration_Kind::IMPORT:
            {
                auto ident_expr = ast_decl->import.ident_expression;
                assert(ident_expr);
                assert(ident_expr->kind == AST_Expression_Kind::IDENTIFIER);

                auto module_name = string_ref(ident_expr->identifier->atom);
                assert(module_name.data);

                auto ta = temp_allocator_get();
                auto file_name = string_append(ta, module_name, ".zdc");
                String file_path = string_append(ta, resolver->first_file_dir, file_name);

                if (!ast_decl->import.parse_queued)
                {

                    //@TODO: Report error
                    assert(is_regular_file(file_path));

                    //@TODO: Do we still need to pass ast_decl?
                    queue_parse_job(resolver, module_name,
                                    string_copy(resolver->allocator, file_path),
                                    ast_decl);
                    ast_decl->import.parse_queued = true;
                    result = true;
                    apply_flag = false;
                    queue_ident_job(resolver, ast_decl, scope);
                }
                else
                {
                    AST_Module *ast_module = nullptr;
                    for (int64_t i = 0; i < resolver->parsed_modules.count; i++)
                    {
                        auto &pm = resolver->parsed_modules[i];
                        if (string_equal(pm.full_path, file_path))
                        {
                            ast_module = pm.ast;
                        }
                    }

                    if (ast_module)
                    {
                        ast_decl->import.ast_module = ast_module;
                        result = true;
                    }
                }
                break;
            }

            case AST_Declaration_Kind::USING:
            {
                auto ident_expr = ast_decl->using_decl.ident_expr;

                result = try_resolve_identifiers(resolver, ident_expr, scope);

                if (result)
                {
                    AST_Declaration *import_decl = nullptr;

                    if (ident_expr->kind == AST_Expression_Kind::IDENTIFIER)
                    {
                        import_decl = ident_expr->identifier->declaration;
                    } 
                    else if (ident_expr->kind == AST_Expression_Kind::DOT)
                    {
                        assert(false); 
                    }
                    else assert(false);

                    assert(import_decl);
                    assert(import_decl->kind == AST_Declaration_Kind::IMPORT);

                    auto module = import_decl->import.ast_module;
                    if (!module)
                    {
                        result = false;
                    }
                    else
                    {
                        assert(module->module_scope);
                        ast_decl->using_decl.import_scope = module->module_scope;
                    }
                }

                break;
            }

            case AST_Declaration_Kind::VARIABLE:
            {
                if (ast_decl->variable.type_spec)
                {
                    if (!try_resolve_identifiers(resolver, ast_decl->variable.type_spec, scope))
                    {
                        result = false;
                    }
                }

                if (ast_decl->variable.init_expression)
                {
                    if (!try_resolve_identifiers(resolver, ast_decl->variable.init_expression,
                                                 scope))
                    {
                        result = false;
                    }
                }

                if (result) queue_type_job(resolver, ast_decl, scope);   

                break;
            }

            case AST_Declaration_Kind::CONSTANT: assert(false);

            case AST_Declaration_Kind::PARAMETER:
            {
                result = try_resolve_identifiers(resolver, ast_decl->parameter.type_spec, scope);

                if (result) queue_type_job(resolver, ast_decl, scope);
                break;
            }

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
                    auto body = ast_decl->function.body;
                    if (body && !try_resolve_identifiers(resolver, body, scope))
                    {
                        result = false;
                    }
                }

                break;
            }

            case AST_Declaration_Kind::TYPE: assert(false);

            case AST_Declaration_Kind::STRUCTURE:
            {
                assert(ast_decl->structure.parameters.count == 0);

                bool mem_res = true;

                for (int64_t i = 0; i < ast_decl->structure.member_declarations.count; i++)
                {
                    if (!try_resolve_identifiers(resolver,
                                                 ast_decl->structure.member_declarations[i],
                                                 ast_decl->structure.member_scope))
                    {
                        mem_res = false;
                    }
                } 

                result = mem_res;
                break;
            }

            case AST_Declaration_Kind::POLY_TYPE: assert(false);
        }

        if (result && apply_flag) ast_decl->flags |= AST_NODE_FLAG_RESOLVED_ID;
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
                result = true;

                auto lhs = ast_stmt->assignment.identifier_expression;
                auto rhs = ast_stmt->assignment.rhs_expression;

                if (!try_resolve_identifiers(resolver, lhs, scope))
                {
                    result = false;
                }

                if (!try_resolve_identifiers(resolver, rhs, scope))
                {
                    result = false;
                }

                break;
            }

            case AST_Statement_Kind::RETURN:
            {
                if (ast_stmt->expression)
                {
                    result = try_resolve_identifiers(resolver, ast_stmt->expression, scope);
                }
                else
                {
                    result = true;
                }
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

            case AST_Statement_Kind::WHILE:
            {
                result = true;

                if (!try_resolve_identifiers(resolver, ast_stmt->while_stmt.cond_expr, scope))
                {
                    assert(false);
                }

                if (!try_resolve_identifiers(resolver, ast_stmt->while_stmt.body,
                                             ast_stmt->while_stmt.body_scope))
                {
                    result = false;
                }

                break;
            }
            
            case AST_Statement_Kind::IF:
            {
                result = true;
                if (!try_resolve_identifiers(resolver, ast_stmt->if_stmt.cond_expr, scope))
                {
                    assert(false); 
                }

                if (!try_resolve_identifiers(resolver, ast_stmt->if_stmt.then_stmt,
                                             ast_stmt->if_stmt.then_scope))
                {
                    result = false;
                }

                if (ast_stmt->if_stmt.else_stmt)
                {
                    if (!try_resolve_identifiers(resolver, ast_stmt->if_stmt.else_stmt,
                                                 ast_stmt->if_stmt.else_scope))
                    {
                        result = false;
                    }
                }
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

            case AST_Expression_Kind::DOT:
            {
                result = try_resolve_identifiers_dot_expr(resolver, ast_expr, scope);
                break;
            }

            case AST_Expression_Kind::IDENTIFIER:
            {
                auto decl = scope_find_declaration(scope, ast_expr->identifier);
                if (!decl)
                {
                    resolver_report_undeclared_identifier(resolver, ast_expr->identifier);
                    result = false; 
                }
                else
                {
                    assert(ast_expr->identifier->declaration);
                    result = true;
                }
                break;
            }

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);

            case AST_Expression_Kind::BINARY:
            {

                result = true;
                if (!try_resolve_identifiers(resolver, ast_expr->binary.lhs, scope))
                {
                    result = false;
                }
                if (!try_resolve_identifiers(resolver, ast_expr->binary.rhs, scope))
                {
                    result = false;
                }
                break;
            }

            case AST_Expression_Kind::UNARY: assert(false);

            case AST_Expression_Kind::CALL:
            {
                result = true;
                if (!ast_expr->call.is_builtin)
                {
                    if (try_resolve_identifiers(resolver, ast_expr->call.ident_expression,
                                                scope))
                    {
                        auto ident_expr = ast_expr->call.ident_expression;
                        AST_Declaration *callee_decl = nullptr;
                        if (ident_expr->kind == AST_Expression_Kind::IDENTIFIER)
                        {
                            callee_decl = ident_expr->identifier->declaration;
                        }
                        else if (ident_expr->kind == AST_Expression_Kind::DOT)
                        {
                            callee_decl = ident_expr->dot.child_decl; 
                        }
                        else assert(false);
                        assert(callee_decl);
                        assert(callee_decl->kind == AST_Declaration_Kind::FUNCTION);
                        ast_expr->call.callee_declaration = callee_decl;
                    } 
                    else
                    {
                        result = false;
                    }
                }
                else
                {
                    auto atom = ast_expr->call.ident_expression->identifier->atom;
                    if (atom == Builtin::atom_exit) { }
                    else if (atom == Builtin::atom_syscall) { }
                    else if (atom == Builtin::atom_cast) {} 
                    else assert(false);
                }

                bool arg_res = true;
                for (int64_t i = 0; i < ast_expr->call.arg_expressions.count; i++)
                {
                    auto arg_expr = ast_expr->call.arg_expressions[i];
                    if (!try_resolve_identifiers(resolver, arg_expr, scope))
                    {
                        arg_res = false;
                    }
                }

                if (result)
                {
                    if (!arg_res) result = false;
                }

                break;
            }

            case AST_Expression_Kind::ADDROF:
            {
                result = try_resolve_identifiers(resolver, ast_expr->addrof.operand_expr, scope);
                break;
            }

            case AST_Expression_Kind::COMPOUND: assert(false);
                                                
            case AST_Expression_Kind::SUBSCRIPT:
            {
                if (!try_resolve_identifiers(resolver, ast_expr->subscript.pointer_expression,
                                             scope))
                {
                    assert(false);
                }
                if (!try_resolve_identifiers(resolver, ast_expr->subscript.index_expression,
                                             scope))
                {
                    assert(false);
                }
                result = true;
                break;
            }

            case AST_Expression_Kind::CAST:  assert(false);


            case AST_Expression_Kind::INTEGER_LITERAL: 
            case AST_Expression_Kind::FLOAT_LITERAL: 
            case AST_Expression_Kind::STRING_LITERAL:
            case AST_Expression_Kind::CHAR_LITERAL:
            case AST_Expression_Kind::BOOL_LITERAL:
            {
                result = true;
                break;
            }
        }

        if (result)
        {
            ast_expr->flags |= AST_NODE_FLAG_RESOLVED_ID;
        }

        return result;
    }

    bool try_resolve_identifiers_dot_expr(Resolver *resolver, AST_Expression *ast_expr,
                                          Scope *scope)
    {
        assert(resolver);
        assert(ast_expr->kind == AST_Expression_Kind::DOT);

        bool result = false;

        auto parent_expr = ast_expr->dot.parent_expression;
        if (try_resolve_identifiers(resolver, parent_expr, scope))
        {
            AST_Declaration *parent_decl = nullptr;

            if (parent_expr->kind == AST_Expression_Kind::IDENTIFIER)
            {
                parent_decl = parent_expr->identifier->declaration;
            }
            else assert(false);

            assert(parent_decl);
            assert(parent_decl->kind == AST_Declaration_Kind::VARIABLE ||
                   parent_decl->kind == AST_Declaration_Kind::PARAMETER ||
                   parent_decl->kind == AST_Declaration_Kind::IMPORT);

            if (parent_decl->kind == AST_Declaration_Kind::IMPORT)
            {
                if (!parent_decl->import.ast_module)
                {
                    result = false;
                }
                else
                {
                    auto module = parent_decl->import.ast_module;
                    auto decl = scope_find_declaration(module->module_scope,
                                                       ast_expr->dot.child_identifier);
                    if (decl)
                    {
                        assert(decl->kind == AST_Declaration_Kind::FUNCTION ||
                               decl->kind == AST_Declaration_Kind::STRUCTURE);
                        ast_expr->dot.child_decl = decl;
                        result = true;
                    }
                    else
                    {
                        resolver_report_undeclared_identifier(resolver,
                                                              ast_expr->dot.child_identifier);
                        result = false;
                    }
                }
            }
            else if (parent_decl->type)
            {
                AST_Type *var_type = parent_decl->type;
                assert(var_type);

                AST_Type *struct_type = var_type;

                if (var_type->kind != AST_Type_Kind::STRUCTURE)
                {
                    assert(var_type->kind == AST_Type_Kind::POINTER);
                    assert(var_type->pointer.base->kind == AST_Type_Kind::STRUCTURE);
                    struct_type = var_type->pointer.base;
                }

                assert(struct_type);
                assert(struct_type->kind == AST_Type_Kind::STRUCTURE);

                assert(struct_type->structure.member_scope);
                auto mem_scope = struct_type->structure.member_scope;
                assert(mem_scope->kind == Scope_Kind::AGGREGATE);

                auto child_ident = ast_expr->dot.child_identifier;
                if (!child_ident->declaration)
                {
                    auto child_decl = scope_find_declaration(mem_scope, child_ident);
                    assert(child_decl);
                    assert(child_ident->declaration);

                    auto struct_decl = struct_type->structure.declaration;
                    assert(struct_decl->kind == AST_Declaration_Kind::STRUCTURE);

                    bool index_found = false;
                    int64_t index = -1;
                    for (int64_t i = 0;
                         i < struct_decl->structure.member_declarations.count;
                         i++)
                    {
                        if (child_decl == struct_decl->structure.member_declarations[i]) 
                        {
                            assert(!index_found);
                            index_found = true;
                            index = i;
                            break;
                        }
                    }

                    assert(index_found);
                    assert(index >= 0);
                    
                    ast_expr->dot.child_index = index;
                }

                result = true;
            }
            else result = false;
        }
        else
        {
            result = false;
        }

        if (result) ast_expr->flags |= AST_NODE_FLAG_RESOLVED_ID;
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
                assert(decl->kind == AST_Declaration_Kind::STRUCTURE ||
                       decl->kind == AST_Declaration_Kind::TYPE);

                break;
            }

            case AST_Type_Spec_Kind::POINTER:
            {
                result = try_resolve_identifiers(resolver, ast_ts->base_type_spec, scope);
                break;
            }

            case AST_Type_Spec_Kind::DOT:
            {
                result = true;
                if (!try_resolve_identifiers_dot_expr(resolver, ast_ts->dot_expression, scope))
                {
                    result = false;
                }

                break;
            }

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

            case AST_Type_Spec_Kind::ARRAY:
            {
                if (!try_resolve_identifiers(resolver, ast_ts->array.length_expression, scope))
                {
                    assert(false);
                }

                if (!try_resolve_identifiers(resolver, ast_ts->array.element_type_spec, scope))
                {
                    assert(false);
                }
                break;
            }

            case AST_Type_Spec_Kind::TEMPLATED: assert(false);
            case AST_Type_Spec_Kind::POLY_IDENTIFIER: assert(false);
        }

        if (result)
        {
            ast_ts->flags |= AST_NODE_FLAG_RESOLVED_ID;
        }
        return result;
    }

    bool try_resolve_types(Resolver *resolver, AST_Node *ast_node, Scope *scope, 
                           AST_Type **inferred_return_type)
    {
        assert(resolver);
        assert(ast_node);

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
                assert(scope);
                auto decl = static_cast<AST_Declaration*>(ast_node);
                //assert(decl->type == nullptr);
                if (!(decl->flags & AST_NODE_FLAG_TYPED))
                {
                    result = try_resolve_types(resolver, decl, scope);
                    if (result)
                        assert(decl->type ||
                               decl->kind == AST_Declaration_Kind::IMPORT ||
                               decl->kind == AST_Declaration_Kind::USING);
                }
                break;
            }

            case AST_Node_Kind::EXPRESSION: assert(false);

            case AST_Node_Kind::STATEMENT:
            {
                auto stmt = static_cast<AST_Statement*>(ast_node);
                result = try_resolve_types(resolver, stmt, scope, nullptr);
                break;
            }

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

        if (ast_decl->flags & AST_NODE_FLAG_TYPED)
        {
            assert(ast_decl->type != nullptr);
            return true;
        }

        bool result = true;

        switch (ast_decl->kind)
        {
            case AST_Declaration_Kind::INVALID: assert(false);

            case AST_Declaration_Kind::IMPORT:
            {
                ast_decl->flags |= AST_NODE_FLAG_TYPED;
                break;
            }

            case AST_Declaration_Kind::USING:
            {
                auto ident_expr = ast_decl->using_decl.ident_expr;
                AST_Declaration *import_decl = nullptr;

                if (ident_expr->kind == AST_Expression_Kind::IDENTIFIER)
                {
                    import_decl = ident_expr->identifier->declaration;
                }
                else if (ident_expr->kind == AST_Expression_Kind::DOT)
                {
                    assert(false);
                }
                else assert(false);

                assert(import_decl->kind == AST_Declaration_Kind::IMPORT);

                if (import_decl->flags & AST_NODE_FLAG_TYPED)
                {
                    result = true;
                    ast_decl->flags |= AST_NODE_FLAG_TYPED;
                }
                else
                {
                    result = false;
                }
                break;
            }

            case AST_Declaration_Kind::VARIABLE:
            {
                AST_Type *ts_type = nullptr;

                auto init_expr = ast_decl->variable.init_expression;

                if (ast_decl->variable.type_spec)
                {
                    if (!try_resolve_types(resolver, ast_decl->variable.type_spec, scope,
                                           &ts_type, nullptr))
                    {
                        result = false;
                    }
                }

                if (ast_decl->variable.init_expression)
                {
                    if (!try_resolve_types(resolver, ast_decl->variable.init_expression, ts_type,
                                           scope))
                    {
                        result = false;
                    }
                }

                if (result)
                {
                    if (ts_type && ast_decl->variable.init_expression)
                    {
                        if (ts_type != ast_decl->variable.init_expression->type)
                        {
                            resolver_report_mismatching_types(resolver, init_expr,
                                                              ts_type, init_expr->type);
                            result = false;
                        }
                    }

                    if (result)
                    {
                        AST_Type *var_type = ts_type;
                        if (var_type == nullptr && ast_decl->variable.init_expression)
                        {
                            var_type = ast_decl->variable.init_expression->type;
                        }

                        assert(var_type);

                        ast_decl->type = var_type;
                        ast_decl->flags |= AST_NODE_FLAG_TYPED;
                    }
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
                                               &ast_decl->type, nullptr);
                    if (result) assert(ast_decl->type);
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
                //if (!ast_decl->function.type_spec->type &&
                    //!try_resolve_types(resolver, ast_decl->function.type_spec, scope,
                                       //&ast_decl->type))
                //{
                    //result = false;
                    //break;
                //}
                //else
                //{
                    //assert(ast_decl->type);
                //}

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
                auto body = ast_decl->function.body;
                AST_Type *inferred_return_type = nullptr;
                if (body && !try_resolve_types(resolver, body, nullptr, &inferred_return_type))
                {
                    result = false;
                    break;
                }

                //if (body) assert(inferred_return_type);
                
                if (!ast_decl->function.type_spec->type &&
                    !try_resolve_types(resolver, ast_decl->function.type_spec, scope,
                                       &ast_decl->type, inferred_return_type))
                {
                    result = false;
                    break;
                }
                else
                {
                    assert(ast_decl->type);

                    auto ret_type = ast_decl->type->function.return_type;

                    if (body && !(ast_decl->decl_flags & AST_DECL_FLAG_NORETURN))
                    {
                        if (!(ret_type == inferred_return_type ||
                               (ret_type == Builtin::type_void &&
                                inferred_return_type == nullptr)))
                        {
                            AST_Type *ac_type = inferred_return_type;
                            if (!ac_type) ac_type = Builtin::type_void;
                            resolver_report_mismatching_types(resolver, ast_decl, ret_type,
                                                              ac_type);
                            result = false;
                        }
                    }
                }

                if (result)
                {
                    if (ast_decl->decl_flags & AST_DECL_FLAG_NORETURN)
                    {
                        assert(ast_decl->type->function.return_type == Builtin::type_void);
                    }
                    assert(ast_decl->type);
                    assert(ast_decl->function.type_spec->type);
                    if (body) assert(body->flags & AST_NODE_FLAG_TYPED);
                    ast_decl->flags |= AST_NODE_FLAG_TYPED;
                }

                break;
            }

            case AST_Declaration_Kind::TYPE: assert(false);

            case AST_Declaration_Kind::STRUCTURE:
            {
                assert(ast_decl->structure.parameters.count == 0);
                
                auto member_count = ast_decl->structure.member_declarations.count;
                Array<AST_Type*> member_types = {};
                array_init(resolver->allocator, &member_types, member_count);
                for (int64_t i = 0; i < member_count; i++)
                {

                    auto mem_decl = ast_decl->structure.member_declarations[i];
                    bool mem_res = try_resolve_types(resolver,
                                                     mem_decl, ast_decl->structure.member_scope);
                    if (!mem_res)
                    {
                        result = false;
                    } 
                    else if (result)
                    {
                        auto mem_type = mem_decl->type;
                        assert(mem_type);
                        array_append(&member_types, mem_type);
                    }
                }
                
                if (result)
                {
                    auto ident = ast_decl->identifier;
                    assert(ident);
                    ast_decl->type = create_structure_type(resolver, ast_decl, member_types, 
                                                           ast_decl->structure.member_scope,
                                                           scope);
                    ast_decl->flags |= AST_NODE_FLAG_TYPED;
                }
                else
                {
                    array_free(&member_types);
                }
                break;
            }

            case AST_Declaration_Kind::POLY_TYPE: assert(false);
        }

        if (result)
        {
            assert(ast_decl->flags & AST_NODE_FLAG_TYPED);
            assert(ast_decl->type ||
                   ast_decl->kind == AST_Declaration_Kind::IMPORT ||
                   ast_decl->kind == AST_Declaration_Kind::USING);
        }

        return result;
    }

    bool try_resolve_types(Resolver *resolver, AST_Statement* ast_stmt, Scope *scope,
                           AST_Type **inferred_return_type)
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
                if (scope)
                {
                    assert(scope == ast_stmt->block.scope->parent);
                }
                assert(ast_stmt->block.scope);

                bool block_res = true;

                auto nodes = ast_stmt->block.statements;
                for (int64_t i = 0; i < nodes.count; i++)
                {
                    if (!try_resolve_types(resolver, nodes[i], ast_stmt->block.scope,
                                           inferred_return_type))
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
                result = true;

                auto lhs = ast_stmt->assignment.identifier_expression;
                auto rhs = ast_stmt->assignment.rhs_expression;

                if (!try_resolve_types(resolver, lhs, scope))
                {
                    result = false;
                }

                if (!try_resolve_types(resolver, rhs, lhs->type, scope))
                {
                    result = false;
                }

                if (result)
                {
                    assert(lhs->type);
                    assert(rhs->type);
                    if (lhs->type != rhs->type)
                    {
                        resolver_report_mismatching_types(resolver, rhs, lhs->type, rhs->type);
                        result = false;
                    }
                    else
                    {
                        ast_stmt->flags |= AST_NODE_FLAG_TYPED;
                    }
                }
                break;
            }

            case AST_Statement_Kind::RETURN:
            {
                if (ast_stmt->expression)
                {
                    assert(inferred_return_type);
                    result = try_resolve_types(resolver, ast_stmt->expression, scope);
                    if (result && (ast_stmt->expression->flags & AST_NODE_FLAG_TYPED))
                    {
                        assert(ast_stmt->expression->type);
                        ast_stmt->flags |= AST_NODE_FLAG_TYPED;

                        if (*inferred_return_type)
                        {
                            assert(*inferred_return_type == ast_stmt->expression->type);
                        }
                        else
                        {
                            *inferred_return_type = ast_stmt->expression->type;
                        }
                    }
                } 
                else
                {
                    result = true;
                }

                if (result)
                {
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

            case AST_Statement_Kind::WHILE:
            {
                result = true;
                if (!try_resolve_types(resolver, ast_stmt->while_stmt.cond_expr, scope))
                {
                    result = false;
                }

                if (!try_resolve_types(resolver, ast_stmt->while_stmt.body,
                                       ast_stmt->while_stmt.body_scope, inferred_return_type))
                {
                    result = false;
                }

                if (result) ast_stmt->flags |= AST_NODE_FLAG_TYPED; 
                break;
            }

            
            case AST_Statement_Kind::IF:
            {
                result = true;
                if (!try_resolve_types(resolver, ast_stmt->if_stmt.cond_expr, scope))
                {
                    result = false;
                }

                if (!try_resolve_types(resolver, ast_stmt->if_stmt.then_stmt, 
                                       ast_stmt->if_stmt.then_scope, inferred_return_type))
                {
                    result = false;
                }

                if (ast_stmt->if_stmt.else_stmt)
                {
                    if (!try_resolve_types(resolver, ast_stmt->if_stmt.else_stmt, 
                                           ast_stmt->if_stmt.else_scope, inferred_return_type))
                    {
                        result = false;
                    }
                }

                if (result) ast_stmt->flags |= AST_NODE_FLAG_TYPED;
                break;
            }
        }

        if (result)
        {
            assert(ast_stmt->flags & AST_NODE_FLAG_TYPED);
        }

        return result;
    }

    bool try_resolve_types(Resolver *resolver, AST_Expression *ast_expr, Scope *scope)
    {
        return try_resolve_types(resolver, ast_expr, nullptr, scope);
    }

    bool try_resolve_types(Resolver *resolver, AST_Expression *ast_expr,
                           AST_Type* suggested_type, Scope *scope)
    {
        assert(resolver);
        assert(ast_expr);
        assert(scope);

        assert(ast_expr->flags & AST_NODE_FLAG_RESOLVED_ID);

        if (ast_expr->flags & AST_NODE_FLAG_TYPED) return true;
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
                    if (ident->declaration->type)
                    {
                        ast_expr->type = ident->declaration->type;
                        result = true;
                    }
                    else
                    {
                        assert(ident->declaration->kind == AST_Declaration_Kind::IMPORT);
                        result = true;
                    }
                }
                else
                {
                    result = false;
                }
                break;
            }

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);

            case AST_Expression_Kind::DOT:
            {
                auto parent_expr = ast_expr->dot.parent_expression;
                bool parent_res = try_resolve_types(resolver, parent_expr, scope);
                assert(parent_res);

                if (parent_expr->kind == AST_Expression_Kind::IDENTIFIER &&
                    parent_expr->identifier->declaration->kind == AST_Declaration_Kind::IMPORT)
                {
                    auto child_decl = ast_expr->dot.child_decl;
                    assert(child_decl);
                    if (child_decl->kind == AST_Declaration_Kind::FUNCTION)
                    {
                        auto func_type = child_decl->type;
                        if (func_type)
                        {
                            assert(func_type->kind == AST_Type_Kind::FUNCTION);

                            ast_expr->type = func_type;
                            result = true;
                        }
                    }
                    else if (child_decl->kind == AST_Declaration_Kind::STRUCTURE)
                    {
                        auto struct_type = child_decl->type;
                        if (struct_type)
                        {
                            assert(struct_type->kind == AST_Type_Kind::STRUCTURE);
                            
                            ast_expr->type = struct_type;
                            result = true;
                        }
                    }
                }
                else
                {
                    auto parent_type = parent_expr->type;
                    assert(parent_type);
                    assert(parent_type->kind == AST_Type_Kind::STRUCTURE ||
                           (parent_type->kind == AST_Type_Kind::POINTER && 
                            parent_type->pointer.base->kind == AST_Type_Kind::STRUCTURE));

                    auto member_ident = ast_expr->dot.child_identifier;
                    assert(member_ident);
                    assert(member_ident->declaration);
                    auto mem_decl = member_ident->declaration;

                    assert(mem_decl->kind == AST_Declaration_Kind::VARIABLE);
                    assert(mem_decl->type);

                    ast_expr->type = mem_decl->type;
                    result = true;
                }
                break;
            }

            case AST_Expression_Kind::BINARY:
            {
                auto lhs = ast_expr->binary.lhs;
                auto rhs = ast_expr->binary.rhs;

                if (!try_resolve_types(resolver, lhs, suggested_type, scope))
                {
                    assert(false);
                }
                if (!try_resolve_types(resolver, rhs, suggested_type, scope))
                {
                    assert(false);
                }

                AST_Type *result_type = nullptr;

                if (lhs->type != rhs->type &&
                    lhs->type->kind == AST_Type_Kind::INTEGER)
                {
                    assert(lhs->type->kind == rhs->type->kind);

                    if (lhs->type->bit_size == rhs->type->bit_size)
                    {
                        assert(false);
                    }
                    else if (lhs->type->bit_size > rhs->type->bit_size)
                    {
                        assert(false);
                    }
                    else if (rhs->type->bit_size > lhs->type->bit_size)
                    {
                        if (lhs->type->integer.sign) assert(rhs->type->integer.sign);

                        result_type = rhs->type;
                        ast_expr->binary.lhs = ast_cast_expression_new(resolver->allocator,
                                                                       lhs, result_type,
                                                                       lhs->begin_file_pos,
                                                                       lhs->end_file_pos);
                        ast_expr->binary.lhs->flags |= AST_NODE_FLAG_RESOLVED_ID;
                        if (!try_resolve_types(resolver, ast_expr->binary.lhs, scope))
                        {
                            assert(false);
                        }
                    }
                    else assert(false);
                }
                else if (lhs->type != rhs->type)
                {
                    if (lhs->type->kind == AST_Type_Kind::FLOAT &&
                        lhs->type->bit_size >= rhs->type->bit_size &&
                        rhs->type->kind == AST_Type_Kind::INTEGER &&
                        rhs->type->integer.sign)
                    {
                        result_type = lhs->type;
                        ast_expr->binary.rhs = ast_cast_expression_new(resolver->allocator,
                                                                       rhs, result_type,
                                                                       rhs->begin_file_pos,
                                                                       rhs->end_file_pos);
                        ast_expr->binary.rhs->flags |= AST_NODE_FLAG_RESOLVED_ID;
                        if (!try_resolve_types(resolver, ast_expr->binary.rhs, scope))
                        {
                            assert(false);
                        }
                    }
                    else
                    {
                        resolver_report_mismatching_types(resolver, ast_expr, lhs->type,
                                                          rhs->type);
                        return false;
                    }
                }
                else
                {
                    assert(lhs->type == rhs->type);
                    result_type = lhs->type;
                }

                assert(result_type);
                result = true;
                ast_expr->type = result_type;
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

                            if (result)
                            {
                                assert(arg_expr->type == func_type->function.param_types[i]);
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

            case AST_Expression_Kind::ADDROF:
            {
                result = try_resolve_types(resolver, ast_expr->addrof.operand_expr, scope);
                assert(result);
                auto operand_type = ast_expr->addrof.operand_expr->type;
                assert(operand_type);
                ast_expr->type = build_data_find_or_create_pointer_type(resolver->allocator,
                                                                        resolver->build_data,
                                                                        operand_type);
                break;
            }

            case AST_Expression_Kind::COMPOUND: assert(false);

            case AST_Expression_Kind::SUBSCRIPT:
            {
                if (!try_resolve_types(resolver, ast_expr->subscript.index_expression, scope))
                {
                    assert(false);
                }

                auto index_type = ast_expr->subscript.index_expression->type;
                assert(index_type->kind == AST_Type_Kind::INTEGER);

                if (!try_resolve_types(resolver, ast_expr->subscript.pointer_expression, scope))
                {
                    assert(false);
                }

                auto pointer_type = ast_expr->subscript.pointer_expression->type;
                AST_Type *result_type = nullptr;
                if (pointer_type->kind == AST_Type_Kind::POINTER)
                {
                    result_type = pointer_type->pointer.base; 
                }
                else if (pointer_type->kind == AST_Type_Kind::ARRAY)
                {
                    result_type = pointer_type->array.element_type;
                }

                assert(result_type);

                ast_expr->type = result_type;
                result = true;
                break;
            }

            case AST_Expression_Kind::CAST:
            {
                if (!try_resolve_types(resolver, ast_expr->cast.operand_expression, scope))
                {
                    assert(false);
                }

                auto operand = ast_expr->cast.operand_expression;
                auto target_type = ast_expr->cast.target_type;

                AST_Type *result_type = nullptr;

                if (operand->type == target_type)
                {
                    result = true;
                    result_type = target_type;
                }
                else
                {
                    if (target_type->kind == AST_Type_Kind::INTEGER)
                    {
                        assert(operand->type->kind == AST_Type_Kind::INTEGER);
                        result = true;
                        result_type = target_type;
                    }
                    else if (target_type->kind == AST_Type_Kind::FLOAT)
                    {
                        assert(operand->type->kind == AST_Type_Kind::FLOAT ||
                               operand->type->kind == AST_Type_Kind::INTEGER);
                        result = true;
                        result_type = target_type;
                    }
                    else assert(false);
                }

                if (result)
                {
                    assert(result_type);
                    ast_expr->type = result_type;
                }

                break;
            }

            case AST_Expression_Kind::INTEGER_LITERAL:
            {
                if (suggested_type)
                {
                    assert(suggested_type->kind == AST_Type_Kind::INTEGER ||
                           suggested_type->kind == AST_Type_Kind::FLOAT);
                    if (resolver_literal_fits_in_type(ast_expr->integer_literal, suggested_type))
                    {
                        ast_expr->type = suggested_type;
                        result = true;
                    }
                    else assert(false);
                }
                else
                {
                    ast_expr->type = Builtin::type_s64;
                    result = true;
                }
                break;
            }

            case AST_Expression_Kind::FLOAT_LITERAL:
            {
                ast_expr->type = Builtin::type_float;
                result = true;
                break;
            }

            case AST_Expression_Kind::STRING_LITERAL:
            {
                ast_expr->type = Builtin::type_ptr_u8;
                result = true;
                break;
            }

            case AST_Expression_Kind::CHAR_LITERAL:
            {
                ast_expr->type = Builtin::type_u8;
                result = true;
                break;
            }

            case AST_Expression_Kind::BOOL_LITERAL:
            {
                ast_expr->type = Builtin::type_bool;
                result = true;
                break;
            }
        }

        if (result)
        {
            assert(ast_expr->type ||
                   (ast_expr->kind == AST_Expression_Kind::IDENTIFIER &&
                    ast_expr->identifier->declaration->kind == AST_Declaration_Kind::IMPORT));
            ast_expr->flags |= AST_NODE_FLAG_TYPED;
        }

        return result;
    }

    bool try_resolve_builtin_call_types(Resolver *resolver, AST_Expression *call_expr,
                                        Scope *scope)
    {
        assert(resolver);
        assert(call_expr);
        assert(call_expr->kind == AST_Expression_Kind::CALL);
        assert(scope);

        auto ident_expr = call_expr->call.ident_expression;
        assert(ident_expr->kind == AST_Expression_Kind::IDENTIFIER);

        auto ident_atom = ident_expr->identifier->atom;

        auto &args = call_expr->call.arg_expressions;

        if (ident_atom == Builtin::atom_exit)
        {
            assert(args.count == 1);
            auto arg = call_expr->call.arg_expressions[0];
            
            if (!try_resolve_types(resolver, arg, scope))
            {
                return false;
            }

            call_expr->type = Builtin::type_void;

            return true;
        }
        else if (ident_atom == Builtin::atom_syscall)
        {
            assert(args.count >= 1);

            bool arg_res = true;
            for (int64_t i = 0;  i < args.count; i++)
            {
                if (!try_resolve_types(resolver, args[i], scope))
                {
                    arg_res = false;
                }
            }

            if (!arg_res)
            {
                return false;
            }

            call_expr->type = Builtin::type_s64;
            return true;
        }
        else if (ident_atom == Builtin::atom_cast)
        {
            bool result = true;

            if (!try_resolve_types(resolver, args[0], scope))
            {
                result = false;
            } 

            if (!try_resolve_types(resolver, args[1], scope))
            {
                result = false;
            }

            if (result)
            {
                assert(args[0]->type);
                call_expr->type = args[0]->type;
            } 
            return result;
        }
        else assert(false);

        assert(false);
        return false;
    }

    bool try_resolve_types(Resolver *resolver, AST_Type_Spec* ts, Scope* scope,
                           AST_Type **type_target, AST_Type *suggested_type)
    {
        assert(resolver);
        assert(ts);
        assert(scope);
        assert(type_target);
        assert(*type_target == nullptr);

        if (ts->type)
        {
            *type_target = ts->type;
            assert(ts->flags & AST_NODE_FLAG_TYPED);
            return true;
        }

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
                assert(decl->kind == AST_Declaration_Kind::STRUCTURE ||
                       decl->kind == AST_Declaration_Kind::TYPE);
                if (decl->type)
                {
                *type_target = decl->type;
                ts->type = decl->type;
                result = true;
                }
                else
                {
                    result = false;
                }
                break;
            }

            case AST_Type_Spec_Kind::POINTER:
            {
                AST_Type *base_type = nullptr;
                result = try_resolve_types(resolver, ts->base_type_spec, scope, &base_type,
                                           nullptr);
                if (result)
                {
                    assert(base_type);
                    *type_target = build_data_find_or_create_pointer_type(resolver->allocator,
                                                                          resolver->build_data,
                                                                          base_type);
                    assert(*type_target);
                    ts->type = *type_target;
                }
                else
                {
                    result = false;
                }
                break;
            }

            case AST_Type_Spec_Kind::DOT:
            {
                if (!try_resolve_types(resolver, ts->dot_expression, scope))
                {
                    result = false;
                }
                else
                {
                    auto type = ts->dot_expression->type;
                    ts->type = type;
                    *type_target = type;
                    result = true;
                }
                break;
            }

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
                    if (param_ts->type)
                    {
                        param_type = param_ts->type;
                    }
                    else
                    {
                        if (!try_resolve_types(resolver, param_ts, scope, &param_type, nullptr))
                        {
                            assert(false);
                        }
                    }

                    assert(param_type);
                    array_append(&param_types, param_type);
                }

                assert(param_types.count == param_count);
                assert(param_types.count <= param_count);

                if (param_types.count < param_count)
                {
                    assert(false);
                    array_free(&param_types);
                }

                AST_Type *return_type = nullptr;
                bool ret_mismatch = false;
                if (ts->function.return_type_spec)
                {
                    if (!try_resolve_types(resolver, ts->function.return_type_spec, scope,
                                           &return_type, nullptr))
                    {
                        assert(false);
                    }

                    assert(return_type);
                    if (suggested_type)
                    {
                        if (return_type != suggested_type)
                        {
                            ret_mismatch = true;
                            resolver_report_mismatching_types(resolver,
                                                              ts->function.return_type_spec,
                                                              suggested_type,
                                                              return_type);
                        }
                    }
                } 
                else if (suggested_type)
                {
                    return_type = suggested_type; 
                }
                else
                {
                    return_type = Builtin::type_void; 
                }

                if (!ret_mismatch)
                {
                    auto func_type = find_or_create_function_type(resolver, param_types,
                                                                  return_type, scope);
                    assert(func_type);
                    result = true;
                    ts->type = func_type;
                    *type_target = func_type;
                }
                break;
            }

            case AST_Type_Spec_Kind::ARRAY:
            {
                AST_Type *element_type = nullptr;
                if (!try_resolve_types(resolver, ts->array.element_type_spec, scope,
                                       &element_type, nullptr))
                {
                    assert(false);
                }

                assert(element_type);

                auto length_expr = ts->array.length_expression;
                int64_t length = -1;
                if (length_expr)
                {
                    if (!try_resolve_types(resolver, length_expr, scope))
                    {
                        assert(false);
                    }

                    assert(length_expr->type);
                    assert(length_expr->type->kind == AST_Type_Kind::INTEGER);

                    assert(length_expr->is_const);

                    Const_Value length_val = const_interpret_expression(length_expr);
                    assert(length_val.type == length_expr->type);
                    assert(length_val.s64 > 0);

                    length = length_val.s64; 
                }

                assert(length);

                auto array_type = find_or_create_array_type(resolver, element_type, length,
                                                            scope);
                assert(array_type);
                result = true;
                ts->type = array_type;
                *type_target = array_type;
                break;
            }

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

        assert(ast_node->flags & AST_NODE_FLAG_TYPED);

        if (ast_node->flags & AST_NODE_FLAG_SIZED) return  true;

        bool result = true;

        switch (ast_node->kind)
        {
            case AST_Node_Kind::INVALID: assert(false);
            case AST_Node_Kind::MODULE: assert(false);
            case AST_Node_Kind::IDENTIFIER: assert(false);

            case AST_Node_Kind::DECLARATION:
            {
                auto decl = static_cast<AST_Declaration*>(ast_node);
                switch (decl->kind)
                {
                    case AST_Declaration_Kind::INVALID: assert(false);
                    case AST_Declaration_Kind::IMPORT: assert(false);

                    case AST_Declaration_Kind::USING: assert(false);

                    case AST_Declaration_Kind::VARIABLE:
                    {
                        result = try_resolve_sizes(resolver, decl->type, scope);
                        if (result && decl->variable.init_expression)
                        {
                            result = try_resolve_sizes(resolver, decl->variable.init_expression,
                                                       scope);
                        }

                        if (result)
                            ast_node->flags |= AST_NODE_FLAG_SIZED;
                        break;
                    }

                    case AST_Declaration_Kind::CONSTANT: assert(false);
                    case AST_Declaration_Kind::PARAMETER: assert(false);
                    case AST_Declaration_Kind::FUNCTION: assert(false);
                    case AST_Declaration_Kind::TYPE: assert(false);
                    case AST_Declaration_Kind::STRUCTURE: assert(false);
                    case AST_Declaration_Kind::POLY_TYPE: assert(false);
                }
                break;
            }

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

            case AST_Type_Kind::INTEGER:
            {
                assert(ast_type->bit_size > 0);
                ast_type->flags |= AST_NODE_FLAG_SIZED;
                result = true;
                break;
            }

            case AST_Type_Kind::FLOAT: assert(false);
            case AST_Type_Kind::BOOL: assert(false);
            case AST_Type_Kind::POINTER: assert(false);

            case AST_Type_Kind::FUNCTION:
            {
                assert(ast_type->bit_size == Builtin::pointer_size);
                ast_type->flags |= AST_NODE_FLAG_SIZED;
                result = true;
                break;
            }
            
            case AST_Type_Kind::STRUCTURE:
            {
                //@TODO: Padding etc...
                uint64_t bit_size = 0;
                for (int64_t i = 0; i < ast_type->structure.member_types.count; i++)
                {
                    auto mem_type = ast_type->structure.member_types[i];
                    assert(mem_type->flags & AST_NODE_FLAG_SIZED);
                    assert(mem_type->bit_size);
                    assert(mem_type->bit_size % 8 == 0);
                    bit_size += mem_type->bit_size;
                    assert(bit_size % 8 == 0);
                }

                assert(bit_size);
                ast_type->bit_size = bit_size;
                ast_type->flags |= AST_NODE_FLAG_SIZED;
                break;
            }

            case AST_Type_Kind::ARRAY:
            {
                auto elem_size = ast_type->array.element_type->bit_size;
                assert(elem_size);
                auto bit_size = elem_size * ast_type->array.element_count;
                assert(bit_size);
                ast_type->bit_size = bit_size;
                ast_type->flags |= AST_NODE_FLAG_SIZED;
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
            func_type->flags |= AST_NODE_FLAG_RESOLVED_ID;
            func_type->flags |= AST_NODE_FLAG_TYPED;
            array_append(&resolver->build_data->type_table, func_type);
            queue_size_job(resolver, func_type, scope);
        }

        assert(func_type);

        return func_type;
    }

    AST_Type* find_or_create_array_type(Resolver *resolver, AST_Type *element_type,
                                        int64_t element_count, Scope *current_scope)
    {
        assert(element_count > 0);

        AST_Type *array_type = build_data_find_array_type(resolver->allocator,
                                                          resolver->build_data,
                                                          element_type,
                                                          element_count);

        if (!array_type)
        {
            array_type = build_data_create_array_type(resolver->allocator,
                                                      resolver->build_data,
                                                      element_type,
                                                      element_count);
            queue_size_job(resolver, array_type, current_scope);
        }
        
        assert(array_type);
        return array_type;

    }

    AST_Type* create_structure_type(Resolver *resolver, AST_Declaration *struct_decl, 
                                    Array<AST_Type*> mem_types, Scope *mem_scope,
                                    Scope *current_scope)
    {
        assert(resolver);
        assert(struct_decl);
        assert(struct_decl->kind == AST_Declaration_Kind::STRUCTURE);
        assert(mem_types.count);
        assert(mem_scope);

        auto result =  ast_structure_type_new(resolver->allocator, struct_decl, mem_types,
                                              mem_scope);
        result->flags |= AST_NODE_FLAG_RESOLVED_ID;
        result->flags |= AST_NODE_FLAG_TYPED;
        //result->flags |= AST_NODE_FLAG_SIZED;
        array_append(&resolver->build_data->type_table, result);
        queue_size_job(resolver, result, current_scope);
        return result;
    }

    void queue_parse_job(Resolver *resolver, String module_name, String module_path,
                         AST_Declaration *import_decl)
    {
        if (import_decl) assert(import_decl->kind == AST_Declaration_Kind::IMPORT);

        auto job = resolve_job_new(resolver->allocator, module_name, module_path,
                                    import_decl);
        assert(job);
        queue_enqueue(&resolver->parse_job_queue, job);
    }

    void queue_ident_job(Resolver *resolver, AST_Node *ast_node, Scope *scope)
    {
        assert(resolver);
        assert(ast_node);

        auto allocator = resolver->allocator;
        assert(allocator);

        auto job = resolve_job_ident_new(allocator, ast_node, scope);
        assert(job);
        queue_enqueue(&resolver->ident_job_queue, job);
    }

    void queue_type_job(Resolver *resolver, AST_Node *ast_node, Scope *scope)
    {
        assert(resolver);
        assert(ast_node);
        assert(scope);

        auto allocator = resolver->allocator;
        assert(allocator);

        auto job = resolve_job_type_new(allocator, ast_node, scope);
        assert(job);
        queue_enqueue(&resolver->type_job_queue, job);
    }

    void queue_size_job(Resolver *resolver, AST_Node *ast_node, Scope *scope)
    {
        assert(resolver);
        assert(ast_node);
        assert(scope);

        auto allocator = resolver->allocator;
        assert(allocator);

        auto job = resolve_job_size_new(allocator, ast_node, scope);
        assert(job);
        queue_enqueue(&resolver->size_job_queue, job);

    }

    void queue_emit_bytecode_job(Resolver *resolver, AST_Node *ast_node, Scope *scope)
    {
        assert(resolver->allocator);

        if (ast_node->flags & AST_NODE_FLAG_QUEUED_BYTECODE_EMISSION)
            return;

        auto job = resolve_job_emit_bytecode_new(resolver->allocator, ast_node, scope);
        assert(job);
        queue_enqueue(&resolver->emit_bytecode_job_queue, job);

        ast_node->flags |= AST_NODE_FLAG_QUEUED_BYTECODE_EMISSION;
    }
    
    void queue_emit_llvm_func_job(Resolver *resolver, Bytecode_Function *bc_func)
    {
        assert(resolver->allocator);

        auto job = resolve_job_emit_llvm_func_new(resolver->allocator, bc_func);
        assert(job);
        queue_enqueue(&resolver->emit_llvm_func_job_queue, job);
    }

    void queue_emit_llvm_binary_job(Resolver *resolver, const char *output_file_name)
    {
        assert(resolver->allocator);

        auto job = resolve_job_emit_llvm_binary_new(resolver->allocator, output_file_name);
        assert(job);
        queue_enqueue(&resolver->emit_llvm_binary_job_queue, job);
    }

    void queue_emit_bytecode_jobs_from_declaration(Resolver *resolver,
                                                   AST_Declaration *entry_decl,
                                                   Scope *scope)
    {
        switch (entry_decl->kind)
        {
            case AST_Declaration_Kind::INVALID: assert(false);
            case AST_Declaration_Kind::IMPORT: assert(false);

            case AST_Declaration_Kind::USING: assert(false);

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

            case AST_Statement_Kind::BLOCK:
            {
                for (int64_t i = 0; i < stmt->block.statements.count; i++)
                {
                    queue_emit_bytecode_jobs_from_statement(resolver, stmt->block.statements[i],
                                                           stmt->block.scope);
                }
                break;
            }

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

            case AST_Statement_Kind::WHILE:
            {
                queue_emit_bytecode_jobs_from_expression(resolver, stmt->while_stmt.cond_expr,
                                                         scope);
                queue_emit_bytecode_jobs_from_statement(resolver, stmt->while_stmt.body, scope);
                break;
            }

            
            case AST_Statement_Kind::IF:
            {
                queue_emit_bytecode_jobs_from_expression(resolver, stmt->if_stmt.cond_expr,
                                                         scope);
                queue_emit_bytecode_jobs_from_statement(resolver, stmt->if_stmt.then_stmt,
                                                        stmt->if_stmt.then_scope);

                if (stmt->if_stmt.else_stmt)
                {
                    queue_emit_bytecode_jobs_from_statement(resolver, stmt->if_stmt.else_stmt,
                                                            stmt->if_stmt.else_scope);
                }
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
                
                auto decl_kind = ident->declaration->kind;

                if (decl_kind != AST_Declaration_Kind::VARIABLE &&
                    decl_kind != AST_Declaration_Kind::PARAMETER &&
                    decl_kind != AST_Declaration_Kind::TYPE)
                {
                    assert(false); 
                }

                break;
            }

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);

            case AST_Expression_Kind::DOT:
            {
                queue_emit_bytecode_jobs_from_expression(resolver, expr->dot.parent_expression,
                                                         scope); 
                break;
            }

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

            case AST_Expression_Kind::ADDROF:
            {
                queue_emit_bytecode_jobs_from_expression(resolver, expr->addrof.operand_expr,
                                                         scope);
                break;
            }

            case AST_Expression_Kind::COMPOUND: assert(false);

            case AST_Expression_Kind::SUBSCRIPT:
            {
                queue_emit_bytecode_jobs_from_expression(resolver,
                                                         expr->subscript.pointer_expression,
                                                         scope);
                queue_emit_bytecode_jobs_from_expression(resolver,
                                                         expr->subscript.index_expression,
                                                         scope);
            }

            case AST_Expression_Kind::CAST:
            {
                queue_emit_bytecode_jobs_from_expression(resolver,
                                                         expr->cast.operand_expression,
                                                         scope);
                break;
            }

            case AST_Expression_Kind::INTEGER_LITERAL:
            case AST_Expression_Kind::FLOAT_LITERAL:
            case AST_Expression_Kind::STRING_LITERAL:
            case AST_Expression_Kind::CHAR_LITERAL:
            case AST_Expression_Kind::BOOL_LITERAL:
            {
                break;
            }
        }
    }

    Resolve_Job *resolve_job_new(Allocator *allocator, Resolve_Job_Kind kind)
    {
        auto result = alloc_type<Resolve_Job>(allocator);
        assert(result);
        result->kind = kind;
        result->result = {};
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
        auto result = resolve_job_new(allocator, Resolve_Job_Kind::EMIT_LLVM_FUNC);
        result->bc_func = bc_func;
        return result;
    }

    Resolve_Job *resolve_job_new(Allocator *allocator, const char *output_file_name)
    {
        auto result = resolve_job_new(allocator, Resolve_Job_Kind::EMIT_LLVM_BINARY);
        result->llvm_bin.output_file_name = output_file_name;
        return result;
    }

    Resolve_Job *resolve_job_new(Allocator *allocator, String module_name, String module_path,
                                 AST_Declaration *import_decl)
    {
        if (import_decl) assert(import_decl->kind == AST_Declaration_Kind::IMPORT);

        auto result = resolve_job_new(allocator, Resolve_Job_Kind::PARSE);
        result->parse.module_name = module_name;
        result->parse.module_path = module_path;
        result->parse.import_decl = import_decl;
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

    Resolve_Job *resolve_job_emit_llvm_func_new(Allocator *allocator, Bytecode_Function *bc_func)
    {
        return resolve_job_new(allocator, bc_func);
    }

    Resolve_Job *resolve_job_emit_llvm_binary_new(Allocator *allocator,
                                                  const char *output_file_name)
    {
        return resolve_job_new(allocator,  output_file_name);
    }

    bool resolver_literal_fits_in_type(const Integer_Literal &number_literal, AST_Type *type)
    {

        if (type->kind == AST_Type_Kind::INTEGER)
        {
            auto val = number_literal.s64;

#define CHECK_BIT_WIDTH_CASE(bit_width) \
            case bit_width: { \
                if (type->integer.sign) { \
                    return val >= INT##bit_width##_MIN && val <= INT##bit_width##_MAX; \
                } else {\
                    return (uint##bit_width##_t)val >= 0 && \
                           (uint##bit_width##_t)val <= UINT##bit_width##_MAX; \
                } \
            }

            switch (type->bit_size)
            {
                CHECK_BIT_WIDTH_CASE(8);
                CHECK_BIT_WIDTH_CASE(16);
                CHECK_BIT_WIDTH_CASE(32);
                CHECK_BIT_WIDTH_CASE(64);

                default: assert(false);
            }

#undef CHECK_BIT_WIDTH_CASE
        }
        else if (type->kind == AST_Type_Kind::FLOAT)
        {
            
            auto val = number_literal.s64;

#define FLOAT_INT_MAX 0x1000000
#define DOUBLE_INT_MAX 0x20000000000000

            switch (type->bit_size)
            {
                case 32:
                {
                    return val >= (-FLOAT_INT_MAX) && val <= FLOAT_INT_MAX; 
                }

                case 64: assert(false);
                {
                    return val >= (-DOUBLE_INT_MAX) && val <= DOUBLE_INT_MAX; 
                }

                default: assert(false); 
            }

#undef FLOAT_INT_MAX 
#undef DOUBLE_INT_MAX 
        }
        else assert(false);
    }

    bool is_entry_decl(Resolver *resolver, AST_Declaration *decl)
    {
        if (resolver->llvm_builder.target_platform == Zodiac_Target_Platform::LINUX)
        {
            if (decl->kind == AST_Declaration_Kind::FUNCTION &&
                decl->identifier->atom == Builtin::atom__start &&
                (decl->decl_flags & AST_DECL_FLAG_IS_NAKED))
            {
                return true;
            }
        }
        else if (resolver->llvm_builder.target_platform == Zodiac_Target_Platform::WINDOWS)
        {
            if (decl->kind == AST_Declaration_Kind::FUNCTION &&
                decl->identifier->atom == Builtin::atom_mainCRTStartup)
            {
                return true;
            }
        }

        return false;
    }

    bool is_bc_entry_decl(Resolver *resolver, AST_Declaration *decl)
    {
        if (decl->kind == AST_Declaration_Kind::FUNCTION &&
            decl->identifier->atom == Builtin::atom_main)
        {
            return true;
        }

        return false;
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

    void resolver_report_mismatching_types(Resolver *resolver, AST_Node *node,
                                           AST_Type *expected_type, AST_Type *actual_type)
    {
        for (int64_t i = 0; i < resolver->errors.count; i++)
        {
            auto &err = resolver->errors[i];
            if (err.kind == Resolve_Error_Kind::MISMATCHING_TYPES &&
                    err.ast_node == node)
            {
                return;
            }
        }

        auto ta = temp_allocator_get();

        auto expected_type_str = ast_type_to_string(ta, expected_type);
        auto actual_type_str = ast_type_to_string(ta, actual_type);

        resolver_report_error(resolver,
                              Resolve_Error_Kind::MISMATCHING_TYPES,
                              node, 
                              "Mismatching types: expected '%s', got '%s'",
                              expected_type_str.data,
                              actual_type_str.data);
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

    void resolver_clear_errors(Resolver *resolver)
    {
        for (int64_t i = 0; i < resolver->errors.count; i++)
        {
            auto &err = resolver->errors[i];
            free(resolver->err_allocator, err.message);
        }

        resolver->errors.count = 0;
    }

    Resolve_Error resolver_make_error(Resolve_Error_Kind kind, const char *message,
                                      int64_t message_size, AST_Node *ast_node)
    {
        return { kind, message, message_size, ast_node };
    }
}
