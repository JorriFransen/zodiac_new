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

#include <tracy/Tracy.hpp>

namespace Zodiac
{
    void resolver_init(Allocator *allocator, Resolver *resolver,
                       Build_Data *build_data, String first_file_path)
    {
        assert(allocator);
        assert(resolver);

        resolver->allocator = allocator;

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

        auto entry_file_path = string_append(allocator, resolver->first_file_dir, "entry.zdc");
        assert(is_regular_file(entry_file_path));
        resolver->entry_module_path = entry_file_path;

        resolver->build_data = build_data;
        assert(build_data);
        resolver->lexer = lexer_create(allocator, build_data);
        resolver->parser = parser_create(allocator, build_data);

        stack_init(allocator, &resolver->break_node_stack);
        stack_init(allocator, &resolver->active_static_branch_stack);

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

        array_init(allocator, &resolver->parsed_modules);

        resolver->llvm_error = false;
        resolver->parse_error = false;

        
        //@TODO: initialize with builtin global count
        resolver->global_scope = scope_new(allocator, Scope_Kind::GLOBAL, nullptr);
        auto decls_to_resolve = builtin_populate_scope(allocator, resolver->global_scope);

        for (int64_t i = 0; i < decls_to_resolve.count; i++)
        {
            queue_ident_job(resolver, decls_to_resolve[i], resolver->global_scope, true);
        }

        array_free(&decls_to_resolve);

        // auto entry_module_path = string_append(allocator, resolver->first_file_dir, "entry.zdc");
        // queue_parse_job(resolver, string_ref("entry"), entry_module_path, true);

        queue_parse_job(resolver, resolver->first_file_name, resolver->first_file_path, true,
                        true);

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

        auto bd = resolver->build_data;

        if (bd->errors.count == 0 &&
            !resolver->parse_error &&
            !resolver->llvm_error)
        {
            assert(queue_count(&resolver->parse_job_queue) == 0);
            assert(queue_count(&resolver->ident_job_queue) == 0);
            assert(queue_count(&resolver->type_job_queue) == 0);
            assert(queue_count(&resolver->size_job_queue) == 0);
            assert(queue_count(&resolver->emit_bytecode_job_queue) == 0);
            assert(queue_count(&resolver->emit_llvm_func_job_queue) == 0);
            assert(queue_count(&resolver->emit_llvm_binary_job_queue) == 0);

            assert(stack_count(&resolver->active_static_branch_stack) == 0);
            assert(stack_count(&resolver->break_node_stack) == 0);
        }

        Resolve_Result result = {};
        result.error_count = bd->errors.count;
        result.parse_error = resolver->parse_error;
        result.llvm_error = resolver->llvm_error;
        return result;
    }

    void start_resolve_pump(Resolver *resolver)
    {
        bool done = false;

        auto options = resolver->build_data->options;

        FrameMark

        int cycle_count = 0;

        while (!done)
        {
            ZoneScopedNCS("compiler_pump", 0xffff00, 32)
            { ZoneScopedNC("parse_jobs", 0xff0000)

            auto parse_job_count = queue_count(&resolver->parse_job_queue);
            while (parse_job_count--)
            {
                auto job = queue_dequeue(&resolver->parse_job_queue);
                bool job_done = try_resolve_job(resolver, job);

                if (!job_done)
                {
                    done = true;
                    resolver->parse_error = true;
                    break;
                }
                else
                {
                    assert(job_done);
                    auto ast_module = job->result.ast_module;
                    assert(ast_module);
                    queue_ident_job(resolver, ast_module, ast_module->module_scope,
                                    job->active_static_branch);
                }
            } }

            { ZoneScopedNC("ident_jobs", 0x00ff00)

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
                        queue_type_job(resolver, job->declaration, job->node_scope,
                                       job->active_static_branch);
                    }

                    free_job(resolver, job);
                }
            } }

            { ZoneScopedNC("type_jobs", 0x0000ff) 

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
                    assert(job->ast_node->kind == AST_Node_Kind::EXPRESSION ||
                           job->ast_node->kind == AST_Node_Kind::DECLARATION);

                    free_job(resolver, job);
                    resolver->progression.type_job_finish_count += 1;
                }

            } }

            { ZoneScopedNC("size_jobs", 0xff0000) 

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
            } }

            { ZoneScopedNC("bytecode_jobs", 0x00ff00) 

            if (resolver->build_data->redeclaration_error) break;

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
                    if (job->ast_node->kind == AST_Node_Kind::DECLARATION)
                    {
                        auto decl = static_cast<AST_Declaration*>(job->ast_node);
                        if (decl->kind == AST_Declaration_Kind::FUNCTION)
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
                            {
                                auto bc_func = job->result.bc_func;
                                bool is_entry = (bc_func->flags & BYTECODE_FUNC_FLAG_CRT_ENTRY);
                                
                                if (!(options->link_c && is_entry))
                                {
                                    queue_emit_llvm_func_job(resolver, bc_func);
                                }
                            }
                        }
                        else if (decl->kind == AST_Declaration_Kind::VARIABLE)
                        {
                            assert(decl->decl_flags & AST_DECL_FLAG_GLOBAL);
                            if (!options->dont_emit_llvm)
                                queue_emit_llvm_global_job(resolver, job->result.bc_global);
                        }
                    }

                    free_job(resolver, job);
                }
            } }

            { ZoneScopedNC("llvm_jobs", 0x0000ff)

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
            } }

            { ZoneScopedNC("llvm_binary_jobs", 0x00ffff)

            auto llvm_binary_job_count = queue_count(&resolver->emit_llvm_binary_job_queue);
            while (llvm_binary_job_count--)
            {

                auto job = queue_dequeue(&resolver->emit_llvm_binary_job_queue);
                bool job_done = try_resolve_job(resolver, job);

                if (!job_done)
                {
                    if (!resolver->build_data->link_error)
                    {
                        queue_enqueue(&resolver->emit_llvm_binary_job_queue, job);
                    }
                }
                else
                {
                    free_job(resolver, job);
                }
            } }

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

            assert(stack_count(&resolver->active_static_branch_stack) == 0);


            if (resolver_has_progressed(resolver) && !resolver->parse_error)
            {
                zodiac_clear_errors(resolver->build_data);
            } 
            else if (resolver->build_data->errors.count)
            {
                done = true;
            }

            resolver_save_progression(resolver);

            FrameMark

            cycle_count += 1;
        }

        zodiac_report_errors(resolver->build_data);

        if (options->verbose) printf("Resolving finished in %d cycles\n", cycle_count);
    }

    bool resolver_has_progressed(Resolver *resolver)
    {
        auto p = &resolver->progression;

        if (p->type_job_finish_count > 0)
        {
            return true;
        }

        if (p->scope_imports_done)
        {
            return true;
        }

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

        p->type_job_finish_count = 0;
        p->scope_imports_done = false;

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

        if (!job->active_static_branch)
        {
            resolver_push_active_static_branch(resolver, false);
        }

        auto options = resolver->build_data->options;

        bool result = false;

        switch (job->kind)
        {
            case Resolve_Job_Kind::INVALID: assert(false);

            case Resolve_Job_Kind::PARSE:
            {
                ZoneScopedNC("try_resolve_job (PARSE)", 0x0000ff)

                bool done = false;

                for (int64_t i = 0; i < resolver->parsed_modules.count; i++)
                {
                    auto &pm = resolver->parsed_modules[i];
                    if (string_equal(pm.full_path, job->parse.module_path))
                    {
                        job->result.ast_module = pm.ast;
                        result = true;
                        done = true;
                        break;
                    }
                }

                if (done) break;

                Parsed_File epf = {};
                bool append = false;

                if (job->parse.insert_entry_module)
                {
                    append = true;

                    Lexed_File lf_entry = lexer_lex_file(&resolver->lexer,
                                                         resolver->entry_module_path);
                    if (!lf_entry.valid)
                    {
                        assert(false);
                        break;
                    }

                    Token_Stream *ets = lexer_new_token_stream(resolver->allocator, &lf_entry); 

                    epf = parser_parse_file(&resolver->parser, ets);
                    if (!epf.valid)
                    {
                        result = false;
                        break;
                    }

                    lexer_free_lexed_file(&resolver->lexer, &lf_entry);
                    ets->free();
                    
                }

                Lexed_File lexed_file = lexer_lex_file(&resolver->lexer, job->parse.module_path);
                if (!lexed_file.valid) 
                {
                    result = false;
                    break;
                }

                Token_Stream *token_stream = lexer_new_token_stream(resolver->allocator,
                                                                    &lexed_file);
                Parsed_File parsed_file = {};

                if (append)
                {
                    parsed_file = epf;
                }
                else
                {
                    parsed_file_init(&resolver->parser, &parsed_file);
                }

                parser_parse_file(&resolver->parser, token_stream, &parsed_file);
                if (!parsed_file.valid) 
                {
                    result = false;
                    break;
                }

                if (options->print_parse_tree) parsed_file_print(&parsed_file);

                AST_Builder ast_builder = { resolver->allocator, resolver->build_data };
                auto module_ast = ast_create_from_parsed_file(&ast_builder, &parsed_file,
                                                              resolver->global_scope);
                assert(module_ast);

                // @CLEANUP: @TODO: @FIXME: This should only check for redeclarations
                //                           (later maybe other errors generated by the 
                //                            ast builder)
                if (resolver->build_data->errors.count)
                {
                    result = false;
                    break;
                }

                if (options->print_ast) ast_print(module_ast);

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

                    if (decl->kind == AST_Declaration_Kind::FUNCTION)
                    {
                        assert(decl->decl_flags & AST_DECL_FLAG_REGISTERED_BYTECODE);

                        auto bc_func =
                            bytecode_emit_function_declaration(&resolver->bytecode_builder, decl);
                        job->result.bc_func = bc_func;
                    }
                    else if (decl->kind == AST_Declaration_Kind::VARIABLE)
                    {
                        assert(decl->decl_flags & AST_DECL_FLAG_GLOBAL);
                        auto bc_glob = bytecode_emit_global_variable(&resolver->bytecode_builder,
                                                                     decl);
                        job->result.bc_global = bc_glob;
                    }
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

            case Resolve_Job_Kind::EMIT_LLVM_GLOB:
            {
                auto bc_glob = job->bc_glob;
                llvm_emit_global(&resolver->llvm_builder, bc_glob);
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

        if (!job->active_static_branch)
        {
            resolver_pop_active_static_branch(resolver);
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
                bool active_branch = resolver_is_active_static_branch(resolver);
                auto module = static_cast<AST_Module*>(ast_node);
                for (int64_t i = 0; i < module->declarations.count; i++)
                {
                    auto decl = module->declarations[i]; 

                    if (!try_resolve_identifiers(resolver, decl, module->module_scope))
                    {
                        queue_ident_job(resolver, decl, module->module_scope, active_branch);
                    }
                    else if ((decl->flags & AST_NODE_FLAG_RESOLVED_ID))
                    {
                        queue_type_job(resolver, decl, module->module_scope, active_branch); 
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

            case AST_Node_Kind::SWITCH_CASE: assert(false);

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

    bool try_resolve_identifiers(Resolver *resolver, AST_Identifier *identifier, Scope *scope)
    {

        bool result = true;

        auto decl = scope_find_declaration(scope, identifier);
        if (!decl)
        {
            resolver_report_undeclared_identifier(resolver, identifier);
            result = false; 
        }
        else
        {
            assert(identifier->declaration);
        }


        if (result)
        {
            identifier->flags |= AST_NODE_FLAG_RESOLVED_ID;
        }
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

                if (!(ast_decl->flags & AST_NODE_FLAG_QUEUED_PARSING))
                {
                    //@TODO: Report error
                    assert(is_regular_file(file_path));

                    bool active_static_branch = resolver_is_active_static_branch(resolver);
                    queue_parse_job(resolver, module_name,
                                    string_copy(resolver->allocator, file_path),
                                    active_static_branch, false);
                    ast_decl->flags |= AST_NODE_FLAG_QUEUED_PARSING;
                    result = true;
                    apply_flag = false;
                    queue_ident_job(resolver, ast_decl, scope, active_static_branch);
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

                        auto using_scope = module->module_scope;
                        result = resolver_import_using_scope(resolver, scope, using_scope);
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

                if (result &&
                    !(ast_decl->decl_flags & AST_DECL_FLAG_GLOBAL))
                {
                    queue_type_job(resolver, ast_decl, scope,
                                   resolver_is_active_static_branch(resolver));   
                }

                break;
            }

            case AST_Declaration_Kind::CONSTANT:
            {
                if (ast_decl->constant.type_spec)
                {
                    result = try_resolve_identifiers(resolver, ast_decl->constant.type_spec,
                                                     scope);
                }

                assert(ast_decl->constant.init_expression);
                if (result)
                {
                    result = try_resolve_identifiers(resolver,
                                                     ast_decl->constant.init_expression,
                                                     scope);
                }

                if (result &&
                    !(ast_decl->decl_flags & AST_DECL_FLAG_IS_ENUM_MEMBER) &&
                    !(ast_decl->decl_flags & AST_DECL_FLAG_GLOBAL))
                {
                    queue_type_job(resolver, ast_decl, scope,
                                   resolver_is_active_static_branch(resolver));   
                }

                break;
            }

            case AST_Declaration_Kind::PARAMETER:
            {
                result = try_resolve_identifiers(resolver, ast_decl->parameter.type_spec, scope);

                if (result)
                    queue_type_job(resolver, ast_decl, scope,
                                   resolver_is_active_static_branch(resolver));
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

            case AST_Declaration_Kind::TYPEDEF:
            {
                result = true;

                if (!try_resolve_identifiers(resolver, ast_decl->typedef_decl.type_spec,
                                             scope))
                {
                    result = false;
                }
                break;
            }

            case AST_Declaration_Kind::STRUCTURE:
            {
                assert(ast_decl->structure.parameters.count == 0);

                bool mem_res = true;

                auto mem_decls = ast_decl->structure.member_declarations;
                for (int64_t i = 0; i < mem_decls.count; i++)
                {
                    if (!try_resolve_identifiers(resolver, mem_decls[i],
                                                 ast_decl->structure.member_scope))
                    {
                        mem_res = false;
                    }
                } 

                result = mem_res;
                break;
            }

            case AST_Declaration_Kind::ENUM:
            {

                if (ast_decl->enum_decl.type_spec)
                {
                    if (!try_resolve_identifiers(resolver, ast_decl->enum_decl.type_spec,
                                                 ast_decl->enum_decl.member_scope))
                    {
                        assert(false);
                    }
                }

                bool mem_res = true;
                auto mem_decls = ast_decl->enum_decl.member_declarations;
                for (int64_t i = 0; i < mem_decls.count; i++)
                {
                    auto mem_decl = mem_decls[i];
                    assert(mem_decl->kind == AST_Declaration_Kind::CONSTANT);

                    auto init_expr = mem_decl->constant.init_expression;
                    if (init_expr)
                    {
                        assert(init_expr->kind == AST_Expression_Kind::INTEGER_LITERAL ||
                               init_expr->kind == AST_Expression_Kind::IDENTIFIER);
                    }
                    else
                    {
                        mem_decl->constant.init_expression =
                            ast_integer_literal_expression_new(resolver->allocator,
                                                               0,
                                                               mem_decl->begin_file_pos,
                                                               mem_decl->end_file_pos);
                    }

                    if (!try_resolve_identifiers(resolver, mem_decl,
                                                 ast_decl->enum_decl.member_scope))
                    {
                        mem_res = false;
                    }

                    result = mem_res;
                }
                break;
            }

            case AST_Declaration_Kind::POLY_TYPE: assert(false);
                                                  
            case AST_Declaration_Kind::STATIC_IF:
            {
                auto cond_expr = ast_decl->static_if.cond_expression;

                if (!try_resolve_identifiers(resolver, cond_expr, scope))
                {
                    result = false;
                }
                else
                {
                    queue_type_job(resolver, cond_expr, scope,
                                   resolver_is_active_static_branch(resolver));
                }

                if (!(cond_expr->flags & AST_NODE_FLAG_TYPED))
                {
                    result = false;
                    break;
                }

                Const_Value cond_val = const_interpret_expression(cond_expr);

                resolver_push_active_static_branch(resolver, cond_val.boolean);

                auto then_decls = ast_decl->static_if.then_declarations;
                for (int64_t i = 0; i < then_decls.count; i++)
                {
                    if (!try_resolve_identifiers(resolver, then_decls[i],
                                                 ast_decl->static_if.then_scope))
                    {
                        result = false;
                    }
                }

                resolver_pop_active_static_branch(resolver);
                resolver_push_active_static_branch(resolver, !cond_val.boolean);

                auto else_decls = ast_decl->static_if.else_declarations;
                for (int64_t i = 0; i < else_decls.count; i++)
                {
                    if (!try_resolve_identifiers(resolver, else_decls[i],
                                                 ast_decl->static_if.else_scope))
                    {
                        result = false;
                    }
                }

                resolver_pop_active_static_branch(resolver);

                break;
            }

            case AST_Declaration_Kind::STATIC_ASSERT:
            {
                if (!try_resolve_identifiers(resolver,
                                             ast_decl->static_assert_decl.cond_expression,
                                             scope))
                {
                    result = false;
                }

                // We need to queue a type job, because otherwise the condition will not be checked,
                // and no error will be shown if there is other errors.

                if (result && resolver_is_active_static_branch(resolver))
                    queue_type_job(resolver, ast_decl, scope, true);

                break;
            }
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
                else
                {
                    AST_Declaration *lhs_decl = resolver_get_declaration(lhs);
                    assert(lhs_decl);

                    if (lhs_decl->kind == AST_Declaration_Kind::CONSTANT)
                    {
                        zodiac_report_error(resolver->build_data,
                                            Zodiac_Error_Kind::ASSIGNING_TO_CONST,
                                            ast_stmt, "Assigning to a constant value");
                        result = false;
                    }
                    else
                    {
                        assert(lhs_decl->kind == AST_Declaration_Kind::VARIABLE ||
                               lhs_decl->kind == AST_Declaration_Kind::PARAMETER);
                    }
                }

                if (result)
                {
                    if (!try_resolve_identifiers(resolver, rhs, scope))
                    {
                        result = false;
                    }
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

            case AST_Statement_Kind::BREAK:
            {
                assert(stack_count(&resolver->break_node_stack));
                result = true;
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
                    result = false;
                }

                resolver_push_break_node(resolver, ast_stmt);

                if (!try_resolve_identifiers(resolver, ast_stmt->while_stmt.body,
                                             ast_stmt->while_stmt.body_scope))
                {
                    result = false;
                }

                resolver_pop_break_node(resolver);

                break;
            }
            
            case AST_Statement_Kind::FOR:
            {
                auto for_scope = ast_stmt->for_stmt.scope;

                result = true;

                for (int64_t i = 0; i < ast_stmt->for_stmt.init_statements.count; i++)
                {
                    auto init_stmt = ast_stmt->for_stmt.init_statements[i];
                    if (!try_resolve_identifiers(resolver, init_stmt, for_scope))
                    {
                        result = false;
                    }
                }

                if (!try_resolve_identifiers(resolver, ast_stmt->for_stmt.cond_expr,
                                             for_scope))
                {
                    result = false;
                }

                if (ast_stmt->for_stmt.it_decl)
                {
                    if (!try_resolve_identifiers(resolver, ast_stmt->for_stmt.it_decl,
                                                 for_scope))
                    {
                        result = false;
                    }
                }

                for (int64_t i = 0; i < ast_stmt->for_stmt.step_statements.count; i++)
                {
                    auto step_stmt = ast_stmt->for_stmt.step_statements[i];
                    if (!try_resolve_identifiers(resolver, step_stmt, for_scope))
                    {
                        result = false;
                    }
                }

                resolver_push_break_node(resolver, ast_stmt);

                if (!try_resolve_identifiers(resolver, ast_stmt->for_stmt.body_stmt,
                                             for_scope))
                {
                    result = false;
                }

                resolver_pop_break_node(resolver);


                break;
            }

            case AST_Statement_Kind::IF:
            {
                result = true;
                if (!try_resolve_identifiers(resolver, ast_stmt->if_stmt.cond_expr, scope))
                {
                    result = false;
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

            case AST_Statement_Kind::SWITCH:
            {
                result = true;

                if (!try_resolve_identifiers(resolver, ast_stmt->switch_stmt.expression, scope))
                {
                    result = false;
                }

                bool case_expr_res = true;
                bool case_body_res = true;

                for (int64_t i = 0; i < ast_stmt->switch_stmt.cases.count; i++)
                {
                    AST_Switch_Case *case_stmt = ast_stmt->switch_stmt.cases[i];

                    if (case_stmt->is_default)
                    {
                        assert(!case_stmt->expressions.count);
                    }
                    else
                    {
                        for (int64_t expr_i = 0; expr_i < case_stmt->expressions.count;
                             expr_i++)
                        {
                            if (!try_resolve_identifiers(resolver,
                                                         case_stmt->expressions[expr_i],
                                                         scope))
                            {
                                case_expr_res = false;
                            }
                        }
                    }

                    resolver_push_break_node(resolver, ast_stmt);

                    if (!try_resolve_identifiers(resolver, case_stmt->body, scope))
                    {
                        case_body_res = false;
                    }

                    resolver_pop_break_node(resolver);
                }

                if (case_expr_res == false || case_body_res == false) result = false;

                break;
            }
        }

        if (result)
        {
            ast_stmt->flags |= AST_NODE_FLAG_RESOLVED_ID;
        }
        return result;
    }

    bool try_resolve_identifiers(Resolver *resolver, AST_Expression *ast_expr, Scope *scope)
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
                result = try_resolve_identifiers(resolver, ast_expr->identifier, scope);
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

            case AST_Expression_Kind::UNARY:
            {
                result = true;
                if (!try_resolve_identifiers(resolver,
                                             ast_expr->unary.operand_expression,
                                             scope))
                {
                    result = false;
                }
                break;
            }

            case AST_Expression_Kind::POST_FIX:
            {
                result = true;
                if (!try_resolve_identifiers(resolver,
                                             ast_expr->post_fix.operand_expression,
                                             scope))
                {
                    result = false;
                }

                break;
            }

            case AST_Expression_Kind::PRE_FIX:
            {
                result = true;
                if (!try_resolve_identifiers(resolver,
                                             ast_expr->pre_fix.operand_expression,
                                             scope))
                {
                    result = false;
                }

                break;
            }

            case AST_Expression_Kind::CALL:
            {
                result = true;
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

            case AST_Expression_Kind::BUILTIN_CALL:
            {
                result = true;

                auto atom = ast_expr->builtin_call.identifier->atom;

                bool check_all_args = true;
                auto &args = ast_expr->builtin_call.arg_expressions;

                if (atom == Builtin::atom_exit) { }
                else if (atom == Builtin::atom_syscall ||
                         atom == Builtin::atom_cast ||
                         atom == Builtin::atom_sizeof)
                {}
                else if (atom == Builtin::atom_offsetof)
                {
                    check_all_args = false;

                    // Arg 0 is the struct member name
                    // Arg 1 is the struct name
                    assert(args.count == 2);

                    auto mem_name = args[0];
                    auto struct_name = args[1];

                    assert(mem_name->kind == AST_Expression_Kind::IDENTIFIER);
                    assert(struct_name->kind == AST_Expression_Kind::IDENTIFIER);

                    if (!try_resolve_identifiers(resolver, struct_name, scope))
                    {
                        assert(false);
                        result = false;
                    }
                    else
                    {
                        auto struct_decl = struct_name->identifier->declaration;
                        assert(struct_decl);
                        assert(struct_decl->kind == AST_Declaration_Kind::STRUCTURE);

                        if (!try_resolve_identifiers(resolver, mem_name,
                                                     struct_decl->structure.member_scope))
                        {
                            result = false;
                        }
                    }
                }
                else 
                {
                    zodiac_report_error(resolver->build_data,
                                        Zodiac_Error_Kind::UNKNOWN_BUILTIN_FUNCTION,
                                        ast_expr, "Unknown builtin function: @%s\n",
                                        atom.data);
                    result = false;
                }

                if (check_all_args)
                {
                    for (int64_t i = 0; i < args.count; i++)
                    {
                        auto arg = args[i];
                        if (!try_resolve_identifiers(resolver, arg, scope))
                        {
                            result = false;
                        }
                    }
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
                result = true;
                if (!try_resolve_identifiers(resolver, ast_expr->subscript.pointer_expression,
                                             scope))
                {
                    result = false;
                }
                if (!try_resolve_identifiers(resolver, ast_expr->subscript.index_expression,
                                             scope))
                {
                    result = false;
                }
                break;
            }

            case AST_Expression_Kind::CAST: assert(false);

            case AST_Expression_Kind::INTEGER_LITERAL: 
            case AST_Expression_Kind::FLOAT_LITERAL: 
            case AST_Expression_Kind::STRING_LITERAL:
            case AST_Expression_Kind::CHAR_LITERAL:
            case AST_Expression_Kind::BOOL_LITERAL:
            case AST_Expression_Kind::NULL_LITERAL:
            {
                result = true;
                break;
            }

            case AST_Expression_Kind::RANGE:
            {
                result = true;

                if (!try_resolve_identifiers(resolver, ast_expr->range.begin, scope))
                {
                    result = false;
                }

                if (!try_resolve_identifiers(resolver, ast_expr->range.end, scope))
                {
                    result = false;
                }

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
                   parent_decl->kind == AST_Declaration_Kind::ENUM ||
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
                               decl->kind == AST_Declaration_Kind::STRUCTURE ||
                               decl->kind == AST_Declaration_Kind::CONSTANT ||
                               decl->kind == AST_Declaration_Kind::TYPEDEF);
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

                if (var_type->kind == AST_Type_Kind::ENUM)
                {
                    auto child_ident = ast_expr->dot.child_identifier;
                    if (!child_ident->declaration)
                    {
                        auto mem_scope = parent_decl->enum_decl.member_scope;
                        auto child_decl = scope_find_declaration(mem_scope, child_ident); 
                        if (!child_decl)
                        {
                            resolver_report_undeclared_identifier(resolver, child_ident);
                            result = false;
                        }
                        else
                        {
                            assert(child_ident->declaration);

                            bool found = false;
                            for (int64_t i = 0;
                                 i < parent_decl->enum_decl.member_declarations.count;
                                 i++)
                            {
                                auto mem_decl =
                                    parent_decl->enum_decl.member_declarations[i];
                                if (mem_decl == child_decl)
                                {
                                    found = true;
                                    break; 
                                }
                            }
                            assert(found);
                        }
                    }

                    ast_expr->dot.child_decl = child_ident->declaration;
                    result = true;
                }
                else if (var_type->kind == AST_Type_Kind::ARRAY)
                {
                    auto child_ident = ast_expr->dot.child_identifier;
                    if (child_ident->atom == Builtin::atom_count)
                    {
                        result = true;
                        ast_expr->expr_flags |= AST_EXPR_FLAG_DOT_COUNT;
                    }
                    else
                    {
                        assert(false);
                    }
                }
                else
                {
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
                        ast_expr->dot.child_decl = child_decl;
                    }

                    result = true;
                }
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
                       decl->kind == AST_Declaration_Kind::ENUM ||
                       decl->kind == AST_Declaration_Kind::TYPE ||
                       decl->kind == AST_Declaration_Kind::TYPEDEF);

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
                if (!try_resolve_identifiers_dot_expr(resolver, ast_ts->dot_expression,
                                                      scope))
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
                                                      
            case AST_Type_Spec_Kind::FROM_TYPE: assert(false);
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
                }
                else
                {
                    result = true;
                }

                if (result)
                    assert(decl->type ||
                           decl->kind == AST_Declaration_Kind::IMPORT ||
                           decl->kind == AST_Declaration_Kind::USING ||
                           decl->kind == AST_Declaration_Kind::STATIC_IF ||
                           decl->kind == AST_Declaration_Kind::STATIC_ASSERT);
                break;
            }

            case AST_Node_Kind::EXPRESSION:
            {
                auto expr = static_cast<AST_Expression*>(ast_node);
                if (ast_node->flags & AST_NODE_FLAG_TYPED)
                {
                    assert(expr->type);
                    return true;
                }
                result = try_resolve_types(resolver, expr, scope);
                break;
            }

            case AST_Node_Kind::SWITCH_CASE: assert(false);

            case AST_Node_Kind::STATEMENT:
            {
                assert(!(ast_node->flags & AST_NODE_FLAG_TYPED));  
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

    bool try_resolve_types(Resolver *resolver, AST_Identifier *identifier, AST_Type **type_dest,
                           Scope *scope)
    {
        if (type_dest) assert(*type_dest == nullptr);

        bool result = true;

        auto ident = identifier;
        assert(ident->declaration);
        if (ident->declaration->flags & AST_NODE_FLAG_TYPED)
        {
            if (!ident->declaration->type)
            {
                assert(ident->declaration->kind == AST_Declaration_Kind::IMPORT);
            }
            else
            {
                *type_dest = ident->declaration->type;
            }
        }
        else
        {
            result = false;
        }

        if (result)
        {
            identifier->flags |= AST_NODE_FLAG_TYPED;
        }

        return result;
    }

    bool try_resolve_types(Resolver *resolver, AST_Declaration *ast_decl, Scope *scope)
    {
        assert(resolver);
        assert(ast_decl);
        assert(scope);

        assert(ast_decl->flags & AST_NODE_FLAG_RESOLVED_ID);

        if (ast_decl->flags & AST_NODE_FLAG_TYPED)
        {
            assert(ast_decl->type != nullptr ||
                   ast_decl->kind == AST_Declaration_Kind::IMPORT ||
                   ast_decl->kind == AST_Declaration_Kind::STATIC_ASSERT ||
                   ast_decl->kind == AST_Declaration_Kind::STATIC_IF);
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

                        if (ast_decl->decl_flags & AST_DECL_FLAG_GLOBAL)
                        {
                            queue_emit_bytecode_job(resolver, ast_decl, scope);
                        }

                        if (init_expr && (ast_decl->decl_flags & AST_DECL_FLAG_GLOBAL))
                        {
                            assert(init_expr->expr_flags & AST_EXPR_FLAG_CONST);
                        }

                        ast_decl->type = var_type;
                        ast_decl->flags |= AST_NODE_FLAG_TYPED;
                    }
                }
                break;
            }

            case AST_Declaration_Kind::CONSTANT:
            {
                AST_Type *ts_type = nullptr;
                auto init_expr = ast_decl->constant.init_expression;

                if (ast_decl->constant.type_spec)
                {
                    result = try_resolve_types(resolver, ast_decl->constant.type_spec, scope, 
                                               &ts_type, nullptr);
                }

                if (result)
                {
                    result = try_resolve_types(resolver, init_expr, ts_type, scope);
                }

                if (result)
                {
                    if (ts_type && (ts_type != init_expr->type))
                    {
                        resolver_report_mismatching_types(resolver, init_expr, ts_type,
                                                          init_expr->type);
                        result = false;
                    }
                    else
                    {
                        assert(init_expr->type);
                        ast_decl->type = init_expr->type;
                        ast_decl->flags |= AST_NODE_FLAG_TYPED;
                    }
                }
                break;
            }

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
                if (!inferred_return_type) inferred_return_type = Builtin::type_void;
                
                if (!ast_decl->function.type_spec->type) 
                {
                    if (!ast_decl->function.type_spec->function.return_type_spec)
                    {
                        auto new_ts =
                            ast_type_spec_from_type_new(resolver->allocator, inferred_return_type);

                        ast_decl->function.type_spec->function.return_type_spec = new_ts;
                    }

                    if (!try_resolve_types(resolver, ast_decl->function.type_spec, scope,
                                           &ast_decl->type, inferred_return_type))
                    {
                        result = false;
                        break;
                    }
                }
                else
                {
                    // assert(ast_decl->type ||
                    //        (ast_decl->function.type_spec && ));
                    AST_Type * func_type = ast_decl->type;
                    if (!func_type)
                    {
                        assert(ast_decl->function.type_spec);
                        assert(ast_decl->function.type_spec->type);

                        func_type = ast_decl->function.type_spec->type;
                        ast_decl->type = func_type;
                    }

                    assert(func_type->kind == AST_Type_Kind::FUNCTION);

                    auto ret_type = func_type->function.return_type;

                    if (body && !(ast_decl->decl_flags & AST_DECL_FLAG_NORETURN))
                    {
                        if (!(ret_type == inferred_return_type ||
                               (ret_type == Builtin::type_void &&
                                inferred_return_type == nullptr)))
                        {
                            AST_Type *ac_type = inferred_return_type;
                            if (!ac_type) ac_type = Builtin::type_void;
                            resolver_report_mismatching_types(resolver, ast_decl,
                                                              ret_type, ac_type);
                            result = false;
                        }
                    }
                }

                if (result)
                {
                    if (ast_decl->decl_flags & AST_DECL_FLAG_NORETURN)
                    {
                        assert(ast_decl->type->function.return_type ==
                                Builtin::type_void);
                    }
                    assert(ast_decl->type);
                    assert(ast_decl->function.type_spec->type);
                    if (body) assert(body->flags & AST_NODE_FLAG_TYPED);
                    ast_decl->flags |= AST_NODE_FLAG_TYPED;

                    if (resolver_is_active_static_branch(resolver))
                    {
                        if (!(ast_decl->decl_flags & AST_DECL_FLAG_REGISTERED_BYTECODE))
                        {
                            auto bc_func =
                                bytecode_register_function(&resolver->bytecode_builder, ast_decl);

                            llvm_register_function(&resolver->llvm_builder, bc_func, ast_decl->type);
                        }
                        queue_emit_bytecode_job(resolver, ast_decl, scope);
                    }
                }

                break;
            }

            case AST_Declaration_Kind::TYPE: assert(false);

            case AST_Declaration_Kind::TYPEDEF:
            {
                result = true;
                AST_Type *result_type = nullptr;
                if (try_resolve_types(resolver, ast_decl->typedef_decl.type_spec,
                                      scope, &result_type, nullptr))
                {
                    assert(result_type);
                    ast_decl->type = result_type;
                    ast_decl->flags |= AST_NODE_FLAG_TYPED;
                }
                else 
                {
                    result = false;
                }
                break;
            }

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
                                                     mem_decl,
                                                     ast_decl->structure.member_scope);
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
                    assert(ast_decl->identifier);
                    ast_decl->type =
                        create_structure_type(resolver, ast_decl, member_types,
                                               ast_decl->structure.member_scope, scope);
                    ast_decl->flags |= AST_NODE_FLAG_TYPED;
                }
                else
                {
                    array_free(&member_types);
                }
                break;
            }

            case AST_Declaration_Kind::ENUM: 
            {
                AST_Type *base_type = nullptr;

                result = true;

                if (ast_decl->enum_decl.type_spec)
                {
                    if (!try_resolve_types(resolver, ast_decl->enum_decl.type_spec,
                                           scope, &base_type, nullptr))
                    {
                        result = false;
                        break;
                    }
                }
                else
                {
                    base_type = Builtin::type_u64;
                }

                assert(base_type->kind == AST_Type_Kind::INTEGER);

                auto enum_type = find_or_create_enum_type(resolver, ast_decl, base_type, 
                                                          ast_decl->enum_decl.member_scope,
                                                          scope);

                auto mem_decls = ast_decl->enum_decl.member_declarations;
                for (int64_t i = 0; i < mem_decls.count; i++)
                {
                    auto mem_decl = mem_decls[i];
                    assert(mem_decl->kind == AST_Declaration_Kind::CONSTANT);
                    if (mem_decl->constant.type_spec == nullptr)
                    {
                        mem_decl->constant.type_spec =
                            ast_type_spec_from_type_new(resolver->allocator, enum_type);
                    }

                    bool mem_res = try_resolve_types(resolver, mem_decl,
                                                     ast_decl->enum_decl.member_scope);
                    if (!mem_res) result = false;
                    else 
                    {
                        assert(mem_decl->type == enum_type);
                    }
                }

                if (result)
                {
                    result = resolver_assign_enum_initializers(resolver, ast_decl,
                                                               enum_type);
                    assert(result);
                }

                if (result)
                {
                   ast_decl->flags |= AST_NODE_FLAG_TYPED;
                   ast_decl->type = enum_type;
                } 
                break;
            }

            case AST_Declaration_Kind::POLY_TYPE: assert(false);
                                                  
            case AST_Declaration_Kind::STATIC_IF:
            {
                result = true;

                auto cond_expr = ast_decl->static_if.cond_expression;

                if (!try_resolve_types(resolver, cond_expr, scope))
                {
                    result = false;
                    break;
                }

                assert(cond_expr->type == Builtin::type_bool);
                assert(cond_expr->expr_flags & AST_EXPR_FLAG_CONST);

                Const_Value cond_val = const_interpret_expression(cond_expr);
                bool import_then = cond_val.boolean;

                auto then_scope = ast_decl->static_if.then_scope;
                auto else_scope = ast_decl->static_if.else_scope;

                resolver_push_active_static_branch(resolver, import_then);

                for (int64_t i = 0; i < ast_decl->static_if.then_declarations.count; i++)
                {
                    auto decl = ast_decl->static_if.then_declarations[i];

                    if (!try_resolve_types(resolver, decl, then_scope))
                    {
                        result = false;
                    }
                    else if (import_then && !resolver_import_from_static_if(resolver, decl, scope))
                    {
                        result = false;
                    }
                }
                
                resolver_pop_active_static_branch(resolver);
                resolver_push_active_static_branch(resolver, !import_then);

                for (int64_t i = 0; i < ast_decl->static_if.else_declarations.count; i++)
                {
                    auto decl = ast_decl->static_if.else_declarations[i];

                    if (!try_resolve_types(resolver, decl, else_scope))
                    {
                        result = false;
                    }
                    else if (!import_then && !resolver_import_from_static_if(resolver, decl, scope))
                    {
                        result = false;
                    }
                }

                resolver_pop_active_static_branch(resolver);

                if (result)
                {
                    ast_decl->flags |= AST_NODE_FLAG_TYPED;
                }

                break;
            }

            case AST_Declaration_Kind::STATIC_ASSERT:
            {
                auto cond_expr = ast_decl->static_assert_decl.cond_expression;
                if (!try_resolve_types(resolver, cond_expr, scope))
                {
                    result = false;
                }
                else 
                {
                    assert(cond_expr->type->kind == AST_Type_Kind::BOOL);
                    assert(cond_expr->expr_flags & AST_EXPR_FLAG_CONST);
                    
                    auto c_res = const_interpret_expression(cond_expr);

                    if (!c_res.boolean && resolver_is_active_static_branch(resolver))
                    {
                        result = false;
                        zodiac_report_error(resolver->build_data, 
                                            Zodiac_Error_Kind::STATIC_ASSERTION_FAILED,
                                            ast_decl, "Static assertion failed!");
                    }
                    else
                    {
                        ast_decl->flags |= AST_NODE_FLAG_TYPED;
                    }

                }
                break;
            }
        }

        if (result)
        {
            assert(ast_decl->flags & AST_NODE_FLAG_TYPED);
            assert(ast_decl->type ||
                   ast_decl->kind == AST_Declaration_Kind::IMPORT ||
                   ast_decl->kind == AST_Declaration_Kind::USING ||
                   ast_decl->kind == AST_Declaration_Kind::STATIC_IF ||
                   ast_decl->kind == AST_Declaration_Kind::STATIC_ASSERT);
        }

        return result;
    }

    bool try_resolve_types(Resolver *resolver, AST_Statement *ast_stmt, Scope *scope,
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
                    assert(ast_stmt->block.scope);
                    assert(scope == ast_stmt->block.scope->parent);
                }

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
                AST_Type *return_type = nullptr;
                if (ast_stmt->expression)
                {
                    result = try_resolve_types(resolver, ast_stmt->expression, scope);
                    if (result && (ast_stmt->expression->flags & AST_NODE_FLAG_TYPED))
                    {
                        assert(ast_stmt->expression->type);
                        ast_stmt->flags |= AST_NODE_FLAG_TYPED;
                        return_type = ast_stmt->expression->type;
                    }
                } 
                else
                {
                    result = true;
                    return_type = Builtin::type_void;
                }

                if (result)
                {
                    assert(inferred_return_type);
                    if (*inferred_return_type)
                    {
                        assert(*inferred_return_type == return_type);
                    }
                    else
                    {
                        *inferred_return_type = return_type;
                    }

                    ast_stmt->flags |= AST_NODE_FLAG_TYPED;
                }
                break;
            }

            case AST_Statement_Kind::BREAK:
            {
                assert(stack_count(&resolver->break_node_stack));
                result = true;
                ast_stmt->flags |= AST_NODE_FLAG_TYPED;
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

                resolver_push_break_node(resolver, ast_stmt);

                if (!try_resolve_types(resolver, ast_stmt->while_stmt.body, scope,
                                       inferred_return_type))
                {
                    result = false;
                }

                resolver_pop_break_node(resolver);

                if (result) ast_stmt->flags |= AST_NODE_FLAG_TYPED; 
                break;
            }

            
            case AST_Statement_Kind::FOR: 
            {
                auto for_scope = ast_stmt->for_stmt.scope;

                result = true;

                for (int64_t i = 0; i < ast_stmt->for_stmt.init_statements.count; i++)
                {
                    auto  init_stmt = ast_stmt->for_stmt.init_statements[i];
                    if (!try_resolve_types(resolver, init_stmt, for_scope,
                                           inferred_return_type))
                    {
                        result = false;
                    }
                }

                if (!try_resolve_types(resolver, ast_stmt->for_stmt.cond_expr, for_scope))
                {
                    result = false;
                }
                else
                {
                    if (ast_stmt->for_stmt.it_decl)
                    {
                        if (!try_resolve_types(resolver, ast_stmt->for_stmt.it_decl,
                                               for_scope))
                        {
                            result = false;
                        }
                    }

#ifndef NDEBUG
                    auto cond_type = ast_stmt->for_stmt.cond_expr->type;
                    assert(cond_type);
                    assert(cond_type->kind == AST_Type_Kind::BOOL);
#endif
                }

                for (int64_t i = 0; i < ast_stmt->for_stmt.step_statements.count; i++)
                {
                    auto step_stmt = ast_stmt->for_stmt.step_statements[i];
                    if (!try_resolve_types(resolver, step_stmt, for_scope,
                                           inferred_return_type))
                    {
                        result = false;
                    }
                }

                resolver_push_break_node(resolver, ast_stmt);

                if (!try_resolve_types(resolver, ast_stmt->for_stmt.body_stmt, for_scope,
                                       inferred_return_type))
                {
                    result = false;
                }

                resolver_pop_break_node(resolver);

                if (result)
                {
                    ast_stmt->flags |= AST_NODE_FLAG_TYPED;
                }
                break;
            }

            case AST_Statement_Kind::IF:
            {
                result = true;
                if (!try_resolve_types(resolver, ast_stmt->if_stmt.cond_expr, scope))
                {
                    result = false;
                }

                if (!try_resolve_types(resolver, ast_stmt->if_stmt.then_stmt, scope,
                                       inferred_return_type))
                {
                    result = false;
                }

                if (ast_stmt->if_stmt.else_stmt)
                {
                    if (!try_resolve_types(resolver, ast_stmt->if_stmt.else_stmt, scope,
                                           inferred_return_type))
                    {
                        result = false;
                    }
                }

                if (result) ast_stmt->flags |= AST_NODE_FLAG_TYPED;
                break;
            }

            case AST_Statement_Kind::SWITCH:
            {
                result = true;

                bool expr_res = try_resolve_types(resolver, ast_stmt->switch_stmt.expression,
                                                  scope);
                AST_Type *expr_type = nullptr;
                if (expr_res)
                {
                    expr_type = ast_stmt->switch_stmt.expression->type;
                    assert(expr_type->kind == AST_Type_Kind::INTEGER ||
                           expr_type->kind == AST_Type_Kind::ENUM);
                }

                bool case_expr_result = true;
                bool case_body_result = true;

                //@TODO: Switch cases should be statement nodes, or have node flags,
                //        so we don't have to re-resolve them on each cycle.
                for (int64_t i = 0; i < ast_stmt->switch_stmt.cases.count; i++)
                {
                    AST_Switch_Case *switch_case = ast_stmt->switch_stmt.cases[i];

                    uint64_t range_count = 0;

                    // Only resolve the case expr type when we have determined the
                    //  switch expr type.
                    if (expr_res && !switch_case->is_default)
                    {
                        assert(expr_type);
                        for (int64_t expr_i = 0; expr_i < switch_case->expressions.count;
                             expr_i++)
                        {
                            auto case_expr = switch_case->expressions[expr_i];
                            if (!try_resolve_types(resolver, case_expr, expr_type, scope))
                            {
                                case_expr_result = false;
                            }
                            else
                            {
                                assert(case_expr->type == expr_type);
                                assert(case_expr->expr_flags & AST_EXPR_FLAG_CONST);

                                if (case_expr->kind == AST_Expression_Kind::RANGE)
                                {
                                    range_count += 1;
                                }
                            }
                        }

                        if (case_expr_result && range_count)
                        {
                            resolver_expand_switch_case_ranges(resolver, ast_stmt,
                                                               switch_case, range_count,
                                                               scope);
                        }
                    }

                    resolver_push_break_node(resolver, ast_stmt);

                    if (!try_resolve_types(resolver, switch_case->body, scope,
                                           inferred_return_type))
                    {
                        case_body_result = false;
                    }

                    resolver_pop_break_node(resolver);
                }

                result = expr_res && case_expr_result && case_body_result;

                if (result)
                {
                    if(resolver_switch_case_expressions_are_typed(ast_stmt))
                    {
                        if (expr_type->kind == AST_Type_Kind::ENUM &&
                            !ast_stmt->switch_stmt.allow_incomplete)
                        {
                            result = resolver_check_switch_completeness(resolver,
                                                                        ast_stmt);
                        }
                    }
                    else
                    {
                        result = false;
                    }
                }

                if (result)
                {
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

    bool try_resolve_types(Resolver *resolver, AST_Expression *ast_expr, Scope *scope)
    {
        return try_resolve_types(resolver, ast_expr, nullptr, scope);
    }

    bool try_resolve_types(Resolver *resolver, AST_Expression *ast_expr,
                           AST_Type *suggested_type, Scope *scope)
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
                AST_Type *result_type = nullptr;
                result = try_resolve_types(resolver, ast_expr->identifier, &result_type,
                                           scope);
                if (result) ast_expr->type = result_type;
                break;
            }

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);

            case AST_Expression_Kind::DOT:
            {
                auto parent_expr = ast_expr->dot.parent_expression;
                bool parent_res = try_resolve_types(resolver, parent_expr, scope);

                if (!parent_res)
                {
                    result = false;
                    break;
                }

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
                    else if (child_decl->kind == AST_Declaration_Kind::CONSTANT)
                    {
                        assert(child_decl->type);
                        ast_expr->type = child_decl->type; 
                        result = true;
                    }
                    else if (child_decl->kind == AST_Declaration_Kind::TYPEDEF)
                    {
                        if (child_decl->type)
                        {
                            ast_expr->type = child_decl->type;
                            result = true;
                        }
                        else
                        {
                            result = false;
                        }
                    }
                    else
                    {
                        assert(false);
                    }
                }
                else
                {
                    auto parent_type = parent_expr->type;
                    assert(parent_type);
                    if (parent_type->kind == AST_Type_Kind::ENUM)
                    {
                        auto member_ident = ast_expr->dot.child_identifier;
                        assert(member_ident);
                        if (member_ident->declaration)
                        {
#ifndef NDEBUG
                            auto mem_decl = member_ident->declaration;
                            assert(mem_decl->kind == AST_Declaration_Kind::CONSTANT);
                            assert(mem_decl->type);
#endif

                            ast_expr->type = parent_type;
                            result = true;
                        }
                        else
                        {
                            result = false;
                        }
                    }
                    else if (parent_type->kind == AST_Type_Kind::ARRAY)
                    {
                        if (ast_expr->expr_flags & AST_EXPR_FLAG_DOT_COUNT)
                        {
                            result = true;
                            ast_expr->type = Builtin::type_s64;
                        }
                        else
                        {
                            assert(false);
                        }
                    }
                    else
                    {
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

                if (binop_is_cmp(ast_expr->binary.op))
                {
                    result_type = Builtin::type_bool;
                }

                assert(result_type);
                result = true;
                ast_expr->type = result_type;
                break;
            }

            case AST_Expression_Kind::UNARY:
            {
                result = true;

                auto op_expr = ast_expr->unary.operand_expression;
                if (try_resolve_types(resolver, op_expr, scope))
                {
                    if (ast_expr->unary.op == UNOP_MINUS)
                    {
                        assert(op_expr->type->kind == AST_Type_Kind::INTEGER);
                        assert(op_expr->type->integer.sign);
                        ast_expr->type = op_expr->type;
                    }
                    else if (ast_expr->unary.op == UNOP_DEREF)
                    {
                        if (op_expr->type->kind != AST_Type_Kind::POINTER)
                        {
                            zodiac_report_error(resolver->build_data,
                                                Zodiac_Error_Kind::INVALID_DEREF,
                                                ast_expr,
                                                "Cannot dereference an expression of non pointer type.");
                            result = false;
                        }
                        else
                        {
                            ast_expr->type = op_expr->type->pointer.base;
                        }
                    }
                    else
                    {
                        assert(false);
                    }
                }
                else
                {
                    result = false;
                }
                break;
            }

            case AST_Expression_Kind::POST_FIX:
            {
                result = true;

                auto operand_expr = ast_expr->post_fix.operand_expression;

                if (try_resolve_types(resolver, operand_expr, scope))
                {
                    assert(operand_expr->type);
                    assert(operand_expr->type->kind == AST_Type_Kind::INTEGER);
                    ast_expr->type = operand_expr->type;
                }
                else
                {
                    result = false;
                }
                break;
            }

            case AST_Expression_Kind::PRE_FIX:
            {
                result = true;

                auto operand_expr = ast_expr->pre_fix.operand_expression;

                if (try_resolve_types(resolver, operand_expr, scope))
                {
                    assert(operand_expr->type);
                    assert(operand_expr->type->kind == AST_Type_Kind::INTEGER);
                    ast_expr->type = operand_expr->type;
                }
                else
                {
                    result = false;
                }
                break;
            }

            case AST_Expression_Kind::CALL:
            {
                auto decl = resolver_get_declaration(ast_expr->call.ident_expression);
                assert(decl);
                assert(decl->kind == AST_Declaration_Kind::FUNCTION);
                auto ts = decl->function.type_spec;

                AST_Type *func_type = nullptr;

                if (!(decl->flags & AST_NODE_FLAG_RESOLVED_ID))
                {
                    result = false;
                    break;
                }

                if (!(ts->flags & AST_NODE_FLAG_TYPED))
                {
                    if (!try_resolve_types(resolver, ts, scope, &func_type, nullptr))
                    {
                        if (!(decl->flags & AST_NODE_FLAG_RESOLVED_ID))
                        {
                            return false;
                        }

                        if (!try_resolve_types(resolver, decl, scope))
                        {
                            return false;
                        }

                        assert(decl->type);
                        func_type = decl->type;
                    }
                }
                else
                {
                    assert(ts->type);
                    func_type = ts->type;
                }

                assert(func_type->kind == AST_Type_Kind::FUNCTION);

                if (resolver_is_active_static_branch(resolver))
                {
                    if (!(decl->decl_flags & AST_DECL_FLAG_REGISTERED_BYTECODE))
                    {
                        auto bc_func = bytecode_register_function(&resolver->bytecode_builder, decl);
                        llvm_register_function(&resolver->llvm_builder, bc_func, func_type);
                    }

                    if (!(decl->flags & AST_NODE_FLAG_QUEUED_BYTECODE_EMISSION))
                    {
                        queue_emit_bytecode_job(resolver, decl, scope);
                    }
                }

                bool arg_res = true;

                assert(ast_expr->call.arg_expressions.count ==
                       func_type->function.param_types.count);

                for (int64_t i = 0; i < ast_expr->call.arg_expressions.count; i++)
                {
                    auto arg_expr = ast_expr->call.arg_expressions[i];
                    auto param_type = func_type->function.param_types[i];

                    if (!try_resolve_types(resolver, arg_expr, param_type, scope))
                    {
                        arg_res = false;
                    }
                    else
                    {
                        assert(param_type);
                        if (arg_res &&
                            !(arg_expr->type == param_type))
                        {
                            if (resolver_valid_type_conversion(arg_expr->type,
                                                               param_type))
                            {
                                auto new_arg_expr =
                                    ast_cast_expression_new(resolver->allocator,
                                                            arg_expr,
                                                            param_type,
                                                            arg_expr->begin_file_pos,
                                                            arg_expr->end_file_pos);

                                new_arg_expr->flags |= AST_NODE_FLAG_RESOLVED_ID;
                                ast_expr->call.arg_expressions[i] = new_arg_expr;

                                if (!try_resolve_types(resolver, new_arg_expr, scope))
                                {
                                    assert(false); 
                                }
                            }
                            else 
                            {
                                resolver_report_mismatching_types(resolver, arg_expr,
                                                                 param_type, arg_expr->type);
                                arg_res = false;
                                result = false;
                            }
                        }
                        else if (!arg_res) result = false;
                    }
                }


                if (arg_res)
                {
                    ast_expr->type = func_type->function.return_type;
                    result = true;
                }

                if (result)
                {
                    assert(ast_expr->type);
                }
                
                break;
            }

            case AST_Expression_Kind::BUILTIN_CALL:
            {
                result = try_resolve_builtin_call_types(resolver, ast_expr, scope);
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

#ifndef NDEBUG
                auto index_type = ast_expr->subscript.index_expression->type;
                assert(index_type->kind == AST_Type_Kind::INTEGER);
#endif

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
                        if (operand->type->kind == AST_Type_Kind::INTEGER)
                        {
                            result = true;
                            result_type = target_type;
                        }
                        else if (operand->type->kind == AST_Type_Kind::ENUM)
                        {
                            result = true;
                            assert(resolver_valid_type_conversion(
                                        operand->type->enum_type.base_type, target_type));
                            result_type = target_type;
                        }
                    }
                    else if (target_type->kind == AST_Type_Kind::FLOAT)
                    {
                        assert(operand->type->kind == AST_Type_Kind::FLOAT ||
                               operand->type->kind == AST_Type_Kind::INTEGER);
                        result = true;
                        result_type = target_type;
                    }
                    else if (target_type->kind == AST_Type_Kind::BOOL)
                    {
                        assert(operand->type->kind == AST_Type_Kind::INTEGER);
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
                           suggested_type->kind == AST_Type_Kind::ENUM ||
                           suggested_type->kind == AST_Type_Kind::FLOAT);
                    if (resolver_literal_fits_in_type(ast_expr->integer_literal,
                                                      suggested_type))
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

            case AST_Expression_Kind::NULL_LITERAL:
            {
                assert(suggested_type);
                assert(suggested_type->kind == AST_Type_Kind::POINTER);

                ast_expr->type = suggested_type;
                result = true;
                break;
            }

            case AST_Expression_Kind::RANGE:
            {
                result = true;

                if (!try_resolve_types(resolver, ast_expr->range.begin, scope))
                {
                    result = false;
                }

                if (!try_resolve_types(resolver, ast_expr->range.end, scope))
                {
                    result = false;
                }

                if (result)
                {
                    assert(ast_expr->range.begin->type == ast_expr->range.end->type);
                    ast_expr->type = ast_expr->range.begin->type;
                }

                break;
            }
        }

        if (result)
        {
            assert(ast_expr->type ||
                   (ast_expr->kind == AST_Expression_Kind::IDENTIFIER &&
                    ast_expr->identifier->declaration->kind == AST_Declaration_Kind::IMPORT));
            resolver_inherit_const(ast_expr);
            ast_expr->flags |= AST_NODE_FLAG_TYPED;
        }

        return result;
    }

    bool try_resolve_builtin_call_types(Resolver *resolver, AST_Expression *call_expr,
                                        Scope *scope)
    {
        assert(resolver);
        assert(call_expr);
        assert(call_expr->kind == AST_Expression_Kind::BUILTIN_CALL);
        assert(scope);

        auto ident_atom = call_expr->builtin_call.identifier->atom;

        auto &args = call_expr->builtin_call.arg_expressions;

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
// #ifndef linux
//             zodiac_report_error(resolver, Zodiac_Error_Kind::UNSUPPORTED,
//                                   call_expr, "Syscall is only supported on linux");
//             return false;
// #endif

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
        else if (ident_atom == Builtin::atom_sizeof)
        {
            bool result = true;

            assert(args.count == 1);

            auto type_expr = args[0];

            if (!try_resolve_types(resolver, type_expr, scope))
            {
                result = false;
            }
            else 
            {
                assert(type_expr->type);

                call_expr->type = Builtin::type_s64;
            }


            return result;
        }
        else if (ident_atom == Builtin::atom_offsetof)
        {
            bool result = true;

            assert(args.count == 2);

            auto mem_name = args[0];
            auto struct_name = args[1];

            if (!try_resolve_types(resolver, struct_name, scope))
            {
                result = false;
            }
            else
            {
                auto struct_decl = struct_name->identifier->declaration;
                assert(struct_decl);

                if (!scope_find_declaration(struct_decl->structure.member_scope, 
                                            mem_name->identifier))
                {
                    result = false;
                }
            }

            if (result)
            {
                call_expr->type = Builtin::type_s64;
            }

            return result;
        }
        else
        {
            zodiac_report_error(resolver->build_data, Zodiac_Error_Kind::UNIMPLEMENTED,
                                call_expr, "Unimplemented builtin function: @%s",
                                ident_atom.data);
            return false;
        }

        assert(false);
        return false;
    }

    bool try_resolve_types(Resolver *resolver, AST_Type_Spec *ts, Scope *scope,
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
            if (ts->kind == AST_Type_Spec_Kind::FROM_TYPE)
            {
                ts->flags |= AST_NODE_FLAG_TYPED;
            }
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
                       decl->kind == AST_Declaration_Kind::ENUM ||
                       decl->kind == AST_Declaration_Kind::TYPE ||
                       decl->kind == AST_Declaration_Kind::TYPEDEF);

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
                result = try_resolve_types(resolver, ts->base_type_spec, scope,
                                           &base_type, nullptr);
                if (result)
                {
                    assert(base_type);
                    *type_target =
                        build_data_find_or_create_pointer_type(resolver->allocator,
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
                if (!ts->function.return_type_spec) return false;

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
                            array_free(&param_types);
                            return false;
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
                    assert(ts->function.from_declaration);
                    if (suggested_type && 
                        !(ts->function.from_declaration->decl_flags & AST_DECL_FLAG_FOREIGN))
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

                    assert(ts->function.from_declaration);

                    if (resolver_is_active_static_branch(resolver))
                    {
                        auto bc_func = bytecode_register_function(&resolver->bytecode_builder,
                                                                  ts->function.from_declaration);

                        llvm_register_function(&resolver->llvm_builder, bc_func, func_type);
                    }
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
                        result = false;
                        break;
                    }

                    assert(length_expr->type);
                    assert(length_expr->type->kind == AST_Type_Kind::INTEGER);

                    assert(length_expr->expr_flags & AST_EXPR_FLAG_CONST);

                    Const_Value length_val = const_interpret_expression(length_expr);
                    assert(length_val.type == length_expr->type);
                    assert(length_val.integer.s64 > 0);

                    length = length_val.integer.s64; 
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
                                                      
            case AST_Type_Spec_Kind::FROM_TYPE: assert(false);
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
                            result = try_resolve_sizes(resolver,
                                                       decl->variable.init_expression,
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
                    case AST_Declaration_Kind::TYPEDEF: assert(false);

                    case AST_Declaration_Kind::STRUCTURE: assert(false);
                    case AST_Declaration_Kind::POLY_TYPE: assert(false);

                    case AST_Declaration_Kind::ENUM: assert(false);
                                                     
                    case AST_Declaration_Kind::STATIC_IF: assert(false);

                    case AST_Declaration_Kind::STATIC_ASSERT: assert(false);
                }
                break;
            }

            case AST_Node_Kind::SWITCH_CASE: assert(false);

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

            case AST_Type_Kind::ENUM: 
            {
                assert(ast_type->enum_type.base_type->flags & AST_NODE_FLAG_SIZED);
                ast_type->bit_size = ast_type->enum_type.base_type->bit_size;
                ast_type->flags |= AST_NODE_FLAG_SIZED;
                break;
            }

            case AST_Type_Kind::ARRAY:
            {
                auto elem_size = ast_type->array.element_type->bit_size;
                assert(elem_size);
                auto bit_size = elem_size  *ast_type->array.element_count;
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

    void resolver_expand_switch_case_ranges(Resolver *resolver,
                                            AST_Statement *stmt,
                                            AST_Switch_Case *switch_case,
                                            uint64_t range_count,
                                            Scope *switch_scope)
    {
        Array<AST_Expression*> temp_case_exprs = {};
        auto ta = temp_allocator_get();
        array_init(ta, &temp_case_exprs, range_count * 3);

        int64_t first_range_index = -1;

        for (int64_t expr_i = 0;
             expr_i < switch_case->expressions.count;
             expr_i++)
        {
            auto case_expr = switch_case->expressions[expr_i];
            if (case_expr->kind == AST_Expression_Kind::RANGE)
            {
                if (first_range_index == -1)
                {
                    first_range_index = expr_i;
                }

                auto begin_expr = case_expr->range.begin;
                auto end_expr = case_expr->range.end;

                array_append(&temp_case_exprs, begin_expr);

                Const_Value begin_val =
                    const_interpret_expression(begin_expr);

                Const_Value end_val =
                    const_interpret_expression(end_expr);

                assert(begin_val.type == end_val.type);
                assert(begin_val.type->kind == AST_Type_Kind::INTEGER ||
                       begin_val.type->kind == AST_Type_Kind::ENUM);
                assert(begin_val.type->integer.sign == false);

                assert(begin_val.integer.u64 < end_val.integer.u64);

                File_Pos begin_fp = begin_expr->begin_file_pos;
                begin_fp.file_name =
                    string_append(resolver->allocator,
                                  string_ref("<expanded from range expression> "),
                                  begin_expr->begin_file_pos.file_name);

                auto end_fp = begin_fp;

                uint64_t val = begin_val.integer.u64 + 1;
                while (val < end_val.integer.u64)
                {
                    AST_Expression *new_expr =
                        ast_integer_literal_expression_new(resolver->allocator,
                                                           val, begin_fp, end_fp);

                    if (begin_expr->type->kind == AST_Type_Kind::ENUM)
                    {
                        auto enum_type = begin_expr->type;

                        auto enum_name =
                            enum_type->enum_type.declaration->identifier->atom;

                        auto parent_ident = ast_identifier_new(resolver->allocator, 
                                                               enum_name,
                                                               begin_fp, end_fp);

                        auto parent_expr =
                            ast_identifier_expression_new(resolver->allocator,
                                                          parent_ident,
                                                          begin_fp, end_fp);;

                        auto member_decl = ast_find_enum_member(enum_type,
                                                                { .type = enum_type,
                                                                  .integer={.u64=val}});
                        assert(member_decl);

                        auto child_name = member_decl->identifier->atom;
                        AST_Identifier *child_ident =
                            ast_identifier_new(resolver->allocator, child_name,
                                               begin_fp, end_fp);

                        new_expr = ast_dot_expression_new(resolver->allocator,
                                                          parent_expr, child_ident,
                                                          begin_fp, end_fp);

                    }
                    
                    queue_ident_job(resolver, new_expr, switch_scope,
                                    resolver_is_active_static_branch(resolver));
                    array_append(&temp_case_exprs, new_expr);

                    val += 1;
                    stmt->switch_stmt.case_expr_count += 1;
                }

                array_append(&temp_case_exprs, end_expr);
                stmt->switch_stmt.case_expr_count += 1;

                //@FIXME:@LEAK: We are leaking 'case_expr' here, right now we can't be
                //               sure about the allocator that was used to allocate the 
                //               expression. (in practice we are only using the c
                //               allocator at the moment, but if we change any of them we
                //               won't know)

            }
            else if (first_range_index >= 0)
            {
                array_append(&temp_case_exprs, case_expr);
            }
        }

        assert(first_range_index >= 0);

        switch_case->expressions.count = first_range_index;

        for (int64_t j = 0; j < temp_case_exprs.count; j++)
        {
            array_append(&switch_case->expressions,
                         temp_case_exprs[j]);
        }

    }

    bool resolver_switch_case_expressions_are_typed(AST_Statement *ast_stmt)
    {
        assert(ast_stmt->kind == AST_Statement_Kind::SWITCH);

        for (int64_t i = 0; i < ast_stmt->switch_stmt.cases.count; i++)
        {
            auto switch_case = ast_stmt->switch_stmt.cases[i];
            for (int64_t j = 0; j < switch_case->expressions.count; j++)
            {
                auto case_expr = switch_case->expressions[j];

                if (!(case_expr->flags & AST_NODE_FLAG_TYPED))
                {
                    return false;
                }
                else
                {
                    assert(case_expr->type);
                    assert(case_expr->type->kind == AST_Type_Kind::INTEGER ||
                           case_expr->type->kind == AST_Type_Kind::ENUM);
                }
            }
        }

        return true;
    }

    bool resolver_check_switch_completeness(Resolver *resolver, AST_Statement *ast_stmt)
    {
        assert(ast_stmt->kind == AST_Statement_Kind::SWITCH);

        AST_Type *enum_type = ast_stmt->switch_stmt.expression->type;
        assert(enum_type);

        if (ast_stmt->switch_stmt.default_case) return true;

        auto umvs = enum_type->enum_type.unique_member_values;

        auto ta = temp_allocator_get();

        Array<Integer_Literal> unhandled_umvs = {};
        array_init(ta, &unhandled_umvs, umvs.count);

        for (int64_t i = 0; i < umvs.count; i++) array_append(&unhandled_umvs, umvs[i]);

        auto cases = ast_stmt->switch_stmt.cases;
        for (int64_t i = 0; i < cases.count; i++)
        {
            AST_Switch_Case *switch_case = cases[i];
            for (int64_t j = 0; j < switch_case->expressions.count; j++)
            {
                auto case_expr = switch_case->expressions[j];

                Const_Value expr_val = const_interpret_expression(case_expr);

                int64_t match_idx = -1;

                for (int64_t umv_i = 0; umv_i < unhandled_umvs.count; umv_i++)
                {
                    auto u_umv = unhandled_umvs[umv_i];
                    if (u_umv.u64 == expr_val.integer.u64)
                    {
                        match_idx = umv_i;
                        break;
                    }
                }

                if (match_idx != -1)
                {
                    array_unordered_remove(&unhandled_umvs, match_idx);
                }

                if (unhandled_umvs.count <= 0) break;
            }

            if (unhandled_umvs.count <= 0) break;
        }

        if (unhandled_umvs.count)
        {
            zodiac_report_error(resolver->build_data,
                                Zodiac_Error_Kind::INCOMPLETE_SWITCH,
                                ast_stmt,
                                "Incomplete switch case, %" PRId64 " unhandled enum values.",
                                unhandled_umvs.count);

            auto edecl = enum_type->enum_type.declaration;

            int64_t report_count = min(unhandled_umvs.count, 3);
            for (int64_t i = 0; i < report_count; i++)
            {
                Const_Value cv = { .type = enum_type, .integer = unhandled_umvs[i] };
                auto emem = ast_find_enum_member(enum_type, cv);

                zodiac_report_error(resolver->build_data, Zodiac_Error_Kind::INCOMPLETE_SWITCH,
                                    ast_stmt, 
                                    "Unhandled enum value: %s.%s",
                                    edecl->identifier->atom.data,
                                    emem->identifier->atom.data);
            }

            return false;
        }

        return true;
    }

    void resolver_push_break_node(Resolver *resolver, AST_Node *node)
    {
        assert(node->kind == AST_Node_Kind::STATEMENT);

#ifndef NDEBUG
        auto stmt = static_cast<AST_Statement*>(node);
        assert(stmt->kind == AST_Statement_Kind::WHILE ||
               stmt->kind == AST_Statement_Kind::SWITCH ||
               stmt->kind == AST_Statement_Kind::FOR);
#endif

        stack_push(&resolver->break_node_stack, node);
    }

    void resolver_pop_break_node(Resolver *resolver)
    {
        assert(resolver);

        stack_pop(&resolver->break_node_stack);
    }

    void resolver_push_active_static_branch(Resolver *resolver, bool active)
    {
        bool current_state = resolver_is_active_static_branch(resolver);

        bool new_state = current_state ? active : false;

        stack_push(&resolver->active_static_branch_stack, new_state);

    }

    void resolver_pop_active_static_branch(Resolver *resolver)
    {
        stack_pop(&resolver->active_static_branch_stack);
    }

    bool resolver_is_active_static_branch(Resolver *resolver)
    {
        if (!stack_count(&resolver->active_static_branch_stack))
            return true;

        return stack_top(&resolver->active_static_branch_stack);
    }

    AST_Type *find_or_create_function_type(Resolver *resolver, Array<AST_Type*> param_types,
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

    AST_Type *find_or_create_array_type(Resolver *resolver, AST_Type *element_type,
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

    AST_Type *create_structure_type(Resolver *resolver, AST_Declaration *struct_decl, 
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

    AST_Type *find_or_create_enum_type(Resolver *resolver, AST_Declaration *enum_decl,
                                      AST_Type *base_type, 
                                      Scope *mem_scope,
                                      Scope *current_scope)
    {
        assert(enum_decl->kind == AST_Declaration_Kind::ENUM);

        AST_Type *enum_type = build_data_find_enum_type(resolver->build_data,
                                                        enum_decl);

        if (!enum_type)
        {
            enum_type = ast_enum_type_new(resolver->allocator, enum_decl, base_type,
                                          mem_scope);
            enum_type->flags |= AST_NODE_FLAG_RESOLVED_ID;
            enum_type->flags |= AST_NODE_FLAG_TYPED;
            array_append(&resolver->build_data->type_table, enum_type);
            queue_size_job(resolver, enum_type, current_scope);
        }

        assert(enum_type);
        return enum_type;
    }

    void queue_parse_job(Resolver *resolver, String module_name, String module_path,
                         bool active_static_branch, bool insert_entry_module)
    {
        auto job = resolve_job_new(resolver->allocator, module_name, module_path,
                                   active_static_branch, insert_entry_module);
        assert(job);
        queue_enqueue(&resolver->parse_job_queue, job);
    }

    void queue_ident_job(Resolver *resolver, AST_Node *ast_node, Scope *scope, bool active_branch)
    {
        assert(resolver);
        assert(ast_node);

        if (ast_node->flags & AST_NODE_FLAG_QUEUED_ID) return;

        auto allocator = resolver->allocator;
        assert(allocator);

        auto job = resolve_job_ident_new(allocator, ast_node, scope, active_branch);
        assert(job);
        queue_enqueue(&resolver->ident_job_queue, job);

        ast_node->flags |= AST_NODE_FLAG_QUEUED_ID;
    }

    void queue_type_job(Resolver *resolver, AST_Node *ast_node, Scope *scope, bool active_branch)
    {
        assert(resolver);
        assert(ast_node);
        assert(scope);

        if (ast_node->flags & AST_NODE_FLAG_QUEUED_TYPING)
            return;

        auto allocator = resolver->allocator;
        assert(allocator);

        auto job = resolve_job_type_new(allocator, ast_node, scope, active_branch);
        assert(job);
        queue_enqueue(&resolver->type_job_queue, job);

        ast_node->flags |= AST_NODE_FLAG_QUEUED_TYPING;
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

#ifndef NDEBUG
        if (ast_node->kind == AST_Node_Kind::DECLARATION)
        {
            auto decl = static_cast<AST_Declaration*>(ast_node);

            if (decl->kind == AST_Declaration_Kind::FUNCTION)
                assert(decl->decl_flags & AST_DECL_FLAG_REGISTERED_BYTECODE);
        }
#endif

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

    void queue_emit_llvm_global_job(Resolver *resolver, Bytecode_Global bc_glob)
    {
        assert(bc_glob.decl);
        assert(bc_glob.type);

        auto job = resolve_job_emit_llvm_global_new(resolver->allocator, bc_glob);
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

    Resolve_Job *resolve_job_new(Allocator *allocator, Resolve_Job_Kind kind,
                                 bool active_static_branch)
    {
        auto result = alloc_type<Resolve_Job>(allocator);
        assert(result);
        result->kind = kind;
        result->result = {};
        result->active_static_branch = active_static_branch;
        return result;
    }

    Resolve_Job *resolve_job_new(Allocator *allocator, Resolve_Job_Kind kind, AST_Node *ast_node, 
                                 Scope *scope, bool active_branch)
    {
        auto result = resolve_job_new(allocator, kind, active_branch);
        result->ast_node = ast_node;
        result->node_scope = scope;
        return result;
    }

    Resolve_Job *resolve_job_new(Allocator *allocator, Bytecode_Function *bc_func,
                                 bool active_branch)
    {
        auto result = resolve_job_new(allocator, Resolve_Job_Kind::EMIT_LLVM_FUNC, active_branch);
        result->bc_func = bc_func;
        return result;
    }

    Resolve_Job *resolve_job_new(Allocator *allocator, Bytecode_Global bc_glob, bool active_branch)
    {
        auto result = resolve_job_new(allocator, Resolve_Job_Kind::EMIT_LLVM_GLOB, active_branch);
        result->bc_glob = bc_glob;
        return result;
    }

    Resolve_Job *resolve_job_new(Allocator *allocator, const char *output_file_name,
                                 bool active_branch)
    {
        auto result = resolve_job_new(allocator, Resolve_Job_Kind::EMIT_LLVM_BINARY,
                                      active_branch);
        result->llvm_bin.output_file_name = output_file_name;
        return result;
    }

    Resolve_Job *resolve_job_new(Allocator *allocator, String module_name, String module_path, 
                                 bool active_static_branch, bool insert_entry_module)
    {
        auto result = resolve_job_new(allocator, Resolve_Job_Kind::PARSE, active_static_branch);
        result->parse.module_name = module_name;
        result->parse.module_path = module_path;
        result->parse.insert_entry_module = insert_entry_module;
        return result;
    }

    Resolve_Job *resolve_job_ident_new(Allocator *allocator, AST_Node *ast_node, Scope *scope, 
                                       bool active_branch)
    {
        auto result = resolve_job_new(allocator, Resolve_Job_Kind::IDENTIFIER, ast_node, scope,
                                      active_branch);

        return result;
    }

    Resolve_Job *resolve_job_type_new(Allocator *allocator, AST_Node *ast_node, Scope *scope,
                                      bool active_branch)
    {
        return resolve_job_new(allocator, Resolve_Job_Kind::TYPE, ast_node, scope, active_branch);
    }

    Resolve_Job *resolve_job_size_new(Allocator *allocator, AST_Node *ast_node, Scope *scope)
    {
        return resolve_job_new(allocator, Resolve_Job_Kind::SIZE, ast_node, scope, true);
    }

    Resolve_Job *resolve_job_emit_bytecode_new(Allocator *allocator, AST_Node *ast_node,
                                               Scope *scope)
    {
        return resolve_job_new(allocator, Resolve_Job_Kind::EMIT_BYTECODE, ast_node, scope, true);
    }

    Resolve_Job *resolve_job_emit_llvm_func_new(Allocator *allocator, Bytecode_Function *bc_func)
    {
        return resolve_job_new(allocator, bc_func, true);
    }

    Resolve_Job *resolve_job_emit_llvm_global_new(Allocator *allocator, Bytecode_Global bc_glob)
    {
        return resolve_job_new(allocator, bc_glob, true); 
    }

    Resolve_Job *resolve_job_emit_llvm_binary_new(Allocator *allocator,
                                                  const char *output_file_name)
    {
        return resolve_job_new(allocator,  output_file_name, true);
    }

    AST_Declaration *resolver_get_declaration(AST_Expression *expr)
    {
        assert(expr->flags & AST_NODE_FLAG_RESOLVED_ID);

        switch (expr->kind)
        {
            case AST_Expression_Kind::INVALID: assert(false);

            case AST_Expression_Kind::IDENTIFIER: return expr->identifier->declaration;

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);

            case AST_Expression_Kind::DOT: return expr->dot.child_decl;

            case AST_Expression_Kind::BINARY: assert(false);

            case AST_Expression_Kind::UNARY:
            {
                assert(expr->unary.op == UNOP_DEREF);
                return resolver_get_declaration(expr->unary.operand_expression);
                break;
            }

            case AST_Expression_Kind::POST_FIX: assert(false);
            case AST_Expression_Kind::PRE_FIX: assert(false);
            case AST_Expression_Kind::CALL: assert(false);
            case AST_Expression_Kind::BUILTIN_CALL: assert(false);
            case AST_Expression_Kind::ADDROF: assert(false);
            case AST_Expression_Kind::COMPOUND: assert(false);

            case AST_Expression_Kind::SUBSCRIPT:
            {
                return resolver_get_declaration(expr->subscript.pointer_expression);
                break;
            }

            case AST_Expression_Kind::CAST: assert(false);
            case AST_Expression_Kind::INTEGER_LITERAL: assert(false);
            case AST_Expression_Kind::FLOAT_LITERAL: assert(false);
            case AST_Expression_Kind::STRING_LITERAL: assert(false);
            case AST_Expression_Kind::CHAR_LITERAL: assert(false);
            case AST_Expression_Kind::BOOL_LITERAL: assert(false);
            case AST_Expression_Kind::NULL_LITERAL: assert(false);

            case AST_Expression_Kind::RANGE: assert(false);
        }

        assert(false);
        return nullptr;
    }

    bool resolver_assign_enum_initializers(Resolver *resolver, AST_Declaration *decl,
                                           AST_Type *enum_type)
    {
        assert(decl->kind == AST_Declaration_Kind::ENUM);
        assert(enum_type->kind == AST_Type_Kind::ENUM);
        assert(enum_type->enum_type.declaration == decl);

        assert(enum_type->enum_type.unique_member_values.count == 0);
        array_init(resolver->allocator, &enum_type->enum_type.unique_member_values);

        bool result = true;

        auto members = decl->enum_decl.member_declarations;

        int64_t next_value = 0;
        int64_t current_value = 0;

        for (int64_t i = 0; i < members.count; i++)
        {
            auto mem_decl = members[i];
            assert(mem_decl->kind == AST_Declaration_Kind::CONSTANT);

            auto init_expr = mem_decl->constant.init_expression;
            if (mem_decl->decl_flags & AST_DECL_FLAG_ENUM_MEMBER_INTINIT)
            {
                assert(init_expr->kind == AST_Expression_Kind::INTEGER_LITERAL);
                auto nv = const_interpret_expression(init_expr);
                next_value = nv.integer.s64 + 1;
                current_value = nv.integer.s64;
            }
            else if (mem_decl->decl_flags & AST_DECL_FLAG_ENUM_MEMBER_IDENTINIT)
            {
                assert(init_expr->kind == AST_Expression_Kind::IDENTIFIER);
                assert(init_expr->expr_flags & AST_EXPR_FLAG_CONST);

                auto id_decl = init_expr->identifier->declaration;
                assert(id_decl);
                assert(id_decl->kind == AST_Declaration_Kind::CONSTANT);

                auto id_init_expr = id_decl->constant.init_expression;
                assert(id_init_expr->type);
                assert(id_init_expr->kind == AST_Expression_Kind::INTEGER_LITERAL);

                auto nv = const_interpret_expression(id_init_expr);
                next_value = nv.integer.s64 + 1;
                current_value = nv.integer.s64;
            }
            else
            {
                assert(init_expr->kind == AST_Expression_Kind::INTEGER_LITERAL);
                init_expr->integer_literal.s64 = next_value;
                current_value = next_value;
                next_value += 1;
            }

            bool match_found = false;

            for (int64_t j = 0; j < enum_type->enum_type.unique_member_values.count; j++)
            {
                auto umv = enum_type->enum_type.unique_member_values[j];
                if (umv.s64 == current_value)
                {
                    match_found = true;
                    break;
                }
            }

            if (!match_found)
            {
                array_append(&enum_type->enum_type.unique_member_values,
                             { .s64 = current_value });
            }
        }

        return result;
    }

    bool resolver_import_using_scope(Resolver *resolver, Scope *current_scope, Scope *using_scope)
    {
        assert(current_scope != using_scope);

        Scope_Block *scope_block = &using_scope->first_block;

        bool result = true;

        while (scope_block)
        {
            for (int64_t i = 0; i < scope_block->decl_count; i++)
            {
                AST_Declaration *decl = scope_block->declarations[i];

                if (decl->identifier)
                {
                    auto redecl = scope_find_declaration(current_scope, decl->identifier);

                    if (redecl)
                    {
                        zodiac_report_error(resolver->build_data,
                                            Zodiac_Error_Kind::REDECLARATION, decl->identifier,
                                            "Redeclaration of identifier: '%s'",
                                            decl->identifier->atom.data);

                        zodiac_report_info(resolver->build_data, redecl->identifier, 
                                           "Previous declaration was here");

                        result = false;
                    }
                    else
                    {
                        resolver->progression.scope_imports_done = true;
                        scope_add_declaration(current_scope, decl);
                    }
                }
                else if (decl->kind == AST_Declaration_Kind::USING)
                {
                    assert(false);
                }
                else
                {
                    assert(false);
                }

            }

            scope_block = scope_block->next_block;
        }

        return result;
    }

    bool resolver_import_from_static_if(Resolver *resolver, AST_Declaration *decl, Scope *scope)
    {
        if (!(decl->decl_flags & AST_DECL_FLAG_IMPORTED_FROM_STATIC_IF))
        {

            while (scope->kind == Scope_Kind::STATIC_IF)
            {
                assert(scope->parent);
                scope = scope->parent;
            }
            if (decl->identifier)
            {
                auto redecl = scope_find_declaration(scope, decl->identifier);
                if (redecl)
                {
                    zodiac_report_error(resolver->build_data, Zodiac_Error_Kind::REDECLARATION,
                                        decl->identifier, "Redelaration of identifier: '%s'",
                                        decl->identifier->atom.data);
                    zodiac_report_info(resolver->build_data, redecl->identifier,
                                       "Previous declaration was here");
                    return false;
                }
            }

            resolver->progression.scope_imports_done = true;

            scope_add_declaration(scope, decl);
            decl->decl_flags |= AST_DECL_FLAG_IMPORTED_FROM_STATIC_IF;
            
            if (scope->kind == Scope_Kind::MODULE)
            {
                assert(!(decl->decl_flags & AST_DECL_FLAG_GLOBAL));
                decl->decl_flags |= AST_DECL_FLAG_GLOBAL;

                assert(decl->flags & AST_NODE_FLAG_TYPED);

                if (decl->kind == AST_Declaration_Kind::FUNCTION && 
                    !(decl->decl_flags & AST_DECL_FLAG_REGISTERED_BYTECODE))
                {
                    assert(decl->type && decl->type->kind == AST_Type_Kind::FUNCTION);
                    auto bc_func = bytecode_register_function(&resolver->bytecode_builder, decl);
                    llvm_register_function(&resolver->llvm_builder, bc_func, decl->type);
                }
                queue_emit_bytecode_job(resolver, decl, scope);

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
        }

        return true;
    }

    void resolver_inherit_const(AST_Expression *expr)
    {
        if (expr->expr_flags & AST_EXPR_FLAG_CONST) return;

        bool is_const = false;

        switch (expr->kind)
        {
            case AST_Expression_Kind::INVALID: assert(false);

            case AST_Expression_Kind::IDENTIFIER:
            case AST_Expression_Kind::DOT:
            {
                if (expr->expr_flags & AST_EXPR_FLAG_DOT_COUNT)
                {
                    is_const = true;
                }
                else
                {
                    auto decl = resolver_get_declaration(expr);
                    assert(decl);

                    switch (decl->kind)
                    {
                        case AST_Declaration_Kind::VARIABLE:
                        case AST_Declaration_Kind::PARAMETER: break;

                        case AST_Declaration_Kind::CONSTANT:
                        case AST_Declaration_Kind::FUNCTION:
                        case AST_Declaration_Kind::IMPORT:
                        case AST_Declaration_Kind::TYPE: 
                        case AST_Declaration_Kind::TYPEDEF: 
                        case AST_Declaration_Kind::STRUCTURE:
                        case AST_Declaration_Kind::ENUM: is_const = true; break;

                        default: assert(false);
                    }
                }
                break;
            }


            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);

            case AST_Expression_Kind::BINARY:
            {
                auto lhs = expr->binary.lhs;
                auto rhs = expr->binary.rhs;

                is_const = ((lhs->expr_flags & AST_EXPR_FLAG_CONST) &&
                            (rhs->expr_flags & AST_EXPR_FLAG_CONST));
                break;
            }

            case AST_Expression_Kind::UNARY:
            {
                auto op_expr = expr->unary.operand_expression;
                is_const = op_expr->expr_flags & AST_EXPR_FLAG_CONST;
                break;
            }

            case AST_Expression_Kind::POST_FIX:
            {
                auto op_expr = expr->post_fix.operand_expression;
                is_const = op_expr->expr_flags & AST_EXPR_FLAG_CONST;
                break;
            }

            case AST_Expression_Kind::PRE_FIX:
            {
                auto op_expr = expr->pre_fix.operand_expression;
                is_const = op_expr->expr_flags & AST_EXPR_FLAG_CONST;
                break;
            }

            case AST_Expression_Kind::BUILTIN_CALL:
            {
                auto atom = expr->builtin_call.identifier->atom;

                if (atom == Builtin::atom_exit)
                {
                    assert(expr->builtin_call.arg_expressions.count == 1);
                    auto op_expr = expr->builtin_call.arg_expressions[0];
                    is_const = op_expr->flags & AST_EXPR_FLAG_CONST;
                }
                else if (atom == Builtin::atom_syscall)
                {
                    is_const = false;
                }
                else if (atom == Builtin::atom_cast)
                {
                    assert(expr->builtin_call.arg_expressions.count == 2);
                    auto op_expr = expr->builtin_call.arg_expressions[1];
                    is_const = op_expr->flags & AST_EXPR_FLAG_CONST;
                }
                else if (atom == Builtin::atom_sizeof ||
                         atom == Builtin::atom_offsetof)
                {
                    is_const = true;
                }
                else 
                {
                    assert(false);
                }
                break;
            }

            case AST_Expression_Kind::CALL:
            case AST_Expression_Kind::ADDROF: break;
            case AST_Expression_Kind::COMPOUND: assert(false);

            case AST_Expression_Kind::SUBSCRIPT:
            {
                auto pointer_expr = expr->subscript.pointer_expression;
                auto index_expr = expr->subscript.index_expression;

                is_const = ((pointer_expr->expr_flags & AST_EXPR_FLAG_CONST) &&
                            (index_expr->expr_flags & AST_EXPR_FLAG_CONST));
                break;
            }

            case AST_Expression_Kind::CAST: 
            {
                is_const =
                    (expr->cast.operand_expression->expr_flags & AST_EXPR_FLAG_CONST);
                break;
            }

            case AST_Expression_Kind::INTEGER_LITERAL: assert(false);
            case AST_Expression_Kind::FLOAT_LITERAL: assert(false);
            case AST_Expression_Kind::STRING_LITERAL: assert(false);
            case AST_Expression_Kind::CHAR_LITERAL: assert(false);
            case AST_Expression_Kind::BOOL_LITERAL: assert(false);

            case AST_Expression_Kind::NULL_LITERAL: assert(false);

            case AST_Expression_Kind::RANGE:
            {
                auto begin = expr->range.begin;
                auto end = expr->range.end;
                is_const = ((begin->expr_flags & AST_EXPR_FLAG_CONST) &&
                            (end->expr_flags & AST_EXPR_FLAG_CONST));
                break;
            }
        }

        if (is_const)
        {
            expr->expr_flags |= AST_EXPR_FLAG_CONST; 
        }
    }

    bool resolver_valid_type_conversion(AST_Type *type, AST_Type *target_type)
    {
        assert(type);
        assert(target_type);

        switch (type->kind)
        {
            case AST_Type_Kind::INVALID: assert(false);
            case AST_Type_Kind::VOID: assert(false);

            case AST_Type_Kind::INTEGER:
            {
                if (target_type->kind == AST_Type_Kind::INTEGER)
                {
                    if (type->integer.sign == target_type->integer.sign)
                    {
                        return type->bit_size < target_type->bit_size;
                    } 
                    else if (type->integer.sign)
                    {
                        return false;
                    }
                    else
                    {
                        assert(target_type->integer.sign);
                        return type->bit_size < target_type->bit_size;
                    }
                }
                else if (target_type->kind == AST_Type_Kind::BOOL)
                {
                    return true;
                }
                else assert(false);

                break;
            }

            case AST_Type_Kind::FLOAT:
            {
                if (target_type->kind == AST_Type_Kind::INTEGER) 
                {
                    return false;
                }
                else assert(false);
            }

            case AST_Type_Kind::BOOL: assert(false);
            case AST_Type_Kind::POINTER: assert(false);
            case AST_Type_Kind::FUNCTION: assert(false);
            case AST_Type_Kind::STRUCTURE: assert(false);

            case AST_Type_Kind::ENUM:
            {
                if (target_type->kind == AST_Type_Kind::INTEGER)
                {
                    assert(type->enum_type.base_type);
                    return resolver_valid_type_conversion(type->enum_type.base_type,
                                                          target_type);
                }
                else assert(false);
                break;
            }

            case AST_Type_Kind::ARRAY: assert(false);
        }

        assert(false);
        return false;
    }

    bool resolver_literal_fits_in_type(const Integer_Literal &number_literal, AST_Type *type)
    {

        if (type->kind == AST_Type_Kind::ENUM)
        {
            type = type->enum_type.base_type; 
        }

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

        assert(false);
        return false;
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
            decl->identifier->atom == Builtin::atom_call_main_and_exit)
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

        zodiac_report_error(resolver->build_data,
                            Zodiac_Error_Kind::UNDECLARED_IDENTIFIER,
                            identifier,
                            "Reference to undeclared identifier: '%.*s'",
                            (int)atom.length, atom.data);
    }

    void resolver_report_mismatching_types(Resolver *resolver, AST_Node *node,
                                           AST_Type *expected_type, AST_Type *actual_type)
    {
        auto ta = temp_allocator_get();

        auto expected_type_str = ast_type_to_string(ta, expected_type);
        auto actual_type_str = ast_type_to_string(ta, actual_type);

        zodiac_report_error(resolver->build_data,
                            Zodiac_Error_Kind::MISMATCHING_TYPES,
                            node, 
                            "Mismatching types: expected '%s', got '%s'",
                            expected_type_str.data,
                            actual_type_str.data);
    }

}
