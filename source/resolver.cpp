#include "resolver.h"

#include "builtin.h"
#include "const_interpreter.h"
#include "interpreter.h"
#include "os.h"
#include "os_linux.h"
#include "temp_allocator.h"

namespace Zodiac
{
    void resolver_init(Allocator *allocator, Resolver *resolver, Build_Data *build_data,
                       String first_file_path)
    {
        resolver->allocator = allocator;
        resolver->build_data = build_data;

        resolver->lexer = lexer_create(allocator, build_data);
        resolver->ast_builder = {
            .allocator = allocator,
            .build_data = build_data,
            .break_stack = {},
        };

        stack_init(allocator, &resolver->ast_builder.break_stack);

        resolver->parser = parser_create(allocator, build_data, resolver);
        resolver->bytecode_builder = bytecode_builder_create(allocator, build_data);
        resolver->llvm_builder = llvm_builder_create(allocator, build_data);

        queue_init(allocator, &resolver->parse_jobs);
        queue_init(allocator, &resolver->resolve_jobs);
        queue_init(allocator, &resolver->size_jobs);
        queue_init(allocator, &resolver->bytecode_jobs);
        queue_init(allocator, &resolver->run_jobs);
        queue_init(allocator, &resolver->llvm_jobs);

        resolver->progressed = false;

        array_init(allocator, &resolver->parsed_modules);

        resolver->global_scope = scope_new(allocator, Scope_Kind::GLOBAL, nullptr);

        resolver->entry_decl = nullptr;

        auto builtin_declarations = builtin_populate_scope(allocator, resolver->global_scope);

        for (int64_t i = 0; i < builtin_declarations.count; i++) {
            ast_flatten_declaration(&resolver->ast_builder, &builtin_declarations[i]);
            queue_resolve_job(resolver, builtin_declarations[i]);
        }

        if (is_relative_path(first_file_path)) {
            first_file_path = get_absolute_path(allocator, first_file_path);
        }

        auto ta = temp_allocator_get();

        assert(string_ends_with(first_file_path, ".zdc"));
        auto file_name = get_file_name(ta, first_file_path);
        auto first_file_name = string_copy(allocator, file_name, file_name.length - 4);

        if (!build_data->options->exe_file_name.data) {
            build_data->options->exe_file_name = first_file_name;
        }

        resolver->first_file_dir = get_file_dir(allocator, first_file_path);

        resolver->zodiac_root_dir = find_zodiac_root(allocator, build_data);
        auto module_dir = string_append(ta, resolver->zodiac_root_dir, "modules");
        module_dir = os_normalize_path(ta, module_dir);
        assert(is_directory(module_dir));

#ifdef linux
        if (!string_ends_with(module_dir, "/")) {
            module_dir = string_append(ta, module_dir, "/");
        }
#elif WIN32
        if (!string_ends_with(module_dir, "\\")) {
            module_dir = string_append(ta, module_dir, "\\");
        }
#endif
        resolver->module_dir = string_copy(allocator, module_dir);

        auto entry_file_path = string_append(allocator, resolver->module_dir,
                                             "entry.zdc");
        assert(is_regular_file(entry_file_path));

        resolver->entry_module_path = entry_file_path;

        queue_parse_job(resolver, first_file_name, first_file_path, nullptr, true);
    }

    void start_resolving(Resolver *resolver)
    {
        // Don't do anything when not running threaded
    }

    Resolve_Result finish_resolving(Resolver *resolver)
    {
        bool done = false;

        auto bd = resolver->build_data;

        Resolve_Result result = {};

        bool progressed = false;
        uint64_t cycle_count = 0;

        while (!done) {

            if (resolver->build_data->options->verbose) {
                printf("[RESOLVER] Starting resolve cycle: %" PRIu64 "\n", cycle_count);
            }

            auto ta = temp_allocator_get();
            temp_allocator_reset(ta);

            auto parse_job_count = queue_count(&resolver->parse_jobs);
            while (parse_job_count--) {
                auto job = queue_dequeue(&resolver->parse_jobs);

                if (!try_parse_job(resolver, &job)) {
                    result.parse_error = true;
                    done = true;
                    break;
                } else {
                    assert(job.parsed_module_index >= 0);

                    progressed = true;

                    Parsed_Module &pm = resolver->parsed_modules[job.parsed_module_index];

                    if (job.insert_entry_module) {
                        assert(!resolver->build_data->entry_module);
                        resolver->build_data->entry_module = pm.ast;
                    }

                    assert(pm.ast->kind == AST_Node_Kind::MODULE);
                    AST_Module *ast_module = pm.ast;

                    auto bl = bucket_array_first(&ast_module->declarations);
                    while (bl.bucket) {
                        auto p_decl = bucket_locator_get_ptr(bl);
                        auto decl = *p_decl;
                        queue_resolve_job(resolver, decl);

                        bucket_locator_advance(&bl);
                    }
                }
            }

            if (result.parse_error) {
                zodiac_report_errors(resolver->build_data);
                break;
            }

            auto resolve_job_count = queue_count(&resolver->resolve_jobs);
            while (resolve_job_count--) {
                auto job = queue_dequeue(&resolver->resolve_jobs);

                if (!try_resolve_job(resolver, &job)) {
                    queue_enqueue(&resolver->resolve_jobs, job);
                } else {
                    progressed = true;
                    queue_size_job(resolver, job.ast_node);

                    if (job.ast_node->kind == AST_Node_Kind::DECLARATION) {
                        auto decl = static_cast<AST_Declaration *>(job.ast_node);
                        if (!resolver->entry_decl && is_entry_decl(resolver, decl)) {
                            resolver->entry_decl = decl;
                            decl->decl_flags |= AST_DECL_FLAG_IS_ENTRY;
                        } else if (is_bytecode_entry_decl(decl)) {
                            // decl->decl_flags |= AST_DECL_FLAG_IS_BYTECODE_ENTRY;
                        }
                    }
                }
            }

            auto size_job_count = queue_count(&resolver->size_jobs);
            while (size_job_count--) {
                auto job = queue_dequeue(&resolver->size_jobs);

                if (!try_size_job(resolver, &job)) {
                    assert(false);
                } else {

                    auto node = job.ast_node;
                    if (node->kind == AST_Node_Kind::DECLARATION) {
                        auto decl = static_cast<AST_Declaration *>(node);
                        if (decl->kind == AST_Declaration_Kind::FUNCTION) {

                            // We already queue a job because this node is at the end of the
                            //  flattened list..
                            //
                            // queue_bytecode_job(resolver, decl);

                        } else if (decl->kind == AST_Declaration_Kind::RUN) {
                            // Any function this might call should have been queued
                            //  by now, or in this loop after we fail this run for
                            //  the first time..

                            queue_bytecode_job(resolver, decl);

                        } else if (decl->kind == AST_Declaration_Kind::CONSTANT ||
                                   decl->kind == AST_Declaration_Kind::VARIABLE) {
                            if (decl->decl_flags & AST_DECL_FLAG_GLOBAL) {
                                queue_bytecode_job(resolver, decl);
                            } else {
                                // We queue jobs for these because we flatten them individually,
                                //  but we don't need to generate bytecode for each seperate one.
                                assert((decl->decl_flags & AST_DECL_FLAG_IS_STRUCT_MEMBER) ||
                                       (decl->decl_flags & AST_DECL_FLAG_IS_UNION_MEMBER));
                            }
                        } else {
                            assert(decl->kind == AST_Declaration_Kind::IMPORT ||
                                   decl->kind == AST_Declaration_Kind::USING ||
                                   decl->kind == AST_Declaration_Kind::STRUCTURE ||
                                   decl->kind == AST_Declaration_Kind::UNION ||
                                   decl->kind == AST_Declaration_Kind::TYPEDEF ||
                                   decl->kind == AST_Declaration_Kind::ENUM ||
                                   decl->kind == AST_Declaration_Kind::STATIC_IF);
                        }
                    } else {
                        assert(node->kind == AST_Node_Kind::EXPRESSION);
                    }

                    progressed = true;
                }
            }

            auto bytecode_job_count = queue_count(&resolver->bytecode_jobs);
            while (bytecode_job_count--) {
                auto job = queue_dequeue(&resolver->bytecode_jobs);

                if (job.decl->kind == AST_Declaration_Kind::FUNCTION) {

                    if (resolver->build_data->options->link_c &&
                        (job.decl->decl_flags & AST_DECL_FLAG_IS_ENTRY)) {
                        continue;
                    }

                    // The function should have been registered at this point
                    //  (after the type spec is resolved).
                    auto bc_func =
                        bytecode_emit_function_declaration(&resolver->bytecode_builder,
                                                           job.decl);
                    assert(bc_func);

                    job.decl->decl_flags |= AST_DECL_FLAG_EMITTED_BYTECODE;

                    if (!resolver->build_data->options->dont_emit_llvm) {
                        queue_llvm_job(resolver, bc_func);
                    }
                } else if (job.decl->kind == AST_Declaration_Kind::VARIABLE) {
                    assert(job.decl->decl_flags & AST_DECL_FLAG_GLOBAL);
                    auto bc_global =
                        bytecode_emit_global_variable(&resolver->bytecode_builder, job.decl);

                    if (!resolver->build_data->options->dont_emit_llvm) {
                        queue_llvm_job(resolver, bc_global);
                    }

                } else if (job.decl->kind == AST_Declaration_Kind::CONSTANT) {
                    // Dont do anything
                } else if (job.decl->kind == AST_Declaration_Kind::RUN) {
                    Bytecode_Function *pre_main_func = nullptr;

                    if (bd->pre_main_func) {
                        pre_main_func = bd->pre_main_func;
                    } else {
                        assert(bd->entry_module);
                        Scope *entry_scope = bd->entry_module->module_scope;
                        assert(entry_scope);
                        assert(entry_scope->kind == Scope_Kind::MODULE);
                        AST_Declaration *pre_main_decl =
                            scope_find_declaration(entry_scope, Builtin::atom_pre_main);
                        assert(pre_main_decl);
                        assert(pre_main_decl->kind == AST_Declaration_Kind::FUNCTION);

                        if ((pre_main_decl->flags & AST_NODE_FLAG_RESOLVED_ID) &&
                            (pre_main_decl->flags & AST_NODE_FLAG_TYPED)) {

                            pre_main_func = bytecode_find_function(&resolver->bytecode_builder,
                                                                   pre_main_decl);
                            if (pre_main_func) {
                                if (!(pre_main_func->flags & BC_FUNC_FLAG_EMITTED)) {
                                    bytecode_emit_function_declaration(
                                            &resolver->bytecode_builder,
                                            pre_main_decl);
                                }

                                bd->pre_main_func = pre_main_func;
                            }
                        }

                    }

                    if (pre_main_func) {
                        Bytecode_Function *wrapper =
                            bytecode_emit_run_wrapper(&resolver->bytecode_builder, job.decl,
                                                      pre_main_func);
                        queue_run_job(resolver, job.decl, wrapper);
                    } else {
                        queue_enqueue(&resolver->bytecode_jobs, job);
                    }
                } else {
                    assert(false);
                }


                progressed = true;
            }

            auto run_job_count = queue_count(&resolver->run_jobs);
            if (!bytecode_ready_to_run(&resolver->bytecode_builder)) {
                run_job_count = 0;
            }
            while (run_job_count--) {
                auto job = queue_dequeue(&resolver->run_jobs);

                auto run_decl = job.run_decl;
                assert(run_decl->kind == AST_Declaration_Kind::RUN);
                auto call_expr = run_decl->run.expression;
                assert(call_expr->kind == AST_Expression_Kind::CALL);
                auto callee_decl = call_expr->call.callee_declaration;
                assert(callee_decl);
                assert(callee_decl->kind == AST_Declaration_Kind::FUNCTION);

                if (all_dependencies_emitted(resolver, callee_decl)) {
                    assert(job.wrapper);
                    assert(job.wrapper->flags & BC_FUNC_FLAG_EMITTED);

                    Interpreter interp = interpreter_create(resolver->allocator,
                                                            resolver->build_data);

                    interpreter_start(&interp, job.wrapper,
                                      resolver->bytecode_builder.global_data_size,
                                      resolver->bytecode_builder.globals,
                                      resolver->bytecode_builder.foreign_functions);

                    if (interp.build_data->options->verbose)
                        printf("Interpreter exited with code: %" PRId64 " after run directive\n",
                               interp.exit_code);
                    progressed = true;
                } else {
                    queue_enqueue(&resolver->run_jobs, job);
                }

            }

            auto llvm_job_count = queue_count(&resolver->llvm_jobs);
            if (!llvm_ready_to_emit(&resolver->llvm_builder)) {
                llvm_job_count = 0;
            }
            while (llvm_job_count--) {
                auto job = queue_dequeue(&resolver->llvm_jobs);

                switch (job.kind) {
                    case LLVM_Job_Kind::FUNCTION: {
                        auto bc_func = job.bc_func;

                        // Should have been registered at the same time as the bytecode
                        llvm_emit_function(&resolver->llvm_builder, bc_func);

                        progressed = true;
                        break;
                    }

                    case LLVM_Job_Kind::GLOBAL: {
                        auto &bc_global = job.bc_global;
                        llvm_emit_global(&resolver->llvm_builder, bc_global);
                        progressed = true;
                        break;
                    }

                    case LLVM_Job_Kind::INVALID: assert(false);
                }
            }

            for (int64_t i = resolver->llvm_builder.struct_types_to_finalize.count - 1;
                 i >= 0;
                 i --) {

                auto &to_finalize = resolver->llvm_builder.struct_types_to_finalize[i];
                if (to_finalize.ast_type->flags & AST_NODE_FLAG_RESOLVED_ID) {
                    progressed = true;
                    llvm_finalize_struct_type(&resolver->llvm_builder, to_finalize.llvm_type,
                                              to_finalize.ast_type);
                    array_unordered_remove(&resolver->llvm_builder.struct_types_to_finalize,
                                           i);
                }
            }

            for (int64_t i = resolver->llvm_builder.union_types_to_finalize.count - 1;
                 i >= 0;
                 i --) {

                auto &to_finalize = resolver->llvm_builder.union_types_to_finalize[i];
                if (to_finalize.ast_type->flags & AST_NODE_FLAG_RESOLVED_ID) {
                    progressed = true;
                    llvm_finalize_union_type(&resolver->llvm_builder, to_finalize.llvm_type,
                                             to_finalize.ast_type);
                    array_unordered_remove(&resolver->llvm_builder.union_types_to_finalize,
                                           i);
                }
            }

            if (queue_count(&resolver->parse_jobs)    == 0 &&
                queue_count(&resolver->resolve_jobs)  == 0 &&
                queue_count(&resolver->size_jobs)     == 0 &&
                queue_count(&resolver->bytecode_jobs) == 0 &&
                queue_count(&resolver->llvm_jobs)     == 0 &&
                resolver->llvm_builder.struct_types_to_finalize.count == 0 &&
                resolver->llvm_builder.union_types_to_finalize.count  == 0) {
                done = true;
            }

            if (fatal_error_reported(resolver) || (!progressed && !resolver->progressed)) {

                if (resolver->build_data->errors.count == 0)
                    resolver_check_circular_dependencies(resolver);
                done = true;
            } else {
                zodiac_clear_errors(resolver->build_data);
            }

            progressed = false;
            resolver->progressed = false;
            cycle_count++;
        }

        if (resolver->build_data->options->verbose) {
            printf("Resolver done after %" PRId64 " cycles\n", cycle_count);
        }
        return result;
    }

    bool queue_parse_job(Resolver *resolver, String module_name, String module_path,
                         AST_Module **ast_module, bool insert_entry_module/*= false*/)
    {
        auto parse_jobs = resolver->parse_jobs;

        for (int64_t i = 0; i < parse_jobs.used; i++) {
            auto index = parse_jobs.front + i;
            if (index >= parse_jobs.capacity) {
                index -= parse_jobs.capacity;
            }

            auto &it = parse_jobs.buffer[index];

            if (string_equal(it.module_path, module_path)) {
                assert(string_equal(it.module_name, module_name));
                assert(it.insert_entry_module == insert_entry_module);
                return true;
            }
        }

        for (int64_t i = 0; i < resolver->parsed_modules.count; i++) {
            auto &pm = resolver->parsed_modules[i];
            if (string_equal(pm.full_path, module_path)) {
                assert(string_equal(pm.name, module_name));
                if (ast_module) {
                    assert(*ast_module == nullptr);
                    *ast_module = pm.ast;
                }
                return false;
            }
        }

        // if (resolver->build_data->options->verbose)
        //     printf("[RESOLVER] Queueing parse job for file: '%s'\n", module_path.data);

        Parse_Job job = { .module_name = string_copy(resolver->allocator, module_name),
                          .module_path = string_copy(resolver->allocator, module_path),
                          .insert_entry_module = insert_entry_module,
                          .parsed_module_index = -1,
        };

        queue_enqueue(&resolver->parse_jobs, job);
        return true;
    }

    bool queue_parse_job(Resolver *resolver, String module_name, String module_path)
    {
        AST_Module *ex_module = nullptr;
        bool in_queue = queue_parse_job(resolver, module_name, module_path, &ex_module, false);
        if (in_queue) {
            assert(!ex_module);
        } else {
            assert(ex_module);
        }
        return in_queue;
    }

    void queue_resolve_job(Resolver *resolver, AST_Node *ast_node)
    {
        assert(ast_node->kind == AST_Node_Kind::DECLARATION);

        // if (resolver->build_data->options->verbose) {
        //     auto decl = static_cast<AST_Declaration *>(ast_node);
        //     if (decl->identifier) {
        //         printf("[RESOLVER] Queueing resolve job for declaration: '%s'\n",
        //                decl->identifier->atom.data);
        //     } else if (decl->kind == AST_Declaration_Kind::USING) {
        //         printf("[RESOLVER] Queueing resolve job for using: '");
        //         ast_print_expression(decl->using_decl.ident_expr, 0);
        //         printf("'\n");
        //     } else if (decl->kind == AST_Declaration_Kind::RUN) {
        //         printf("[RESOLVER] Queueing resolve job for #run: '");
        //         ast_print_expression(decl->run.expression, 0);
        //         printf("'\n");
        //     } else if (decl->kind == AST_Declaration_Kind::STATIC_IF) {
        //         printf("[RESOLVER] Queueing resolve job for static_if: '");
        //         ast_print_expression(decl->static_if.cond_expression, 0);
        //         printf("'\n");
        //     } else {
        //         assert(false);
        //     }
        // }

        Resolve_Job job = { .ast_node = ast_node, };
        queue_enqueue(&resolver->resolve_jobs, job);
    }

    void queue_size_job(Resolver *resolver, AST_Node *node)
    {
        assert(node->kind == AST_Node_Kind::DECLARATION ||
               node->kind == AST_Node_Kind::EXPRESSION);

        // if (resolver->build_data->options->verbose) {
        //     auto decl = static_cast<AST_Declaration *>(node);
        //     if (decl->identifier) {
        //         printf("[RESOLVER] Queueing size job for declaration: '%s'\n",
        //                decl->identifier->atom.data);
        //     } else if (decl->kind == AST_Declaration_Kind::RUN) {
        //         printf("[RESOLVER] Queueing size job for #run: '");
        //         ast_print_expression(decl->run.expression, 0);
        //         printf("'\n");
        //     } else {
        //         assert(false);
        //     }
        // }

        Size_Job job = { .ast_node = node };
        queue_enqueue(&resolver->size_jobs, job);
    }

    void queue_bytecode_job(Resolver *resolver, AST_Declaration *decl)
    {
        assert(decl->kind == AST_Declaration_Kind::FUNCTION ||
               decl->kind == AST_Declaration_Kind::RUN ||
               decl->kind == AST_Declaration_Kind::VARIABLE ||
               decl->kind == AST_Declaration_Kind::CONSTANT);

        assert(!(decl->decl_flags & AST_DECL_FLAG_QUEUED_BYTECODE));

        // if (decl->kind == AST_Declaration_Kind::FUNCTION &&
        //     resolver->build_data->options->verbose) {
        //     printf("Queueing bytecode job for function: %s\n", decl->identifier->atom.data);
        // }

        Bytecode_Job job = { .decl = decl };
        queue_enqueue(&resolver->bytecode_jobs, job);

        decl->decl_flags |= AST_DECL_FLAG_QUEUED_BYTECODE;
    }

    void queue_run_job(Resolver *resolver, AST_Declaration *run_decl, Bytecode_Function *wrapper)
    {
        assert(run_decl->kind == AST_Declaration_Kind::RUN);

        Run_Job run_job = { .run_decl = run_decl, .wrapper = wrapper };
        queue_enqueue(&resolver->run_jobs, run_job);
    }

    void queue_llvm_job(Resolver *resolver, Bytecode_Function *bc_func)
    {
        LLVM_Job job = { .kind = LLVM_Job_Kind::FUNCTION };
        job.bc_func = bc_func;
        queue_enqueue(&resolver->llvm_jobs, job);
    }

    void queue_llvm_job(Resolver *resolver, Bytecode_Global_Info bc_global)
    {
        LLVM_Job job = { .kind = LLVM_Job_Kind::GLOBAL };
        job.bc_global = bc_global;
        queue_enqueue(&resolver->llvm_jobs, job);
    }

    bool try_parse_job(Resolver *resolver, Parse_Job *job)
    {
        Parsed_File epf = {};
        bool insert_epf = false;

        bool opt_verbose = resolver->build_data->options->verbose;

        if (opt_verbose) {
            printf("[RESOLVER] Parsing file: '%s'\n", job->module_path.data);
        }

        if (job->insert_entry_module) {
            insert_epf = true;

            if (opt_verbose) {
                printf("           ..Inserting entry module\n");
            }

            Lexed_File lf_entry = lexer_lex_file(&resolver->lexer, resolver->entry_module_path);

            if (!lf_entry.valid) return false;

            Token_Stream *ets = lexer_new_token_stream(resolver->allocator, &lf_entry);

            Atom entry_module_name = atom_get(&resolver->build_data->atom_table, "entry");
            epf = parser_parse_file(&resolver->parser, ets, entry_module_name);

            if (!epf.valid) return false;

            lexer_free_lexed_file(&resolver->lexer, &lf_entry);
            ets->free();
        }

        Lexed_File lexed_file = lexer_lex_file(&resolver->lexer, job->module_path);

        if (!lexed_file.valid) return false;

        Token_Stream *token_stream = lexer_new_token_stream(resolver->allocator, &lexed_file);

        Parsed_File parsed_file = {};

        Atom module_name = atom_get(&resolver->build_data->atom_table, job->module_name);

        if (insert_epf) {
            parsed_file = epf;
        } else {
            parsed_file_init(&resolver->parser, &parsed_file, module_name);
        }

        parser_parse_file(&resolver->parser, token_stream, &parsed_file, module_name);

        if (!parsed_file.valid) return false;

        if (resolver->build_data->options->print_parse_tree) parsed_file_print(&parsed_file);

        AST_Module *module_ast = ast_create_from_parsed_file(&resolver->ast_builder, &parsed_file,
                                                             resolver->global_scope);
        //
        // @CLEANUP: @TODO: @FIXME: This should only check for redeclarations
        //                           (later maybe other errors generated by the
        //                            ast builder)
        if (resolver->build_data->errors.count) return false;

        lexer_free_lexed_file(&resolver->lexer, &lexed_file);
        token_stream->free();
        parser_free_parsed_file(&resolver->parser, &parsed_file);

        auto index = resolver->parsed_modules.count;
        array_append(&resolver->parsed_modules, { job->module_path,
                                                  job->module_name,
                                                  module_ast });
        job->parsed_module_index = index;

        return true;
    }

    bool try_resolve_job(Resolver *resolver, Resolve_Job *job)
    {
        assert(job->ast_node->kind == AST_Node_Kind::DECLARATION);
        AST_Declaration *decl = static_cast<AST_Declaration *>(job->ast_node);

        // if (resolver->build_data->options->verbose) {
        //     if (decl->identifier) {
        //     printf("[RESOLVER] Trying to resolve declaration: '%s'\n",
        //            decl->identifier->atom.data);
        //     } else if (decl->kind == AST_Declaration_Kind::USING) {
        //         printf("[RESOLVER] Trying to resolve using: '");
        //         ast_print_expression(decl->using_decl.ident_expr, 0);
        //         printf("'\n");
        //     } else if (decl->kind == AST_Declaration_Kind::RUN) {
        //         printf("[RESOLVER] Trying to resolve #run: '");
        //         ast_print_expression(decl->run.expression, 0);
        //         printf("'\n");
        //     } else if (decl->kind == AST_Declaration_Kind::STATIC_IF) {
        //         printf("[RESOLVER] Trying to resolve static_if: '");
        //         ast_print_expression(decl->static_if.cond_expression, 0);
        //         printf("'\n");
        //     } else {
        //         assert(false);
        //     }
        // }

        assert(decl->flat);

        bool result = true;

        AST_Node *waiting_on = nullptr;
        int64_t was_waiting_on = decl->flat->waiting_on;

        for (int64_t i = decl->flat->waiting_on; i < decl->flat->nodes.count; i++) {
            AST_Node **p_node = decl->flat->nodes[i];
            AST_Node *node = *p_node;

            switch (node->kind) {
                case AST_Node_Kind::INVALID: assert(false);
                case AST_Node_Kind::MODULE: assert(false);

                case AST_Node_Kind::DECLARATION: {
                    auto decl = static_cast<AST_Declaration *>(node);
                    result = try_resolve_declaration(resolver, decl);
                    break;
                }

                case AST_Node_Kind::STATEMENT: {
                    auto stmt = static_cast<AST_Statement *>(node);
                    result = try_resolve_statement(resolver, stmt);
                    break;
                }

                case AST_Node_Kind::EXPRESSION: {
                    auto expr = static_cast<AST_Expression *>(node);
                    result = try_resolve_expression(resolver, expr);

                    if (decl->kind == AST_Declaration_Kind::FUNCTION &&
                        result &&
                        expr->kind == AST_Expression_Kind::CALL) {
                        auto callee = expr->call.callee_declaration;
                        assert(callee);
                        array_append_unique(&decl->function.called_functions, callee);
                    }
                    break;
                }

                case AST_Node_Kind::IDENTIFIER: assert(false);
                case AST_Node_Kind::SWITCH_CASE: assert(false);

                case AST_Node_Kind::TYPE_SPEC: {
                    auto ts = static_cast<AST_Type_Spec *>(node);
                    result = try_resolve_type_spec(resolver, ts);
                    break;
                }

                case AST_Node_Kind::TYPE: assert(false);
            }

            if (!result) {
                if (node->waiting_on) waiting_on = node->waiting_on;
                else waiting_on = node;

                decl->flat->waiting_on = i;

                if (i > was_waiting_on) resolver->progressed = true;
                break;
            }
        }

        if (!result && resolver->build_data->options->verbose) {
            assert(waiting_on);
            auto bfp = waiting_on->begin_file_pos;

            Atom name = { .data = "", .length = 0 };
            if (waiting_on->kind == AST_Node_Kind::DECLARATION) {
                name = static_cast<AST_Declaration *>(waiting_on)->identifier->atom;
            }

            printf("           ..Failed! (waiting on: '%s' '%s:%" PRIu64 ":%" PRIu64 "')\n",
                    name.data, bfp.file_name.data, bfp.line, bfp.column);

        }

        if (!result) {
            assert(waiting_on);
            job->ast_node->waiting_on = waiting_on;
        }

        return result;
    }

    bool try_size_job(Resolver *resolver, Size_Job *job)
    {
        if (job->ast_node->kind == AST_Node_Kind::EXPRESSION) {
            AST_Expression *expr = static_cast<AST_Expression *>(job->ast_node);
            return try_size_expression(resolver, expr);
        }

        assert(job->ast_node->kind == AST_Node_Kind::DECLARATION);
        AST_Declaration *decl = static_cast<AST_Declaration *>(job->ast_node);
        assert(decl->type ||
               decl->kind == AST_Declaration_Kind::IMPORT ||
               decl->kind == AST_Declaration_Kind::USING);
        assert(decl->flags & AST_NODE_FLAG_RESOLVED_ID);
        assert(decl->flags & AST_NODE_FLAG_TYPED);

        bool result = true;

        for (int64_t i = 0; i < decl->flat->nodes.count; i++) {
            AST_Node **p_node = decl->flat->nodes[i];
            AST_Node *node = *p_node;

            if (node->flags & AST_NODE_FLAG_SIZED) {
                continue;
            }

            switch (node->kind) {
                case AST_Node_Kind::INVALID: assert(false);
                case AST_Node_Kind::MODULE: assert(false);
                case AST_Node_Kind::IDENTIFIER: assert(false);

                case AST_Node_Kind::DECLARATION:
                {
                    AST_Declaration *decl = static_cast<AST_Declaration *>(node);
                    result = try_size_declaration(resolver, decl);
                    break;
                }

                case AST_Node_Kind::SWITCH_CASE: assert(false);

                case AST_Node_Kind::STATEMENT: {
                    AST_Statement *stmt = static_cast<AST_Statement *>(node);
                    result = try_size_statement(resolver, stmt);
                    break;
                }

                case AST_Node_Kind::EXPRESSION: {
                    AST_Expression *expr = static_cast<AST_Expression *>(node);
                    result = try_size_expression(resolver, expr);
                    break;
                }

                case AST_Node_Kind::TYPE_SPEC:
                {
                    AST_Type_Spec *ts = static_cast<AST_Type_Spec *>(node);
                    result = try_size_type_spec(resolver, ts);
                    break;
                }

                case AST_Node_Kind::TYPE: assert(false);
            }

            if (!result) assert(false);
        }

        return result;
    }

    bool try_resolve_declaration(Resolver *resolver, AST_Declaration *declaration)
    {
        switch (declaration->kind) {
            case AST_Declaration_Kind::INVALID: assert(false);

            case AST_Declaration_Kind::IMPORT: {

                assert(declaration->import.ast_module == nullptr);
                AST_Expression *ident_expr = declaration->import.ident_expression;
                assert(ident_expr->kind == AST_Expression_Kind::IDENTIFIER);

                Atom _module_name = ident_expr->identifier->atom;

                String module_name = string_ref(_module_name);
                String file_path = find_module_path(resolver, module_name);

                AST_Module *ast_module = nullptr;
                bool in_queue = queue_parse_job(resolver, module_name, file_path, &ast_module);
                if (in_queue) return false;

                assert(ast_module);
                declaration->import.ast_module = ast_module;

                declaration->flags |= AST_NODE_FLAG_RESOLVED_ID;
                declaration->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Declaration_Kind::USING: {
                AST_Expression *ident_expr = declaration->using_decl.ident_expr;
                assert(ident_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(ident_expr->flags & AST_NODE_FLAG_TYPED);

                AST_Declaration *import_decl = nullptr;
                if (ident_expr->kind == AST_Expression_Kind::IDENTIFIER) {
                    import_decl = ident_expr->identifier->declaration;
                } else {
                    assert(false);
                }
                assert(import_decl);

                assert(import_decl->kind == AST_Declaration_Kind::IMPORT);
                assert(import_decl->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(import_decl->flags & AST_NODE_FLAG_TYPED);

                AST_Module *module_to_import = import_decl->import.ast_module;
                Scope *scope_to_import = module_to_import->module_scope;

                Scope *current_scope = declaration->scope;
                assert(current_scope != scope_to_import);

                Scope_Block *scope_block = &scope_to_import->first_block;

                bool result = true;

                while (scope_block) {
                    for (int64_t i = 0; i < scope_block->decl_count; i++) {
                        AST_Declaration *decl = scope_block->declarations[i];
                        if (decl->identifier) {
                            auto redecl = scope_find_declaration(current_scope, decl->identifier);
                            if (redecl) {
                                zodiac_report_error(resolver->build_data,
                                                    Zodiac_Error_Kind::REDECLARATION,
                                                    decl->identifier,
                                                    "Redeclaration of identifier: '%s'",
                                                    decl->identifier->atom.data);

                                zodiac_report_info(resolver->build_data, redecl->identifier,
                                                   "Previous declaration was here");
                                result = false;
                            } else {
                                scope_add_declaration(current_scope, decl);
                            }
                        } else {
                            assert(false);
                        }
                    }

                    scope_block = scope_block->next_block;
                }

                assert(result);
                declaration->flags |= AST_NODE_FLAG_RESOLVED_ID;
                declaration->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Declaration_Kind::VARIABLE: {

                AST_Type_Spec *ts = nullptr;
                if (declaration->variable.type_spec) {
                    ts = declaration->variable.type_spec;
                    assert(ts->type);
                    assert(ts->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(ts->flags & AST_NODE_FLAG_TYPED);
                }

                AST_Expression **p_init_expr = &declaration->variable.init_expression;
                auto init_expr = *p_init_expr;

                if (init_expr) {
                    assert(init_expr->type);
                    assert(init_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(init_expr->flags & AST_NODE_FLAG_TYPED);
                }

                AST_Type *type = nullptr;

                if (ts) {
                    if (init_expr) {
                        if (ts->type != init_expr->type) {
                            if (is_valid_type_conversion(init_expr, ts->type)) {
                                do_type_conversion(resolver, p_init_expr, ts->type);
                            } else {
                                zodiac_report_error(resolver->build_data,
                                                    Zodiac_Error_Kind::MISMATCHING_TYPES,
                                                    init_expr,
                                                    "Mismatching types in variable declaration");
                                auto exp_str = ast_type_to_tstring(ts->type);
                                zodiac_report_info(resolver->build_data, ts,
                                                   "Expected type: %.*s",
                                                   (int)exp_str.length, exp_str.data);
                                auto got_str = ast_type_to_tstring(ts->type);
                                zodiac_report_info(resolver->build_data, init_expr,
                                                   "Given type: %.*s",
                                                   (int)got_str.length, got_str.data);
                                return false;
                            }
                        }
                    }

                    type = ts->type;

                } else {
                    type = init_expr->type;
                }

                assert(type);

                declaration->type = type;
                declaration->flags |= AST_NODE_FLAG_RESOLVED_ID;
                declaration->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Declaration_Kind::CONSTANT:
            {
                AST_Type_Spec *ts = declaration->constant.type_spec;
                if (ts) {
                    assert(ts->type);
                    assert(ts->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(ts->flags & AST_NODE_FLAG_TYPED);
                }

                AST_Type *type = nullptr;

                AST_Expression *init_expr = declaration->constant.init_expression;
                if (init_expr) {
                    if (!(init_expr->flags & AST_NODE_FLAG_RESOLVED_ID)) return false;
                    assert(init_expr->type);
                    assert(init_expr->flags & AST_NODE_FLAG_TYPED);

                    if (ts) {
                        if (ts->type != init_expr->type) {
                            if (is_valid_type_conversion(init_expr, ts->type)) {
                                 // Can we do a regular conversion here or do we
                                 //  need to do something special for constants?
                                assert(false);
                            } else {
                                zodiac_report_error(resolver->build_data,
                                                    Zodiac_Error_Kind::MISMATCHING_TYPES,
                                                    init_expr,
                                                "Mismatching type in initializer for constant");
                                auto exp_str = ast_type_to_tstring(ts->type);
                                zodiac_report_info(resolver->build_data, ts,
                                                   "Expected type: '%.*s'",
                                                   (int)exp_str.length, exp_str.data);
                                auto got_str = ast_type_to_tstring(init_expr->type);
                                zodiac_report_info(resolver->build_data, init_expr,
                                                   "Given type: '%.*s'",
                                                   (int)got_str.length, got_str.data);
                                return false;
                            }
                        }
                    }

                    type = init_expr->type;
                } else {
                    assert(declaration->decl_flags & AST_DECL_FLAG_IS_ENUM_MEMBER);
                    assert(ts);
                    type = ts->type;
                }

                assert(type);

                declaration->type = type;
                declaration->flags |= AST_NODE_FLAG_RESOLVED_ID;
                declaration->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Declaration_Kind::PARAMETER: {
                AST_Type_Spec *ts = declaration->parameter.type_spec;
                assert(ts->type);
                assert(ts->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(ts->flags & AST_NODE_FLAG_TYPED);

                declaration->type = ts->type;
                declaration->flags |= AST_NODE_FLAG_RESOLVED_ID;
                declaration->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Declaration_Kind::FUNCTION: {

#ifndef NDEBUG
                for (int64_t i = 0; i < declaration->function.parameter_declarations.count; i++) {
                    AST_Declaration *param_decl = declaration->function.parameter_declarations[i];
                    assert(param_decl->type);
                    assert(param_decl->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(param_decl->flags & AST_NODE_FLAG_TYPED);
                }
#endif

                AST_Type_Spec *func_ts = declaration->function.type_spec;
                assert(func_ts->type);
                assert(func_ts->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(func_ts->flags & AST_NODE_FLAG_TYPED);

                AST_Statement *body = declaration->function.body;
                if (body) {
                    assert(body->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(body->flags & AST_NODE_FLAG_TYPED);
                } else {
                    assert(declaration->decl_flags & AST_DECL_FLAG_FOREIGN);
                }

                declaration->type = func_ts->type;
                declaration->flags |= AST_NODE_FLAG_RESOLVED_ID;
                declaration->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Declaration_Kind::TYPE: assert(false);

            case AST_Declaration_Kind::TYPEDEF: {
                AST_Type_Spec *type_spec = declaration->typedef_decl.type_spec;
                assert(type_spec->type);
                assert(type_spec->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(type_spec->flags & AST_NODE_FLAG_TYPED);

                declaration->type = type_spec->type;
                declaration->flags |= AST_NODE_FLAG_RESOLVED_ID;
                declaration->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Declaration_Kind::STRUCTURE: {
                return try_resolve_struct_declaration(resolver, declaration);
                break;
            }

            case AST_Declaration_Kind::UNION:
            {
                return try_resolve_union_declaration(resolver, declaration);
                break;
            }

            case AST_Declaration_Kind::ENUM: {
                AST_Type_Spec *mem_ts = declaration->enum_decl.type_spec;
                assert(mem_ts->type);
                assert(mem_ts->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(mem_ts->flags & AST_NODE_FLAG_TYPED);

                AST_Type *mem_type = mem_ts->type;
                assert(mem_type->kind == AST_Type_Kind::INTEGER);

                bool all_members_resolved = true;

                for (int64_t i = 0; i < declaration->enum_decl.member_declarations.count; i++) {
                    auto mem_decl = declaration->enum_decl.member_declarations[i];
                    assert(mem_decl->kind == AST_Declaration_Kind::CONSTANT);
                    AST_Expression *init_expr = mem_decl->constant.init_expression;

                    if (init_expr) {
                        init_expr->infer_type_from = mem_type;

                        assert(init_expr->kind == AST_Expression_Kind::INTEGER_LITERAL ||
                               init_expr->kind == AST_Expression_Kind::IDENTIFIER);

                        if (!((init_expr->flags & AST_NODE_FLAG_RESOLVED_ID) &&
                              (init_expr->flags & AST_NODE_FLAG_TYPED))) {
                            if (!try_resolve_expression(resolver, init_expr)) {
                                all_members_resolved = false;
                                continue;
                            }
                        }

                        assert (init_expr->type == mem_type);

                        if (!((mem_decl->flags & AST_NODE_FLAG_RESOLVED_ID) &&
                              (mem_decl->flags & AST_NODE_FLAG_TYPED))) {
                            if (!try_resolve_declaration(resolver, mem_decl)) {
                                all_members_resolved = false;
                            }
                        }
                    } else {
                        mem_decl->flags |= AST_NODE_FLAG_RESOLVED_ID;
                        mem_decl->flags |= AST_NODE_FLAG_TYPED;
                        mem_decl->type = mem_type;
                    }
                }

                if (!all_members_resolved) return false;

                AST_Type *enum_type =
                    find_or_create_enum_type(resolver, declaration, mem_type,
                                             declaration->enum_decl.member_scope,
                                             declaration->scope);

                assert(enum_type->enum_type.unique_member_values.count == 0);
                array_init(resolver->allocator, &enum_type->enum_type.unique_member_values);

                Scope *member_scope = declaration->enum_decl.member_scope;

                int64_t next_value = 0;
                int64_t current_value = 0;

                for (int64_t i = 0; i < declaration->enum_decl.member_declarations.count; i++) {
                    auto mem_decl = declaration->enum_decl.member_declarations[i];

                    assert(mem_decl->type);
                    assert(mem_decl->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(mem_decl->flags & AST_NODE_FLAG_TYPED);
                    assert(mem_decl->type == mem_type);

                    if (mem_decl->decl_flags & AST_DECL_FLAG_ENUM_MEMBER_INTINIT) {
                        auto nv = const_interpret_expression(mem_decl->constant.init_expression);
                        next_value = nv.integer.s64 + 1;
                        current_value = nv.integer.s64;
                        mem_decl->type = enum_type;
                        mem_decl->constant.init_expression->type = enum_type;
                    } else if (mem_decl->decl_flags & AST_DECL_FLAG_ENUM_MEMBER_IDENTINIT) {

                        assert(mem_decl->kind == AST_Declaration_Kind::CONSTANT);
                        assert(mem_decl->constant.init_expression);

                        AST_Expression *id_init_expr = mem_decl->constant.init_expression;

                        auto nv = const_interpret_expression(id_init_expr);
                        next_value = nv.integer.s64 + 1;
                        current_value = nv.integer.s64;

                        mem_decl->type = enum_type;
                        mem_decl->constant.init_expression->type = enum_type;
                    } else {
                        assert(!mem_decl->constant.init_expression);
                        mem_decl->constant.init_expression =
                            ast_integer_literal_expression_new(resolver->allocator,
                                                               next_value, member_scope,
                                                               mem_decl->begin_file_pos,
                                                               mem_decl->end_file_pos);
                        if (!try_resolve_expression(resolver,
                                                    mem_decl->constant.init_expression)) {
                            assert(false);
                        }

                        mem_decl->type = enum_type;
                        mem_decl->constant.init_expression->type = enum_type;

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

                declaration->type = enum_type;
                declaration->flags |= AST_NODE_FLAG_RESOLVED_ID;
                declaration->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Declaration_Kind::POLY_TYPE: assert(false);

            case AST_Declaration_Kind::RUN: {
                AST_Expression *run_expr = declaration->run.expression;
                assert(run_expr->type);
                assert(run_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(run_expr->flags & AST_NODE_FLAG_TYPED);

                assert(run_expr->kind == AST_Expression_Kind::CALL);

                declaration->type = run_expr->type;
                declaration->flags |= AST_NODE_FLAG_RESOLVED_ID;
                declaration->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Declaration_Kind::STATIC_IF: {
                AST_Expression *cond_expr = declaration->static_if.cond_expression;
                assert(cond_expr->type);
                assert(cond_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(cond_expr->flags & AST_NODE_FLAG_TYPED);

                assert(cond_expr->type->kind == AST_Type_Kind::BOOL);

                Const_Value cv = const_interpret_expression(cond_expr);
                Array<AST_Declaration *> *decls_to_add = nullptr;
                if (cv.boolean) {
                    decls_to_add = &declaration->static_if.then_declarations;
                } else if (declaration->static_if.else_declarations.count) {
                    decls_to_add = &declaration->static_if.else_declarations;
                }

                if (decls_to_add) {
                    for (int64_t i = 0; i < decls_to_add->count; i++) {
                        AST_Declaration **then_decl = &(*decls_to_add)[i];
                        bool imported = resolver_import_from_static_if(resolver, *then_decl,
                                                                     declaration->scope);
                        if (!imported) return false;

                        ast_flatten_declaration(&resolver->ast_builder, then_decl);
                        queue_resolve_job(resolver, *then_decl);
                    }
                }

                declaration->type = Builtin::type_void;
                declaration->flags |= AST_NODE_FLAG_RESOLVED_ID;
                declaration->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Declaration_Kind::STATIC_ASSERT: {
                AST_Expression **p_cond_expr = &declaration->static_assert_decl.cond_expression;
                auto cond_expr = *p_cond_expr;

                assert(cond_expr->type);
                assert(cond_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(cond_expr->flags & AST_NODE_FLAG_TYPED);

                if (cond_expr->type->kind != AST_Type_Kind::BOOL) {
                    if (is_valid_type_conversion(cond_expr, Builtin::type_bool)) {
                        do_type_conversion(resolver, p_cond_expr, Builtin::type_bool);
                    } else {
                        zodiac_report_error(resolver->build_data,
                                            Zodiac_Error_Kind::MISMATCHING_TYPES,
                                            cond_expr,
                            "Argument to  static iassert is not of boolean type, and cannot be converted to boolean type");
                        return false;
                    }
                }

                Const_Value cv = const_interpret_expression(cond_expr);

                if (!cv.boolean) {
                    zodiac_report_error(resolver->build_data,
                                        Zodiac_Error_Kind::STATIC_ASSERTION_FAILED,
                                        declaration, "Static assert failed!");
                    return false;
                }

                declaration->type = Builtin::type_void;
                declaration->flags |= AST_NODE_FLAG_RESOLVED_ID;
                declaration->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }
        }

        assert(false);
        return false;
    }

    bool try_resolve_struct_declaration(Resolver *resolver, AST_Declaration *declaration)
    {
        assert(declaration->kind == AST_Declaration_Kind::STRUCTURE);
        assert(declaration->structure.parameters.count == 0);

        if (!declaration->type) {
            declaration->type = ast_structure_type_new(resolver->allocator, declaration,
                                                       declaration->structure.member_scope);
            array_append(&resolver->build_data->type_table, declaration->type);
        } else {
            assert(declaration->type->kind == AST_Type_Kind::STRUCTURE);
        }

        bool all_members_resolved = true;

        for (int64_t i = 0; i < declaration->structure.member_declarations.count; i++) {
            AST_Declaration **p_member_decl = &declaration->structure.member_declarations[i];
            auto member_decl = *p_member_decl;

            if (!member_decl->flat) {
                ast_flatten_declaration(&resolver->ast_builder, p_member_decl);
                queue_resolve_job(resolver, member_decl);
                all_members_resolved = false;

            } else if (!((member_decl->flags & AST_NODE_FLAG_RESOLVED_ID) &&
                         (member_decl->flags & AST_NODE_FLAG_TYPED))) {
                all_members_resolved = false;
                break;
            }

        }

        if (!all_members_resolved) return false;

        for (int64_t i = 0; i < declaration->structure.member_declarations.count; i++) {
            AST_Declaration *member_decl = declaration->structure.member_declarations[i];

            assert(member_decl->type);
            assert(member_decl->kind == AST_Declaration_Kind::VARIABLE);

            array_append(&declaration->type->structure.member_types, member_decl->type);
        }

        declaration->flags |= AST_NODE_FLAG_RESOLVED_ID;
        declaration->flags |= AST_NODE_FLAG_TYPED;

        declaration->type->flags |= AST_NODE_FLAG_RESOLVED_ID;
        declaration->type->flags |= AST_NODE_FLAG_TYPED;
        return true;
    }

    bool try_resolve_union_declaration(Resolver *resolver, AST_Declaration *declaration)
    {
        assert(declaration->kind == AST_Declaration_Kind::UNION);
        assert(declaration->union_decl.parameters.count == 0);

        if (!declaration->type) {
            declaration->type = ast_union_type_new(resolver->allocator, declaration,
                                                   declaration->union_decl.member_scope);
            array_append(&resolver->build_data->type_table, declaration->type);
        } else {
            assert(declaration->type->kind == AST_Type_Kind::UNION);
        }

        bool all_members_resolved = true;

        for (int64_t i = 0; i < declaration->union_decl.member_declarations.count; i++) {
            AST_Declaration **p_member_decl = &declaration->union_decl.member_declarations[i];
            auto member_decl = *p_member_decl;

            if (!member_decl->flat) {
                ast_flatten_declaration(&resolver->ast_builder, p_member_decl);
                queue_resolve_job(resolver, member_decl);
                all_members_resolved = false;

            } else if (!((member_decl->flags & AST_NODE_FLAG_RESOLVED_ID) &&
                         (member_decl->flags & AST_NODE_FLAG_TYPED))) {
                all_members_resolved = false;
                break;
            }

        }

        if (!all_members_resolved) return false;

        for (int64_t i = 0; i < declaration->union_decl.member_declarations.count; i++) {
            AST_Declaration *member_decl = declaration->union_decl.member_declarations[i];

            assert(member_decl->type);
            assert(member_decl->kind == AST_Declaration_Kind::VARIABLE);

            array_append(&declaration->type->union_type.member_types, member_decl->type);
        }

        declaration->flags |= AST_NODE_FLAG_RESOLVED_ID;
        declaration->flags |= AST_NODE_FLAG_TYPED;

        declaration->type->flags |= AST_NODE_FLAG_RESOLVED_ID;
        declaration->type->flags |= AST_NODE_FLAG_TYPED;
        return true;
    }

    bool try_resolve_statement(Resolver *resolver, AST_Statement *statement)
    {
        switch (statement->kind) {
            case AST_Statement_Kind::INVALID: assert(false);

            case AST_Statement_Kind::BLOCK: {
                for (int64_t i = 0; i < statement->block.statements.count; i++) {
                    auto mem_stmt = statement->block.statements[i];
                    if (!(mem_stmt->flags &  AST_NODE_FLAG_RESOLVED_ID))
                        return false;
                    assert(mem_stmt->flags & AST_NODE_FLAG_TYPED);
                }

                statement->flags |= AST_NODE_FLAG_RESOLVED_ID;
                statement->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Statement_Kind::ASSIGNMENT: {
                AST_Expression *ident_expr = statement->assignment.identifier_expression;

                AST_Expression **p_rhs_expr = &statement->assignment.rhs_expression;
                auto rhs_expr = *p_rhs_expr;
#ifndef NDEBUG
                assert(ident_expr->type);
                assert(ident_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(ident_expr->flags & AST_NODE_FLAG_TYPED);


                assert(rhs_expr->type);
                assert(rhs_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(rhs_expr->flags & AST_NODE_FLAG_TYPED);
#endif

                if (ident_expr->type != rhs_expr->type) {
                    if (is_valid_type_conversion(rhs_expr, ident_expr->type)) {
                        do_type_conversion(resolver, p_rhs_expr, ident_expr->type);
                    } else {
                        zodiac_report_error(resolver->build_data,
                                            Zodiac_Error_Kind::MISMATCHING_TYPES, rhs_expr,
                                            "Mismatching types in assignment statement");
                        auto exp_str = ast_type_to_tstring(ident_expr->type);
                        zodiac_report_info(resolver->build_data, ident_expr,
                                           "Expected type: '%.*s'",
                                           (int)exp_str.length, exp_str.data);
                        auto got_str = ast_type_to_tstring(rhs_expr->type);
                        zodiac_report_info(resolver->build_data, rhs_expr,
                                           "Given type: '%.*s'",
                                           (int)got_str.length, got_str.data);
                        return false;
                    }
                }

                statement->flags |= AST_NODE_FLAG_RESOLVED_ID;
                statement->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Statement_Kind::RETURN: {
                AST_Expression **p_operand_expr = &statement->expression;
                auto operand_expr = *p_operand_expr;

                AST_Declaration *func = enclosing_function(resolver, statement);
                AST_Type_Spec *func_ts = func->function.type_spec;
                assert(func_ts);
                AST_Type *func_type = func_ts->type;
                assert(func_type);
                AST_Type *ret_type = func_type->function.return_type;

                if (operand_expr) {
                    assert(operand_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(operand_expr->flags & AST_NODE_FLAG_TYPED);
                    assert(operand_expr->type);

                    if (operand_expr->type != ret_type) {
                        if (is_valid_type_conversion(operand_expr, ret_type)) {
                            do_type_conversion(resolver, p_operand_expr, ret_type);
                        }  else {
                            zodiac_report_error(resolver->build_data,
                                                Zodiac_Error_Kind::MISMATCHING_TYPES,
                                                operand_expr,
                                                "Mismatching type in return statement");
                            auto exp_str = ast_type_to_tstring(ret_type);
                            zodiac_report_info(resolver->build_data,
                                               func_ts->function.return_type_spec,
                                               "Expected type: '%.*s'",
                                               (int)exp_str.length, exp_str.data);
                            auto given_str = ast_type_to_tstring(operand_expr->type);
                            zodiac_report_info(resolver->build_data, operand_expr,
                                               "Given type: '%.*s'",
                                               (int)given_str.length, given_str.data);
                            return false;
                        }
                    }
                } else {
                    assert(ret_type == Builtin::type_void);
                }

                statement->flags |= AST_NODE_FLAG_RESOLVED_ID;
                statement->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Statement_Kind::BREAK: {
#ifndef NDEBUG
                AST_Statement *break_from = statement->break_stmt.break_from;
                assert(break_from);
                assert(break_from->kind == AST_Statement_Kind::WHILE ||
                       break_from->kind == AST_Statement_Kind::SWITCH);
#endif

                statement->flags |= AST_NODE_FLAG_RESOLVED_ID;
                statement->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Statement_Kind::DECLARATION: {
#ifndef NDEBUG
                AST_Declaration *decl = statement->declaration;
                assert(decl->type);
                assert(decl->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(decl->flags & AST_NODE_FLAG_TYPED);
#endif

                statement->flags |= AST_NODE_FLAG_RESOLVED_ID;
                statement->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Statement_Kind::EXPRESSION: {
#ifndef NDEBUG
                AST_Expression *expr = statement->expression;
                assert(expr->type);
                assert(expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(expr->flags & AST_NODE_FLAG_TYPED);
#endif

                statement->flags |= AST_NODE_FLAG_RESOLVED_ID;
                statement->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Statement_Kind::WHILE: {
#ifndef NDEBUG
                AST_Expression **p_cond_expr = &statement->while_stmt.cond_expr;
                auto cond_expr = *p_cond_expr;

                assert(cond_expr->type);
                assert(cond_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(cond_expr->flags & AST_NODE_FLAG_TYPED);

                if (!(cond_expr->type->kind == AST_Type_Kind::BOOL)) {
                    if (is_valid_type_conversion(cond_expr->type, Builtin::type_bool)) {
                        do_type_conversion(resolver, p_cond_expr, Builtin::type_bool);
                    } else {
                        zodiac_report_error(resolver->build_data,
                                            Zodiac_Error_Kind::MISMATCHING_TYPES,
                                            cond_expr,
                            "Conditional expression of while statement is not of boolean type, and cannot be converted to boolean type");
                        return false;
                    }
                }

                AST_Statement *body = statement->while_stmt.body;
                assert(body->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(body->flags & AST_NODE_FLAG_TYPED);
#endif

                statement->flags |= AST_NODE_FLAG_RESOLVED_ID;
                statement->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Statement_Kind::FOR:
            {
#ifndef NDEBUG
                for (int64_t i = 0; i < statement->for_stmt.init_statements.count; i++) {
                    AST_Statement *init_stmt = statement->for_stmt.init_statements[i];
                    assert(init_stmt->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(init_stmt->flags & AST_NODE_FLAG_TYPED);
                }

                if (statement->for_stmt.it_decl) {
                    assert(statement->for_stmt.it_decl->type);
                    assert(statement->for_stmt.it_decl->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(statement->for_stmt.it_decl->flags & AST_NODE_FLAG_TYPED);
                }

                AST_Expression **p_cond_expr = &statement->for_stmt.cond_expr;
                auto cond_expr = *p_cond_expr;

                assert(cond_expr->type);
                assert(cond_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(cond_expr->flags & AST_NODE_FLAG_TYPED);

                if (cond_expr->type != Builtin::type_bool) {
                    if (is_valid_type_conversion(cond_expr, Builtin::type_bool)) {
                        do_type_conversion(resolver, p_cond_expr, Builtin::type_bool);
                    } else {
                        zodiac_report_error(resolver->build_data,
                                            Zodiac_Error_Kind::MISMATCHING_TYPES,
                                            cond_expr,
                            "Conditional expression of for statement is not of boolean type, and cannot be converted to boolean type");
                        return false;
                    }

                }

                for (int64_t i = 0; i < statement->for_stmt.step_statements.count; i++) {
                    AST_Statement *step_stmt = statement->for_stmt.step_statements[i];
                    assert(step_stmt->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(step_stmt->flags & AST_NODE_FLAG_TYPED);
                }
#endif

                assert(statement->for_stmt.body_stmt->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(statement->for_stmt.body_stmt->flags & AST_NODE_FLAG_TYPED);

                statement->flags |= AST_NODE_FLAG_RESOLVED_ID;
                statement->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Statement_Kind::IF: {
                AST_Expression **p_cond_expr = &statement->if_stmt.cond_expr;
                auto cond_expr = *p_cond_expr;

#ifndef NDEBUG
                assert(cond_expr->type);
                assert(cond_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(cond_expr->flags & AST_NODE_FLAG_TYPED);

                AST_Statement *then_stmt = statement->if_stmt.then_stmt;
                assert(then_stmt->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(then_stmt->flags & AST_NODE_FLAG_TYPED);

                AST_Statement *else_stmt = statement->if_stmt.else_stmt;
                if (else_stmt) {
                    assert(else_stmt->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(else_stmt->flags & AST_NODE_FLAG_TYPED);
                }
#endif

                if (!(cond_expr->type->kind == AST_Type_Kind::BOOL)) {
                    if (is_valid_type_conversion(cond_expr, Builtin::type_bool)) {
                        do_type_conversion(resolver, p_cond_expr, Builtin::type_bool);
                    } else {
                        zodiac_report_error(resolver->build_data,
                                            Zodiac_Error_Kind::MISMATCHING_TYPES,
                                            cond_expr,
                            "Conditional expression of if statement is not of boolean type, and cannot be converted to boolean type");
                        return false;
                    }
                }


                statement->flags |= AST_NODE_FLAG_RESOLVED_ID;
                statement->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Statement_Kind::SWITCH: {
#ifndef NDEBUG
                AST_Expression *switch_expr = statement->switch_stmt.expression;
                assert(switch_expr->type);
                assert(switch_expr->flags  & AST_NODE_FLAG_RESOLVED_ID);
                assert(switch_expr->flags  & AST_NODE_FLAG_TYPED);
#endif

                assert(switch_expr->type->kind == AST_Type_Kind::INTEGER ||
                       switch_expr->type->kind == AST_Type_Kind::ENUM);

                for (int64_t i = 0; i < statement->switch_stmt.cases.count; i++) {
                    AST_Switch_Case *switch_case = statement->switch_stmt.cases[i];
                    int64_t range_count = 0;

                    // assert(switch_case->flags & AST_NODE_FLAG_RESOLVED_ID);
                    // assert(switch_case->flags & AST_NODE_FLAG_TYPED);

                    assert(switch_case->body->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(switch_case->body->flags & AST_NODE_FLAG_TYPED);

                    if (switch_case->is_default) continue;

                    auto el = bucket_array_first(&switch_case->expressions);
                    while (el.bucket) {
                        auto case_expr = *bucket_locator_get_ptr(el);
                        assert(case_expr->type);
                        assert(case_expr->type == switch_expr->type);
                        assert(case_expr->expr_flags & AST_EXPR_FLAG_CONST);
                        assert(case_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                        assert(case_expr->flags & AST_NODE_FLAG_TYPED);

                        if (case_expr->kind == AST_Expression_Kind::RANGE) {
                            range_count++;
                        }

                        bucket_locator_advance(&el);
                    }

                    if (range_count) {
                        resolver_expand_switch_case_ranges(resolver, statement,
                                                           switch_case, range_count,
                                                           statement->scope);
                    }
                }

                if (!statement->switch_stmt.allow_incomplete &&
                    !resolver_check_switch_completeness(resolver, statement)) {
                    return false;
                }

                statement->flags |= AST_NODE_FLAG_RESOLVED_ID;
                statement->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }
        }

        assert(false);
        return false;
    }

    bool try_resolve_expression(Resolver *resolver, AST_Expression *expression)
    {
        bool allow_null_type = false;

        switch (expression->kind) {
            case AST_Expression_Kind::INVALID: assert(false);

            case AST_Expression_Kind::IDENTIFIER: {
                auto decl = scope_find_declaration(expression->scope, expression->identifier);
                if (!decl) {
                    zodiac_report_error(resolver->build_data,
                                        Zodiac_Error_Kind::UNDECLARED_IDENTIFIER,
                                        expression->identifier,
                                        "Reference to undeclared identifier '%s'",
                                        expression->identifier->atom.data);
                    return false;
                }

                bool recursive = false;
                bool use_fn_ts = false;

                if (!(decl->flags & AST_NODE_FLAG_RESOLVED_ID)) {
                    if (decl->kind == AST_Declaration_Kind::FUNCTION) {
                        AST_Declaration *current_function = enclosing_function(resolver,
                                                                               expression);
                        if (current_function && current_function == decl) {
                            // Recursive call or reference
                            recursive = true;
                            expression->expr_flags |= AST_EXPR_FLAG_RECURSIVE_IDENT;
                        } else if (decl->function.type_spec->type) {
#ifndef NDEBUG
                            auto ts = decl->function.type_spec;
#endif
                            assert(ts->flags & AST_NODE_FLAG_RESOLVED_ID);
                            assert(ts->flags & AST_NODE_FLAG_TYPED);
                            use_fn_ts = true;
                            expression->expr_flags |= AST_EXPR_FLAG_IDENT_USES_FN_TS;
                        } else {
                            expression->waiting_on = decl;
                            return false;
                        }
                    }
                }

                if (!recursive && !use_fn_ts && !(decl->flags & AST_NODE_FLAG_TYPED)) {
                    return false;
                }

                if (decl->kind == AST_Declaration_Kind::IMPORT) {
                    allow_null_type = true;
                } else if (recursive || use_fn_ts) {
                    if (decl->function.type_spec->type) {
                        expression->type = decl->function.type_spec->type;
                    } else {
                        assert(false);
                    }
                } else {
                    assert(decl->type);
                    expression->type = decl->type;
                }

                expression->identifier->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->identifier->flags |= AST_NODE_FLAG_TYPED;

                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                break;
            }

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);

            case AST_Expression_Kind::DOT: {
                AST_Expression *parent_expr = expression->dot.parent_expression;
                if (!(parent_expr->flags & AST_NODE_FLAG_RESOLVED_ID)) return false;
                assert(parent_expr->flags & AST_NODE_FLAG_TYPED);

                AST_Declaration *parent_decl = nullptr;
                if (parent_expr->kind == AST_Expression_Kind::IDENTIFIER) {
                    parent_decl = parent_expr->identifier->declaration;
                } else if (parent_expr->kind == AST_Expression_Kind::DOT) {
                    parent_decl = parent_expr->dot.child_decl;
                }
                assert(parent_decl);

                AST_Identifier *child_ident = expression->dot.child_identifier;

                if (parent_decl->kind == AST_Declaration_Kind::IMPORT) {
                    assert(parent_decl->flags  & AST_NODE_FLAG_RESOLVED_ID);
                    assert(parent_decl->flags  & AST_NODE_FLAG_TYPED);

                    AST_Module *ast_module = parent_decl->import.ast_module;
                    assert(ast_module);
                    // assert(ast_module->flags & AST_NODE_FLAG_RESOLVED_ID);
                    // assert(ast_module->flags & AST_NODE_FLAG_TYPED);

                    AST_Declaration *child_decl = scope_find_declaration(ast_module->module_scope,
                                                                         child_ident);
                    if (!child_decl) {
                        zodiac_report_error(resolver->build_data,
                                            Zodiac_Error_Kind::UNDECLARED_IDENTIFIER,
                                            child_ident,
                                            "Reference to undeclared identifier: '%s'",
                                            child_ident->atom.data);
                        zodiac_report_info(resolver->build_data, parent_decl,
                                           "In module: '%s'\n",
                                           ast_module->name.data);

                        return false;
                    }

                    if (!(child_decl->flags & AST_NODE_FLAG_RESOLVED_ID)) {
                        return false;
                    }
                    assert(child_decl->type);
                    assert(child_decl->flags & AST_NODE_FLAG_TYPED);

                    expression->dot.child_decl = child_decl;
                    expression->type = child_decl->type;
                    expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                    expression->flags |= AST_NODE_FLAG_TYPED;
                    break;
                } else {
                    assert(parent_decl->type);
                    assert(parent_decl->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(parent_decl->flags & AST_NODE_FLAG_TYPED);

                    AST_Type *parent_type = parent_decl->type;
                    if (parent_type->kind == AST_Type_Kind::ARRAY) {
                        if (child_ident->atom == Builtin::atom_count) {
                            expression->expr_flags |= AST_EXPR_FLAG_DOT_COUNT;
                            expression->type = Builtin::type_s64;
                            expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                            expression->flags |= AST_NODE_FLAG_TYPED;
                            break;
                        } else {
                            assert(false);
                        }
                    } else if (parent_type->kind == AST_Type_Kind::ENUM) {

                        Scope *enum_scope = parent_type->enum_type.member_scope;

                        AST_Declaration *child_decl = nullptr;

                        if (child_ident->declaration) child_decl = child_ident->declaration;
                        else child_decl = scope_find_declaration(enum_scope, child_ident);

                        // The parent should be fully resolved (the enum declaration),
                        //  so we know that scope_find_declaration can only fail with
                        //  an actual undeclared identifier.
                        if (!child_decl) {
                            zodiac_report_error(resolver->build_data,
                                                Zodiac_Error_Kind::UNDECLARED_IDENTIFIER,
                                                child_ident,
                                                "Reference to undeclared identifier: '%s'",
                                                child_ident->atom.data);
                            zodiac_report_info(resolver->build_data, child_ident,
                                               "'%s' is not a member of enum '%s'",
                                               child_ident->atom.data,
                                               parent_decl->identifier->atom.data);
                            zodiac_report_info(resolver->build_data, parent_decl,
                                               "See declaration of enum '%s'",
                                               parent_decl->identifier->atom.data);
                            return false;
                        }

                        if (!(child_decl->flags & AST_NODE_FLAG_RESOLVED_ID)) return false;
                        assert(child_decl->type);
                        assert(child_decl->flags & AST_NODE_FLAG_TYPED);

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

                        expression->dot.child_decl = child_ident->declaration;
                        expression->type = child_decl->type;
                        expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                        expression->flags |= AST_NODE_FLAG_TYPED;
                        break;
                    } else {
                        if (parent_type->kind != AST_Type_Kind::STRUCTURE &&
                            parent_type->kind != AST_Type_Kind::UNION) {
                            assert(parent_type->kind == AST_Type_Kind::POINTER);
                            parent_type = parent_type->pointer.base;
                            assert(parent_type->kind == AST_Type_Kind::STRUCTURE ||
                                   parent_type->kind == AST_Type_Kind::UNION);
                        }

                        assert(parent_type);
                        if (!(parent_type->flags & AST_NODE_FLAG_RESOLVED_ID)) {
                            return false;
                        }
                        assert(parent_type->flags & AST_NODE_FLAG_TYPED);

                        assert(parent_type->structure.member_scope);
                        auto mem_scope = parent_type->structure.member_scope;
                        assert(mem_scope->kind == Scope_Kind::AGGREGATE);

                        AST_Declaration *child_decl = nullptr;
                        if (!child_ident->declaration) {
                            child_decl = scope_find_declaration(mem_scope, child_ident);
                            assert(child_decl);
                            assert(child_decl->type);
                            assert(child_decl->flags & AST_NODE_FLAG_RESOLVED_ID);
                            assert(child_decl->flags & AST_NODE_FLAG_TYPED);

                            AST_Declaration *aggregate_decl = nullptr;
                            Array<AST_Declaration *> member_decls = {};
                            if (parent_type->kind == AST_Type_Kind::STRUCTURE) {
                                aggregate_decl = parent_type->structure.declaration;
                                member_decls = aggregate_decl->structure.member_declarations;
                            } else {
                                assert(parent_type->kind == AST_Type_Kind::UNION);
                                aggregate_decl = parent_type->union_type.declaration;
                                member_decls = aggregate_decl->union_decl.member_declarations;
                            }
                            assert(aggregate_decl);
                            assert(aggregate_decl->kind == AST_Declaration_Kind::STRUCTURE ||
                                   aggregate_decl->kind == AST_Declaration_Kind::UNION);

                            bool index_found = false;
                            int64_t index = -1;
                            for (int64_t i = 0; i < member_decls.count; i++) {

                                if (child_decl == member_decls[i]) {
                                    assert(!index_found);
                                    index_found = true;
                                    index = i;
                                    break;
                                }
                            }
                            assert(index_found);
                            assert(index >= 0);

                            expression->dot.child_index = index;
                            expression->dot.child_decl = child_decl;
                            expression->type = child_decl->type;
                            expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                            expression->flags |= AST_NODE_FLAG_TYPED;
                            break;

                        } else {
                            assert(false);
                        }

                        assert(false);
                    }
                }

                assert(false);
                break;
            }

            case AST_Expression_Kind::BINARY: {
                AST_Expression **p_lhs = &expression->binary.lhs;
                auto lhs = *p_lhs;
                assert(lhs->type);
                assert(lhs->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(lhs->flags & AST_NODE_FLAG_TYPED);

                AST_Expression **p_rhs = &expression->binary.rhs;
                auto rhs = *p_rhs;
                assert(rhs->type);
                assert(rhs->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(rhs->flags & AST_NODE_FLAG_TYPED);

                bool is_compare = binop_is_cmp(expression->binary.op);

                AST_Type *result_type = nullptr;

                if (lhs->type == rhs->type) {
                    if (!is_compare) result_type = lhs->type;
                } else {

                    bool lhs_int_lit = lhs->kind == AST_Expression_Kind::INTEGER_LITERAL;
                    bool rhs_int_lit = rhs->kind == AST_Expression_Kind::INTEGER_LITERAL;

                    bool lhs_float_lit = lhs->kind == AST_Expression_Kind::FLOAT_LITERAL;
                    bool rhs_float_lit = rhs->kind == AST_Expression_Kind::FLOAT_LITERAL;



                    bool valid = true;

#define MAYBE_CONVERT(p_source, dest) \
if (is_valid_type_conversion(*(p_source), (dest)->type)) { \
    result_type = do_type_conversion(resolver, (p_source), (dest)->type); \
} else valid = false; \


                    // Integer literal
                    if (lhs_int_lit && !rhs_int_lit) {
                        MAYBE_CONVERT(p_lhs, rhs);
                    } else if (rhs_int_lit && !lhs_int_lit) {
                        MAYBE_CONVERT(p_rhs, lhs);
                    } else if (lhs_int_lit && rhs_int_lit) {
                        assert(false);

                    // Float literal
                    } else if (lhs_float_lit && !rhs_float_lit) {
                        MAYBE_CONVERT(p_lhs, rhs);
                    } else if (rhs_float_lit && !lhs_float_lit) {
                        MAYBE_CONVERT(p_rhs, lhs);
                    } else if (lhs_float_lit && rhs_float_lit) {
                        assert(false);
                    } else {

                        // Anything else...
                        if (is_valid_type_conversion(lhs, rhs->type)) {
                            result_type = do_type_conversion(resolver, p_lhs, rhs->type);
                        } else if (is_valid_type_conversion(rhs, lhs->type)) {
                            result_type = do_type_conversion(resolver, p_rhs, lhs->type);
                        } else {
                            valid = false;
                        }
                    }

                    if (!valid) {
                        zodiac_report_error(resolver->build_data,
                                            Zodiac_Error_Kind::MISMATCHING_TYPES, expression,
                                            "Mismatching types in binary expression:");
                        auto lhs_type_str = ast_type_to_tstring(lhs->type);
                        zodiac_report_info(resolver->build_data, lhs, "Left type: '%.*s'",
                                           (int)lhs_type_str.length, lhs_type_str.data);
                        auto rhs_type_str = ast_type_to_tstring(rhs->type);
                        zodiac_report_info(resolver->build_data, rhs, "Right type: '%.*s'\n",
                                           (int)rhs_type_str.length, rhs_type_str.data);
                        return false;
                    } else {
                        assert(result_type);
                    }
                }

#undef MAYBE_CONVERT

                lhs = *p_lhs;
                rhs = *p_rhs;


                if (is_compare) {
                    if (result_type) {
                        assert(result_type == lhs->type || result_type == rhs->type);
                        if (result_type->kind != AST_Type_Kind::BOOL) {
                            result_type = Builtin::type_bool;
                        }
                    } else {
                        result_type = Builtin::type_bool;
                    }
                } else {
                    assert(result_type);
                    assert(result_type == lhs->type || result_type == rhs->type);
                }
                assert(result_type);

                expression->type = result_type;
                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                break;
            }

            case AST_Expression_Kind::UNARY: {
                AST_Expression *op_expr = expression->unary.operand_expression;
                assert(op_expr->type);
                assert(op_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(op_expr->flags & AST_NODE_FLAG_TYPED);

                AST_Type *result_type = nullptr;

                switch (expression->unary.op) {
                    case UNOP_INVALID: assert(false);

                    case UNOP_DEREF: {
                        assert(op_expr->type->kind == AST_Type_Kind::POINTER);
                        result_type = op_expr->type->pointer.base;
                        break;
                    }

                    case UNOP_MINUS: {
                        if (op_expr->type->kind == AST_Type_Kind::INTEGER) {
                            assert(op_expr->type->integer.sign);
                            result_type = op_expr->type;
                        } else if (op_expr->type->kind == AST_Type_Kind::FLOAT) {
                            result_type = op_expr->type;
                        } else {
                            assert(false);
                        }
                        break;
                    }

                    case UNOP_NOT: {
#ifndef NDEBUG
                        auto tk = op_expr->type->kind;
                        assert(tk == AST_Type_Kind::BOOL ||
                               tk == AST_Type_Kind::INTEGER ||
                               tk == AST_Type_Kind::POINTER);
#endif
                        result_type = Builtin::type_bool;;
                        break;
                    }
                }

                assert(result_type);

                expression->type = result_type;
                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                break;
            }

            case AST_Expression_Kind::CALL: {

                AST_Expression *ident_expr = expression->call.ident_expression;
                assert(ident_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(ident_expr->flags & AST_NODE_FLAG_TYPED);

                AST_Declaration *callee_decl = nullptr;
                if (ident_expr->kind == AST_Expression_Kind::IDENTIFIER) {
                    callee_decl = ident_expr->identifier->declaration;
                } else if (ident_expr->kind == AST_Expression_Kind::DOT) {
                    callee_decl = ident_expr->dot.child_decl;
                }
                assert(callee_decl);

                bool recursive = false;
                bool use_fn_ts = false;

                AST_Type *func_type = callee_decl->type;
                if (!func_type) {
                    if (ident_expr->expr_flags & AST_EXPR_FLAG_RECURSIVE_IDENT) {
                        recursive = true;
                    } else if (ident_expr->expr_flags & AST_EXPR_FLAG_IDENT_USES_FN_TS) {
                        use_fn_ts = true;
                    } else {
                        assert(false);
                    }
                    assert(callee_decl->function.type_spec->type);
                    func_type = callee_decl->function.type_spec->type;
                }
                assert(func_type);

                assert(recursive || use_fn_ts ||(callee_decl->flags & AST_NODE_FLAG_RESOLVED_ID));
                assert(recursive || use_fn_ts ||(callee_decl->flags & AST_NODE_FLAG_TYPED));

                if (recursive) {
#ifndef NDEBUG
                    auto ts = callee_decl->function.type_spec;
#endif
                    assert(ts->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(ts->flags & AST_NODE_FLAG_TYPED);
                }

                if (!(callee_decl->decl_flags & AST_DECL_FLAG_REGISTERED_BYTECODE)) {
                    bytecode_register_function(&resolver->bytecode_builder, callee_decl);
                }

                expression->call.callee_declaration = callee_decl;

                auto args = expression->call.arg_expressions;
                auto params = callee_decl->function.parameter_declarations;

                assert(args.count == params.count);
                for (int64_t i = 0; i < args.count; i++) {
                    AST_Expression **p_arg_expr = &args[i];
                    auto arg_expr = *p_arg_expr;
                    assert(arg_expr->type);
                    if (arg_expr->type != params[i]->type) {

                        if (is_valid_type_conversion(arg_expr, params[i]->type)) {
                            do_type_conversion(resolver, p_arg_expr, params[i]->type);
                        } else {
                            resolver_report_mismatching_call_arg(resolver, i, arg_expr,
                                                                 params[i]);
                            return false;
                        }
                    }
                }

                expression->type = func_type->function.return_type;
                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                break;
            }

            case AST_Expression_Kind::BUILTIN_CALL: {

                auto args = expression->builtin_call.arg_expressions;

#ifdef NDEBUG
#define _ENSURE_ARGS_ARE_TYPED
#else
#define _ENSURE_ARGS_ARE_TYPED \
    for (int64_t i = 0; i < args.count; i++) { \
        AST_Expression *arg_expr = args[i]; \
        assert(arg_expr->type); \
        assert(arg_expr->flags |= AST_NODE_FLAG_RESOLVED_ID); \
        assert(arg_expr->flags |= AST_NODE_FLAG_TYPED); \
    }
#endif
                Atom name = expression->builtin_call.identifier->atom;

                if (name == Builtin::atom_exit) {
                    _ENSURE_ARGS_ARE_TYPED
                    assert(args.count == 1);
                    if (args[0]->type != Builtin::type_s64) {
                        if (is_valid_type_conversion(args[0], Builtin::type_s64)) {
                            do_type_conversion(resolver, &args[0], Builtin::type_s64);
                        } else  {
                            resolver_report_mismatching_call_arg(resolver, 0, args[0],
                                                                 Builtin::type_s64, true);
                            return false;
                        }
                    }

                    expression->type = Builtin::type_void;
                    expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                    expression->flags |= AST_NODE_FLAG_TYPED;
                    return true;

                } else if (name == Builtin::atom_syscall) {
                    _ENSURE_ARGS_ARE_TYPED
                    assert(args.count >= 1);

                    for (int64_t i = 0; i < args.count; i++) {
                        assert(args[i]->type);
                        assert(args[i]->flags & AST_NODE_FLAG_RESOLVED_ID);
                        assert(args[i]->flags & AST_NODE_FLAG_TYPED);

                        if (args[i]->type->kind == AST_Type_Kind::POINTER) {
                            args[i] = ast_cast_expression_new(resolver->allocator,
                                                              args[i],
                                                              Builtin::type_s64,
                                                              args[i]->scope,
                                                              args[i]->begin_file_pos,
                                                              args[i]->end_file_pos);
#ifndef NDEBUG
                            bool cast_res =
#endif
                                try_resolve_expression(resolver, args[i]);
                            assert(cast_res);
                        } else {
                            if (args[i]->type != Builtin::type_s64) {
                                resolver_report_mismatching_call_arg(resolver, i, args[i],
                                                                     Builtin::type_s64, true);
                                return false;
                            }
                        }
                    }

                    expression->type = Builtin::type_s64;
                    expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                    expression->flags |= AST_NODE_FLAG_TYPED;
                    break;
                } else if (name == Builtin::atom_cast) {
                    _ENSURE_ARGS_ARE_TYPED
                    assert(args.count == 2);

                    AST_Expression *type_expr = args[0];
#ifndef NDEBUG
                    AST_Expression *op_expr = args[1];
#endif

                    assert(type_expr->type);
                    assert(type_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(type_expr->flags & AST_NODE_FLAG_TYPED);

                    assert(op_expr->type);
                    assert(op_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(op_expr->flags & AST_NODE_FLAG_TYPED);

                    expression->type = type_expr->type;
                    expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                    expression->flags |= AST_NODE_FLAG_TYPED;
                    break;
                } else if (name == Builtin::atom_offsetof) {
                    assert(args.count == 2);

                    assert(args[1]->type);
                    assert(args[1]->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(args[1]->flags & AST_NODE_FLAG_TYPED);

                    assert(args[1]->type->kind == AST_Type_Kind::STRUCTURE);
                    assert(args[1]->type->structure.declaration);

                    AST_Declaration *struct_decl = args[1]->type->structure.declaration;
                    assert(struct_decl->kind == AST_Declaration_Kind::STRUCTURE);

                    Scope *struct_scope = struct_decl->structure.member_scope;
                    assert(struct_scope);

                    assert(args[0]->scope != struct_scope);
                    args[0]->scope = struct_scope;

                    if (!try_resolve_expression(resolver, args[0])) {
                        assert(false);
                    }

                    assert(args[0]->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(args[0]->flags & AST_NODE_FLAG_TYPED);

                    expression->type = Builtin::type_s64;
                    expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                    expression->flags |= AST_NODE_FLAG_TYPED;
                    break;
                } else if (name == Builtin::atom_sizeof) {
                    _ENSURE_ARGS_ARE_TYPED
                    assert(args.count == 1);

                    expression->type = Builtin::type_s64;
                    expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                    expression->flags |= AST_NODE_FLAG_TYPED;
                    break;
                }else if (name == Builtin::atom_assert) {
                    _ENSURE_ARGS_ARE_TYPED
                    assert(args.count == 1);

                    if (args[0]->type->kind != AST_Type_Kind::BOOL) {
                        if (is_valid_type_conversion(args[0], Builtin::type_bool)) {
                            do_type_conversion(resolver, &args[0], Builtin::type_bool);
                        } else {
                            zodiac_report_error(resolver->build_data,
                                                Zodiac_Error_Kind::MISMATCHING_TYPES,
                                                args[0],
                                                "First argument of @assert must be of boolean type, or be able to convert to bool implicitly");
                            return false;
                        }
                    }

                    Scope *entry_scope = resolver->build_data->entry_module->module_scope;
                    auto default_handler_decl =
                        scope_find_declaration(entry_scope,
                                               Builtin::atom_default_assert_handler);
                    if (!default_handler_decl) {
                        assert(false);
                    } else {
                        assert(default_handler_decl->kind == AST_Declaration_Kind::FUNCTION);
                        if (!(default_handler_decl->decl_flags &
                              AST_DECL_FLAG_REGISTERED_BYTECODE)) {
                            bytecode_register_function(&resolver->bytecode_builder,
                                                       default_handler_decl);
                        }
                    }

                    expression->type = Builtin::type_void;
                    expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                    expression->flags |= AST_NODE_FLAG_TYPED;
                    break;
                } else {
                    _ENSURE_ARGS_ARE_TYPED
                    zodiac_report_error(resolver->build_data,
                                        Zodiac_Error_Kind::UNIMPLEMENTED,
                                        expression,
                                        "Builtin call '%s' is not implemented!",
                                        name.data);
                    return false;
                }

#undef _ENSURE_ARGS_ARE_TYPED

                assert(false);

                break;
            }

            case AST_Expression_Kind::ADDROF: {
                AST_Expression *op_expr = expression->addrof.operand_expr;
                assert(op_expr->type);
                assert(op_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(op_expr->flags & AST_NODE_FLAG_TYPED);

                AST_Type *pointer_type =
                    build_data_find_or_create_pointer_type(resolver->allocator,
                                                           resolver->build_data,
                                                           op_expr->type);
                assert(pointer_type);

                expression->type = pointer_type;
                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                break;
            }

            case AST_Expression_Kind::COMPOUND: assert(false);

            case AST_Expression_Kind::SUBSCRIPT: {
#ifndef NDEBUG
                AST_Expression *index_expr = expression->subscript.index_expression;
#endif
                assert(index_expr->type);
                assert(index_expr->type->kind == AST_Type_Kind::INTEGER);
                assert(index_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(index_expr->flags & AST_NODE_FLAG_TYPED);

                AST_Expression *pointer_expr = expression->subscript.pointer_expression;
                assert(pointer_expr->type);
                assert(pointer_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(pointer_expr->flags & AST_NODE_FLAG_TYPED);

                if (pointer_expr->type->kind == AST_Type_Kind::POINTER) {
                    expression->type = pointer_expr->type->pointer.base;
                } else if (pointer_expr->type->kind == AST_Type_Kind::ARRAY) {
                    expression->type = pointer_expr->type->array.element_type;
                } else {
                    assert(false);
                }

                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                break;
            }

            case AST_Expression_Kind::CAST: {
                AST_Expression *op_expr = expression->cast.operand_expression;
                assert(op_expr->type);
                assert(op_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(op_expr->flags & AST_NODE_FLAG_TYPED);

                AST_Type *target_type = expression->cast.target_type;
                assert(target_type);
                assert(target_type->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(target_type->flags & AST_NODE_FLAG_TYPED);

                bool result = true;
                AST_Type *result_type = nullptr;

                if (op_expr->type == target_type) {
                    result = true;
                    result_type = target_type;
                } else if (target_type->kind == AST_Type_Kind::INTEGER) {
                    if (op_expr->type->kind == AST_Type_Kind::INTEGER) {
                        if (op_expr->type->integer.sign) assert(target_type->integer.sign);
                        assert(target_type->bit_size >= op_expr->type->bit_size);
                        result = true;
                        result_type = target_type;
                    } else if (op_expr->type->kind == AST_Type_Kind::POINTER) {
                        assert(target_type->bit_size >= op_expr->type->bit_size);
                        result = true;
                        result_type = target_type;
                    } else if (op_expr->type->kind == AST_Type_Kind::ENUM) {
                        assert(is_valid_type_conversion(op_expr->type, target_type));
                        result = true;
                        result_type = target_type;
                    } else {
                        assert(false);
                    }
                } else if (target_type->kind == AST_Type_Kind::FLOAT) {

                    assert(target_type->bit_size >= op_expr->type->bit_size);
                    result = true;
                    result_type = target_type;

                } else if (target_type->kind == AST_Type_Kind::POINTER) {
                    assert(false);
                } else {
                    assert(false);
                }

                if (result) {
                    assert(result_type);
                    expression->type = result_type;
                    expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                    expression->flags |= AST_NODE_FLAG_TYPED;
                } else {
                    return false;
                }

                break;
            }

            case AST_Expression_Kind::INTEGER_LITERAL: {
                assert(!expression->type);
                if (!expression->infer_type_from) {
                    expression->type = Builtin::type_s64;
                } else {
                    auto infer_type_from = expression->infer_type_from;
                    assert(infer_type_from->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(infer_type_from->flags & AST_NODE_FLAG_TYPED);
                    auto type = infer_type(infer_type_from);
                    assert(type);
                    assert(integer_literal_fits_in_type(expression->integer_literal, type));
                    assert(type->kind == AST_Type_Kind::INTEGER);
                    expression->type = type;
                }
                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                break;
            }

            case AST_Expression_Kind::FLOAT_LITERAL: {
                assert(!expression->type);
                expression->type = Builtin::type_float;
                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                break;
            }

            case AST_Expression_Kind::STRING_LITERAL: {
                assert(!expression->type);
                expression->type = Builtin::type_ptr_u8;
                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                break;
            }

            case AST_Expression_Kind::CHAR_LITERAL: {
                assert(!expression->type);
                expression->type = Builtin::type_u8;
                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                break;
            }

            case AST_Expression_Kind::BOOL_LITERAL: {
                assert(!expression->type);
                expression->type = Builtin::type_bool;
                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                break;
            }

            case AST_Expression_Kind::NULL_LITERAL: {
                AST_Type *result_type = nullptr;

                if (expression->infer_type_from) {
                    AST_Node *infer_type_from = expression->infer_type_from;
                    assert(infer_type_from->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(infer_type_from->flags & AST_NODE_FLAG_TYPED);

                    result_type = infer_type(infer_type_from);
                } else {
                    result_type = build_data_find_or_create_pointer_type(resolver->allocator,
                                                                         resolver->build_data,
                                                                         Builtin::type_void);
                }

                assert(result_type);
                assert(result_type->kind == AST_Type_Kind::POINTER);

                expression->type = result_type;
                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                break;
            }

            case AST_Expression_Kind::RANGE: {
                AST_Expression *begin = expression->range.begin;
                assert(begin->type);
                assert(begin->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(begin->flags & AST_NODE_FLAG_TYPED);

#ifndef NDEBUG
                AST_Expression *end = expression->range.end;
#endif
                assert(end->type);
                assert(end->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(end->flags & AST_NODE_FLAG_TYPED);

                assert(begin->type == end->type);
                assert(begin->type->kind == AST_Type_Kind::INTEGER ||
                       begin->type->kind == AST_Type_Kind::ENUM);

                expression->type = begin->type;
                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                break;
            }
        }

        assert(expression->flags & AST_NODE_FLAG_RESOLVED_ID);
        assert(expression->flags & AST_NODE_FLAG_TYPED);
        assert(expression->type || allow_null_type);
        resolver_inherit_const(expression);
        return true;
    }

    bool try_resolve_type_spec(Resolver *resolver, AST_Type_Spec *type_spec)
    {
        switch (type_spec->kind) {
            case AST_Type_Spec_Kind::INVALID: assert(false);

            case AST_Type_Spec_Kind::IDENTIFIER: {
                AST_Declaration *decl = scope_find_declaration(type_spec->scope,
                                                               type_spec->identifier);
                if (!decl) {
                    zodiac_report_error(resolver->build_data,
                                        Zodiac_Error_Kind::UNDECLARED_IDENTIFIER,
                                        type_spec->identifier,
                                        "Reference to undeclared identifier '%s'",
                                        type_spec->identifier->atom.data);
                    return false;
                }

                if (!(decl->flags & AST_NODE_FLAG_RESOLVED_ID)) {
                    if (decl->kind == AST_Declaration_Kind::STRUCTURE) {
                        if (!decl->type) {
                            return false;
                        }
                        assert(decl->type->kind == AST_Type_Kind::STRUCTURE);
                        if (!(type_spec->ts_flags & AST_TS_FLAG_CHILD_OF_POINTER_TS)) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                } else {
                    assert(decl->flags & AST_NODE_FLAG_TYPED);
                }

                assert(decl->type);

                type_spec->type = decl->type;
                type_spec->flags |= AST_NODE_FLAG_RESOLVED_ID;
                type_spec->flags |= AST_NODE_FLAG_TYPED;

                return true;
            }

            case AST_Type_Spec_Kind::POINTER:
            {
                AST_Type_Spec *base_type_spec = type_spec->base_type_spec;
                assert(base_type_spec->type);
                assert(base_type_spec->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(base_type_spec->flags & AST_NODE_FLAG_TYPED);

                AST_Type *pointer_type =
                    build_data_find_or_create_pointer_type(resolver->allocator,
                                                           resolver->build_data,
                                                           base_type_spec->type);

                type_spec->type = pointer_type;
                type_spec->flags |= AST_NODE_FLAG_RESOLVED_ID;
                type_spec->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Type_Spec_Kind::DOT: {
                AST_Expression *dot_expr = type_spec->dot_expression;
                assert(dot_expr->type);
                assert(dot_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(dot_expr->flags & AST_NODE_FLAG_TYPED);


                type_spec->type = dot_expr->type;
                type_spec->flags |= AST_NODE_FLAG_RESOLVED_ID;
                type_spec->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Type_Spec_Kind::FUNCTION: {

                Array<AST_Type *> param_types = {};
                array_init(resolver->allocator, &param_types,
                           type_spec->function.parameter_type_specs.count);

                for (int64_t i = 0; i < type_spec->function.parameter_type_specs.count; i++) {
                    auto param_ts = type_spec->function.parameter_type_specs[i];
                    assert(param_ts->type);
                    assert(param_ts->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(param_ts->flags & AST_NODE_FLAG_TYPED);

                    array_append(&param_types, param_ts->type);
                }

                AST_Type_Spec *return_ts = nullptr;
                if (type_spec->function.return_type_spec) {
                    return_ts = type_spec->function.return_type_spec;
                    assert(return_ts->type);
                    assert(return_ts->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(return_ts->flags & AST_NODE_FLAG_TYPED);
                }

                AST_Type *return_type = nullptr;

                if (return_ts) {
                    return_type = return_ts->type;
                } else {
                    return_type = Builtin::type_void;
                }

                AST_Type *function_type =
                    build_data_find_function_type(resolver->build_data,
                                                            param_types, return_type);
                if (!function_type) {
                    function_type = ast_function_type_new(resolver->allocator, param_types,
                                                          return_type);
                    function_type->flags |= AST_NODE_FLAG_RESOLVED_ID;
                    function_type->flags |= AST_NODE_FLAG_TYPED;
                    array_append(&resolver->build_data->type_table, function_type);
                } else {
                    array_free(&param_types);
                }
                assert(function_type);

                type_spec->type = function_type;
                type_spec->flags |= AST_NODE_FLAG_RESOLVED_ID;
                type_spec->flags |= AST_NODE_FLAG_TYPED;

                return true;
                break;
            }

            case AST_Type_Spec_Kind::ARRAY: {
                AST_Expression *length_expr = type_spec->array.length_expression;
                assert(length_expr->type);
                assert(length_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(length_expr->flags & AST_NODE_FLAG_TYPED);

                AST_Type_Spec *elem_ts = type_spec->array.element_type_spec;
                assert(elem_ts->type);
                assert(elem_ts->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(elem_ts->flags & AST_NODE_FLAG_TYPED);

                Const_Value elem_count = const_interpret_expression(length_expr);
                assert(elem_count.type == Builtin::type_s64);

                AST_Type *array_type = build_data_find_array_type(resolver->build_data,
                                                                  elem_ts->type,
                                                                  elem_count.integer.s64);
                if (!array_type) {
                    array_type = build_data_create_array_type(resolver->allocator,
                                                              resolver->build_data,
                                                              elem_ts->type,
                                                              elem_count.integer.s64);
                }
                assert(array_type);

                type_spec->type = array_type;
                type_spec->flags |= AST_NODE_FLAG_RESOLVED_ID;
                type_spec->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Type_Spec_Kind::TEMPLATED: assert(false);
            case AST_Type_Spec_Kind::POLY_IDENTIFIER: assert(false);

            case AST_Type_Spec_Kind::FROM_TYPE: {
                assert(type_spec->type);
                type_spec->flags |= AST_NODE_FLAG_RESOLVED_ID;
                type_spec->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }
        }

        assert(false);
        return false;
    }

    bool try_size_declaration(Resolver *resolver, AST_Declaration *decl)
    {
        assert(!(decl->flags & AST_NODE_FLAG_SIZED));

        switch (decl->kind) {
            case AST_Declaration_Kind::INVALID: assert(false);

            case AST_Declaration_Kind::IMPORT: {
                assert(decl->import.ast_module);
                decl->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Declaration_Kind::USING: {
                decl->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Declaration_Kind::FUNCTION: {
                assert(decl->type);
                assert(decl->type->flags & AST_NODE_FLAG_SIZED);
                decl->flags |= AST_NODE_FLAG_SIZED;

                    // Don't emit the entry (_start) if we're linking with libc
                if (!(resolver->build_data->options->link_c &&
                    (decl->decl_flags & AST_DECL_FLAG_IS_ENTRY))) {

                    auto bc_func = bytecode_register_function(&resolver->bytecode_builder, decl);

                    if (!resolver->build_data->options->dont_emit_llvm)
                        llvm_register_function(&resolver->llvm_builder, bc_func);

                    if (!(decl->decl_flags & AST_DECL_FLAG_QUEUED_BYTECODE)) {
                        queue_bytecode_job(resolver, decl);
                    }
                }

                return true;
            }

            case AST_Declaration_Kind::VARIABLE:
            case AST_Declaration_Kind::CONSTANT:
            case AST_Declaration_Kind::PARAMETER:
            case AST_Declaration_Kind::TYPEDEF:
            case AST_Declaration_Kind::STRUCTURE:
            case AST_Declaration_Kind::UNION:
            case AST_Declaration_Kind::ENUM:
            case AST_Declaration_Kind::RUN:
            case AST_Declaration_Kind::STATIC_IF: {
                assert(decl->type);
                if (!(decl->type->flags & AST_NODE_FLAG_SIZED)) {
#ifndef NDEBUG
                    bool result =
#endif
                        try_size_type(resolver, decl->type);
                    assert(result);
                }
                decl->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Declaration_Kind::TYPE: assert(false);
            case AST_Declaration_Kind::POLY_TYPE: assert(false);
            case AST_Declaration_Kind::STATIC_ASSERT: assert(false);
        }

        assert(false);
        return false;
    }

    bool try_size_statement(Resolver *resolver, AST_Statement *statement)
    {
        assert(!(statement->flags & AST_NODE_FLAG_SIZED));

        switch (statement->kind) {
            case AST_Statement_Kind::INVALID: assert(false);

            case AST_Statement_Kind::BLOCK: {
                statement->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Statement_Kind::ASSIGNMENT: {
#ifndef NDEBUG
                AST_Expression *ident_expr = statement->assignment.identifier_expression;
                assert(ident_expr->flags & AST_NODE_FLAG_SIZED);

                AST_Expression *rhs_expr = statement->assignment.rhs_expression;
                assert(rhs_expr->flags & AST_NODE_FLAG_SIZED);
#endif

                statement->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Statement_Kind::RETURN: {
                AST_Expression *op_expr = statement->expression;
                if (op_expr) {
                    assert(op_expr->flags & AST_NODE_FLAG_SIZED);
                }

                statement->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Statement_Kind::BREAK: {
                statement->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Statement_Kind::DECLARATION: {
#ifndef NDEBUG
                AST_Declaration *decl = statement->declaration;
                assert(decl->type);
                assert(decl->flags & AST_NODE_FLAG_SIZED);
#endif

                statement->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Statement_Kind::EXPRESSION: {
#ifndef NDEBUG
                AST_Expression *expr = statement->expression;
                assert(expr->type);
                assert(expr->flags & AST_NODE_FLAG_SIZED);
#endif

                statement->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Statement_Kind::WHILE: {
#ifndef NDEBUG
                AST_Expression *cond_expr = statement->while_stmt.cond_expr;
                assert(cond_expr->flags & AST_NODE_FLAG_SIZED);

                AST_Statement *body = statement->while_stmt.body;
                assert(body->flags & AST_NODE_FLAG_SIZED);
#endif

                statement->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Statement_Kind::FOR: {
#ifndef NDEBUG
                for (int64_t i = 0; i < statement->for_stmt.init_statements.count; i++) {
                    AST_Statement *init_stmt = statement->for_stmt.init_statements[i];
                    assert(init_stmt->flags & AST_NODE_FLAG_SIZED);
                }
                AST_Expression *cond_expr = statement->for_stmt.cond_expr;
                assert(cond_expr->flags & AST_NODE_FLAG_SIZED);

                for (int64_t i = 0; i < statement->for_stmt.step_statements.count; i++) {
                    AST_Statement *step_stmt = statement->for_stmt.step_statements[i];
                    assert(step_stmt->flags & AST_NODE_FLAG_SIZED);
                }

                AST_Statement *body_stmt = statement->for_stmt.body_stmt;
                assert(body_stmt->flags & AST_NODE_FLAG_SIZED);
#endif

                statement->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Statement_Kind::IF: {
#ifndef NDEBUG
                AST_Expression *cond_expr = statement->if_stmt.cond_expr;
                assert(cond_expr->flags & AST_NODE_FLAG_SIZED);

                AST_Statement *then_stmt = statement->if_stmt.then_stmt;
                assert(then_stmt->flags & AST_NODE_FLAG_SIZED);

                AST_Statement *else_stmt = statement->if_stmt.else_stmt;
                if (else_stmt) {
                    assert(else_stmt->flags & AST_NODE_FLAG_SIZED);
                }
#endif

                statement->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Statement_Kind::SWITCH: {
#ifndef NDEBUG
                AST_Expression *switch_expr = statement->switch_stmt.expression;
                assert(switch_expr->flags & AST_NODE_FLAG_SIZED);
#endif

                for (int64_t i = 0; i < statement->switch_stmt.cases.count; i++) {
                    AST_Switch_Case *switch_case = statement->switch_stmt.cases[i];

                    assert(switch_case->body->flags & AST_NODE_FLAG_SIZED);

                    if (switch_case->is_default) continue;

                    auto el = bucket_array_first(&switch_case->expressions);
                    while (el.bucket) {
                        auto case_expr = *bucket_locator_get_ptr(el);

                        if (!(case_expr->flags & AST_NODE_FLAG_SIZED)) {
                            try_size_expression(resolver, case_expr);
                            assert(case_expr->flags & AST_NODE_FLAG_SIZED);
                        }

                        bucket_locator_advance(&el);
                    }
                }
                return true;
                break;
            }
        }

        assert(false);
        return false;
    }

    bool try_size_expression(Resolver *resolver, AST_Expression *expression)
    {
        assert(!(expression->flags & AST_NODE_FLAG_SIZED));

        switch (expression->kind) {
            case AST_Expression_Kind::INVALID: assert(false);

            case AST_Expression_Kind::IDENTIFIER: {
                if (expression->type) {

                    if (!(expression->type->flags & AST_NODE_FLAG_SIZED)) {
                        if (!try_size_type(resolver, expression->type)) {
                            return false;
                        }
                    }

                    assert(expression->type->flags & AST_NODE_FLAG_SIZED);
                } else {
                    assert(expression->identifier->declaration);
                    assert(expression->identifier->declaration->kind ==
                           AST_Declaration_Kind::IMPORT);
                }


                expression->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Expression_Kind::CALL: {
                assert(expression->type);
                assert(expression->type->flags & AST_NODE_FLAG_SIZED);

                expression->flags |= AST_NODE_FLAG_SIZED;

                AST_Declaration *callee = expression->call.callee_declaration;
                assert(callee);

                auto bc_func = bytecode_find_function(&resolver->bytecode_builder, callee);

                if (!resolver->build_data->options->dont_emit_llvm)
                    llvm_register_function(&resolver->llvm_builder, bc_func);

                return true;
                break;
            }

            case AST_Expression_Kind::DOT:
            case AST_Expression_Kind::BINARY:
            case AST_Expression_Kind::UNARY:
            case AST_Expression_Kind::BUILTIN_CALL:
            case AST_Expression_Kind::ADDROF:
            case AST_Expression_Kind::SUBSCRIPT:
            case AST_Expression_Kind::INTEGER_LITERAL:
            case AST_Expression_Kind::CHAR_LITERAL:
            case AST_Expression_Kind::FLOAT_LITERAL:
            case AST_Expression_Kind::STRING_LITERAL:
            case AST_Expression_Kind::BOOL_LITERAL:
            case AST_Expression_Kind::NULL_LITERAL:
            case AST_Expression_Kind::RANGE:
            case AST_Expression_Kind::CAST: {
                AST_Type *type = expression->type;
                assert(type);

                if (!(type->flags & AST_NODE_FLAG_SIZED)) {
                    if (!try_size_type(resolver, type)) {
                        return false;
                    }
                }

                expression->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);
            case AST_Expression_Kind::COMPOUND: assert(false);

        }

        assert(false);
        return false;
    }

    bool try_size_type(Resolver *resolver, AST_Type *type)
    {
        assert(!(type->flags & AST_NODE_FLAG_SIZED));

        switch (type->kind) {
            case AST_Type_Kind::INVALID: assert(false);
            case AST_Type_Kind::VOID: assert(false);
            case AST_Type_Kind::INTEGER: assert(false);
            case AST_Type_Kind::FLOAT: assert(false);
            case AST_Type_Kind::BOOL: assert(false);
            case AST_Type_Kind::POINTER: assert(false);

            case AST_Type_Kind::FUNCTION: {
                assert(type->bit_size == Builtin::pointer_size);
                type->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Type_Kind::STRUCTURE:
            {
                uint64_t bit_size = 0;
                for (int64_t i = 0; i < type->structure.member_types.count; i++) {
                    AST_Type *mem_type = type->structure.member_types[i];
                    assert(mem_type->flags & AST_NODE_FLAG_SIZED);

                    bit_size += mem_type->bit_size;
                }

                type->bit_size = bit_size;
                type->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Type_Kind::UNION: {
                assert(!type->union_type.biggest_member_type);
                uint64_t biggest_size = 0;
                AST_Type *biggest_type = nullptr;
                for (int64_t i = 0; i < type->union_type.member_types.count; i++) {
                    AST_Type *mem_type = type->union_type.member_types[i];
                    assert(mem_type->flags & AST_NODE_FLAG_SIZED);

                    if (mem_type->bit_size > biggest_size) {
                        biggest_size = mem_type->bit_size;
                        biggest_type = mem_type;
                    }
                }

                assert(biggest_size > 0);
                assert(biggest_type);

                type->bit_size = biggest_size;
                type->union_type.biggest_member_type = biggest_type;
                type->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
                break;
            }

            case AST_Type_Kind::ENUM: {
                assert(type->enum_type.base_type->flags & AST_NODE_FLAG_SIZED);
                type->bit_size = type->enum_type.base_type->bit_size;
                type->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Type_Kind::ARRAY: {
                AST_Type *element_type = type->array.element_type;
                assert(element_type->flags & AST_NODE_FLAG_TYPED);

                type->bit_size = element_type->bit_size * type->array.element_count;
                type->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }
        }

        assert(false);
        return false;
    }

    bool try_size_type_spec(Resolver *resolver, AST_Type_Spec *type_spec)
    {
            assert(type_spec->type);

            AST_Declaration *decl = nullptr;
            if (type_spec->kind == AST_Type_Spec_Kind::IDENTIFIER) {
                decl = type_spec->identifier->declaration;
                assert(decl);
            } else if (type_spec->kind == AST_Type_Spec_Kind::DOT) {
                decl = resolver_get_declaration(type_spec->dot_expression);
                assert(decl);
            }

            if (decl) {
                assert(type_spec->type == decl->type);
            }

            if (!(type_spec->type->flags & AST_NODE_FLAG_SIZED)) {
                if (!try_size_type(resolver, type_spec->type)) {
                    return false;
                }
            }

            type_spec->flags |= AST_NODE_FLAG_SIZED;
            return true;
    }

    AST_Type *infer_type(AST_Node *ast_node)
    {
        assert(ast_node->flags & AST_NODE_FLAG_RESOLVED_ID);
        assert(ast_node->flags & AST_NODE_FLAG_TYPED);

        AST_Type *result_type = nullptr;

        switch (ast_node->kind) {
            case AST_Node_Kind::INVALID: assert(false);
            case AST_Node_Kind::MODULE: assert(false);
            case AST_Node_Kind::IDENTIFIER: assert(false);
            case AST_Node_Kind::DECLARATION: assert(false);
            case AST_Node_Kind::SWITCH_CASE: assert(false);
            case AST_Node_Kind::STATEMENT: assert(false);

            case AST_Node_Kind::EXPRESSION: {
                auto expr = static_cast<AST_Expression *>(ast_node);
                assert(expr->type);
                result_type = expr->type;
                break;
            }

            case AST_Node_Kind::TYPE_SPEC: {
                auto ts = static_cast<AST_Type_Spec *>(ast_node);
                assert(ts->type);
                result_type = ts->type;
                break;
            }

            case AST_Node_Kind::TYPE: {
                auto type = static_cast<AST_Type *>(ast_node);
                result_type = type;
                break;
            }
        }

        assert(result_type);
        return result_type;
    }

    AST_Declaration *enclosing_function(Resolver *resolver, Scope *scope)
    {
        assert(scope->kind != Scope_Kind::PARAMETER);

        while (scope->kind != Scope_Kind::MODULE) {

            Scope *parent_scope = scope->parent;
            assert(parent_scope);

            if (parent_scope->kind == Scope_Kind::PARAMETER) {
                assert(parent_scope->function_declaration);
                return parent_scope->function_declaration;
            }

            scope = parent_scope;
        }

        return nullptr;

    }

    AST_Declaration *enclosing_function(Resolver *resolver, AST_Node *node)
    {
        auto scope = node->scope;
        assert(scope);
        return enclosing_function(resolver, scope);
    }

    AST_Declaration *resolver_get_declaration(AST_Expression *expr)
    {
        assert(expr->flags & AST_NODE_FLAG_RESOLVED_ID);

        switch (expr->kind) {
            case AST_Expression_Kind::INVALID: assert(false);
            case AST_Expression_Kind::IDENTIFIER: return expr->identifier->declaration;
            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);
            case AST_Expression_Kind::DOT: return expr->dot.child_decl;
            case AST_Expression_Kind::BINARY: assert(false);

            case AST_Expression_Kind::UNARY: {
                assert(expr->unary.op == UNOP_DEREF);
                return resolver_get_declaration(expr->unary.operand_expression);
                break;
            }

            case AST_Expression_Kind::CALL: assert(false);
            case AST_Expression_Kind::BUILTIN_CALL: assert(false);
            case AST_Expression_Kind::ADDROF: assert(false);
            case AST_Expression_Kind::COMPOUND: assert(false);

            case AST_Expression_Kind::SUBSCRIPT: {
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

    void resolver_inherit_const(AST_Expression *expr)
    {
        if (expr->expr_flags & AST_EXPR_FLAG_CONST) return;

        bool is_const = false;

        switch (expr->kind) {
            case AST_Expression_Kind::INVALID: assert(false);

            case AST_Expression_Kind::IDENTIFIER:
            case AST_Expression_Kind::DOT: {
                if (expr->expr_flags & AST_EXPR_FLAG_DOT_COUNT) {
                    is_const = true;
                } else {
                    auto decl = resolver_get_declaration(expr);
                    assert(decl);

                    switch (decl->kind) {
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

            case AST_Expression_Kind::BINARY: {
                auto lhs = expr->binary.lhs;
                auto rhs = expr->binary.rhs;

                is_const = ((lhs->expr_flags & AST_EXPR_FLAG_CONST) &&
                            (rhs->expr_flags & AST_EXPR_FLAG_CONST));
                break;
            }

            case AST_Expression_Kind::UNARY: {
                auto op_expr = expr->unary.operand_expression;
                is_const = op_expr->expr_flags & AST_EXPR_FLAG_CONST;
                break;
            }

            case AST_Expression_Kind::BUILTIN_CALL: {
                auto atom = expr->builtin_call.identifier->atom;

                if (atom == Builtin::atom_exit) {
                    assert(expr->builtin_call.arg_expressions.count == 1);
                    auto op_expr = expr->builtin_call.arg_expressions[0];
                    is_const = op_expr->expr_flags & AST_EXPR_FLAG_CONST;

                } else if (atom == Builtin::atom_syscall) {
                    is_const = false;

                } else if (atom == Builtin::atom_cast) {
                    assert(expr->builtin_call.arg_expressions.count == 2);
                    auto op_expr = expr->builtin_call.arg_expressions[1];
                    is_const = op_expr->expr_flags & AST_EXPR_FLAG_CONST;

                } else if (atom == Builtin::atom_sizeof ||
                           atom == Builtin::atom_offsetof) {
                    is_const = true;
                } else if (atom == Builtin::atom_assert) {
                    assert(expr->builtin_call.arg_expressions.count == 1);
                    auto cond_expr = expr->builtin_call.arg_expressions[0];
                    is_const = cond_expr->expr_flags & AST_EXPR_FLAG_CONST;
                } else {
                    assert(false);
                }
                break;
            }

            case AST_Expression_Kind::CALL:
            case AST_Expression_Kind::ADDROF: break;
            case AST_Expression_Kind::COMPOUND: assert(false);

            case AST_Expression_Kind::SUBSCRIPT: {
                auto pointer_expr = expr->subscript.pointer_expression;
                auto index_expr = expr->subscript.index_expression;

                is_const = ((pointer_expr->expr_flags & AST_EXPR_FLAG_CONST) &&
                            (index_expr->expr_flags & AST_EXPR_FLAG_CONST));
                break;
            }

            case AST_Expression_Kind::CAST: {
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

            case AST_Expression_Kind::RANGE: {
                auto begin = expr->range.begin;
                auto end = expr->range.end;
                is_const = ((begin->expr_flags & AST_EXPR_FLAG_CONST) &&
                            (end->expr_flags & AST_EXPR_FLAG_CONST));
                break;
            }
        }

        if (is_const) {
            expr->expr_flags |= AST_EXPR_FLAG_CONST;
        }
    }

    void convert_condition_to_bool(Resolver *resolver, AST_Expression **expr_ptr)
    {
        assert(false);
        AST_Expression *cond_expr = *expr_ptr;
        assert(cond_expr);
        assert(cond_expr->type != Builtin::type_bool);

        AST_Expression *null_lit =
            ast_null_literal_expression_new(resolver->allocator,
                                            cond_expr->scope,
                                            cond_expr->begin_file_pos,
                                            cond_expr->end_file_pos);

        null_lit->infer_type_from = cond_expr;

        *expr_ptr =
            ast_binary_expression_new(resolver->allocator, BINOP_NEQ, cond_expr,
                                      null_lit,
                                      cond_expr->scope,
                                      cond_expr->begin_file_pos,
                                      cond_expr->end_file_pos);

        cond_expr = *expr_ptr;

        if (!try_resolve_expression(resolver, null_lit)) assert(false);
        if (!try_resolve_expression(resolver, cond_expr)) assert(false);

        queue_size_job(resolver, cond_expr);
    }

    AST_Type *do_type_conversion(Resolver *resolver, AST_Expression **p_expr,
                                 AST_Type *target_type)
    {
        assert(is_valid_type_conversion(*p_expr, target_type));

        auto expr = *p_expr;

        AST_Expression *new_expr = nullptr;

        if (expr->kind == AST_Expression_Kind::INTEGER_LITERAL ||
            expr->kind == AST_Expression_Kind::FLOAT_LITERAL) {
            expr->type = target_type;

        } else if (expr->type->kind == AST_Type_Kind::POINTER &&
                   target_type->kind == AST_Type_Kind::BOOL) {
            auto null_expr = ast_null_literal_expression_new(resolver->allocator, expr->scope,
                                                             expr->begin_file_pos,
                                                             expr->end_file_pos);
            null_expr->infer_type_from = expr->type;
            new_expr = ast_binary_expression_new(resolver->allocator, BINOP_NEQ,
                                                 expr, null_expr, expr->scope,
                                                 expr->begin_file_pos,
                                                 expr->end_file_pos);
#ifndef NDEBUG
            bool null_expr_res =
#endif
                try_resolve_expression(resolver, null_expr);
            assert(null_expr_res);

        } else {
            new_expr = ast_cast_expression_new(resolver->allocator,
                                               expr, target_type,
                                               expr->scope,
                                               expr->begin_file_pos,
                                               expr->end_file_pos);
        }

        if (new_expr) {

#ifndef NDEBUG
            bool new_res =
#endif
                try_resolve_expression(resolver, new_expr);
            assert(new_res);
            *p_expr = new_expr;
        }

        return (*p_expr)->type;
    }

    bool is_valid_type_conversion(AST_Expression *expr, AST_Type *target_type)
    {
        if (expr->kind == AST_Expression_Kind::INTEGER_LITERAL &&
            integer_literal_fits_in_type(expr->integer_literal, target_type)) {
            return true;
        } else if (expr->kind == AST_Expression_Kind::FLOAT_LITERAL) {
            assert(target_type->bit_size  > expr->type->bit_size);
            return true;
        }

        return is_valid_type_conversion(expr->type, target_type);
    }

    bool is_valid_type_conversion(AST_Type *type, AST_Type *target_type)
    {
        assert(type);
        assert(target_type);

        switch (type->kind) {
            case AST_Type_Kind::INVALID: assert(false);
            case AST_Type_Kind::VOID: assert(false);

            case AST_Type_Kind::INTEGER: {
                if (target_type->kind == AST_Type_Kind::INTEGER) {
                    if (type->integer.sign == target_type->integer.sign) {
                        return type->bit_size < target_type->bit_size;
                    } else if (type->integer.sign) {
                        return false;
                    } else {
                        assert(target_type->integer.sign);
                        return type->bit_size < target_type->bit_size;
                    }
                } else if (target_type->kind == AST_Type_Kind::BOOL) {
                    return true;
                } else if (target_type->kind == AST_Type_Kind::STRUCTURE ||
                           target_type->kind == AST_Type_Kind::POINTER) {
                    return false;
                } else {
                    assert(false);
                }

                break;
            }

            case AST_Type_Kind::FLOAT: {
                if (target_type->kind == AST_Type_Kind::FLOAT) {
                    return target_type->bit_size > type->bit_size;
                } else if (target_type->kind == AST_Type_Kind::INTEGER) {
                    return false;
                } else assert(false);
            }

            case AST_Type_Kind::BOOL: assert(false);

            case AST_Type_Kind::POINTER: {
                if (target_type->kind == AST_Type_Kind::BOOL) {
                    return true;
                } else if (target_type->kind == AST_Type_Kind::INTEGER) {
                    return false;
                } else {
                    assert(false);
                }
                break;
            }

            case AST_Type_Kind::FUNCTION: assert(false);

            case AST_Type_Kind::STRUCTURE: return false;

            case AST_Type_Kind::UNION: assert(false);

            case AST_Type_Kind::ENUM: {
                if (target_type->kind == AST_Type_Kind::INTEGER) {
                    assert(type->enum_type.base_type);
                    return is_valid_type_conversion(type->enum_type.base_type, target_type);
                }
                else assert(false);
                break;
            }

            case AST_Type_Kind::ARRAY: assert(false);
        }

        assert(false);
        return false;
    }

    bool integer_literal_fits_in_type(Integer_Literal il, AST_Type *type)
    {
        if (type->kind == AST_Type_Kind::ENUM) {
            type = type->enum_type.base_type;
        }

        if (type->kind == AST_Type_Kind::INTEGER) {
            auto val = il.s64;

#define CHECK_BIT_WIDTH_CASE(bit_width) \
            case bit_width: { \
                if (type->integer.sign) { \
                    return val >= INT##bit_width##_MIN && val <= INT##bit_width##_MAX; \
                } else {\
                    return (uint##bit_width##_t)val >= 0 && \
                           (uint##bit_width##_t)val <= UINT##bit_width##_MAX; \
                } \
            }

            switch (type->bit_size) {
                CHECK_BIT_WIDTH_CASE(8);
                CHECK_BIT_WIDTH_CASE(16);
                CHECK_BIT_WIDTH_CASE(32);
                CHECK_BIT_WIDTH_CASE(64);

                default: assert(false);
            }

#undef CHECK_BIT_WIDTH_CASE
        } else if (type->kind == AST_Type_Kind::FLOAT) {

            auto val = il.s64;

#define FLOAT_INT_MAX 0x1000000
#define DOUBLE_INT_MAX 0x20000000000000

            switch (type->bit_size) {
                case 32: {
                    return val >= (-FLOAT_INT_MAX) && val <= FLOAT_INT_MAX;
                }

                case 64: {
                    return val >= (-DOUBLE_INT_MAX) && val <= DOUBLE_INT_MAX;
                }

                default: assert(false);
            }

#undef FLOAT_INT_MAX
#undef DOUBLE_INT_MAX

        } else assert(false);

        assert(false);
        return false;
    }

    void resolver_report_mismatching_call_arg(Resolver *resolver,  int64_t index,
                                              AST_Expression *arg_expr,
                                              AST_Declaration *param_decl)
    {
        zodiac_report_error(resolver->build_data, Zodiac_Error_Kind::MISMATCHING_TYPES,
                            arg_expr,
                            "Mismatching types for argument %d", index);
        auto param_type_str = ast_type_to_tstring(param_decl->type);
        zodiac_report_info(resolver->build_data, param_decl, "Expected type: '%.*s'",
                           (int)param_type_str.length, param_type_str.data);
        auto arg_type_str = ast_type_to_tstring(arg_expr->type);
        zodiac_report_info(resolver->build_data, arg_expr, "Given type: '%.*s'",
                           (int)arg_type_str.length, arg_type_str.data);
    }

    void resolver_report_mismatching_call_arg(Resolver *resolver,  int64_t index,
                                              AST_Expression *arg_expr,
                                              AST_Type *expected_type, bool is_builtin)
    {
        auto err_fmt = is_builtin ?
            "Mismatching types for argument %d" :
            "Mismatching types for argument %d of call to foreign function";

        zodiac_report_error(resolver->build_data, Zodiac_Error_Kind::MISMATCHING_TYPES,
                            arg_expr, err_fmt, index);
        auto param_type_str = ast_type_to_tstring(expected_type);
        zodiac_report_info(resolver->build_data, arg_expr, "Expected type: ''%.*s'",
                           (int)param_type_str.length, param_type_str.data);
        auto arg_type_str = ast_type_to_tstring(arg_expr->type);
        zodiac_report_info(resolver->build_data, arg_expr, "Given type: '%.*s'",
                           (int)arg_type_str.length, arg_type_str.data);
    }

    bool is_entry_decl(Resolver *resolver, AST_Declaration *decl)
    {
        if (decl->kind != AST_Declaration_Kind::FUNCTION)
            return false;

        if (resolver->llvm_builder.target_platform == Zodiac_Target_Platform::LINUX) {
            if (decl->identifier->atom == Builtin::atom__start &&
                (decl->decl_flags & AST_DECL_FLAG_IS_NAKED)) {
                return true;
            }
        } else if (resolver->llvm_builder.target_platform ==
                   Zodiac_Target_Platform::WINDOWS) {
            if (decl->identifier->atom == Builtin::atom_mainCRTStartup) {
                return true;
            }
        } else {
            assert(false);
        }

        return false;
    }

    bool is_bytecode_entry_decl(AST_Declaration *decl)
    {
        if (decl->kind == AST_Declaration_Kind::FUNCTION &&
            decl->identifier->atom == Builtin::atom_main) {
            return true;
        }

        return false;
    }

    bool case_range_valid(bool signed_type, Integer_Literal val, Integer_Literal end_val)
    {
        if (signed_type) return val.s64 < end_val.s64;
        else             return val.u64 < end_val.u64;
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

        Array<AST_Node **> new_nodes = {};
        array_init(ta, &new_nodes,  temp_case_exprs.capacity * 2);

        auto el = bucket_array_first(&switch_case->expressions);
        int expr_i = 0;
        while (el.bucket) {
            auto case_expr = *bucket_locator_get_ptr(el);

            if (case_expr->kind == AST_Expression_Kind::RANGE) {

                if (first_range_index == -1) {
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

                bool signed_type = false;

                if (begin_val.type->kind == AST_Type_Kind::INTEGER) {
                    if (begin_val.type->integer.sign) signed_type = true;
                } else {
                    assert(begin_val.type->kind == AST_Type_Kind::ENUM);
                    AST_Type *base_type = begin_val.type->enum_type.base_type;
                    assert(base_type->kind == AST_Type_Kind::INTEGER);
                    if (base_type->integer.sign) signed_type = true;
                }

                if (signed_type) {
                    assert(begin_val.integer.s64 < end_val.integer.s64);
                } else {
                    assert(begin_val.integer.u64 < end_val.integer.u64);
                }

                File_Pos begin_fp = begin_expr->begin_file_pos;
                begin_fp.file_name =
                    string_append(resolver->allocator,
                                  string_ref("<expanded from range expression> "),
                                  begin_expr->begin_file_pos.file_name);

                auto end_fp = begin_fp;

                Integer_Literal val;
                if (signed_type) val.s64 = begin_val.integer.s64 + 1;
                else val.u64 = begin_val.integer.u64 + 1;

                while (case_range_valid(signed_type, val, end_val.integer)) {
                    AST_Expression *new_expr = nullptr;

                    if (signed_type) {
                        new_expr = ast_integer_literal_expression_new(resolver->allocator,
                                                                      val.s64, switch_scope,
                                                                      begin_fp, end_fp);
                    } else {
                        new_expr = ast_integer_literal_expression_new(resolver->allocator,
                                                                      val.u64, switch_scope,
                                                                      begin_fp, end_fp);
                    }

                    assert(new_expr);

                    if (begin_expr->type->kind == AST_Type_Kind::ENUM) {
                        auto enum_type = begin_expr->type;

                        auto enum_name =
                            enum_type->enum_type.declaration->identifier->atom;

                        auto parent_ident = ast_identifier_new(resolver->allocator,
                                                               enum_name, switch_scope,
                                                               begin_fp, end_fp);

                        auto parent_expr =
                            ast_identifier_expression_new(resolver->allocator,
                                                          parent_ident, switch_scope,
                                                          begin_fp, end_fp);;

                        auto cv = create_const_value(enum_type, val);
                        auto member_decl = ast_find_enum_member(enum_type, cv);
                        if (member_decl) {

                            auto child_name = member_decl->identifier->atom;
                            AST_Identifier *child_ident =
                                ast_identifier_new(resolver->allocator, child_name,
                                                   switch_scope,
                                                   begin_fp, end_fp);

                            new_expr = ast_dot_expression_new(resolver->allocator,
                                                              parent_expr, child_ident,
                                                              switch_scope,
                                                              begin_fp, end_fp);

                        } else {
                            new_expr = nullptr;
                        }
                    }

                    if (new_expr) {
                        array_append(&temp_case_exprs, new_expr);
                    }

                    if (signed_type) val.s64 += 1;
                    else val.u64 += 1;

                    stmt->switch_stmt.case_expr_count += 1;
                }

                array_append(&temp_case_exprs, end_expr);
                stmt->switch_stmt.case_expr_count += 1;

                //@FIXME:@LEAK: We are leaking 'case_expr' here, right now we can't be
                //               sure about the allocator that was used to allocate the
                //               expression. (in practice we are only using the c
                //               allocator at the moment, but if we change any of them we
                //               won't know)

            } else if (first_range_index >= 0) {
                array_append(&temp_case_exprs, case_expr);
            }

            expr_i += 1;
            bucket_locator_advance(&el);
        }

        assert(first_range_index >= 0);

        el = bucket_array_locator_by_index(&switch_case->expressions, first_range_index);

        for (int64_t j = 0; j < temp_case_exprs.count; j++) {

            AST_Expression ** ptr = nullptr;
            if (el.bucket) {
                ptr = bucket_locator_get_ptr(el);
                *ptr = temp_case_exprs[j];

                bucket_locator_advance(&el);
            } else {
                auto nl = bucket_array_add(&switch_case->expressions, temp_case_exprs[j]);
                ptr = bucket_locator_get_ptr(nl);
            }

            assert(ptr);

            ast_flatten_expression(&resolver->ast_builder, ptr, &new_nodes);
        }

        for (int64_t i = 0; i < new_nodes.count; i++) {
            auto node = *new_nodes[i];
            assert(node->kind == AST_Node_Kind::EXPRESSION);
            if (!try_resolve_expression(resolver,
                                        static_cast<AST_Expression *>(node))) {
                assert(false);
            }
        }
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

        for (int64_t i = 0; i < cases.count; i++) {
            AST_Switch_Case *switch_case = cases[i];

            auto el = bucket_array_first(&switch_case->expressions);
            while (el.bucket) {
                auto case_expr = *bucket_locator_get_ptr(el);

                Const_Value expr_val = const_interpret_expression(case_expr);

                int64_t match_idx = -1;

                for (int64_t umv_i = 0; umv_i < unhandled_umvs.count; umv_i++) {
                    auto u_umv = unhandled_umvs[umv_i];
                    if (u_umv.u64 == expr_val.integer.u64) {
                        match_idx = umv_i;
                        break;
                    }
                }

                if (match_idx != -1) {
                    array_unordered_remove(&unhandled_umvs, match_idx);
                }

                if (unhandled_umvs.count <= 0) break;

                bucket_locator_advance(&el);
            }

            if (unhandled_umvs.count <= 0) break;
        }

        if (unhandled_umvs.count) {
            zodiac_report_error(resolver->build_data,
                                Zodiac_Error_Kind::INCOMPLETE_SWITCH,
                                ast_stmt,
                                "Incomplete switch case, %" PRId64 " unhandled enum values.",
                                unhandled_umvs.count);

            auto edecl = enum_type->enum_type.declaration;

            int64_t report_count = min(unhandled_umvs.count, 3);
            for (int64_t i = 0; i < report_count; i++)
            {
                auto cv = create_const_value(enum_type, unhandled_umvs[i]);
                auto emem = ast_find_enum_member(enum_type, cv);

                zodiac_report_error(resolver->build_data,
                                    Zodiac_Error_Kind::INCOMPLETE_SWITCH,
                                    ast_stmt,
                                    "Unhandled enum value: %s.%s",
                                    edecl->identifier->atom.data,
                                    emem->identifier->atom.data);
            }

            return false;
        }

        return true;
    }

    bool resolver_import_from_static_if(Resolver *resolver, AST_Declaration *decl, Scope *scope)
    {
        while (scope->kind == Scope_Kind::STATIC_IF) {
            assert(scope->parent);
            scope = scope->parent;
        }

        bool is_global = scope->kind == Scope_Kind::GLOBAL ||
                         scope->kind == Scope_Kind::MODULE;

        if (decl->identifier) {
            auto redecl = scope_find_declaration(scope, decl->identifier);
            if (redecl) {
                zodiac_report_error(resolver->build_data, Zodiac_Error_Kind::REDECLARATION,
                                    decl->identifier, "Redelaration of identifier: '%s'",
                                    decl->identifier->atom.data);
                zodiac_report_info(resolver->build_data, redecl->identifier,
                                   "Previous declaration was here");
                return false;
            }
        }

        // resolver->progression.scope_imports_done = true;

        scope_add_declaration(scope, decl);
        // decl->decl_flags |= AST_DECL_FLAG_IMPORTED_FROM_STATIC_IF;
        if (is_global) decl->decl_flags |= AST_DECL_FLAG_GLOBAL;
        return true;
    }

    AST_Type *find_or_create_enum_type(Resolver *resolver, AST_Declaration *enum_decl,
                                       AST_Type *base_type, Scope *mem_scope,
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
        }

        assert(enum_type);
        return enum_type;
    }

    bool all_dependencies_emitted(Resolver *resolver, AST_Declaration *decl)
    {
        assert(decl->kind == AST_Declaration_Kind::FUNCTION);

        assert(!(decl->decl_flags & AST_DECL_FLAG_CHECKING_DEPENDECIES));

        decl->decl_flags |= AST_DECL_FLAG_CHECKING_DEPENDECIES;


        if (!(decl->decl_flags & AST_DECL_FLAG_EMITTED_BYTECODE)) {
            decl->decl_flags &= ~AST_DECL_FLAG_CHECKING_DEPENDECIES;
            return false;
        }

        bool result = true;

        for (int64_t i = 0; i < decl->function.called_functions.count; i++) {
            AST_Declaration *called_func = decl->function.called_functions[i];

            if (!(called_func->decl_flags & AST_DECL_FLAG_EMITTED_BYTECODE)) {
                result = false;
                break;
            }
        }

        if (result) {
            for (int64_t i = 0; i < decl->function.called_functions.count; i++) {
                AST_Declaration *called_func = decl->function.called_functions[i];

                if (called_func->decl_flags & AST_DECL_FLAG_CHECKING_DEPENDECIES) {
                    continue;
                }

                if (!all_dependencies_emitted(resolver, called_func)) {
                    result = false;
                    break;
                }
            }
        }

        decl->decl_flags &= ~AST_DECL_FLAG_CHECKING_DEPENDECIES;
        return result;
    }

    void resolver_check_circular_dependencies(Resolver *resolver)
    {
        if (!queue_count(&resolver->resolve_jobs)) return;

        auto ta = temp_allocator_get();
        // temp_allocator_reset(ta);

        auto queue = &resolver->resolve_jobs;

        for (int64_t i = 0; i < queue->used; i++) {
            auto index = queue->front + i;
            if (index >= queue->capacity) index -= queue->capacity;

            Resolve_Job *job = &queue->buffer[index];
            AST_Node *ast_node = job->ast_node;

            auto nodes_in_chain = array_create<AST_Node *>(ta, 4);
            array_append(&nodes_in_chain, ast_node);

            AST_Node *waiting_on = ast_node->waiting_on;
            int64_t dup_index = -1;

            while (waiting_on) {
                auto idx = array_index_of(&nodes_in_chain, waiting_on);
                if (idx != -1) {
                    dup_index = idx;
                    // array_append(&nodes_in_chain, waiting_on);
                    break;
                } else {
                    array_append(&nodes_in_chain, waiting_on);
                }

                waiting_on = waiting_on->waiting_on;
            }

            if (dup_index != -1) {
                AST_Node *first_err_node = nodes_in_chain[dup_index];
                zodiac_report_error(resolver->build_data, Zodiac_Error_Kind::CIRCULAR_DEPENDENCY,
                                    first_err_node, "Circular dependency detected");

                for (int64_t i = dup_index; i < nodes_in_chain.count; i++) {
                    AST_Node *w = nodes_in_chain[i];
                    // zodiac_report_info(resolver->build_data, w, "...");
                    auto waiter = static_cast<AST_Declaration *>(w);
                    auto wait_on = static_cast<AST_Declaration *>(waiter->waiting_on);
                    zodiac_report_info(resolver->build_data, waiter,
                                       "Declaration '%s' is waiting on declaration '%s'",
                                       waiter->identifier->atom.data,
                                       wait_on->identifier->atom.data);
                }

                break;
            }
        }

    }

    bool fatal_error_reported(Resolver *resolver)
    {
        auto bd = resolver->build_data;

        for (int64_t i = 0; i < bd->errors.count; i++) {
            if (bd->errors[i].kind == Zodiac_Error_Kind::UNIMPLEMENTED) {
                return true;
            }
        }

        return false;
    }

    String find_module_path(Resolver *resolver, String module_name)
    {
        auto ta = temp_allocator_get();

        String file_name = string_append(ta, module_name, string_ref(".zdc"));

        String candidate_dirs[] = {
            resolver->first_file_dir,
            resolver->module_dir,
        };

        bool found = false;
        String file_path = {};

        for (uint64_t i = 0; i < STATIC_ARRAY_LENGTH(candidate_dirs); i++) {

            String candidate_file_path = string_append(ta, candidate_dirs[i], file_name);

            if (is_regular_file(candidate_file_path)) {
                found = true;
                file_path = candidate_file_path;
                break;
            }
        }

        assert(found);

        return file_path;
    }

    String find_zodiac_root(Allocator *allocator, Build_Data *build_data)
    {
        String exe_path = build_data->options->zodiac_exe_path;

        assert(is_regular_file(exe_path));

        auto ta = temp_allocator_get();

        String current_dir = get_file_dir(ta, exe_path);

        bool found = false;
        while (true) {
            assert(is_directory(current_dir));

            String current_dir_name = get_dir_name(ta, current_dir);

            if (string_equal(current_dir_name, "zodiac")) {
                found = true;
                break;
            }

            current_dir.length -= current_dir_name.length;

#ifdef linux
            current_dir.length -= 1;
            assert(string_ends_with(current_dir, "/"));
#elif WIN32
            current_dir.length -= 1;
            assert(string_ends_with(current_dir, "\\"));
#else
            assert(false);
#endif
        }

        assert(found);

        String result = string_copy(allocator, current_dir);

        String module_path = string_append(ta, result, "modules/");
        if (!is_directory(module_path)) {
            assert(false && "Failed to find module path");
        }

        return result;
    }
}
