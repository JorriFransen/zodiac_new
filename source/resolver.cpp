#include "resolver.h"

#include "builtin.h"
#include "const_interpreter.h"
#include "interpreter.h"
#include "os.h"
#include "temp_allocator.h"

namespace Zodiac
{
    void resolver_init(Allocator *allocator, Resolver *resolver, Build_Data *build_data,
                       String first_file_path)
    {
        resolver->allocator = allocator;
        resolver->build_data = build_data;

        resolver->lexer = lexer_create(allocator, build_data);
        resolver->parser = parser_create(allocator, build_data);
        resolver->bytecode_builder = bytecode_builder_create(allocator, build_data);
        resolver->llvm_builder = llvm_builder_create(allocator, build_data);

        queue_init(allocator, &resolver->parse_jobs);
        queue_init(allocator, &resolver->resolve_jobs);
        queue_init(allocator, &resolver->size_jobs);
        queue_init(allocator, &resolver->bytecode_jobs);
        queue_init(allocator, &resolver->run_jobs);
        queue_init(allocator, &resolver->llvm_jobs);

        array_init(allocator, &resolver->parsed_modules);

        resolver->global_scope = scope_new(allocator, Scope_Kind::GLOBAL, nullptr);

        resolver->entry_decl = nullptr;

        auto builtin_declarations = builtin_populate_scope(allocator, resolver->global_scope);
        assert(builtin_declarations.count);

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
        auto entry_file_path = string_append(allocator, resolver->first_file_dir, "entry.zdc");
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

        Resolve_Result result = {};

        bool progressed = false;
        uint64_t cycle_count = 0;

        while (!done) {

            if (resolver->build_data->options->verbose) {
                printf("[RESOLVER] Starting resolve cycle: %lu\n", cycle_count);
            }

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

                    assert(pm.ast->kind == AST_Node_Kind::MODULE);
                    AST_Module *ast_module = pm.ast;

                    for (int64_t i = 0; i < ast_module->declarations.count; i++) {
                        AST_Declaration *decl = ast_module->declarations[i];
                        queue_resolve_job(resolver, decl);
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
                            assert(decl->decl_flags & AST_DECL_FLAG_GLOBAL);
                            queue_bytecode_job(resolver, decl);
                        } else {
                            assert(decl->kind == AST_Declaration_Kind::IMPORT ||
                                   decl->kind == AST_Declaration_Kind::USING ||
                                   decl->kind == AST_Declaration_Kind::STRUCTURE ||
                                   decl->kind == AST_Declaration_Kind::TYPEDEF);
                        }
                    } else {
                        assert(false);
                    }

                    progressed = true;
                }
            }

            auto bytecode_job_count = queue_count(&resolver->bytecode_jobs);
            while (bytecode_job_count--) {
                auto job = queue_dequeue(&resolver->bytecode_jobs);

                if (job.decl->kind == AST_Declaration_Kind::FUNCTION) {

                    bytecode_register_function(&resolver->bytecode_builder, job.decl);
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
                    Bytecode_Function *wrapper =
                        bytecode_emit_run_wrapper(&resolver->bytecode_builder, job.decl);
                    queue_run_job(resolver, job.decl, wrapper);
                } else {
                    assert(false);
                }


                progressed = true;
            }

            auto run_job_count = queue_count(&resolver->run_jobs);
            while (run_job_count--) {
                auto job = queue_dequeue(&resolver->run_jobs);

                assert(job.wrapper);
                assert(job.wrapper->flags & BC_FUNC_FLAG_EMITTED);

                Interpreter interp = interpreter_create(resolver->allocator,
                                                        resolver->build_data);

                interpreter_start(&interp, job.wrapper,
                                  resolver->bytecode_builder.global_data_size,
                                  resolver->bytecode_builder.globals,
                                  resolver->bytecode_builder.foreign_functions);

                printf("Interpreter exited with code: %" PRId64 " after run directive\n",
                       interp.exit_code);

                progressed = true;
            }

            auto llvm_job_count = queue_count(&resolver->llvm_jobs);
            while (llvm_job_count--) {
                auto job = queue_dequeue(&resolver->llvm_jobs);

                switch (job.kind) {
                    case LLVM_Job_Kind::FUNCTION: {
                        auto bc_func = job.bc_func;

                        llvm_register_function(&resolver->llvm_builder, bc_func);
                        llvm_emit_function(&resolver->llvm_builder, bc_func);

                        if (bc_func->flags & BC_FUNC_FLAG_CRT_ENTRY) {
                            llvm_emit_binary(&resolver->llvm_builder,
                                             resolver->build_data->options->exe_file_name.data);
                        }

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

            if (queue_count(&resolver->parse_jobs)    == 0 &&
                queue_count(&resolver->resolve_jobs)  == 0 &&
                queue_count(&resolver->size_jobs)     == 0 &&
                queue_count(&resolver->bytecode_jobs) == 0 &&
                queue_count(&resolver->llvm_jobs)     == 0) {
                done = true;
            }

            if (fatal_error_reported(resolver) || !progressed) {
                done = true;
            } else {
                zodiac_clear_errors(resolver->build_data);
            }

            progressed = false;
            cycle_count++;
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

        if (resolver->build_data->options->verbose)
            printf("[RESOLVER] Queueing parse job for file: '%s'\n", module_path.data);

        Parse_Job job = { .module_name = string_copy(resolver->allocator, module_name),
                          .module_path = string_copy(resolver->allocator, module_path),
                          .insert_entry_module = insert_entry_module,
                          .parsed_module_index = -1,
        };

        queue_enqueue(&resolver->parse_jobs, job);
        return true;
    }

    void queue_resolve_job(Resolver *resolver, AST_Node *ast_node)
    {
        assert(ast_node->kind == AST_Node_Kind::DECLARATION);

        if (resolver->build_data->options->verbose) {
            auto decl = static_cast<AST_Declaration *>(ast_node);
            if (decl->identifier) {
                printf("[RESOLVER] Queueing resolve job for declaration: '%s'\n",
                       decl->identifier->atom.data);
            } else if (decl->kind == AST_Declaration_Kind::USING) {
                printf("[RESOLVER] Queueing resolve job for using: '");
                ast_print_expression(decl->using_decl.ident_expr, 0);
                printf("'\n");
            } else if (decl->kind == AST_Declaration_Kind::RUN) {
                printf("[RESOLVER] Queueing resolve job for #run: '");
                ast_print_expression(decl->run.expression, 0);
                printf("'\n");
            } else {
                assert(false);
            }
        }

        Resolve_Job job = { .ast_node = ast_node, };
        queue_enqueue(&resolver->resolve_jobs, job);
    }

    void queue_size_job(Resolver *resolver, AST_Node *node)
    {
        assert(node->kind == AST_Node_Kind::DECLARATION);

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
        LLVM_Job job = { .kind = LLVM_Job_Kind::FUNCTION, .bc_func = bc_func };
        queue_enqueue(&resolver->llvm_jobs, job);
    }

    void queue_llvm_job(Resolver *resolver, Bytecode_Global_Info bc_global)
    {
        LLVM_Job job = { .kind = LLVM_Job_Kind::GLOBAL, .bc_global = bc_global };
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

            Lexed_File lf_entry = lexer_lex_file(&resolver->lexer,resolver->entry_module_path);

            if (!lf_entry.valid) return false;

            Token_Stream *ets = lexer_new_token_stream(resolver->allocator, &lf_entry);

            epf = parser_parse_file(&resolver->parser, ets);

            if (!epf.valid) return false;

            lexer_free_lexed_file(&resolver->lexer, &lf_entry);
            ets->free();
        }

        Lexed_File lexed_file = lexer_lex_file(&resolver->lexer, job->module_path);

        if (!lexed_file.valid) return false;

        Token_Stream *token_stream = lexer_new_token_stream(resolver->allocator, &lexed_file);

        Parsed_File parsed_file = {};

        if (insert_epf) {
            parsed_file = epf;
        } else {
            parsed_file_init(&resolver->parser, &parsed_file);
        }

        parser_parse_file(&resolver->parser, token_stream, &parsed_file);

        if (!parsed_file.valid) return false;

        if (resolver->build_data->options->print_parse_tree) parsed_file_print(&parsed_file);

        AST_Builder ast_builder = { resolver->allocator, resolver->build_data };
        stack_init(resolver->allocator, &ast_builder.break_stack);

        AST_Module *module_ast = ast_create_from_parsed_file(&ast_builder, &parsed_file,
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

        if (resolver->build_data->options->verbose) {
            if (decl->identifier) {
            printf("[RESOLVER] Trying to resolve declaration: '%s'\n",
                   decl->identifier->atom.data);
            } else if (decl->kind == AST_Declaration_Kind::USING) {
                printf("[RESOLVER] Trying to resolve using: '");
                ast_print_expression(decl->using_decl.ident_expr, 0);
                printf("'\n");
            } else if (decl->kind == AST_Declaration_Kind::RUN) {
                printf("[RESOLVER] Trying to resolve #run: '");
                ast_print_expression(decl->run.expression, 0);
                printf("'\n");
            } else {
                assert(false);
            }
        }

        assert(decl->flat);

        bool result = true;

        AST_Node *waiting_on = nullptr;

        for (int64_t i = decl->flat->waiting_on; i < decl->flat->nodes.count; i++) {
            AST_Node *node = decl->flat->nodes[i];

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
                waiting_on = node;
                decl->flat->waiting_on = i;
                break;
            }
        }

        if (!result && resolver->build_data->options->verbose) {
            assert(waiting_on);
            auto bfp = waiting_on->begin_file_pos;
            printf("           ..Failed! (waiting on: '%s:%lu:%lu')\n",
                    bfp.file_name.data, bfp.line, bfp.column);
        }

        return result;
    }

    bool try_size_job(Resolver *resolver, Size_Job *job)
    {
        assert(job->ast_node->kind == AST_Node_Kind::DECLARATION);
        AST_Declaration *decl = static_cast<AST_Declaration *>(job->ast_node);
        assert(decl->type ||
               decl->kind == AST_Declaration_Kind::IMPORT ||
               decl->kind == AST_Declaration_Kind::USING);
        assert(decl->flags & AST_NODE_FLAG_RESOLVED_ID);
        assert(decl->flags & AST_NODE_FLAG_TYPED);

        // if (resolver->build_data->options->verbose) {
        //     if (decl->identifier) {
        //     printf("[RESOLVER] Trying to size declaration: '%s'\n",
        //            decl->identifier->atom.data);
        //     } else if (decl->kind == AST_Declaration_Kind::RUN) {
        //         printf("[RESOLVER] Trying to size #run: '");
        //         ast_print_expression(decl->run.expression, 0);
        //         printf("'\n");
        //     } else {
        //         assert(false);
        //     }
        // }

        bool result = true;

        for (int64_t i = 0; i < decl->flat->nodes.count; i++) {
            AST_Node *node = decl->flat->nodes[i];

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

                auto ta = temp_allocator_get();

                Atom _module_name = ident_expr->identifier->atom;

                String module_name = string_ref(_module_name);
                String file_name = string_append(ta, module_name, string_ref(".zdc"));
                String file_path = string_append(ta, resolver->first_file_dir, file_name);

                assert(is_regular_file(file_path));

                AST_Module *ast_module = nullptr;
                bool in_queue = queue_parse_job(resolver, module_name, file_path, &ast_module);
                if (in_queue) {
                    return false;
                }

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

                AST_Expression *init_expr = declaration->variable.init_expression;
                if (init_expr) {
                    assert(init_expr->type);
                    assert(init_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(init_expr->flags & AST_NODE_FLAG_TYPED);
                }

                AST_Type *type = nullptr;

                if (ts) {
                    if (init_expr) {
                        if (ts->type != init_expr->type) {
                            if (ts->type->kind == AST_Type_Kind::INTEGER &&
                                init_expr->kind == AST_Expression_Kind::INTEGER_LITERAL &&
                                integer_literal_fits_in_type(init_expr->integer_literal,
                                                             ts->type)) {
                                init_expr->type = ts->type;
                            } else {
                                assert(false);
                            }
                        } else {
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

                AST_Expression *init_expr = declaration->constant.init_expression;
                assert(init_expr->type);
                assert(init_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(init_expr->flags & AST_NODE_FLAG_TYPED);

                if (ts) {
                    assert(ts->type == init_expr->type);
                }

                declaration->type = init_expr->type;
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

                for (int64_t i = 0; i < declaration->function.parameter_declarations.count; i++) {
                    AST_Declaration *param_decl = declaration->function.parameter_declarations[i];
                    assert(param_decl->type);
                    assert(param_decl->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(param_decl->flags & AST_NODE_FLAG_TYPED);
                }

                AST_Type_Spec *func_ts = declaration->function.type_spec;
                assert(func_ts->type);
                assert(func_ts->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(func_ts->flags & AST_NODE_FLAG_TYPED);

                AST_Statement *body = declaration->function.body;
                assert(body->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(body->flags & AST_NODE_FLAG_TYPED);

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
                assert(declaration->structure.parameters.count == 0);

                Array<AST_Type *> member_types = {};
                array_init(resolver->allocator, &member_types,
                           declaration->structure.member_declarations.count);

                for (int64_t i = 0; i < declaration->structure.member_declarations.count; i++) {
                    AST_Declaration *mem_decl = declaration->structure.member_declarations[i];
                    assert(mem_decl->type);
                    assert(mem_decl->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(mem_decl->flags & AST_NODE_FLAG_TYPED);

                    array_append(&member_types, mem_decl->type);
                }

                AST_Type *struct_type =
                    ast_structure_type_new(resolver->allocator, declaration, member_types,
                                           declaration->structure.member_scope);
                assert(struct_type);

                struct_type->flags |= AST_NODE_FLAG_RESOLVED_ID;
                struct_type->flags |= AST_NODE_FLAG_TYPED;
                array_append(&resolver->build_data->type_table, struct_type);


                declaration->type = struct_type;
                declaration->flags |= AST_NODE_FLAG_RESOLVED_ID;
                declaration->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Declaration_Kind::ENUM: assert(false);
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

            case AST_Declaration_Kind::STATIC_IF: assert(false);
            case AST_Declaration_Kind::STATIC_ASSERT: assert(false);
        }
    }

    bool try_resolve_statement(Resolver *resolver, AST_Statement *statement)
    {
        switch (statement->kind) {
            case AST_Statement_Kind::INVALID: assert(false);

            case AST_Statement_Kind::BLOCK: {
                for (int64_t i = 0; i < statement->block.statements.count; i++) {
                    auto mem_stmt = statement->block.statements[i];
                    assert(mem_stmt->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(mem_stmt->flags & AST_NODE_FLAG_TYPED);
                }

                statement->flags |= AST_NODE_FLAG_RESOLVED_ID;
                statement->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Statement_Kind::ASSIGNMENT: {
                AST_Expression *ident_expr = statement->assignment.identifier_expression;
                assert(ident_expr->type);
                assert(ident_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(ident_expr->flags & AST_NODE_FLAG_TYPED);

                AST_Expression *rhs_expr = statement->assignment.rhs_expression;
                assert(rhs_expr->type);
                assert(rhs_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(rhs_expr->flags & AST_NODE_FLAG_TYPED);

                assert(ident_expr->type == rhs_expr->type);

                statement->flags |= AST_NODE_FLAG_RESOLVED_ID;
                statement->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Statement_Kind::RETURN: {
                AST_Expression *operand_expr = statement->expression;
                if (operand_expr) {
                    assert(operand_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(operand_expr->flags & AST_NODE_FLAG_TYPED);
                    assert(operand_expr->type);
                }

                statement->flags |= AST_NODE_FLAG_RESOLVED_ID;
                statement->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Statement_Kind::BREAK: {
                AST_Statement *break_from = statement->break_stmt.break_from;
                assert(break_from);
                assert(break_from->kind == AST_Statement_Kind::WHILE);

                statement->flags |= AST_NODE_FLAG_RESOLVED_ID;
                statement->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Statement_Kind::DECLARATION: {
                AST_Declaration *decl = statement->declaration;
                assert(decl->type);
                assert(decl->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(decl->flags & AST_NODE_FLAG_TYPED);

                statement->flags |= AST_NODE_FLAG_RESOLVED_ID;
                statement->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Statement_Kind::EXPRESSION: {
                AST_Expression *expr = statement->expression;
                assert(expr->type);
                assert(expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(expr->flags & AST_NODE_FLAG_TYPED);

                statement->flags |= AST_NODE_FLAG_RESOLVED_ID;
                statement->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Statement_Kind::WHILE: {
                AST_Expression *cond_expr = statement->while_stmt.cond_expr;
                assert(cond_expr->type);
                assert(cond_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(cond_expr->flags & AST_NODE_FLAG_TYPED);

                assert(cond_expr->type->kind == AST_Type_Kind::BOOL);

                AST_Statement *body = statement->while_stmt.body;
                assert(body->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(body->flags & AST_NODE_FLAG_TYPED);

                statement->flags |= AST_NODE_FLAG_RESOLVED_ID;
                statement->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Statement_Kind::FOR: assert(false);

            case AST_Statement_Kind::IF:
            {
                AST_Expression *cond_expr = statement->if_stmt.cond_expr;
                assert(cond_expr->type);
                assert(cond_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(cond_expr->flags & AST_NODE_FLAG_TYPED);

                assert(cond_expr->type->kind == AST_Type_Kind::BOOL);

                AST_Statement *then_stmt = statement->if_stmt.then_stmt;
                assert(then_stmt->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(then_stmt->flags & AST_NODE_FLAG_TYPED);

                AST_Statement *else_stmt = statement->if_stmt.else_stmt;
                if (else_stmt) {
                    assert(else_stmt->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(else_stmt->flags & AST_NODE_FLAG_TYPED);
                }

                statement->flags |= AST_NODE_FLAG_RESOLVED_ID;
                statement->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Statement_Kind::SWITCH: assert(false);
        }
    }

    bool try_resolve_expression(Resolver *resolver, AST_Expression *expression)
    {
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

                if (!(decl->flags & AST_NODE_FLAG_RESOLVED_ID)) {
                    return false;
                }

                if (!(decl->flags & AST_NODE_FLAG_TYPED)) {
                    return false;
                }

                if (decl->kind != AST_Declaration_Kind::IMPORT) {
                    assert(decl->type);
                    expression->type = decl->type;
                }

                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);

            case AST_Expression_Kind::DOT: {
                AST_Expression *parent_expr = expression->dot.parent_expression;
                assert(parent_expr->flags |= AST_NODE_FLAG_RESOLVED_ID);
                assert(parent_expr->flags |= AST_NODE_FLAG_TYPED);

                AST_Declaration *parent_decl = nullptr;
                if (parent_expr->kind == AST_Expression_Kind::IDENTIFIER) {
                    parent_decl = parent_expr->identifier->declaration;
                } else if (parent_expr->kind == AST_Expression_Kind::DOT) {
                    parent_decl = parent_expr->dot.parent_decl;
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
                    assert(child_decl);
                    if (!(child_decl->flags & AST_NODE_FLAG_RESOLVED_ID)) {
                        return false;
                    }
                    assert(child_decl->type);
                    assert(child_decl->flags & AST_NODE_FLAG_TYPED);

                    expression->dot.child_decl = child_decl;
                    expression->type = child_decl->type;
                    expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                    expression->flags |= AST_NODE_FLAG_TYPED;
                    return true;
                } else {
                    assert(parent_decl->type);
                    AST_Type *struct_type = parent_decl->type;
                    if (struct_type->kind != AST_Type_Kind::STRUCTURE) {
                        assert(struct_type->kind == AST_Type_Kind::POINTER);
                        struct_type = struct_type->pointer.base;
                        assert(struct_type->kind == AST_Type_Kind::STRUCTURE);
                    }

                    assert(struct_type);

                    assert(struct_type->structure.member_scope);
                    auto mem_scope = struct_type->structure.member_scope;
                    assert(mem_scope->kind == Scope_Kind::AGGREGATE);

                    AST_Declaration *child_decl = nullptr;
                    if (!child_ident->declaration) {
                        child_decl = scope_find_declaration(mem_scope, child_ident);
                        assert(child_decl);
                        assert(child_decl->type);
                        assert(child_decl->flags & AST_NODE_FLAG_RESOLVED_ID);
                        assert(child_decl->flags & AST_NODE_FLAG_TYPED);

                        auto struct_decl = struct_type->structure.declaration;
                        assert(struct_decl->kind == AST_Declaration_Kind::STRUCTURE);

                        bool index_found = false;
                        int64_t index = -1;
                        for (int64_t i = 0;
                             i < struct_decl->structure.member_declarations.count;
                             i++) {
                            if (child_decl == struct_decl->structure.member_declarations[i]) {
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
                        return true;

                    } else {
                        assert(false);
                    }

                    assert(false);
                }

                assert(false);
                break;
            }

            case AST_Expression_Kind::BINARY: {
                AST_Expression *lhs = expression->binary.lhs;
                assert(lhs->type);
                assert(lhs->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(lhs->flags & AST_NODE_FLAG_TYPED);

                AST_Expression *rhs = expression->binary.rhs;
                assert(rhs->type);
                assert(rhs->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(rhs->flags & AST_NODE_FLAG_TYPED);

                bool is_compare = binop_is_cmp(expression->binary.op);

                AST_Type *result_type = nullptr;

                if (lhs->type != rhs->type) {

                    if (lhs->kind != AST_Expression_Kind::INTEGER_LITERAL &&
                        rhs->kind == AST_Expression_Kind::INTEGER_LITERAL &&
                        integer_literal_fits_in_type(rhs->integer_literal, lhs->type)) {

                                rhs->type = lhs->type;

                    } else if (lhs->type->kind == AST_Type_Kind::INTEGER &&
                               rhs->type->kind == AST_Type_Kind::INTEGER){

                        if (lhs->type->bit_size == rhs->type->bit_size) {
                            assert(false);
                        } else if (lhs->type->bit_size > rhs->type->bit_size) {
                            assert(false);
                        } else if (rhs->type->bit_size > lhs->type->bit_size) {
                            if (lhs->type->integer.sign) assert(rhs->type->integer.sign);
                            result_type = rhs->type;
                            expression->binary.lhs =
                                ast_cast_expression_new(resolver->allocator, lhs,
                                                        result_type, lhs->scope,
                                                        lhs->begin_file_pos,
                                                        lhs->begin_file_pos);
                            lhs = expression->binary.lhs;
                            if (!try_resolve_expression(resolver, lhs)) assert(false);
                        } else {
                            assert(false);
                        }

                    } else if (rhs->type->kind == AST_Type_Kind::INTEGER &&
                               lhs->type->kind == AST_Type_Kind::FLOAT) {
                        assert(lhs->type->bit_size >= rhs->type->bit_size);
                        result_type = lhs->type;
                        expression->binary.rhs =
                            ast_cast_expression_new(resolver->allocator, rhs,
                                                    result_type, rhs->scope,
                                                    rhs->begin_file_pos,
                                                    rhs->end_file_pos);
                        rhs = expression->binary.rhs;
                        if (!try_resolve_expression(resolver, rhs)) assert(false);
                    } else if (lhs->type == Builtin::type_double &&
                               rhs->type == Builtin::type_float) {
                        if (rhs->kind == AST_Expression_Kind::FLOAT_LITERAL) {
                            result_type = Builtin::type_double;
                            rhs->type = Builtin::type_double;
                        } else {
                            assert(false);
                        }
                    } else {
                        assert(false);
                    }
                }

                if (is_compare) {
                    result_type = Builtin::type_bool;
                } else if (!result_type) {
                    result_type = lhs->type;
                }
                assert(result_type);

                expression->type = result_type;
                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                return true;
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
                        assert(op_expr->type->kind == AST_Type_Kind::INTEGER);
                        assert(op_expr->type->integer.sign);
                        result_type = op_expr->type;
                        break;
                    }
                }

                assert(result_type);

                expression->type = result_type;
                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                return true;
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
                assert(callee_decl->type);
                assert(callee_decl->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(callee_decl->flags & AST_NODE_FLAG_TYPED);

                expression->call.callee_declaration = callee_decl;

                auto args = expression->call.arg_expressions;
                auto params = callee_decl->function.parameter_declarations;

                assert(args.count == params.count);
                for (int64_t i = 0; i < args.count; i++) {
                    AST_Expression *arg_expr = args[i];
                    assert(arg_expr->type);
                    if (arg_expr->type != params[i]->type) {
                        if (is_valid_type_conversion(arg_expr->type, params[i]->type)) {
                            args[i] = ast_cast_expression_new(resolver->allocator,
                                                              arg_expr, params[i]->type,
                                                              arg_expr->scope,
                                                              arg_expr->begin_file_pos,
                                                              arg_expr->end_file_pos);
                            bool cast_res = try_resolve_expression(resolver, args[i]);
                            assert(cast_res);
                            arg_expr = args[i];
                        } else {
                            assert(false);
                        }
                    }
                }

                expression->type = callee_decl->type->function.return_type;
                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Expression_Kind::BUILTIN_CALL: {

                auto args = expression->builtin_call.arg_expressions;
                for (int64_t i = 0; i < args.count; i++) {
                    AST_Expression *arg_expr = args[i];
                    assert(arg_expr->type);
                    assert(arg_expr->flags |= AST_NODE_FLAG_RESOLVED_ID);
                    assert(arg_expr->flags |= AST_NODE_FLAG_TYPED);
                }

                Atom name = expression->builtin_call.identifier->atom;

                if (name == Builtin::atom_exit) {
                    assert(args.count == 1);
                    assert(args[0]->type == Builtin::type_s64);

                    expression->type = Builtin::type_void;
                    expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                    expression->flags |= AST_NODE_FLAG_TYPED;
                    return true;

                } else if (name == Builtin::atom_syscall) {
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
                            bool cast_res = try_resolve_expression(resolver, args[i]);
                            assert(cast_res);
                        } else {
                            assert(args[i]->type == Builtin::type_s64);
                        }
                    }

                    expression->type = Builtin::type_s64;
                    expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                    expression->flags |= AST_NODE_FLAG_TYPED;
                    return true;
                } else if (name == Builtin::atom_cast) {
                    assert(args.count == 2);

                    AST_Expression *type_expr = args[0];
                    AST_Expression *op_expr = args[1];

                    assert(type_expr->type);
                    assert(type_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(type_expr->flags & AST_NODE_FLAG_TYPED);

                    assert(op_expr->type);
                    assert(op_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(op_expr->flags & AST_NODE_FLAG_TYPED);

                    expression->type = type_expr->type;
                    expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                    expression->flags |= AST_NODE_FLAG_TYPED;
                    return true;
                } else {
                    zodiac_report_error(resolver->build_data,
                                        Zodiac_Error_Kind::UNIMPLEMENTED,
                                        expression,
                                        "Builtin call '%s' is not implemented!",
                                        name.data);
                    return false;
                }

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
                return true;
                break;
            }

            case AST_Expression_Kind::COMPOUND: assert(false);

            case AST_Expression_Kind::SUBSCRIPT: {
                AST_Expression *index_expr = expression->subscript.index_expression;
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
                return true;
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
                    return true;
                } else {
                    return false;
                }

                break;
            }

            case AST_Expression_Kind::INTEGER_LITERAL: {
                assert(!expression->type);
                expression->type = Builtin::type_s64;
                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Expression_Kind::FLOAT_LITERAL: {
                assert(!expression->type);
                expression->type = Builtin::type_float;
                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Expression_Kind::STRING_LITERAL: {
                assert(!expression->type);
                expression->type = Builtin::type_ptr_u8;
                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Expression_Kind::CHAR_LITERAL: {
                assert(!expression->type);
                expression->type = Builtin::type_u8;
                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Expression_Kind::BOOL_LITERAL: {
                assert(!expression->type);
                expression->type = Builtin::type_bool;
                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Expression_Kind::NULL_LITERAL: {
                assert(expression->infer_type_from);
                AST_Node *_infer_type_from = expression->infer_type_from;
                assert(_infer_type_from->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(_infer_type_from->flags & AST_NODE_FLAG_TYPED);

                AST_Type *result_type = nullptr;

                switch (_infer_type_from->kind) {
                    case AST_Node_Kind::INVALID: assert(false);
                    case AST_Node_Kind::MODULE: assert(false);
                    case AST_Node_Kind::IDENTIFIER: assert(false);
                    case AST_Node_Kind::DECLARATION: assert(false);
                    case AST_Node_Kind::SWITCH_CASE: assert(false);
                    case AST_Node_Kind::STATEMENT: assert(false);
                    case AST_Node_Kind::EXPRESSION: assert(false);

                    case AST_Node_Kind::TYPE_SPEC: {
                        auto ts = static_cast<AST_Type_Spec *>(_infer_type_from);
                        assert(ts->type);
                        assert(ts->type->kind == AST_Type_Kind::POINTER);
                        result_type = ts->type;
                        break;
                    }

                    case AST_Node_Kind::TYPE: assert(false);
                }

                assert(result_type);
                expression->type = result_type;
                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Expression_Kind::RANGE: assert(false);
        }
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

                if (!(decl->flags & AST_NODE_FLAG_RESOLVED_ID)) return false;
                assert(decl->flags & AST_NODE_FLAG_TYPED);
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

            case AST_Type_Spec_Kind::DOT: assert(false);

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
            case AST_Type_Spec_Kind::FROM_TYPE: assert(false);
        }
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

            case AST_Declaration_Kind::VARIABLE: {
                assert(decl->type);
                assert(decl->type->flags & AST_NODE_FLAG_SIZED);
                decl->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Declaration_Kind::CONSTANT: {
                assert(decl->type);
                assert(decl->type->flags & AST_NODE_FLAG_SIZED);
                decl->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
                break;
            }

            case AST_Declaration_Kind::PARAMETER: {
                assert(decl->type);
                assert(decl->type->flags & AST_NODE_FLAG_SIZED);
                decl->flags |= AST_NODE_FLAG_SIZED;
                return true;
            }

            case AST_Declaration_Kind::FUNCTION: {
                assert(decl->type);
                assert(decl->type->flags & AST_NODE_FLAG_SIZED);
                decl->flags |= AST_NODE_FLAG_SIZED;

                if (!(decl->decl_flags & AST_DECL_FLAG_QUEUED_BYTECODE)) {
                    queue_bytecode_job(resolver, decl);
                }
                return true;
            }

            case AST_Declaration_Kind::TYPE: assert(false);

            case AST_Declaration_Kind::TYPEDEF: {
                assert(decl->type);
                if (!(decl->type->flags & AST_NODE_FLAG_SIZED)) {
                    bool result = try_size_type(resolver, decl->type);
                    assert(result);
                }
                decl->flags |= AST_NODE_FLAG_SIZED;
                return true;
            }

            case AST_Declaration_Kind::STRUCTURE: {
                assert(decl->type);
                if (!(decl->type->flags & AST_NODE_FLAG_SIZED)) {
                    bool result = try_size_type(resolver, decl->type);
                    assert(result);
                }
                decl->flags |= AST_NODE_FLAG_SIZED;
                return true;
            }

            case AST_Declaration_Kind::ENUM: assert(false);
            case AST_Declaration_Kind::POLY_TYPE: assert(false);

            case AST_Declaration_Kind::RUN: {
                assert(decl->type);
                assert(decl->type->flags & AST_NODE_FLAG_SIZED);
                decl->flags |= AST_NODE_FLAG_SIZED;
                return true;
            }

            case AST_Declaration_Kind::STATIC_IF: assert(false);
            case AST_Declaration_Kind::STATIC_ASSERT: assert(false);
        }
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
                AST_Expression *ident_expr = statement->assignment.identifier_expression;
                assert(ident_expr->flags & AST_NODE_FLAG_SIZED);

                AST_Expression *rhs_expr = statement->assignment.rhs_expression;
                assert(rhs_expr->flags & AST_NODE_FLAG_SIZED);

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
                AST_Declaration *decl = statement->declaration;
                assert(decl->type);
                assert(decl->flags & AST_NODE_FLAG_SIZED);

                statement->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Statement_Kind::EXPRESSION: {
                AST_Expression *expr = statement->expression;
                assert(expr->type);
                assert(expr->flags & AST_NODE_FLAG_SIZED);

                statement->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Statement_Kind::WHILE: {
                AST_Expression *cond_expr = statement->while_stmt.cond_expr;
                assert(cond_expr->flags & AST_NODE_FLAG_SIZED);

                AST_Statement *body = statement->while_stmt.body;
                assert(body->flags & AST_NODE_FLAG_SIZED);

                statement->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Statement_Kind::FOR: assert(false);

            case AST_Statement_Kind::IF: {
                AST_Expression *cond_expr = statement->if_stmt.cond_expr;
                assert(cond_expr->flags & AST_NODE_FLAG_SIZED);

                AST_Statement *then_stmt = statement->if_stmt.then_stmt;
                assert(then_stmt->flags & AST_NODE_FLAG_SIZED);

                AST_Statement *else_stmt = statement->if_stmt.else_stmt;
                if (else_stmt) {
                    assert(else_stmt->flags & AST_NODE_FLAG_SIZED);
                }

                statement->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Statement_Kind::SWITCH: assert(false);
        }
    }

    bool try_size_expression(Resolver *resolver, AST_Expression *expression)
    {
        assert(!(expression->flags & AST_NODE_FLAG_SIZED));

        switch (expression->kind) {
            case AST_Expression_Kind::INVALID: assert(false);

            case AST_Expression_Kind::IDENTIFIER: {
                if (expression->type) {
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

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);

            case AST_Expression_Kind::DOT: {
                assert(expression->type);
                assert(expression->type->flags & AST_NODE_FLAG_SIZED);

                expression->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Expression_Kind::BINARY: {
                assert(expression->type);
                assert(expression->type->flags & AST_NODE_FLAG_SIZED);

                expression->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Expression_Kind::UNARY: {
                assert(expression->type);
                assert(expression->type->flags & AST_NODE_FLAG_SIZED);

                expression->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Expression_Kind::CALL: {
                assert(expression->type);
                assert(expression->type->flags & AST_NODE_FLAG_SIZED);

                expression->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Expression_Kind::BUILTIN_CALL: {
                assert(expression->type);
                assert(expression->type->flags & AST_NODE_FLAG_SIZED);

                expression->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Expression_Kind::ADDROF: {
                assert(expression->type);
                assert(expression->type->flags & AST_NODE_FLAG_SIZED);

                expression->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Expression_Kind::COMPOUND: assert(false);

            case AST_Expression_Kind::SUBSCRIPT: {
                assert(expression->type);
                assert(expression->type->flags & AST_NODE_FLAG_SIZED);

                expression->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Expression_Kind::CAST: assert(false);

            case AST_Expression_Kind::INTEGER_LITERAL:
            case AST_Expression_Kind::CHAR_LITERAL: {
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

            case AST_Expression_Kind::FLOAT_LITERAL: {
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

            case AST_Expression_Kind::STRING_LITERAL: {
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

            case AST_Expression_Kind::BOOL_LITERAL:
            case AST_Expression_Kind::NULL_LITERAL: {
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

            case AST_Expression_Kind::RANGE: assert(false);
        }
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
                int64_t bit_size = 0;
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

            case AST_Type_Kind::ENUM: assert(false);

            case AST_Type_Kind::ARRAY: {
                AST_Type *element_type = type->array.element_type;
                assert(element_type->flags & AST_NODE_FLAG_TYPED);

                type->bit_size = element_type->bit_size * type->array.element_count;
                type->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }
        }
    }

    bool try_size_type_spec(Resolver *resolver, AST_Type_Spec *type_spec)
    {
        switch (type_spec->kind) {
            case AST_Type_Spec_Kind::INVALID: assert(false);

            case AST_Type_Spec_Kind::IDENTIFIER: {

                AST_Declaration *decl = type_spec->identifier->declaration;
                assert(decl);
                assert(decl->type);

                assert(type_spec->type == decl->type);

                if (!(type_spec->type->flags & AST_NODE_FLAG_SIZED)) {
                    if (!try_size_type(resolver, type_spec->type)) {
                        return false;
                    }
                }

                type_spec->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Type_Spec_Kind::POINTER: {
                assert(type_spec->type);
                if (!(type_spec->type->flags & AST_NODE_FLAG_SIZED)) {
                    if (!try_size_type(resolver, type_spec->type)) {
                        return false;
                    }
                }

                type_spec->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Type_Spec_Kind::DOT: assert(false);

            case AST_Type_Spec_Kind::FUNCTION: {
                assert(type_spec->type);
                if (!(type_spec->type->flags & AST_NODE_FLAG_SIZED)) {
                    if (!try_size_type(resolver, type_spec->type)) {
                        return false;
                    }
                }

                type_spec->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Type_Spec_Kind::ARRAY: {
                assert(type_spec->type);
                if (!(type_spec->type->flags & AST_NODE_FLAG_SIZED)) {
                    if (!try_size_type(resolver, type_spec->type)) {
                        return false;
                    }
                }

                type_spec->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Type_Spec_Kind::TEMPLATED: assert(false);
            case AST_Type_Spec_Kind::POLY_IDENTIFIER: assert(false);
            case AST_Type_Spec_Kind::FROM_TYPE: assert(false);
        }
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
                }
                else assert(false);

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

            case AST_Type_Kind::POINTER:
            {
                assert(false);
                break;
            }

            case AST_Type_Kind::FUNCTION: assert(false);
            case AST_Type_Kind::STRUCTURE: assert(false);

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

    bool is_entry_decl(Resolver *resolver, AST_Declaration *decl)
    {
        if (decl->kind != AST_Declaration_Kind::FUNCTION)
            return false;

        if (resolver->llvm_builder.target_platform == Zodiac_Target_Platform::LINUX) {
            if (decl->identifier->atom == Builtin::atom__start &&
                (decl->decl_flags & AST_DECL_FLAG_IS_NAKED)) {
                return true;
            }
        } else if (resolver->llvm_builder.target_platform == Zodiac_Target_Platform::WINDOWS) {
            if (decl->identifier->atom == Builtin::atom_mainCRTStartup) {
                return true;
            }
        } else {
            assert(false);
        }

        return false;
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
}
