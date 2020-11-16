#include "resolver.h"

#include "builtin.h"
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

        auto first_file_dir = get_file_dir(ta, first_file_path);
        auto entry_file_path = string_append(allocator, first_file_dir, "entry.zdc");
        assert(is_regular_file(entry_file_path));

        resolver->entry_module_path = entry_file_path;

        queue_parse_job(resolver, first_file_name, first_file_path, true);
    }

    void start_resolving(Resolver *resolver)
    {
        // Don't do anything when not running threaded
    }

    Resolve_Result finish_resolving(Resolver *resolver)
    {
        bool done = false;

        Resolve_Result result = {};

        while (!done) {

            auto parse_job_count = queue_count(&resolver->parse_jobs);
            while (parse_job_count--) {
                auto job = queue_dequeue(&resolver->parse_jobs);

                if (!try_parse_job(resolver, &job)) {
                    result.parse_error = true;
                    done = true;
                    break;
                } else {
                    assert(job.parsed_module_index >= 0);
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
                break;
            }

            auto resolve_job_count = queue_count(&resolver->resolve_jobs);
            while (resolve_job_count--) {
                auto job = queue_dequeue(&resolver->resolve_jobs);

                if (!try_resolve_job(resolver, &job)) {
                    queue_enqueue(&resolver->resolve_jobs, job);
                } else {
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
                            queue_bytecode_job(resolver, decl);
                        }
                    } else {
                        assert(false);
                    }
                }
            }

            auto bytecode_job_count = queue_count(&resolver->bytecode_jobs);
            while (bytecode_job_count--) {
                auto job = queue_dequeue(&resolver->bytecode_jobs);

                bytecode_register_function(&resolver->bytecode_builder, job.decl);
                auto bc_func = bytecode_emit_function_declaration(&resolver->bytecode_builder,
                                                                  job.decl);
                assert(bc_func);

                queue_llvm_job(resolver, bc_func);
            }

            auto llvm_job_count = queue_count(&resolver->llvm_jobs);
            while (llvm_job_count--) {
                auto job = queue_dequeue(&resolver->llvm_jobs);

                auto bc_func = job.bc_func;

                llvm_register_function(&resolver->llvm_builder, bc_func);
                llvm_emit_function(&resolver->llvm_builder, bc_func);

                if (bc_func->flags & BC_FUNC_FLAG_CRT_ENTRY) {
                    llvm_emit_binary(&resolver->llvm_builder,
                                     resolver->build_data->options->exe_file_name.data);
                }
            }

            if (queue_count(&resolver->parse_jobs)    == 0 &&
                queue_count(&resolver->resolve_jobs)  == 0 &&
                queue_count(&resolver->size_jobs)     == 0 &&
                queue_count(&resolver->bytecode_jobs) == 0 &&
                queue_count(&resolver->llvm_jobs)     == 0) {
                done = true;
            }
        }

        return result;
    }

    void queue_parse_job(Resolver *resolver, String module_name, String module_path,
                         bool insert_entry_module/*= false*/)
    {
        auto parse_jobs = resolver->parse_jobs;

        for (int64_t i = 0; i < parse_jobs.used; i++) {
            auto index = parse_jobs.front + i;
            if (index >= parse_jobs.capacity) {
                index -= parse_jobs.capacity;
            }

            auto &it = parse_jobs.buffer[index];

            if (it.module_path == module_path) {
                assert(it.module_name == module_name);
                assert(it.insert_entry_module == insert_entry_module);
                return;
            }
        }

        for (int64_t i = 0; i < resolver->parsed_modules.count; i++) {
            if (resolver->parsed_modules[i].full_path == module_path) {
                return;
            }
        }

        if (resolver->build_data->options->verbose)
            printf("[RESOLVER] Queueing parse job for file: '%s'\n", module_path.data);

        Parse_Job job = { .module_name = module_name,
                          .module_path = module_path,
                          .insert_entry_module = insert_entry_module,
                          .parsed_module_index = -1,
        };

        queue_enqueue(&resolver->parse_jobs, job);
    }

    void queue_resolve_job(Resolver *resolver, AST_Node *ast_node)
    {
        assert(ast_node->kind == AST_Node_Kind::DECLARATION);

        if (resolver->build_data->options->verbose) {
        auto decl = static_cast<AST_Declaration *>(ast_node);
        if (decl->identifier) {
            printf("[RESOLVER] Queueing resolve job for declaration: '%s'\n",
                   decl->identifier->atom.data);
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

        Size_Job job = { .ast_node = node };
        queue_enqueue(&resolver->size_jobs, job);
    }

    void queue_bytecode_job(Resolver *resolver, AST_Declaration *func_decl)
    {
        assert(func_decl->kind == AST_Declaration_Kind::FUNCTION);

        Bytecode_Job job = { .decl = func_decl };
        queue_enqueue(&resolver->bytecode_jobs, job);
    }

    void queue_llvm_job(Resolver *resolver, Bytecode_Function *bc_func)
    {
        LLVM_Job job = { .bc_func = bc_func };
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
        array_append(&resolver->parsed_modules, { job->module_path, module_ast });
        job->parsed_module_index = index;

        return true;
    }

    bool try_resolve_job(Resolver *resolver, Resolve_Job *job)
    {
        assert(job->ast_node->kind == AST_Node_Kind::DECLARATION);
        AST_Declaration *decl = static_cast<AST_Declaration *>(job->ast_node);

        if (resolver->build_data->options->verbose) {
            assert(decl->identifier);
            printf("[RESOLVER] Trying to resolve declaration: '%s'\n",
                   decl->identifier->atom.data);
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
        assert(decl->type);
        assert(decl->flags & AST_NODE_FLAG_RESOLVED_ID);
        assert(decl->flags & AST_NODE_FLAG_TYPED);

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
            case AST_Declaration_Kind::IMPORT: assert(false);
            case AST_Declaration_Kind::USING: assert(false);

            case AST_Declaration_Kind::VARIABLE: {

                AST_Type_Spec *ts = nullptr;
                if (declaration->variable.type_spec) {
                    AST_Type_Spec *ts = declaration->variable.type_spec;
                    assert(ts->type);
                    assert(ts->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(ts->flags & AST_NODE_FLAG_TYPED);
                }

                AST_Expression *init_expr = declaration->variable.init_expression;
                assert(init_expr->type);
                assert(init_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(init_expr->flags & AST_NODE_FLAG_TYPED);

                AST_Type *type = nullptr;

                if (ts) {
                    assert(ts->type == init_expr->type);
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

            case AST_Declaration_Kind::CONSTANT: assert(false);
            case AST_Declaration_Kind::PARAMETER: assert(false);

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
            case AST_Declaration_Kind::TYPEDEF: assert(false);
            case AST_Declaration_Kind::STRUCTURE: assert(false);
            case AST_Declaration_Kind::ENUM: assert(false);
            case AST_Declaration_Kind::POLY_TYPE: assert(false);
            case AST_Declaration_Kind::RUN: assert(false);
            case AST_Declaration_Kind::STATIC_IF: assert(false);
            case AST_Declaration_Kind::STATIC_ASSERT: assert(false);
        }
    }

    bool try_resolve_statement(Resolver *resolver, AST_Statement *statement)
    {
        switch (statement->kind) {
            case AST_Statement_Kind::INVALID: assert(false);

            case AST_Statement_Kind::BLOCK:
            {
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

            case AST_Statement_Kind::ASSIGNMENT: assert(false);

            case AST_Statement_Kind::RETURN: {
                AST_Expression *operand_expr = statement->expression;
                assert(operand_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(operand_expr->flags & AST_NODE_FLAG_TYPED);
                assert(operand_expr->type);

                statement->flags |= AST_NODE_FLAG_RESOLVED_ID;
                statement->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Statement_Kind::BREAK: assert(false);

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

            case AST_Statement_Kind::WHILE: assert(false);
            case AST_Statement_Kind::FOR: assert(false);
            case AST_Statement_Kind::IF: assert(false);
            case AST_Statement_Kind::SWITCH: assert(false);
        }
    }

    bool try_resolve_expression(Resolver *resolver, AST_Expression *expression)
    {
        switch (expression->kind) {
            case AST_Expression_Kind::INVALID: assert(false);

            case AST_Expression_Kind::IDENTIFIER: {
                auto decl = scope_find_declaration(expression->scope, expression->identifier);
                assert(decl);

                if (!(decl->flags & AST_NODE_FLAG_RESOLVED_ID)) {
                    return false;
                }

                if (!(decl->flags & AST_NODE_FLAG_TYPED)) {
                    return false;
                }

                assert(decl->type);

                expression->type = decl->type;
                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);
            case AST_Expression_Kind::DOT: assert(false);
            case AST_Expression_Kind::BINARY: assert(false);
            case AST_Expression_Kind::UNARY: assert(false);

            case AST_Expression_Kind::CALL: {

                AST_Expression *ident_expr = expression->call.ident_expression;
                assert(ident_expr->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(ident_expr->flags & AST_NODE_FLAG_TYPED);

                AST_Identifier *callee_ident = ident_expr->identifier;
                assert(callee_ident->declaration);

                AST_Declaration *callee_decl = callee_ident->declaration;
                assert(callee_decl->type);
                assert(callee_decl->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(callee_decl->flags & AST_NODE_FLAG_TYPED);

                expression->call.callee_declaration = callee_decl;

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
                } else {
                    assert(false);
                }

                assert(false);

                break;
            }

            case AST_Expression_Kind::ADDROF: assert(false);
            case AST_Expression_Kind::COMPOUND: assert(false);
            case AST_Expression_Kind::SUBSCRIPT: assert(false);
            case AST_Expression_Kind::CAST: assert(false);

            case AST_Expression_Kind::INTEGER_LITERAL: {
                assert(!expression->type);
                expression->type = Builtin::type_s64;
                expression->flags |= AST_NODE_FLAG_RESOLVED_ID;
                expression->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Expression_Kind::FLOAT_LITERAL: assert(false);
            case AST_Expression_Kind::STRING_LITERAL: assert(false);
            case AST_Expression_Kind::CHAR_LITERAL: assert(false);
            case AST_Expression_Kind::BOOL_LITERAL: assert(false);
            case AST_Expression_Kind::NULL_LITERAL: assert(false);
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
                assert(decl);
                assert(decl->type);
                assert(decl->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(decl->flags & AST_NODE_FLAG_TYPED);

                type_spec->type = decl->type;
                type_spec->flags |= AST_NODE_FLAG_RESOLVED_ID;
                type_spec->flags |= AST_NODE_FLAG_TYPED;

                return true;
            }

            case AST_Type_Spec_Kind::POINTER: assert(false);
            case AST_Type_Spec_Kind::DOT: assert(false);

            case AST_Type_Spec_Kind::FUNCTION: {

                for (int64_t i = 0; i < type_spec->function.parameter_type_specs.count; i++) {
                    auto param_ts = type_spec->function.parameter_type_specs[i];
                    assert(param_ts->type);
                    assert(param_ts->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(param_ts->flags & AST_NODE_FLAG_TYPED);
                }

                AST_Type_Spec *return_ts = nullptr;
                if (type_spec->function.return_type_spec) {
                    return_ts = type_spec->function.return_type_spec;
                    assert(return_ts->type);
                    assert(return_ts->flags & AST_NODE_FLAG_RESOLVED_ID);
                    assert(return_ts->flags & AST_NODE_FLAG_TYPED);
                }

                Array<AST_Type *> param_types = {};
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
                }
                assert(function_type);

                type_spec->type = function_type;
                type_spec->flags |= AST_NODE_FLAG_RESOLVED_ID;
                type_spec->flags |= AST_NODE_FLAG_TYPED;
                return true;
                break;
            }

            case AST_Type_Spec_Kind::ARRAY: assert(false);
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
            case AST_Declaration_Kind::IMPORT: assert(false);
            case AST_Declaration_Kind::USING: assert(false);

            case AST_Declaration_Kind::VARIABLE: {
                assert(decl->type);
                assert(decl->type->flags & AST_NODE_FLAG_SIZED);
                decl->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Declaration_Kind::CONSTANT: assert(false);
            case AST_Declaration_Kind::PARAMETER: assert(false);

            case AST_Declaration_Kind::FUNCTION: {
                assert(decl->type->flags & AST_NODE_FLAG_SIZED);
                decl->flags |= AST_NODE_FLAG_SIZED;
                return true;
            }

            case AST_Declaration_Kind::TYPE: assert(false);
            case AST_Declaration_Kind::TYPEDEF: assert(false);
            case AST_Declaration_Kind::STRUCTURE: assert(false);
            case AST_Declaration_Kind::ENUM: assert(false);
            case AST_Declaration_Kind::POLY_TYPE: assert(false);
            case AST_Declaration_Kind::RUN: assert(false);
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

            case AST_Statement_Kind::ASSIGNMENT: assert(false);

            case AST_Statement_Kind::RETURN: {
                AST_Expression *op_expr = statement->expression;
                assert(op_expr);
                assert(op_expr->flags & AST_NODE_FLAG_SIZED);

                statement->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Statement_Kind::BREAK: assert(false);

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

            case AST_Statement_Kind::WHILE: assert(false);
            case AST_Statement_Kind::FOR: assert(false);
            case AST_Statement_Kind::IF: assert(false);
            case AST_Statement_Kind::SWITCH: assert(false);
        }
    }

    bool try_size_expression(Resolver *resolver, AST_Expression *expression)
    {
        assert(!(expression->flags & AST_NODE_FLAG_SIZED));

        switch (expression->kind) {
            case AST_Expression_Kind::INVALID: assert(false);

            case AST_Expression_Kind::IDENTIFIER: {
                assert(expression->type);
                assert(expression->type->flags & AST_NODE_FLAG_SIZED);

                expression->flags |= AST_NODE_FLAG_SIZED;
                return true;
                break;
            }

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);
            case AST_Expression_Kind::DOT: assert(false);
            case AST_Expression_Kind::BINARY: assert(false);
            case AST_Expression_Kind::UNARY: assert(false);

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

            case AST_Expression_Kind::ADDROF: assert(false);
            case AST_Expression_Kind::COMPOUND: assert(false);
            case AST_Expression_Kind::SUBSCRIPT: assert(false);
            case AST_Expression_Kind::CAST: assert(false);

            case AST_Expression_Kind::INTEGER_LITERAL: {
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

            case AST_Expression_Kind::FLOAT_LITERAL: assert(false);
            case AST_Expression_Kind::STRING_LITERAL: assert(false);
            case AST_Expression_Kind::CHAR_LITERAL: assert(false);
            case AST_Expression_Kind::BOOL_LITERAL: assert(false);
            case AST_Expression_Kind::NULL_LITERAL: assert(false);
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

            case AST_Type_Kind::STRUCTURE: assert(false);
            case AST_Type_Kind::ENUM: assert(false);
            case AST_Type_Kind::ARRAY: assert(false);
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

            case AST_Type_Spec_Kind::POINTER: assert(false);
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

            case AST_Type_Spec_Kind::ARRAY: assert(false);
            case AST_Type_Spec_Kind::TEMPLATED: assert(false);
            case AST_Type_Spec_Kind::POLY_IDENTIFIER: assert(false);
            case AST_Type_Spec_Kind::FROM_TYPE: assert(false);
        }
    }

    bool is_entry_decl(Resolver *resolver, AST_Declaration *decl)
    {
        assert(decl->kind == AST_Declaration_Kind::FUNCTION);

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
}
