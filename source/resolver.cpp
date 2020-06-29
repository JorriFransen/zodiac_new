#include "resolver.h"

#include "builtin.h"
#include "scope.h"
#include "string_builder.h"
#include "ast.h"

namespace Zodiac
{
    void resolver_init(Allocator *allocator, Resolver *resolver)
    {
        assert(allocator);
        assert(resolver);
    }

    Resolve_Job *start_resolving(Allocator *allocator, Resolver *resolver, AST_Node *ast_node)
    {
        assert(allocator);
        assert(resolver);
        assert(ast_node);

        switch (ast_node->kind)
        {
            case AST_Node_Kind::INVALID: assert(false);

            case AST_Node_Kind::MODULE:
            {
                auto module = static_cast<AST_Module*>(ast_node);

                Array<Resolve_Job*> module_decl_jobs = {};
                array_init(allocator, &module_decl_jobs, module->declarations.count);

                for (int64_t i = 0; i < module->declarations.count; i++)
                {
                    auto module_decl_job = start_resolving(allocator, resolver,
                                                           module->declarations[i]);
                    assert(module_decl_job);
                    array_append(&module_decl_jobs, module_decl_job);
                }

                return resolve_job_module_new(allocator, module_decl_jobs);

                break;
            }

            case AST_Node_Kind::IDENTIFIER: assert(false);

            case AST_Node_Kind::DECLARATION:
            {
                auto decl = static_cast<AST_Declaration*>(ast_node);
                return start_resolving_declaration(allocator, resolver, decl);
            }

            case AST_Node_Kind::STATEMENT: assert(false);
            case AST_Node_Kind::EXPRESSION: assert(false);
            case AST_Node_Kind::TYPE_SPEC: assert(false);
        }

        assert(false);
        return nullptr;
    }

    Resolve_Job *start_resolving_declaration(Allocator *allocator, Resolver *resolver,
                                             AST_Declaration *decl)
    {
        assert(allocator);
        assert(resolver);
        assert(decl);

        switch (decl->kind)
        {
            case AST_Declaration_Kind::INVALID: assert(false);
            case AST_Declaration_Kind::IMPORT: assert(false);
            case AST_Declaration_Kind::VARIABLE: assert(false);
            case AST_Declaration_Kind::CONSTANT: assert(false);
            case AST_Declaration_Kind::PARAMETER: assert(false);

            case AST_Declaration_Kind::FUNCTION:
            {
                auto ts_rj = start_resolving_type_spec(allocator, resolver,
                                                       decl->function.type_spec);

                Array<Resolve_Job*> param_rjs = {};
                if (decl->function.parameter_declarations.count)
                {
                    array_init(allocator, &param_rjs,
                               decl->function.parameter_declarations.count);

                    for (int64_t i = 0; i < decl->function.parameter_declarations.count; i++)
                    {
                        auto param_rj =
                            start_resolving_declaration(allocator, resolver,
                                                        decl->function.parameter_declarations[i]);
                        array_append(&param_rjs, param_rj);
                    }
                }

                Resolve_Job *body_rj = nullptr;
                if (decl->function.body)
                {
                    body_rj = start_resolving_statement(allocator, resolver, decl->function.body,
                                                        nullptr);
                    assert(body_rj);
                }

                return resolve_job_function_declaration_new(allocator, ts_rj, param_rjs, body_rj);
                break;
            }

            case AST_Declaration_Kind::STRUCTURE: assert(false);
            case AST_Declaration_Kind::POLY_TYPE: assert(false);
        }

        assert(false);
        return nullptr;
    }

    Resolve_Job *start_resolving_statement(Allocator *allocator, Resolver *resolver,
                                           AST_Statement *statement, Scope *scope)
    {
        assert(allocator);
        assert(resolver);
        assert(statement);

        switch (statement->kind)
        {
            case AST_Statement_Kind::INVALID: assert(false);

            case AST_Statement_Kind::BLOCK:
            {
                Array<Resolve_Job*> block_rjs = {};
                if (statement->block.statements.count)
                {
                    array_init(allocator, &block_rjs, statement->block.statements.count);
                    
                    for (int64_t i = 0; i < statement->block.statements.count; i++)
                    {
                        auto st_rj = start_resolving_statement(allocator, resolver,
                                                               statement->block.statements[i],
                                                               statement->block.scope);
                        assert(st_rj);
                        array_append(&block_rjs, st_rj);
                    }

                    return resolve_job_block_statement_new(allocator, block_rjs);
                }

                return nullptr;
                break;
            }

            case AST_Statement_Kind::ASSIGNMENT: assert(false);
            case AST_Statement_Kind::RETURN: assert(false);
            case AST_Statement_Kind::DECLARATION: assert(false);

            case AST_Statement_Kind::EXPRESSION:
            {
                return start_resolving_expression(allocator, resolver, statement->expression,
                                                  scope);
                break;
            }
        }

        assert(false);
    }

    Resolve_Job *start_resolving_expression(Allocator *allocator, Resolver *resolver,
                                            AST_Expression *expression, Scope* scope)
    {
        switch (expression->kind)
        {
            case AST_Expression_Kind::INVALID: assert(false);

            case AST_Expression_Kind::IDENTIFIER:
            {
                return resolve_job_identifier_new(allocator, expression->identifier, scope);
                break;
            }

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);
            case AST_Expression_Kind::DOT: assert(false);
            case AST_Expression_Kind::BINARY: assert(false);
            case AST_Expression_Kind::UNARY: assert(false);

            case AST_Expression_Kind::CALL:
            {
                Resolve_Job *ident_rj = nullptr;
                Array<Resolve_Job*> arg_expr_rjs = {};

                if (expression->call.arg_expressions.count)
                {
                    array_init(allocator, &arg_expr_rjs,
                            expression->call.arg_expressions.count);
                }

                if (expression->call.is_builtin)
                {
                    ident_rj =
                        start_resolving_builtin_identifier(allocator, resolver,
                                                           expression->call.ident_expression);
                    assert(ident_rj);

                    if (expression->call.arg_expressions.count)
                    {
                        for (int64_t i = 0; i < expression->call.arg_expressions.count; i++)
                        {
                            auto arg_expr_rj =
                                start_resolving_expression(allocator, resolver,
                                                           expression->call.arg_expressions[i],
                                                           scope);
                            assert(arg_expr_rj);
                            array_append(&arg_expr_rjs, arg_expr_rj);
                        }
                    }
                }
                else
                {
                    ident_rj = start_resolving_expression(allocator, resolver,
                                                          expression->call.ident_expression,
                                                          scope);
                    assert(ident_rj);

                    if (expression->call.arg_expressions.count)
                    {
                        for (int64_t i = 0; i < expression->call.arg_expressions.count; i++)
                        {
                            auto arg_expr_rj =
                                start_resolving_expression(allocator, resolver,
                                                           expression->call.arg_expressions[i],
                                                           scope);
                            assert(arg_expr_rj);
                            array_append(&arg_expr_rjs, arg_expr_rj);
                        }
                    }

                }

                return resolve_job_call_expression_new(allocator, ident_rj, arg_expr_rjs,
                                                       expression->call.is_builtin, expression);

                break;
            }

            case AST_Expression_Kind::COMPOUND: assert(false);

            case AST_Expression_Kind::NUMBER_LITERAL:
            {
                return resolve_job_number_literal_new(allocator, expression);
                break;
            }

            case AST_Expression_Kind::STRING_LITERAL: assert(false);
        }

        assert(allocator);
        assert(resolver);
        assert(false);
    }

    Resolve_Job *start_resolving_builtin_identifier(Allocator *allocator, Resolver *resolver,
                                                    AST_Expression *ident_expr)
    {
        assert(allocator);
        assert(resolver);
        assert(ident_expr);
        assert(ident_expr->kind == AST_Expression_Kind::IDENTIFIER);

        auto ident = ident_expr->identifier;
        if (ident->atom == Builtin::atom_exit)
        {
            return resolve_job_builtin_identifier_new(allocator, ident);
        }

        assert(false);
    }

    Resolve_Job *start_resolving_type_spec(Allocator *allocator, Resolver *resolver,
                                           AST_Type_Spec *type_spec)
    {
        assert(allocator);
        assert(resolver);
        assert(type_spec);

        switch (type_spec->kind)
        {
            case AST_Type_Spec_Kind::INVALID: assert(false);
            case AST_Type_Spec_Kind::IDENTIFIER: assert(false);
            case AST_Type_Spec_Kind::POINTER: assert(false);
            case AST_Type_Spec_Kind::DOT: assert(false);

            case AST_Type_Spec_Kind::FUNCTION:
            {
                Array<Resolve_Job*> param_rjs = {};
                if (type_spec->function.parameter_type_specs.count)
                {
                    array_init(allocator, &param_rjs,
                               type_spec->function.parameter_type_specs.count);

                    for (int64_t i  =  0; i < type_spec->function.parameter_type_specs.count; i++)
                    {
                        auto param_rj =
                            start_resolving_type_spec(allocator, resolver,
                                                      type_spec->function.parameter_type_specs[i]);
                        assert(param_rj);

                        array_append(&param_rjs, param_rj);
                    }
                }

                Resolve_Job *return_ts_rj = nullptr;
                if (type_spec->function.return_type_spec)
                {
                    return_ts_rj = start_resolving_type_spec(allocator, resolver,
                                                             type_spec->function.return_type_spec);
                    assert(return_ts_rj);
                }

                return resolve_job_function_type_spec_new(allocator, param_rjs, return_ts_rj);
                
                break;
            }

            case AST_Type_Spec_Kind::ARRAY: assert(false);
            case AST_Type_Spec_Kind::TEMPLATED: assert(false);
            case AST_Type_Spec_Kind::POLY_IDENTIFIER: assert(false);
        }
    }

    Resolve_Result finish_resolving(Resolver *resolver, Resolve_Job *resolve_job, bool blocking)
    {
        assert(resolver);
        assert(resolve_job);

        assert(blocking);

        switch (resolve_job->kind)
        {
            case Resolve_Job_Kind::INVALID: assert(false);

            case Resolve_Job_Kind::MODULE:
            {
                if (blocking)
                {
                    Resolve_Result module_res = {};

                    for (int64_t i = 0; i < resolve_job->module_jobs.count; i++)
                    {
                        auto decl_res = finish_resolving(resolver, resolve_job->module_jobs[i],
                                                         true);
                        module_res.error_count += decl_res.error_count;
                    }

                    return module_res;
                }
                else
                {
                    assert(false);
                }

                break;
            }

            case Resolve_Job_Kind::FUNCTION_DECL:
            {
                if (blocking)
                {
                    Resolve_Result function_res = {};

                    auto type_spec_res = finish_resolving(resolver,
                                                          resolve_job->function_decl.type_spec_rj,
                                                          true);
                    function_res.error_count += type_spec_res.error_count;

                    for (int64_t i = 0; i < resolve_job->function_decl.parameter_jobs.count; i++)
                    {
                        auto param_res =
                            finish_resolving(resolver, resolve_job->function_decl.parameter_jobs[i],
                                             true);
                        function_res.error_count += param_res.error_count;
                    }

                    auto body_res = finish_resolving(resolver, resolve_job->function_decl.body_rj,
                                                     true);

                    function_res.error_count += body_res.error_count;

                    return function_res;
                }
                else
                {
                    assert(false);
                }

                break;
            }

            case Resolve_Job_Kind::BLOCK_STATEMENT:
            {
                if (blocking)
                {
                    Resolve_Result block_res = {};

                    for (int64_t i = 0; i < resolve_job->block_jobs.count; i++)
                    {
                        auto stmt_res = finish_resolving(resolver, resolve_job->block_jobs[i],
                                                         true);
                        block_res.error_count += stmt_res.error_count;
                    }

                    return block_res;
                }
                else
                {
                    assert(false);
                }
                break;
            }

            case Resolve_Job_Kind::IDENTIFIER:
            {
                assert(resolve_job->identifier.scope);

                auto id = resolve_job->identifier.identifier;

                Resolve_Result id_res = {};

                if (id->declaration) return id_res; 

                auto decl = scope_find_declaration(resolve_job->identifier.scope,
                                                   resolve_job->identifier.identifier);

                if (!decl)
                {
                    assert(false);
                    id_res.error_count += 1;
                }

                return id_res;

                break;
            }

            case Resolve_Job_Kind::BUILTIN_IDENTIFIER: assert(false);

            case Resolve_Job_Kind::CALL_EXPRESSION:
            {
                if (blocking)
                {
                    Resolve_Result call_expr_res = {};
                    
                   if (resolve_job->call_expression.is_builtin)
                   {
                       return finish_resolving_builtin_call(resolver, resolve_job, true);
                   } 
                   else
                   {
                       auto ident_res =
                           finish_resolving(resolver, resolve_job->call_expression.identifier_rj,
                                            true);

                       call_expr_res.error_count += ident_res.error_count;

                       for (int64_t i = 0; i < resolve_job->call_expression.arg_jobs.count; i++)
                       {
                           auto arg_res = finish_resolving(resolver,
                                                           resolve_job->call_expression.arg_jobs[i],
                                                           false);
                           call_expr_res.error_count += arg_res.error_count;
                       }
                   }

                   return call_expr_res;
                }
                else
                {
                    assert(false);
                }
                break;
            }

            case Resolve_Job_Kind::NUMBER_LITERAL:
            {
                assert(resolve_job->expected_type);
                auto et = resolve_job->expected_type;
                assert(et->kind == INTEGER);

                assert(et->integer.sign);

                assert(resolve_job->number_literal.s64 <= INT64_MAX);
                assert(resolve_job->number_literal.s64 >= INT64_MIN);

                assert(resolve_job->origin.expression);
                resolve_job->origin.expression->type = et;

                return {};

                break;
            }

            case Resolve_Job_Kind::FUNCTION_TYPE_SPEC:
            {
                if (blocking)
                {
                    Resolve_Result function_ts_res = {};
            
                    for (int64_t i = 0;
                         i < resolve_job->function_type_spec.param_ts_jobs.count;
                         i++)
                    {
                        auto param_res =
                            finish_resolving(resolver,
                                             resolve_job->function_type_spec.param_ts_jobs[i],
                                             true);

                        function_ts_res.error_count += param_res.error_count;
                    }

                    if (resolve_job->function_type_spec.return_ts_job)
                    {
                        auto return_ts_res =
                            finish_resolving(resolver,
                                             resolve_job->function_type_spec.return_ts_job,
                                             true);

                        function_ts_res.error_count += return_ts_res.error_count;
                    }

                    return function_ts_res;
                }
                else
                {
                    assert(false);
                }
                break;
            }
        }

        assert(false);
    }

    Resolve_Result finish_resolving_builtin_call(Resolver *resolver, Resolve_Job *rj, bool blocking)
    {
        assert(resolver);
        assert(rj);
        assert(rj->kind == Resolve_Job_Kind::CALL_EXPRESSION);
        assert(rj->call_expression.is_builtin);

        assert(rj->origin.expression);

        auto expr = rj->origin.expression;

        if (blocking)
        {
            auto ident = expr->call.ident_expression;
            auto atom = ident->identifier->atom;

            if (atom == Builtin::atom_exit)
            {
                Resolve_Result res = {};

                assert(expr->call.arg_expressions.count == 1);
                assert(rj->call_expression.arg_jobs.count == 1);

                auto arg_rj = rj->call_expression.arg_jobs[0];
                arg_rj->expected_type = Builtin::type_s64;
                auto arg_expr = expr->call.arg_expressions[0];

                auto arg_res = finish_resolving(resolver, arg_rj, true);

                assert(arg_expr->type);
                assert(arg_expr->type == Builtin::type_s64);

                res.error_count += arg_res.error_count;

                return res;

            }
            else
            {
                assert(false);
            }
        }
        else
        {
            assert(false);
        }

        assert(false);
    }
    
    Resolve_Job *resolve_job_module_new(Allocator *allocator, Array<Resolve_Job*> module_jobs)
    {
        assert(allocator);
        assert(module_jobs.count);
        assert(module_jobs.data);

        auto result = resolve_job_new(allocator, Resolve_Job_Kind::MODULE);
        result->module_jobs = module_jobs;

        return result;
    }

    Resolve_Job *resolve_job_function_declaration_new(Allocator *allocator,
                                                      Resolve_Job *type_spec_rj,
                                                      Array<Resolve_Job*> param_rjs,
                                                      Resolve_Job *body_rj)
    {
        auto result = resolve_job_new(allocator, Resolve_Job_Kind::FUNCTION_DECL);
        result->function_decl.type_spec_rj = type_spec_rj;
        result->function_decl.parameter_jobs = param_rjs;
        result->function_decl.body_rj = body_rj;

        return result;
    }

    Resolve_Job *resolve_job_block_statement_new(Allocator *allocator,
                                                 Array<Resolve_Job*> block_jobs)
    {
        assert(allocator);
        assert(block_jobs.count);

        auto result = resolve_job_new(allocator, Resolve_Job_Kind::BLOCK_STATEMENT);
        result->block_jobs = block_jobs;

        return result;
    }

    Resolve_Job *resolve_job_identifier_new(Allocator *allocator, AST_Identifier *identifier,
                                            Scope* scope)
    {
        assert(allocator);
        assert(identifier);

        auto result = resolve_job_new(allocator, Resolve_Job_Kind::IDENTIFIER);
        result->identifier.identifier = identifier;
        result->identifier.scope = scope;

        return result;
    }

    Resolve_Job *resolve_job_builtin_identifier_new(Allocator *allocator,
                                                    AST_Identifier *identifier)
    {
        auto result = resolve_job_new(allocator, Resolve_Job_Kind::BUILTIN_IDENTIFIER);
        result->identifier.identifier = identifier;
        result->identifier.scope = nullptr;

        return result;
    }

    Resolve_Job *resolve_job_call_expression_new(Allocator *allocator, Resolve_Job *ident_rj,
                                                 Array<Resolve_Job*> arg_rjs, bool is_builtin,
                                                 AST_Expression *origin)
    {
        auto result = resolve_job_new(allocator, Resolve_Job_Kind::CALL_EXPRESSION);
        result->origin.expression = origin;

        result->call_expression.identifier_rj = ident_rj;
        result->call_expression.arg_jobs = arg_rjs;
        result->call_expression.is_builtin = is_builtin;

        return result;
    }

    Resolve_Job *resolve_job_number_literal_new(Allocator *allocator, AST_Expression *origin_expr)
    {
        assert(origin_expr->kind == AST_Expression_Kind::NUMBER_LITERAL);

        auto result = resolve_job_new(allocator, Resolve_Job_Kind::NUMBER_LITERAL);
        result->origin.expression = origin_expr;

        result->number_literal = origin_expr->number_literal;

        return result;
    }

    Resolve_Job *resolve_job_function_type_spec_new(Allocator *allocator,
                                                    Array<Resolve_Job*> param_ts_jobs,
                                                    Resolve_Job *return_ts_job)
    {
        assert(allocator);
        assert(param_ts_jobs.count >= 0);

        auto result = resolve_job_new(allocator, Resolve_Job_Kind::FUNCTION_TYPE_SPEC);

        result->function_type_spec.param_ts_jobs = param_ts_jobs;
        result->function_type_spec.return_ts_job = return_ts_job;

        return result;
    }

    Resolve_Job* resolve_job_new(Allocator *allocator, Resolve_Job_Kind kind)
    {
        assert(allocator);
        assert(kind != Resolve_Job_Kind::INVALID);

        auto result = alloc_type<Resolve_Job>(allocator);
        assert(result);
        result->kind = kind;

        return result;
    }

    void resolver_report_errors(Resolver *resolver, Resolve_Result *resolve_result,
                                String_Builder *sb)
    {
        assert(resolver);
        assert(resolve_result);
        assert(sb);

        if (resolve_result->error_count != 0)
        {
            assert(false);
        }
    }

    void resolver_report_errors(Resolver* resolver, Resolve_Result *resolve_result, 
                                Allocator *allocator)
    {

        String_Builder sb = {};
        string_builder_init(allocator, &sb, 2048);

        resolver_report_errors(resolver, resolve_result, &sb);

        string_builder_free(&sb);
    }
}

