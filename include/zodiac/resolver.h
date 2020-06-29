#pragma once

#include "allocator.h"
#include "atom.h"
#include "struct_predecls.h"
#include "queue.h"

namespace Zodiac
{
    enum class Resolve_Job_Kind
    {
        INVALID,

        MODULE,

        FUNCTION_DECL,

        BLOCK_STATEMENT,

        IDENTIFIER,
        BUILTIN_IDENTIFIER,
        CALL_EXPRESSION,
        NUMBER_LITERAL,

        FUNCTION_TYPE_SPEC,
    };

    struct Resolve_Job
    {
        Resolve_Job_Kind kind = Resolve_Job_Kind::INVALID;

        union
        {
            AST_Expression *expression; 
        } origin;

        union
        {
            Array<Resolve_Job*> module_jobs;
            Array<Resolve_Job*> block_jobs;

            struct 
            {
                AST_Identifier *identifier;
                Scope *scope;
            } identifier;

            struct 
            {
                Resolve_Job *type_spec_rj;
                Array<Resolve_Job*> parameter_jobs;
                Resolve_Job *body_rj;
            } function_decl;

            struct
            {
                Resolve_Job *identifier_rj;
                Array<Resolve_Job*> arg_jobs;
                bool is_builtin;
            } call_expression;

            struct
            {
                Array<Resolve_Job*> param_ts_jobs;
                Resolve_Job *return_ts_job;
            } function_type_spec;

            Number_Literal number_literal;
        };

        Resolve_Job() {}
    };

    struct Resolver
    {

    };

    struct Resolve_Result
    {
        int64_t error_count = 0;
    };

    void resolver_init(Allocator *allocator, Resolver *resolver);

    Resolve_Job *start_resolving(Allocator *allocator, Resolver *resolver, AST_Node *ast_node);
    Resolve_Job *start_resolving_declaration(Allocator *allocator, Resolver *resolver,
                                             AST_Declaration *decl);
    Resolve_Job *start_resolving_statement(Allocator *allocator, Resolver *resolver,
                                           AST_Statement *statement, Scope *scope);
    Resolve_Job *start_resolving_expression(Allocator *allocator, Resolver *resolver,
                                            AST_Expression *expression, Scope *scope);
    Resolve_Job *start_resolving_builtin_identifier(Allocator *allocator, Resolver *resolver,
                                                    AST_Expression *ident_expr);
    Resolve_Job *start_resolving_type_spec(Allocator *allocator, Resolver *resolver,
                                           AST_Type_Spec *type_spec);

    Resolve_Result finish_resolving(Resolver *resolver, Resolve_Job *resolve_job, bool blocking);
    Resolve_Result finish_resolving_builtin_call(Resolver *resolver, Resolve_Job *rj, bool blocking);


    Resolve_Job *resolve_job_module_new(Allocator *allocator, Array<Resolve_Job*> module_jobs);
    Resolve_Job *resolve_job_function_declaration_new(Allocator *allocator, Resolve_Job *type_spec_rj,
                                                      Array<Resolve_Job*> param_rjs,
                                                      Resolve_Job *body_rj);
    Resolve_Job *resolve_job_block_statement_new(Allocator *allocator,
                                                 Array<Resolve_Job*> block_jobs);
    Resolve_Job *resolve_job_identifier_new(Allocator *allocator, AST_Identifier *identifier,
                                            Scope *scope);
    Resolve_Job *resolve_job_builtin_identifier_new(Allocator *allocator, AST_Identifier *identifier);
    Resolve_Job *resolve_job_call_expression_new(Allocator *allocator, Resolve_Job *ident_rj,
                                                 Array<Resolve_Job*> arg_rjs, bool is_builtin, 
                                                 AST_Expression *origin);
    Resolve_Job *resolve_job_number_literal_new(Allocator *allocator, Number_Literal number_literal);
    Resolve_Job *resolve_job_function_type_spec_new(Allocator *allocator,
                                                    Array<Resolve_Job*> param_ts_jobs,
                                                    Resolve_Job *return_ts_job);

    Resolve_Job* resolve_job_new(Allocator *allocator, Resolve_Job_Kind kind);

    void resolver_report_errors(Resolver *resolver, Resolve_Result *resolve_result,
                                String_Builder *sb);
    void resolver_report_errors(Resolver* resolver, Resolve_Result *resolve_result, 
                                Allocator *allocator);
}
