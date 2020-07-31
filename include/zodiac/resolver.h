#pragma once

#include "build_data.h"
#include "struct_predecls.h"
#include "queue.h"

namespace Zodiac
{
    struct Resolve_Job;
    struct Resolver
    {
        Allocator *allocator = nullptr;
        Build_Data *build_data = nullptr;
        AST_Node *root_node = nullptr;

        Queue<Resolve_Job*> ident_job_queue = {};
        Queue<Resolve_Job*> type_job_queue = {};
        Queue<Resolve_Job*> size_job_queue = {};
    };

    struct Resolve_Result
    {
        uint64_t error_count = 0;
    };

    enum class Resolve_Job_Kind
    {
        INVALID,
        IDENTIFIER,
        TYPE,
        SIZE,
    };

    struct Resolve_Job
    {
        Resolve_Job_Kind kind = Resolve_Job_Kind::INVALID;

        union
        {
            AST_Node *ast_node = nullptr;
            AST_Declaration *declaration;
            AST_Statement *statement;
            AST_Expression *expression;
            AST_Identifier *identifier;
        };

        Scope *node_scope = nullptr;
    };

    void resolver_init(Allocator *allocator, Resolver *resolver, Build_Data *build_data);

    void start_resolving(Resolver *resolver, AST_Node *ast_node, bool blocking);
    Resolve_Result finish_resolving(Resolver *resolver);

    void start_resolve_pump(Resolver *resolver);

    bool try_resolve_job(Resolver *resolver, Resolve_Job *job);

    bool try_resolve_identifiers(Resolver *resolver, AST_Node *ast_node, Scope *scope);
    bool try_resolve_identifiers(Resolver *resolver, AST_Declaration *ast_decl, Scope *scope);
    bool try_resolve_identifiers(Resolver *resolver, AST_Statement *ast_stmt, Scope *scope);
    bool try_resolve_identifiers(Resolver *resolver, AST_Expression* ast_expr, Scope *scope);
    bool try_resolve_identifiers(Resolver *resolver, AST_Type_Spec *ast_ts);

    bool try_resolve_types(Resolver *resolver, AST_Node *ast_node, Scope *scope);
    bool try_resolve_types(Resolver *resolver, AST_Declaration *ast_decl, Scope *scope);
    bool try_resolve_types(Resolver *resolver, AST_Statement *ast_stmt, Scope *scope);
    bool try_resolve_types(Resolver *resolver, AST_Expression *ast_expr, Scope *scope);
    bool try_resolve_builtin_call_types(Resolver *resolver, AST_Expression *call_expr, Scope *scope);
    bool try_resolve_types(Resolver *resolver, AST_Type_Spec *ts, Scope *scope,
                           AST_Type **type_target);


    AST_Type* find_or_create_function_type(Resolver *resolver, Array<AST_Type*> param_types,
                                           AST_Type *return_type, Scope *scope);

    void queue_ident_job(Resolver *resolver, AST_Node *ast_node, Scope *scope);
    void queue_type_job(Resolver *resolver, AST_Node *ast_node, Scope *scope);
    void queue_size_job(Resolver *resolver, AST_Node *ast_node, Scope *scope);

    Resolve_Job *resolve_job_new(Allocator *allocator, Resolve_Job_Kind kind, AST_Node *ast_node,
                                 Scope *scope);
    Resolve_Job *resolve_job_ident_new(Allocator *allocator, AST_Node *ast_node, Scope *scope);
    Resolve_Job *resolve_job_type_new(Allocator *allocator, AST_Node *ast_node, Scope *scope);
    Resolve_Job *resolve_job_size_new(Allocator *allocator, AST_Node *ast_node, Scope *scope);

    void free_job(Resolver *resolver, Resolve_Job *job);
}
