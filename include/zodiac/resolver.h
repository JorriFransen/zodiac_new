#pragma once

#include "build_data.h"
#include "bytecode.h"
#include "struct_predecls.h"
#include "queue.h"
#include "llvm_builder.h"

#include <cstdarg>

namespace Zodiac
{
    struct Resolve_Job;

    enum class Resolve_Error_Kind
    {
        INVALID,
        UNDECLARED_IDENTIFIER,
    };

    struct Resolve_Error
    {
        Resolve_Error_Kind kind = Resolve_Error_Kind::INVALID;

        const char *message = nullptr;
        int64_t message_size = -1;
        AST_Node *ast_node = nullptr;
    };

    struct Resolver
    {
        Allocator *allocator = nullptr;
        Allocator *err_allocator = nullptr;

        Build_Data *build_data = nullptr;
        Bytecode_Builder bytecode_builder = {};
        LLVM_Builder llvm_builder = {};

        AST_Node *root_node = nullptr;

        Queue<Resolve_Job*> ident_job_queue = {};
        Queue<Resolve_Job*> type_job_queue = {};
        Queue<Resolve_Job*> size_job_queue = {};
        Queue<Resolve_Job*> emit_bytecode_job_queue = {};
        Queue<Resolve_Job*> emit_llvm_job_queue = {};

        Array<Resolve_Error> errors = {};
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
        EMIT_BYTECODE,
        EMIT_LLVM,
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
            Bytecode_Function *bc_func;
        };

        Bytecode_Function *result = nullptr;

        Scope *node_scope = nullptr;
    };

    void resolver_init(Allocator *allocator, Allocator *err_allocator, Resolver *resolver,
                       Build_Data *build_data);

    void start_resolving(Resolver *resolver, AST_Node *ast_node, bool blocking);
    Resolve_Result finish_resolving(Resolver *resolver);

    void start_resolve_pump(Resolver *resolver, AST_Declaration *entry_decl);

    bool try_resolve_job(Resolver *resolver, Resolve_Job *job);

    bool try_resolve_identifiers(Resolver *resolver, AST_Node *ast_node, Scope *scope);
    bool try_resolve_identifiers(Resolver *resolver, AST_Declaration *ast_decl, Scope *scope);
    bool try_resolve_identifiers(Resolver *resolver, AST_Statement *ast_stmt, Scope *scope);
    bool try_resolve_identifiers(Resolver *resolver, AST_Expression* ast_expr, Scope *scope);
    bool try_resolve_identifiers(Resolver *resolver, AST_Type_Spec *ast_ts, Scope *scope);

    bool try_resolve_types(Resolver *resolver, AST_Node *ast_node, Scope *scope);
    bool try_resolve_types(Resolver *resolver, AST_Declaration *ast_decl, Scope *scope);
    bool try_resolve_types(Resolver *resolver, AST_Statement *ast_stmt, Scope *scope);
    bool try_resolve_types(Resolver *resolver, AST_Expression *ast_expr, Scope *scope);
    bool try_resolve_builtin_call_types(Resolver *resolver, AST_Expression *call_expr, Scope *scope);
    bool try_resolve_types(Resolver *resolver, AST_Type_Spec *ts, Scope *scope,
                           AST_Type **type_target);

    bool try_resolve_sizes(Resolver *resolver, AST_Node *ast_node, Scope *scope);
    bool try_resolve_sizes(Resolver *resolver, AST_Type *ast_type, Scope *scope);

    AST_Type* find_or_create_function_type(Resolver *resolver, Array<AST_Type*> param_types,
                                           AST_Type *return_type, Scope *scope);

    void queue_ident_job(Resolver *resolver, AST_Node *ast_node, Scope *scope);
    void queue_type_job(Resolver *resolver, AST_Node *ast_node, Scope *scope);
    void queue_size_job(Resolver *resolver, AST_Node *ast_node, Scope *scope);
    void queue_emit_bytecode_job(Resolver *resolver, AST_Node *ast_node, Scope *scope);
    void queue_emit_llvm_job(Resolver *resolver, Bytecode_Function *bc_func);

    void queue_emit_bytecode_jobs_from_declaration(Resolver *resolver, AST_Declaration *entry_decl,
                                                   Scope *scope);
    void queue_emit_bytecode_jobs_from_statement(Resolver *resolver, AST_Statement *stmt,
                                                 Scope *scope);
    void queue_emit_bytecode_jobs_from_expression(Resolver *resolver, AST_Expression *expr,
                                                  Scope *scope);

    Resolve_Job *resolve_job_new(Allocator *allocator, Resolve_Job_Kind kind);
    Resolve_Job *resolve_job_new(Allocator *allocator, Resolve_Job_Kind kind, AST_Node *ast_node,
                                 Scope *scope);
    Resolve_Job *resolve_job_new(Allocator *allocator, Bytecode_Function *bc_func);
    Resolve_Job *resolve_job_ident_new(Allocator *allocator, AST_Node *ast_node, Scope *scope);
    Resolve_Job *resolve_job_type_new(Allocator *allocator, AST_Node *ast_node, Scope *scope);
    Resolve_Job *resolve_job_size_new(Allocator *allocator, AST_Node *ast_node, Scope *scope);
    Resolve_Job *resolve_job_emit_bytecode_new(Allocator *allocator, AST_Node *ast_node,
                                               Scope *scope);
    Resolve_Job *resolve_job_emit_llvm_new(Allocator *allocator, Bytecode_Function *bc_func);

    void free_job(Resolver *resolver, Resolve_Job *job);

    void resolver_report_undeclared_identifier(Resolver *resolver, AST_Identifier *identifier);
    void resolver_report_error(Resolver *resolver, Resolve_Error_Kind kind, AST_Node *ast_node,
                               const char *fmt, ...);
    void resolver_report_error(Resolver *resolver, Resolve_Error_Kind kind, AST_Node *ast_node,
                               const char *fmt, va_list args);

    void resolver_report_errors(Resolver *resolver);

    Resolve_Error resolver_make_error(Resolve_Error_Kind kind, const char *message,
                                      int64_t message_size, AST_Node *ast_node);
}
