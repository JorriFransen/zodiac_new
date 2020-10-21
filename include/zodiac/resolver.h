#pragma once

#include "build_data.h"
#include "bytecode.h"
#include "struct_predecls.h"
#include "queue.h"
#include "parser.h"
#include "lexer.h"
#include "llvm_builder.h"
#include "zodiac_error.h"

#include <cstdarg>

namespace Zodiac
{
    struct Resolve_Job;

    struct Parsed_Module
    {
        String full_path = {};
        AST_Module *ast;
    };

    struct Resolver_Progression
    {
        bool scope_imports_done = false;

        int64_t parse_job_count = 0;
        int64_t ident_job_count = 0;
        int64_t type_job_count = 0;
        int64_t size_job_count = 0;
        int64_t bytecode_job_count = 0;
        int64_t llvm_job_count = 0;
        int64_t binary_job_count = 0;
    };

    struct Resolver
    {
        Allocator *allocator = nullptr;

        String first_file_path = {}; // Full absolute path with extension
        String first_file_name = {}; // File name without extension
        String first_file_dir = {};; // Full absolute path without file name

        Build_Data *build_data = nullptr;
        Lexer lexer = {};
        Parser parser = {};
        Scope *global_scope = nullptr;
        Bytecode_Builder bytecode_builder = {};
        LLVM_Builder llvm_builder = {};

        AST_Declaration *entry_decl = nullptr;
        AST_Declaration *bc_entry_decl = nullptr;

        Stack<AST_Node *> break_node_stack = {};

        Queue<Resolve_Job*> parse_job_queue = {};
        Queue<Resolve_Job*> ident_job_queue = {};
        Queue<Resolve_Job*> type_job_queue = {};
        Queue<Resolve_Job*> size_job_queue = {};
        Queue<Resolve_Job*> emit_bytecode_job_queue = {};
        Queue<Resolve_Job*> emit_llvm_func_job_queue = {};
        Queue<Resolve_Job*> emit_llvm_binary_job_queue = {};

        Resolver_Progression progression = {};

        Array<Parsed_Module> parsed_modules = {};

        bool parse_error = false;
        bool llvm_error = false;
    };

    struct Resolve_Result
    {
        uint64_t error_count = 0;
        bool parse_error = false;
        bool llvm_error = false;
    };

    enum class Resolve_Job_Kind
    {
        INVALID,
        PARSE,
        IDENTIFIER,
        TYPE,
        SIZE,
        EMIT_BYTECODE,
        EMIT_LLVM_FUNC,
        EMIT_LLVM_GLOB,
        EMIT_LLVM_BINARY,
    };

    struct Resolve_Job
    {
        Resolve_Job() {};

        Resolve_Job_Kind kind = Resolve_Job_Kind::INVALID;
        Scope *node_scope = nullptr;

        union
        {
            AST_Node *ast_node = nullptr;
            AST_Declaration *declaration;
            AST_Statement *statement;
            AST_Expression *expression;
            AST_Identifier *identifier;
            Bytecode_Function *bc_func;
            Bytecode_Global bc_glob;

            struct
            {
                String module_name;
                String module_path;
                AST_Declaration *import_decl;
            } parse;

            struct
            {
                const char *output_file_name;
            } llvm_bin;
        };


        union
        {
            AST_Module *ast_module = nullptr;
            Bytecode_Function *bc_func;
            Bytecode_Global bc_global;
        } result = {};
    };

    void resolver_init(Allocator *allocator,  Resolver *resolver,
                       Build_Data *build_data, String first_file_path);

    void start_resolving(Resolver *resolver, bool blocking);
    Resolve_Result finish_resolving(Resolver *resolver);

    void start_resolve_pump(Resolver *resolver);

    bool resolver_has_progressed(Resolver *resolver);
    void resolver_save_progression(Resolver *resolver);

    bool try_resolve_job(Resolver *resolver, Resolve_Job *job);

    bool try_resolve_identifiers(Resolver *resolver, AST_Node *ast_node, Scope *scope);
    bool try_resolve_identifiers(Resolver *resolver, AST_Identifier *identifier, Scope *scope);
    bool try_resolve_identifiers(Resolver *resolver, AST_Declaration *ast_decl, Scope *scope);
    bool try_resolve_identifiers(Resolver *resolver, AST_Statement *ast_stmt, Scope *scope);
    bool try_resolve_identifiers(Resolver *resolver, AST_Expression *ast_expr, Scope *scope);
    bool try_resolve_identifiers_dot_expr(Resolver *resolver, AST_Expression *ast_expr,
                                          Scope *scope);
    bool try_resolve_identifiers(Resolver *resolver, AST_Type_Spec *ast_ts, Scope *scope);

    bool try_resolve_types(Resolver *resolver, AST_Node *ast_node, Scope *scope, 
                           AST_Type **inferred_return_type);
    bool try_resolve_types(Resolver *resolver, AST_Identifier *identifier,
                           AST_Type **type_dest, Scope *scope);
    bool try_resolve_types(Resolver *resolver, AST_Declaration *ast_decl, Scope *scope);
    bool try_resolve_types(Resolver *resolver, AST_Statement *ast_stmt, Scope *scope,
                           AST_Type **inferred_return_type);
    bool try_resolve_types(Resolver *resolver, AST_Expression *ast_expr, Scope *scope);
    bool try_resolve_types(Resolver *resolver, AST_Expression *ast_expr,
                           AST_Type *suggested_type, Scope *scope);
    bool try_resolve_builtin_call_types(Resolver *resolver, AST_Expression *call_expr,
                                        Scope *scope);
    bool try_resolve_types(Resolver *resolver, AST_Type_Spec *ts, Scope *scope,
                           AST_Type **type_target, AST_Type *suggested_type);

    bool try_resolve_sizes(Resolver *resolver, AST_Node *ast_node, Scope *scope);
    bool try_resolve_sizes(Resolver *resolver, AST_Type *ast_type, Scope *scope);

    void resolver_expand_switch_case_ranges(Resolver *resolver,
                                            AST_Statement *stmt,
                                            AST_Switch_Case *switch_case,
                                            uint64_t range_count,
                                            Scope *switch_scope);

    bool resolver_switch_case_expressions_are_typed(AST_Statement *ast_stmt);
    bool resolver_check_switch_completeness(Resolver *resolver, AST_Statement *ast_stmt);

    void resolver_push_break_node(Resolver *resolver, AST_Node *node);
    void resolver_pop_break_node(Resolver *resolver);

    AST_Type *find_or_create_function_type(Resolver *resolver, Array<AST_Type*> param_types,
                                           AST_Type *return_type, Scope *scope);
    AST_Type *find_or_create_array_type(Resolver *resolver, AST_Type *element_type,
                                        int64_t element_count, Scope *current_scope);
    AST_Type *create_structure_type(Resolver *resolver, AST_Declaration *struct_decl, 
                                    Array<AST_Type*> mem_types, Scope *mem_scope,
                                    Scope *current_scope);
    AST_Type *find_or_create_enum_type(Resolver *resolver, AST_Declaration *enum_decl,
                                       AST_Type *base_type, 
                                       Scope *mem_scope,
                                       Scope *current_scope);

    void queue_parse_job(Resolver *resolver, String module_name, String module_path,
                         AST_Declaration *import_decl);
    void queue_ident_job(Resolver *resolver, AST_Node *ast_node, Scope *scope);
    void queue_type_job(Resolver *resolver, AST_Node *ast_node, Scope *scope);
    void queue_size_job(Resolver *resolver, AST_Node *ast_node, Scope *scope);
    void queue_emit_bytecode_job(Resolver *resolver, AST_Node *ast_node, Scope *scope);
    void queue_emit_llvm_func_job(Resolver *resolver, Bytecode_Function *bc_func);
    void queue_emit_llvm_global_job(Resolver *resolver, Bytecode_Global bc_glob);
    void queue_emit_llvm_binary_job(Resolver *resolver, const char *output_file_name);

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
    Resolve_Job *resolve_job_new(Allocator *allocator, Bytecode_Global bc_glob);
    Resolve_Job *resolve_job_new(Allocator *allocator, const char *output_file_name);
    Resolve_Job *resolve_job_new(Allocator *allocator, String module_name, String module_path,
                                 AST_Declaration *import_decl);
    Resolve_Job *resolve_job_ident_new(Allocator *allocator, AST_Node *ast_node, Scope *scope);
    Resolve_Job *resolve_job_type_new(Allocator *allocator, AST_Node *ast_node, Scope *scope);
    Resolve_Job *resolve_job_size_new(Allocator *allocator, AST_Node *ast_node, Scope *scope);
    Resolve_Job *resolve_job_emit_bytecode_new(Allocator *allocator, AST_Node *ast_node,
                                               Scope *scope);
    Resolve_Job *resolve_job_emit_llvm_func_new(Allocator *allocator, Bytecode_Function *bc_func);
    Resolve_Job *resolve_job_emit_llvm_global_new(Allocator *allocator, Bytecode_Global bc_glob);
    Resolve_Job *resolve_job_emit_llvm_binary_new(Allocator *allocator,
                                                  const char *output_file_name);

    AST_Declaration *resolver_get_declaration(AST_Expression *expr);

    bool resolver_assign_enum_initializers(Resolver *resolver, AST_Declaration *decl,
                                           AST_Type *enum_type);
    bool resolver_import_using_scope(Resolver *resolver, Scope *current_scope, Scope *using_scope);
    bool resolver_import_from_static_if(Resolver *resolver, AST_Declaration *decl, Scope *scope);

    void resolver_inherit_const(AST_Expression *expr);

    bool resolver_valid_type_conversion(AST_Type *type, AST_Type *target_type);
    bool resolver_literal_fits_in_type(const Integer_Literal &number_literal, AST_Type *type);

    bool is_entry_decl(Resolver *resolver, AST_Declaration *decl);
    bool is_bc_entry_decl(Resolver *resolver, AST_Declaration *decl);

    void free_job(Resolver *resolver, Resolve_Job *job);

    void resolver_report_undeclared_identifier(Resolver *resolver, AST_Identifier *identifier);
    void resolver_report_mismatching_types(Resolver *resolver, AST_Node *node,
                                           AST_Type *expected_type, AST_Type *actual_type);
}
