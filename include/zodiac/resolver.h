#pragma once


#include "allocator.h"
#include "build_data.h"
#include "bytecode.h"
#include "lexer.h"
#include "llvm_builder.h"
#include "parser.h"
#include "queue.h"
#include "scope.h"
#include "zodiac_string.h"

namespace Zodiac
{
    struct Parsed_Module
    {
        String full_path = {};
        String name = {};
        AST_Module *ast;
    };

    struct Parse_Job
    {
        String module_name = {};
        String module_path = {};
        bool insert_entry_module = false;

        int64_t parsed_module_index = -1;
    };

    struct Resolve_Job
    {
        AST_Node *ast_node = nullptr;
    };

    struct Size_Job
    {
        AST_Node *ast_node = nullptr;
    };

    struct Bytecode_Job
    {
        AST_Declaration *decl = nullptr;
    };

    struct Run_Job
    {
        AST_Declaration *run_decl = nullptr;
        Bytecode_Function *wrapper = nullptr;
    };

    enum class LLVM_Job_Kind
    {
        INVALID,
        FUNCTION,
        GLOBAL,
    };

    struct LLVM_Job
    {
        LLVM_Job_Kind kind = LLVM_Job_Kind::INVALID;

        union
        {
            Bytecode_Global_Info bc_global = {};
            Bytecode_Function *bc_func;
        };
    };

    struct Resolver
    {
        Allocator *allocator = nullptr;
        Build_Data *build_data = nullptr;

        Lexer lexer = {};
        AST_Builder ast_builder = {};
        Parser parser = {};
        Bytecode_Builder bytecode_builder = {};
        LLVM_Builder llvm_builder = {};

        Queue<Parse_Job> parse_jobs = {};
        Queue<Resolve_Job> resolve_jobs = {};
        Queue<Size_Job> size_jobs = {};
        Queue<Bytecode_Job> bytecode_jobs = {};
        Queue<Run_Job> run_jobs = {};
        Queue<LLVM_Job> llvm_jobs = {};

        bool progressed = false;

        Array<Parsed_Module> parsed_modules = {};

        Scope *global_scope =  nullptr;

        AST_Declaration *entry_decl = nullptr;

        String first_file_dir = {};
        String zodiac_root_dir = {};
        String module_dir = {};
        String entry_module_path = {};
    };

    struct Resolve_Result
    {
        int64_t error_count = 0;
        bool parse_error = false;
        bool llvm_error = false;
    };

    void resolver_init(Allocator *allocator, Resolver *resolver, Build_Data *build_data,
                       String first_file_path);

    void start_resolving(Resolver *resolver);
    Resolve_Result finish_resolving(Resolver *resolver);

    bool queue_parse_job(Resolver *resolver, String module_name, String module_path,
                         AST_Module **ast_module, bool insert_entry_module = false);
    bool queue_parse_job(Resolver *resolver, String module_name, String module_path);

    void queue_resolve_job(Resolver *resolver, AST_Node *ast_node);
    void queue_size_job(Resolver *resolver, AST_Node *node);
    void queue_bytecode_job(Resolver *resolver, AST_Declaration *func_decl);
    void queue_run_job(Resolver *resolver, AST_Declaration *run_decl, Bytecode_Function *wrapper);
    void queue_llvm_job(Resolver *resolver, Bytecode_Function *bc_func);
    void queue_llvm_job(Resolver *resolver, Bytecode_Global_Info bc_global);

    bool try_parse_job(Resolver *resolver, Parse_Job *job);
    bool try_resolve_job(Resolver *resolver, Resolve_Job *job);
    bool try_size_job(Resolver *resolver, Size_Job *job);

    bool try_resolve_declaration(Resolver *resolver, AST_Declaration *declaration);
    bool try_resolve_struct_declaration(Resolver *resolver, AST_Declaration *declaration);
    bool try_resolve_statement(Resolver *resolver, AST_Statement *statement);
    bool try_resolve_expression(Resolver *resolver, AST_Expression *expression);
    bool try_resolve_type_spec(Resolver *resolver, AST_Type_Spec *type_spec);

    bool try_size_declaration(Resolver *resolver, AST_Declaration *decl);
    bool try_size_statement(Resolver *resolver, AST_Statement *statement);
    bool try_size_expression(Resolver *resolver, AST_Expression *expression);
    bool try_size_type(Resolver *resolver, AST_Type *type);
    bool try_size_type_spec(Resolver *resolver, AST_Type_Spec *type_spec);

    AST_Type *infer_type(AST_Node *ast_node);

    AST_Declaration *enclosing_function(Resolver *resolver, AST_Expression *expr);

    AST_Declaration *resolver_get_declaration(AST_Expression *expr);
    void resolver_inherit_const(AST_Expression *expr);

    void convert_condition_to_bool(Resolver *resolver, AST_Expression **p_expr);

    void do_type_conversion(Resolver *resolver, AST_Expression **p_expr, AST_Type *target_type);

    bool is_valid_type_conversion(AST_Expression *expr, AST_Type *target_type);
    bool is_valid_type_conversion(AST_Type *type, AST_Type *target_type);
    bool integer_literal_fits_in_type(Integer_Literal il, AST_Type *type);

    bool is_entry_decl(Resolver *resolver, AST_Declaration *decl);
    bool is_bytecode_entry_decl(Resolver *resolver, AST_Declaration *decl);

    void resolver_expand_switch_case_ranges(Resolver *resolver, AST_Statement *stmt,
                                            AST_Switch_Case *switch_case, uint64_t range_count,
                                            Scope *switch_scope);
    bool resolver_check_switch_completeness(Resolver *resolver, AST_Statement *ast_stmt);

    bool resolver_import_from_static_if(Resolver *resolver, AST_Declaration *decl, Scope *scope);

    AST_Type *find_or_create_enum_type(Resolver *resolver, AST_Declaration *enum_decl,
                                       AST_Type *base_type, Scope *mem_scope,
                                       Scope *current_scope);

    bool all_dependencies_emitted(Resolver *resolver, AST_Declaration *decl);

    void resolver_check_circular_dependencies(Resolver *resolver);

    bool fatal_error_reported(Resolver *resolver);

    String find_module_path(Resolver *resolver, String module_name);
    String find_zodiac_root(Allocator *allocator, Build_Data *build_data);
}
