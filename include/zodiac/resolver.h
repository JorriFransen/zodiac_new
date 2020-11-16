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

    struct LLVM_Job
    {
        Bytecode_Function *bc_func = nullptr;
    };

    struct Resolver
    {
        Allocator *allocator = nullptr;
        Build_Data *build_data = nullptr;

        Lexer lexer = {};
        Parser parser = {};
        Bytecode_Builder bytecode_builder = {};
        LLVM_Builder llvm_builder = {};

        Queue<Parse_Job> parse_jobs = {};
        Queue<Resolve_Job> resolve_jobs = {};
        Queue<Size_Job> size_jobs = {};
        Queue<Bytecode_Job> bytecode_jobs = {};
        Queue<LLVM_Job> llvm_jobs = {};

        Array<Parsed_Module> parsed_modules = {};

        Scope *global_scope =  nullptr;

        AST_Declaration *entry_decl = nullptr;

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

    void queue_parse_job(Resolver *resolver, String module_name, String module_path,
                         bool insert_entry_module = false);
    void queue_resolve_job(Resolver *resolver, AST_Node *ast_node);
    void queue_size_job(Resolver *resolver, AST_Node *node);
    void queue_bytecode_job(Resolver *resolver, AST_Declaration *func_decl);
    void queue_llvm_job(Resolver *resolver, Bytecode_Function *bc_func);

    bool try_parse_job(Resolver *resolver, Parse_Job *job);
    bool try_resolve_job(Resolver *resolver, Resolve_Job *job);
    bool try_size_job(Resolver *resolver, Size_Job *job);

    bool try_resolve_declaration(Resolver *resolver, AST_Declaration *declaration);
    bool try_resolve_statement(Resolver *resolver, AST_Statement *statement);
    bool try_resolve_expression(Resolver *resolver, AST_Expression *expression);
    bool try_resolve_type_spec(Resolver *resolver, AST_Type_Spec *type_spec);

    bool try_size_declaration(Resolver *resolver, AST_Declaration *decl);
    bool try_size_statement(Resolver *resolver, AST_Statement *statement);
    bool try_size_expression(Resolver *resolver, AST_Expression *expression);
    bool try_size_type(Resolver *resolver, AST_Type *type);
    bool try_size_type_spec(Resolver *resolver, AST_Type_Spec *type_spec);

    bool is_entry_decl(Resolver *resolver, AST_Declaration *decl);
}
