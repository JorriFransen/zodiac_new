#pragma once


#include "allocator.h"
#include "build_data.h"
#include "bytecode.h"
#include "lexer.h"
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

    struct Resolver
    {
        Allocator *allocator = nullptr;
        Build_Data *build_data = nullptr;

        Lexer lexer = {};
        Parser parser = {};
        Bytecode_Builder bytecode_builder = {};

        Queue<Parse_Job> parse_jobs = {};
        Queue<Resolve_Job> resolve_jobs = {};
        Array<Parsed_Module> parsed_modules = {};

        Scope *global_scope =  nullptr;

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

    bool try_parse_job(Resolver *resolver, Parse_Job *job);
    bool try_resolve_job(Resolver *resolver, Resolve_Job *job);

    bool try_resolve_declaration(Resolver *resolver, AST_Declaration *declaration);
    bool try_resolve_statement(Resolver *resolver, AST_Statement *statement);
    bool try_resolve_expression(Resolver *resolver, AST_Expression *expression);
    bool try_resolve_type_spec(Resolver *resolver, AST_Type_Spec *type_spec);
}
