
#include "builtin.h"
#include "lexer.h"
#include "parser.h"
#include "c_allocator.h"
#include "ast.h"
#include "scope.h"
#include "resolver.h"
#include "interpreter.h"

#include <stdio.h>
#include <cassert>

using namespace Zodiac;

int main(int argc, char** argv)
{
    if (argc != 2)
    {
        fprintf(stderr, "Error: Expected 1 argument (file name).\n");
        return 1;
    }

    auto ca = c_allocator_get();

    Build_Data build_data = {};
    build_data_init(ca, &build_data);
    builtin_initialize_atoms(&build_data.atom_table);
    builtin_initialize_types(ca);

    Lexer lexer = lexer_create(ca, &build_data);
    String file_path = string_ref(argv[1]);
    Lexed_File lexed_file = lexer_lex_file(&lexer, file_path);
    if (!lexed_file.valid) return 1;

    // lexed_file_print(&lexed_file);

    Token_Stream* token_stream = lexer_new_token_stream(ca, &lexed_file);

    Parser parser = parser_create(ca, &build_data);
    Parsed_File parsed_file = parser_parse_file(&parser, token_stream);

    //parsed_file_print(&parsed_file);

    //@TODO: initialize with builtin global count
    Scope *global_scope = scope_new(ca, Scope_Kind::GLOBAL, nullptr);

    AST_Node* ast_root = ast_create_from_parsed_file(ca, &parsed_file);
    assert(ast_root);
    ast_print(ast_root);

    builtin_populate_scope(ca, global_scope);
    scope_populate_ast(ca, ast_root, global_scope);

    //printf("\nSCOPE DUMP:\n");
    //ast_print_scope(ca, ast_root);

    Resolver resolver = {};
    resolver_init(ca, ca, &resolver, &build_data);

    start_resolving(&resolver, ast_root, true);
    Resolve_Result rr = finish_resolving(&resolver);
    assert(rr.error_count == 0);

    bytecode_print(ca, &resolver.bytecode_builder);
    assert(resolver.bytecode_builder.program.entry_function);

    Interpreter interp;
    interpreter_init(ca, &interp);

    interpreter_execute_entry(&interp, &resolver.bytecode_builder.program);
    printf("Interpreter exited with code: %" PRId64 "\n",
           interp.exit_code_value.integer_literal.value);

    interpreter_free(&interp);

    llvm_print(ca, &resolver.llvm_builder);

    return 0;
}
