
#include "builtin.h"
#include "lexer.h"
#include "parser.h"
#include "c_allocator.h"
#include "ast.h"

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

    Lexer lexer = lexer_create(ca, &build_data);
    String file_path = string_ref(argv[1]);
    Lexed_File lexed_file = lexer_lex_file(&lexer, file_path);
    if (!lexed_file.valid) return 1;

    // lexed_file_print(&lexed_file);

    Token_Stream* token_stream = lexer_new_token_stream(ca, &lexed_file);

    Parser parser = parser_create(ca);
    Parsed_File parsed_file = parser_parse_file(&parser, token_stream);

    //parsed_file_print(&parsed_file);

    AST_Node* ast_root = ast_create_from_parsed_file(ca, &parsed_file);
    assert(ast_root);
    ast_print(ast_root);

    printf("\nSCOPE DUMP:\n");
    ast_print_scope(ast_root);

    return 0;
}
