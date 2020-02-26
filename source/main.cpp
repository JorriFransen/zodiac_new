
#include "lexer.h"
#include "parser.h"
#include "c_allocator.h"

#include <stdio.h>
#include <cassert>

int main(int argc, char** argv)
{
    assert(argc == 2);

    auto ca = c_allocator_get();

    Lexer lexer = lexer_create(ca);
    Lexed_File lexed_file = lexer_lex_file(&lexer, argv[1]);

    // lexed_file_print(&lexed_file);

    Token_Stream* token_stream = lexer_new_token_stream(ca, &lexed_file);

    Parser parser = parser_create(ca);
    Parsed_File parsed_file = parser_parse_file(&parser, token_stream);

    parsed_file_print(&parsed_file);

    return 0;
}
