
#include "lexer.h"
#include "c_allocator.h"

#include <stdio.h>
#include <cassert>

int main(int argc, char** argv)
{
    assert(argc == 2);

    auto ca = c_allocator_get();

    Lexer lexer = lexer_create(ca);
    Lexed_File lexed_file = lexer_lex_file(&lexer, argv[1]);

    lexed_file_print(&lexed_file);

    return 0;
}
