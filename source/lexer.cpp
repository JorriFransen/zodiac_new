
#include "lexer.h"

#include "os.h"
#include "temp_allocator.h"
#include "zodiac_string.h"

#include <cassert>

Lexer lexer_create(Allocator* allocator)
{
    Lexer result = {};
    lexer_init(allocator, &result);
    return result;
}

void lexer_init(Allocator* allocator, Lexer* lexer)
{
    lexer->allocator = allocator;
}

Lexed_File lexer_lex_file(Lexer* lexer, const char* file_path)
{
    if (is_relative_path(file_path)) {
        file_path = get_absolute_path(temp_allocator_get(), file_path);
    }

    Lexed_File result = {};
    result.path = copy_string(lexer->allocator, file_path);
    array_init(lexer->allocator, &result.tokens);
    hash_table_init(lexer->allocator, &result.file_positions);

    return result;
}

void lexed_file_print(Lexed_File* lf)
{
    assert(false);
}
