
#pragma once

#include "allocator.h"
#include "array.h"
#include "hash_table.h"
#include "token.h"
#include "file_pos.h"

struct Lexer
{
    Allocator* allocator = nullptr;
};

struct Lexed_File
{
    const char* path = nullptr;
    Array<Token> tokens = {};
    Hash_Table<Token, File_Pos> file_positions = {};
};

Lexer lexer_create(Allocator* allocator);
void lexer_init(Allocator* allocator, Lexer* lexer);
Lexed_File lexer_lex_file(Lexer* lexer, const char* file_path);

void lexed_file_print(Lexed_File* lf);
