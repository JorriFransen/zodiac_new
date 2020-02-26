
#pragma once

#include "allocator.h"
#include "atom.h"
#include "array.h"
#include "hash_table.h"
#include "token.h"
#include "token_stream.h"
#include "file_pos.h"
#include "zodiac_string.h"

using namespace Zodiac;

struct Lexer
{
    Allocator* allocator = nullptr;
    Atom_Table atom_table = { };
};

struct Lexed_File
{
    const char* path = nullptr;
    Array<Token> tokens = {};
    Hash_Table<Token, File_Pos> file_positions = {};
};

struct Lexer_Data
{
    Lexer* lexer = nullptr;
    String file_path = nullptr;
    String file_data = nullptr;
    uint64_t file_index = 0;
    uint64_t file_size = 0;

    uint64_t current_line = 1;
    uint64_t current_column = 1;

    Lexed_File lexed_file = {};
};

struct Lexed_File_Token_Stream : public Token_Stream
{
    Lexed_File* lexed_file = nullptr;
    int64_t current_index = 0;

    Lexed_File_Token_Stream(){}
    ~Lexed_File_Token_Stream(){}

    Token current_token();
    Token next_token();
    Token peek_token(uint64_t offset);

};

Lexer lexer_create(Allocator* allocator);
void lexer_init(Allocator* allocator, Lexer* lexer);
Lexed_File lexer_lex_file(Lexer* lexer, const char* file_path);

Lexer_Data lexer_data_create(Lexer* lexer, String file_path, String file_data, uint64_t file_size);

Token next_token(Lexer_Data* ld);

Token lex_identifier(Lexer_Data* ld);
Token lex_number_literal(Lexer_Data* ld);

void advance(Lexer_Data* ld, uint64_t count = 1);

char current_char(Lexer_Data* ld);
char peek_char(Lexer_Data* ld, uint64_t offset);

const char* current_char_ptr(Lexer_Data* ld);

void skip_whitespace(Lexer_Data* ld);

bool is_alpha(char c);
bool is_alpha_num(char c);
bool is_num(char c);

bool is_whitespace(char c);
bool is_newline(char c);

File_Pos get_file_pos(Lexer_Data* ld);

void lexed_file_print(Lexed_File* lf);

Token_Stream* lexer_new_token_stream(Allocator* allocator, Lexed_File* lf);
