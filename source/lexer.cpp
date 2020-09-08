
#include "lexer.h"

#include "os.h"
#include "temp_allocator.h"
#include "zodiac_string.h"

#include <stdio.h>
#include <inttypes.h>

Lexer lexer_create(Allocator* allocator, Build_Data* build_data)
{
    Lexer result = {};
    lexer_init(allocator, build_data, &result);
    return result;
}

void lexer_init(Allocator* allocator, Build_Data* build_data, Lexer* lexer)
{
    lexer->allocator = allocator;
    lexer->build_data = build_data;
}

Lexed_File lexer_lex_file(Lexer* lexer, const String& _file_path)
{
    String file_path = _file_path;

    Lexed_File result = {};

    if (!is_regular_file(file_path))
    {
        fprintf(stderr, "Error: Invalid file path: '%s'\n", file_path.data);
        return result;
    }

    if (is_relative_path(file_path))
    {
        file_path = get_absolute_path(temp_allocator_get(), file_path);
    }

    auto file_data = read_file_string(lexer->allocator, file_path);
    auto file_size = string_length(file_data);
    Lexer_Data ld = lexer_data_create(lexer, file_path, file_data, file_size);

    result.path = string_copy(lexer->allocator, file_path);
    array_init(lexer->allocator, &result.tokens);

    result.valid = true;

    ld.lexed_file = result;

    while (current_char(&ld) != EOF && ld.file_index < ld.file_size)
    {
        Token t = next_token(&ld);
        array_append(&ld.lexed_file.tokens, t);
    }

    return ld.lexed_file;
}

void lexer_free_lexed_file(Lexer *lexer, Lexed_File *lexed_file)
{
    string_free(lexer->allocator, lexed_file->path);
    array_free(&lexed_file->tokens);    
}

Lexer_Data lexer_data_create(Lexer* lexer, String file_path, String file_data, uint64_t file_size)
{
    Lexer_Data result = {};
    result.lexer = lexer;
    result.file_path = file_path;
    result.file_data = file_data;
    result.file_size = file_size;
    result.current_line = 1;
    result.current_column = 1;

    return result;
}

Token next_token(Lexer_Data* ld)
{
restart:
    skip_whitespace(ld);

    auto c = current_char(ld);

#define __1_CHAR_TOKEN_CASE(_c, kind) \
        case _c: { \
            auto fp = get_file_pos(ld); \
            auto atom = atom_get(&ld->lexer->build_data->atom_table, &c, 1); \
            advance(ld); \
            return token_create(fp, kind, atom); \
        }

#define __2_CHAR_TOKEN_CASE(_c1, kind1, _c2, kind2) \
        case _c1: { \
            auto fp = get_file_pos(ld); \
            auto ccp = current_char_ptr(ld); \
            uint64_t len = 0; \
            Token_Kind kind = TOK_INVALID; \
            if (peek_char(ld, 1) == _c2) { \
                kind = kind2; \
                len = 2; \
            } else { \
                kind = kind1; \
                len = 1; \
            } \
            advance(ld, len); \
            auto atom = atom_get(&ld->lexer->build_data->atom_table, ccp, len); \
            return token_create(fp, kind, atom); \
        }

    switch (c)
    {
        __1_CHAR_TOKEN_CASE('#', TOK_POUND);
        __1_CHAR_TOKEN_CASE(':', TOK_COLON);
        __1_CHAR_TOKEN_CASE(';', TOK_SEMICOLON);
        __1_CHAR_TOKEN_CASE('@', TOK_AT);
        __1_CHAR_TOKEN_CASE('.', TOK_DOT);
        __1_CHAR_TOKEN_CASE(',', TOK_COMMA);
        __1_CHAR_TOKEN_CASE('$', TOK_DOLLAR);

        __1_CHAR_TOKEN_CASE('(', TOK_LPAREN);
        __1_CHAR_TOKEN_CASE(')', TOK_RPAREN);
        __1_CHAR_TOKEN_CASE('{', TOK_LBRACE);
        __1_CHAR_TOKEN_CASE('}', TOK_RBRACE);
        __1_CHAR_TOKEN_CASE('[', TOK_LBRACK);
        __1_CHAR_TOKEN_CASE(']', TOK_RBRACK);

        __1_CHAR_TOKEN_CASE('<', TOK_LT);
        __1_CHAR_TOKEN_CASE('>', TOK_GT);

        __1_CHAR_TOKEN_CASE('+', TOK_PLUS);
        __1_CHAR_TOKEN_CASE('*', TOK_STAR);
        __1_CHAR_TOKEN_CASE('%', TOK_PERCENT);

        __2_CHAR_TOKEN_CASE('=', TOK_EQ, '=', TOK_EQ_EQ);
        __2_CHAR_TOKEN_CASE('-', TOK_MINUS, '>', TOK_RARROW);
        __2_CHAR_TOKEN_CASE('!', TOK_BANG, '=', TOK_NEQ);

        case '/':
        {
            if (peek_char(ld, 1) == '/')
            {
                while (!is_newline(current_char(ld))) advance(ld);
                goto restart;
            }
            else 
            {
                auto fp = get_file_pos(ld);
                auto ccp = current_char_ptr(ld);
                advance(ld);
                auto atom = atom_get(&ld->lexer->build_data->atom_table, ccp, 1);
                return token_create(fp, TOK_FORWARD_SLASH, atom);
            }
            break;
        }

        case EOF: // This can happen here after we've skipped whitespace
        {
            auto fp = get_file_pos(ld);
            return token_create(fp, TOK_EOF, {});
            break;
        }

        default:
        {
            if (c == '"')
            {
                return lex_string_literal(ld);
            }
            else if (is_alpha(c) || c == '_')
            {
                return lex_keyword_or_identifier(ld);
            }
            else if (is_num(c) || c == '.')
            {
                return lex_number_literal(ld);
            }

            fprintf(stderr, "%s:%" PRIu64 ":%" PRIu64 ": Error: Unexpected character: '%c'\n", 
                    ld->file_path.data, ld->current_line, ld->current_column, c);
            assert(false);
            break;
        }
    }

    assert(false);
    return {};
}

Token lex_keyword_or_identifier(Lexer_Data* ld)
{
    Token result = lex_identifier(ld);

    const auto& kw_atoms = ld->lexer->build_data->kw_atoms;

    for (int64_t i = 0; i < kw_atoms.count; i++)
    {
        if (result.atom == kw_atoms[i])
        {
            result.kind = KW_Tokens[i].kind;
            break;
        }
    }

    return result;
}

Token lex_identifier(Lexer_Data* ld)
{
#if DEBUG
    auto fc = current_char(ld);
    assert(is_alpha(fc) || fc == '_');
#endif

    File_Pos begin_fp = get_file_pos(ld);

    File_Pos last_valid_fp = begin_fp;

    while (is_alpha_num(current_char(ld)) || current_char(ld) == '_')
    {
        advance(ld);
        last_valid_fp = get_file_pos(ld);
    }

    auto end_fp = last_valid_fp;
    auto length = end_fp.index - begin_fp.index;
    Atom atom = atom_get(&ld->lexer->build_data->atom_table, &ld->file_data[begin_fp.index],
                         length);

    return token_create(begin_fp, end_fp, TOK_IDENTIFIER, atom);
}

Token lex_number_literal(Lexer_Data* ld)
{
    auto fc = current_char(ld);
    assert(is_num(fc) || fc == '.');

    bool found_dot = false;
    if (fc == '.') found_dot = true;

    File_Pos begin_fp = get_file_pos(ld);
    File_Pos last_valid_fp = begin_fp;

    while (is_num(current_char(ld)) || (current_char(ld) == '.' && !found_dot))
    {
        advance(ld);
        last_valid_fp = get_file_pos(ld);
    }

    auto end_fp = last_valid_fp;
    auto length = end_fp.index - begin_fp.index;
    Atom atom = atom_get(&ld->lexer->build_data->atom_table, &ld->file_data[begin_fp.index],
                         length);

    return token_create(begin_fp, end_fp, TOK_NUMBER_LITERAL, atom);
}

Token lex_string_literal(Lexer_Data* ld)
{
#if DEBUG
    auto fc = current_char(ld);
    assert(fc == '"');
#endif

    File_Pos begin_fp = get_file_pos(ld);

    advance(ld);

    while (current_char(ld) != '"')
    {
        advance(ld);
    }

    advance(ld);

    auto end_fp = get_file_pos(ld);
    auto length = end_fp.index - begin_fp.index;
    assert(length >= 2);
    auto ta = temp_allocator_get();
    String string = lexer_replace_character_literals(ta, &ld->file_data[begin_fp.index] + 1,
                                                     length - 2);
    Atom atom = atom_get(&ld->lexer->build_data->atom_table, string);

    return token_create(begin_fp, end_fp, TOK_STRING_LITERAL, atom);
}

void advance(Lexer_Data* ld, uint64_t count/*=1*/)
{
    assert(ld->file_index < ld->file_size);

    while (count && current_char(ld) != EOF)
    {
        char c = current_char(ld);

        ld->file_index += 1;
        ld->current_column += 1;

        if (c == '\n')
        {
            ld->current_line += 1;
            ld->current_column = 1;
        }

        count -= 1;
    }
}

char current_char(Lexer_Data* ld)
{
    return peek_char(ld, 0);
}

char peek_char(Lexer_Data* ld, uint64_t offset)
{
    if (ld->file_index + offset < ld->file_size)
        return ld->file_data[ld->file_index + offset];

    return EOF;
}

const char* current_char_ptr(Lexer_Data* ld)
{
    if (ld->file_index < ld->file_size)
    {
        return &ld->file_data[ld->file_index];
    }

    return nullptr;
}

void skip_whitespace(Lexer_Data* ld)
{
    while (is_whitespace(current_char(ld))) 
    {
        advance(ld);
    }
}

bool is_alpha(char c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

bool is_alpha_num(char c)
{
    return is_alpha(c) || is_num(c);
}

bool is_num(char c)
{
    return c >= '0' && c <= '9';
}

bool is_whitespace(char c)
{
    return c == ' ' || c == '\t' || is_newline(c);
}

bool is_newline(char c)
{
    return c == '\n' || c == '\r';
}

File_Pos get_file_pos(Lexer_Data* ld)
{
    File_Pos result = {};

    result.index = ld->file_index;
    result.line = ld->current_line;
    result.column = ld->current_column;
    result.file_name = ld->file_path;

    return result;
}

void lexed_file_print(Lexed_File* lf)
{
    printf("Lexed file: '%s'\n", lf->path.data);
    for (int64_t i = 0; i < lf->tokens.count; i++)
    {
        token_print(lf->tokens[i]);
    }
}

String lexer_replace_character_literals(Allocator *allocator, const char* str, int64_t length)
{
    auto buf = alloc_array<char>(allocator, length + 1);
    int64_t actual_length = 0;

    for (int64_t i = 0; i < length; i++)
    {
        char c = str[i];
        if (c == '\\')
        {
            assert(i + 1 < length);
            char c2 = str[i + 1];

            switch (c2)
            {
                case 'n': c = '\n'; break;

                default: assert(false);
            }

            i += 1;
        }

        buf[actual_length++] = c;
    }

    buf[actual_length] = '\0';

    return string_ref(buf, actual_length);
}

Token_Stream* lexer_new_token_stream(Allocator* allocator, Lexed_File* lf)
{
    assert(lf);

    auto lfts = alloc_type<Lexed_File_Token_Stream>(allocator);
    assert(lfts);

    lfts->lexed_file = lf;

    return lfts;
}

Token Lexed_File_Token_Stream::current_token()
{
    return peek_token(0);
}

Token Lexed_File_Token_Stream::next_token()
{
    current_index++;
    return current_token();
}

Token Lexed_File_Token_Stream::peek_token(uint64_t offset)
{
    if (current_index + offset >= (uint64_t)lexed_file->tokens.count)
    {
        Token result = {};
        result.kind = TOK_EOF;
        return result;
    }

    return lexed_file->tokens[current_index + offset];
}

