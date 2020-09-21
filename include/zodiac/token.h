#pragma once

#include "atom.h"
#include "file_pos.h"
#include "zodiac_string.h"

namespace Zodiac
{

#define KW_TOKEN_LIST                                      \
    DEFINE_KW_TOKEN(TOK_KW_IMPORT, "import"),              \
    DEFINE_KW_TOKEN(TOK_KW_RETURN, "return"),              \
    DEFINE_KW_TOKEN(TOK_KW_BREAK, "break"),                \
    DEFINE_KW_TOKEN(TOK_KW_FUNC,   "func"),                \
    DEFINE_KW_TOKEN(TOK_KW_STRUCT, "struct"),              \
    DEFINE_KW_TOKEN(TOK_KW_WHILE,  "while"),               \
    DEFINE_KW_TOKEN(TOK_KW_IF,     "if"),                  \
    DEFINE_KW_TOKEN(TOK_KW_ELSE,   "else"),                \
    DEFINE_KW_TOKEN(TOK_KW_TRUE,   "true"),                \
    DEFINE_KW_TOKEN(TOK_KW_FALSE,  "false"),               \
    DEFINE_KW_TOKEN(TOK_KW_USING,  "using"),               \


#define TOKEN_LIST                                         \
    DEFINE_TOKEN(TOK_INVALID),                             \
                                                           \
    KW_TOKEN_LIST                                          \
                                                           \
    DEFINE_TOKEN(TOK_COLON),                               \
    DEFINE_TOKEN(TOK_SEMICOLON),                           \
    DEFINE_TOKEN(TOK_POUND),                               \
    DEFINE_TOKEN(TOK_AT),                                  \
    DEFINE_TOKEN(TOK_DOT),                                 \
    DEFINE_TOKEN(TOK_COMMA),                               \
    DEFINE_TOKEN(TOK_DOLLAR),                              \
    DEFINE_TOKEN(TOK_BANG),                                \
                                                           \
    DEFINE_TOKEN(TOK_LPAREN),                              \
    DEFINE_TOKEN(TOK_RPAREN),                              \
    DEFINE_TOKEN(TOK_LBRACE),                              \
    DEFINE_TOKEN(TOK_RBRACE),                              \
    DEFINE_TOKEN(TOK_LBRACK),                              \
    DEFINE_TOKEN(TOK_RBRACK),                              \
                                                           \
    DEFINE_TOKEN(TOK_EQ),                                  \
    DEFINE_TOKEN(TOK_EQ_EQ),                               \
    DEFINE_TOKEN(TOK_NEQ),                                 \
    DEFINE_TOKEN(TOK_LT),                                  \
    DEFINE_TOKEN(TOK_LTEQ),                                \
    DEFINE_TOKEN(TOK_GT),                                  \
    DEFINE_TOKEN(TOK_GTEQ),                                \
                                                           \
    DEFINE_TOKEN(TOK_PLUS),                                \
    DEFINE_TOKEN(TOK_MINUS),                               \
    DEFINE_TOKEN(TOK_STAR),                                \
    DEFINE_TOKEN(TOK_FORWARD_SLASH),                       \
    DEFINE_TOKEN(TOK_PERCENT),                             \
                                                           \
    DEFINE_TOKEN(TOK_RARROW),                              \
                                                           \
                                                           \
    DEFINE_TOKEN(TOK_IDENTIFIER),                          \
    DEFINE_TOKEN(TOK_NUMBER_LITERAL),                      \
    DEFINE_TOKEN(TOK_STRING_LITERAL),                      \
    DEFINE_TOKEN(TOK_CHAR_LITERAL),                        \
                                                           \
    DEFINE_TOKEN(TOK_EOF),                                 \

enum Token_Kind
{
    #define DEFINE_TOKEN(x) x
    #define DEFINE_KW_TOKEN(x, y) x
    TOKEN_LIST
    #undef DEFINE_KW_TOKEN
    #undef DEFINE_TOKEN
};

struct KW_Token
{
    Token_Kind kind = TOK_INVALID;
    const char* string = nullptr;
};

static const KW_Token KW_Tokens[] =
{
    #define DEFINE_KW_TOKEN(x, y) { x, y }
    KW_TOKEN_LIST
    #undef DEFINE_KW_TOKEN
};

struct Token
{
    Token_Kind kind = TOK_INVALID;

    File_Pos begin_file_pos = {};
    File_Pos end_file_pos = {};

    union
    {
        Atom atom = {};
        char c;
    };
};

Token token_create(File_Pos file_pos, Token_Kind kind, Atom atom);
Token token_create(File_Pos begin_fp, File_Pos end_fp, Token_Kind kind, Atom atom);
Token token_create(File_Pos begin_fp, File_Pos end_fp, Token_Kind kind, char c);

bool token_equal(const Token& a, const Token& b);

const char* token_kind_name(Token_Kind);
void token_print(const Token& token);

}
