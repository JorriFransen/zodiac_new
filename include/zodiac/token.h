#pragma once

#include "atom.h"
#include "file_pos.h"
#include "zodiac_string.h"

namespace Zodiac
{

#define TOKEN_LIST                                         \
    DEFINE_TOKEN(TOK_INVALID),                             \
                                                           \
    DEFINE_TOKEN(TOK_COLON),                               \
    DEFINE_TOKEN(TOK_SEMICOLON),                           \
    DEFINE_TOKEN(TOK_POUND),                               \
    DEFINE_TOKEN(TOK_AT),                                  \
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
                                                           \
    DEFINE_TOKEN(TOK_RARROW),                              \
                                                           \
    DEFINE_TOKEN(TOK_IDENTIFIER),                          \
    DEFINE_TOKEN(TOK_NUMBER_LITERAL),                      \
                                                           \
    DEFINE_TOKEN(TOK_EOF),                                 \

enum Token_Kind
{
    #define DEFINE_TOKEN(x) x
    TOKEN_LIST
    #undef DEFINE_TOKEN
};

static const String Token_Kind_Names[] =
{
    #define DEFINE_TOKEN(x) #x
    TOKEN_LIST
    #undef DEFINE_TOKEN
};

struct Token
{
    Token_Kind kind = TOK_INVALID;

    File_Pos begin_file_pos = {};
    File_Pos end_file_pos = {};

    Atom atom = {};
};

Token token_create(File_Pos file_pos, Token_Kind kind, Atom atom);
Token token_create(File_Pos begin_fp, File_Pos end_fp, Token_Kind kind, Atom atom);

bool token_equal(const Token& a, const Token& b);

String token_kind_name(Token_Kind);
void token_print(const Token& token);

}
