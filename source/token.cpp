#include "token.h"

#include <stdio.h>

namespace Zodiac
{

Token token_create(File_Pos file_pos, Token_Kind kind, Atom atom)
{
    Token result = { kind, file_pos, file_pos, { .atom = atom } };
    return result;
}

Token token_create(File_Pos begin_fp, File_Pos end_fp, Token_Kind kind, Atom atom)
{
    return { kind, begin_fp, end_fp, { .atom = atom } };
}

Token token_create(File_Pos begin_fp, File_Pos end_fp, Token_Kind kind, char c)
{
    return { kind, begin_fp, end_fp, { .c = c }  };
}

bool token_equal(const Token& a, const Token& b)
{
    if (a.kind != b.kind) return false;
    if (a.begin_file_pos != b.begin_file_pos) return false;
    if (a.end_file_pos != b.end_file_pos) return false;
    if (a.atom.data != b.atom.data) return false;

    return true;
}

static const char* Token_Kind_Names[] =
{
    #define DEFINE_TOKEN(x) #x
    #define DEFINE_KW_TOKEN(x, y) #x
    TOKEN_LIST
    #undef DEFINE_KW_TOKEN
    #undef DEFINE_TOKEN
};

const char* token_kind_name(Token_Kind kind)
{
    return Token_Kind_Names[kind];
}

void token_print(const Token& token)
{
    printf("%s", token_kind_name(token.kind));
    if (token.atom.data)
    {
        printf(" \"%.*s\"", (int)token.atom.length, token.atom.data);
    }
    printf("\n");
}

}
