#include "token.h"

#include <cassert>
#include <stdio.h>

namespace Zodiac
{

Token token_create(File_Pos file_pos, Token_Kind kind, String string)
{
    Token result = { kind, file_pos, file_pos, string };
    return result;
}

Token token_create(File_Pos begin_fp, File_Pos end_fp, Token_Kind kind, String string)
{
    return { kind, begin_fp, end_fp, string };
}

String token_kind_name(Token_Kind kind)
{
    return Token_Kind_Names[kind];
}

void token_print(const Token& token)
{
    printf("%s", token_kind_name(token.kind));
    if (token.string)
    {
        printf(" \"%s\"", token.string);
    }
    printf("\n");
}

}
