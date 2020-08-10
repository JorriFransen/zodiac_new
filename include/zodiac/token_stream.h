#pragma once

#include "token.h"

namespace Zodiac
{

struct Token_Stream
{
    Token_Stream(){}
    ~Token_Stream(){}

    virtual Token current_token() = 0;
    virtual Token next_token() = 0;
    virtual Token peek_token(uint64_t offset) = 0;
};

}
