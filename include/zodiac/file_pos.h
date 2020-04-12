
#pragma once

#include "zodiac_string.h"

#include <stdint.h>

namespace Zodiac
{

struct File_Pos
{
    uint64_t index = 0;
    uint64_t line = 0;
    uint64_t column = 0;
    String file_name = {};
};

bool operator==(const File_Pos& a, const File_Pos& b);
bool operator!=(const File_Pos& a, const File_Pos& b);

}
