
#pragma once

#include <stdint.h>

struct File_Pos
{
    uint64_t index = 0;
    uint64_t line = 0;
    uint64_t column = 0;
    const char* file_name = nullptr;
};

bool operator==(const File_Pos& a, const File_Pos& b);
bool operator!=(const File_Pos& a, const File_Pos& b);
