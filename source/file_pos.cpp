#include "file_pos.h"

#include <string.h>

bool operator==(const File_Pos& a, const File_Pos& b)
{
    if (a.index != b.index) return false;
    if (a.line != b.line) return false;
    if (a.column != b.column) return false;

    return strcmp(a.file_name, b.file_name) == 0;

}

bool operator!=(const File_Pos& a, const File_Pos& b)
{
    return !operator==(a, b);
}
