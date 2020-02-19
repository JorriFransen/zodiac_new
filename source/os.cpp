#include "os.h"

#ifdef linux
#include "os_linux.h"
#include "sys/stat.h"
#include <stdio.h>
#endif

#include <cassert>

bool is_relative_path(const char* path)
{
    return os_is_relative_path(path);
}

const char* get_absolute_path(Allocator* allocator, const char* path)
{
    assert(is_relative_path(path));
    return os_get_absolute_path(allocator, path);
}

bool is_regular_file(String file_path)
{
    struct stat statbuf;
    auto stat_res = stat(file_path, &statbuf);
    if (stat_res != 0)
    {
        return false;
    }

    return S_ISREG(statbuf.st_mode);
}

String read_file_string(Allocator* allocator, String file_path)
{
    assert(is_regular_file(file_path));

    auto file = fopen(file_path, "rb");
    assert(file);

    int64_t length = 0;
    fseek(file, 0, SEEK_END);
    length = ftell(file);
    fseek(file, 0, SEEK_SET);

    char* result = alloc_array<char>(allocator, length + 1);

    auto read_res = fread(result, 1, length, file);
    assert((int64_t)read_res == length);

    fclose(file);

    result[length] = '\0';

    return (String)result;
}
