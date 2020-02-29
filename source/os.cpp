#include "os.h"

#ifdef linux
#include "os_linux.h"
#include "sys/stat.h"
#include <stdio.h>
#endif

#include <cassert>

bool is_relative_path(const String& path)
{
    return os_is_relative_path(path);
}

const String get_absolute_path(Allocator* allocator, const String& path)
{
    assert(is_relative_path(path));
    return os_get_absolute_path(allocator, path);
}

bool is_regular_file(const String& file_path)
{
    struct stat statbuf;
    auto stat_res = stat(file_path.data, &statbuf);
    if (stat_res != 0)
    {
        return false;
    }

    return S_ISREG(statbuf.st_mode);
}

String read_file_string(Allocator* allocator, const String& file_path)
{
    assert(is_regular_file(file_path));

    auto file = fopen(file_path.data, "rb");
    assert(file);

    int64_t length = 0;
    fseek(file, 0, SEEK_END);
    length = ftell(file);
    fseek(file, 0, SEEK_SET);

    String result = { alloc_array<char>(allocator, length + 1), length };

    auto read_res = fread(result.data, 1, length, file);
    assert((int64_t)read_res == length);

    fclose(file);

    result.data[length] = '\0';

    return result;
}
