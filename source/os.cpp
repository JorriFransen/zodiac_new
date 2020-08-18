#include "os.h"

#include <stdio.h>

#ifdef linux
#include "os_linux.h"
//#include "sys/stat.h"
#elif WIN32
#include "os_windows.h"
#endif

#include <cassert>

namespace Zodiac
{

bool is_relative_path(const String& path)
{
    return os_is_relative_path(path);
}

const String get_absolute_path(Allocator* allocator, const String& path)
{
    assert(is_relative_path(path));
    return os_get_absolute_path(allocator, path);
}

const String get_file_name(Allocator *allocator, const String &path)
{
    return os_get_file_name(allocator, path);
}

bool is_regular_file(const String& file_path)
{
    return os_is_regular_file(file_path);
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

    result.data[read_res] = '\0';

    return result;
}

Process_Info execute_process(Allocator *allocator, const String &command, const String &args)
{
    return os_execute_process(allocator, command, args);
}

}
