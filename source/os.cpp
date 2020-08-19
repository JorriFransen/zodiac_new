#include "os.h"

#include <stdio.h>

#ifdef linux
#include "os_linux.h"
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
    return os_read_file_string(allocator, file_path);
}

Process_Info execute_process(Allocator *allocator, const String &command, const String &args)
{
    return os_execute_process(allocator, command, args);
}

}
