#include "os_windows.h"

#include "windows.h"
#include "shlwapi.h"

namespace Zodiac
{

bool os_is_relative_path(const String& path)
{
    bool result = PathIsRelativeA(path.data);
    return result;
}

bool os_is_regular_file(const String& path)
{
    auto attrs = GetFileAttributesA(path.data);
    bool result = attrs | FILE_ATTRIBUTE_NORMAL;
    return result;
}

const String os_get_file_name(Allocator *allocator, const String &path)
{
    auto start_idx = string_last_index_of(path, '\\');
    if (start_idx == -1) return string_copy(allocator, path);
    start_idx += 1;

    return string_copy(allocator, path, start_idx, path.length - start_idx);
}

const String os_get_absolute_path(Allocator* allocator, const String& path)
{
    auto size = GetFullPathNameA(path.data, 0, nullptr, nullptr);
    auto buf = alloc_array<char>(allocator, size);
    auto written_size = GetFullPathNameA(path.data, size, buf, nullptr);
    assert(written_size == size - 1);
    buf[written_size] = '\0';
    return string_ref(buf, size - 1);
}

//const String os_normalize_path(Allocator *allocator, const String &path)
//{
    //assert(false);
    //return {};
//}

const char* os_get_cwd(Allocator* allocator)
{
    assert(false);
    return nullptr;
}

Process_Info os_execute_process(Allocator *allocator, const String &command, const String &args)
{
    assert(false);
    Process_Info result = {};
    return result;
}
}
