
#ifdef linux
#include "os_linux.h"

#include "temp_allocator.h"

#include "sys/stat.h"

#include <stdio.h>
#include <cassert>
#include <cstring>
#include <unistd.h>
#include <limits.h>
#include <stdlib.h>

namespace Zodiac
{

bool os_is_relative_path(const String& path)
{
    assert(path.data);

    return path[0] != '/' || string_contains(path, "../") || string_contains(path, "./");
}

bool os_is_regular_file(const String& path)
{
    struct stat statbuf;
    auto stat_res = stat(path.data, &statbuf);
    if (stat_res != 0)
    {
        return false;
    }

    return S_ISREG(statbuf.st_mode);
}

const String os_get_file_name(Allocator *allocator, const String &path)
{
    auto start_idx = string_last_index_of(path, '/');
    if (start_idx == -1) return string_copy(allocator, path);
    start_idx += 1;

    return string_copy(allocator, path, start_idx, path.length - start_idx);
}

const String os_get_absolute_path(Allocator* allocator, const String& path)
{
    auto ta = temp_allocator_get();

    auto cwd = os_get_cwd(ta);

    auto cwd_len = strlen(cwd);
    auto path_len = path.length;
    auto total_len = cwd_len + path_len + 1;

    String result = { alloc_array<char>(ta, total_len + 1), (int64_t)total_len };

    memcpy(result.data, cwd, cwd_len);
    result[cwd_len] = '/';
    memcpy(result.data + cwd_len + 1, path.data, path_len);
    result.data[total_len] = '\0';

    return os_normalize_path(allocator, result);
}

const String os_normalize_path(Allocator *allocator, const String &path)
{
    char *_result = realpath(path.data, nullptr);
    assert(_result);

    String result = string_copy(allocator, _result);
    ::free(_result);
    return result;
}

const char* os_get_cwd(Allocator* allocator)
{
    const auto BUF_SIZE = 4096;
    char buf[BUF_SIZE];

    auto ret = getcwd(buf, BUF_SIZE);
    assert(ret == buf);

    auto result_len = strlen(ret);
    char* result = alloc_array<char>(allocator, result_len + 1);
    memcpy(result, ret, result_len);
    result[result_len] = '\0';

    return result;
}

const String os_read_file_string(Allocator *allocator, const String &path)
{
    auto file = fopen(path.data, "rb");
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

Process_Info os_execute_process(Allocator *allocator, const String &command, const String &args)
{
    assert(allocator);
    assert(command.length);
    assert(args.length); 
    assert(false);
}
}
#endif // #ifdef linux
