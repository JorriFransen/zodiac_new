#include "os_linux.h"

#include "temp_allocator.h"

#include <cassert>
#include <cstring>
#include <unistd.h>

bool os_is_relative_path(const char* path)
{
    assert(path);

    return path[0] != '/';
}

const char* os_get_absolute_path(Allocator* allocator, const char* path)
{
    auto ta = temp_allocator_get();

    auto cwd = os_get_cwd(ta);

    auto cwd_len = strlen(cwd);
    auto path_len = strlen(path);
    auto total_len = cwd_len + path_len + 1;

    char* result = alloc_array<char>(allocator, total_len + 1);

    memcpy(result, cwd, cwd_len);
    result[cwd_len] = '/';
    memcpy(result + cwd_len + 1, path, path_len);
    result[total_len] = '\0';

    return result;
}

const char* os_get_cwd(Allocator* allocator)
{
    const auto BUF_SIZE = 4096;
    char buf[BUF_SIZE];

    auto ret = getcwd(buf, BUF_SIZE);
    assert(ret == buf);

    auto result_len = strlen(buf);
    char* result = alloc_array<char>(allocator, result_len + 1);
    memcpy(result, buf, result_len);
    result[result_len] = '\0';

    return result;
}
