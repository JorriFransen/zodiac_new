
#ifdef linux

#include "os_linux.h"

#include "temp_allocator.h"

#include <cstring>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>

namespace Zodiac
{

bool os_is_path(const String &path)
{
    struct stat statbuf;
    auto stat_res = stat(path.data, &statbuf);

    if (stat_res != 0) {
        return false;
    }

    return S_ISDIR(statbuf.st_mode);
}

bool os_is_absolute_path(const String &path)
{
    struct stat statbuf;
    auto stat_res = stat(path.data, &statbuf);

    if (stat_res != 0) {
        return false;
    }

    return (!S_ISLNK(statbuf.st_mode)) && S_ISDIR(statbuf.st_mode);
}

bool os_is_relative_path(const String &path)
{
    assert(path.data);

    return path[0] != '/' || string_contains(path, "../") || string_contains(path, "./");
}

bool os_is_regular_file(const String &path)
{
    struct stat statbuf;
    auto stat_res = stat(path.data, &statbuf);
    if (stat_res != 0) {
        return false;
    }

    return S_ISREG(statbuf.st_mode);
}

bool os_is_directory(const String &path)
{
    struct stat statbuf;
    auto stat_res = stat(path.data, &statbuf);
    if (stat_res != 0) {
        return false;
    }

    return S_ISDIR(statbuf.st_mode);
}

const String os_get_file_name(Allocator *allocator, const String &path)
{
    auto start_idx = string_last_index_of(path, '/');
    if (start_idx == -1) return string_copy(allocator, path);
    start_idx += 1;

    return string_copy(allocator, path, start_idx, path.length - start_idx);
}

const String os_get_file_dir(Allocator *allocator, const String &path)
{
    auto end_idx = string_last_index_of(path, '/');
    if (end_idx == -1) assert(false);

    return string_copy(allocator, path, 0, end_idx + 1);
}

const String os_get_dir_name(Allocator *allocator, const String &path)
{
    assert(os_is_directory(path));
    assert(string_ends_with(path, "/"));

    auto _path = path;
    _path.length -= 1;

    return os_get_file_name(allocator, _path);
}

const String os_get_absolute_path(Allocator *allocator, const String& path)
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

const char *os_get_cwd(Allocator *allocator)
{
    const auto BUF_SIZE = 4096;
    char buf[BUF_SIZE];

    auto ret = getcwd(buf, BUF_SIZE);
    assert(ret == buf);

    auto result_len = strlen(ret);
    char *result = alloc_array<char>(allocator, result_len + 1);
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

Process_Info os_execute_process(const String &command, const String &args)
{
    assert(command.length);
    assert(args.length);
    assert(false);

    return {};
}

int64_t os_syscall(Array<int64_t> args)
{
    switch (args.count)
    {
        case 1: return syscall(args[0]);
        case 2: return syscall(args[0], args[1]);
        case 3: return syscall(args[0], args[1], args[2]);
        case 4: return syscall(args[0], args[1], args[2], args[3]);
        case 5: return syscall(args[0], args[1], args[2], args[3], args[4]);
        case 6: return syscall(args[0], args[1], args[2], args[3], args[4], args[5]);
        case 7: return syscall(args[0], args[1], args[2], args[3], args[4], args[5], args[6]);

        default: assert(false);
    }

    assert(false);
    return 0;
}

bool linux_find_crt_path(String *result_ptr)
{
    assert(result_ptr);
    assert(result_ptr->data == nullptr);
    assert(result_ptr->length == 0);

    const char *paths[] = {
        "/usr/lib64/",
        "/usr/lib/x86_64-linux-gnu/",
    };

    auto ta = temp_allocator_get();

    for (uint64_t i = 0; i < STATIC_ARRAY_LENGTH(paths); i++) {
        auto path = string_ref(paths[i]);

        if (os_is_absolute_path(path)) {

            auto scrt1_path = string_append(ta, path, "Scrt1.o");

            if (os_is_regular_file(scrt1_path)) {
                *result_ptr = path;
                return true;
            }

        }
    }

    assert(false && "Failed to find srt path, required for --link_c option!!!");
    return false;
}

}
#endif // #ifdef linux
