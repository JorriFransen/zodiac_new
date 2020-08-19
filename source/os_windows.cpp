#include "os_windows.h"

#ifdef _WIN32

#include "temp_allocator.h"

#include <stdio.h>

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

String os_read_file_string(Allocator *allocator, const String &path)
{
    auto ta = temp_allocator_get();

    auto wide_path = widen(ta, path);
    auto wide_mode = widen(ta, "rb");

    auto file = _wfopen(wide_path.c_str(), wide_mode.c_str());
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

Unicode_String widen(Allocator *allocator, const String &str)
{
    if (str.length == 0)
    {
        assert(str.data == nullptr);
        return {};
    }

    auto size = MultiByteToWideChar(CP_UTF8, MB_PRECOMPOSED,
                                    str.data, str.length + 1,
                                    nullptr, 0);
    if (size == 0)
    {
        assert(false);
    }

    LPWSTR buf = alloc_array<WCHAR>(allocator, size);
    auto written_size = MultiByteToWideChar(CP_UTF8, MB_PRECOMPOSED,
                                            str.data, str.length + 1,
                                            buf, size);
    assert(written_size == size);

    return unicode_string_ref((LPCWSTR)buf, size - 1);
}

Unicode_String widen(Allocator *allocator, const char *cstr)
{
    return widen(allocator, string_ref(cstr));
}

Process_Info os_execute_process(Allocator *allocator, const String &command, const String &args)
{
    Process_Info result = {};

    PROCESS_INFORMATION process_info;
    STARTUPINFO startup_info;
    ZeroMemory(&startup_info, sizeof(startup_info));
    ZeroMemory(&process_info, sizeof(process_info));
    startup_info.cb = sizeof(startup_info);

    auto ta = temp_allocator_get();

    auto cmd_str = widen(ta, command);
    auto arg_str = widen(ta, args);

    bool proc_res = CreateProcessW(cmd_str.c_str(), (LPWSTR)arg_str.c_str(),
                                   nullptr, nullptr, true, 0, nullptr,
                                   nullptr, &startup_info, &process_info);

    if (!proc_res)
    {
        auto err = GetLastError();
        LPSTR message_buf = nullptr;
        size_t size = FormatMessageA((FORMAT_MESSAGE_ALLOCATE_BUFFER |
                                      FORMAT_MESSAGE_FROM_SYSTEM |
                                      FORMAT_MESSAGE_IGNORE_INSERTS),
                                     nullptr, err, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                                     (LPSTR)&message_buf, 0, nullptr);

        fprintf(stderr, "%.*s", (int)size, message_buf);
        LocalFree(message_buf);
        assert(false);
    }
    else
    {
        WaitForSingleObject(process_info.hProcess, INFINITE);
        DWORD exit_code;
        GetExitCodeProcess(process_info.hProcess, &exit_code);

        result.success = true;
        result.exit_code = exit_code;
    }

    return result;
}
}

#endif // #ifdef WIN32
