
#pragma once

#include "os.h"
#include "allocator.h"
#include "array.h"
#include "zodiac_string.h"

namespace Zodiac
{

bool os_is_relative_path(const String& path);
bool os_is_regular_file(const String& path);
const String os_get_file_name(Allocator *allocator, const String &path);
const String os_get_file_dir(Allocator *allocator, const String &path);
const String os_get_absolute_path(Allocator *allocator, const String& path);
const String os_normalize_path(Allocator *allocator, const String &path);
const char *os_get_cwd(Allocator *allocator);

const String os_read_file_string(Allocator *allocator, const String &path);

Process_Info os_execute_process(Allocator *allocator, const String &command, const String &args);

int64_t os_syscall(Array<int64_t> args);

}
