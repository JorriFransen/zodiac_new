#pragma once


#ifdef WIN32

#include "zodiac_string.h"
#include "os.h"

namespace Zodiac
{

bool os_is_relative_path(const String& path);
bool os_is_regular_file(const String& path);
const String os_get_file_name(Allocator *allocator, const String &path);
const String os_get_absolute_path(Allocator* allocator, const String& path);
const String os_normalize_path(Allocator *allocator, const String &path);
const char* os_get_cwd(Allocator* allocator);

Process_Info os_execute_process(Allocator *allocator, const String &command, const String &args);
}

#endif // #ifdef WIN32
