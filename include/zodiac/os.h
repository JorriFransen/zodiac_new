#pragma once

#include "allocator.h"
#include "array.h"
#include "zodiac_string.h"

namespace Zodiac
{

bool is_relative_path(const String& path);
const String get_absolute_path(Allocator* allocator, const String& path);
const String get_file_name(Allocator *allocator, const String &path);
const String get_file_dir(Allocator *allocator, const String &path);

bool is_regular_file(const String& file_path);

String read_file_string(Allocator* allocator, const String& file_path);

const char *get_cwd(Allocator *allocator);

struct Process_Info
{
    int64_t exit_code = 0;
    bool success = false; 
};

Process_Info execute_process(Allocator *allocator, const String &command, const String &args);

int64_t os_syscall(Array<int64_t> args);

}
