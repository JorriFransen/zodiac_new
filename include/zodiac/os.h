#pragma once

#include "allocator.h"
#include "zodiac_string.h"

bool is_relative_path(const char* path);
const char* get_absolute_path(Allocator* allocator, const char* path);

bool is_regular_file(String file_path);

String read_file_string(Allocator* allocator, String file_path);
