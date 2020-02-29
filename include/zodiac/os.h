#pragma once

#include "allocator.h"
#include "zodiac_string.h"

bool is_relative_path(const String& path);
const String get_absolute_path(Allocator* allocator, const String& path);

bool is_regular_file(const String& file_path);

String read_file_string(Allocator* allocator, const String& file_path);
