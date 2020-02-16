
#pragma once

#include "allocator.h"

bool os_is_relative_path(const char* path);
const char* os_get_absolute_path(Allocator* allocator, const char* path);
const char* os_get_cwd(Allocator* allocator);
