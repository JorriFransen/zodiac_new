#pragma once

#include "allocator.h"

bool is_relative_path(const char* path);
const char* get_absolute_path(Allocator* allocator, const char* path);
