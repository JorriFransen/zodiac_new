
#pragma once

#include "allocator.h"
#include "zodiac_string.h"

namespace Zodiac
{

bool os_is_relative_path(const String& path);
const String os_get_absolute_path(Allocator* allocator, const String& path);
const char* os_get_cwd(Allocator* allocator);

}
