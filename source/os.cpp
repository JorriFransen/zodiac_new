#include "os.h"

#ifdef linux
#include "os_linux.h"
#endif

#include <cassert>

bool is_relative_path(const char* path)
{
    return os_is_relative_path(path);
}

const char* get_absolute_path(Allocator* allocator, const char* path)
{
    assert(is_relative_path(path));
    return os_get_absolute_path(allocator, path);
}
