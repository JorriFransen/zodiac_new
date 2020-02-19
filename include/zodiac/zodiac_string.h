
#pragma once

#include "allocator.h"

typedef const char* String;

int64_t string_length(String string);
String copy_string(Allocator* allocator, String string, int64_t length);
String copy_string(Allocator* allocator, String string);
