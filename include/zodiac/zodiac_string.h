
#pragma once

#include "allocator.h"

typedef const char* String;

String copy_string(Allocator* allocator, String string);
