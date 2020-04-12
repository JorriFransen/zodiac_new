
#pragma once

#include "allocator.h"

namespace Zodiac
{

static Allocator instance;

Allocator* c_allocator_get();

}
