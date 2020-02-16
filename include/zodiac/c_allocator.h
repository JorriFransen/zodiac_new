
#pragma once

#include "allocator.h"

static Allocator instance;
static bool instance_initialized;

Allocator* c_allocator_get();

