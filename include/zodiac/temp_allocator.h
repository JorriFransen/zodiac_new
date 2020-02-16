
#pragma once

#include "allocator.h"

struct Temp_Allocator
{
    Allocator* allocator = nullptr;
    uint8_t* data = nullptr;
    int64_t next_index = 0;
    int64_t capacity = 0;
};

static Allocator __global_instance__ = {};
static bool __global_instance_initialized__ = false;

Allocator* temp_allocator_get();
