
#pragma once

#include "allocator.h"

struct Temp_Allocator
{
    Allocator allocator = {};
    uint8_t* data = nullptr;
    int64_t next_index = 0;
    int64_t capacity = 0;
};

Allocator* temp_allocator_get();
