#pragma once

#include "allocator.h"

#include <cassert>
#include <stdint.h>

namespace Zodiac
{

    template <typename Element_Type, int16_t bucket_capacity>
    struct Bucket_Array_Bucket
    {
        typedef Bucket_Array_Bucket<Element_Type, bucket_capacity> Bucket;

        Bucket *next_bucket = nullptr;
        Element_Type data[bucket_capacity];
        int16_t count = 0;
    };

    template <typename Element_Type, int16_t bucket_capacity>
    struct Bucket_Locator;

    template <typename Element_Type, int16_t bucket_capacity>
    struct Bucket_Array
    {
        typedef Bucket_Array_Bucket<Element_Type, bucket_capacity> Bucket;

        Allocator *allocator = nullptr;

        Bucket *first_bucket = nullptr;
        Bucket *last_bucket = nullptr;
    };

    template <typename Element_Type, int16_t bucket_capacity>
    struct Bucket_Locator
    {
        typedef Bucket_Array_Bucket<Element_Type, bucket_capacity> Bucket;

        Bucket *bucket = nullptr;
        int16_t index = -1;
    };


    template <typename Element_Type, int16_t bucket_capacity>
    Element_Type *bucket_locator_get_ptr(Bucket_Locator<Element_Type, bucket_capacity> locator)
    {
        assert(locator.bucket->count > locator.index);
        return &locator.bucket->data[locator.index];
    }

    template <typename Element_Type, int16_t bucket_capacity>
    void bucket_locator_advance(Bucket_Locator<Element_Type, bucket_capacity> *locator)
    {
        locator->index += 1;
        if (locator->index >= bucket_capacity) {
            assert(locator->bucket->next_bucket);
            locator->index = 0;
            locator->bucket = locator->bucket->next_bucket;
        }
    }

    template <typename Element_Type, int16_t bucket_capacity>
    Bucket_Array_Bucket<Element_Type, bucket_capacity> *
    new_bucket(Bucket_Array<Element_Type, bucket_capacity> *ba)
    {
        auto result = alloc_type<Bucket_Array_Bucket<Element_Type, bucket_capacity>>(ba->allocator);
        result->count = 0;
        result->next_bucket = nullptr;
        return result;
    }

    template <typename Element_Type, int16_t bucket_capacity>
    void add_bucket(Bucket_Array<Element_Type, bucket_capacity> *ba)
    {
        assert(ba->last_bucket->next_bucket == nullptr);
        ba->last_bucket->next_bucket = new_bucket(ba);
        ba->last_bucket = ba->last_bucket->next_bucket;
    }

    template <typename Element_Type, int16_t bucket_capacity>
    void bucket_array_init(Allocator *allocator,
                           Bucket_Array<Element_Type, bucket_capacity> *ba) {

        ba->allocator = allocator;
        ba->first_bucket = new_bucket(ba);
        ba->last_bucket = ba->first_bucket;
    }

    template <typename Element_Type, int16_t cap>
    Bucket_Locator<Element_Type, cap>
    bucket_array_locator_by_index(Bucket_Array<Element_Type, cap> *ba, int64_t index)
    {
        int64_t ci = 0;

        Bucket_Array_Bucket<Element_Type, cap> *current_bucket = ba->first_bucket;
        assert(current_bucket);

        while (ci < index) {

            if ((index - ci) < cap) {
                ci = index - ci;
                assert(false && "Untested path...!!");
                break;
            } else {
                ci += cap;
                assert(current_bucket->next_bucket);
                current_bucket = current_bucket->next_bucket;
            }
        }

        assert(current_bucket);
        assert(ci >= 0);
        assert(ci < cap);
        return { .bucket = current_bucket, .index = (int16_t)ci };
    }

    template <typename Element_Type, int16_t bucket_capacity>
    Bucket_Locator<Element_Type, bucket_capacity>
    bucket_array_add(Bucket_Array<Element_Type, bucket_capacity> *ba, Element_Type element)
    {
        if (ba->last_bucket->count >= bucket_capacity) {
            add_bucket(ba);
        }

        ba->last_bucket->data[ba->last_bucket->count] = element;
        Bucket_Locator<Element_Type, bucket_capacity> locator = {
            .bucket = ba->last_bucket,
            .index = ba->last_bucket->count,
        };
        ba->last_bucket->count += 1;

        return locator;
    }

    template <typename Element_Type, int16_t bucket_capacity>
    Element_Type *bucket_array_add_uninitialized(Bucket_Array<Element_Type, bucket_capacity> *ba)
    {
        Element_Type t;
        auto locator = bucket_array_add(ba, t);
        return &locator.bucket->data[locator.index];
    }

    template <typename Element_Type, int16_t bucket_capacity>
    Bucket_Locator<Element_Type, bucket_capacity>
    bucket_array_next_empty(Bucket_Array<Element_Type, bucket_capacity> *ba) {
        assert(ba->last_bucket);


        auto last_bucket = ba->last_bucket;
        if (last_bucket->count + 1 >= bucket_capacity) {
            add_bucket(ba);
        }

        Bucket_Locator<Element_Type, bucket_capacity> result = {
            .bucket = ba->last_bucket,
            .index = (int16_t)(ba->last_bucket->count),
        };

        return result;
    }

    template <typename Element_Type, int16_t bucket_capacity>
    Bucket_Locator<Element_Type, bucket_capacity>
    bucket_array_first(Bucket_Array<Element_Type, bucket_capacity> *ba) {
        assert(ba->first_bucket);
        assert(ba->first_bucket->count > 0);
        assert(ba->last_bucket);
        assert(ba->last_bucket->count > 0);

        Bucket_Locator<Element_Type, bucket_capacity> result = {
            .bucket = ba->first_bucket,
            .index = 0,
        };

        return result;
    }

    template <typename Element_Type, int16_t bucket_capacity>
    Bucket_Locator<Element_Type, bucket_capacity>
    bucket_array_last(Bucket_Array<Element_Type, bucket_capacity> *ba) {
        assert(ba->first_bucket);
        assert(ba->first_bucket->count > 0);
        assert(ba->last_bucket);
        assert(ba->last_bucket->count > 0);

        Bucket_Locator<Element_Type, bucket_capacity> result = {
            .bucket = ba->last_bucket,
            .index = (int16_t)(ba->last_bucket->count - 1),
        };

        return result;
    }
}
