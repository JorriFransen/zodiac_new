#include "bucket_array.h"

#include "c_allocator.h"

#include <cstdio>

namespace Zodiac
{
    void bucket_array_test()
    {
        auto ca = c_allocator_get();

        Bucket_Array<int, 1> ba1 = {};
        bucket_array_init(ca, &ba1);

        for (int i = 0; i < 4; i++) {
            bucket_array_add(&ba1, i + 1);
        }

        auto l1 = bucket_array_first(&ba1);
        int i1 = 1;
        while (l1.bucket) {

            int value = l1.bucket->data[l1.index];
            printf("%d\n", value);
            assert(value == i1);

            i1 += 1;
            bucket_locator_advance(&l1);
        }

        bucket_array_free(&ba1);

        printf("\n\n");

        Bucket_Array<int, 2> ba2 = {};
        bucket_array_init(ca, &ba2);

        for (int i = 0; i < 8; i++) {
            bucket_array_add(&ba2, i + 1);
        }

        auto l2 = bucket_array_first(&ba2);
        int i2 = 1;
        while (l2.bucket) {

            int value = l2.bucket->data[l2.index];
            printf("%d\n", value);
            assert(value == i2);

            i2 += 1;
            bucket_locator_advance(&l2);
        }

        bucket_array_free(&ba2);
    }
}
