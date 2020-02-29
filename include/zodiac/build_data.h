#pragma once

#include "allocator.h"
#include "atom.h"
#include "array.h"

namespace Zodiac
{
    struct Build_Data
    {
        Atom_Table atom_table = {};
        Array<Atom> kw_atoms = {};
    };

    void build_data_init(Allocator* allocator, Build_Data* build_data);

}
