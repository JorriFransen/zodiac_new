#pragma once

#include "atom.h"

namespace Zodiac
{
    struct Builtin
    {
        static Atom atom_exit;
        static Atom atom_naked;
    };

    void builtin_initialize_atoms(Atom_Table* at);
}
