#include "builtin.h"

namespace Zodiac
{

Atom Builtin::atom_naked = {};

void builtin_initialize_atoms(Atom_Table* at)
{
    Builtin::atom_naked = atom_get(at, "naked");
}

}
