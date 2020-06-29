#include "builtin.h"

namespace Zodiac
{

Atom Builtin::atom_exit = {};
Atom Builtin::atom_naked = {};

void builtin_initialize_atoms(Atom_Table* at)
{
    Builtin::atom_exit = atom_get(at, "exit");
    Builtin::atom_naked = atom_get(at, "naked");
}

}
