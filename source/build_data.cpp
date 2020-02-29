#include "build_data.h"

#include "token.h"

namespace Zodiac
{
    void build_data_init(Allocator* allocator, Build_Data* build_data)
    {
        atom_table_init(allocator, &build_data->atom_table);

        const auto kw_token_count = sizeof(KW_Tokens) / sizeof(KW_Token);
        array_init(allocator, &build_data->kw_atoms, kw_token_count);

        for (uint64_t i = 0; i < kw_token_count; i++)
        {
            auto atom = atom_get(&build_data->atom_table, KW_Tokens[i].string);
            array_append(&build_data->kw_atoms, atom);
        }
    }
}
