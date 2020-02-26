#pragma once

#include "atom.h"

namespace Zodiac
{

struct Parse_Tree_Node
{
    
};

struct Identifier_Parse_Tree_Node : public Parse_Tree_Node
{
    
};

Identifier_Parse_Tree_Node* new_identifier_parse_tree_node(Allocator* allocator, const Atom& atom);

}
