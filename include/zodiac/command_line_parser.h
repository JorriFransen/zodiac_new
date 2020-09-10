#pragma once

#include "array.h"
#include "zodiac_string.h"

namespace Zodiac
{

#define EMPTY_STRING ( (String){ nullptr, 0 } )

#define CMD_OPTION_LIST \
    DEFINE_OPTION(bool, verbose, false)                \
    DEFINE_OPTION(bool, run_bytecode, false)           \
    DEFINE_OPTION(bool, print_link_command, false)     \
    DEFINE_OPTION(bool, print_llvm, false)             \
    DEFINE_OPTION(String, exe_file_name, EMPTY_STRING) 


enum Option_Template_Kind
{
    OT_Kind_invalid,
    OT_Kind_bool,
    OT_Kind_String,
};

union Default_Option_Value
{
    bool _bool;
    String _String;
};

struct Option_Template
{
    Option_Template_Kind kind = OT_Kind_invalid;
    const char *name = nullptr;
    
    Default_Option_Value default_value;
    uint64_t option_offset = 0;
};

struct Options
{
    bool valid = true;
    String file_path = {};

#define DEFINE_OPTION(type, name, default_value) type name = (default_value);
    CMD_OPTION_LIST
#undef DEFINE_OPTION
};


static const Option_Template option_templates[] = 
{

#define OPTION_VALUE_bool(v) { ._bool = (v) }
#define OPTION_VALUE_String(v) { ._String = (v) }

#define DEFINE_OPTION(type, name, default_value) \
    { OT_Kind_##type, #name, OPTION_VALUE_##type(default_value), offsetof(Options, name) },

    CMD_OPTION_LIST

#undef DEFINE_OPTION
#undef OPTION_VALUE_bool
#undef OPTION_VALUE_String

};

Options parse_command_line(int argc, char **argv);

#undef EMPTY_STRING
}

