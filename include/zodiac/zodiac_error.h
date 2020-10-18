#pragma once

#include "file_pos.h"
#include "struct_predecls.h"

#include <stdint.h>
#include <cstdarg>

namespace Zodiac
{
    enum class Zodiac_Error_Kind
    {
        INVALID,
        
        // Parse errors
        UNEXPECTED_TOKEN,

        REDECLARATION,

        // Resolve errors
        UNDECLARED_IDENTIFIER,
        UNKNOWN_BUILTIN_FUNCTION,
        UNIMPLEMENTED,
        MISMATCHING_TYPES,
        ASSIGNING_TO_CONST,
        INCOMPLETE_SWITCH,
        INVALID_DEREF,

        // Iterp errors
        FOREIGN_FUNCTION_NOT_FOUND,
    };

    struct Zodiac_Error_Info
    {
        bool is_ast_node = false;

        union
        {
            struct
            {
                File_Pos begin_file_pos = {};
                File_Pos end_file_pos = {};
            };

            AST_Node *ast_node;
        };
    };

    struct Zodiac_Error
    {
        Zodiac_Error_Kind kind = Zodiac_Error_Kind::INVALID;

        const char *message = nullptr;
        int64_t message_size = -1;

        Zodiac_Error_Info info = {};
    };

    Zodiac_Error zodiac_make_error(Zodiac_Error_Kind kind, const char *message,
                                   int64_t message_size, Zodiac_Error_Info info);

    void zodiac_report_error(Build_Data *build_data, Zodiac_Error_Kind kind,
                             AST_Node *ast_node, const char *fmt, ...);
    void zodiac_report_error(Build_Data *build_data, Zodiac_Error_Kind kind,
                             File_Pos bfp, File_Pos efp, const char *fmt, ...);
    void zodiac_report_error(Build_Data *build_data, Zodiac_Error_Kind kind,
                             Zodiac_Error_Info err_info, const char *fmt, va_list args);

    void zodiac_report_errors(Build_Data *build_data);
    void zodiac_clear_errors(Build_Data *build_data);

}
