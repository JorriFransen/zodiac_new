#include "zodiac_error.h"

#include "build_data.h"

#include <stdio.h>
#include <inttypes.h>

namespace Zodiac
{
    Zodiac_Error zodiac_make_error(Zodiac_Error_Kind kind, const char *message,
                                   int64_t message_size, Zodiac_Error_Info info)
    {
        return { kind, message, message_size, info };
    }

    void zodiac_report_error(Build_Data *build_data, Zodiac_Error_Kind kind,
                             AST_Node *ast_node, const char *fmt, ...)
    {
        va_list args;
        va_start(args, fmt);
        Zodiac_Error_Info info = { .is_ast_node = true, .ast_node = ast_node };
        zodiac_report_error(build_data, kind, info, fmt, args);
        va_end(args);
    }

    void zodiac_report_error(Build_Data *build_data, Zodiac_Error_Kind kind,
                             File_Pos bfp, File_Pos efp, const char *fmt, ...)
    {
        va_list args;
        va_start(args, fmt);
        Zodiac_Error_Info info = { false, { { bfp, efp } } };
        zodiac_report_error(build_data, kind, info, fmt, args);
        va_end(args);

    }

    void zodiac_report_error(Build_Data *build_data, Zodiac_Error_Kind kind,
                             Zodiac_Error_Info err_info, const char *fmt, va_list args)
    {
        auto allocator = build_data->err_allocator;

        va_list args_copy;
        va_copy(args_copy, args);
        auto size = vsnprintf(nullptr, 0, fmt, args_copy);
        va_end(args_copy);
        char *buf = alloc_array<char>(allocator, size + 1); assert(buf);

        auto written_size = vsnprintf(buf, size + 1, fmt, args);
        assert(written_size <= size); 

        Zodiac_Error err = zodiac_make_error(kind, buf, written_size, err_info);
        array_append(&build_data->errors, err);
    }

    void zodiac_report_errors(Build_Data *build_data)
    {
        fprintf(stderr, "\n");
        for (int64_t i = 0; i < build_data->errors.count; i++)
        {
            auto &err = build_data->errors[i];

            File_Pos bfp = {};
            if (err.info.is_ast_node)
            {
                bfp = err.info.ast_node->begin_file_pos;
            }
            else
            {
                bfp = err.info.begin_file_pos;
            }


            fprintf(stderr, "Error: %.*s:%" PRIu64 ":%" PRIu64 ": %.*s\n",
                    (int)bfp.file_name.length, bfp.file_name.data,
                    bfp.line, bfp.column,
                    (int)err.message_size, err.message);
        }
        fprintf(stderr, "\n");
    }

    void zodiac_clear_errors(Build_Data *build_data)
    {
        for (int64_t i = 0; i < build_data->errors.count; i++)
        {
            auto &err = build_data->errors[i];
            free(build_data->err_allocator, err.message);
        }

        build_data->errors.count = 0;
    }

}
