#include "zodiac_error.h"

#include "build_data.h"

#include <stdio.h>
#include <inttypes.h>

namespace Zodiac
{
    Zodiac_Error zodiac_make_error(Build_Data *build_data, Zodiac_Error_Kind kind,
                                   String message, Zodiac_Error_Site site)
    {
        Zodiac_Error result = { kind, site, {} };
        array_init(build_data->err_allocator, &result.messages, 1);
        array_append(&result.messages, { message, site });
        return result; 
    }

    void zodiac_report_error(Build_Data *build_data, Zodiac_Error_Kind kind,
                             AST_Node *ast_node, const char *fmt, ...)
    {
        va_list args;
        va_start(args, fmt);
        Zodiac_Error_Site site = { .is_ast_node = true, .ast_node = ast_node };
        zodiac_report_error(build_data, kind, site, fmt, args);
        va_end(args);
    }

    void zodiac_report_error(Build_Data *build_data, Zodiac_Error_Kind kind,
                             File_Pos bfp, File_Pos efp, const char *fmt, ...)
    {
        va_list args;
        va_start(args, fmt);
        Zodiac_Error_Site site = { false, { { bfp, efp } } };
        zodiac_report_error(build_data, kind, site, fmt, args);
        va_end(args);

    }

    void zodiac_report_error(Build_Data *build_data, Zodiac_Error_Kind kind,
                             Zodiac_Error_Site site, const char *fmt, va_list args)
    {
        bool duplicate = false;

        for (int64_t i = 0; i < build_data->errors.count; i++)
        {
            auto &ex_err = build_data->errors[i];

            if (ex_err.kind != kind) continue;

            if (ex_err.site.is_ast_node == true && site.is_ast_node == true)
            {
                if (ex_err.site.ast_node == site.ast_node)
                {
                    duplicate = true;
                    break;
                }
            }
            else if (ex_err.site.is_ast_node == false && site.is_ast_node == false)
            {
                if (ex_err.site.range.begin == site.range.begin &&
                    ex_err.site.range.end == site.range.end)
                {
                    duplicate = true;
                    break;
                }
            }
        }

        if (!duplicate)
        {
            if (kind == Zodiac_Error_Kind::REDECLARATION)
            {
                build_data->redeclaration_error = true;
            }

            String message = string_print_format(build_data->err_allocator, fmt, args);
            Zodiac_Error err = zodiac_make_error(build_data, kind, message, site);
            array_append(&build_data->errors, err);
        }
    }

    void zodiac_report_info(Build_Data *build_data, AST_Node *ast_node,
                            const char *fmt, ...)
    {
        va_list args;
        va_start(args, fmt);
        Zodiac_Error_Site site = { .is_ast_node = true, .ast_node = ast_node };
        zodiac_report_info(build_data, site, fmt, args);
        va_end(args);
    }

    void zodiac_report_info(Build_Data *build_data, File_Pos bfp, File_Pos efp,
                            const char *fmt, ...)
    {
        va_list args;
        va_start(args, fmt);
        Zodiac_Error_Site site = { .is_ast_node = false, .range = { .begin = bfp, .end = efp } };
        zodiac_report_info(build_data, site, fmt, args);
        va_end(args);

    }

    void zodiac_report_info(Build_Data *build_data, Zodiac_Error_Site site,
                            const char *fmt, va_list args)
    {
        assert(build_data->errors.count);

        auto &last_err = array_last(build_data->errors);

        String message = string_print_format(build_data->err_allocator, fmt, args);
        array_append(&last_err.messages, { message, site });
    }

    void zodiac_report_errors(Build_Data *build_data)
    {
        if (!build_data->errors.count) return;

        fprintf(stderr, "\n");
        for (int64_t i = 0; i < build_data->errors.count; i++)
        {
            auto &err = build_data->errors[i];

            for (int64_t m_i = 0; m_i < err.messages.count; m_i++)
            {
                    auto &msg = err.messages[m_i];

                File_Pos bfp = {};
                if (msg.site.is_ast_node)
                {
                    bfp = msg.site.ast_node->begin_file_pos;
                }
                else
                {
                    bfp = msg.site.range.begin;
                }


                if (m_i == 0)
                {
                    fprintf(stderr, "Error: ");
                }
                else 
                {
                    fprintf(stderr, "       ");
                }

                fprintf(stderr, "%.*s:%" PRIu64 ":%" PRIu64 ": %.*s\n",
                        (int)bfp.file_name.length, bfp.file_name.data,
                        bfp.line, bfp.column,
                        (int)msg.message.length, msg.message.data);
            }
        }
        fprintf(stderr, "\n");
    }

    void zodiac_clear_errors(Build_Data *build_data)
    {

        for (int64_t i = build_data->errors.count - 1; i >= 0; i--)
        {
            auto &err = build_data->errors[i];

            // if (err.kind != Zodiac_Error_Kind::STATIC_ASSERTION_FAILED)
            // {
                for (int64_t m_i = 0; m_i < err.messages.count; m_i++)
                {
                    auto &msg = err.messages[m_i];

                    free(build_data->err_allocator, msg.message.data);
                }

                // array_unordered_remove(&build_data->errors, i);
            // }
        }

        build_data->errors.count = 0;
    }

}
