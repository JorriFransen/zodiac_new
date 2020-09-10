#include "command_line_parser.h"

#include "os.h"
#include "temp_allocator.h"
#include "lexer.h"

#include <stdio.h>

namespace Zodiac
{

    struct OPC
    {
        Array<String> tokens = {};
        int64_t current_index = 0;
    };

    static void tokenize_command_line(int argc, char **argv, Array<String> *tokens);
    static bool parse_command_line(Options *options, Array<String> tokens);
    static bool tokens_remaining(OPC *opc);
    static String current_token(OPC *opc);
    static void advance(OPC *opc);
    static void print_usage();

    Options parse_command_line(int argc, char **argv)
    {

        Options result = {};
        if (argc < 2)
        {
            fprintf(stderr, "zodiac: Expected FILE_PATH as first argument\n");
            result.valid = false; 
            print_usage();
            return result;
        }

        String file_path = string_ref(argv[1]);
        assert(is_regular_file(file_path));

        result.file_path = file_path;

        if (argc > 2)
        {
            auto ta = temp_allocator_get();
            Array<String> option_tokens = {};
            array_init(ta, &option_tokens, argc - 2);

            tokenize_command_line(argc - 2, argv + 2, &option_tokens); 
            result.valid = parse_command_line(&result, option_tokens);

            if (!result.valid)
            {
                print_usage();
            }

            array_free(&option_tokens);
        }
     
        return result;
    }

    static void tokenize_command_line(int argc, char **argv, Array<String> *tokens)
    {
        for (int i = 0; i < argc; i++)
        { 
            auto arg = string_ref(argv[i]);

            int64_t begin = 0;
            int64_t length = 0;    

            for (int64_t j = 0; j < arg.length; j++)
            {
                auto c = arg[j];

                if (c == '=')
                {
                    if (length)
                    {
                        String token = string_ref(arg.data + begin, length);
                        array_append(tokens, token);
                    }

                    length = 0;
                    begin = j + 1;

                    array_append(tokens, string_ref("="));
                }
                else if (c == '-' || c == '_' || c == '.' || is_alpha(c))
                {
                    length++;
                }
                else
                {
                    assert(false);
                }
            }

            if (length)
            {
                String token = string_ref(arg.data + begin, length);
                array_append(tokens, token);
            }
        }
    }

    static bool parse_command_line(Options *options, Array<String> tokens)
    {
        OPC opc = { tokens, 0 };

        while (opc.current_index < opc.tokens.count)
        {
            auto option = current_token(&opc);
            advance(&opc);

            String option_name = {};
            bool valid = true;

            if (string_starts_with(option, "-"))
            {
                if (string_starts_with(option, "--"))
                {
                    valid = false;
                }
                else
                {
                    option_name = string_ref(option.data + 1, option.length - 1);
                }
            }
            else 
            {
                valid = false;
            }

            if (!valid)
            {
                fprintf(stderr, "zodiac: Invalid option format: '%.*s'\n",
                        (int)option.length, option.data);
                return false;
            }

            const Option_Template *ot = nullptr;

            for (uint64_t ti = 0; ti < STATIC_ARRAY_LENGTH(option_templates); ti++)
            {
                auto cot = &option_templates[ti];
                if (string_equal(string_ref(cot->name), option_name))
                {
                    ot = cot;
                    break;
                }
            }

            if (!ot)
            {
                fprintf(stderr, "zodiac: Unknown option: '%.*s'\n",
                        (int)option.length, option.data);
                return false;
            }

            switch (ot->kind)
            {
                case OT_Kind_invalid: assert(false);

                case OT_Kind_bool:
                {
                    bool value = true;
                    if (tokens_remaining(&opc))
                    {
                        auto next = current_token(&opc);

                        if (string_equal(next, "="))
                        {
                            advance(&opc);
                            auto val_str = current_token(&opc);

                            if (string_equal(val_str, "true"))
                            {
                                value = true;
                            }
                            else if (string_equal(val_str, "false"))
                            {
                                value = false;
                            }
                            else
                            {
                                fprintf(stderr,
                                        "zodiac: Invalid value for boolean option: '%.*s'\n",
                                        (int)val_str.length, val_str.data);
                                return false;
                            }

                            advance(&opc);
                        }
                    }

                    bool *lval = (bool*)(((uint8_t*)options) + ot->option_offset);
                    *lval = value;
                    break;
                }

                case OT_Kind_String:
                {
                    bool valid = true;
                    if (!tokens_remaining(&opc))
                    {
                        valid = false;
                    }
                    else
                    {
                        auto eq = current_token(&opc);
                        if (!string_equal(eq, "="))
                        {
                            valid = false;
                        }
                        else advance(&opc);
                    }

                    if (!tokens_remaining(&opc))
                    {
                        valid = false;
                    }

                    if (!valid)
                    {
                        fprintf(stderr, "zodiac: Execpted '=value' after string option: '-%s'\n",
                                ot->name);
                        return false;
                    }

                    auto value = current_token(&opc);
                    advance(&opc);

                    String *lval = (String*)(((uint8_t*)options) + ot->option_offset);
                    *lval = value;

                    break;
                }
            }
        }

        return true;
    }

    static bool tokens_remaining(OPC *opc)
    {
        return (opc->current_index < opc->tokens.count);
    }

    static String current_token(OPC *opc)
    {
        assert(opc->current_index < opc->tokens.count);
        return opc->tokens[opc->current_index];
    }

    static void advance(OPC *opc)
    {
        if (opc->current_index < opc->tokens.count)
        {
            opc->current_index++; 
        }
    }

    static void print_usage()
    {
        printf("Usage: zodiac FILE_PATH [options]\n");
        printf("\noptions:\n");

        int longest_option_length = 0;
        for (uint64_t i = 0; i < STATIC_ARRAY_LENGTH(option_templates); i++)
        {
            auto len = strlen(option_templates[i].name);
            if (len > longest_option_length) longest_option_length = len;
        }

        for (uint64_t i = 0; i < STATIC_ARRAY_LENGTH(option_templates); i++)
        {
            auto &ot = option_templates[i];
            printf("-%s", ot.name);

            auto name_len = strlen(ot.name);
            auto space_count = longest_option_length + 4 - name_len;
            printf("%*s", (int)space_count, "");

            if (ot.description)
            {
                printf("%s", ot.description);
            }
            printf("\n");
        }

        printf("\n");
    }
}
