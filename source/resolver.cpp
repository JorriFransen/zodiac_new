#include "resolver.h"

#include "builtin.h"
#include "os.h"
#include "temp_allocator.h"

namespace Zodiac
{
    void resolver_init(Allocator *allocator, Resolver *resolver, Build_Data *build_data,
                       String first_file_path)
    {
        resolver->allocator = allocator;
        resolver->build_data = build_data;

        resolver->lexer = lexer_create(allocator, build_data);
        resolver->parser = parser_create(allocator, build_data);
        resolver->bytecode_builder = bytecode_builder_create(allocator, build_data);

        queue_init(allocator, &resolver->parse_jobs);
        array_init(allocator, &resolver->parsed_modules);

        resolver->global_scope = scope_new(allocator, Scope_Kind::GLOBAL, nullptr);

        auto builtin_declarations = builtin_populate_scope(allocator, resolver->global_scope);
        assert(builtin_declarations.count);

        if (is_relative_path(first_file_path)) {
            first_file_path = get_absolute_path(allocator, first_file_path);
        }

        auto ta = temp_allocator_get();

        assert(string_ends_with(first_file_path, ".zdc"));
        auto first_file_name = get_file_name(ta, first_file_path);

        auto first_file_dir = get_file_dir(ta, first_file_path);
        auto entry_file_path = string_append(allocator, first_file_dir, "entry.zdc");
        assert(is_regular_file(entry_file_path));

        resolver->entry_module_path = entry_file_path;

        queue_parse_job(resolver, first_file_name, first_file_path, true);
    }

    void start_resolving(Resolver *resolver)
    {
        // Don't do anything when not running threaded
    }

    Resolve_Result finish_resolving(Resolver *resolver)
    {
        bool done = false;

        Resolve_Result result = {};

        while (!done) {

            auto parse_job_count = queue_count(&resolver->parse_jobs);
            while (parse_job_count--) {
                auto job = queue_dequeue(&resolver->parse_jobs);

                if (!try_parse_job(resolver, job)) {
                    result.parse_error = true;
                    done = true;
                    break;
                }
            }

            if (result.parse_error) {
                break;
            }

            if (queue_count(&resolver->parse_jobs) == 0) {
                done = true;
            }
        }

        return result;
    }

    void queue_parse_job(Resolver *resolver, String module_name, String module_path,
                         bool insert_entry_module/*= false*/)
    {
        auto parse_jobs = resolver->parse_jobs;

        for (int64_t i = 0; i < parse_jobs.used; i++) {
            auto index = parse_jobs.front + i;
            if (index >= parse_jobs.capacity) {
                index -= parse_jobs.capacity;
            }

            auto &it = parse_jobs.buffer[index];

            if (it.module_path == module_path) {
                assert(it.module_name == module_name);
                assert(it.insert_entry_module == insert_entry_module);
                return;
            }
        }

        for (int64_t i = 0; i < resolver->parsed_modules.count; i++) {
            if (resolver->parsed_modules[i].full_path == module_path) {
                return;
            }
        }

        Parse_Job job = { .module_name = module_name,
                          .module_path = module_path,
                          .insert_entry_module = insert_entry_module
        };

        queue_enqueue(&resolver->parse_jobs, job);
    }

    bool try_parse_job(Resolver *resolver, Parse_Job job)
    {
        Parsed_File epf = {};
        bool insert_epf = false;

        if (job.insert_entry_module) {
            insert_epf = true;

            Lexed_File lf_entry = lexer_lex_file(&resolver->lexer,resolver->entry_module_path);

            if (!lf_entry.valid) return false;

            Token_Stream *ets = lexer_new_token_stream(resolver->allocator, &lf_entry);

            epf = parser_parse_file(&resolver->parser, ets);

            if (!epf.valid) return false;

            lexer_free_lexed_file(&resolver->lexer, &lf_entry);
            ets->free();
        }

        Lexed_File lexed_file = lexer_lex_file(&resolver->lexer, job.module_path);

        if (!lexed_file.valid) return false;

        Token_Stream *token_stream = lexer_new_token_stream(resolver->allocator, &lexed_file);

        Parsed_File parsed_file = {};

        if (insert_epf) {
            parsed_file = epf;
        } else {
            parsed_file_init(&resolver->parser, &parsed_file);
        }

        parser_parse_file(&resolver->parser, token_stream);

        if (!parsed_file.valid) return false;

        if (resolver->build_data->options->print_parse_tree) parsed_file_print(&parsed_file);

        AST_Builder ast_builder = { resolver->allocator, resolver->build_data };

        AST_Module *module_ast = ast_create_from_parsed_file(&ast_builder, &parsed_file,
                                                             resolver->global_scope);
        //
        // @CLEANUP: @TODO: @FIXME: This should only check for redeclarations
        //                           (later maybe other errors generated by the
        //                            ast builder)
        if (resolver->build_data->errors.count) return false;

        lexer_free_lexed_file(&resolver->lexer, &lexed_file);
        token_stream->free();
        parser_free_parsed_file(&resolver->parser, &parsed_file);

        array_append(&resolver->parsed_modules, { job.module_path, module_ast });

        return true;
    }
}
