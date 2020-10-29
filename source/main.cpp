
#include "builtin.h"
#include "lexer.h"
#include "parser.h"
#include "c_allocator.h"
#include "command_line_parser.h"
#include "ast.h"
#include "scope.h"
#include "resolver.h"
#include "interpreter.h"

#include <stdio.h>

#include <tracy/TracyC.h>

using namespace Zodiac;

int main(int argc, char **argv)
{
    TracyCZoneN(tcz_init, "init", true);

    auto options = parse_command_line(argc, argv);

    if (!options.valid) return 1;

    auto ca = c_allocator_get();

    Build_Data build_data = {};
    build_data_init(ca, &build_data, ca, &options);
    builtin_initialize_atoms(&build_data.atom_table);
    builtin_initialize_types(ca, &build_data);

    Resolver resolver = {};
    resolver_init(ca, &resolver, &build_data, options.file_path);

    TracyCZoneEnd(tcz_init);

    start_resolving(&resolver, true);
    Resolve_Result rr = finish_resolving(&resolver);
    assert(rr.error_count == 0);
    if (rr.parse_error || rr.llvm_error) return 1;

    if (options.print_scope) scope_print(resolver.global_scope);
    if (options.print_resolved_ast)
    {
        for (int64_t i = 0; i < resolver.parsed_modules.count; i++)
        {
            ast_print(resolver.parsed_modules[i].ast);
        }
    }

    if (options.print_bytecode) bytecode_print(ca, &resolver.bytecode_builder);
    // assert(resolver.bytecode_builder.program.entry_function);

    if (options.run_bytecode)
    {
        assert(false);
        // Interpreter interp;
        // interpreter_init(ca, &interp, &build_data);

        // interpreter_execute_program(&interp, &resolver.bytecode_builder.program);
        // printf("Interpreter exited with code: %" PRId64 "\n",
        //        interp.exit_code_value.value.integer.s64);

        // interpreter_free(&interp);
    }

    // if (options.print_llvm) llvm_print(ca, &resolver.llvm_builder);

    if (build_data.errors.count != 0)
    {
        zodiac_report_errors(&build_data);
    }

    return 0;
}
