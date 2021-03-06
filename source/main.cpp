
#include "ast.h"
#include "builtin.h"
#include "c_allocator.h"
#include "command_line_parser.h"
#include "lexer.h"
#include "parser.h"
#include "resolver.h"
#include "scope.h"

#include <tracy/TracyC.h>

// #include <syscall.h>
// #include <signal.h>

using namespace Zodiac;

int main(int argc, char **argv)
{
    // bucket_array_test();

    // printf("SYS_kill: %d\n", SYS_kill);
    // printf("SYS_tkill: %d\n", SYS_tkill);
    // printf("SYS_getpid: %d\n", SYS_getpid);
    // printf("SYS_gettid: %d\n", SYS_gettid);
    // printf("SYS_fork): %d\n", SYS_fork);
    // printf("SYS_waitpid): %d\n", SYS_wait);

    // printf("SIGABRT: %d\n", SIGABRT);

    TracyCZoneN(tcz_init, "init", true);

    auto ca = c_allocator_get();

    // The allocator is only used to allocate the full path for the zodiac exe
    auto options = parse_command_line(ca, argc, argv);

    if (!options.valid) return 1;
    Build_Data build_data = {};

    build_data_init(ca, &build_data, ca, &options);

    builtin_initialize_atoms(&build_data.atom_table);
    builtin_initialize_types(ca, &build_data);

    Resolver resolver = {};
    resolver_init(ca, &resolver, &build_data, options.file_path);

    TracyCZoneEnd(tcz_init);

    resolver_start(&resolver);
    Resolve_Result rr = resolver_finish(&resolver);
    assert(rr.error_count == 0);
    if (rr.parse_error || rr.llvm_error) return 1;

    if (options.print_scope) scope_print(resolver.global_scope);

    if (options.print_resolved_ast) {
        for (int64_t i = 0; i < resolver.parsed_modules.count; i++) {
            ast_print(resolver.parsed_modules[i].ast);
        }
    }

    if (options.print_bytecode) bc_print(ca, &resolver.bytecode_builder);

    if (options.verbose)
        printf("Total bytecode instructions: %" PRId64 "\n", build_data.bytecode_instruction_count);

    if (build_data.errors.count == 0) {

        if (!options.dont_emit_llvm)
            llvm_emit_binary(&resolver.llvm_builder, build_data.options->exe_file_name.data);

    } else {
        zodiac_report_errors(&build_data);
    }

    return 0;
}
