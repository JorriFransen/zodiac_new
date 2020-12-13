
#include "builtin.h"
#include "lexer.h"
#include "parser.h"
#include "c_allocator.h"
#include "command_line_parser.h"
#include "ast.h"
#include "scope.h"
#include "resolver.h"
#include "interpreter.h"

#define MICROSOFT_CRAZINESS_IMPLEMENTATION
#include "microsoft_craziness.h"

#include <tracy/TracyC.h>

using namespace Zodiac;

int main(int argc, char **argv)
{
    TracyCZoneN(tcz_init, "init", true);

#ifdef WIN32
    auto win_sdk_info = find_visual_studio_and_windows_sdk();
    printf("windows_sdk_version: %d\n", win_sdk_info.windows_sdk_version);
    printf("windows_sdk_root: %ls\n", win_sdk_info.windows_sdk_root);
    printf("windows_sdk_um_library_path: %ls\n", win_sdk_info.windows_sdk_um_library_path);
    printf("windows_sdk_ucrt_library_path: %ls\n", win_sdk_info.windows_sdk_ucrt_library_path);
    printf("vs_exe_path: %ls\n", win_sdk_info.vs_exe_path);
    printf("vs_library_path: %ls\n", win_sdk_info.vs_library_path);
    free_resources(&win_sdk_info);
#endif

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

    start_resolving(&resolver);
    Resolve_Result rr = finish_resolving(&resolver);
    assert(rr.error_count == 0);
    if (rr.parse_error || rr.llvm_error) return 1;

    if (options.print_scope) scope_print(resolver.global_scope);

    if (options.print_resolved_ast) {
        for (int64_t i = 0; i < resolver.parsed_modules.count; i++) {
            ast_print(resolver.parsed_modules[i].ast);
        }
    }

    if (options.print_bytecode) bytecode_print(ca, &resolver.bytecode_builder);

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
