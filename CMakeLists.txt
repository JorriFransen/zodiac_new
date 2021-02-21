cmake_minimum_required(VERSION 3.13)

project(zodiac)

find_package(LLVM 11 REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
message("LLVM_INCLUDE_DIRS=${LLVM_INCLUDE_DIRS}")
message("LLVM_DEFINITIONS=${LLVM_DEFINITIONS}")

include_directories(${LLVM_INCLUDE_DIRS})

set (ZODIAC_COMPILER_SOURCE
    source/main.cpp

    source/ast.cpp
    source/allocator.cpp
    source/atom.cpp
    source/bucket_array.cpp
    source/build_data.cpp
    source/builtin.cpp
    source/bytecode.cpp
    source/c_allocator.cpp
    source/command_line_parser.cpp
    source/common.cpp
    source/const_interpreter.cpp
    source/ffi.cpp
    source/file_pos.cpp
    source/hash_table.cpp
    source/interpreter.cpp
    source/lexer.cpp
    source/llvm_builder.cpp
    source/operator.cpp
    source/os.cpp
    source/os_linux.cpp
    source/os_windows.cpp
    source/parser.cpp
    source/parse_tree_node.cpp
    source/resolver.cpp
    source/temp_allocator.cpp
    source/token.cpp
    source/token_stream.cpp
    source/scope.cpp
    source/stack.cpp
    source/string_builder.cpp
    source/struct_predecls.cpp
    source/zodiac_error.cpp
    source/zodiac_string.cpp
)


add_executable(zodiac source/main.cpp)
target_include_directories(zodiac PRIVATE include/zodiac)

llvm_map_components_to_libnames(llvm_libs support core irreader analysis x86codegen x86AsmParser)
target_compile_definitions(zodiac PRIVATE ${LLVM_DEFINITIONS})

if (${TRACY_ENABLE})
    message("-- Tracy is enabled")
    target_compile_definitions(zodiac PRIVATE TRACY_ENABLE=1)
    set(ZODIAC_COMPILER_SOURCE ${ZODIAC_COMPILER_SOURCE} ./lib/tracy/TracyClient.cpp)
endif()
target_include_directories(zodiac PRIVATE ./lib)

target_sources(zodiac PRIVATE ${ZODIAC_COMPILER_SOURCE})
target_link_libraries(zodiac ${llvm_libs})

# add_subdirectory(lib/dyncall-1.1)
target_include_directories(zodiac PRIVATE lib/dyncall-1.1/dyncall)
target_include_directories(zodiac PRIVATE lib/dyncall-1.1/dynload)
target_link_directories(zodiac PRIVATE lib/dyncall-1.1/dyncall lib/dyncall-1.1/dynload)

if(MSVC)
    target_compile_options(zodiac PRIVATE /W3 /WX)

    target_link_libraries(zodiac libdyncall_s libdynload_s)
    set(platform_libs shlwapi.lib)

    # add_definitions(-D_DEBUG)
    # add_definitions(-D_ITERATOR_DEBUG_LEVEL=2)
    set(CMAKE_MSVC_RUNTIME_LIBRARY MultiThreadedDebugDLL)
else()
   target_link_libraries(zodiac dyncall_s dynload_s)

   #target_compile_options(zodiac PRIVATE -Wall -Wextra -pedantic -Werror)
   #target_compile_options(zodiac PRIVATE -Wall -Wextra -Werror)
   target_compile_options(zodiac PRIVATE  -Wall -Werror)
endif()

target_link_libraries(zodiac ${platform_libs})
