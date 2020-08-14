cmake_minimum_required(VERSION 3.18)

project(zodiac)

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
message("LLVM_INCLUDE_DIRS=${LLVM_INCLUDE_DIRS}")
message("LLVM_DEFINITIONS=${LLVM_DEFINITIONS}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

set (ZODIAC_COMPILER_SOURCE
    source/main.cpp

    source/ast.cpp
    source/allocator.cpp
    source/atom.cpp
    source/build_data.cpp
    source/builtin.cpp
    source/bytecode.cpp
    source/c_allocator.cpp
    source/file_pos.cpp
    source/hash_table.cpp
    source/interpreter.cpp
    source/lexer.cpp
    source/llvm_builder.cpp
    source/os.cpp
    source/os_linux.cpp
    source/parser.cpp
    source/parse_tree_node.cpp
    source/resolver.cpp
    source/temp_allocator.cpp
    source/token.cpp
    source/token_stream.cpp
    source/scope.cpp
    source/stack.cpp
    source/string_builder.cpp
    source/zodiac_string.cpp
)

add_executable(zodiac source/main.cpp ${ZODIAC_COMPILER_SOURCE})
target_include_directories(zodiac PRIVATE include/zodiac)

llvm_map_components_to_libnames(llvm_libs support core irreader)

target_link_libraries(zodiac ${llvm_libs})

if(MSVC)
    target_compile_options(zodiac PRIVATE /W4 /WX)
else()
   #target_compile_options(zodiac PRIVATE -Wall -Wextra -pedantic -Werror)
   #target_compile_options(zodiac PRIVATE -Wall -Wextra -Werror)
   target_compile_options(zodiac PRIVATE  -Wall -Werror)
endif()

