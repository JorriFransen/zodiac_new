

project(zodiac)

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

if(MSVC)
    target_compile_options(zodiac PRIVATE /W4 /WX)
else()
   #target_compile_options(zodiac PRIVATE -Wall -Wextra -pedantic -Werror)
   target_compile_options(zodiac PRIVATE -Wall -Wextra -Werror)
endif()

