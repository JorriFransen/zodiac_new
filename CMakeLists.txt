cmake_minimum_required(VERSION 3.13.5)

project(zodiac)

set (ZODIAC_COMPILER_SOURCE
    source/main.cpp

    source/allocator.cpp
    source/c_allocator.cpp
    source/lexer.cpp
    source/os.cpp
    source/os_linux.cpp
    source/temp_allocator.cpp
    source/token.cpp
    source/zodiac_string.cpp
)

add_executable(zodiac source/main.cpp ${ZODIAC_COMPILER_SOURCE})
target_include_directories(zodiac PRIVATE include/zodiac)
