cmake_minimum_required(VERSION 2.8.7)
project(MosquitoNet)

if (CMAKE_CXX_COMPILER_ID STREQUAL Clang)
    # using Clang
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -Wall -Werror -pedantic")
elseif (CMAKE_CXX_COMPILER_ID STREQUAL GNU)
    # GCC
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -Wall -Werror -pedantic -fsanitize=address -fsanitize=undefined")
elseif (CMAKE_CXX_COMPILER_ID STREQUAL Intel)
    # using Intel C++
elseif (CMAKE_CXX_COMPILER_ID STREQUAL MSVC)
    # using Visual Studio C++
    add_definitions(/D_CONSOLE)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")
    set(LINK_FLAGS "${LINK_FLAGS} /SUBSYSTEM:CONSOLE")
endif()

include_directories(cpp/single-include)

add_executable(example cpp/test/src/SingleIncludeHarness.cpp cpp/test/src/Example.cpp)
