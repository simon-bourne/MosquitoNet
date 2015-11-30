cmake_minimum_required(VERSION 2.8.7)
project(Enhedron)

if (CMAKE_CXX_COMPILER_ID STREQUAL Clang)
    # using Clang
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -Wall -Wconversion -Werror -pedantic")
elseif (CMAKE_CXX_COMPILER_ID STREQUAL GNU)
    # GCC
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -Wall -Wconversion -Werror -pedantic -fsanitize=address -fsanitize=undefined")
elseif (CMAKE_CXX_COMPILER_ID STREQUAL Intel)
    # using Intel C++
elseif (CMAKE_CXX_COMPILER_ID STREQUAL MSVC)
    # using Visual Studio C++
    add_definitions(/D_CONSOLE)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")
    set(LINK_FLAGS "${LINK_FLAGS} /SUBSYSTEM:CONSOLE")
endif()

include_directories(cpp/test/include cpp/lib/include)

add_executable(test-harness
        cpp/test/src/Test/Harness.cpp
        cpp/test/src/Test/Util.cpp
        cpp/test/src/Test/Assert.cpp
        cpp/test/src/Test/CommandLine.cpp)

add_executable(integration-test cpp/test/src/Test/Harness.cpp cpp/test/src/Test/Integration.cpp)
