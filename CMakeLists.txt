cmake_minimum_required(VERSION 2.8.7)
project(Enhedron_CppUtil)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -Wall -Wconversion -Werror -pedantic")

include_directories(test/include external/CppUtil external/CppLog external/CppAssert)
set(Boost_USE_STATIC_LIBS   ON)
find_package(Boost COMPONENTS program_options REQUIRED)

add_executable(test-harness
        test/src/TestHarness.cpp
        test/src/TestUtil.cpp
        test/src/TestContainer.cpp
        test/src/TestAssert.cpp)

target_link_libraries(test-harness ${Boost_LIBRARIES})
