cmake_minimum_required(VERSION 2.8.7)
project(MosquitoNet)

include (CMakeLists.flags.txt)

include_directories(cpp/single-include)

add_executable(Harness cpp/test/src/Examples/Harness.cpp)
add_executable(Introductory cpp/test/src/Examples/Harness.cpp cpp/test/src/Examples/Introductory.cpp)
add_executable(MinimalTest cpp/test/src/Examples/Harness.cpp cpp/test/src/Examples/MinimalTest.cpp)
add_executable(AllExamples cpp/test/src/Examples/Harness.cpp cpp/test/src/Examples/AllExamples.cpp)
