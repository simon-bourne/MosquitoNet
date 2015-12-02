cmake_minimum_required(VERSION 2.8.7)
project(MosquitoNet)

include (CMakeLists.flags.txt)

include_directories(cpp/single-include)

add_executable(example cpp/test/src/Examples/Harness.cpp cpp/test/src/Examples/Introductory.cpp)
