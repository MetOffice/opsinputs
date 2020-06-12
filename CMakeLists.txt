cmake_minimum_required(VERSION 3.12)
project(cxvarobs)

enable_testing()

add_executable(TestApp1 testapp1.cpp)
add_executable(TestApp2 testapp2.cpp)
add_test(NAME Test1 COMMAND TestApp1)
add_test(NAME Test2 COMMAND TestApp2)


