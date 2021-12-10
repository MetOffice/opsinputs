cmake_minimum_required( VERSION 3.12 FATAL_ERROR )

project( opsinputs-ci VERSION 0.1 LANGUAGES C CXX Fortran )

add_subdirectory("${CMAKE_CURRENT_SOURCE_DIR}/jedicmake")
if(NOT DEFINED jedicmake_DIR)
  set(jedicmake_DIR "${CMAKE_BINARY_DIR}/jedicmake")
endif()
add_subdirectory("${CMAKE_CURRENT_SOURCE_DIR}/oops" EXCLUDE_FROM_ALL)
add_subdirectory("${CMAKE_CURRENT_SOURCE_DIR}/ioda" EXCLUDE_FROM_ALL)
add_subdirectory("${CMAKE_CURRENT_SOURCE_DIR}/ufo" EXCLUDE_FROM_ALL)
add_subdirectory("${CMAKE_CURRENT_SOURCE_DIR}/ropp-ufo" EXCLUDE_FROM_ALL)
add_subdirectory("${CMAKE_CURRENT_SOURCE_DIR}/opsinputs")