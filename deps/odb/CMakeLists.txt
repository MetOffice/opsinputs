# (C) Crown Copyright 2021, the Met Office. All rights reserved.
#

if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
  set(ODB_EXTRA_Fortran_FLAGS "-Wno-unused-dummy-argument -Wno-unused")
elseif("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
  set(ODB_EXTRA_Fortran_FLAGS "-warn nounused")
endif()

set(CMAKE_Fortran_FLAGS "\
${OPS_DEFAULT_Fortran_FLAGS} \
${ODB_EXTRA_Fortran_FLAGS}")

set(ODB_SOURCE_FILES
  stubs/parkind1.F90
  stubs/yomhook.F90
)

if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "NAG")
  ecbuild_add_library(
    TARGET odb
    SOURCES ${ODB_SOURCE_FILES}
    LINKER_LANGUAGE CXX)
else()
  ecbuild_add_library(
    TARGET odb
    SOURCES ${ODB_SOURCE_FILES})
endif()

get_target_property(ODB_Fortran_MODULE_DIRECTORY odb Fortran_MODULE_DIRECTORY)

target_include_directories(odb INTERFACE $<BUILD_INTERFACE:${ODB_Fortran_MODULE_DIRECTORY}>)
