# (C) Crown Copyright 2020, the Met Office. All rights reserved.
#
# Refer to COPYRIGHT.txt of this distribution for details.

################################################################################
# opsinputs
################################################################################

cmake_minimum_required( VERSION 3.12 FATAL_ERROR )

project( opsinputs VERSION 0.1 LANGUAGES C CXX Fortran )

set( CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake;${CMAKE_MODULE_PATH})

set( CMAKE_DIRECTORY_LABELS "opsinputs" )

set( ECBUILD_DEFAULT_BUILD_TYPE Release )
set( ENABLE_OS_TESTS           OFF CACHE BOOL "Disable OS tests" FORCE )
set( ENABLE_LARGE_FILE_SUPPORT OFF CACHE BOOL "Disable testing of large file support" FORCE )

find_package( ecbuild 3.3.2 REQUIRED )
include( ecbuild_system NO_POLICY_SCOPE )

ecbuild_requires_macro_version( 2.7 )

################################################################################
# Project
################################################################################

ecbuild_declare_project()

ecbuild_enable_fortran( REQUIRED )
# set(CMAKE_CXX_STANDARD 11)
# set(CMAKE_CXX_STANDARD_REQUIRED ON)

set( OPSINPUTS_LINKER_LANGUAGE CXX )

################################################################################
# Dependencies
################################################################################

## Dependencies
find_package( jedicmake QUIET )  # Prefer find modules from jedi-cmake
find_package( MPI REQUIRED COMPONENTS C CXX Fortran )

# Boost
include_directories( ${Boost_INCLUDE_DIR} )

# NetCDF
find_package( NetCDF REQUIRED COMPONENTS C Fortran)

# eckit
find_package( eckit REQUIRED )
include_directories( ${ECKIT_INCLUDE_DIRS} )

# fckit
find_package( fckit REQUIRED )
include_directories( ${FCKIT_INCLUDE_DIRS} )

# ufo
find_package( ufo REQUIRED )
include_directories( ${UFO_INCLUDE_DIRS} )

# ioda
find_package( ioda REQUIRED )
include_directories( ${IODA_INCLUDE_DIRS} )

# oops
find_package( oops REQUIRED )
include_directories( ${OOPS_INCLUDE_DIRS} )

################################################################################
# Export package info
################################################################################

list( APPEND OPSINPUTS_TPLS ufo ioda oops )

set( OPSINPUTS_INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/src ${CMAKE_CURRENT_BINARY_DIR}/src
                      ${CMAKE_Fortran_MODULE_DIRECTORY} )
set( OPSINPUTS_LIBRARIES opsinputs )

get_directory_property( OPSINPUTS_DEFINITIONS COMPILE_DEFINITIONS )

foreach( _tpl ${OPSINPUTS_TPLS} )
  string( TOUPPER ${_tpl} TPL )
  list( APPEND OPSINPUTS_EXTRA_DEFINITIONS   ${${TPL}_DEFINITIONS}  ${${TPL}_TPL_DEFINITIONS}  )
  list( APPEND OPSINPUTS_EXTRA_INCLUDE_DIRS  ${${TPL}_INCLUDE_DIRS} ${${TPL}_TPL_INCLUDE_DIRS} )
  list( APPEND OPSINPUTS_EXTRA_LIBRARIES     ${${TPL}_LIBRARIES}    ${${TPL}_TPL_LIBRARIES}    )
endforeach()

################################################################################
# Sources
################################################################################

include( opsinputs_compiler_flags )

add_subdirectory( deps )
add_subdirectory( etc )
add_subdirectory( src )
add_subdirectory( test )

if(ECBUILD_INSTALL_FORTRAN_MODULES)
  install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/${CMAKE_CFG_INTDIR} DESTINATION ${INSTALL_INCLUDE_DIR} )
endif()

################################################################################
# Finalise configuration
################################################################################

# prepares a tar.gz of the sources and/or binaries
ecbuild_install_project( NAME opsinputs )

# print the summary of the configuration
ecbuild_print_summary()

ecbuild_add_test( TARGET opsinputs_coding_norms
                  TYPE SCRIPT
                  COMMAND ${oops_BINDIR}/oops_cpplint.py
                  ARGS --quiet --recursive ${CMAKE_CURRENT_SOURCE_DIR}/src ${CMAKE_CURRENT_SOURCE_DIR}/test
                  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/bin )

