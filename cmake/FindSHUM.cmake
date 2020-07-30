# (C) Copyright 2020 Met Office UK
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

#[=======================================================================[.rst:

FindSHUM
-------

Finds the shumlib build. Note: the build directory must contain the ``include`` and
``lib`` subdirectories.

Result Variables

^^^^^^^^^^^^^^^^

This will define the following variables:

``SHUM_FOUND``

  True if an SHUM directory containing all the required subdirectories is found.

``SHUM_INCLUDE_DIR``

  The directory containing the include files.

``SHUM_LIBRARIES``

  A list of all the libraries to be linked.


Notes
^^^^^
To guide the search process, set the SHUM_ROOT environment variable or CMake variable to the path to the SHUM build directory.

#]=======================================================================]

find_path(SHUM_INCLUDE_DIR
  NAMES f_shum_fieldsfile_class_version_mod.mod
  PATH_SUFFIXES ${SHUM_PATH}/include
)

find_library(SHUM_BYTESWAP_LIBRARY NAMES shum_byteswap)
find_library(SHUM_STRING_CONV_LIBRARY NAMES shum_string_conv)
find_library(SHUM_WGDOS_PACKING_LIBRARY NAMES shum_wgdos_packing)
find_library(SHUM_CONSTANTS_LIBRARY NAMES shum_constants)
find_library(SHUM_NUMBER_TOOLS_LIBRARY NAMES shum_number_tools)
find_library(SHUM_DATA_CONV_LIBRARY NAMES shum_data_conv)
find_library(SHUM_SPIRAL_SEARCH_LIBRARY NAMES shum_spiral_search)
find_library(SHUM_FIELDSFILE_LIBRARY NAMES shum_fieldsfile)
find_library(SHUM_FIELDSFILE_CLASS_LIBRARY NAMES shum_fieldsfile_class)
find_library(SHUM_THREAD_UTILS_LIBRARY NAMES shum_thread_utils)
find_library(SHUM_HORIZONTAL_FIELD_INTERP_LIBRARY NAMES shum_horizontal_field_interp)
find_library(SHUM_LATLON_EQ_GRIDS_LIBRARY NAMES shum_latlon_eq_grids)

set(SHUM_LIBRARIES
    ${SHUM_BYTESWAP_LIBRARY}
    ${SHUM_STRING_CONV_LIBRARY}
    ${SHUM_WGDOS_PACKING_LIBRARY}
    ${SHUM_CONSTANTS_LIBRARY}
    ${SHUM_NUMBER_TOOLS_LIBRARY}
    ${SHUM_DATA_CONV_LIBRARY}
    ${SHUM_SPIRAL_SEARCH_LIBRARY}
    ${SHUM_FIELDSFILE_LIBRARY}
    ${SHUM_FIELDSFILE_CLASS_LIBRARY}
    ${SHUM_THREAD_UTILS_LIBRARY}
    ${SHUM_HORIZONTAL_FIELD_INTERP_LIBRARY}
    ${SHUM_LATLON_EQ_GRIDS_LIBRARY})

if (SHUM_ROOT AND (NOT EXISTS "${SHUM_ROOT}/include" OR NOT EXISTS "${SHUM_ROOT}/lib"))
  message(WARNING "\
    The 'include' and/or 'lib' subdirectories of the SHUM build directory haven't been found.\n\
    Locate a build or rebuild SHUM.\n\
    Pass the location of the build via SHUM_ROOT and try again.")
endif()

mark_as_advanced(SHUM_FOUND SHUM_INCLUDE_DIR SHUM_LIBRARIES)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(SHUM
    REQUIRED_VARS SHUM_INCLUDE_DIR SHUM_LIBRARIES
)
