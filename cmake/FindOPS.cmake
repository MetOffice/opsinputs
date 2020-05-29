# (C) Copyright 2020 Met Office UK
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

#[=======================================================================[.rst:
FindOPS
-------

Finds an OPS build. Note: the build directory must contain not only the ``bin`` and
``install`` subdirectories, but also the ``o`` and ``include`` subdirectories. The standard OPS
build task only extracts the former two directories, leaving ``o`` and ``include`` in the
``build.tgz`` file located next to the ``build`` directory. This file must be extracted manually
into the ``build`` directory before configuring ``cxvarobs``.

Result Variables
^^^^^^^^^^^^^^^^

This will define the following variables:

``OPS_FOUND``
  True if an OPS build directory containing all the required subdirectories is found.
``OPS_BIN_DIR``
  The directory containing OPS executables.
``OPS_INSTALLS_DIR``
  The directory containing OPS dependencies.
``OPS_INCLUDE_DIR``
  The directory containing OPS include files and Fortran modules.
``OPS_O_DIR``
  The directory containing OPS object files.

Notes
^^^^^

To guide the search process, set the OPS_ROOT environment variable or CMake variable to the path to
the OPS build directory.

#]=======================================================================]

find_path(OPS_BIN_DIR
  NAMES OpsProg_PrintVarobs.exe
  PATH_SUFFIXES bin
)

find_path(OPS_INSTALLS_DIR
  NAMES shumlib/lib/libshum_wgdos_packing.so
  PATH_SUFFIXES installs
)

find_path(OPS_INCLUDE_DIR
  NAMES opsmod_obsinfo.mod
  PATH_SUFFIXES include
)

find_path(OPS_O_DIR
  NAMES opsmod_obsinfo.o
  PATH_SUFFIXES o
)

if (OPS_ROOT_DIR AND (NOT EXISTS "${OPS_ROOT_DIR}/include" OR NOT EXISTS "${OPS_ROOT_DIR}/o"))
  message(WARNING "\
    The 'include' and/or 'o' subdirectories of the OPS build directory haven't been found.\n\
    The OPS build task stores their contents in the build.tgz file located next to the build directory. \n\
    Extract that archive into the OPS build directory and try again.")
endif()

mark_as_advanced(OPS_FOUND OPS_BIN_DIR OPS_INSTALLS_DIR OPS_INCLUDE_DIR OPS_O_DIR)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(OPS
    REQUIRED_VARS OPS_BIN_DIR OPS_INSTALLS_DIR OPS_INCLUDE_DIR OPS_O_DIR
)
