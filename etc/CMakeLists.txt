# (C) Copyright 2021 Met Office.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

# Include macros for creating links and symlinks
include( opsinputs_functions )

# Add list of resource files
list( APPEND opsinputs_etc
  global/cx/AMSR.nl
  global/cx/ATMS.nl
  global/cx/SSMIS.nl
  global/cx/SatTCWV.nl
  global/varobs/ATMS.nl
)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/global/cx)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/global/varobs)
CREATE_SYMLINKS( ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR} ${opsinputs_etc} )
