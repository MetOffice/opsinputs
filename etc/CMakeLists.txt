# (C) Crown Copyright 2021, the Met Office. All rights reserved.
#

# Include macros for creating links and symlinks
include( opsinputs_functions )

# Add list of resource files
list( APPEND opsinputs_etc
  global/cx/AIRS.nl
  global/cx/AMSR.nl
  global/cx/ATMS.nl
  global/cx/ATOVS.nl
  global/cx/CrIS.nl
  global/cx/GMIlow.nl
  global/cx/GPSRO.nl
  global/cx/GroundGPS.nl
  global/cx/IASI.nl
  global/cx/MWRI.nl
  global/cx/SatTCWV.nl
  global/cx/Sonde.nl
  global/cx/SSMIS.nl
  global/varobs/AIRS.nl
  global/varobs/ATMS.nl
  global/varobs/ATOVS.nl
  global/varobs/CrIS.nl
  global/varobs/GPSRO.nl
  global/varobs/IASI.nl
  global/varobs/Scatwind.nl
  global/varobs/Sonde.nl
  ukv/cx/Sonde.nl
  ukv/cx/SurfaceCloud.nl
  ukv/varobs/Sonde.nl
  ukv/varobs/SurfaceCloud.nl
)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/global/cx)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/global/varobs)
CREATE_SYMLINKS( ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR} ${opsinputs_etc} )
