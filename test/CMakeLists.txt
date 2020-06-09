# (C) Copyright 2020 Met Office UK
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

# macro to create a symlink from src to dst
function(CREATE_SYMLINK src dst)
  foreach (FILENAME ${ARGN})
    execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
                     ${src}/${FILENAME}
                     ${dst}/${FILENAME} )
  endforeach(FILENAME)
endfunction()

# macro to create a symlink from src to dst with just filename
function(CREATE_SYMLINK_FILENAME src dst)
  foreach (FILENAME ${ARGN})
    get_filename_component(filename ${FILENAME} NAME )
    execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
                     ${src}/${FILENAME}
                     ${dst}/${filename} )
  endforeach(FILENAME)
endfunction()

# macro to define a VarObsWriter or a CxWriter test
function( ADD_WRITER_TEST )
  set( options )
  set( single_value_args WRITER NAME MPI )
  set( multi_value_args )
  cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}"
                         ${_FIRST_ARG} ${ARGN} )
  if(_PAR_UNPARSED_ARGUMENTS)
    ecbuild_critical("Unknown keywords given to ADD_WRITER_TEST(): \"${_PAR_UNPARSED_ARGUMENTS}\"")
  endif()

  set( environment
         OOPS_TRAPFPE=1
         LD_LIBRARY_PATH=${OPS_INSTALLS_DIR}/odb/lib:${OPS_INSTALLS_DIR}/shumlib/lib:$ENV{LD_LIBRARY_PATH}
         OPS_VAROB_OUTPUT_DIR=${CXVAROBS_OUTPUT_DIR}/test_cxvarobs_${_PAR_WRITER}_${_PAR_NAME} )

  if (_PAR_MPI)
    ecbuild_add_test( TARGET  test_cxvarobs_${_PAR_WRITER}_${_PAR_NAME}
                      COMMAND ${CMAKE_BINARY_DIR}/bin/test_CxVarObsFilters.x
                      ARGS    "testinput/${_PAR_NAME}.yaml"
                      ENVIRONMENT "${environment}"
                      MPI     ${_PAR_MPI}
                      DEPENDS test_CxVarObsFilters.x )
  else()
    ecbuild_add_test( TARGET  test_cxvarobs_${_PAR_WRITER}_${_PAR_NAME}
                      COMMAND ${CMAKE_BINARY_DIR}/bin/test_CxVarObsFilters.x
                      ARGS    "testinput/${_PAR_NAME}.yaml"
                      ENVIRONMENT "${environment}"
                      DEPENDS test_CxVarObsFilters.x )
  endif()
  set_tests_properties( test_cxvarobs_${_PAR_WRITER}_${_PAR_NAME} PROPERTIES FIXTURES_REQUIRED CleanOutputDir )
endfunction()

set( CXVAROBS_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/testoutput)

# Create a directory for test input config and symlink all files
list( APPEND cxvarobs_test_input
  testinput/000030_StashItem_modelsurface.yaml
  testinput/001_VarField_pstar.yaml
  testinput/002_VarField_temperature_RadarZ.yaml
  testinput/002_VarField_temperature_RadarZ_MPI_1.yaml
  testinput/002_VarField_temperature_RadarZ_MPI_4.yaml
  testinput/002_VarField_temperature_Surface.yaml
  testinput/003_VarField_rh_Sonde.yaml
  testinput/003_VarField_rh_Surface.yaml
  testinput/004_VarField_u_Sonde.yaml
  testinput/004_VarField_u_Surface.yaml
  testinput/005_VarField_v_Sonde.yaml
  testinput/005_VarField_v_Surface.yaml
  testinput/010_VarField_britemp.yaml
  testinput/019_VarField_satzenith.yaml
  testinput/023_VarField_modelsurface_geoval.yaml
  testinput/028_VarField_satid.yaml
  testinput/031_VarField_solzenith.yaml
  testinput/054_VarField_numchans.yaml
  testinput/055_VarField_channum.yaml
  testinput/066_VarField_radarobazim.yaml
  testinput/071_VarField_bendingangle.yaml
  testinput/072_VarField_impactparam.yaml
  testinput/073_VarField_ro_rad_curv.yaml
  testinput/074_VarField_ro_geoid_und.yaml
  testinput/077_VarField_aod.yaml
  testinput/078_VarField_theta.yaml
  testinput/empty.yaml
  testinput/FixedHeader_GridStagger.yaml
  testinput/FixedHeader_HorizGrid.yaml
  testinput/FixedHeader_Times.yaml
  testinput/FixedHeader_VertCoord.yaml
  testinput/IntegerConstants_LensAndLevels.yaml
  testinput/RealConstants.yaml
  testinput/reject_obs_with_all_variables_failing_qc.yaml
  testinput/VarObsWriterNamelists_010_VarField_britemp/AMSUB.nl
  testinput/VarObsWriterNamelists_019_VarField_satzenith/AMSUB.nl
  testinput/VarObsWriterNamelists_023_VarField_modelsurface/Surface.nl
  testinput/VarObsWriterNamelists_031_VarField_solzenith/AMSUB.nl
  testinput/VarObsWriterNamelists_054_VarField_numchans/AMSUB.nl
  testinput/VarObsWriterNamelists_055_VarField_channum/AMSUB.nl
)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput)
# TODO(wsmigaj): find a more elegant solution to create the required subdirectories of testinput
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput/VarObsWriterNamelists_010_VarField_britemp)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput/VarObsWriterNamelists_019_VarField_satzenith)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput/VarObsWriterNamelists_023_VarField_modelsurface)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput/VarObsWriterNamelists_031_VarField_solzenith)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput/VarObsWriterNamelists_054_VarField_numchans)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput/VarObsWriterNamelists_055_VarField_channum)
CREATE_SYMLINK( ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR} ${cxvarobs_test_input} )

# Add files to cmake resources
ecbuild_add_resources( TARGET cxvarobs_test_scripts
                       SOURCES_PACK
                       ${cxvarobs_test_input} )

# Create Data directory for test data and symlink files
list( APPEND cxvarobs_test_data
  001_VarField_pstar.nc4
  002_VarField_temperature_RadarZ.nc4
  002_VarField_temperature_Surface.nc4
  003_VarField_rh_Sonde.nc4
  003_VarField_rh_Surface.nc4
  004_VarField_u_Sonde.nc4
  004_VarField_u_Surface.nc4
  005_VarField_v_Sonde.nc4
  005_VarField_v_Surface.nc4
  010_VarField_britemp.nc4
  019_VarField_satzenith.nc4
  023_VarField_modelsurface_geoval.nc4
  028_VarField_satid.nc4
  031_VarField_solzenith.nc4
  066_VarField_radarobazim.nc4
  071_VarField_bendingangle.nc4
  072_VarField_impactparam.nc4
  073_VarField_ro_rad_curv.nc4
  074_VarField_ro_geoid_und.nc4
  077_VarField_aod.nc4
  078_VarField_theta.nc4
  reject_obs_with_all_variables_failing_qc.nc4
)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Data)
CREATE_SYMLINK_FILENAME( ${CMAKE_CURRENT_SOURCE_DIR}/testinput
                         ${CMAKE_CURRENT_BINARY_DIR}/Data ${cxvarobs_test_data} )

ecbuild_add_executable( TARGET  test_CxVarObsFilters.x
                        SOURCES cxvarobs/instantiateObsFilterFactory.h
                                cxvarobs/ResetFlagsToPass.cc
                                cxvarobs/ResetFlagsToPass.h
                                cxvarobs/ResetFlagsToPassParameters.h
                                cxvarobs/VarObsChecker.cc
                                cxvarobs/VarObsChecker.h
                                cxvarobs/VarObsCheckerParameters.h
                                mains/TestCxVarObsFilters.cc
                        LIBS    cxvarobs )

### Cleans up the output directory
ecbuild_add_test( TARGET  test_cxvarobs_setup
                  COMMAND ${CMAKE_COMMAND}
                  ARGS    "-E" "remove_directory" ${CXVAROBS_OUTPUT_DIR})
set_tests_properties(test_cxvarobs_setup PROPERTIES FIXTURES_SETUP CleanOutputDir)

### VarObsWriter tests

# Tests of individual varfields
ADD_WRITER_TEST(WRITER varobswriter NAME 001_VarField_pstar)
ADD_WRITER_TEST(WRITER varobswriter NAME 002_VarField_temperature_Surface)
ADD_WRITER_TEST(WRITER varobswriter NAME 002_VarField_temperature_RadarZ)
ADD_WRITER_TEST(WRITER varobswriter NAME 003_VarField_rh_Surface)
ADD_WRITER_TEST(WRITER varobswriter NAME 003_VarField_rh_Sonde)
ADD_WRITER_TEST(WRITER varobswriter NAME 004_VarField_u_Surface)
ADD_WRITER_TEST(WRITER varobswriter NAME 004_VarField_u_Sonde)
ADD_WRITER_TEST(WRITER varobswriter NAME 005_VarField_v_Surface)
ADD_WRITER_TEST(WRITER varobswriter NAME 005_VarField_v_Sonde)
ADD_WRITER_TEST(WRITER varobswriter NAME 010_VarField_britemp)
ADD_WRITER_TEST(WRITER varobswriter NAME 019_VarField_satzenith)
ADD_WRITER_TEST(WRITER varobswriter NAME 023_VarField_modelsurface_geoval)
ADD_WRITER_TEST(WRITER varobswriter NAME 028_VarField_satid)
ADD_WRITER_TEST(WRITER varobswriter NAME 031_VarField_solzenith)
ADD_WRITER_TEST(WRITER varobswriter NAME 054_VarField_numchans)
ADD_WRITER_TEST(WRITER varobswriter NAME 055_VarField_channum)
ADD_WRITER_TEST(WRITER varobswriter NAME 066_VarField_radarobazim)
ADD_WRITER_TEST(WRITER varobswriter NAME 071_VarField_bendingangle)
ADD_WRITER_TEST(WRITER varobswriter NAME 072_VarField_impactparam)
ADD_WRITER_TEST(WRITER varobswriter NAME 073_VarField_ro_rad_curv)
ADD_WRITER_TEST(WRITER varobswriter NAME 074_VarField_ro_geoid_und)
ADD_WRITER_TEST(WRITER varobswriter NAME 077_VarField_aod)
ADD_WRITER_TEST(WRITER varobswriter NAME 078_VarField_theta)

ADD_WRITER_TEST(WRITER varobswriter NAME 002_VarField_temperature_RadarZ_MPI_1 MPI 1)
ADD_WRITER_TEST(WRITER varobswriter NAME 002_VarField_temperature_RadarZ_MPI_4 MPI 4)

# Header field tests
ADD_WRITER_TEST(WRITER varobswriter NAME FixedHeader_VertCoord)
ADD_WRITER_TEST(WRITER varobswriter NAME FixedHeader_HorizGrid)
ADD_WRITER_TEST(WRITER varobswriter NAME FixedHeader_GridStagger)
ADD_WRITER_TEST(WRITER varobswriter NAME FixedHeader_Times)
ADD_WRITER_TEST(WRITER varobswriter NAME IntegerConstants_LensAndLevels)
ADD_WRITER_TEST(WRITER varobswriter NAME RealConstants)

# Other tests
ADD_WRITER_TEST(WRITER varobswriter NAME reject_obs_with_all_variables_failing_qc)

### CxWriter tests
ADD_WRITER_TEST(WRITER cxwriter NAME 000030_StashItem_modelsurface)

### Tests of other classes
ecbuild_add_test( TARGET  test_cxvarobs_mpiexceptionsynchronizer
                  SOURCES mains/TestMPIExceptionSynchronizer.cc
                  ARGS    "testinput/empty.yaml"
                  ENVIRONMENT OOPS_TRAPFPE=1
                  MPI     4
                  LIBS    cxvarobs)                
