# (C) Copyright 2020 Met Office UK
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

# Syntax: CREATE_SYMLINK(src dst [rel_path1 [rel_path2 [...]]])
#
# Arguments:
# - src: Source folder
# - dst: Destination folder
# - rel_path1, rel_path2, ...: Files within the source folder, specified in terms of their
#                              their relative path with respect to that folder.
#
# Creates symbolic links in the destination folder pointing to the specified files in the source
# folder, preserving subfolder hierarchy.
function(CREATE_SYMLINKS src dst)
  file(MAKE_DIRECTORY ${dst})
  foreach (FILENAME ${ARGN})
    get_filename_component(absolute_subdir ${src}/${FILENAME} DIRECTORY)
    file(RELATIVE_PATH relative_subdir ${src} ${absolute_subdir})
    file(MAKE_DIRECTORY ${dst}/${relative_subdir})
    execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink
                    ${src}/${FILENAME}
                    ${dst}/${FILENAME})
  endforeach(FILENAME)
endfunction()

# Syntax: ADD_WRITER_TEST( NAME <name> [INPUTS <input>...] [DATA <data>...] [MPI <num_tasks>] )
#
# Defines a VarObsWriter or CxWriter test.
#
# Arguments:
# - writer: filter to test (varobswriter or cxwriter)
# - name: test name
# - input: paths to 0 or more input files (typically YAML files and/or namelist files) relative
#   to the testinput folder. Symlinks to these files will be created from the testinput folder in
#   the build folder.
# - data: paths to 0 or more data files (typically NetCDF files) relative to the testinput folder.
#   Symlinks to these files will be created from the Data folder in the build folder.
# - num_tasks: number of MPI tasks to spawn when running the test. If the MPI clause is not present,
#   the test will be run without MPI.
function( ADD_WRITER_TEST )
  set( options )
  set( single_value_args WRITER NAME MPI )
  set( multi_value_args INPUTS DATA)
  cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}"
                         ${_FIRST_ARG} ${ARGN} )
  if(_PAR_UNPARSED_ARGUMENTS)
    ecbuild_critical("Unknown keywords given to ADD_WRITER_TEST(): \"${_PAR_UNPARSED_ARGUMENTS}\"")
  endif()

  list( APPEND opsinputs_test_input ${_PAR_INPUTS} )
  set ( opsinputs_test_input ${opsinputs_test_input} PARENT_SCOPE )
  list( APPEND opsinputs_test_data ${_PAR_DATA})
  set ( opsinputs_test_data ${opsinputs_test_data} PARENT_SCOPE )

  set( environment
         OOPS_TRAPFPE=1
         LD_LIBRARY_PATH=${OPS_INSTALLS_DIR}/odb/lib:${OPS_INSTALLS_DIR}/shumlib/lib:$ENV{LD_LIBRARY_PATH}
         OPS_VAROB_OUTPUT_DIR=${OPSINPUTS_OUTPUT_DIR}/test_opsinputs_${_PAR_WRITER}_${_PAR_NAME}
         OPSINPUTS_OPS_BIN_DIR=${OPS_BIN_DIR})

  if (_PAR_MPI)
    ecbuild_add_test( TARGET  test_opsinputs_${_PAR_WRITER}_${_PAR_NAME}
                      COMMAND ${CMAKE_BINARY_DIR}/bin/test_OpsInputsFilters.x
                      ARGS    "testinput/${_PAR_NAME}.yaml"
                      ENVIRONMENT "${environment}"
                      MPI     ${_PAR_MPI}
                      DEPENDS test_OpsInputsFilters.x )
  else()
    ecbuild_add_test( TARGET  test_cxvarobs_${_PAR_WRITER}_${_PAR_NAME}
                      COMMAND ${CMAKE_BINARY_DIR}/bin/test_OpsInputsFilters.x
                      ARGS    "testinput/${_PAR_NAME}.yaml"
                      ENVIRONMENT "${environment}"
                      DEPENDS test_OpsInputsFilters.x )
  endif()
  set_tests_properties( test_cxvarobs_${_PAR_WRITER}_${_PAR_NAME} PROPERTIES FIXTURES_REQUIRED CleanOutputDir )
endfunction()

macro(ADD_VAROBSWRITER_TEST)
  ADD_WRITER_TEST(WRITER varobswriter ${ARGN})
endmacro()

macro(ADD_CXWRITER_TEST)
  ADD_WRITER_TEST(WRITER cxwriter ${ARGN})
endmacro()

set( OPSINPUTS_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/testoutput)

ecbuild_add_executable( TARGET  test_OpsInputsFilters.x
                        SOURCES opsinputs/instantiateObsFilterFactory.h
                                cxvarobs/CheckerUtils.cc
                                cxvarobs/CheckerUtils.h
                                cxvarobs/CxChecker.cc
                                cxvarobs/CxChecker.h
                                cxvarobs/CxCheckerParameters.h
                                opsinputs/ResetFlagsToPass.cc
                                opsinputs/ResetFlagsToPass.h
                                opsinputs/ResetFlagsToPassParameters.h
                                cxvarobs/TempFile.cc
                                cxvarobs/TempFile.h
                                opsinputs/VarObsChecker.cc
                                opsinputs/VarObsChecker.h
                                opsinputs/VarObsCheckerParameters.h
                                mains/TestOpsInputsFilters.cc
                        LIBS    opsinputs )

# Cleans up the output directory
ecbuild_add_test( TARGET  test_opsinputs_varobswriter_setup
                  COMMAND ${CMAKE_COMMAND}
                  ARGS    "-E" "remove_directory" ${OPSINPUTS_OUTPUT_DIR})
set_tests_properties(test_opsinputs_varobswriter_setup PROPERTIES FIXTURES_SETUP CleanOutputDir)

### VarObsWriter tests

# Tests of individual varfields
ADD_VAROBSWRITER_TEST(NAME   001_VarField_pstar
                      INPUTS 001_VarField_pstar.yaml
                      DATA   001_VarField_pstar.nc4)
ADD_VAROBSWRITER_TEST(NAME   002_VarField_temperature_Surface
                      INPUTS 002_VarField_temperature_Surface.yaml
                      DATA   002_VarField_temperature_Surface.nc4)
ADD_VAROBSWRITER_TEST(NAME   002_VarField_temperature_RadarZ
                      INPUTS 002_VarField_temperature_RadarZ.yaml
                      DATA   002_VarField_temperature_RadarZ.nc4)
ADD_VAROBSWRITER_TEST(NAME   003_VarField_rh_Surface
                      INPUTS 003_VarField_rh_Surface.yaml
                      DATA   003_VarField_rh_Surface.nc4)
ADD_VAROBSWRITER_TEST(NAME   003_VarField_rh_Sonde
                      INPUTS 003_VarField_rh_Sonde.yaml
                      DATA   003_VarField_rh_Sonde.nc4)
ADD_VAROBSWRITER_TEST(NAME   004_VarField_u_Surface
                      INPUTS 004_VarField_u_Surface.yaml
                      DATA   004_VarField_u_Surface.nc4)
ADD_VAROBSWRITER_TEST(NAME   004_VarField_u_Sonde
                      INPUTS 004_VarField_u_Sonde.yaml
                      DATA   004_VarField_u_Sonde.nc4)
ADD_VAROBSWRITER_TEST(NAME   005_VarField_v_Surface
                      INPUTS 005_VarField_v_Surface.yaml
                      DATA   005_VarField_v_Surface.nc4)
ADD_VAROBSWRITER_TEST(NAME   005_VarField_v_Sonde
                      INPUTS 005_VarField_v_Sonde.yaml
                      DATA   005_VarField_v_Sonde.nc4)
ADD_VAROBSWRITER_TEST(NAME   010_VarField_britemp
                      INPUTS 010_VarField_britemp.yaml VarObsWriterNamelists_010_VarField_britemp/AMSUB.nl
                      DATA   010_VarField_britemp.nc4)
ADD_VAROBSWRITER_TEST(NAME   019_VarField_satzenith
                      INPUTS 019_VarField_satzenith.yaml VarObsWriterNamelists_019_VarField_satzenith/AMSUB.nl
                      DATA   019_VarField_satzenith.nc4)
ADD_VAROBSWRITER_TEST(NAME   023_VarField_modelsurface_geoval
                      INPUTS 023_VarField_modelsurface_geoval.yaml VarObsWriterNamelists_023_VarField_modelsurface/Surface.nl
                      DATA   023_VarField_modelsurface_geoval.nc4)
ADD_VAROBSWRITER_TEST(NAME   028_VarField_satid
                      INPUTS 028_VarField_satid.yaml
                      DATA   028_VarField_satid.nc4)
ADD_VAROBSWRITER_TEST(NAME   031_VarField_solzenith
                      INPUTS 031_VarField_solzenith.yaml VarObsWriterNamelists_031_VarField_solzenith/AMSUB.nl
                      DATA   031_VarField_solzenith.nc4)
ADD_VAROBSWRITER_TEST(NAME   054_VarField_numchans
                      INPUTS 054_VarField_numchans.yaml VarObsWriterNamelists_054_VarField_numchans/AMSUB.nl
                      DATA   010_VarField_britemp.nc4)
ADD_VAROBSWRITER_TEST(NAME   055_VarField_channum
                      INPUTS 055_VarField_channum.yaml VarObsWriterNamelists_055_VarField_channum/AMSUB.nl
                      DATA   010_VarField_britemp.nc4)
ADD_VAROBSWRITER_TEST(NAME   066_VarField_radarobazim
                      INPUTS 066_VarField_radarobazim.yaml
                      DATA   066_VarField_radarobazim.nc4)
ADD_VAROBSWRITER_TEST(NAME   071_VarField_bendingangle
                      INPUTS 071_VarField_bendingangle.yaml
                      DATA   071_VarField_bendingangle.nc4)
ADD_VAROBSWRITER_TEST(NAME   072_VarField_impactparam
                      INPUTS 072_VarField_impactparam.yaml
                      DATA   072_VarField_impactparam.nc4)
ADD_VAROBSWRITER_TEST(NAME   073_VarField_ro_rad_curv
                      INPUTS 073_VarField_ro_rad_curv.yaml
                      DATA   073_VarField_ro_rad_curv.nc4)
ADD_VAROBSWRITER_TEST(NAME   074_VarField_ro_geoid_und
                      INPUTS 074_VarField_ro_geoid_und.yaml
                      DATA   074_VarField_ro_geoid_und.nc4)
ADD_VAROBSWRITER_TEST(NAME   077_VarField_aod
                      INPUTS 077_VarField_aod.yaml
                      DATA   077_VarField_aod.nc4)
ADD_VAROBSWRITER_TEST(NAME   078_VarField_theta
                      INPUTS 078_VarField_theta.yaml
                      DATA   078_VarField_theta.nc4)

# MPI tests
ADD_VAROBSWRITER_TEST(NAME   002_VarField_temperature_RadarZ_MPI_1
                      MPI    1
                      INPUTS 002_VarField_temperature_RadarZ_MPI_1.yaml
                      DATA   002_VarField_temperature_RadarZ.nc4)
ADD_VAROBSWRITER_TEST(NAME   002_VarField_temperature_RadarZ_MPI_4
                      MPI    4
                      INPUTS 002_VarField_temperature_RadarZ_MPI_4.yaml
                      DATA   002_VarField_temperature_RadarZ.nc4)

# Header field tests
ADD_VAROBSWRITER_TEST(NAME   FixedHeader_VertCoord
                      INPUTS FixedHeader_VertCoord.yaml
                      DATA   028_VarField_satid.nc4)
ADD_VAROBSWRITER_TEST(NAME   FixedHeader_HorizGrid
                      INPUTS FixedHeader_HorizGrid.yaml
                      DATA   028_VarField_satid.nc4)
ADD_VAROBSWRITER_TEST(NAME   FixedHeader_GridStagger
                      INPUTS FixedHeader_GridStagger.yaml
                      DATA   028_VarField_satid.nc4)
ADD_VAROBSWRITER_TEST(NAME   FixedHeader_Times
                      INPUTS FixedHeader_Times.yaml
                      DATA   028_VarField_satid.nc4)
ADD_VAROBSWRITER_TEST(NAME   IntegerConstants_LensAndLevels
                      INPUTS IntegerConstants_LensAndLevels.yaml
                      DATA   028_VarField_satid.nc4)
ADD_VAROBSWRITER_TEST(NAME   RealConstants
                      INPUTS RealConstants.yaml
                      DATA   028_VarField_satid.nc4)

# Tests of other options
ADD_VAROBSWRITER_TEST(NAME   reject_obs_with_all_variables_failing_qc
                      INPUTS reject_obs_with_all_variables_failing_qc.yaml
                      DATA   reject_obs_with_all_variables_failing_qc.nc4)

### CxWriter tests

ADD_CXWRITER_TEST(NAME   000004_StashItem_theta
                  INPUTS 000004_StashItem_theta.yaml
                  DATA   000004_StashItem_theta.nc4)

ADD_CXWRITER_TEST(NAME   000030_StashItem_modelsurface
                  INPUTS 000030_StashItem_modelsurface.yaml
                  DATA   000030_StashItem_modelsurface.nc4)

# Tests of auxiliary classes
list( APPEND opsinputs_test_input empty.yaml )
ecbuild_add_test( TARGET  test_opsinputs_mpiexceptionsynchronizer
                  SOURCES mains/TestMPIExceptionSynchronizer.cc
                  ARGS    "testinput/empty.yaml"
                  ENVIRONMENT OOPS_TRAPFPE=1
                  MPI     4
                  LIBS    opsinputs)

# Create symlinks to test inputs and data files
CREATE_SYMLINKS( ${CMAKE_CURRENT_SOURCE_DIR}/testinput ${CMAKE_CURRENT_BINARY_DIR}/testinput ${opsinputs_test_input} )
CREATE_SYMLINKS( ${CMAKE_CURRENT_SOURCE_DIR}/testinput ${CMAKE_CURRENT_BINARY_DIR}/Data      ${opsinputs_test_data}  )

# Add files to cmake resources
list( TRANSFORM opsinputs_test_input PREPEND testinput/ )
ecbuild_add_resources( TARGET opsinputs_test_scripts
                       SOURCES_PACK
                       ${opsinputs_test_input} )
