# (C) Crown Copyright 2020, the Met Office. All rights reserved.
#
# Refer to COPYRIGHT.txt of this distribution for details.

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

# Syntax: ADD_WRITER_TEST( NAME <name> YAML <yaml> [NAMELIST <namelist>]
#                          [DATA <data>...] [MPI <num_tasks>] [DONT_SET_OPS_OUTPUT_DIR] )
#
# Defines a VarObsWriter or CxWriter test.
#
# Arguments:
# - name: Test name (will be prefixed by "test_opsvalues_").
# - yaml: path to the test's input YAML file, relative to the testinput subfolder.
# - namelist: path to the test's input namelist file, relative to the testinput subfolder. May be
#   omitted if the test does not use a namelist file.
#   Symlinks to the input YAML and namelist files will be added to the testinput subfolder of the
#   build folder.
# - data: Paths to any data files (typically NetCDF files) required by the test, relative to the
#   testinput folder. Symlinks to these files will be added to the Data subfolder of the build
#   folder.
# - num_tasks: Number of MPI tasks to spawn when running the test. If the MPI option is not present,
#   the test will be run without MPI.
# Options:
# - DONT_SET_OPS_OUTPUT_DIR_ENV_VAR: if true, OPS environment variables specifying output folders
#   won't be set, so the output folder needs to be defined in the YAML file.
function( ADD_WRITER_TEST )
  set( options DONT_SET_OPS_OUTPUT_DIR_ENV_VAR)
  set( single_value_args NAME YAML NAMELIST MPI )
  set( multi_value_args DATA)
  cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}"
                         ${_FIRST_ARG} ${ARGN} )
  if(_PAR_UNPARSED_ARGUMENTS)
    ecbuild_critical("Unknown keywords given to ADD_WRITER_TEST(): \"${_PAR_UNPARSED_ARGUMENTS}\"")
  endif()

  list( APPEND opsinputs_test_input ${_PAR_YAML} )
  list( APPEND opsinputs_test_input ${_PAR_NAMELIST} )
  set ( opsinputs_test_input ${opsinputs_test_input} PARENT_SCOPE )
  list( APPEND opsinputs_test_data ${_PAR_DATA})
  set( opsinputs_test_data ${opsinputs_test_data} PARENT_SCOPE )

  set( environment
       OOPS_TRAPFPE=1
       OPSINPUTS_OPSPROG_RUNNER=${OPSINPUTS_OPSPROG_RUNNER} )
  if ( NOT ${_PAR_DONT_SET_OPS_OUTPUT_DIR_ENV_VAR} )
    list ( APPEND environment
      OPS_VAROB_OUTPUT_DIR=${OPSINPUTS_OUTPUT_DIR}/test_opsinputs_${_PAR_NAME}
      OPS_CX_DIR_LIST=${OPSINPUTS_OUTPUT_DIR}/test_opsinputs_${_PAR_NAME} )
  endif()

  if ( _PAR_MPI )
    ecbuild_add_test( TARGET  test_opsinputs_${_PAR_NAME}
                      COMMAND ${CMAKE_BINARY_DIR}/bin/test_OpsInputsFilters.x
                      ARGS    "testinput/${_PAR_YAML}"
                      ENVIRONMENT "${environment}"
                      MPI     ${_PAR_MPI}
                      DEPENDS test_OpsInputsFilters.x )
  else()
    ecbuild_add_test( TARGET  test_opsinputs_${_PAR_NAME}
                      COMMAND ${CMAKE_BINARY_DIR}/bin/test_OpsInputsFilters.x
                      ARGS    "testinput/${_PAR_YAML}"
                      ENVIRONMENT "${environment}"
                      DEPENDS test_OpsInputsFilters.x )
  endif()
  set_tests_properties( test_opsinputs_${_PAR_NAME} PROPERTIES FIXTURES_REQUIRED CleanOutputDir )
endfunction()

set( OPSINPUTS_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/testoutput)
set( OPSINPUTS_OPSPROG_RUNNER ${CMAKE_CURRENT_BINARY_DIR}/opsprog_runner.sh)

# Shell script running OPS programs with PATH set appropriately
get_target_property( OPSPROG_BIN_DIR OpsProg_PrintVarobs RUNTIME_OUTPUT_DIRECTORY )
configure_file( opsprog_runner.sh.in ${OPSINPUTS_OPSPROG_RUNNER} @ONLY )

ecbuild_add_executable( TARGET  test_OpsInputsFilters.x
                        SOURCES opsinputs/instantiateObsFilterFactory.h
                                opsinputs/CheckerUtils.cc
                                opsinputs/CheckerUtils.h
                                opsinputs/CxChecker.cc
                                opsinputs/CxChecker.h
                                opsinputs/CxCheckerParameters.h
                                opsinputs/ResetFlagsToPass.cc
                                opsinputs/ResetFlagsToPass.h
                                opsinputs/ResetFlagsToPassParameters.h
                                opsinputs/TempFile.cc
                                opsinputs/TempFile.h
                                opsinputs/VarObsChecker.cc
                                opsinputs/VarObsChecker.h
                                opsinputs/VarObsCheckerParameters.h
                                mains/TestObsFilters.cc
                        LIBS    opsinputs )

# Cleans up the output directory
ecbuild_add_test( TARGET  test_opsinputs_varobswriter_setup
                  COMMAND ${CMAKE_COMMAND}
                  ARGS    "-E" "remove_directory" ${OPSINPUTS_OUTPUT_DIR})
set_tests_properties(test_opsinputs_varobswriter_setup PROPERTIES FIXTURES_SETUP CleanOutputDir)

### VarObsWriter tests

# Tests of individual varfields
ADD_WRITER_TEST(NAME     varobswriter_001_VarField_pstar
                YAML     001_VarField_pstar.yaml
                DATA     001_VarField_pstar.nc4)
ADD_WRITER_TEST(NAME     varobswriter_002_VarField_temperature_Surface
                YAML     002_VarField_temperature_Surface.yaml
                DATA     002_VarField_temperature_Surface.nc4)
ADD_WRITER_TEST(NAME     varobswriter_002_VarField_temperature_RadarZ
                YAML     002_VarField_temperature_RadarZ.yaml
                DATA     002_VarField_temperature_RadarZ.nc4)
ADD_WRITER_TEST(NAME     varobswriter_003_VarField_rh_Surface
                YAML     003_VarField_rh_Surface.yaml
                DATA     003_VarField_rh_Surface.nc4)
ADD_WRITER_TEST(NAME     varobswriter_003_VarField_rh_Sonde
                YAML     003_VarField_rh_Sonde.yaml
                DATA     003_VarField_rh_Sonde.nc4)
ADD_WRITER_TEST(NAME     varobswriter_004_VarField_u_Surface
                YAML     004_VarField_u_Surface.yaml
                DATA     004_VarField_u_Surface.nc4)
ADD_WRITER_TEST(NAME     varobswriter_004_VarField_u_Sonde
                YAML     004_VarField_u_Sonde.yaml
                DATA     004_VarField_u_Sonde.nc4)
ADD_WRITER_TEST(NAME     varobswriter_005_VarField_v_Surface
                YAML     005_VarField_v_Surface.yaml
                DATA     005_VarField_v_Surface.nc4)
ADD_WRITER_TEST(NAME     varobswriter_005_VarField_v_Sonde
                YAML     005_VarField_v_Sonde.yaml
                DATA     005_VarField_v_Sonde.nc4)
ADD_WRITER_TEST(NAME     varobswriter_010_VarField_britemp
                YAML     010_VarField_britemp.yaml
                NAMELIST VarObsWriterNamelists_010_VarField_britemp/AMSUB.nl
                DATA     010_VarField_britemp.nc4)
ADD_WRITER_TEST(NAME     varobswriter_011_VarField_tskin
                YAML     011_VarField_tskin.yaml
                NAMELIST VarObsWriterNamelists_011_VarField_tskin/AMSUB.nl
                DATA     011_VarField_tskin.nc4)
ADD_WRITER_TEST(NAME     varobswriter_017_VarField_mwemiss
                YAML     017_VarField_mwemiss.yaml
                NAMELIST VarObsWriterNamelists_017_VarField_mwemiss/AMSUB.nl
                DATA     017_VarField_mwemiss.nc4)
ADD_WRITER_TEST(NAME     varobswriter_019_VarField_satzenith
                YAML     019_VarField_satzenith.yaml
                NAMELIST VarObsWriterNamelists_019_VarField_satzenith/AMSUB.nl
                DATA     019_VarField_satzenith.nc4)
ADD_WRITER_TEST(NAME     varobswriter_021_VarField_surface
                YAML     021_VarField_surface.yaml
                NAMELIST VarObsWriterNamelists_021_VarField_surface/AMSUB.nl
                DATA     021_VarField_surface.nc4)
ADD_WRITER_TEST(NAME     varobswriter_023_VarField_modelsurface_geoval
                YAML     023_VarField_modelsurface_geoval.yaml
                NAMELIST VarObsWriterNamelists_023_VarField_modelsurface/Surface.nl
                DATA     023_VarField_modelsurface_geoval.nc4)
ADD_WRITER_TEST(NAME     varobswriter_028_VarField_satid
                YAML     028_VarField_satid.yaml
                NAMELIST VarObsWriterNamelists_028_VarField_satid/AMSUB.nl
                DATA     028_VarField_satid.nc4)
ADD_WRITER_TEST(NAME     varobswriter_031_VarField_solzenith
                YAML     031_VarField_solzenith.yaml
                NAMELIST VarObsWriterNamelists_031_VarField_solzenith/AMSUB.nl
                DATA     031_VarField_solzenith.nc4)
ADD_WRITER_TEST(NAME     varobswriter_054_VarField_numchans
                YAML     054_VarField_numchans.yaml
                NAMELIST VarObsWriterNamelists_054_VarField_numchans/AMSUB.nl
                DATA     010_VarField_britemp.nc4)
ADD_WRITER_TEST(NAME     varobswriter_055_VarField_channum
                YAML     055_VarField_channum.yaml
                NAMELIST VarObsWriterNamelists_055_VarField_channum/AMSUB.nl
                DATA     010_VarField_britemp.nc4)
ADD_WRITER_TEST(NAME     varobswriter_066_VarField_radarobazim
                YAML     066_VarField_radarobazim.yaml
                DATA     066_VarField_radarobazim.nc4)
ADD_WRITER_TEST(NAME     varobswriter_071_VarField_bendingangle
                YAML     071_VarField_bendingangle.yaml
                DATA     071_VarField_bendingangle.nc4)
ADD_WRITER_TEST(NAME     varobswriter_072_VarField_impactparam
                YAML     072_VarField_impactparam.yaml
                DATA     072_VarField_impactparam.nc4)
ADD_WRITER_TEST(NAME     varobswriter_073_VarField_ro_rad_curv
                YAML     073_VarField_ro_rad_curv.yaml
                DATA     073_VarField_ro_rad_curv.nc4)
ADD_WRITER_TEST(NAME     varobswriter_074_VarField_ro_geoid_und
                YAML     074_VarField_ro_geoid_und.yaml
                DATA     074_VarField_ro_geoid_und.nc4)
ADD_WRITER_TEST(NAME     varobswriter_077_VarField_aod
                YAML     077_VarField_aod.yaml
                DATA     077_VarField_aod.nc4)
ADD_WRITER_TEST(NAME     varobswriter_078_VarField_theta
                YAML     078_VarField_theta.yaml
                DATA     078_VarField_theta.nc4)
ADD_WRITER_TEST(NAME     varobswriter_080_VarField_biaspredictors
                YAML     080_VarField_biaspredictors.yaml
                NAMELIST VarObsWriterNamelists_080_VarField_biaspredictors/ATMS.nl
                DATA     080_VarField_biaspredictors.nc4)

# MPI tests
ADD_WRITER_TEST(NAME     varobswriter_002_VarField_temperature_RadarZ_MPI_1
                MPI      1
                YAML     002_VarField_temperature_RadarZ_MPI_1.yaml
                DATA     002_VarField_temperature_RadarZ.nc4)
ADD_WRITER_TEST(NAME     varobswriter_002_VarField_temperature_RadarZ_MPI_4
                MPI      4
                YAML     002_VarField_temperature_RadarZ_MPI_4.yaml
                DATA     002_VarField_temperature_RadarZ.nc4)

# Header field tests
ADD_WRITER_TEST(NAME     varobswriter_FixedHeader_VertCoord
                YAML     varobswriter_FixedHeader_VertCoord.yaml
                DATA     028_VarField_satid.nc4)
ADD_WRITER_TEST(NAME     varobswriter_FixedHeader_HorizGrid
                YAML     varobswriter_FixedHeader_HorizGrid.yaml
                DATA     028_VarField_satid.nc4)
ADD_WRITER_TEST(NAME     varobswriter_FixedHeader_GridStagger
                YAML     varobswriter_FixedHeader_GridStagger.yaml
                DATA     028_VarField_satid.nc4)
ADD_WRITER_TEST(NAME     varobswriter_FixedHeader_Times
                YAML     varobswriter_FixedHeader_Times.yaml
                DATA     028_VarField_satid.nc4)
ADD_WRITER_TEST(NAME     varobswriter_FixedHeader_SubModel
                YAML     varobswriter_FixedHeader_SubModel.yaml
                DATA     028_VarField_satid.nc4)
ADD_WRITER_TEST(NAME     varobswriter_IntegerConstants_LensAndLevels
                YAML     varobswriter_IntegerConstants_LensAndLevels.yaml
                DATA     028_VarField_satid.nc4)
ADD_WRITER_TEST(NAME     varobswriter_RealConstants
                YAML     varobswriter_RealConstants.yaml
                DATA     028_VarField_satid.nc4)

# Tests of other options
ADD_WRITER_TEST(NAME     varobswriter_reject_obs_with_all_variables_failing_qc
                YAML     varobswriter_reject_obs_with_all_variables_failing_qc.yaml
                DATA     reject_obs_with_all_variables_failing_qc.nc4)
ADD_WRITER_TEST(NAME     varobswriter_nested_output_directory
                YAML     varobswriter_nested_output_directory.yaml
                DATA     028_VarField_satid.nc4
                DONT_SET_OPS_OUTPUT_DIR_ENV_VAR)

### CxWriter tests

# Tests of individual surface varfields
ADD_WRITER_TEST(NAME     cxwriter_001_SurfaceCxField_Orog
                YAML     001_SurfaceCxField_Orog.yaml
                NAMELIST CxWriterNamelists_001_SurfaceCxField_Orog/AMSUB.nl
                DATA     001_SurfaceCxField_Orog.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_002_SurfaceCxField_pstar
                YAML     002_SurfaceCxField_pstar.yaml
                NAMELIST CxWriterNamelists_002_SurfaceCxField_pstar/HLOSwind.nl
                DATA     002_SurfaceCxField_pstar.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_003_SurfaceCxField_t2
                YAML     003_SurfaceCxField_t2.yaml
                NAMELIST CxWriterNamelists_003_SurfaceCxField_t2/AMSUB.nl
                DATA     003_SurfaceCxField_t2.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_004_SurfaceCxField_rh2
                YAML     004_SurfaceCxField_rh2.yaml
                NAMELIST CxWriterNamelists_004_SurfaceCxField_rh2/AMSUB.nl
                DATA     004_SurfaceCxField_rh2.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_005_SurfaceCxField_u10
                YAML     005_SurfaceCxField_u10.yaml
                NAMELIST CxWriterNamelists_005_SurfaceCxField_u10/AMSUB.nl
                DATA     005_SurfaceCxField_u10.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_006_SurfaceCxField_v10
                YAML     006_SurfaceCxField_v10.yaml
                NAMELIST CxWriterNamelists_006_SurfaceCxField_v10/AMSUB.nl
                DATA     006_SurfaceCxField_v10.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_013_SurfaceCxField_TskinSea
                YAML     013_SurfaceCxField_TskinSea.yaml
                NAMELIST CxWriterNamelists_013_SurfaceCxField_TskinSea/AMSUB.nl
                DATA     013_SurfaceCxField_TskinSea.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_016_SurfaceCxField_pmsl
                YAML     016_SurfaceCxField_pmsl.yaml
                NAMELIST CxWriterNamelists_016_SurfaceCxField_pmsl/AMSUB.nl
                DATA     016_SurfaceCxField_pmsl.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_017_SurfaceCxField_SeaIce
                YAML     017_SurfaceCxField_SeaIce.yaml
                NAMELIST CxWriterNamelists_017_SurfaceCxField_SeaIce/AMSUB.nl
                DATA     017_SurfaceCxField_SeaIce.nc4 dummy.nc4)

# Tests of individual upper-air varfields
ADD_WRITER_TEST(NAME     cxwriter_001_UpperAirCxField_theta
                YAML     001_UpperAirCxField_theta.yaml
                NAMELIST CxWriterNamelists_001_UpperAirCxField_theta/AMSUB.nl
                DATA     001_UpperAirCxField_theta.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_003_UpperAirCxField_u
                YAML     003_UpperAirCxField_u.yaml
                NAMELIST CxWriterNamelists_003_UpperAirCxField_u/HLOSwind.nl
                DATA     003_UpperAirCxField_u.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_004_UpperAirCxField_v
                YAML     004_UpperAirCxField_v.yaml
                NAMELIST CxWriterNamelists_004_UpperAirCxField_v/HLOSwind.nl
                DATA     004_UpperAirCxField_v.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_005_UpperAirCxField_q
                YAML     005_UpperAirCxField_q.yaml
                NAMELIST CxWriterNamelists_005_UpperAirCxField_q/AMSUB.nl
                DATA     005_UpperAirCxField_q.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_011_UpperAirCxField_P
                YAML     011_UpperAirCxField_P.yaml
                NAMELIST CxWriterNamelists_011_UpperAirCxField_P/AMSUB.nl
                DATA     011_UpperAirCxField_P.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_029_UpperAirCxField_qcf
                YAML     029_UpperAirCxField_qcf.yaml
                NAMELIST CxWriterNamelists_029_UpperAirCxField_qcf/AMSUB.nl
                DATA     029_UpperAirCxField_qcf.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_030_UpperAirCxField_qcl
                YAML     030_UpperAirCxField_qcl.yaml
                NAMELIST CxWriterNamelists_030_UpperAirCxField_qcl/AMSUB.nl
                DATA     030_UpperAirCxField_qcl.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_033_UpperAirCxField_p_bar
                YAML     033_UpperAirCxField_p_bar.yaml
                NAMELIST CxWriterNamelists_033_UpperAirCxField_p_bar/AMSUB.nl
                DATA     033_UpperAirCxField_p_bar.nc4 dummy.nc4)

ADD_WRITER_TEST(NAME     cxwriter_041-042_UpperAirCxField_dust1-dust2
                YAML     041-042_UpperAirCxField_dust1-dust2.yaml
                NAMELIST CxWriterNamelists_041-042_UpperAirCxField_dust1-dust2/AOD.nl
                DATA     041-046_UpperAirCxField_dust1-dust6.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_041-046_UpperAirCxField_dust1-dust6
                YAML     041-046_UpperAirCxField_dust1-dust6.yaml
                NAMELIST CxWriterNamelists_041-046_UpperAirCxField_dust1-dust6/AOD.nl
                DATA     041-046_UpperAirCxField_dust1-dust6.nc4 dummy.nc4)

# Header field tests
ADD_WRITER_TEST(NAME     cxwriter_FixedHeader_VertCoord
                YAML     cxwriter_FixedHeader_VertCoord.yaml
                NAMELIST CxWriterNamelists_001_UpperAirCxField_theta/AMSUB.nl
                DATA     001_UpperAirCxField_theta.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_FixedHeader_HorizGrid
                YAML     cxwriter_FixedHeader_HorizGrid.yaml
                NAMELIST CxWriterNamelists_001_UpperAirCxField_theta/AMSUB.nl
                DATA     001_UpperAirCxField_theta.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_FixedHeader_GridStagger
                YAML     cxwriter_FixedHeader_GridStagger.yaml
                NAMELIST CxWriterNamelists_001_UpperAirCxField_theta/AMSUB.nl
                DATA     001_UpperAirCxField_theta.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_FixedHeader_ObsFileType
                YAML     cxwriter_FixedHeader_ObsFileType.yaml
                NAMELIST CxWriterNamelists_001_UpperAirCxField_theta/AMSUB.nl
                DATA     001_UpperAirCxField_theta.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_FixedHeader_Times
                YAML     cxwriter_FixedHeader_Times.yaml
                NAMELIST CxWriterNamelists_001_UpperAirCxField_theta/AMSUB.nl
                DATA     001_UpperAirCxField_theta.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_FixedHeader_SubModel
                YAML     cxwriter_FixedHeader_SubModel.yaml
                NAMELIST CxWriterNamelists_001_UpperAirCxField_theta/AMSUB.nl
                DATA     001_UpperAirCxField_theta.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_IntegerConstants_PLevels
                YAML     cxwriter_IntegerConstants_PLevels.yaml
                NAMELIST CxWriterNamelists_001_UpperAirCxField_theta/AMSUB.nl
                DATA     001_UpperAirCxField_theta.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_RealConstants
                YAML     cxwriter_RealConstants.yaml
                NAMELIST CxWriterNamelists_001_UpperAirCxField_theta/AMSUB.nl
                DATA     001_UpperAirCxField_theta.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_Lookup_Times
                YAML     cxwriter_Lookup_Times.yaml
                NAMELIST CxWriterNamelists_001_UpperAirCxField_theta/AMSUB.nl
                DATA     001_UpperAirCxField_theta.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_Lookup_Numbers
                YAML     cxwriter_Lookup_Numbers.yaml
                NAMELIST CxWriterNamelists_001_UpperAirCxField_theta/AMSUB.nl
                DATA     001_UpperAirCxField_theta.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_LevelDependentConstants
                YAML     cxwriter_LevelDependentConstants.yaml
                NAMELIST CxWriterNamelists_001_UpperAirCxField_theta/AMSUB.nl
                DATA     001_UpperAirCxField_theta.nc4 dummy.nc4)

# MPI tests
ADD_WRITER_TEST(NAME     cxwriter_001_UpperAirCxField_theta_MPI_1
                MPI      1
                YAML     001_UpperAirCxField_theta_MPI_1.yaml
                NAMELIST CxWriterNamelists_001_UpperAirCxField_theta/AMSUB.nl
                DATA     001_UpperAirCxField_theta.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_001_UpperAirCxField_theta_MPI_4
                MPI      4
                YAML     001_UpperAirCxField_theta_MPI_4.yaml
                NAMELIST CxWriterNamelists_001_UpperAirCxField_theta/AMSUB.nl
                DATA     001_UpperAirCxField_theta.nc4 dummy.nc4)

# Tests of other options
ADD_WRITER_TEST(NAME     cxwriter_reject_obs_with_all_variables_failing_qc
                YAML     cxwriter_reject_obs_with_all_variables_failing_qc.yaml
                NAMELIST CxWriterNamelists_001_UpperAirCxField_theta/AMSUB.nl
                DATA     001_UpperAirCxField_theta.nc4 reject_obs_with_all_variables_failing_qc.nc4)
ADD_WRITER_TEST(NAME     cxwriter_nested_output_directory
                YAML     cxwriter_nested_output_directory.yaml
                DATA     001_UpperAirCxField_theta.nc4 dummy.nc4
                NAMELIST CxWriterNamelists_001_UpperAirCxField_theta/AMSUB.nl
                DONT_SET_OPS_OUTPUT_DIR_ENV_VAR)

### Tests of auxiliary classes

list( APPEND opsinputs_test_input empty.yaml )
ecbuild_add_test( TARGET  test_opsinputs_mpiexceptionsynchronizer
                  SOURCES mains/TestMPIExceptionSynchronizer.cc
                  ARGS    "testinput/empty.yaml"
                  ENVIRONMENT OOPS_TRAPFPE=1
                  MPI     4
                  LIBS    opsinputs)

### Final things to do

# Create symlinks to test inputs and data files
CREATE_SYMLINKS( ${CMAKE_CURRENT_SOURCE_DIR}/testinput ${CMAKE_CURRENT_BINARY_DIR}/testinput ${opsinputs_test_input} )
CREATE_SYMLINKS( ${CMAKE_CURRENT_SOURCE_DIR}/testinput ${CMAKE_CURRENT_BINARY_DIR}/Data      ${opsinputs_test_data}  )

# Add files to cmake resources
list( TRANSFORM opsinputs_test_input PREPEND testinput/ )
ecbuild_add_resources( TARGET opsinputs_test_scripts
                       SOURCES_PACK
                       ${opsinputs_test_input} )
