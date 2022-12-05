# (C) Crown Copyright 2020, the Met Office. All rights reserved.
#
# Refer to COPYRIGHT.txt of this distribution for details.

# Include macros for creating links and symlinks
include( opsinputs_functions )

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

  if (NOT _PAR_MPI)
     set(_PAR_MPI 1)
  endif()
  ecbuild_add_test( TARGET  test_opsinputs_${_PAR_NAME}
                    COMMAND ${CMAKE_BINARY_DIR}/bin/test_OpsInputsFilters.x
                    ARGS    "testinput/${_PAR_YAML}"
                    ENVIRONMENT "${environment}"
                    MPI     ${_PAR_MPI}
                    DEPENDS test_OpsInputsFilters.x )
  set_tests_properties( test_opsinputs_${_PAR_NAME} PROPERTIES FIXTURES_REQUIRED CleanOutputDir )
endfunction()

set( OPSINPUTS_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/testoutput)
set( OPSINPUTS_OPSPROG_RUNNER ${CMAKE_CURRENT_BINARY_DIR}/opsprog_runner.sh)

# Shell script running OPS programs with PATH set appropriately
get_target_property( OPSPROG_BIN_DIR OpsProg_PrintVarobs RUNTIME_OUTPUT_DIRECTORY )
configure_file( opsprog_runner.sh.in ${OPSINPUTS_OPSPROG_RUNNER} @ONLY )

ecbuild_add_executable( TARGET  test_OpsInputsFilters.x
                        SOURCES opsinputs/instantiateObsFilterFactory.cc
                                opsinputs/instantiateObsFilterFactory.h
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
if (ufo_SOURCE_DIR)
  target_include_directories(test_OpsInputsFilters.x PRIVATE
    ${ufo_SOURCE_DIR}
  )
else()
  target_include_directories(test_OpsInputsFilters.x PRIVATE
    ${CMAKE_INSTALL_PREFIX}/include/ufo
  )
endif()

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
ADD_WRITER_TEST(NAME     varobswriter_007_VarField_tcwv
                YAML     007_VarField_tcwv.yaml
                DATA     007_VarField_tcwv.nc4)
ADD_WRITER_TEST(NAME     varobswriter_010_VarField_britemp
                YAML     010_VarField_britemp.yaml
                NAMELIST VarObsWriterNamelists_010_VarField_britemp/AMSUB.nl
                DATA     010_VarField_britemp.nc4)
ADD_WRITER_TEST(NAME     varobswriter_011_VarField_tskin
                YAML     011_VarField_tskin.yaml
                NAMELIST VarObsWriterNamelists_011_VarField_tskin/AMSUB.nl
                DATA     011_VarField_tskin.nc4)
ADD_WRITER_TEST(NAME     varobswriter_012_VarField_gpstzdelay
                YAML     012_VarField_gpstzdelay.yaml
                DATA     012_VarField_gpstzdelay.nc4)
ADD_WRITER_TEST(NAME     varobswriter_017_VarField_mwemiss
                YAML     017_VarField_mwemiss.yaml
                NAMELIST VarObsWriterNamelists_017_VarField_mwemiss/AMSUB.nl
                DATA     017_VarField_mwemiss.nc4)
ADD_WRITER_TEST(NAME     varobswriter_018_VarField_tcozone
                YAML     018_VarField_tcozone.yaml
                NAMELIST VarObsWriterNamelists_018_VarField_tcozone/IASI.nl
                DATA     018_VarField_tcozone.nc4)
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
ADD_WRITER_TEST(NAME     varobswriter_034_VarField_iremiss
                YAML     034_VarField_iremiss.yaml
                NAMELIST VarObsWriterNamelists_034_VarField_iremiss/AMSUB.nl
                DATA     034_VarField_iremiss.nc4)
ADD_WRITER_TEST(NAME     varobswriter_035_VarField_cloudtopp
                YAML     035_VarField_cloudtopp.yaml
                NAMELIST VarObsWriterNamelists_035_VarField_cloudtopp/IASI.nl
                DATA     035_VarField_cloudtopp.nc4)
ADD_WRITER_TEST(NAME     varobswriter_036_VarField_cloudfrac
                YAML     036_VarField_cloudfrac.yaml
                NAMELIST VarObsWriterNamelists_036_VarField_cloudfrac/IASI.nl
                DATA     036_VarField_cloudfrac.nc4)
ADD_WRITER_TEST(NAME     varobswriter_051_VarField_u10ambwind
                YAML     051_VarField_u10ambwind.yaml
                DATA     051_VarField_u10ambwind.nc4)
ADD_WRITER_TEST(NAME     varobswriter_052_VarField_v10ambwind
                YAML     052_VarField_v10ambwind.yaml
                DATA     052_VarField_v10ambwind.nc4)
ADD_WRITER_TEST(NAME     varobswriter_053_VarField_awpriorpcorrect
                YAML     053_VarField_awpriorpcorrect.yaml
                DATA     053_VarField_awpriorpcorrect.nc4)
ADD_WRITER_TEST(NAME     varobswriter_054_VarField_numchans
                YAML     054_VarField_numchans.yaml
                NAMELIST VarObsWriterNamelists_054_VarField_numchans/AMSUB.nl
                DATA     010_VarField_britemp.nc4)
ADD_WRITER_TEST(NAME     varobswriter_055_VarField_channum
                YAML     055_VarField_channum.yaml
                NAMELIST VarObsWriterNamelists_055_VarField_channum/AMSUB.nl
                DATA     010_VarField_britemp.nc4)
ADD_WRITER_TEST(NAME     varobswriter_057_VarField_emissivity
                YAML     057_VarField_emissivity.yaml
                NAMELIST VarObsWriterNamelists_057_VarField_emissivity/IASI.nl
                DATA     057_VarField_emissivity.nc4)
ADD_WRITER_TEST(NAME     varobswriter_066_VarField_radarobazim
                YAML     066_VarField_radarobazim.yaml
                DATA     066_VarField_radarobazim.nc4)
ADD_WRITER_TEST(NAME     varobswriter_067_VarField_GPS_Station_Height
                YAML     067_VarField_GPS_Station_Height.yaml
                DATA     067_VarField_GPS_Station_Height.nc4)
ADD_WRITER_TEST(NAME     varobswriter_068_VarField_clw
                YAML     068_VarField_clw.yaml
                NAMELIST VarObsWriterNamelists_068_VarField_clw/AMSUB.nl
                DATA     068_VarField_clw.nc4)
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
ADD_WRITER_TEST(NAME     varobswriter_076_VarField_britempvarerror
                YAML     076_VarField_britempvarerror.yaml
                NAMELIST VarObsWriterNamelists_076_VarField_britempvarerror/AMSUB.nl
                DATA     076_VarField_britempvarerror.nc4)
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
ADD_WRITER_TEST(NAME     varobswriter_081_VarField_level_time
                YAML     081_VarField_level_time.yaml
                NAMELIST VarObsWriterNamelists_081_VarField_level_time/Sonde.nl
                DATA     078_VarField_theta.nc4)
ADD_WRITER_TEST(NAME     varobswriter_082_VarField_level_lat
                YAML     082_VarField_level_lat.yaml
                NAMELIST VarObsWriterNamelists_082_VarField_level_lat/Sonde.nl
                DATA     078_VarField_theta.nc4)
ADD_WRITER_TEST(NAME     varobswriter_083_VarField_level_lon
                YAML     083_VarField_level_lon.yaml
                NAMELIST VarObsWriterNamelists_083_VarField_level_lon/Sonde.nl
                DATA     078_VarField_theta.nc4)

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
ADD_WRITER_TEST(NAME     varobswriter_FixedHeader_Offset_Time
                YAML     varobswriter_FixedHeader_Offset_Time.yaml
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

# Tests the namelist files in the etc directory
ADD_WRITER_TEST(NAME     varobswriter_globalnamelist_atms
                YAML     varobswriter_globalnamelist_atms.yaml
                NAMELIST ../../etc/global/varobs/ATMS.nl
                DATA     varobs_globalnamelist_atms.nc4)
ADD_WRITER_TEST(NAME     varobswriter_globalnamelist_atovs
                YAML     varobswriter_globalnamelist_atovs.yaml
                NAMELIST ../../etc/global/varobs/ATOVS.nl
                DATA     varobs_globalnamelist_atovs.nc4)
ADD_WRITER_TEST(NAME     varobswriter_globalnamelist_gnssro
                YAML     varobswriter_globalnamelist_gnssro.yaml
                NAMELIST ../../etc/global/varobs/GPSRO.nl
                DATA     varobs_globalnamelist_gnssro.nc4)
ADD_WRITER_TEST(NAME     varobswriter_globalnamelist_iasi
                YAML     varobswriter_globalnamelist_iasi.yaml
                NAMELIST ../../etc/global/varobs/IASI.nl
                DATA     varobs_globalnamelist_iasi.nc4 018_VarField_tcozone_obsdiag.nc4)
ADD_WRITER_TEST(NAME     varobswriter_globalnamelist_sonde
                YAML     varobswriter_globalnamelist_sonde.yaml
                NAMELIST ../../etc/global/varobs/Sonde.nl
                DATA     varobs_globalnamelist_sonde.nc4)
ADD_WRITER_TEST(NAME     varobswriter_globalnamelist_scatwind
                YAML     varobswriter_globalnamelist_scatwind.yaml
                NAMELIST ../../etc/global/varobs/Scatwind.nl
                DATA     varobs_globalnamelist_scatwind.nc4)

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
ADD_WRITER_TEST(NAME     cxwriter_024_SurfaceCxField_CloudAmount
                YAML     024_SurfaceCxField_CloudAmount.yaml
                NAMELIST CxWriterNamelists_024_SurfaceCxField_CloudAmount/SatTCWV.nl
                DATA     024_SurfaceCxField_CloudAmount.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_056_SurfaceCxField_obukhov_length
                YAML     056_SurfaceCxField_obukhov_length.yaml
                NAMELIST CxWriterNamelists_056_SurfaceCxField_obukhov_length/Scatwind.nl
                DATA     056_SurfaceCxField_obukhov_length.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_057_SurfaceCxField_friction_velocity_over_water
                YAML     057_SurfaceCxField_friction_velocity_over_water.yaml
                NAMELIST CxWriterNamelists_057_SurfaceCxField_friction_velocity_over_water/Scatwind.nl
                DATA     057_SurfaceCxField_friction_velocity_over_water.nc4 dummy.nc4)

# Tests of individual upper-air varfields
ADD_WRITER_TEST(NAME     cxwriter_001_UpperAirCxField_theta
                YAML     001_UpperAirCxField_theta.yaml
                NAMELIST CxWriterNamelists_001_UpperAirCxField_theta/AMSUB.nl
                DATA     001_UpperAirCxField_theta.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_002_UpperAirCxField_rh_AMSUB
                YAML     002_UpperAirCxField_rh_AMSUB.yaml
                NAMELIST CxWriterNamelists_002_UpperAirCxField_rh/AMSUB.nl
                DATA     002_UpperAirCxField_relative_humidity.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_002_UpperAirCxField_rh_Sonde
                YAML     002_UpperAirCxField_rh_Sonde.yaml
                NAMELIST CxWriterNamelists_002_UpperAirCxField_rh/Sonde.nl
                DATA     002_UpperAirCxFieldForMultiLevelObs_relative_humidity.nc4 relative_humidity_Sonde.nc4)
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
ADD_WRITER_TEST(NAME     cxwriter_031_UpperAirCxField_cloud_bulk
                YAML     031_UpperAirCxField_cloud_bulk.yaml
                NAMELIST CxWriterNamelists_031_UpperAirCxField_cloud_bulk/AMSUB.nl
                DATA     031_UpperAirCxField_cloud_bulk.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_033_UpperAirCxField_p_bar
                YAML     033_UpperAirCxField_p_bar.yaml
                NAMELIST CxWriterNamelists_033_UpperAirCxField_p_bar/AMSUB.nl
                DATA     033_UpperAirCxField_p_bar.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_034_UpperAirCxField_Cf
                YAML     034_UpperAirCxField_Cf.yaml
                NAMELIST CxWriterNamelists_034_UpperAirCxField_Cf/AMSUB.nl
                DATA     034_UpperAirCxField_Cf.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_035_UpperAirCxField_Cl
                YAML     035_UpperAirCxField_Cl.yaml
                NAMELIST CxWriterNamelists_035_UpperAirCxField_Cl/AMSUB.nl
                DATA     035_UpperAirCxField_Cl.nc4 dummy.nc4)

ADD_WRITER_TEST(NAME     cxwriter_041-042_UpperAirCxField_dust1-dust2
                YAML     041-042_UpperAirCxField_dust1-dust2.yaml
                NAMELIST CxWriterNamelists_041-042_UpperAirCxField_dust1-dust2/AOD.nl
                DATA     041-046_UpperAirCxField_dust1-dust6.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_041-046_UpperAirCxField_dust1-dust6
                YAML     041-046_UpperAirCxField_dust1-dust6.yaml
                NAMELIST CxWriterNamelists_041-046_UpperAirCxField_dust1-dust6/AOD.nl
                DATA     041-046_UpperAirCxField_dust1-dust6.nc4 dummy.nc4)

# Test of wind un-rotation

ADD_WRITER_TEST(NAME     cxwriter_UnRotateWinds
                YAML     CxWriter_UnRotateWinds.yaml
                NAMELIST CxWriterNamelists_UnRotateWinds/Sonde.nl
                DATA     CxWriter_UnRotateWinds.nc4 dummy.nc4)

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
ADD_WRITER_TEST(NAME     cxwriter_FixedHeader_Offset_Time
                YAML     cxwriter_FixedHeader_Offset_Time.yaml
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
                DATA     001_UpperAirCxField_theta.nc4 reject_obs_with_all_variables_failing_qc_satellite.nc4)
ADD_WRITER_TEST(NAME     cxwriter_nested_output_directory
                YAML     cxwriter_nested_output_directory.yaml
                DATA     001_UpperAirCxField_theta.nc4 dummy.nc4
                NAMELIST CxWriterNamelists_001_UpperAirCxField_theta/AMSUB.nl
                DONT_SET_OPS_OUTPUT_DIR_ENV_VAR)

# Tests the namelist files in the etc directory
ADD_WRITER_TEST(NAME     cxwriter_globalnamelist_asmr
                YAML     cxwriter_globalnamelist_amsr.yaml
                NAMELIST ../../etc/global/cx/AMSR.nl
                DATA     cx_globalnamelist_amsr.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_globalnamelist_gmilow
                YAML     cxwriter_globalnamelist_gmilow.yaml
                NAMELIST ../../etc/global/cx/GMIlow.nl
                DATA     cx_globalnamelist_gmilow.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_globalnamelist_atms
                YAML     cxwriter_globalnamelist_atms.yaml
                NAMELIST ../../etc/global/cx/ATMS.nl
                DATA     cx_globalnamelist_atms.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_globalnamelist_atovs
                YAML     cxwriter_globalnamelist_atovs.yaml
                NAMELIST ../../etc/global/cx/ATOVS.nl
                DATA     cx_globalnamelist_atovs.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_globalnamelist_ssmis
                YAML     cxwriter_globalnamelist_ssmis.yaml
                NAMELIST ../../etc/global/cx/SSMIS.nl
                DATA     cx_globalnamelist_ssmis.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_globalnamelist_gnssro
                YAML     cxwriter_globalnamelist_gnssro.yaml
                NAMELIST ../../etc/global/cx/GPSRO.nl
                DATA     cx_globalnamelist_gnssro.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_globalnamelist_groundgps
                YAML     cxwriter_globalnamelist_groundgps.yaml
                NAMELIST ../../etc/global/cx/GroundGPS.nl
                DATA     cx_globalnamelist_groundgps.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_globalnamelist_iasi
                YAML     cxwriter_globalnamelist_iasi.yaml
                DATA     cx_globalnamelist_iasi.nc4 varobs_globalnamelist_iasi.nc4)
ADD_WRITER_TEST(NAME     cxwriter_globalnamelist_sonde
                YAML     cxwriter_globalnamelist_sonde.yaml
                NAMELIST ../../etc/global/cx/Sonde.nl
                DATA     cx_globalnamelist_sonde.nc4 dummy.nc4)
ADD_WRITER_TEST(NAME     cxwriter_globalnamelist_sattcwv
                YAML     cxwriter_globalnamelist_sattcwv.yaml
                NAMELIST ../../etc/global/cx/SatTCWV.nl
                DATA     cx_globalnamelist_sattcwv.nc4 dummy.nc4)

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
