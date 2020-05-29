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
endfunction(CREATE_SYMLINK)

# macro to create a symlink from src to dst with just filename
function(CREATE_SYMLINK_FILENAME src dst)
    foreach (FILENAME ${ARGN})
        get_filename_component(filename ${FILENAME} NAME )
        execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
            ${src}/${FILENAME}
            ${dst}/${filename} )
        endforeach(FILENAME)
endfunction(CREATE_SYMLINK_FILENAME)

# macro to define a VarObsWriter test
function(ADD_VAROBSWRITER_TEST name)
  ecbuild_add_test( TARGET  test_cxvarobs_varobswriter_${name}
                    COMMAND ${CMAKE_BINARY_DIR}/bin/test_CxVarObsFilters.x
                    ARGS    "testinput/${name}.yaml"
                    ENVIRONMENT
                      OOPS_TRAPFPE=1
                      LD_LIBRARY_PATH=${OPS_ROOT}/installs/odb/lib:${OPS_ROOT}/installs/shumlib/lib:$ENV{LD_LIBRARY_PATH}
                    DEPENDS test_CxVarObsFilters.x )
endfunction(ADD_VAROBSWRITER_TEST)

# Create a directory for test input config and symlink all files
list( APPEND cxvarobs_test_input
  testinput/varobswriter.yaml
  testinput/VarObsWriterNamelistsAMSUB/AMSUB.nl
  testinput/001_VarField_pstar.yaml
  testinput/002_VarField_temperature_RadarZ.yaml
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
  testinput/071_VarField_bendingangle.yaml
  testinput/072_VarField_impactparam.yaml
  testinput/073_VarField_ro_rad_curv.yaml
  testinput/074_VarField_ro_geoid_und.yaml
  testinput/077_VarField_aod.yaml
  testinput/078_VarField_theta.yaml
  testinput/FixedHeader_GridStagger.yaml
  testinput/FixedHeader_HorizGrid.yaml
  testinput/FixedHeader_VertCoord.yaml
  testinput/IntegerConstants_LensAndLevels.yaml
  testinput/IntegerConstants_TorTheta.yaml
  testinput/RealConstants.yaml
  testinput/VarObsWriterNamelists_010_VarField_britemp/AMSUB.nl
  testinput/VarObsWriterNamelists_019_VarField_satzenith/AMSUB.nl
  testinput/VarObsWriterNamelists_023_VarField_modelsurface/Surface.nl
  testinput/VarObsWriterNamelists_031_VarField_solzenith/AMSUB.nl
  testinput/VarObsWriterNamelists_054_VarField_numchans/AMSUB.nl
  testinput/VarObsWriterNamelists_055_VarField_channum/AMSUB.nl
)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput)
# TODO(wsmigaj): find a more elegant solution to create the required subdirectories of testinput
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput/VarObsWriterNamelistsAMSUB)
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
                       ${cxvarobs_test_input}
                     )

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
  071_VarField_bendingangle.nc4
  072_VarField_impactparam.nc4
  073_VarField_ro_rad_curv.nc4
  074_VarField_ro_geoid_und.nc4
  077_VarField_aod.nc4
  078_VarField_theta.nc4
)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Data)
CREATE_SYMLINK_FILENAME( ${CMAKE_CURRENT_SOURCE_DIR}/testinput
                         ${CMAKE_CURRENT_BINARY_DIR}/Data ${cxvarobs_test_data} )


file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Data)

# IODA observation files 
list( APPEND cxvarobs_ioda_test_data
      testinput_tier_1.tar.gz)

# Set URL for IODA test files
set(CXVAROBS_IODA_DOWNLOAD_BASE_URL https://jedi-test-files.s3.amazonaws.com)

# If local path to testfiles is defined don't download
if (DEFINED ENV{LOCAL_PATH_TESTFILES_IODA})
        set(LOCAL_PATH_TESTFILES_IODA "$ENV{LOCAL_PATH_TESTFILES_IODA}")
endif()

if( NOT DEFINED LOCAL_PATH_TESTFILES_IODA )

  message(STATUS "LOCAL_PATH_TESTFILES_IODA is not defined, download test files")
  if ( NOT DEFINED CXVAROBS_IODA_TESTFILES_BRANCH)

    # Get the current git branch of ioda repo
    execute_process(
      COMMAND git rev-parse --abbrev-ref HEAD
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      OUTPUT_VARIABLE GIT_BRANCH_CXVAROBS
      OUTPUT_STRIP_TRAILING_WHITESPACE)
    message(STATUS "CXVAROBS is in branch " ${GIT_BRANCH_CXVAROBS})

  else()
    message(STATUS "Branch name provided by user")
    set(GIT_BRANCH_CXVAROBS ${CXVAROBS_IODA_TESTFILES_BRANCH})
  endif()

  # Check whether the URL exists or not
  set(ECBUILD_DOWNLOAD_BASE_URL ${CXVAROBS_IODA_DOWNLOAD_BASE_URL}/ioda)

  ecbuild_check_multiurl(NAMES   ${cxvarobs_ioda_test_data}
                         DIRHOST ${GIT_BRANCH_CXVAROBS}
                         RESULT  CXVAROBS_SPECIFIC_TEST_FILES)

  # Set distant directory
  if(${CXVAROBS_SPECIFIC_TEST_FILES} MATCHES 0)
    # Download and extract new test files (distant directory = git branch)
    set(DIRNAME ${GIT_BRANCH_CXVAROBS})
    message(STATUS "GIT_BRANCH_CXVAROBS found, will download " ${GIT_BRANCH_CXVAROBS})
  else()
    # Download and extract develop test files (distant directory = develop)
    set(DIRNAME "develop")
    message(STATUS "GIT_BRANCH_CXVAROBS not found, will download develop")
  endif()

  message(STATUS "Test data will be download from: " ${CXVAROBS_IODA_DOWNLOAD_BASE_URL}/${DIRNAME})

  set(CXVAROBS_IODA_REP_NAME ioda)
  set(CXVAROBS_IODA_TESTFILES_NAME ${cxvarobs_ioda_test_data})
  set(CXVAROBS_IODA_BRANCH_NAME ${DIRNAME})
  set(CXVAROBS_IODA_TESTFILES_PATH ${CMAKE_BINARY_DIR}/test_data/ioda/${DIRNAME})
  message(STATUS "Save data to " ${TESTFILE_DIR_IODA})
  file(MAKE_DIRECTORY ${CXVAROBS_IODA_TESTFILES_PATH})

  # Create download script for get_ioda_test_data test
  set ( FILENAME cxvarobs_ioda_data_downloader.py)
  set ( SOURCE_FILE ${CMAKE_CURRENT_SOURCE_DIR}/${FILENAME} )
  set ( DEST_FILE ${CMAKE_BINARY_DIR}/bin/${FILENAME} )
  list( APPEND bin_cxvarobs_ioda_test_download_scripts_deps ${DEST_FILE} )

  if( EXISTS "${SOURCE_FILE}.in" )
    configure_file( ${SOURCE_FILE}.in ${DEST_FILE} @ONLY )
  else()
    configure_file( ${SOURCE_FILE}    ${DEST_FILE} @ONLY )
  endif()

  add_custom_target( bin_cxvarobs_ioda_test_download_scripts ALL
      COMMAND chmod +x ${bin_cxvarobs_ioda_test_download_scripts_deps}
      DEPENDS ${bin_cxvarobs_ioda_test_download_scripts_deps})

  ecbuild_add_test( TARGET    cxvarobs_get_ioda_test_data
                    TYPE      SCRIPT
                    COMMAND    ${CMAKE_BINARY_DIR}/bin/cxvarobs_ioda_data_downloader.py
                    ARGS      testoutput/download_test_cxvarobs.log)

else()
  set(CXVAROBS_IODA_TESTFILES_PATH ${LOCAL_PATH_TESTFILES_IODA})
  message(STATUS "use LOCAL_PATH_TESTFILES_IODA : " ${LOCAL_PATH_TESTFILES_IODA})
endif()

execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
                 ${CXVAROBS_IODA_TESTFILES_PATH}
                 ${CMAKE_CURRENT_BINARY_DIR}/Data/ioda )

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

ecbuild_add_test( TARGET  test_cxvarobs_varobswriter
                  COMMAND ${CMAKE_BINARY_DIR}/bin/test_CxVarObsFilters.x
                  ARGS    "testinput/varobswriter.yaml"
                  ENVIRONMENT OOPS_TRAPFPE=1
                  DEPENDS test_CxVarObsFilters.x )

# Tests of individual varfields
ADD_VAROBSWRITER_TEST(001_VarField_pstar)
ADD_VAROBSWRITER_TEST(002_VarField_temperature_Surface)
ADD_VAROBSWRITER_TEST(002_VarField_temperature_RadarZ)
ADD_VAROBSWRITER_TEST(003_VarField_rh_Surface)
ADD_VAROBSWRITER_TEST(003_VarField_rh_Sonde)
ADD_VAROBSWRITER_TEST(004_VarField_u_Surface)
ADD_VAROBSWRITER_TEST(004_VarField_u_Sonde)
ADD_VAROBSWRITER_TEST(005_VarField_v_Surface)
ADD_VAROBSWRITER_TEST(005_VarField_v_Sonde)
ADD_VAROBSWRITER_TEST(010_VarField_britemp)
ADD_VAROBSWRITER_TEST(019_VarField_satzenith)
ADD_VAROBSWRITER_TEST(023_VarField_modelsurface_geoval)
ADD_VAROBSWRITER_TEST(028_VarField_satid)
ADD_VAROBSWRITER_TEST(031_VarField_solzenith)
ADD_VAROBSWRITER_TEST(054_VarField_numchans)
ADD_VAROBSWRITER_TEST(055_VarField_channum)
ADD_VAROBSWRITER_TEST(071_VarField_bendingangle)
ADD_VAROBSWRITER_TEST(072_VarField_impactparam)
ADD_VAROBSWRITER_TEST(073_VarField_ro_rad_curv)
ADD_VAROBSWRITER_TEST(074_VarField_ro_geoid_und)
ADD_VAROBSWRITER_TEST(077_VarField_aod)
ADD_VAROBSWRITER_TEST(078_VarField_theta)

# Parameter tests
ADD_VAROBSWRITER_TEST(FixedHeader_VertCoord)
ADD_VAROBSWRITER_TEST(FixedHeader_HorizGrid)
ADD_VAROBSWRITER_TEST(FixedHeader_GridStagger)
ADD_VAROBSWRITER_TEST(IntegerConstants_LensAndLevels)
ADD_VAROBSWRITER_TEST(IntegerConstants_TorTheta)
ADD_VAROBSWRITER_TEST(RealConstants)
