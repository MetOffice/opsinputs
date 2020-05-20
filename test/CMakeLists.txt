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

# Create a directory for test input config and symlink all files
list( APPEND cxvarobs_test_input
  testinput/varobswriter.yaml
  testinput/VarObsWriterNamelistsAMSUB/AMSUB.nl
)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput)
# TODO(wsmigaj): find a more elegant solution to create the required subdirectories of testinput
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput/VarObsWriterNamelistsAMSUB)
CREATE_SYMLINK( ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR} ${cxvarobs_test_input} )

# Add files to cmake resources
ecbuild_add_resources( TARGET cxvarobs_test_scripts
                       SOURCES_PACK
                       ${cxvarobs_test_input}
                     )

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
                        SOURCES mains/TestCxVarObsFilters.cc
                        LIBS    cxvarobs )

ecbuild_add_test( TARGET  test_cxvarobs_varobswriter
                  COMMAND ${CMAKE_BINARY_DIR}/bin/test_CxVarObsFilters.x
                  ARGS    "testinput/varobswriter.yaml"
                  ENVIRONMENT OOPS_TRAPFPE=1
                  DEPENDS test_CxVarObsFilters.x )
