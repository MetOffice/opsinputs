# (C) Crown Copyright 2020, the Met Office. All rights reserved.
#

list( APPEND opsinputs_src_files
    opsinputs_cxfields_mod.F90
    opsinputs_fill_mod.F90
    opsinputs_obsspace_f.cc
    opsinputs_obsspace_f.h
    opsinputs_obsspace_mod.F90
    opsinputs_obsdatavector_f.cc
    opsinputs_obsdatavector_f.h
    opsinputs_obsdatavector_mod.F90
    opsinputs_varobswriter_mod.F90
    opsinputs_cxgenerate_mod.F90
    opsinputs_cxwriter_mod.F90
    opsinputs_jeditoopslayoutmapping_mod.F90
    opsinputs_utils_mod.F90
    CxWriter.cc
    CxWriter.h
    CxWriter.interface.h
    CxWriter.interface.F90
    CxWriterParameters.h
    opsinputs_mpl_mod.F90
    instantiateObsFilterFactory.cc
    instantiateObsFilterFactory.h
    LocalEnvironment.cc
    LocalEnvironment.h
    MPIExceptionSynchronizer.cc
    MPIExceptionSynchronizer.h
    VarObsWriter.cc
    VarObsWriter.h
    VarObsWriter.interface.F90
    VarObsWriter.interface.h
    VarObsWriterParameters.h
    GnssroStationIDMetOffice.cc
    GnssroStationIDMetOffice.h
)

# The opsinputs_cxgenerate_mod.F90 module includes *.inc files from OPS,
# so it must be compiled with similar flags as OPS,
# in particular assuming that integers and reals without kind are 64-bit.
if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
  set(FORTRAN_FLAGS_64_BIT_TYPES "-fdefault-real-8 -fdefault-integer-8")
elseif ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
  set(FORTRAN_FLAGS_64_BIT_TYPES "-integer-size 64 -real-size 64")
endif()
set_source_files_properties(opsinputs_cxgenerate_mod.F90 PROPERTIES
                            COMPILE_FLAGS "${FORTRAN_FLAGS_64_BIT_TYPES}")

# In addition, we need to add the folder with these *.inc files to the
# include path used when compiling that module.
set_source_files_properties(opsinputs_cxgenerate_mod.F90 PROPERTIES
                            INCLUDE_DIRECTORIES ${CMAKE_CURRENT_SOURCE_DIR}/../../deps/ops/code/OpsMod_CXGenerate)

ecbuild_add_library( TARGET   opsinputs
                     SOURCES  ${opsinputs_src_files}
                     INSTALL_HEADERS LISTED
                     HEADER_DESTINATION ${INSTALL_INCLUDE_DIR}/opsinputs
                     LINKER_LANGUAGE ${OPSINPUTS_LINKER_LANGUAGE}
                     PRIVATE_INCLUDES ${OPS_INCLUDE_DIR} )
target_link_libraries( opsinputs PUBLIC ufo ioda oops PRIVATE ops )

## Include paths
target_include_directories(opsinputs PUBLIC $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src>
                                            $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>)

## Fortran modules
set(MODULE_DIR ${PROJECT_NAME}/module)
set_target_properties(${PROJECT_NAME} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/${MODULE_DIR})
install(DIRECTORY ${CMAKE_BINARY_DIR}/${MODULE_DIR}/ DESTINATION ${CMAKE_INSTALL_LIBDIR}/${MODULE_DIR})
target_include_directories(${PROJECT_NAME} INTERFACE
                            $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/${MODULE_DIR}>
                            $<INSTALL_INTERFACE:${CMAKE_INSTALL_LIBDIR}/${MODULE_DIR}>)
