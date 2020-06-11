# (C) Copyright 2020 Met Office UK
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

list( APPEND cxvarobs_src_files
    cxvarobs_cxgenerate_mod.F90
    cxvarobs_cxwriter_mod.F90
    cxvarobs_mpl_mod.F90
    cxvarobs_obsspace_f.cc
    cxvarobs_obsspace_f.h
    cxvarobs_obsspace_mod.F90
    cxvarobs_obsdatavector_f.cc
    cxvarobs_obsdatavector_f.h
    cxvarobs_obsdatavector_mod.F90
    cxvarobs_utils_mod.F90
    cxvarobs_varobswriter_mod.F90
    CxWriter.cc
    CxWriter.h
    CxWriter.interface.F90
    CxWriter.interface.h
    CxWriterParameters.h
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
)

# TODO(wsmigaj): support Intel's compiler option
set_source_files_properties(cxvarobs_cxgenerate_mod.F90 PROPERTIES
                            COMPILE_FLAGS "-fdefault-real-8 -fdefault-integer-8")

set(SHUM_LIB_DIR "${OPS_INSTALLS_DIR}/shumlib/lib")
set(SHUM_LIBS 
    "${SHUM_LIB_DIR}/libshum_byteswap.so"
    "${SHUM_LIB_DIR}/libshum_string_conv.so"
    "${SHUM_LIB_DIR}/libshum_wgdos_packing.so")
file(GLOB OPS_O_FILES LIST_DIRECTORIES false "${OPS_O_DIR}/*.o")

ecbuild_add_library( TARGET   ops
                     SOURCES  ${OPS_O_FILES}
                     LINKER_LANGUAGE Fortran
                     TYPE STATIC
                     LIBS ${SHUM_LIBS}
                    )

list( APPEND CXVAROBS_LIBS_DEP oops ioda ufo ${NETCDF_LIBRARIES} ops )

ecbuild_add_library( TARGET   cxvarobs
                     SOURCES  ${cxvarobs_src_files}
                     LIBS     ${CXVAROBS_LIBS_DEP}
                     INSTALL_HEADERS LISTED
                     HEADER_DESTINATION ${INSTALL_INCLUDE_DIR}/cxvarobs
                     LINKER_LANGUAGE ${OOPS_LINKER_LANGUAGE}
                     PRIVATE_INCLUDES ${OPS_INCLUDE_DIR}
                     TYPE STATIC
                    )
