# (C) Copyright 2020 Met Office UK
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

list( APPEND cxvarobs_src_files
    cxvarobs_varobswriter_mod.F90
    instantiateObsFilterFactory.h
    obsdatavector_f.cc
    obsdatavector_f.h
    obsdatavector_mod.F90
    VarObsWriter.h
    VarObsWriter.cc
    VarObsWriter.interface.F90
    VarObsWriter.interface.h
)

# TEMPORARY

set(OPS_ROOT "/net/data/users/wsmigaj/cylc-run/r12482_2330_buddy_check_near_lon_180/share/fcm_make_ops_x86_64_gfortran_debug/build" CACHE STRING "OPS root")

set(OPS_INCLUDE_DIR "${OPS_ROOT}/include")
set(OPS_OBJECT_DIR "${OPS_ROOT}/o")
set(SHUM_LIB_DIR "${OPS_ROOT}/installs/shumlib/lib")
set(SHUM_LIBS 
    "${SHUM_LIB_DIR}/libshum_byteswap.so"
    "${SHUM_LIB_DIR}/libshum_string_conv.so"
    "${SHUM_LIB_DIR}/libshum_wgdos_packing.so")
file(GLOB OPS_OBJ_FILES LIST_DIRECTORIES false "${OPS_OBJECT_DIR}/*.o")

ecbuild_add_library( TARGET   ops
                     SOURCES  ${OPS_OBJ_FILES}
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
