# (C) Copyright 2020 Met Office UK
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

list( APPEND cxvarobs_src_files
    VarObsWriter.h
    VarObsWriter.cc
    VarObsWriter.interface.F90
    VarObsWriter.interface.h
    cxvarobs_varobswriter_mod.F90
)

list( APPEND CXVAROBS_LIBS_DEP oops ioda ufo ${NETCDF_LIBRARIES} )

ecbuild_add_library( TARGET   cxvarobs
                     SOURCES  ${cxvarobs_src_files}
                     LIBS     ${CXVAROBS_LIBS_DEP}
                     INSTALL_HEADERS LISTED
                     HEADER_DESTINATION ${INSTALL_INCLUDE_DIR}/cxvarobs
                     LINKER_LANGUAGE ${OOPS_LINKER_LANGUAGE}
                    )
