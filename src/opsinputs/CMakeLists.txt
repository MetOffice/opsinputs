# (C) Copyright 2020 Met Office UK
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

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
    opsinputs_utils_mod.F90
    CxWriter.cc
    CxWriter.h
    CxWriter.interface.h
    CxWriter.interface.F90
    CxWriterParameters.h
    opsinputs_mpl_mod.F90
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
set_source_files_properties(opsinputs_cxgenerate_mod.F90 PROPERTIES
                            COMPILE_FLAGS "-fdefault-real-8 -fdefault-integer-8")

set(ODB_LIB_DIR "${OPS_INSTALLS_DIR}/odb/lib")
set(ODB_LIBS odb odbifsaux odbemos)
list(TRANSFORM ODB_LIBS PREPEND "${ODB_LIB_DIR}/lib")
list(TRANSFORM ODB_LIBS APPEND ".so")

set(SHUM_LIB_DIR "${OPS_INSTALLS_DIR}/shumlib/lib")
set(SHUM_LIBS shum_byteswap shum_string_conv shum_wgdos_packing)
list(TRANSFORM SHUM_LIBS PREPEND "${SHUM_LIB_DIR}/lib")
list(TRANSFORM SHUM_LIBS APPEND ".so")

set(OPS_O_FILES
    gc__abort.o
    gc__flush.o
    gc__get_mpi_type.o
    gc__stamp.o
    gc_abort.o
    gc_getopt.o
    gc_globals_mod.o
    gc_init.o
    gc_me.o
    gc_nproc.o
    gc_rsumr.o
    gc_setopt.o
    gcg__mpi_rank.o
    gcg_isum.o
    gcg_rsumr.o
    gen_clockticks.o
    gen_cputime.o
    gen_memory.o
    gen_sleepwrapper.o
    genmod_control.o
    genmod_core.o
    genmod_matmul.o
    genmod_modelio.o
    genmod_platform.o
    genmod_setup.o
    genmod_sleep.o
    genmod_utilities.o
    mpl_abort.o
    mpl_allgather.o
    mpl_allreduce.o
    mpl_alltoallv.o
    mpl_barrier.o
    mpl_bcast.o
    mpl_bsend.o
    mpl_buffer_attach.o
    mpl_comm_get_attr.o
    mpl_comm_group.o
    mpl_comm_rank.o
    mpl_comm_size.o
    mpl_comm_split.o
    mpl_comm_split_type.o
    mpl_finalize.o
    mpl_gather.o
    mpl_gatherv.o
    mpl_group_free.o
    mpl_group_translate_ranks.o
    mpl_init.o
    mpl_initialized.o
    mpl_irecv.o
    mpl_isend.o
    mpl_recv.o
    mpl_scatterv.o
    mpl_send.o
    mpl_type_commit.o
    mpl_type_free.o
    mpl_type_vector.o
    mpl_waitall.o
    mpl_win_allocate_shared.o
    mpl_win_free.o
    mpl_win_shared_query.o
    ops_ereport_mod.o
    ops_file_size.o
    ops_netcdf_wrappers.o
    ops_portio2a.o
    opsmod_aodgeneral.o
    opsmod_charutils.o
    opsmod_control.o
    opsmod_cxinfo.o
    opsmod_datetime.o
    opsmod_envutils.o
    opsmod_fieldread.o
    opsmod_gatherspread.o
    opsmod_gcom.o
    opsmod_gpsro.o
    opsmod_gpsroinfo_ba.o
    opsmod_groundgps.o
    opsmod_horizontalinterp.o
    opsmod_ioutils.o
    opsmod_misctypes.o
    opsmod_modelcolumnio.o
    opsmod_modelio.o
    opsmod_modelobinfo.o
    opsmod_mplinterface.o
    opsmod_obsgroupinfo.o
    opsmod_obsinfo.o
    opsmod_obtypeutils.o
    opsmod_portio_wrappers.o
    opsmod_process.o
    opsmod_qc.o
    opsmod_qcbuddy.o
    opsmod_radar.o
    opsmod_radarz.o
    opsmod_random.o
    opsmod_satrad_rtmodel.o
    opsmod_satrad_setup.o
    opsmod_sondedrift.o
    opsmod_sort.o
    opsmod_utilities.o
    opsmod_varobs.o
    opsmod_varobslib.o)
list(TRANSFORM OPS_O_FILES PREPEND "${OPS_O_DIR}/")

ecbuild_find_mpi( COMPONENTS Fortran REQUIRED )

ecbuild_add_library( TARGET ops
                     SOURCES ${OPS_O_FILES}
                     LINKER_LANGUAGE Fortran
                     TYPE STATIC
                     LIBS ${ODB_LIBS} ${SHUM_LIBS} ${MPI_Fortran_LIBRARIES}
                    )

list( APPEND OPSINPUTS_LIBS_DEP oops ioda ufo ${NETCDF_LIBRARIES} ops )

ecbuild_add_library( TARGET   opsinputs
                     SOURCES  ${opsinputs_src_files}
                     LIBS     ${OPSINPUTS_LIBS_DEP}
                     INSTALL_HEADERS LISTED
                     HEADER_DESTINATION ${INSTALL_INCLUDE_DIR}/opsinputs
                     LINKER_LANGUAGE ${OPSINPUTS_LINKER_LANGUAGE}
                     PRIVATE_INCLUDES ${OPS_INCLUDE_DIR}
                     TYPE STATIC
                    )
