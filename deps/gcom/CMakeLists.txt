# (C) Crown Copyright 2021, the Met Office. All rights reserved.
#

if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
  if("${CMAKE_Fortran_COMPILER_VERSION}" VERSION_GREATER "10.1.0")
    set(GCOM_EXTRA_Fortran_FLAGS "-Wno-unused-dummy-argument -Wno-unused -fallow-argument-mismatch")
  else()
    set(GCOM_EXTRA_Fortran_FLAGS "-Wno-unused-dummy-argument -Wno-unused")
  endif()
elseif("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
  set(GCOM_EXTRA_Fortran_FLAGS "-warn nounused")
endif()

set(CMAKE_Fortran_FLAGS "\
${OPS_DEFAULT_Fortran_FLAGS} \
${GCOM_EXTRA_Fortran_FLAGS}")

set(GCOM_SOURCE_FILES
  gc/gc__abort.c
  gc/gc__buildconst.F90
  gc/gc__errlim.F90
  gc/gc__flush.F90
  gc/gc__get_mpi_type.F90
  gc/gc__stamp.F90
  gc/gc_abort.F90
  gc/gc_bbcast.F90
  gc/gc_brecv.F90
  gc/gc_bsend.F90
  gc/gc_cbcast.F90
  gc/gc_config.F90
  gc/gc_crecv.F90
  gc/gc_csend.F90
  gc/gc_exit.F90
  gc/gc_finalized.F90
  gc/gc_get_communicator.F90
  gc/gc_getopt.F90
  gc/gc_globals_mod.F90
  gc/gc_gsync.F90
  gc/gc_ibcast.F90
  gc/gc_imax.F90
  gc/gc_imax_single_task.F90
  gc/gc_imin.F90
  gc/gc_imin_single_task.F90
  gc/gc_init.F90
  gc/gc_init_thread.F90
  gc/gc_initialized.F90
  gc/gc_irecv.F90
  gc/gc_isend.F90
  gc/gc_isum.F90
  gc/gc_kinds_mod.F90
  gc/gc_me.F90
  gc/gc_nproc.F90
  gc/gc_rbcast.F90
  gc/gc_rmax.F90
  gc/gc_rmax_single_task.F90
  gc/gc_rmin.F90
  gc/gc_rmin_single_task.F90
  gc/gc_rrecv.F90
  gc/gc_rsend.F90
  gc/gc_rsum.F90
  gc/gc_rsumr.F90
  gc/gc_rsumr_kind32.F90
  gc/gc_set_communicator.F90
  gc/gc_setopt.F90
  gc/gc_ssync.F90
  gc/gc_zsum.F90
  gc/gc_zsumr.F90
  gc/gcom_mod.F90
  gcg/gcg__errlim.F90
  gcg/gcg__mpi_rank.F90
  gcg/gcg_bbcast.F90
  gcg/gcg_cbcast.F90
  gcg/gcg_config.F90
  gcg/gcg_gsync.F90
  gcg/gcg_ibcast.F90
  gcg/gcg_imax.F90
  gcg/gcg_imin.F90
  gcg/gcg_isum.F90
  gcg/gcg_me.F90
  gcg/gcg_r2darrsum.F90
  gcg/gcg_r2darrsum_kind32.F90
  gcg/gcg_ralltoalle.F90
  gcg/gcg_ralltoalle_multi.F90
  gcg/gcg_rbcast.F90
  gcg/gcg_rmax.F90
  gcg/gcg_rmin.F90
  gcg/gcg_rsum.F90
  gcg/gcg_rsum_kind32.F90
  gcg/gcg_rsumr.F90
  gcg/gcg_rsumr_kind32.F90
  gcg/gcg_rvecshift.F90
  gcg/gcg_rvecsumf.F90
  gcg/gcg_rvecsumr.F90
  gcg/gcg_rvecsumrf.F90
  gcg/gcg_split.F90
  gcg/gcg_ssync.F90
  gcg/gcg_zsum.F90
  gcg/gcg_zsumr.F90
  mpl/mpl.F90
  mpl/mpl_abort.F90
  mpl/mpl_allgather.F90
  mpl/mpl_allgatherv.F90
  mpl/mpl_allreduce.F90
  mpl/mpl_alltoall.F90
  mpl/mpl_alltoallv.F90
  mpl/mpl_barrier.F90
  mpl/mpl_bcast.F90
  mpl/mpl_bsend.F90
  mpl/mpl_buffer_attach.F90
  mpl/mpl_cancel.F90
  mpl/mpl_cart_create.F90
  mpl/mpl_comm_call_errhandler.F90
  mpl/mpl_comm_create.F90
  mpl/mpl_comm_create_errhandler.F90
  mpl/mpl_comm_dup.F90
  mpl/mpl_comm_get_attr.F90
  mpl/mpl_comm_get_errhandler.F90
  mpl/mpl_comm_group.F90
  mpl/mpl_comm_rank.F90
  mpl/mpl_comm_set_errhandler.F90
  mpl/mpl_comm_size.F90
  mpl/mpl_comm_split.F90
  mpl/mpl_comm_split_type.F90
  mpl/mpl_compare_and_swap.F90
  mpl/mpl_error_class.F90
  mpl/mpl_error_string.F90
  mpl/mpl_fetch_and_op.F90
  mpl/mpl_file_close.F90
  mpl/mpl_file_get_position.F90
  mpl/mpl_file_get_size.F90
  mpl/mpl_file_open.F90
  mpl/mpl_file_read.F90
  mpl/mpl_file_seek.F90
  mpl/mpl_file_set_errhandler.F90
  mpl/mpl_file_sync.F90
  mpl/mpl_file_write.F90
  mpl/mpl_file_write_at_all.F90
  mpl/mpl_finalize.F90
  mpl/mpl_finalized.F90
  mpl/mpl_gather.F90
  mpl/mpl_gatherv.F90
  mpl/mpl_get.F90
  mpl/mpl_get_count.F90
  mpl/mpl_get_processor_name.F90
  mpl/mpl_group_free.F90
  mpl/mpl_group_incl.F90
  mpl/mpl_group_translate_ranks.F90
  mpl/mpl_ibarrier.F90
  mpl/mpl_info_create.F90
  mpl/mpl_info_free.F90
  mpl/mpl_info_get.F90
  mpl/mpl_info_set.F90
  mpl/mpl_init.F90
  mpl/mpl_init_thread.F90
  mpl/mpl_initialized.F90
  mpl/mpl_iprobe.F90
  mpl/mpl_irecv.F90
  mpl/mpl_isend.F90
  mpl/mpl_issend.F90
  mpl/mpl_op_create.F90
  # These files are excluded, since they are unused and warnings are emitted during
  # their compilation with ifort, which prevents us from enabling the "treat warnings
  # as errors" mode.
  # mpl/mpl_pack.F90
  # mpl/mpl_pack_size.F90
  # mpl/mpl_unpack.F90
  mpl/mpl_probe.F90
  mpl/mpl_put.F90
  mpl/mpl_query_thread.F90
  mpl/mpl_recv.F90
  mpl/mpl_reduce.F90
  mpl/mpl_rget.F90
  mpl/mpl_rput.F90
  mpl/mpl_scatterv.F90
  mpl/mpl_send.F90
  mpl/mpl_sendrecv.F90
  mpl/mpl_ssend.F90
  mpl/mpl_test.F90
  mpl/mpl_testall.F90
  mpl/mpl_type_commit.F90
  mpl/mpl_type_create_resized.F90
  mpl/mpl_type_create_struct.F90
  mpl/mpl_type_extent.F90
  mpl/mpl_type_free.F90
  mpl/mpl_type_get_extent.F90
  mpl/mpl_type_struct.F90
  mpl/mpl_type_vector.F90
  mpl/mpl_wait.F90
  mpl/mpl_waitall.F90
  mpl/mpl_waitany.F90
  mpl/mpl_win_allocate.F90
  mpl/mpl_win_allocate_shared.F90
  mpl/mpl_win_create.F90
  mpl/mpl_win_fence.F90
  mpl/mpl_win_flush.F90
  mpl/mpl_win_flush_all.F90
  mpl/mpl_win_flush_local.F90
  mpl/mpl_win_flush_local_all.F90
  mpl/mpl_win_free.F90
  mpl/mpl_win_lock.F90
  mpl/mpl_win_lock_all.F90
  mpl/mpl_win_shared_query.F90
  mpl/mpl_win_sync.F90
  mpl/mpl_win_unlock.F90
  mpl/mpl_win_unlock_all.F90
  mpl/mpl_wtime.F90
)

set(GCOM_SOURCE_DIRS
  gc gcg include mpl
)

# Setup the parallel version of the gcom library

if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "NAG")
  ecbuild_add_library(
    TARGET gcom
    SOURCES ${GCOM_SOURCE_FILES}
    LINKER_LANGUAGE CXX)
else()
  ecbuild_add_library(
    TARGET gcom
    SOURCES ${GCOM_SOURCE_FILES})
endif()
 
target_compile_definitions(gcom PRIVATE 
  GC_VERSION="'7.2'" 
  GC_DESCRIP="'MPP'" 
  GC_BUILD_DATE="'927'"
  MPI_SRC 
  MPILIB_32B 
  PREC_64B 
  GC__FORTERRUNIT=0 
  GC__FLUSHUNIT6 
#  USE_MPIF_INCLUDE
)

target_link_libraries(gcom PRIVATE MPI::MPI_Fortran)

get_target_property(GCOM_Fortran_MODULE_DIRECTORY gcom Fortran_MODULE_DIRECTORY)

target_include_directories(gcom
  PRIVATE ${GCOM_SOURCE_DIRS}
  INTERFACE $<BUILD_INTERFACE:${GCOM_Fortran_MODULE_DIRECTORY}>
)

# Setup the serial version of the gcom library

if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "NAG")
  ecbuild_add_library(
    TARGET gcom_serial
    SOURCES ${GCOM_SOURCE_FILES}
    LINKER_LANGUAGE CXX)
else()
  ecbuild_add_library(
    TARGET gcom_serial
    SOURCES ${GCOM_SOURCE_FILES})
endif()

# For the serial version, skip the MPI_SRC symbol...
target_compile_definitions(gcom_serial PRIVATE
  GC_VERSION="'7.2'"
  GC_DESCRIP="'SERIAL'"
  GC_BUILD_DATE="'927'"
  MPILIB_32B
  PREC_64B
  GC__FORTERRUNIT=0
  GC__FLUSHUNIT6
#  USE_MPIF_INCLUDE
)

# ... and don't link to MPI::MPI_Fortran.

get_target_property(GCOM_SERIAL_Fortran_MODULE_DIRECTORY gcom_serial Fortran_MODULE_DIRECTORY)
set(GCOM_SERIAL_Fortran_MODULE_DIRECTORY ${GCOM_SERIAL_Fortran_MODULE_DIRECTORY}/serial)
set_target_properties(gcom_serial PROPERTIES
                      Fortran_MODULE_DIRECTORY ${GCOM_SERIAL_Fortran_MODULE_DIRECTORY})

target_include_directories(gcom_serial
  PRIVATE ${GCOM_SOURCE_DIRS}
  INTERFACE $<BUILD_INTERFACE:${GCOM_SERIAL_Fortran_MODULE_DIRECTORY}>
)
