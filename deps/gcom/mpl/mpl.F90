! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE mpl

!     ******************************************************************
!     * Purpose:
!     *
!     *  A Module to select the Correct kind for Integer, Real and Log
!     *  variables expected by the MPI library.
!     *  Cast the MPI parameters used in GCOM/the model to their MPL
!     *  equivalents.
!     *  And define MPL_INteger and MPL_Real to match MPI_INTEGER(8)
!     *  or MPI_REAL(8) as appropriate.
!     *
!     ******************************************************************

#if defined (MPI_SRC) && !defined (USE_MPIF_INCLUDE)
USE mpi, ONLY: mpi_comm_type_shared, mpi_comm_world, mpi_comm_self,            &
               mpi_comm_null, mpi_status_size, mpi_tag_ub, mpi_any_source,     &
               mpi_any_tag, mpi_sum, mpi_min, mpi_max, mpi_prod, mpi_land,     &
               mpi_band, mpi_lor, mpi_bor, mpi_lxor, mpi_bxor, mpi_maxloc,     &
               mpi_minloc, mpi_no_op, mpi_replace, mpi_address_kind,           &
               mpi_tag, mpi_source, mpi_undefined, mpi_error, mpi_packed,      &
               mpi_success, mpi_info_null, mpi_request_null,                   &
               mpi_max_error_string, mpi_max_processor_name,                   &
               mpi_thread_multiple, mpi_thread_single, mpi_thread_funneled,    &
               mpi_thread_serialized, mpi_file_null, mpi_offset_kind,          &
               mpi_seek_set, mpi_mode_rdonly, mpi_mode_wronly, mpi_mode_rdwr,  &
               mpi_mode_create, mpi_errors_are_fatal, mpi_mode_nocheck,        &
               mpi_mode_nostore, mpi_mode_noput, mpi_mode_noprecede,           &
               mpi_lock_exclusive, mpi_lock_shared, mpi_byte, mpi_character,   &
               mpi_integer4, mpi_integer8, mpi_real4, mpi_real8, mpi_integer4, &
               mpi_integer8, mpi_complex8, mpi_wtime,                          &
#if defined(PREC_32B)
               mpi_logical, mpi_integer, mpi_real, mpi_complex
#else
mpi_complex16
#endif
#endif

USE gc_kinds_mod, ONLY:                                                        &
  gc_integer32,                                                                &
  gc_integer64,                                                                &
  gc_real32,                                                                   &
  gc_real64,                                                                   &
  gc_int_kind

IMPLICIT NONE

#if defined (MPI_SRC) && defined (USE_MPIF_INCLUDE)
#include "mpif.h"
#endif

! Select the kind to use for variables to match the MPI Library
#if defined(MPILIB_32B)
INTEGER, PARAMETER :: MPL_Int_Kind  = gc_integer32
INTEGER, PARAMETER :: MPL_Log_Kind  = gc_integer32
INTEGER, PARAMETER :: MPL_Real_Kind = gc_real32
#else
INTEGER, PARAMETER :: MPL_Int_Kind  = gc_integer64
INTEGER, PARAMETER :: MPL_Log_Kind  = gc_integer64
INTEGER, PARAMETER :: MPL_Real_Kind = gc_real64
#endif


#if defined (MPI_SRC)
! Cast the parameters used in the MPI library to subtly renamed
! versions for the MPL interface.
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_comm_type_shared = mpi_comm_type_shared
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_comm_world       = mpi_comm_world
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_comm_self        = mpi_comm_self
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_comm_null        = mpi_comm_null
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_status_size      = mpi_status_size
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_tag_ub           = mpi_tag_ub
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_any_source       = mpi_any_source
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_any_tag          = mpi_any_tag
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_sum              = mpi_sum
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_min              = mpi_min
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_max              = mpi_max
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_prod             = mpi_prod
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_land             = mpi_land
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_band             = mpi_band
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_lor              = mpi_lor
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_bor              = mpi_bor
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_lxor             = mpi_lxor
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_bxor             = mpi_bxor
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_maxloc           = mpi_maxloc
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_minloc           = mpi_minloc
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_no_op            = mpi_no_op
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_replace          = mpi_replace
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_address_kind     = mpi_address_kind
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_tag              = mpi_tag
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_source           = mpi_source
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_undefined        = mpi_undefined
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_error            = mpi_error
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_packed           = mpi_packed
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_success          = mpi_success
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_info_null        = mpi_info_null
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_request_null     = mpi_request_null
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_max_error_string   = mpi_max_error_string
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_max_processor_name = mpi_max_processor_name
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_thread_multiple    = mpi_thread_multiple
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_thread_single      = mpi_thread_single
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_thread_funneled    = mpi_thread_funneled
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_thread_serialized  = mpi_thread_serialized

INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_file_null         = mpi_file_null
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_offset_kind       = mpi_offset_kind
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_seek_set          = mpi_seek_set
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_mode_rdonly       = mpi_mode_rdonly
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_mode_wronly       = mpi_mode_wronly
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_mode_rdwr         = mpi_mode_rdwr
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_mode_create       = mpi_mode_create
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_errors_are_fatal  = mpi_errors_are_fatal

INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_mode_nocheck      = mpi_mode_nocheck
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_mode_nostore      = mpi_mode_nostore
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_mode_noput        = mpi_mode_noput
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_mode_noprecede    = mpi_mode_noprecede

INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_lock_exclusive    = mpi_lock_exclusive
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_lock_shared       = mpi_lock_shared

! Set parameters for MPI types, including some set according to
! GC precision (integer, real)
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_byte       = mpi_byte
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_character  = mpi_character
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_integer4   = mpi_integer4
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_integer8   = mpi_integer8
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_real4      = mpi_real4
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_real8      = mpi_real8
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_logical4   = mpi_integer4
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_logical8   = mpi_integer8
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_complex8   = mpi_complex8

#if defined(PREC_32B)
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_logical    = mpi_logical
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_integer    = mpi_integer
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_real       = mpi_real
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_complex    = mpi_complex
#else
! mpich2 does not have MPI_LOGICAL8, so use MPI_INTEGER8
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_logical    = mpi_integer8
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_integer    = mpi_integer8
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_real       = mpi_real8
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_complex    = mpi_complex16
#endif

! Dummy version for NON-MPI GCOM
#else
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_comm_type_shared = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_comm_world       = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_comm_self        = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_comm_null        = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_status_size      = 1
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_tag_ub           = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_any_source       = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_any_tag          = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_sum              = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_min              = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_max              = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_prod             = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_land             = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_band             = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_lor              = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_bor              = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_lxor             = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_bxor             = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_maxloc           = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_minloc           = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_no_op            = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_replace          = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_address_kind     = gc_int_kind
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_tag              = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_source           = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_undefined        = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_error            = -91
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_packed           = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_success          = -90
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_info_null        = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_request_null     = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_max_error_string   = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_max_processor_name = 1
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_thread_multiple    = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_thread_single      = -98
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_thread_funneled    = -97
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_thread_serialized  = -96

INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_byte         = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_character    = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_integer4     = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_integer8     = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_real4        = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_real8        = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_logical4     = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_logical8     = -99

INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_logical      = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_integer      = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_real         = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_complex      = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_complex8     = -99

INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_file_null         = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_offset_kind       = gc_int_kind
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_seek_set          = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_mode_rdonly       = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_mode_wronly       = -98
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_mode_rdwr         = -97
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_mode_create       = -96
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_errors_are_fatal  = -99

INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_mode_nocheck      = -95
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_mode_nostore      = -94
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_mode_noput        = -93
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_mode_noprecede    = -92

INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_lock_exclusive    = -99
INTEGER (KIND=gc_int_kind), PARAMETER :: mpl_lock_shared       = -99

#endif


END MODULE mpl
