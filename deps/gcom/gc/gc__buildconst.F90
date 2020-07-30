! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

MODULE gc__buildconst

#if defined(PREC_32B)
#undef PREC_64B
#else
#if !defined(PREC_64B)
#define PREC_64B
#endif
#endif

#if defined(PREC_32B)
! Using 32bit INTEGERs & REALs
#define GC_INT_TYPE "32bit INTEGERs"
#define GC_REAL_TYPE "32bit REALs"
#else
! Using 64bit INTEGERs & REALs
#define GC_INT_TYPE "64bit INTEGERs"
#define GC_REAL_TYPE "64bit REALs"
#endif

#define gc_version_str() GC_VERSION
#define gc_build_date_str() GC_BUILD_DATE
#define gc_int_type_str() GC_INT_TYPE
#define gc_real_type_str() GC_REAL_TYPE
#define gc_descrip_str() GC_DESCRIP

#if !defined(GC__FORTERRUNIT)
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT
#define gc__forterrunit_val() error_unit
! Error messages are written to iso_fortran_env error_unit
#else
#define gc__forterrunit_val() GC__FORTERRUNIT
! Error messages are written to unit GC__FORTERRUNIT
#endif

#if defined(MPI_SRC)

#if !defined(MPIABORT_ERRNO)
#define mpiabort_errno_val() 9
! GC_ABORT will cause exit code 9 to be output
#else
#define mpiabort_errno_val() MPIABORT_ERRNO
! GC_ABORT will cause exit code MPIABORT_ERRNO to be output
#endif

!   GCOM dimensioning. The user may change this by preprocessor
!   directives on the command line when compiling GCOM. The
!   default setting corresponds to:
!    -DMPI_BSEND_BUFFER_SIZE=160000
#if !defined(MPI_BSEND_BUFFER_SIZE)
#define mpi_bsend_buffer_size_val() 160000
! Size of MPI Send buffer is 160000
#else
#define mpi_bsend_buffer_size_val() MPI_BSEND_BUFFER_SIZE
! Size of MPI Send buffer is MPI_BSEND_BUFFER_SIZE
#endif

#endif

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE
PRIVATE

PUBLIC ::                                                                      &
#if defined(MPI_SRC)
  mpiabort_errno, mpi_bsend_buffer_size,                                       &
#endif
  gc_version, gc_build_date, gc_int_type, gc_real_type, gc_descrip, gc__isize, &
  gc__rsize, gc__forterrunit

CHARACTER(LEN=*), PARAMETER ::                                                 &
  gc_version = gc_version_str(),                                               &
  gc_build_date = gc_build_date_str(),                                         &
  gc_int_type = gc_int_type_str(),                                             &
  gc_real_type = gc_real_type_str(),                                           &
  gc_descrip = gc_descrip_str()

#if defined(PREC_32B)
INTEGER(KIND=gc_int_kind), PARAMETER :: gc__isize = 4
INTEGER(KIND=gc_int_kind), PARAMETER :: gc__rsize = 4
#else
INTEGER(KIND=gc_int_kind), PARAMETER :: gc__isize = 8
INTEGER(KIND=gc_int_kind), PARAMETER :: gc__rsize = 8
#endif

INTEGER(KIND=gc_int_kind), PARAMETER :: gc__forterrunit = gc__forterrunit_val()

#if defined(MPI_SRC)
INTEGER(KIND=gc_int_kind), PARAMETER :: mpiabort_errno = mpiabort_errno_val()
INTEGER(KIND=gc_int_kind), PARAMETER ::                                        &
  mpi_bsend_buffer_size = mpi_bsend_buffer_size_val()
#endif

END MODULE gc__buildconst
