! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"

SUBROUTINE gc_rsum (len1, nproc, istat, ssum)
!     ******************************************************************
!     * Purpose:
!     *  Calculate the real sum across all processors and distribute
!     *  the result to all the processors.
!     *
!     * Input:
!     *  LEN1    - number of elements in message
!     *  NPROC   - number of processors
!     *  SSUM    - array with elements to be added up across the nodes
!     *
!     * Output:
!     *  SSUM    - array containing the sums across the nodes
!     *  ISTAT   - status of rsum. 0 is OK (MPI_SRC only),
!     *            refer to the header files for nonzero status codes
!     *
!     * NOTES:
!     *
!     ******************************************************************

USE mpl, ONLY:                                                                 &
    mpl_real,                                                                  &
    mpl_sum

#if defined(MPI_SRC)
USE gc_globals_mod, ONLY:                                                      &
    gc__my_mpi_comm_world

USE gcom_mod, ONLY:                                                            &
    gc_force_bitrep,                                                           &
    gc_on
#endif

USE gc_kinds_mod, ONLY: gc_int_kind, gc_real_kind

IMPLICIT NONE

#include "gc_constants.h"

INTEGER (KIND=gc_int_kind) :: len1, nproc, istat
INTEGER (KIND=gc_int_kind) :: opt
REAL (KIND=gc_real_kind)   :: ssum(len1)
REAL (KIND=gc_real_kind)   :: reduce_data_wrk(len1)

#if defined(MPI_SRC)
INTEGER (KIND=gc_int_kind) :: i
#endif

#if defined(MPI_SRC)
CALL gc_getopt(gc_force_bitrep, opt, istat)
IF (opt == gc_on) THEN
  CALL gc_rsumr(len1, nproc, istat, ssum)
ELSE
  DO i = 1,len1
    reduce_data_wrk(i) = ssum(i)
  END DO
  CALL mpl_allreduce(reduce_data_wrk, ssum, len1, mpl_real,                    &
       mpl_sum, gc__my_mpi_comm_world, istat)
END IF
#endif

#if defined(SERIAL_SRC)
istat = gc__ok
#endif

RETURN
END SUBROUTINE gc_rsum
