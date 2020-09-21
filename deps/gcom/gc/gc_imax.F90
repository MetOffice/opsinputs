! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"

SUBROUTINE gc_imax (len1, nproc, istat, imax)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Finds the integer maximum across all processors and distribute
!     *  the result to all the processors.
!     *
!     * Input:
!     *  LEN1     - number of elements in message
!     *  NPROC   - number of processors
!     *  IMAX    - array with elements of which the elementwise maximum
!     *            across the nodes is to be found
!     *
!     * Output:
!     *  IMAX    - array containing the maximums across the nodes
!     *  ISTAT   - status of rsum. 0 is OK (MPI_SRC only),
!     *            refer to the header files for nonzero status codes
!     *
!     * NOTES:
!     *
!     ******************************************************************

USE mpl, ONLY:                                                                 &
    mpl_integer,                                                               &
    mpl_max

#if defined(MPI_SRC)
USE gc_globals_mod, ONLY:                                                      &
    gc__my_mpi_comm_world
#endif

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

#include "gc_constants.h"

INTEGER (KIND=gc_int_kind) :: len1, nproc, istat, imax(len1)
INTEGER (KIND=gc_int_kind) :: reduce_data_iwrk(len1)

#if defined(MPI_SRC)
INTEGER (KIND=gc_int_kind) :: i
#endif

#if defined(MPI_SRC)
DO i = 1,len1
  reduce_data_iwrk(i) = imax(i)
END DO
CALL mpl_allreduce(reduce_data_iwrk, imax, len1, mpl_integer,                  &
     mpl_max, gc__my_mpi_comm_world, istat)

#endif

#if defined(SERIAL_SRC)
istat = gc__ok
#endif

RETURN
END SUBROUTINE gc_imax
