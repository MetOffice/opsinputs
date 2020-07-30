! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"

SUBROUTINE gc_isum (len1, nproc, istat, isum)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Calculate the integer sum across all processors and distribute
!     *  the result to all the processors.
!     *
!     * Input:
!     *  LEN1    - number of elements in message
!     *  NPROC   - number of processors
!     *  ISUM    - array with elements to be added up across the nodes
!     *
!     * Output:
!     *  ISUM    - array containing the sums across the nodes
!     *  ISTAT   - status of isum. 0 is OK (MPI_SRC only),
!     *            refer to the header files for nonzero status codes
!     *
!     * NOTES:
!     *
!     ******************************************************************

USE mpl, ONLY:                                                                 &
    mpl_integer,                                                               &
    mpl_sum

#if defined(MPI_SRC)
USE gc_globals_mod, ONLY:                                                      &
    gc__my_mpi_comm_world
#endif

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

#include "gc_constants.h"

INTEGER (KIND=gc_int_kind) :: len1, nproc, istat, isum(len1)
INTEGER (KIND=gc_int_kind) :: reduce_data_iwrk(len1)

#if defined(MPI_SRC)
INTEGER (KIND=gc_int_kind) :: i
#endif

#if defined(MPI_SRC)
DO i = 1,len1
  reduce_data_iwrk(i) = isum(i)
END DO
CALL mpl_allreduce(reduce_data_iwrk, isum, len1, mpl_integer,                  &
     mpl_sum, gc__my_mpi_comm_world, istat)

#endif

#if defined(SERIAL_SRC)
istat = gc__ok
#endif

RETURN
END SUBROUTINE gc_isum
