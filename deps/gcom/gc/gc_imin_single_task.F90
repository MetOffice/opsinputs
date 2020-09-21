! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"

SUBROUTINE gc_imin_single_task (len1, nproc, istat, imin, root)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Finds the real minimum across all processors and distribute
!     *  the result to the root processor.
!     *
!     * Input:
!     *  LEN1    - number of elements in message
!     *  NPROC   - number of processors
!     *  IMIN    - array with elements of which the elementwise minimum
!     *            across the nodes is to be found
!     *  ROOT    - process to receive the answer
!     *
!     * Output:
!     *  IMIN    - array containing the minimums across the nodes
!     *  ISTAT   - status of rsum. 0 is OK (MPI_SRC only),
!     *            refer to the header files for nonzero status codes
!     *
!     * NOTES:
!     *
!     ******************************************************************

USE mpl, ONLY:                                                                 &
    mpl_integer,                                                               &
    mpl_min

#if defined(MPI_SRC)
USE gc_globals_mod, ONLY:                                                      &
    gc__my_mpi_comm_world
#endif

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

#include "gc_constants.h"

INTEGER (KIND=gc_int_kind), INTENT(IN) :: len1
INTEGER (KIND=gc_int_kind), INTENT(IN) :: nproc
INTEGER (KIND=gc_int_kind), INTENT(IN) :: root
INTEGER (KIND=gc_int_kind), INTENT(OUT) :: istat
INTEGER (KIND=gc_int_kind), INTENT(IN OUT) :: imin(len1)

INTEGER (KIND=gc_int_kind) :: reduce_data_iwrk(len1)

#if defined(MPI_SRC)
INTEGER (KIND=gc_int_kind) :: i
#endif

#if defined(MPI_SRC)
DO i = 1,len1
  reduce_data_iwrk(i) = imin(i)
END DO
CALL mpl_reduce(reduce_data_iwrk, imin, len1, mpl_integer,                     &
     mpl_min, root, gc__my_mpi_comm_world, istat)

#endif

#if defined(SERIAL_SRC)
istat = gc__ok
#endif

RETURN
END SUBROUTINE gc_imin_single_task
