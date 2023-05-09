! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"

SUBROUTINE gc_imax_single_task (len1, nproc, istat, imax, root)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Finds the integer maximum across all processors and distribute
!     *  the result to the root processor.
!     *
!     * Input:
!     *  LEN1    - number of elements in message
!     *  NPROC   - number of processors
!     *  IMAX    - array with elements of which the elementwise maximum
!     *            across the nodes is to be found
!     *  ROOT    - process to receive the answer
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

INTEGER (KIND=gc_int_kind), INTENT(IN) :: len1
INTEGER (KIND=gc_int_kind), INTENT(IN) :: nproc
INTEGER (KIND=gc_int_kind), INTENT(IN) :: root
INTEGER (KIND=gc_int_kind), INTENT(OUT) :: istat
INTEGER (KIND=gc_int_kind), INTENT(IN OUT) :: imax(len1)

INTEGER (KIND=gc_int_kind) :: reduce_data_iwrk(len1)

#if defined(MPI_SRC)
INTEGER (KIND=gc_int_kind) :: i
#endif

#if defined(MPI_SRC)
DO i = 1,len1
  reduce_data_iwrk(i) = imax(i)
END DO
CALL mpl_reduce(reduce_data_iwrk, imax, len1, mpl_integer,                     &
     mpl_max, root, gc__my_mpi_comm_world, istat)

#endif

#if defined(SERIAL_SRC)
istat = gc__ok
#endif

RETURN
END SUBROUTINE gc_imax_single_task
