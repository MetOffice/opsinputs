! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"

SUBROUTINE gc_rmin_single_task (len1, nproc, istat, smin, root)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Finds the real minimum across all processors and distribute
!     *  the result to the root processor.
!     *
!     * Input:
!     *  LEN1    - number of elements in message
!     *  NPROC   - number of processors
!     *  SMIN    - array with elements of which the elementwise minimum
!     *            across the nodes is to be found
!     *  ROOT    - process to receive the answer
!     *
!     * Output:
!     *  SMIN    - array containing the minimums across the nodes
!     *  ISTAT   - status of rsum. 0 is OK (MPI_SRC only),
!     *            refer to the header files for nonzero status codes
!     *
!     * NOTES:
!     *
!     ******************************************************************

USE mpl, ONLY:                                                                 &
    mpl_real,                                                                  &
    mpl_min

#if defined(MPI_SRC)
USE gc_globals_mod, ONLY:                                                      &
    gc__my_mpi_comm_world
#endif

USE gc_kinds_mod, ONLY: gc_int_kind, gc_real_kind

IMPLICIT NONE

#include "gc_constants.h"

INTEGER (KIND=gc_int_kind), INTENT(IN) :: len1
INTEGER (KIND=gc_int_kind), INTENT(IN) :: nproc
INTEGER (KIND=gc_int_kind), INTENT(IN) :: root
INTEGER (KIND=gc_int_kind), INTENT(OUT) :: istat
REAL    (KIND=gc_real_kind), INTENT(IN OUT) :: smin(len1)

REAL (KIND=gc_real_kind)   :: reduce_data_wrk(len1)

#if defined(MPI_SRC)
INTEGER (KIND=gc_int_kind) :: i
#endif

#if defined(MPI_SRC)
DO i = 1,len1
  reduce_data_wrk(i) = smin(i)
END DO
CALL mpl_reduce(reduce_data_wrk, smin, len1, mpl_real, mpl_min,                &
     root, gc__my_mpi_comm_world, istat)
#endif

#if defined(SERIAL_SRC)
istat = gc__ok
#endif

RETURN
END SUBROUTINE gc_rmin_single_task
