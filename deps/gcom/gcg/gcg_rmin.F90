! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

#include "gcg_prolog.h"

SUBROUTINE gcg_rmin (len1, gid, istat, smin)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Calculate the real minimum across all processors of a group and
!     *  distribute the result to all members of the group.
!     *
!     * Input:
!     *  LEN1     - number of elements in message
!     *  GID     - processor group ID
!     *  SMIN    - array with elements to be added up across the nodes
!     *
!     * Output:
!     *  SMIN    - array containing the sums across the nodes
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
#include "gcg_constants.h"

INTEGER (KIND=gc_int_kind) :: len1, gid, istat
REAL (KIND=gc_real_kind)   :: smin(len1)

REAL (KIND=gc_real_kind)   :: reduce_data_wrk(len1)

#if defined(MPI_SRC)
INTEGER (KIND=gc_int_kind) :: igid
#endif
INTEGER (KIND=gc_int_kind) :: i

istat = gc__ok

#if defined(MPI_SRC)
IF (gid  ==  gcg__allgroup) THEN
  igid = gc__my_mpi_comm_world
ELSE
  igid = gid
END IF
DO i = 1,len1
  reduce_data_wrk(i) = smin(i)
END DO
CALL mpl_allreduce(reduce_data_wrk, smin, len1, mpl_real, mpl_min,             &
     igid, istat)
#endif

RETURN
END SUBROUTINE gcg_rmin
