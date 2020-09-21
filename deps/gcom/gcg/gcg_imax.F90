! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

#include "gcg_prolog.h"

SUBROUTINE gcg_imax (len1, gid, istat, imax)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Calculate the integer maximum across all processors of a group
!     *  and distribute the result to all members of the group.
!     *
!     * Input:
!     *  LEN1    - number of elements in message
!     *  GID     - processor group ID
!     *  IMAX    - array with elements to be added up across the nodes
!     *
!     * Output:
!     *  IMAX    - array containing the sums across the nodes
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
#include "gcg_constants.h"

INTEGER (KIND=gc_int_kind) :: len1, gid, istat, imax(len1)

INTEGER (KIND=gc_int_kind) :: reduce_data_iwrk(len1)

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
  reduce_data_iwrk(i) = imax(i)
END DO
CALL mpl_allreduce(reduce_data_iwrk, imax, len1, mpl_integer,                  &
     mpl_max, igid, istat)
#endif

RETURN
END SUBROUTINE gcg_imax
