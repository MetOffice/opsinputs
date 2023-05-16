! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

#include "gcg_prolog.h"

SUBROUTINE gcg_zsum (len1, gid, istat, ssum)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Calculate the complex sum across all processors of a group and
!     *  distribute the result to all members of the group.
!     *
!     * Input:
!     *  LEN1    - number of elements in message
!     *  GID     - processor group ID
!     *  SSUM    - array with elements to be added up across the nodes
!     *
!     * Output:
!     *  SSUM    - array containing the sums across the nodes
!     *  ISTAT   - status of zsum. 0 is OK (MPI_SRC only),
!     *            refer to the header files for nonzero status codes
!     *
!     * NOTES:
!     *
!     ******************************************************************

USE mpl, ONLY:                                                                 &
    mpl_complex,                                                               &
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
#include "gcg_constants.h"

INTEGER (KIND=gc_int_kind) :: len1, gid, istat
INTEGER (KIND=gc_int_kind) :: opt
COMPLEX (KIND=gc_real_kind) :: ssum(len1)

COMPLEX (KIND=gc_real_kind) :: reduce_data_wrk(len1)

#if defined(MPI_SRC)
INTEGER (KIND=gc_int_kind) :: igid
#endif

INTEGER (KIND=gc_int_kind) :: i

istat = gc__ok

#if defined(MPI_SRC)
CALL gc_getopt(gc_force_bitrep, opt, istat)
IF (opt == gc_on) THEN
  CALL gcg_zsumr(len1, gid, istat, ssum)
ELSE
  IF (gid  ==  gcg__allgroup) THEN
    igid = gc__my_mpi_comm_world
  ELSE
    igid = gid
  END IF
  DO i = 1,len1
    reduce_data_wrk(i) = ssum(i)
  END DO
  CALL mpl_allreduce(reduce_data_wrk, ssum, len1, mpl_complex,                 &
       mpl_sum, igid, istat)
END IF
#endif

RETURN
END SUBROUTINE gcg_zsum
