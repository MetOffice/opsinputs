! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

#include "gcg_prolog.h"

SUBROUTINE gcg_rvecsumr(lvl, lsl, lso, nv, field, gid,                         &
                        istat, sums)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Calculate in a reproducible way the real sum of a set of
!     *  vectors across all members of a group, and distribute the
!     *  results to all members of the group.
!     *
!     * Input:
!     *  LVL     - Local Vector Length
!     *  LSL     - Local Sum Length (the length of the subsection to be
!     *            summed for each vector)
!     *  LSO     - Local Sum Offset (element where the summation start)
!     *  NV      - Number of Vectors
!     *  FIELD   - local array containing the vectors to be summed
!     *  GID     - processor group ID
!     *
!     * Output:
!     *  SUMS    - array containing the sums across the nodes
!     *  ISTAT   - status of rsum. 0 is OK (MPI_SRC only),
!     *            refer to the header files for nonzero status codes
!     *
!     * NOTES:
!     *
!     ******************************************************************

USE mpl, ONLY:                                                                 &
    mpl_status_size,                                                           &
    mpl_byte

#if defined(MPI_SRC)
USE gc_globals_mod, ONLY:                                                      &
    gc__my_mpi_comm_world

USE gc__buildconst, ONLY: gc__rsize
#endif

USE gc_kinds_mod, ONLY: gc_int_kind, gc_real_kind

IMPLICIT NONE

#include "gc_constants.h"
#include "gcg_constants.h"
#include "gcg_mtags.h"

INTEGER (KIND=gc_int_kind) :: lvl, lsl, lso, nv, gid, istat
REAL (KIND=gc_real_kind)   :: field(lvl,nv), sums(nv)

#if defined(MPI_SRC)
INTEGER (KIND=gc_int_kind) :: STAT(mpl_status_size)
#endif

INTEGER (KIND=gc_int_kind) :: i, j, me, grank, gsize, igid, iloc
INTEGER (KIND=gc_int_kind) :: g0, gprev, gnext, glast

istat = gc__ok

!---  Set all sums to zero
DO j = 1, nv
  sums(j) = 0.0
END DO

#if defined(MPI_SRC)
IF (gid  ==  gcg__allgroup) THEN
  igid = gc__my_mpi_comm_world
ELSE
  igid = gid
END IF

CALL mpl_comm_rank(igid, grank, istat)
CALL mpl_comm_size(igid, gsize, istat)
glast = gsize - 1
gprev = grank - 1
gnext = grank + 1
#endif

#if defined(SERIAL_SRC)
DO j = 1, nv
  DO i = lso, lsl+lso-1
    sums(j) = sums(j) + field(i,j)
  END DO
END DO
#else

!---  Perform the reproducible global sums for this GID. The first
!     group member (GRANK = 0) starts. He sums his elements, pass on
!     the partial results to the next member, and so on until the last
!     member, which broadcast the sums after adding his contributions.
IF (grank  /=  0) THEN
  CALL mpl_recv(sums, gc__rsize*nv, mpl_byte, gprev,                           &
                gcgid__vec0, igid, STAT, istat)
END IF

DO j = 1, nv
  !cdir novector
  DO i = lso, lsl+lso-1
    sums(j) = sums(j) + field(i,j)
  END DO
END DO

IF (grank  <   gsize-1) THEN
  CALL mpl_send(sums, gc__rsize*nv, mpl_byte, gnext,                           &
                gcgid__vec0, igid, istat)
END IF

IF (gsize  >   1) THEN
  CALL mpl_bcast(sums, gc__rsize*nv, mpl_byte, glast,                          &
                 igid, istat)
END IF
#endif

RETURN
END SUBROUTINE gcg_rvecsumr
