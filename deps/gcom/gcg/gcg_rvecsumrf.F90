! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

#include "gcg_prolog.h"

SUBROUTINE gcg_rvecsumrf(gvl, lvl, lsl, lso, nv, field, gid,                   &
                        istat, sums)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Calculate in a reproducible way the real sum of a set of
!     *  vectors across all members of a group, and distribute the
!     *  results to all members of the group.
!     *
!     * Input:
!     *  GVL       Global vector length
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
    mpl_byte,                                                                  &
    mpl_real, mpl_max, mpl_sum, mpl_integer

#if defined(MPI_SRC)
USE gc_globals_mod, ONLY:                                                      &
    gc__my_mpi_comm_world
#endif

USE gc_kinds_mod, ONLY: gc_int_kind, gc_real_kind

IMPLICIT NONE

#include "gc_constants.h"
#include "gcg_constants.h"

INTEGER (KIND=gc_int_kind) :: gvl, lvl, lsl, lso, nv, gid, istat
REAL (KIND=gc_real_kind)   :: field(lvl,nv), sums(nv)

INTEGER (KIND=gc_int_kind) :: i, j, me, grank, gsize, igid, iloc
INTEGER (KIND=gc_int_kind) :: lslt
INTEGER (KIND=gc_int_kind) :: zero_count
INTEGER (KIND=gc_int_kind) :: isum(nv)
INTEGER (KIND=gc_int_kind) :: jsum(nv)
REAL (KIND=gc_real_kind) :: tmax(nv)
REAL (KIND=gc_real_kind) :: ttmax(nv)
REAL (KIND=gc_real_kind) :: fac(nv)

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
#endif

#if defined(SERIAL_SRC)
DO j = 1, nv
  DO i = lso, lsl+lso-1
    sums(j) = sums(j) + field(i,j)
  END DO
END DO
#else
! Algorithm obtains a reproducible sum by doing it in fixed
! precision (integer) arithmetic. This loses accuracy so needs
! to be used with care.

! Fist find the maximum data size over all processors
tmax(:) = 0
DO j=1,nv
  DO i=lso,lsl+lso-1
    tmax(j)=MAX(tmax(j),ABS(field(i,j)))
  END DO
END DO

lslt = gvl
CALL mpl_allreduce(tmax, ttmax, nv, mpl_real, mpl_max,                         &
                   igid, istat)

! And find scaling factors
DO j=1,nv
  fac(j)=(ttmax(j)*lslt)/(2.0**62)
END DO

! Sum and convert to integer in 1 dimension in one go
zero_count=0
isum(:)=0
DO j=1,nv
  IF (fac(j) /= 0.0) THEN
    DO i=lso,lsl+lso-1
      isum(j)=isum(j)+INT(field(i,j)*(1.0/fac(j)))
    END DO
  ELSE
    zero_count=zero_count+1
  END IF
END DO

! If field isn't entirely zero, do the summation across processors
! and convert back to real.
IF (zero_count /= nv) THEN
  CALL mpl_allreduce(isum, jsum, nv, mpl_integer, mpl_sum,                     &
                     igid, istat)
  DO j=1,nv
    sums(j)=jsum(j)*fac(j)
  END DO

ELSE

  DO j=1,nv
    sums(j)=0.0
  END DO

END IF
#endif

RETURN
END SUBROUTINE gcg_rvecsumrf
