! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

#include "gcg_prolog.h"

SUBROUTINE gcg_rvecsumf(lvl, lsl, lso, nv, field, gid,                         &
           istat, sums)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Calculate fast, but not necessarily reproducible, the real sum
!     *  of a set of vectors across all members of a group, and
!     *  distribute the results to all members of the group.
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

USE gcom_mod, ONLY:                                                            &
    gc_on,                                                                     &
    gc_force_bitrep

USE gc_kinds_mod, ONLY: gc_int_kind, gc_real_kind

IMPLICIT NONE

#include "gc_constants.h"
#include "gcg_constants.h"

INTEGER (KIND=gc_int_kind) :: lvl, lsl, lso, nv, gid, istat
INTEGER (KIND=gc_int_kind) :: opt
REAL (KIND=gc_real_kind)   :: field(lvl,nv), sums(nv)

INTEGER (KIND=gc_int_kind) :: i, j

CALL gc_getopt(gc_force_bitrep, opt, istat)
IF (opt == gc_on) THEN
  CALL gcg_rvecsumr(lvl, lsl, lso, nv, field, gid,                             &
                    istat, sums)
ELSE
  istat = gc__ok

  !---  Set all sums to zero
  DO j = 1, nv
    sums(j) = 0.0
  END DO

  !---  Not necessary to verify GID in this routine, as this is done
  !     in GCG_RSUM


#if defined(SERIAL_SRC)
  DO j = 1, nv
    DO i = lso, lsl+lso-1
      sums(j) = sums(j) + field(i,j)
    END DO
  END DO
#else

  !---  Find my sums
  DO j = 1, nv
    DO i = lso, lsl+lso-1
      sums(j) = sums(j) + field(i,j)
    END DO
  END DO

  !---  Call RSUM to do the global sums
  CALL gcg_rsum(nv, gid, istat, sums)

#endif

END IF

RETURN
END SUBROUTINE gcg_rvecsumf
