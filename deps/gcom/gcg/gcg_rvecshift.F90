! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

#include "gcg_prolog.h"

SUBROUTINE gcg_rvecshift (lvl, lsl, lso, nv, shft, wrap, field,                &
     gid, istat)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Shift (rotate) the elements in a set of vectors distributed
!     *  across all members of a group.
!     *
!     * Input:
!     *  LVL     - Local Vector Length
!     *  LSL     - Local Shift Length (the length of the subsection to
!     *            be shifted for each vector)
!     *  LSO     - Local Shift Offset (element where the summation
!     *            start)
!     *  NV      - Number of Vectors
!     *  SHFT    - Number of Shifts to be done
!     *  WRAP    - Logical indicating whether the vectors should be
!     *            wrapped around on shifts
!     *  FIELD   - local array containing the vectors to be shifted
!     *  GID     - processor group ID
!     *
!     * Output:
!     *  FIELD   - Local array containing the shifted data
!     *  ISTAT   - status of rsum. 0 is OK (MPI_SRC only),
!     *            refer to the header files for nonzero status codes
!     *
!     * NOTES:
!     *
!     ******************************************************************

USE mpl, ONLY:                                                                 &
    mpl_status_size,                                                           &
    mpl_byte

USE gc_globals_mod, ONLY:                                                      &
    max_rotate

#if defined(MPI_SRC)
USE gc_globals_mod, ONLY:                                                      &
    gc__my_mpi_comm_world

USE gc__buildconst, ONLY: gc__rsize, gc__isize
#endif

USE gc_kinds_mod, ONLY: gc_int_kind, gc_real_kind

IMPLICIT NONE

#include "gc_constants.h"
#include "gcg_constants.h"
#include "gcg_mtags.h"


INTEGER (KIND=gc_int_kind)  :: lvl, lsl, lso, nv, shft, gid, istat
REAL (KIND=gc_real_kind)    :: field(lvl,nv)
LOGICAL (KIND=gc_int_kind)  :: wrap

#if defined(MPI_SRC)
INTEGER (KIND=gc_int_kind)  :: STAT(mpl_status_size)
#endif
INTEGER (KIND=gc_int_kind)  :: i, j, k, me, grank, gsize, grank0,              &
                               igid, iloc, gj
INTEGER (KIND=gc_int_kind)  :: ldlen(1_gc_int_kind)
INTEGER (KIND=gc_int_kind)  :: lst(2)
INTEGER (KIND=gc_int_kind), ALLOCATABLE  :: glst(:,:)
REAL    (KIND=gc_real_kind) :: garr(max_rotate)
REAL    (KIND=gc_real_kind) :: harr(max_rotate)

istat = gc__ok

!---  Return if no shifts
IF (shft  ==  0) RETURN

#if defined(MPI_SRC)
IF (gid  ==  gcg__allgroup) THEN
  igid = gc__my_mpi_comm_world
ELSE
  igid = gid
END IF
CALL mpl_comm_rank(igid, grank, istat)
CALL mpl_comm_size(igid, gsize, istat)
ALLOCATE( glst(2,0:gsize-1) )
grank0 = 0
#endif


!---  Check if one or more processors in the group is asked to shift
!     more elements than it has
ldlen(1) = lvl - (lso + lsl - 1)
CALL gcg_imin(1_gc_int_kind, gid, istat, ldlen)
IF (ldlen(1) < 0 .OR. istat < 0) THEN
  istat = -1
  RETURN
END IF

#if defined(SERIAL_SRC)
!---  Loop over all vectors
DO k = 1,nv

  !---  Rotate by copying into a vector, copying this to another vector
  !     and copying back (!) Can obviously be simplyfied, but how often
  !     is this operation done in serial mode?
  DO j = 1, lsl
    garr(j) = field(lso+j-1,k)
  END DO

  IF (shft  >   0) THEN
    DO i = 1, lsl-shft
      harr(i+shft) = garr(i)
    END DO
    IF (wrap) THEN
      DO i = 1, shft
        harr(i) = garr(lsl-shft+i)
      END DO
    ELSE
      DO i = 1, shft
        harr(i) = garr(i)
      END DO
    END IF
  ELSE
    DO i = 1-shft, lsl
      harr(i+shft) = garr(i)
    END DO
    IF (wrap) THEN
      DO i = 0,-1-shft
        harr(lsl-i) = garr(-i-shft)
      END DO
    ELSE
      DO i = 0,-1-shft
        harr(lsl-i) = garr(lsl-i)
      END DO
    END IF
  END IF

  DO j = 1, lsl
    field(lso+j-1,k) = harr(j)
  END DO

END DO
#else

!---  Send the shift length and offset of all processors to the first
!     processor in the group.
lst(1) = lsl
lst(2) = lso
IF (grank  /=  0) THEN
  CALL mpl_bsend(lst, gc__isize*2, mpl_byte, grank0,                           &
       gcgid__rot0, igid, istat)
ELSE
  DO i = 1, gsize-1
    CALL mpl_recv(glst(1,i), gc__isize*2, mpl_byte,                            &
                  i, gcgid__rot0,                                              &
                  igid, STAT, istat)
  END DO

  !---  Exit if total length of the vectors is greater than MAX_ROTATE

  glst(1,0) = lsl
  glst(2,0) = lso
  gj = 0
  DO i = 0,gsize-1
    gj = gj + glst(1,i)
  END DO
  IF (gj  >   max_rotate)                                                      &
     CALL gcg__errlim(1_gc_int_kind, 'RVECSHIFT',                              &
                      'ROTATE', max_rotate, gj)

END IF

!---  Loop over all vectors
DO k = 1,nv

  !---  Send to first processor in the group, which do the rotate
  !     and distribute back again
  IF (grank  /=  0) THEN
    CALL mpl_bsend(field(lso,k), gc__rsize*lsl,                                &
                   mpl_byte, grank0,                                           &
                   gcgid__rot1, igid, istat)
    CALL mpl_recv(field(lso,k), gc__rsize*lsl,                                 &
                  mpl_byte, grank0,                                            &
                  gcgid__rot2, igid, STAT, istat)

  ELSE

    DO j = 1, lsl
      garr(j) = field(lso+j-1,k)
    END DO
    gj = lsl

    DO  i = 1, gsize-1
      CALL mpl_recv(garr(gj+1), gc__rsize*glst(1,i),                           &
                    mpl_byte, i, gcgid__rot1,                                  &
                    igid, STAT, istat)
      gj = gj + glst(1,i)
    END DO

    IF (shft  >   0) THEN
      IF (wrap) THEN
        DO i = 1, shft
          harr(i) = garr(gj-shft+i)
        END DO
      ELSE
        DO i = 1, shft
          harr(i) = garr(i)
        END DO
      END IF
      DO i = 1, gj-shft
        harr(i+shft) = garr(i)
      END DO
    ELSE
      DO i = 1-shft, gj
        harr(i+shft) = garr(i)
      END DO
      IF (wrap) THEN
        DO i = 1,-shft
          harr(gj+shft+i) = garr(i)
        END DO
      ELSE
        DO i = 1,-shft
          harr(gj+shft+i) = garr(gj+shft+i)
        END DO
      END IF
    END IF

    DO j = 1, lsl
      field(lso+j-1,k) = harr(j)
    END DO
    gj = lsl

    DO i = 1, gsize-1
      CALL mpl_bsend(harr(gj+1), gc__rsize*glst(1,i),                          &
                     mpl_byte, i, gcgid__rot2, igid, istat)
      gj = gj + glst(1,i)
    END DO

  END IF
END DO

DEALLOCATE( glst )
#endif

RETURN
END SUBROUTINE gcg_rvecshift
