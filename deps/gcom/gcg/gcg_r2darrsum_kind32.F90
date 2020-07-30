! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

#include "gcg_prolog.h"

SUBROUTINE gcg_r2darrsum_kind32(field, row_length, rows, off_x, off_y, levels, &
                                gid, sum2d, istat)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Calculate in a reproducible way the real sum of an array of
!     *  2d arrays (with halos) across all processors of a group and
!     *  distribute the result vector to all members  of the group.
!     *
!     * Input:
!     *  FIELD   - the field to be summed
!     *  ROW_LENGTH - the x dimension of field (without halos)
!     *  ROWS - the y dimension of field (without halos)
!     *  OFF_X - the x dimension halo size
!     *  OFF_Y - the y dimension halo size
!     *  LEVELS - the number of 2d fields provided
!     *  GID     - processor group ID
!     *
!     * Output:
!     *  SUM2D - the resultant sums
!     *  ISTAT - error code
!     *
!     * NOTES: For MPI this will use He and Ding algorithm
!     *        (Journal of Supercomputing, 18, pp259-277, 2001)
!     *        using double-double arithmetic and self-compensating
!     *        summation.
!     *
!     ******************************************************************


USE mpl, ONLY:                                                                 &
    mpl_complex8,                                                              &
    mpl_int_kind

USE gc_kinds_mod, ONLY: gc_int_kind, gc_real32, gc_log_kind

IMPLICIT NONE

#include "gc_constants.h"

! Arguments
INTEGER (KIND=gc_int_kind) :: rows           ! number of rows
INTEGER (KIND=gc_int_kind) :: row_length     ! length of row
INTEGER (KIND=gc_int_kind) :: off_x          ! x dirn halo size
INTEGER (KIND=gc_int_kind) :: off_y          ! y dirn halo size
INTEGER (KIND=gc_int_kind) :: levels         ! number of 2d fields
INTEGER (KIND=gc_int_kind) :: istat          ! error code
INTEGER (KIND=gc_int_kind) :: gid            ! group id

REAL (KIND=gc_real32)      :: field(1-off_x:row_length+off_x,                  &
                                    1-off_y: rows+off_y,                       &
                                    levels)    ! Data to sum
REAL (KIND=gc_real32)      :: sum2d(levels)  ! sum results

! Local variables
INTEGER (KIND=gc_int_kind)  :: i, j, k       ! loopers
INTEGER (KIND=gc_int_kind)  :: my_comm       ! communicator
INTEGER (KIND=mpl_int_kind) :: itype         ! loopback argument
COMPLEX (KIND=gc_real32)    :: local_sum(levels)  ! local partial sum
COMPLEX (KIND=gc_real32)    :: global_sum(levels) ! global sum
REAL    (KIND=gc_real32) :: e             ! error term
REAL    (KIND=gc_real32) :: t1            ! temporary
REAL    (KIND=gc_real32) :: t2            ! temporary
INTEGER (KIND=gc_int_kind), SAVE  :: mpl_sumds       ! MPI operator
LOGICAL (KIND=gc_log_kind), SAVE  :: first =.TRUE.   ! First call to routine?
EXTERNAL :: dspds                                    ! Callback routine

istat = gc__ok

#if defined(MPI_SRC)
IF (first) THEN
  ! On first call to routine setup the MPI double-double summation operator
  CALL mpl_op_create( dspds, .TRUE., mpl_sumds, istat)
  first = .FALSE.
END IF
#endif

!$OMP  PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC) PRIVATE(k,j,i,t1,t2,e)       &
!$OMP  SHARED(levels,local_sum,rows,row_length,field)
DO k = 1, levels

  local_sum(k) = CMPLX(0.0_gc_real32, 0.0_gc_real32, KIND=gc_real32)

  DO j = 1, rows
    DO i = 1, row_length
      ! Calculate sum using Knuth's trick inline (rather than via routine
      ! dspds) for better performance.
      t1 = field(i,j,k) + REAL(local_sum(k), KIND=gc_real32)
      e  = t1 - field(i,j,k)
      t2 = ((REAL(local_sum(k), KIND=gc_real32) - e) +                         &
            (field(i,j,k) - (t1 - e)))  +                                      &
             AIMAG(local_sum(k))

      ! Result is t1+t2, after normalisation
      local_sum(k) = CMPLX(t1 + t2, t2 - ((t1+t2) - t1), KIND=gc_real32)
    END DO
  END DO
END DO
!$OMP END PARALLEL DO

#if defined(MPI_SRC)
CALL mpl_allreduce(local_sum, global_sum, levels, mpl_complex8, mpl_sumds,     &
                   gid,  istat)

sum2d(:) = REAL(global_sum(:), KIND=gc_real32)
#endif

#if defined(SERIAL_SRC)
sum2d(:) = REAL(local_sum(:), KIND=gc_real32)
#endif


END SUBROUTINE gcg_r2darrsum_kind32

!---------------------------------------------------------------------------

SUBROUTINE dspds( dda, ddb, len1, itype)
! This subroutine calculates ddb(i) = dda(i) + ddb(i) for i = 1..len
! Based on original codes by David H. Bailey

USE mpl, ONLY:                                                                 &
    mpl_int_kind

USE gc_kinds_mod, ONLY: gc_int_kind, gc_real32

IMPLICIT NONE

! Note these variables need to be MPL types as they are in a callback
! from MPI internal routines.
! Arguments
INTEGER (KIND=mpl_int_kind) :: len1       ! vector length
INTEGER (KIND=mpl_int_kind) :: itype      ! dummy

COMPLEX (KIND=gc_real32)    :: dda(len1)  ! input array
COMPLEX (KIND=gc_real32)    :: ddb(len1)  ! input/output array

! Internal variables
INTEGER (KIND=gc_int_kind) :: i         ! looper
REAL    (KIND=gc_real32)   :: e         ! error term
REAL    (KIND=gc_real32)   :: t1        ! temporary
REAL    (KIND=gc_real32)   :: t2        ! temporary

!$OMP  PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC) PRIVATE(i,t1,t2,e)           &
!$OMP  SHARED(len1,dda,ddb)
DO i = 1, len1
  ! Calculate dda+ddb using Knuth's trick
  t1 = REAL(dda(i), KIND=gc_real32) + REAL(ddb(i), KIND=gc_real32)
  e  = t1 - REAL(dda(i), KIND=gc_real32)
  t2 = ((REAL(ddb(i), KIND=gc_real32) - e) +                                   &
        (REAL(dda(i), KIND=gc_real32) - (t1 - e)))  +                          &
         AIMAG(dda(i)) + AIMAG(ddb(i))

  ! Result is t1+t2, after normalisation
  ddb(i) = CMPLX(t1 + t2, t2 - ((t1+t2) - t1), KIND=gc_real32)
END DO
!$OMP END PARALLEL DO

END SUBROUTINE dspds
