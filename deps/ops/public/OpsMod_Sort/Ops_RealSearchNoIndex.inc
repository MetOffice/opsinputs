!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Searches the monotonic real array y for the integer value x
!
! Method:
!   The upper and lower limits are conditionally incremented by a
!   step which halves between iterations.
!   The increment is only applied if the new position for the lower
!   or upper is acceptable.
!
! Inputs:
!   y: a real array
!   x: a real to be located in y
!
! Inputs/Outputs:
!   lower, upper:
!   on entry these are the limits of the range of the index to be searched.
!   on exit they are the limits of the range having the value x.
!   If only element of the correct value exists then lower=upper.
!   If the value is not found, then upper points to the largest value
!   below x and lower points to the smallest value above x.
!   Hence lower is one greater than upper.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_RealSearchNoIndex (x,     &
                                  y,     &
                                  lower, &
                                  upper)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

IMPLICIT NONE

! Subroutine arguments:
REAL                        :: x        ! The value to be searched for in y.
REAL                        :: y(:)     ! An real array to be searched.
INTEGER                     :: lower
INTEGER, OPTIONAL           :: upper

! Local declarations:
INTEGER                     :: Range  ! The range of lower and upper on entry. Used to calculate step and nsteps
INTEGER                     :: Step   ! The size of step taken in the search
INTEGER                     :: nsteps ! The total number of steps used
INTEGER                     :: i      ! Step count
INTEGER                     :: u      ! Upper position used in the search
INTEGER                     :: l      ! Lower position used in the search
INTEGER                     :: new    ! tentative new position for upper or lower
LOGICAL                     :: Ascending
INTEGER                     :: top
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_RealSearchNoIndex"

!----------------------------------
! 0. Tracing and initialisations
!----------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! Determine number and initial size of steps

IF (PRESENT (upper)) THEN
  range = upper - lower + 1
  top = upper
ELSE
  ! Assume that if upper has not been given, then
  ! range is size of array, and set lower to 1
  lower = 1
  range = SIZE (y)
  top = range
END IF

IF (range > 0) THEN
  step = 1
  nsteps = 0
  DO
    IF (step > range) EXIT
    step = step * 2
    nsteps = nsteps + 1
  END DO
  step = step / 2
  Ascending = (y(top) > y(lower))

  ! Conduct search

  l = lower - 1
  u = lower - 1
  DO i = 1, nsteps
    new = MIN (top, l + step)
    if (y(new) < x .EQV. Ascending .AND. y(new) /= x) l = new
    new = MIN (top, u + step)
    if (y(new) < x .EQV. Ascending .OR. y(new) == x) u = new
    step = step / 2
  END DO

  IF (PRESENT (upper)) upper = u
  lower = l + 1
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_RealSearchNoIndex
