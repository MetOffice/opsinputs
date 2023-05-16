!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Searches the real array y for the real value x using the index.
!
! Method:
!   The upper and lower limits are conditionally incremented by a
!   step which halves between iterations.
!   The increment is only applied if the new position for the lower
!   or upper is acceptable.
!
! Inputs:
!   y: a real array
!   index: an integer array, pointing to y in ascending order
!   x: a real to be located in y
!   keylen: The length of the strings in y and x
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

SUBROUTINE Ops_RealSearchIndex (x,     &
                                y,     &
                                index, &
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
INTEGER                     :: index(:) ! an index pointing to y in ascending order.
INTEGER                     :: lower
INTEGER                     :: upper

! Local declarations:
INTEGER                     :: Range  ! The range of lower and upper on entry. Used to calculate Step and nsteps
INTEGER                     :: Step   ! The size of step taken in the search
INTEGER                     :: nsteps ! The total number of steps used
INTEGER                     :: istep  ! Step count
INTEGER                     :: u      ! Upper position used in the search
INTEGER                     :: l      ! Lower position used in the search
INTEGER                     :: new    ! tentative new position for upper or lower
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_RealSearchIndex"

!----------------------------------
! 0. Tracing and initialisations
!----------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! Determine number and initial size of steps

range = upper - lower + 1

IF (range > 0) THEN
  step = 1
  nsteps = 0
  DO
    IF (step > range) EXIT
    step = step * 2
    nsteps = nsteps + 1
  END DO
  step = step / 2

  ! Conduct search

  l = lower - 1
  u = lower - 1
  DO istep = 1, nsteps
    new = MIN (upper, l + step)
    IF (y(index(new)) < x) l = new
    new = MIN (upper, u + step)
    IF (y(index(new)) <= x) u = new
    step = step / 2
  END DO
  upper = u
  lower = l + 1
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_RealSearchIndex
