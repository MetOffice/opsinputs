!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! This subroutine returns the value y corresponding to a given value of x using
! linear interpolation of a look up table of x-y pairs.  The x-y pairs must be
! order of increasing x.  If x falls out of range, extrapolation is used.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_LinearInterp (x,    &
                             y,    &
                             xtab, &
                             ytab)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

IMPLICIT NONE

! Subroutines arguments:
REAL, INTENT(IN)            :: xtab(:)
REAL, INTENT(IN)            :: ytab(:)
REAL, INTENT(IN)            :: x
REAL, INTENT(OUT)           :: y

! Local declarations:
INTEGER                     :: n
INTEGER                     :: step
INTEGER                     :: nsteps
INTEGER                     :: l
INTEGER                     :: u
INTEGER                     :: i
INTEGER                     :: new
INTEGER                     :: pos
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_LinearInterp"

!----------------------------------
! 0. Tracing and initialisations
!----------------------------------
IF (UseTrace) CALL gen_trace_entry (RoutineName)

n = MIN (SIZE (xtab),SIZE (ytab))
IF (n == 1) THEN
  y = ytab(1)
ELSE IF (n > 1) THEN
  step = 1
  nsteps = 0
  DO
    IF (step > n - 2) EXIT
    step = step * 2
    nsteps = nsteps + 1
  END DO
  step = step / 2

  ! Conduct search
  pos = 1
  DO i = 1,nsteps
    new = MIN (n - 1,pos + step)
    IF (xtab(new) < x) pos = new
    step = step / 2
  END DO

  l = pos
  u = pos + 1
  ! interpolate (extrapolate) between u and l
  y = ((x - xtab(l)) * ytab(u) - (x - xtab(u)) * ytab(l)) / (xtab(u) - xtab(l))
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_LinearInterp
