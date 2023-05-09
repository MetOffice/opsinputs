!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Generate random numbers using the Meto method.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_RandomNumberMeto_array1d (Out)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

IMPLICIT NONE

! Subroutine arguments:
REAL, INTENT(OUT)           :: Out(:)

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_RandomNumberMeto_array1d"
INTEGER                     :: i

IF (UseTrace) CALL gen_trace_entry (RoutineName)

DO i = 1,SIZE (Out)
  IntRand = MOD (IntRand * meto_a + meto_c,meto_m)
  Out(i) = REAL (IntRand) / REAL (meto_m)
END DO

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_RandomNumberMeto_array1d
