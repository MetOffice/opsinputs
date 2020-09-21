!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Generate random numbers using the Fortran intrinsic method
!-------------------------------------------------------------------------------

SUBROUTINE Ops_RandomNumberInt_array1d (Out)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

IMPLICIT NONE

! Subroutine arguments:
REAL, INTENT(OUT)           :: Out(:)

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_RandomNumberInt_array1d"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL RANDOM_NUMBER (Out)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_RandomNumberInt_array1d
