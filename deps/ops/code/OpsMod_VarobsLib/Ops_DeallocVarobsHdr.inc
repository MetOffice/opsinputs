!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Deallocate space for a header from a varobs file.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_DeallocVarobsHdr (varobs)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

IMPLICIT NONE

! Subroutine arguments:
CLASS (varobs_type), INTENT(OUT) :: varobs

! Local declarations:
CHARACTER(len=*), PARAMETER      :: RoutineName = "Ops_DeallocVarobsHdr"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_DeallocVarobsHdr
