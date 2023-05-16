!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Deallocate the model state.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_FinalFieldDataShared

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

IMPLICIT NONE

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = 'Ops_FinalFieldDataShared'

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL FieldDataShared % DestroySharedData

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_FinalFieldDataShared
