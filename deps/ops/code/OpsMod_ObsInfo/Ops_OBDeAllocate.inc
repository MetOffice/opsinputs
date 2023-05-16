!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Deallocate OB structure.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_OBDeAllocate (OB)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

IMPLICIT NONE

! Subroutine arguments:
CLASS (OB_type), INTENT (INOUT) :: Ob

! Local declarations:
CHARACTER(len=*), PARAMETER     :: RoutineName = "Ops_OBDeAllocate"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (ASSOCIATED (Ob % Header % ObsPerBatchPerPE)) DEALLOCATE (Ob % Header % ObsPerBatchPerPE)
IF (ASSOCIATED (Ob % Header % ObsPerPE)) DEALLOCATE (Ob % Header % ObsPerPE)

CALL Ops_ObsGlobalAction (Ob,                   &
                          Ops_ObsActionDealloc)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_OBDeAllocate
