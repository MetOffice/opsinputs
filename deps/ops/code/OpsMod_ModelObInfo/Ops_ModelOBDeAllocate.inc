!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Deallocate space for ModelOb (Model Ob equivalent).
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ModelOBDeAllocate (BG)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE OpsMod_ObsInfo, ONLY: &
    Ops_ObsActionDealloc

IMPLICIT NONE

! Subroutine arguments:
CLASS (ModelOB_type), INTENT(INOUT) :: BG ! Model equivalent of OB

! Local declarations:
CHARACTER(len=*), PARAMETER         :: RoutineName = "Ops_ModelOBDeAllocate"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL Ops_ModelObGlobalAction (BG,                   &
                              Ops_ObsActionDealloc)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ModelOBDeAllocate
