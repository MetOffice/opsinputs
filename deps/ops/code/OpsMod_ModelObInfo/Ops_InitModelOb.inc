!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Sets data presence indicators to missing.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_InitModelOb (ModelOb)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE OpsMod_ObsInfo, ONLY: &
  Ops_ObsActionInit

IMPLICIT NONE

! Subroutine arguments:
CLASS (ModelOb_Type), INTENT(INOUT) :: ModelOb

! Local declarations:
CHARACTER(len=*), PARAMETER         :: RoutineName = "Ops_InitModelOb"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL Ops_ModelObGlobalAction (ModelOb,           &
                              Ops_ObsActionInit)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_InitModelOb
