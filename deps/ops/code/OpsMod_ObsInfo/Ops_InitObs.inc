!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Sets data presence indicators to missing.
!
! Method: The data presence indicator is initialised to absent.
!
! Inputs: Obs - Observation structure with data presence indicators not set
!
! Outputs: Obs - Observation structure with data presence indicators set
!-------------------------------------------------------------------------------

SUBROUTINE Ops_InitObs (Obs)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

IMPLICIT NONE

! Subroutine arguments:
CLASS (Ob_Type), INTENT(INOUT) :: Obs

! Local declarations:
CHARACTER(len=*), PARAMETER    :: RoutineName = "Ops_InitObs"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL Ops_ObsGlobalAction (Obs,               &
                          Ops_ObsActionInit)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_InitObs
