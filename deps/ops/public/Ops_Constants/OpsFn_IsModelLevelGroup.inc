!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Return true or false according to whether ObsGroup is model level.
!-------------------------------------------------------------------------------

FUNCTION OpsFn_IsModelLevelGroup (ObsGroup)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

IMPLICIT NONE

! Function arguments:
INTEGER, INTENT(IN)         :: ObsGroup

! Function result:
LOGICAL                     :: OpsFn_IsModelLevelGroup

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "OpsFn_IsModelLevelGroup"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (ANY ((/ObsGroupSonde,ObsGroupOcean/) == ObsGroup)) THEN
  OpsFn_IsModelLevelGroup = .TRUE.
ELSE
  OpsFn_IsModelLevelGroup = .FALSE.
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END FUNCTION OpsFn_IsModelLevelGroup
