!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Converts a model run string (QU etc) to a string for use by monitoring.
!-------------------------------------------------------------------------------

FUNCTION OpsFn_ModelRuntoModelId (run)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

IMPLICIT NONE

! Function arguments:
CHARACTER(len=*), INTENT(IN) :: run

! Function result:
INTEGER                      :: OpsFn_ModelRuntoModelId

! Local declarations:
CHARACTER(len=*), PARAMETER  :: RoutineName = "OpsFn_ModelRuntoModelId"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

SELECT CASE (run(1:2))
  CASE ('QU')
    OpsFn_ModelRuntoModelId = 1  ! GLOBL_AIR
  CASE ('QG')
    OpsFn_ModelRuntoModelId = 2  ! GLOBL_AIR_FRCT
  CASE ('QO')
    OpsFn_ModelRuntoModelId = 8  ! GLOBL_SEA
  CASE ('QV')
    OpsFn_ModelRuntoModelId = 13 ! UK variable resolution
  CASE DEFAULT
    OpsFn_ModelRuntoModelId = IMDI
END SELECT

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END FUNCTION OpsFn_ModelRuntoModelId
