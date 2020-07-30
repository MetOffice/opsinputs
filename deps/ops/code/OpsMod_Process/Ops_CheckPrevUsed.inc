!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Checks whether data have already been assimilated.
!
! Method:
!   Observation times of receipt are checked against specified cut-off time
!   to see whether they have already been available for assimilation.
!   If so, the assimilated flag is set.
!
! Inputs:
!   Obs - Structure containing data about observations
!
! Outputs:
!   Obs - With assimilated flags set appropriately
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CheckPrevUsed (Obs)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  gen_warn,            &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE OpsMod_Control, ONLY: &
  CycleTime

USE OpsMod_DateTime, ONLY:   &
    OpsFn_DateTime_ToMinUTC

USE OpsMod_EnvUtils, ONLY: &
  ops_get_env

USE OpsMod_ObsInfo, ONLY: &
  OB_type,                &
  NewReport,              &
  AssimilatedReport

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_type), INTENT(INOUT) :: Obs

! Local declarations:
INTEGER                       :: i           ! Loop variable over observations
INTEGER                       :: HKutc       ! cycle time in UTC
INTEGER                       :: NumObsLocal ! Total number of observations
INTEGER                       :: CutPrev
CHARACTER(len=*),PARAMETER    :: RoutineName = 'Ops_CheckPrevUsed'

IF(UseTrace) CALL gen_trace_entry (RoutineName)

!------------------------------------
! 1. Read value of CutPrev if possible
!------------------------------------

CALL ops_get_env ("OPS_CUTPREV",                &
                  CutPrev,                      &
                  result_if_env_not_set = IMDI)

IF (CutPrev /= IMDI) THEN
  !------------------------------------
  ! 1. Convert cycle time to UTC
  !------------------------------------

  HKutc = OpsFn_DateTime_ToMinUTC (CycleTime)

  !-------------------------------------------------
  ! 2. Check receipt times against prev. use cut off
  !-------------------------------------------------

  NumObsLocal = Obs % header % NumObsLocal

  IF (Obs % header % ReceiptTime % Present) THEN
    DO i = 1, NumObsLocal
      IF (Obs % ReceiptTime(i) <= HKutc - CutPrev) THEN
        Obs % AssimFlags(i) = AssimilatedReport
      ELSE IF (Obs % ReceiptTime(i) /= IMDI) THEN
        Obs % AssimFlags(i) = NewReport
      END IF
    END DO
  ELSE
    CALL gen_warn (RoutineName,                   &
                   'Receipt times not available')
  END IF
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END  SUBROUTINE Ops_CheckPrevUsed
