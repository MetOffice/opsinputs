!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Reads the cycle time and run id from the environment variables OPS_CYCLE_TIME
! and OPS_RUNID respectively.  OPS_RUNID is optional, OPS_CYCLE_TIME is
! mandatory
!
! Outputs:
!   cycle time as an array(4) - Year, Month, Day, Hour, Minute
!   CycleUTC
!   runid (if set)
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ReadCycleTime (status_if_cycle_time_not_set)

USE GenMod_Core, ONLY: &
    gen_message,       &
    gen_trace_entry,   &
    gen_trace_exit,    &
    StatusWarning,     &
    UseTrace

USE OpsMod_DateTime, ONLY:   &
    OpsFn_DateTime_ToMinUTC

USE OpsMod_EnvUtils, ONLY: &
    ops_get_env

IMPLICIT NONE

! Subroutine arguments:
INTEGER, OPTIONAL, INTENT(IN) :: status_if_cycle_time_not_set

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName = "Ops_ReadCycleTime"
CHARACTER(len=100)            :: cycle_time_use
CHARACTER(len=100)            :: message(5)

!------------------------------------------------------------------------------
! 0. Tracing
!------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-------------------------------------------------------------------------------
! 1. Read the runid
!-------------------------------------------------------------------------------

CALL ops_get_env ("OPS_RUNID",                           &
                  runid,                                 &
                  status_if_env_not_set = StatusWarning, &
                  result_if_env_not_set = "XX99")

!-------------------------------------------------------------------------------
! 2. Read the cycle time
!-------------------------------------------------------------------------------

CALL ops_get_env ("OPS_CYCLE_TIME",                                     &
                  cycle_time_use,                                       &
                  status_if_env_not_set = status_if_cycle_time_not_set)
IF (cycle_time_use /= "") THEN
  READ (cycle_time_use, '(I4.4,3I2.2)') CycleTime(cycle_year),  &
                                        CycleTime(cycle_month), &
                                        CycleTime(cycle_day),   &
                                        CycleTime(cycle_hour)

  CycleUTC = OpsFn_DateTime_ToMinUTC (CycleTime)

  !-------------------------------------------------------------------------------
  ! 3. Print the cycle time
  !-------------------------------------------------------------------------------

  WRITE (Message(1), '(A15,I4)') "OPS_YEAR     = ", CycleTime(cycle_year)
  WRITE (Message(2), '(A15,I4)') "OPS_MONTH    = ", CycleTime(cycle_month)
  WRITE (Message(3), '(A15,I4)') "OPS_DAY      = ", CycleTime(cycle_day)
  WRITE (Message(4), '(A15,I4)') "OPS_HOUR     = ", CycleTime(cycle_hour)
  WRITE (Message(5), '(A15,A4)') "OPS_RUNID    = ", TRIM (runid)

  CALL gen_message (RoutineName, &
                    Message)

END IF

!-------------------------------------------------------------------------------
! 2. Tracing and end of routine
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ReadCycleTime
