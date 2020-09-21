!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Basic setup of an ob_type structure.  Basic grid information comes from the
! UM header.  For GRIB types more detailed grid and time information also comes
! from the UM header, for other types validity time information comes from the
! cycle time environment variables.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_SetupObType (Observations)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE OpsMod_Control, ONLY: &
    CycleTime,            &
    cycle_year,           &
    cycle_month,          &
    cycle_day,            &
    cycle_hour

USE OpsMod_ObsGroupInfo, ONLY: &
    OpsFn_ObsGroupNumToName

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_type), INTENT(INOUT) :: Observations

! Local declarations:
CHARACTER(len=*),  PARAMETER  :: RoutineName = 'Ops_SetupObType'

!-------------------------------------
! 0. Tracing, Initialise and setup
!-------------------------------------
IF (UseTrace) CALL gen_trace_entry (RoutineName)

Observations % Header % ObsGroupName = OpsFn_ObsGroupNumToName (Observations % Header % ObsGroup)

CALL Observations % init

! Set observation header information
Observations % Header % ObsRelease = 2 ! new-style ops
Observations % Header % ModelLevel = .FALSE.
Observations % Header % NumCxBatches = 0
Observations % Header % varobs_biascorrected = .FALSE.
Observations % Header % NumObsLocal = 0
Observations % Header % NumObsTotal = 0

Observations % Header % ValidityTime % Year = CycleTime(cycle_year)
Observations % Header % ValidityTime % Month = CycleTime(cycle_month)
Observations % Header % ValidityTime % Day = CycleTime(cycle_day)
Observations % Header % ValidityTime % hour = CycleTime(cycle_hour)
Observations % Header % ValidityTime % minute = 0
Observations % Header % ValidityTime % second = 0

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_SetupObType
