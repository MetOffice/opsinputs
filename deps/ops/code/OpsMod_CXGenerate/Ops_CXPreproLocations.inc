!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Called by Ops_CXGenerate to preprocess the observation locations and times for
! each observation group.
!
! - The observations are sorted on time, excluding those lying outside
!       of the required space and time.
! - Arrays in time order are set up for latitude and longitude in radians.
! - The observations are divided up into time windows, recording the positions
!       of the first and last observations lying within each time window.
! - Position of each observation within its time window is calculated,
!       from 0 to 1.
!
! Inputs:
!     Obs: Array of structures containing observations for each obsgroup,
!       required for observation locations.
!     ForecastTimes: The forecast times to be interpolated between.
!
! Outputs:
!     LocsInfo: Array of LocInfo_Type, which contains information about
!       the locations and times of the observations for each group,
!       as required for horizontal and time interpolation.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CXPreproLocations (Obs,             &
                                  ForecastTimes,   &
                                  domain,          &
                                  LocsInfo,        &
                                  obs_from_varobs)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI,                                  &
  RMDI

USE OpsMod_Constants, ONLY: &
  deg2rad

USE OpsMod_Control, ONLY: &
  CycleUTC

USE OpsMod_DateTime, ONLY: &
  seconds_in_hour

USE OpsMod_ModelIO, ONLY: &
  DomainInfo_type

USE OpsMod_ObsInfo, ONLY: &
  Ob_Type,                &
  OutOfAreaReport

USE OpsMod_Sort, ONLY: &
  Ops_InitSortMask,    &
  Ops_RealSort

USE OpsMod_Utilities, ONLY: &
  Ops_WorkingCoordinates

IMPLICIT NONE

! Subroutine arguments:
TYPE (Ob_Type), INTENT(IN)         :: Obs(:)
INTEGER, INTENT(IN)                :: ForecastTimes(:)
TYPE (DomainInfo_type), INTENT(IN) :: domain
TYPE (LocInfo_Type), INTENT(INOUT) :: LocsInfo(:)
LOGICAL, INTENT(IN)                :: obs_from_varobs

! Local declarations:
INTEGER                            :: iTime
INTEGER                            :: iGroup
INTEGER                            :: iOb
INTEGER                            :: NumObs
INTEGER                            :: Pos
INTEGER                            :: NumForecastTimes
INTEGER                            :: CentralForecastTime
INTEGER                            :: loop
INTEGER, ALLOCATABLE               :: SortGrpInfo(:) ! required for sorting
REAL                               :: FieldTime ! Time of forecast, in seconds relative to analysis time
REAL                               :: Time
REAL                               :: TimeToSec
REAL, ALLOCATABLE                  :: RotatedLong(:)
REAL, ALLOCATABLE                  :: RotatedLat (:)
CHARACTER(len=*), PARAMETER        :: RoutineName = 'Ops_CXPreproLocations'

IF (UseTrace) CALL gen_trace_entry (RoutineName)

NumObs = 0
NumForecastTimes = COUNT (ForecastTimes(:) /= IMDI)

DO IGroup = 1, SIZE (Obs)

  IF (Obs(IGroup) % header % NumObsTotal > 0) THEN

    IF (.NOT. UseValidityTimes) THEN
      CentralForecastTime = ForecastTimes(ISHFT (NumForecastTimes + 1, -1))
      TimeToSec = seconds_in_hour
    ELSE
      CentralForecastTime = CycleUTC
      TimeToSec = 60.0
    END IF

    NumObs = Obs(iGroup) % Header % NumObsLocal

    ALLOCATE (LocsInfo(iGroup) % Index(NumObs))
    ALLOCATE (LocsInfo(iGroup) % Lambda(NumObs))
    ALLOCATE (LocsInfo(iGroup) % Phi(NumObs))
    ALLOCATE (SortGrpInfo(NumObs))

    ALLOCATE (RotatedLong(NumObs))
    ALLOCATE (RotatedLat(NumObs))
    LocsInfo(iGroup) % Index(:) = IMDI
    RotatedLong(:) = RMDI
    RotatedLat(:) = RMDI
    LocsInfo(iGroup) % Lambda(:) = RMDI
    LocsInfo(iGroup) % Phi(:) = RMDI

    ! Sort observations on time, excluding observations outside of required area.
    !-----------------------------------------------------------------------------
    IF (obs_from_varobs) THEN ! change if reading from an Obstore?
      CALL Ops_InitSortMask (LocsInfo(iGroup) % Index, &
                             SortGrpInfo,              &
                             NumObs)
    ELSE
      CALL Ops_InitSortMask (LocsInfo(iGroup) % Index,                                     &
                             SortGrpInfo,                                                  &
                             NumObs,                                                       &
                             Mask = (Obs(iGroup) % Time(:) /= RMDI .AND.                   &
                             .NOT. BTEST (Obs(iGroup) % ReportFlags(:), OutOfAreaReport)))
    END IF
    CALL Ops_RealSort (Obs(iGroup) % Time(:),    &
                       NumObs,                   &
                       LocsInfo(iGroup) % Index, &
                       SortGrpInfo)
    DEALLOCATE (SortGrpInfo)

    ! Rotate and convert latitude and longitude to radians as required by
    ! interpolation
    !-----------------------------------------------------------------------------

    IF (obs_from_varobs) THEN

      ! take from ob structure
      RotatedLat(1:NumObs) = Obs(iGroup) % Latitude(1:NumObs)
      RotatedLong(1:NumObs) = Obs(iGroup) % Longitude(1:NumObs)

    ELSE

      IF (NumObs > 0) THEN
        ! calculate working coordinates
        CALL Ops_WorkingCoordinates (Obs(iGroup) % Latitude(:),  &
                                     Obs(iGroup) % Longitude(:), &
                                     domain % pole_lat,          &
                                     domain % pole_lon,          &
                                     domain % grid_type,         &
                                     RotatedLat(:),              &
                                     RotatedLong(:))
      END IF

    END IF

    DO loop = 1, NumObs
      IF (RotatedLong(LocsInfo(iGroup) % Index(loop)) < 0.0) THEN
        LocsInfo(iGroup) % Lambda(loop) = (360.0 + RotatedLong(LocsInfo(iGroup) % Index(loop))) * deg2rad
      ELSE
        LocsInfo(iGroup) % Lambda(loop) = RotatedLong(LocsInfo(iGroup) % Index(loop)) * deg2rad
      END IF
      LocsInfo(iGroup) % Phi(loop) = RotatedLat(LocsInfo(iGroup) % Index(loop)) * deg2rad
    END DO
    DEALLOCATE (RotatedLat)
    DEALLOCATE (RotatedLong)

    ! Determine the range of observations which require interpolation for each
    ! forecast time
    !--------------------------------------------------------------------------
    ALLOCATE (LocsInfo(iGroup) % FirstInWindow(0:NumForecastTimes))
    ALLOCATE (LocsInfo(iGroup) % LastInWindow(0:NumForecastTimes))

    Pos = 0
    DO iTime = 1, NumForecastTimes - 1
      ! find the last observation that has the same time as this time
      FieldTime = (ForecastTimes(iTime) - CentralForecastTime) * TimeToSec
      DO iOb = Pos + 1, NumObs
        Time = Obs(iGroup) % Time(LocsInfo(iGroup) % Index(iOb))
        IF (Time > FieldTime) EXIT
        Pos = iOb
      END DO
      LocsInfo(iGroup) % FirstInWindow(iTime) = Pos + 1
      ! find the last observation which requires this time,
      ! ie just before the next forecast time

      FieldTime = (ForecastTimes(iTime + 1) - CentralForecastTime) * TimeToSec
      DO iOb = Pos + 1, NumObs
        Time = Obs(iGroup) % Time(LocsInfo(iGroup) % Index(iOb))
        IF (Time > FieldTime) EXIT
        Pos = iOb
      END DO
      LocsInfo(iGroup) % LastInWindow(iTime) = Pos
    END DO

    ! Set up time windows outside of the forecast times, so that
    ! constant value extrapolation occurs. Also means that if there
    ! is only one forecast time, this time is used for all obs.
    LocsInfo(iGroup) % FirstInWindow(0) = 1
    LocsInfo(iGroup) % LastInWindow(0) = 0
    LocsInfo(iGroup) % FirstInWindow(NumForecastTimes) = NumObs + 1
    LocsInfo(iGroup) % LastInWindow(NumForecastTimes) = NumObs
  ELSE
    ALLOCATE (LocsInfo(iGroup) % FirstInWindow(0:NumForecastTimes))
    ALLOCATE (LocsInfo(iGroup) % LastInWindow(0:NumForecastTimes))
    LocsInfo(iGroup) % FirstInWindow(:) = 1
    LocsInfo(iGroup) % LastInWindow(:) = 0
  END IF
END DO

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CXPreproLocations
