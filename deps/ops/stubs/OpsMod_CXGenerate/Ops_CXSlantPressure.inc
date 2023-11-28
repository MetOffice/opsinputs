!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Calculate slanted CX column positions.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CXSlantPressure (locs,      &
                                Obs,       &
                                domain,    &
                                first,     &
                                New,       &
                                Last,      &
                                DataOut,   &
                                STime,     &
                                SPhi,      &
                                SLambda,   &
                                iLevel,    &
                                NumLevels, &
                                iter,      &
                                Niter)

USE GenMod_Control, ONLY: &
  GeneralMode,            &
  VerboseMode

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  MessageOut,          &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  RMDI

USE OpsMod_Constants, ONLY: &
  deg2rad

USE OpsMod_Extract, ONLY: &
  Ops_CheckTimeAndArea

USE OpsMod_ModelIO, ONLY: &
  DomainInfo_type

USE OpsMod_ObsInfo, ONLY: &
  Ob_type

USE OpsMod_SondeDrift, ONLY: &
  SLevel,                    &
  SondeDrift

USE OpsMod_Utilities, ONLY: &
  Ops_WorkingCoordinates

IMPLICIT NONE

! Subroutine arguments:
TYPE (LocInfo_type), INTENT(INOUT) :: locs
TYPE (Ob_Type), INTENT(IN)         :: Obs
TYPE (DomainInfo_type), INTENT(IN) :: domain
INTEGER, INTENT(IN)                :: first
INTEGER, INTENT(IN)                :: New
INTEGER, INTENT(IN)                :: Last
REAL, INTENT(IN)                   :: DataOut(first:)
REAL, INTENT(INOUT)                :: STime(first:)
REAL, INTENT(INOUT)                :: SPhi(first:)
REAL, INTENT(INOUT)                :: SLambda(first:)
INTEGER, INTENT(IN)                :: iLevel
INTEGER, INTENT(IN)                :: NumLevels
INTEGER, INTENT(IN)                :: iter
INTEGER, INTENT(IN)                :: Niter

! Local declarations:
CHARACTER(len=*), PARAMETER        :: RoutineName = 'Ops_CXSlantPressure'
INTEGER                            :: iSorted
INTEGER                            :: iOb
INTEGER                            :: Lev1
INTEGER                            :: jlev
LOGICAL                            :: ObsInTimeWindow(first:last)
LOGICAL                            :: ObsInArea(first:last)
INTEGER                            :: SLev(first:last)
REAL                               :: SLatitude(first:last)
REAL                               :: SLongitude(first:last)
INTEGER                            :: ZTime(first:last)

IF (UseTrace) CALL gen_trace_entry (RoutineName)

SLev = -1
ZTime = 0

! Check through slanted obs updating position information
DO iSorted = New, Last
  iOb = locs % Index(iSorted)
  Lev1 = 1
  IF (iLevel > 1) THEN  ! Don't have to restart search from level 1
    Lev1 = MAX (1, SLevel(iLevel - 1, locs % Index(iSorted)))
  END IF
  DO jLev = Lev1, Obs % NLevels(iOb)
    IF (Obs % PlevelsA(iOb,jLev) % value == RMDI) CYCLE
    IF (Obs % PlevelsA(iOb,jLev) % value <= DataOut(iSorted)) EXIT
    IF (Obs % level_time(iOb,jLev) /= RMDI .AND. &
        Obs % level_lat(iOb,jLev) /= RMDI .AND. &
        Obs % level_lon(iOb,jLev) /= RMDI) THEN
      SLev(iSorted) = jLev
    END IF
  ! Interpolate in the vertical? - not needed for 2 second data
  END DO

  IF (SLev(iSorted) > 0) THEN
    jLev = SLev(iSorted)
    IF (SondeDrift > 0) THEN  ! STime defaults to launch time above
      STime(iSorted) = Obs % level_time(iOb,jLev)
    END IF
    SLatitude(iSorted) = Obs % level_lat(iOb,jLev)
    SLongitude(iSorted) = Obs % level_lon(iOb,jLev)
  ELSE  ! Values aren't used for LocsInfo
    STime(iSorted) = Obs % time(iOb)
    SLatitude(iSorted) = Obs % latitude(iOb)
    SLongitude(iSorted) = Obs % longitude(iOb)
  END IF
END DO

CALL Ops_WorkingCoordinates (SLatitude(New:Last),  &
                             SLongitude(New:Last), &
                             domain % pole_lat,    &
                             domain % pole_lon,    &
                             domain % grid_type,   &
                             SPhi(New:Last),       &
                             SLambda(New:Last))

! Check lat/long (x/y) but not time
CALL Ops_CheckTimeAndArea (SPhi(New:Last),            &
                           SLambda(New:Last),         &
                           ZTime(New:Last),           &
                           0,                         &
                           0,                         &
                           ObsInTimeWindow(New:Last), &
                           ObsInArea(New:Last),       &
                           .FALSE.)
IF (GeneralMode >= VerboseMode .AND. COUNT (.NOT. ObsInArea) > 0) THEN
  WRITE (MessageOut, '(A,2(1X,I0))') 'Level, obs out of area', iLevel, COUNT (.NOT. ObsInArea)
END IF

DO iSorted = New, Last
  IF (SLambda(iSorted) < 0.0) THEN
    SLambda(iSorted) = (360.0 + SLambda(iSorted)) * deg2rad
  ELSE
    SLambda(iSorted) = SLambda(iSorted) * deg2rad
  END IF
  SPhi(iSorted) = SPhi(iSorted) * deg2rad
  IF (.NOT. ObsInArea(iSorted)) THEN
    ! Don't use this level or those above it
    Locs % UseLevel(iLevel:NumLevels,iSorted) = .FALSE.
  END IF

  IF (iter == Niter .AND. SLev(iSorted) > 0) THEN  ! Store and spread upwards
    SLevel(iLevel,Locs % Index(New:Last)) = SLev(New:Last)
    ! Levels > iLevel are overwritten later except if gap in level_ values
    Locs % SPhi(iLevel:NumLevels,iSorted) = SPhi(iSorted)
    Locs % SLambda(iLevel:NumLevels,iSorted) = SLambda(iSorted)
    Locs % STime(iLevel:NumLevels,iSorted) = STime(iSorted)
  END IF
END DO

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CXSlantPressure
