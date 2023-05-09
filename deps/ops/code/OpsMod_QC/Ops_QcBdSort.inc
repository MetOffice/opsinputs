!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Select and sort observations for buddy check
! Set (geographically varying) horizontal error correlation scale
! Set flag for obs to be printed during buddy check
!
! See OSDP 2, section 3.3
!
! Inputs:  see argument list
!
! Outputs: Index of sorted obs and associated information
!-------------------------------------------------------------------------------

SUBROUTINE Ops_QcBdSort (Obs,         &
                         Latitude,    &
                         Longitude,   &
                         Time,        &
                         PLevelsA,    &
                         Callsign,    &
                         ReportFlags, &
                         BdIndex,     &
                         BdNumObs,    &
                         BdList,      &
                         BdHCscale,   &
                         BdBandStart)

USE GenMod_Control, ONLY: &
    DiagnosticMode,       & ! (PARAMETER) normal program development
    mype

USE GenMod_Core, ONLY: &
    gen_message,       &
    gen_trace_entry,   &
    gen_trace_exit,    &
    MessageOut,        &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI

USE OpsMod_Control, ONLY: &
    GlobalRun

USE OpsMod_ObsGroupInfo, ONLY: &
    ObsGroupSatwind,           &
    ObsGroupAircraft,          &
    ObsGroupSurface

USE OpsMod_ObsInfo, ONLY: &
    OB_type,              & ! Structure storing observations
    FinalRejectReport,    & ! => do not use in analysis or verification
    SurplusReport           ! => do not use in analysis (might use in VER)

USE OpsMod_QC, ONLY: &
    QcMode

USE OpsMod_Sort, ONLY: &
    Ops_InitSortMask,  &
    Ops_IntegerSort,   &
    Ops_RealSort

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_type), INTENT(IN)   :: Obs         ! observation data
REAL, INTENT(IN)             :: Latitude(:)
REAL, INTENT(IN)             :: Longitude(:)
REAL, INTENT(IN)             :: Time(:)
REAL, INTENT(IN)             :: PLevelsA(:,:)
CHARACTER(len=*), INTENT(IN) :: Callsign(:)
INTEGER, INTENT(INOUT)       :: ReportFlags(:)
INTEGER, INTENT(OUT)         :: BdIndex(:)      ! pointers to sorted obs
INTEGER, INTENT(OUT)         :: BdNumObs        ! number of sorted observations
LOGICAL, INTENT(OUT)         :: BdList(:)       ! list buddy checks for these obs?
REAL, INTENT(OUT)            :: BdHCscale(:)    ! background error corrn scales
INTEGER, INTENT(OUT)         :: BdBandStart(:)  ! first ob in each latitude Band

! Local declarations:
INTEGER                      :: J         ! loops over observations
INTEGER                      :: Box       ! loops over BdPrint boxes
INTEGER                      :: JSort     ! loops over sorted observations
INTEGER                      :: JBand     ! loops over bands
INTEGER                      :: BP        ! Position within BdPrintBoxes
INTEGER                      :: BdPrintNumBoxes = 0 ! Number of boxes in use
INTEGER                      :: BdPrintNumObs = 0 ! Number of obs to list
INTEGER                      :: NumObsTotal
REAL                         :: HCfact          ! scaling for horizontal correlation scale
REAL                         :: Plevel          ! level - for print
INTEGER                      :: LBand           ! Previous Band number
LOGICAL                      :: Mask(Obs % header % NumObsTotal)   ! Mask for sorting
INTEGER                      :: IBand(Obs % header % NumObsTotal)   ! Band number
INTEGER                      :: GrpInfo(Obs % header % NumObsTotal)   ! Used for sorting
CHARACTER(len=*), PARAMETER  :: RoutineName = "Ops_QcBdSort"
CHARACTER(len=100)           :: Message

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! 0.  Initilisation
DO Box = 1, 3
  BP = 4 * (Box - 1)
  IF (BdPrintBox(1 + BP) <= BdPrintBox(2 + BP) .AND. &
      BdPrintBox(3 + BP) <= BdPrintBox(4 + BP)) BdPrintNumBoxes = Box
END DO

IF (QcMode >= DiagnosticMode .AND. mype == 0) THEN
  WRITE (MessageOut, '(A,I0)') 'BdPrintNumBoxes = ', BdPrintNumBoxes
END IF

! 1.  Loop through observations, calculating band and setting mask

Mask(:) = .TRUE.

NumObsTotal = Obs % Header % NumObsTotal

DO J = 1, NumObsTotal

  ! Use 89.999 to round up values  eg ranges are 70.0 to 74.999
  IBand(J) = MAX (1, INT ((89.999 - Latitude (J)) / BdDlat) + 1)
  ! Omit permanently rejected or unusable observations
  IF (BTEST (ReportFlags(J), SurplusReport) .OR. &
      BTEST (ReportFlags(J), FinalRejectReport)) THEN
    Mask(J) = .FALSE.
  END IF

  ! Omit aircraft and satobs with pressure <= 0
  IF (Obs % header % ObsGroup == ObsGroupSatwind .OR. &
      Obs % header % ObsGroup == ObsGroupAircraft) THEN
    IF (PlevelsA(J,1) <= 0.0) Mask(J) = .FALSE.
  END IF

  IF (Latitude(J) == RMDI) Mask(J) = .FALSE.
END DO

! 2. Sort the selected observations (set up index)

! Initialise the index and group information arrays.
BdNumObs = NumObsTotal

CALL Ops_InitSortMask (BdIndex,  &
                       GrpInfo,  &
                       BdNumObs, &
                       Mask)

IF (BdNumObs > 1) THEN
  ! sort on each key in turn, starting with the most significant.
  CALL Ops_IntegerSort (IBand(:), &
                        BdNumObs, &
                        BdIndex,  &
                        GrpInfo)
  CALL Ops_RealSort (Longitude(:), &
                     BdNumObs,     &
                     BdIndex,      &
                     GrpInfo)
  CALL Ops_RealSort (-Latitude(:), &
                     BdNumObs,     &
                     BdIndex,      &
                     GrpInfo)
  ! Sorting on pressure is mainly useful for aircraft data, possibly also satobs
  IF (Obs % header % ObsGroup == ObsGroupSatwind .OR. &
      Obs % header % ObsGroup == ObsGroupAircraft) THEN
    CALL Ops_RealSort (PlevelsA(:,1), &
                       BdNumObs,      &
                       BdIndex,       &
                       GrpInfo)
  END IF
  CALL Ops_RealSort (Time(:),  &
                     BdNumObs, &
                     BdIndex,  &
                     GrpInfo)
ELSE IF (BdNumObs == 1) THEN
  BdIndex(1) = 1
END IF

! 3. Set HCscale, List and BandStart arrays

IF (Obs % header % ObsGroup == ObsGroupSurface) THEN
  BdHCscale(:) = CorScaleHsurf
ELSE
  BdHCscale(:) = CorScaleH
END IF

LBand = 0
BdList(:) = .FALSE.
DO JSort = 1, BdNumObs
  J = BdIndex(JSort)

  IF (GlobalRun) THEN   !  Global model - vary scale with latitude
    IF (Latitude(J) > TropLat) THEN
      HCfact = HCfact_NH
    ELSE IF (Latitude(J) > TropLat - TropInt) THEN
      HCfact = HCfact_NH + (HCfact_NH - HCfact_TR) * (Latitude(J) - TropLat) / TropInt
    ELSE IF (Latitude(J) > -TropLat + TropInt) THEN
      HCfact = HCfact_TR
    ELSE IF (Latitude(J) > -TropLat) THEN
      HCfact = HCfact_SH + (HCfact_TR - HCfact_SH) * (Latitude(J) + TropLat) / TropInt
    ELSE
      HCfact = HCfact_SH
    END IF
    BdHCscale(J) = BdHCscale(J) * HCfact
  END IF

  DO Box = 1, BdPrintNumBoxes
    BP = 4 * (Box - 1)
    IF (Latitude(J) >= BdPrintBox(1 + BP) .AND. &
        Latitude(J) <= BdPrintBox(2 + BP) .AND. &
        Longitude(J) >= BdPrintBox(3 + BP) .AND. &
        Longitude(J) <= BdPrintBox(4 + BP)) THEN
      BdList(J) = .TRUE.
    END IF
  END DO

  IF (BdList(J)) THEN
    BdPrintNumObs = BdPrintNumObs + 1
    Plevel = 0.0
    IF (Obs % header % PlevelsA % Present) Plevel = PlevelsA(J,1)
    IF (mype == 0) THEN
      WRITE (MessageOut, '(3I5,2X,A10,2F8.2,3F9.1)') &
        JSort, J, IBand(J), Callsign(J), Latitude(J), Longitude(J), &
        Plevel, Time(J) / 60.0, BdHCscale(J)
    END IF
  END IF

  DO JBand = LBand + 1, IBand(J)
    BdBandStart(JBand) = JSort
  END DO
  LBand = IBand(J)
END DO

DO JBand = LBand + 1, BdNumBands + 1
  BdBandStart(JBand) = BdNumObs + 1
END DO

IF (QcMode >= DiagnosticMode) THEN
  WRITE (Message, '(I0,A,I0,A,I0,A)') NumObsTotal, ' observations input, ', BdNumObs, ' sorted, ', &
                                      BdPrintNumObs, ' to be listed'
  CALL gen_message (RoutineName, &
                    Message)
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_QcBdSort
