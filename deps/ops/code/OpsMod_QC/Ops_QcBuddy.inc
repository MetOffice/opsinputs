!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Buddy check between two sets of obs A and B (can be the same)
! Calls routines to sort the observations geographically
! form pairs of observations and then check them, updating the PGEs
!
! Method:  see OSDP 2, section 3
!
! Inputs:  see argument list
!
! Outputs: PGEs/flags - see argument list
!-------------------------------------------------------------------------------

SUBROUTINE Ops_QcBuddy (ObsA,       & ! Observations set A
                        BackgrA,    & ! Background value
                        BackgrErrA, & ! Estimated error of background value
                        PGEBdA,     & ! Probability of Gross Error before/after check
                        RepObs)       ! Obs on reported levels

USE GenMod_Control, ONLY: &
    DiagnosticMode,       & ! (PARAMETER) normal program development
    mype

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    MessageOut,        &
    UseTrace

USE OpsMod_GatherSpread, ONLY: &
    Ops_Gather,                &
    Ops_Spread

USE OpsMod_MiscTypes, ONLY: &
    ZCodeLevelsA

USE OpsMod_ModelObInfo, ONLY: &
    ModelOb_type

USE OpsMod_ObsGroupInfo, ONLY: &
    ObsGroupSatwind

USE OpsMod_ObsInfo, ONLY: &
    OB_type,              & ! Structure storing observations
    LenCallsign,          &
    Ops_Alloc

USE OpsMod_QC, ONLY: &
    QcMode

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_type), INTENT(INOUT)           :: ObsA         ! observation data
TYPE (ModelOB_type), INTENT(IN)         :: BackgrA      ! Background value
TYPE (ModelOB_type), INTENT(IN)         :: BackgrErrA   ! Estimated background error
TYPE (ModelOB_type), INTENT(INOUT)      :: PGEBdA       ! PGE
TYPE (OB_type), OPTIONAL, INTENT(INOUT) :: RepObs       ! observation data

! Local declarations:
INTEGER                                 :: J            ! loop variable
INTEGER                                 :: BdIndexA(ObsA % header % NumObsTotal)  ! pointers to sorted obs
INTEGER                                 :: BdNumObsA                              ! number of sorted observations
LOGICAL                                 :: BdListA(ObsA % header % NumObsTotal)   ! list buddy checks for these obs?
REAL                                    :: BdHCscaleA(ObsA % header % NumObsTotal)! background error corrn scales
INTEGER                                 :: BdBandStartA(BdNumBands + 1)           ! first ob in each latitude Band
INTEGER                                 :: NumPairs                               ! number of Pairs found overall
INTEGER                                 :: PairObA(ObsA % header % NumObsTotal * MaxBdTot(ObsA % header % ObsGroup))
INTEGER                                 :: PairObB(ObsA % header % NumObsTotal * MaxBdTot(ObsA % header % ObsGroup))
REAL                                    :: PairDist(ObsA % header % NumObsTotal * MaxBdTot(ObsA % header % ObsGroup))
REAL                                    :: PairRotA(ObsA % header % NumObsTotal * MaxBdTot(ObsA % header % ObsGroup))
REAL                                    :: PairRotB(ObsA % header % NumObsTotal * MaxBdTot(ObsA % header % ObsGroup))
INTEGER, POINTER                        :: ReportFlagsA(:)
REAL, POINTER                           :: TimeA(:)
REAL, POINTER                           :: LatitudeA(:)
REAL, POINTER                           :: LongitudeA(:)
REAL, POINTER                           :: Value1A(:)
REAL, POINTER                           :: Value1vA(:)
REAL, POINTER                           :: Value2A(:,:)
REAL, POINTER                           :: Value2vA(:,:)
REAL, POINTER                           :: ObErr1A(:)
REAL, POINTER                           :: ObErr2A(:,:)
REAL, POINTER                           :: Backgr1A(:)
REAL, POINTER                           :: Backgr1vA(:)
REAL, POINTER                           :: Backgr2A(:,:)
REAL, POINTER                           :: Backgr2vA(:,:)
REAL, POINTER                           :: BackgrErr1A(:)
REAL, POINTER                           :: BackgrErr2A(:,:)
REAL, POINTER                           :: PGEBd1A(:)
REAL, POINTER                           :: PGEBd2A(:,:)
REAL, POINTER                           :: PLevelsAA(:,:) ! pressure levels for ObsA
REAL, POINTER                           :: PLevelsBA(:,:) ! pressure levels for ObsA
REAL, POINTER                           :: PLevelA(:,:)
INTEGER                                 :: NumLevA
CHARACTER(len=LenCallSign), POINTER     :: CallSignA(:)
CHARACTER(len=*), PARAMETER             :: RoutineName = "Ops_QcBuddy"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! Simplifies buddy-check listing and pairing if obs have callsigns (esp SatWind)
IF (PRESENT (RepObs)) THEN
  IF (.NOT. RepObs % header % Callsign % Present) THEN
    CALL Ops_Alloc (RepObs % Header % Callsign,    &
                    "RepObs % Callsign",           &
                    RepObs % header % NumObsLocal, &
                    RepObs % Callsign)
  END IF
ELSE
  IF (.NOT. ObsA % header % Callsign % Present) THEN
    CALL Ops_Alloc (ObsA % Header % Callsign,    &
                    "ObsA % Callsign",           &
                    ObsA % header % NumObsLocal, &
                    ObsA % Callsign)
    IF (ObsA % header % SatId % Present) THEN
      DO J = 1, ObsA % header % NumObsLocal
        WRITE (ObsA % Callsign(J), '(I8.8)') ObsA % SatId(J)
      END DO
    END IF
  END IF
END IF

IF (PRESENT (RepObs)) THEN
  CALL Ops_Gather (RepObs,        &
                   RepObs % Time, &
                   TimeA)
  CALL Ops_Gather (RepObs,            &
                   RepObs % Latitude, &
                   LatitudeA)
  CALL Ops_Gather (RepObs,             &
                   RepObs % Longitude, &
                   LongitudeA)
  CALL Ops_Gather (RepObs,            &
                   RepObs % CallSign, &
                   CallSignA)
ELSE
  CALL Ops_Gather (ObsA,        &
                   ObsA % Time, &
                   TimeA)
  CALL Ops_Gather (ObsA,            &
                   ObsA % Latitude, &
                   LatitudeA)
  CALL Ops_Gather (ObsA,             &
                   ObsA % Longitude, &
                   LongitudeA)
  CALL Ops_Gather (ObsA,            &
                   ObsA % CallSign, &
                   CallSignA)
END IF
CALL Ops_Gather (ObsA,               &
                 ObsA % ReportFlags, &
                 ReportFlagsA)

IF (ObsA % header % PLevelsA % Present) THEN
  CALL Ops_Gather (ObsA,                    &
                   ObsA % PLevelsA % value, &
                   PLevelsAA)
ELSE
  ALLOCATE (PLevelsAA(1,1)) ! Dummy size
END IF

IF (ObsA % header % PLevelsB % Present) THEN
  CALL Ops_Gather (ObsA,                    &
                   ObsA % PLevelsB % value, &
                   PLevelsBA)
ELSE
  ALLOCATE (PLevelsBA(1,1)) ! Dummy size
END IF

IF (QcMode >= DiagnosticMode .AND. mype == 0) THEN
  WRITE (MessageOut, '(/A,3X,2A/A,F7.2,A,F7.2)') RoutineName, &
    ObsA % header % ObsGroupName, ' BUDDY CHECK', &
    '   BdDamp1 = ', BdDamp1, '  BdDamp2 = ', BdDamp2
END IF

CALL Ops_QcBdSort (ObsA,         &  ! in
                   LatitudeA,    &  ! in
                   LongitudeA,   &  ! in
                   TimeA,        &  ! in
                   PLevelsAA,    &  ! in
                   CallsignA,    &  ! in
                   ReportFlagsA, &  ! inout
                   BdIndexA,     &  ! out
                   BdNumObsA,    &  ! out
                   BdListA,      &  ! out
                   BdHCscaleA,   &  ! out
                   BdBandStartA)    ! out

! Any obs to buddy check?  BdBandStartA(BdNumBands+1) = BdNumObs+1
IF (BdBandStartA(BdNumBands + 1) > 1) THEN
  CALL Ops_QcBdPair (ObsA,         & ! In
                     LatitudeA,    & ! in
                     LongitudeA,   & ! in
                     CallSignA,    & ! in
                     BdIndexA,     & ! In
                     BdBandStartA, & ! In
                     NumPairs,     & ! Out
                     PairObA,      & ! Out
                     PairObB,      & ! Out
                     PairDist,     & ! Out
                     PairRotA,     & ! Out
                     PairRotB)       ! Out

  !--------------------
  ! Upper-air variables
  !--------------------
  IF (ObsA % header % t % Present .AND. &
      ObsA % header % ObsGroup /= ObsGroupSatwind) THEN
    PlevelA => PlevelsAA

    CALL Ops_Gather (ObsA,             &
                     ObsA % t % value, &
                     Value2A)
    CALL Ops_Gather (ObsA,             &
                     ObsA % t % ObErr, &
                     ObErr2A)
    CALL Ops_Gather (ObsA,        &
                     BackgrA % t, &
                     Backgr2A)
    CALL Ops_Gather (ObsA,           &
                     BackGrErrA % t, &
                     BackgrErr2A)
    CALL Ops_Gather (ObsA,       &
                     PGEBdA % t, &
                     PGEBd2A)

    CALL Ops_QcBdChkUs (BdListA,                    & ! in      list buddy checks for these obs?
                        BdHCscaleA,                 & ! in      background error corrn scales
                        CallSignA,                  & ! in      Observation callsigns (for listing)
                        TimeA,                      & ! in      Seconds after ValidityTime
                        PlevelA,                    & ! in      Pressure levels
                        ObsA % header % t % NumLev, & ! in      Number of levels per observation
                        Value2A,                    & ! in      Observed value
                        ObErr2A,                    & ! in      Estimated error of observed value
                        Backgr2A,                   & ! in      Background value
                        BackgrErr2A,                & ! in      Estimated error of background value
                        PGEBd2A,                    & ! inout   Probability of Gross Error
                        NumPairs,                   & ! in      Number of pairs
                        PairObA,                    & ! in      Ob from set A in pair
                        PairObB,                    & ! in      Ob from set B in pair
                        PairDist)                     ! in      horizontal distance (km) between obs

    ! Copy the PGE information back
    CALL Ops_Spread (ObsA,       &
                     PGEBd2A,    &
                     PGEBdA % t)

    DEALLOCATE (PGEBd2A)
    DEALLOCATE (BackgrErr2A)
    DEALLOCATE (Backgr2A)
    DEALLOCATE (ObErr2A)
    DEALLOCATE (Value2A)
  END IF

  IF (ObsA % header % u % Present .AND. &
      ObsA % header % v % Present) THEN
    PLevelA => PLevelsAA
    ! For multi-level (radiosonde) data provide layer boundaries
    ! Does this work?
    ! Does PlevelsA surround PLevelsB?
    ! It does at the lower end, but we run the risk of failures higher up?
    NumLevA = ObsA % header % u % NumLev

    IF (NumLevA > 1) THEN
      IF (ObsA % Header % u % ZCode == ZCodeLevelsA) THEN
        PlevelA => PLevelsBA
      ELSE
        PlevelA => PLevelsAA
      END IF
    END IF

    CALL Ops_Gather (ObsA,             &
                     ObsA % u % value, &
                     Value2A)
    CALL Ops_Gather (ObsA,             &
                     ObsA % v % value, &
                     Value2vA)
    CALL Ops_Gather (ObsA,             &
                     ObsA % u % ObErr, &
                     ObErr2A)
    CALL Ops_Gather (ObsA,        &
                     BackgrA % u, &
                     Backgr2A)
    CALL Ops_Gather (ObsA,        &
                     BackgrA % v, &
                     Backgr2vA)
    CALL Ops_Gather (ObsA,           &
                     BackGrErrA % u, &
                     BackgrErr2A)
    CALL Ops_Gather (ObsA,       &
                     PGEBdA % u, &
                     PGEBd2A)

    CALL Ops_QcBdChkUv (BdListA,     & ! in    list buddy checks for these obs?
                        BdHCscaleA,  & ! in    background error corrn scales
                        CallSignA,   & ! in    Observation callsigns (for listing)
                        TimeA,       & ! in    Seconds after ValidityTime
                        PlevelA,     & ! in    Pressure levels
                        NumLevA,     & ! in    Number of levels per observation
                        Value2A,     & ! in    Observed value (u)
                        Value2vA,    & ! in    Observed value (v)
                        ObErr2A,     & ! in    Estimated error of observed value
                        Backgr2A,    & ! in    Background value
                        Backgr2vA,   & ! in    Background value
                        BackgrErr2A, & ! in    Estimated error of background value
                        PGEBd2A,     & ! inout Probability of Gross Error
                        NumPairs,    & ! in    Number of pairs
                        PairObA,     & ! in    Ob from set A in pair
                        PairObB,     & ! in    Ob from set B in pair
                        PairDist,    & ! in    horizontal distance (km) between obs
                        PairRotA,    & ! in    ) rotation at A/B to give
                        PairRotB)      ! in    ) longitudinal/transverse winds

    ! Copy the PGE information back
    CALL Ops_Spread (ObsA,       &
                     PGEBd2A,    &
                     PGEBdA % u)

    DEALLOCATE (PGEBd2A)
    DEALLOCATE (BackgrErr2A)
    DEALLOCATE (Backgr2vA)
    DEALLOCATE (Backgr2A)
    DEALLOCATE (ObErr2A)
    DEALLOCATE (Value2vA)
    DEALLOCATE (Value2A)
  END IF

  !------------------
  ! Surface variables (necessary to check that BG % pstar is associated
  ! because if the only surface variables are TCBogus obs it will be
  ! null.
  !------------------
  IF (ObsA % header % pstar % Present .AND. &
      ASSOCIATED (BackgrA % pstar)) THEN

    CALL Ops_Gather (ObsA,                 &
                     ObsA % pstar % value, &
                     Value1A)
    CALL Ops_Gather (ObsA,                 &
                     ObsA % pstar % ObErr, &
                     ObErr1A)
    CALL Ops_Gather (ObsA,            &
                     BackgrA % pstar, &
                     Backgr1A)
    CALL Ops_Gather (ObsA,               &
                     BackgrErrA % pstar, &
                     BackgrErr1A)
    CALL Ops_Gather (ObsA,           &
                     PGEBdA % pstar, &
                     PGEBd1A)

    CALL Ops_QcBdChkSs (BdListA,     & ! in      list buddy checks for these obs?
                        BdHCscaleA,  & ! in      background error corrn scales
                        CallSignA,   & ! in      Observation callsigns (for listing)
                        TimeA,       & ! in      Seconds after ValidityTime
                        Value1A,     & ! in      Observed value
                        ObErr1A,     & ! in      Estimated error of observed value
                        Backgr1A,    & ! in      Background value
                        BackgrErr1A, & ! in      Estimated error of background value
                        PGEBd1A,     & ! inout   Probability of Gross Error
                        NumPairs,    & ! in      Number of pairs
                        PairObA,     & ! in      Ob from set A in pair
                        PairObB,     & ! in      Ob from set B in pair
                        PairDist)      ! in      horizontal distance (km) between obs

    ! Copy the PGE information back

    CALL Ops_Spread (ObsA,           &
                     PGEBd1A,        &
                     PGEBdA % pstar)

    DEALLOCATE (PGEBd1A)
    DEALLOCATE (BackgrErr1A)
    DEALLOCATE (Backgr1A)
    DEALLOCATE (ObErr1A)
    DEALLOCATE (Value1A)
  END IF

  IF (ObsA % header % t2 % Present) THEN

    CALL Ops_Gather (ObsA,              &
                     ObsA % t2 % value, &
                     Value1A)
    CALL Ops_Gather (ObsA,              &
                     ObsA % t2 % ObErr, &
                     ObErr1A)
    CALL Ops_Gather (ObsA,         &
                     BackgrA % t2, &
                     Backgr1A)
    CALL Ops_Gather (ObsA,            &
                     BackgrErrA % t2, &
                     BackgrErr1A)
    CALL Ops_Gather (ObsA,        &
                     PGEBdA % t2, &
                     PGEBd1A)

    CALL Ops_QcBdChkSs (BdListA,     & ! in      list buddy checks for these obs?
                        BdHCscaleA,  & ! in      background error corrn scales
                        CallSignA,   & ! in      Observation callsigns (for listing)
                        TimeA,       & ! in      Seconds after ValidityTime
                        Value1A,     & ! in      Observed value
                        ObErr1A,     & ! in      Estimated error of observed value
                        Backgr1A,    & ! in      Background value
                        BackgrErr1A, & ! in      Estimated error of background value
                        PGEBd1A,     & ! inout   Probability of Gross Error
                        NumPairs,    & ! in      Number of pairs
                        PairObA,     & ! in      Ob from set A in pair
                        PairObB,     & ! in      Ob from set B in pair
                        PairDist)      ! in      horizontal distance (km) between obs

    ! Copy the PGE information back
    CALL Ops_Spread (ObsA,        &
                     PGEBd1A,     &
                     PGEBdA % t2)

    DEALLOCATE (PGEBd1A)
    DEALLOCATE (BackgrErr1A)
    DEALLOCATE (Backgr1A)
    DEALLOCATE (ObErr1A)
    DEALLOCATE (Value1A)
  END IF

  IF (ObsA % header % rh2 % Present) THEN

    CALL Ops_Gather (ObsA,               &
                     ObsA % rh2 % value, &
                     Value1A)
    CALL Ops_Gather (ObsA,               &
                     ObsA % rh2 % ObErr, &
                     ObErr1A)
    CALL Ops_Gather (ObsA,          &
                     BackgrA % rh2, &
                     Backgr1A)
    CALL Ops_Gather (ObsA,             &
                     BackgrErrA % rh2, &
                     BackgrErr1A)
    CALL Ops_Gather (ObsA,         &
                     PGEBdA % rh2, &
                     PGEBd1A)

    CALL Ops_QcBdChkSs (BdListA,     & ! in      list buddy checks for these obs?
                        BdHCscaleA,  & ! in      background error corrn scales
                        CallSignA,   & ! in      Observation callsigns (for listing)
                        TimeA,       & ! in      Seconds after ValidityTime
                        Value1A,     & ! in      Observed value
                        ObErr1A,     & ! in      Estimated error of observed value
                        Backgr1A,    & ! in      Background value
                        BackgrErr1A, & ! in      Estimated error of background value
                        PGEBd1A,     & ! inout   Probability of Gross Error
                        NumPairs,    & ! in      Number of pairs
                        PairObA,     & ! in      Ob from set A in pair
                        PairObB,     & ! in      Ob from set B in pair
                        PairDist)      ! in      horizontal distance (km) between obs

    ! Copy the PGE information back
    CALL Ops_Spread (ObsA,         &
                     PGEBd1A,      &
                     PGEBdA % rh2)

    DEALLOCATE (PGEBd1A)
    DEALLOCATE (BackgrErr1A)
    DEALLOCATE (Backgr1A)
    DEALLOCATE (ObErr1A)
    DEALLOCATE (Value1A)
  END IF

  IF (ObsA % header % u10 % Present .AND. &
      ObsA % header % v10 % Present) THEN

    CALL Ops_Gather (ObsA,               &
                     ObsA % u10 % value, &
                     Value1A)
    CALL Ops_Gather (ObsA,               &
                     ObsA % v10 % value, &
                     Value1vA)
    CALL Ops_Gather (ObsA,               &
                     ObsA % u10 % ObErr, &
                     ObErr1A)
    CALL Ops_Gather (ObsA,          &
                     BackgrA % u10, &
                     Backgr1A)
    CALL Ops_Gather (ObsA,          &
                     BackgrA % v10, &
                     Backgr1vA)
    CALL Ops_Gather (ObsA,             &
                     BackgrErrA % u10, &
                     BackgrErr1A)
    CALL Ops_Gather (ObsA,         &
                     PGEBdA % u10, &
                     PGEBd1A)

    CALL Ops_QcBdChkSv (BdListA,     & ! in      list buddy checks for these obs?
                        BdHCscaleA,  & ! in      background error corrn scales
                        CallSignA,   & ! in      Observation callsigns (for listing)
                        TimeA,       & ! in      Seconds after ValidityTime
                        Value1A,     & ! in      Observed value
                        Value1vA,    & ! in      Observed value
                        ObErr1A,     & ! in      Estimated error of observed value
                        Backgr1A,    & ! in      Background value
                        Backgr1vA,   & ! in      Background value
                        BackgrErr1A, & ! in      Estimated error of background value
                        PGEBd1A,     & ! inout   Probability of Gross Error
                        NumPairs,    & ! in      Number of pairs
                        PairObA,     & ! in      Ob from set A in pair
                        PairObB,     & ! in      Ob from set B in pair
                        PairDist,    & ! in      horizontal distance (km) between obs
                        PairRotA,    & ! in      ) rotation at A/B to give
                        PairRotB)      ! in      ) longitudinal/transverse winds

    ! Copy the PGE information back
    CALL Ops_Spread (ObsA,         &
                     PGEBd1A,      &
                     PGEBdA % u10)

    DEALLOCATE (PGEBd1A)
    DEALLOCATE (BackgrErr1A)
    DEALLOCATE (Backgr1vA)
    DEALLOCATE (Backgr1A)
    DEALLOCATE (ObErr1A)
    DEALLOCATE (Value1vA)
    DEALLOCATE (Value1A)
  END IF
END IF

DEALLOCATE (PLevelsAA)
DEALLOCATE (PLevelsBA)
DEALLOCATE (CallSignA)
DEALLOCATE (ReportFlagsA)
DEALLOCATE (LongitudeA)
DEALLOCATE (LatitudeA)
DEALLOCATE (TimeA)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_QcBuddy
