!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Buddy check ocean observations
! Calls routines to sort the observations geographically
! form pairs of observations and then check them, updating the PGEs
!
! Method:  see OSDP 2, section 3
!
! Inputs:  see argument list
!
! Outputs: PGEs/flags - see argument list
!-------------------------------------------------------------------------------

SUBROUTINE Ops_QcBuddyOcean (ObsA,         & ! Observations set A
                             BackgrA,      & ! Background value
                             BackgrErrA,   & ! Estimated background error (scales may be altered)
                             PGEBdA,       & ! Probability of Gross Error before/after check
                             NumObsActive, & ! Number of obs without final report flag
                             RepObs)         ! Reported level observations

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

USE OpsMod_ModelObInfo, ONLY: &
    ModelOb_type

USE OpsMod_ObsInfo, ONLY: &
    LenCallsign,          &
    OB_type

USE OpsMod_Process, ONLY: &
    FOAMCovStats

USE OpsMod_QC, ONLY: &
    QcMode

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_type), TARGET, INTENT(INOUT)      :: ObsA       ! observation data
TYPE (ModelOB_type), TARGET, INTENT(IN)    :: BackgrA    ! Background value
TYPE (ModelOB_type), TARGET, INTENT(INOUT) :: BackgrErrA ! Est. background error
TYPE (ModelOB_type), TARGET, INTENT(INOUT) :: PGEBdA     ! PGE
INTEGER, INTENT(IN)                        :: NumObsActive ! Used to dimension Pair* arrays
TYPE (OB_type), INTENT(INOUT)              :: RepObs     ! observation data (reported levels)

! Local declarations:
INTEGER                                    :: BdIndexA(ObsA % header % NumObsTotal) ! pointers to sorted obs
INTEGER                                    :: BdNumObsA                         ! number of sorted observations
LOGICAL                                    :: BdListA(ObsA % header % NumObsTotal)  ! list buddy checks for these obs?
INTEGER                                    :: BdBandStartA(BdNumBands + 1)        ! first ob in each latitude Band
REAL                                       :: SynScaleA(ObsA % header % NumObsTotal)! bk error synoptic corrn scales
REAL                                       :: MesScaleA(ObsA % header % NumObsTotal)! bk error mesoscale corrn scales
REAL                                       :: SynAnisoA(ObsA % header % NumObsTotal)! bk error synoptic anisotropy
REAL                                       :: MesAnisoA(ObsA % header % NumObsTotal)! bk error mesoscale anisotropy
INTEGER                                    :: NumPairs         ! number of Pairs found overall
INTEGER                                    :: PairObA(NumObsActive * MaxBdTot(ObsA % header % ObsGroup))
INTEGER                                    :: PairObB(NumObsActive * MaxBdTot(ObsA % header % ObsGroup))
REAL                                       :: PairDist(NumObsActive * MaxBdTot(ObsA % header % ObsGroup))
REAL                                       :: PairRotA(NumObsActive * MaxBdTot(ObsA % header % ObsGroup))
REAL                                       :: PairRotB(NumObsActive * MaxBdTot(ObsA % header % ObsGroup))
INTEGER                                    :: i
CHARACTER(len=*), PARAMETER                :: RoutineName = "Ops_QcBuddyOcean"
REAL, POINTER                              :: Latitude(:)
REAL, POINTER                              :: Longitude(:)
REAL, POINTER                              :: Time(:)
CHARACTER(len=LenCallSign), POINTER        :: Callsign(:)
REAL, POINTER                              :: PLevelsA(:,:)
REAL, POINTER                              :: TempSyn_Var(:,:)
REAL, POINTER                              :: TempMes_Var(:,:)
REAL, POINTER                              :: SalSyn_Var(:,:)
REAL, POINTER                              :: SalMes_Var(:,:)
REAL, POINTER                              :: TempSyn_HCor(:,:)
REAL, POINTER                              :: TempMes_HCor(:,:)
REAL, POINTER                              :: SalSyn_HCor(:,:)
REAL, POINTER                              :: SalMes_HCor(:,:)
INTEGER, POINTER                           :: ReportFlags(:)
REAL, POINTER                              :: theta_value(:,:)
REAL, POINTER                              :: theta_error(:,:)
REAL, POINTER                              :: theta_bg(:,:)
REAL, POINTER                              :: theta_bgerr(:,:)
REAL, POINTER                              :: theta_pge(:,:)
REAL, POINTER                              :: salt_value(:,:)
REAL, POINTER                              :: salt_error(:,:)
REAL, POINTER                              :: salt_bg(:,:)
REAL, POINTER                              :: salt_pge(:,:)

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (QcMode >= DiagnosticMode .AND. mype == 0) THEN
  WRITE (MessageOut, '(/A,3X,2A/A,F7.2,A,F7.2)') RoutineName, &
    ObsA % header % ObsGroupName, ' BUDDY CHECK', &
    '   BdDamp1 = ', BdDamp1, '  BdDamp2 = ', BdDamp2
END IF

CALL Ops_Gather (RepObs,            &
                 RepObs % Latitude, &
                 Latitude)
CALL Ops_Gather (RepObs,             &
                 RepObs % Longitude, &
                 Longitude)
CALL Ops_Gather (RepObs,        &
                 RepObs % Time, &
                 Time)
CALL Ops_Gather (RepObs,            &
                 RepObs % Callsign, &
                 Callsign)
CALL Ops_Gather (ObsA,                    &
                 ObsA % PLevelsA % value, &
                 PLevelsA)
CALL Ops_Gather (ObsA,               &
                 ObsA % ReportFlags, &
                 ReportFlags)
CALL Ops_Gather (ObsA,                 &
                 ObsA % theta % value, &
                 theta_value)
CALL Ops_Gather (ObsA,                 &
                 ObsA % theta % ObErr, &
                 theta_error)
CALL Ops_Gather (ObsA,            &
                 BackgrA % theta, &
                 theta_bg)
CALL Ops_Gather (ObsA,           &
                 PGEBdA % theta, &
                 theta_pge)

CALL Ops_QcBdSort (ObsA,         &  ! in
                   Latitude,     &  ! in
                   Longitude,    &  ! in
                   Time,         &  ! in
                   PLevelsA,     &  ! in
                   Callsign,     &  ! in
                   ReportFlags,  &  ! inout
                   BdIndexA,     &  ! out
                   BdNumObsA,    &  ! out
                   BdListA,      &  ! out
                   SynScaleA,    &  ! out
                   BdBandStartA)    ! out

! Any obs to buddy check?  BdBandStartA(BdNumBands+1) = BdNumObs+1
IF (BdBandStartA(BdNumBands + 1) > 1) THEN
  CALL Ops_QcBdPair (ObsA,         & ! in
                     Latitude,     & ! in
                     Longitude,    & ! in
                     Callsign,     & ! in
                     BdIndexA,     & ! in
                     BdBandStartA, & ! in
                     NumPairs,     & ! out
                     PairObA,      & ! out
                     PairObB,      & ! out
                     PairDist,     & ! out
                     PairRotA,     & ! out
                     PairRotB)       ! out

  !----------------------------------------
  ! Buddy check theta and salinity profiles
  !----------------------------------------

  IF (FOAMCovStats > 0) THEN

    CALL Ops_Gather (ObsA,                &
                     ObsA % salt % value, &
                     salt_value)
    CALL Ops_Gather (ObsA,                &
                     ObsA % salt % ObErr, &
                     salt_error)
    CALL Ops_Gather (ObsA,           &
                     BackgrA % salt, &
                     salt_bg)
    CALL Ops_Gather (ObsA,          &
                     PGEBdA % salt, &
                     salt_pge)
    CALL Ops_Gather (ObsA,                      &
                     BackgrErrA % TempSyn_HCor, &
                     TempSyn_HCor)
    CALL Ops_Gather (ObsA,                      &
                     BackgrErrA % TempMes_HCor, &
                     TempMes_HCor)
    CALL Ops_Gather (ObsA,                     &
                     BackgrErrA % SalSyn_HCor, &
                     SalSyn_HCor)
    CALL Ops_Gather (ObsA,                     &
                     BackgrErrA % SalMes_HCor, &
                     SalMes_HCor)
    CALL Ops_Gather (ObsA,                     &
                     BackgrErrA % TempSyn_Var, &
                     TempSyn_Var)
    CALL Ops_Gather (ObsA,                     &
                     BackgrErrA % TempMes_Var, &
                     TempMes_Var)
    CALL Ops_Gather (ObsA,                    &
                     BackgrErrA % SalSyn_Var, &
                     SalSyn_Var)
    CALL Ops_Gather (ObsA,                    &
                     BackgrErrA % SalMes_Var, &
                     SalMes_Var)

    CALL Ops_QcOceanScales (Latitude,  & ! in
                            SynScaleA, & ! out
                            MesScaleA, & ! out
                            SynAnisoA, & ! out
                            MesAnisoA)   ! out

    ! FoamCovStats background error file contains syn/mes scales, these are
    ! usually overwritten with simple latitudinally dependent values here.
    IF (OceanScaleHsyn > 0.0 .AND. OceanScaleHmes > 0.0) THEN
      DO i = 1, ObsA % Header % Theta % NumLev
        TempSyn_HCor(:,i) = SynScaleA(:)
        TempMes_HCor(:,i) = MesScaleA(:)
        SalSyn_HCor(:,i) = SynScaleA(:)
        SalMes_HCor(:,i) = MesScaleA(:)
      END DO
    END IF

    ! Theta
    CALL Ops_QcBdChkOcean (BdListA,                        & ! in     list buddy checks for these obs?
                           Callsign,                       & ! in     Observation callsigns (for listing)
                           Time,                           & ! in     Seconds after ValidityTime
                           ObsA % header % theta % NumLev, & ! in     Number of levels per observation
                           theta_value,                    & ! in     Observed value
                           theta_error,                    & ! in     Estimated SD of observation error
                           theta_bg,                       & ! in     Background value
                           TempSyn_Var,                    & ! in     Est. SD of synoptic bk error
                           TempSyn_HCor,                   & ! in     Est. scale of synoptic bk error
                           SynAnisoA,                      & ! in     Est. anisotropy of synoptic error
                           TempMes_Var,                    & ! in     Est. SD of mesoscale bk error
                           TempMes_HCor,                   & ! in     Est. scale of mesoscale bk error
                           MesAnisoA,                      & ! in     Est. anisotropy of mesoscale error
                           theta_pge,                      & ! inout  Probability of Gross Error
                           NumPairs,                       & ! in     Number of pairs
                           PairObA,                        & ! in     Ob from set A in pair
                           PairObB,                        & ! in     Ob from set B in pair
                           PairDist,                       & ! in     horizontal distance (km) between obs
                           PairRotA,                       & ! in     direction of ob B from ob A
                           PairRotB)                         ! in     180 + direction of ob A from ob B

    ! Salinity
    CALL Ops_QcBdChkOcean (BdListA,                       & ! in     list buddy checks for these obs?
                           Callsign,                      & ! in     Observation callsigns (for listing)
                           Time,                          & ! in     Seconds after ValidityTime
                           ObsA % header % salt % NumLev, & ! in     Number of levels per observation
                           salt_value,                    & ! in     Observed value
                           salt_error,                    & ! in     Estimated SD of observation error
                           salt_bg,                       & ! in     Background value
                           SalSyn_Var,                    & ! in     Est. SD of synoptic bk error
                           SalSyn_HCor,                   & ! in     Est. scale of synoptic bk error
                           SynAnisoA,                     & ! in     Est. anisotropy of synoptic error
                           SalMes_Var,                    & ! in     Est. SD of mesoscale bk error
                           SalMes_HCor,                   & ! in     Est. scale of mesoscale bk error
                           MesAnisoA,                     & ! in     Est. anisotropy of mesoscale error
                           salt_pge,                      & ! inout  Probability of Gross Error
                           NumPairs,                      & ! in     Number of pairs
                           PairObA,                       & ! in     Ob from set A in pair
                           PairObB,                       & ! in     Ob from set B in pair
                           PairDist,                      & ! in     horizontal distance (km) between obs
                           PairRotA,                      & ! in     direction of ob B from ob A
                           PairRotB)                        ! in     180 + direction of ob A from ob B

    CALL Ops_Spread (ObsA,                      &
                     TempSyn_HCor,              &
                     BackgrErrA % TempSyn_HCor)
    CALL Ops_Spread (ObsA,                      &
                     TempMes_HCor,              &
                     BackgrErrA % TempMes_HCor)
    CALL Ops_Spread (ObsA,                     &
                     SalSyn_HCor,              &
                     BackgrErrA % SalSyn_HCor)
    CALL Ops_Spread (ObsA,                     &
                     SalMes_HCor,              &
                     BackgrErrA % SalMes_HCor)
    CALL Ops_Spread (ObsA,          &
                     salt_pge,      &
                     PGEBdA % salt)

    DEALLOCATE (salt_value)
    DEALLOCATE (salt_error)
    DEALLOCATE (salt_bg)
    DEALLOCATE (salt_pge)
    DEALLOCATE (TempSyn_Var)
    DEALLOCATE (TempMes_Var)
    DEALLOCATE (SalSyn_Var)
    DEALLOCATE (SalMes_Var)
    DEALLOCATE (TempSyn_HCor)
    DEALLOCATE (TempMes_HCor)
    DEALLOCATE (SalSyn_HCor)
    DEALLOCATE (SalMes_HCor)

  ELSE  ! Old style statistics

    CALL Ops_Gather (ObsA,               &
                     BackgrErrA % theta, &
                     theta_bgerr)

    CALL Ops_QcBdChkUs (BdListA,                    & ! in      list buddy checks for these obs?
                        SynScaleA,                  & ! in      background error corrn scales
                        Callsign,                   & ! in      Observation callsigns (for listing)
                        Time,                       & ! in      Seconds after ValidityTime
                        PlevelsA,                   & ! in      Pressure levels
                        ObsA % header % t % NumLev, & ! in      Number of levels per observation
                        theta_value,                & ! in      Observed value
                        theta_error,                & ! in      Estimated error of observed value
                        theta_bg,                   & ! in      Background value
                        theta_bgerr,                & ! in      Estimated error of background value
                        theta_pge,                  & ! inout   Probability of Gross Error
                        NumPairs,                   & ! in      Number of pairs
                        PairObA,                    & ! in      Ob from set A in pair
                        PairObB,                    & ! in      Ob from set B in pair
                        PairDist)                     ! in      horizontal distance (km) between obs

    DEALLOCATE (theta_bgerr)

  END IF

END IF

CALL Ops_Spread (ObsA,           &
                 theta_pge,      &
                 PGEBdA % theta)

DEALLOCATE (Latitude)
DEALLOCATE (Longitude)
DEALLOCATE (Time)
DEALLOCATe (Callsign)
DEALLOCATE (PLevelsA)
DEALLOCATE (ReportFlags)
DEALLOCATE (theta_value)
DEALLOCATE (theta_error)
DEALLOCATE (theta_bg)
DEALLOCATE (theta_pge)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_QcBuddyOcean
