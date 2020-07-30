!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! This routine completes the remaining details that are needed to complete the
!   ModelColumn information
!
! Outputs : Completed Cx structure, and optionally the Observation structure on
!           model levels
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CxComplete (Obs,      & ! OBstructre header from MetDB
                           Cx,       & ! Cx structure
                           UmHeader, & ! UM header from dump
                           NumCXLev)   ! Number of levels per ob in Cx file

USE GenMod_CLookAdd, ONLY: &
    LBFT

USE GenMod_Control, ONLY: &
    VerboseMode,          &
    GeneralMode,          &
    DebugMode,            &
    mype

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    gen_warn,          &
    MessageOut,        &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    IMDI,                                &
    RMDI

USE GenMod_ModelIO, ONLY: &
    UM_header_type

USE GenMod_UMHeaderConstants, ONLY: &
    FH_HorizGrid,                   &
    FH_HorizGrid_Eq,                &
    FH_LevDepCSize1,                &
    FH_LookupSize2,                 &
    FH_RowDepCStart,                &
    FH_SubModel,                    &
    FH_SubModel_Ocean,              &
    FH_VertCoord,                   &
    IC_BLevels,                     &
    IC_XLen,                        &
    IC_YLen,                        &
    RC_FirstLat,                    &
    RC_FirstLong,                   &
    RC_LatSpacing,                  &
    RC_LongSpacing,                 &
    RC_PoleLat,                     &
    RC_PoleLong,                    &
    RC_z_ModelTop

USE OpsMod_Constants, ONLY: &
    Eta_Ref

USE OpsMod_Control, ONLY: &
    ModelType,            &
    ModelType_Atmos,      &
    ModelType_SST

USE OpsMod_CXInfo, ONLY: &
    Cx_type,             &
    Ops_CXP

USE OpsMod_ObsGroupInfo, ONLY: &
    ObsGroupGeoCloud,          &
    ObsGroupRadar,             &
    ObsGroupRadarZ,            &
    ObsGroupSBUV,              &
    ObsGroupSeaIce,            &
    ObsGroupOceanColour,       &
    ObsGroupSurface,           &
    OpsFn_IsSatRadGroup

USE OpsMod_ObsInfo, ONLY: &
    OB_Type,              &
    Ops_Alloc

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_type), INTENT(INOUT), TARGET     :: Obs
TYPE (Cx_type), INTENT(INOUT), TARGET     :: Cx
TYPE (UM_header_type), INTENT(IN), TARGET :: UmHeader
INTEGER, INTENT(INOUT)                    :: NumCxLev

! Local declarations:
INTEGER, PARAMETER                        :: LevRef = 2
INTEGER                                   :: Ilev        ! Loop counter over levels
INTEGER                                   :: PntLevDepC  ! Pointer to Level Dependent Constants
INTEGER                                   :: n
INTEGER                                   :: Size1
REAL                                      :: Eta
REAL                                      :: Pref
CHARACTER(len=80)                         :: ErrMess(2)       ! Message for ErrorReport
CHARACTER(len=*), PARAMETER               :: RoutineName = 'Ops_CxComplete'

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-------------------------------------------------------------------------------
!  1. Set up pointers and extract info from the UMhdr to add to Cx header
!-------------------------------------------------------------------------------

IF (UmHeader % FixHd(FH_RowDepCStart) <= 0 .OR. &
    UmHeader % FixHd(FH_SubModel) == FH_SubModel_Ocean) THEN
  Cx % Header % StartLong = UmHeader % RealC(RC_FirstLong)
  Cx % Header % StartLat = UmHeader % RealC(RC_FirstLat)
  Cx % Header % LatInterval = UmHeader % RealC(RC_LatSpacing)
  Cx % Header % LongInterval = UmHeader % RealC(RC_LongSpacing)
ELSE
  Cx % Header % StartLong = MINVAL (UmHeader % ColDepC(1:UMHeader % IntC(IC_XLen)))
  Cx % Header % StartLat = MINVAL (UmHeader % RowDepC(1:UMHeader % IntC(IC_YLen)))
  Cx % Header % LatInterval = RMDI
  Cx % Header % LongInterval = RMDI
END IF
IF (SIZE (UmHeader % RealC) >= RC_z_ModelTop) THEN
  Cx % Header % ZModelTop = UmHeader % RealC(RC_z_ModelTop)
  Cx % Header % PModelTop = UmHeader % RealC(RC_z_ModelTop+1)
ELSE
  Cx % Header % ZModelTop = RMDI
  Cx % Header % PModelTop = RMDI
END IF
Cx % Header % NumLat = UmHeader % IntC(IC_YLen)
Cx % Header % NumLong = UmHeader % IntC(IC_XLen)
Cx % Header % NumLevels = NumCXLev
Cx % Header % NumLevelsBL = UmHeader % IntC(IC_BLevels)
Cx % Header % VertCoord = UmHeader % FixHd(FH_VertCoord) ! 5 Charney_Philips grid

!------------------------------------------------
! 2. Read data from Obstore header to Cx Header.
!------------------------------------------------

ALLOCATE (Cx % Id(Obs % Header % NumObsLocal))
Cx % Header % NumLocal = Obs % Header % NumObsLocal
Cx % Header % NumTotal = Obs % Header % NumObsTotal
Cx % Header % PoleLat = UmHeader % RealC(RC_PoleLat)
Cx % Header % PoleLong = UmHeader % RealC(RC_PoleLong)
Cx % Header % ValidityTime = Obs % Header % ValidityTime
IF (UmHeader % FixHd(FH_HorizGrid) > FH_HorizGrid_Eq) THEN  ! Rotated area
  Cx % Header % Rotated = .TRUE.
ELSE
  Cx % Header % Rotated = .FALSE.
END IF

!--------------------------------
! 3. Store CX level information
!--------------------------------

IF (NumCXLev > 0) THEN
  IF (Cx % Header % SubModel == FH_SubModel_Ocean .OR. &
      Obs % header % ObsGroup == ObsGroupSeaIce .OR. &
      Obs % header % ObsGroup == ObsGroupOceanColour) THEN    ! Ocean background

    ALLOCATE (Cx % Header % Depths(NumCXLev))
    PntLevDepC = UmHeader % FixHd(FH_LevDepCSize1)

    DO Ilev = 1, NumCxLev
      Cx % Header % Depths(Ilev) = UmHeader % LevDepC(PntLevDepC + ILev + 2)
    END DO

  ELSE IF (Cx % Header % NewDynamics) THEN
    IF (ModelType == ModelType_Atmos) THEN
      n = UmHeader % FixHd(FH_LevDepCSize1) - 2
      ALLOCATE (Cx % Header % EtaTheta(n))
      ALLOCATE (Cx % Header % EtaRho(n))

      PntLevDepC = UmHeader % FixHd(FH_LevDepCSize1) + 3
      Cx % Header % EtaTheta(:) = UmHeader % LevDepC(PntLevDepC: PntLevDepC + n - 1)

      PntLevDepC = 2 * UmHeader % FixHd(FH_LevDepCSize1) + 3
      Cx % Header % EtaRho(:) = UmHeader % LevDepC(PntLevDepC: PntLevDepC + n - 1)
    END IF
  ELSE
    ALLOCATE (Cx % Header % AK(NumCxLev))
    ALLOCATE (Cx % Header % BK(NumCxLev))
    ALLOCATE (Cx % Header % AKH(NumCxLev + 1))
    ALLOCATE (Cx % Header % BKH(NumCxLev + 1))

    Size1 = UmHeader % FixHd(FH_LevDepCSize1) + 3
    Cx % Header % AK(1:NumCxLev) = UmHeader % LevDepC(Size1:Size1 + NumCxLev - 1)
    Size1 = Size1 + UmHeader % FixHd(FH_LevDepCSize1)
    Cx % Header % BK(1:NumCxLev) = UmHeader % LevDepC(Size1: Size1 + NumCxLev - 1)
    Cx % Header % AKH(1) = 0.0
    Cx % Header % BKH(1) = 1.0
    Size1 = Size1 + UmHeader % FixHd(FH_LevDepCSize1)
    DO Ilev = 1, NumCxLev
      Cx % Header % AKH(Ilev + 1) = Cx % Header % AKH(Ilev) + &
                                      UmHeader % LevDepC(Size1 + ILev - 1)
      Cx % Header % BKH(Ilev + 1) = Cx % Header % BKH(Ilev) + &
             UmHeader % LevDepC (Size1 + UmHeader % FixHd(FH_LevDepCSize1) + Ilev - 1)
    END DO

    IF (LevRef <= NumCxLev) THEN
      ! Set LevelAboveBL
      Pref = Cx % Header % AK(LevRef) + Cx % Header % BK(LevRef) * 100000.0
      DO ilev = 1, NumCxLev
        Cx % Header % LevelAboveBL = ilev
        Eta = Cx % Header % AK(ilev) / Pref + Cx % Header % BK(ilev)
        IF (Eta <= Eta_ref) EXIT
      END DO
    ELSE
      WRITE (ErrMess(1), '(A,I0)') 'NumCxLev less than LevRef. NumCxLev =', &
                                   NumCxLev
      CALL gen_warn (RoutineName, &
                     ErrMess(1))
      Cx % Header % LevelAboveBL = IMDI
    END IF

  END IF

  CALL Ops_CXP (Cx)
END IF ! NumCxLevels

!---------------------------------------------------------------------------
! 4. If present, keep the orographic height and sea-ice fraction, and
!    set up the model surface type and forecast length, to use in Ops_Final.
!---------------------------------------------------------------------------

! First set sea-ice fraction to missing when outside [0,1]
IF (Cx % Header % SeaIce % Present)  THEN
  WHERE (CX % SeaIce(:) < 0.0 .OR. CX % SeaIce(:) > 1.0) CX % SeaIce(:) = RMDI
END IF

IF (Cx % Header % orog % Present .AND. &
    .NOT.(Obs % Header % ObsGroup == ObsGroupRadar .OR. &
          Obs % Header % ObsGroup == ObsGroupRadarZ))  THEN
  Obs % ModelOrog(:) = CX % orog(:)
  ! See BUFR flag table table 008204 for Model Surface Types:
  WHERE (Obs % ModelOrog(:)  ==  0.0)
    Obs % ModelSurface(:) = 0.0    ! Assume SfcHt=0 => a sea point
  ELSEWHERE
    Obs % ModelSurface(:) = 3.0    ! Assume SfcHt/=0 => a land point
  END WHERE

  IF (Cx % Header % SeaIce % Present)  THEN
    WHERE (Obs % ModelSurface(:) == 0.0 .AND. CX % SeaIce(:) < 1.1 .AND. CX  % SeaIce(:) > 0.0)
      Obs % ModelSurface(:) = 6.0  ! A sea-ice point
    END WHERE
    ! Copy sea-ice
    IF (Obs % Header % ObsGroup == ObsGroupGeoCloud .OR. &
        OpsFn_IsSatRadGroup(Obs % Header % ObsGroup))  THEN
      Obs % ModelSeaIce(:) = CX % SeaIce(:)
    END IF
  ELSE IF (GeneralMode >= DebugMode)  THEN
    ErrMess(1) = 'BackGrCX % SeaIce  not present, so'
    ErrMess(2) = 'ModelSurface is only land or sea'
    CALL gen_warn (RoutineName,  &
                   ErrMess(1:2))
  END IF

END IF

IF (ModelType == ModelType_SST) THEN
  IF (Cx % Header % SeaIce % Present) THEN
    WHERE (CX  % SeaIce(:) > seaice_theshold_for_sst)
      Obs % ModelSurface(:) = 6.0  ! A sea-ice point
    END WHERE
  END IF
END IF

IF (Cx % Header % CloudAmount % Present) THEN
  CALL ops_alloc (Obs % Header % ModelCloudAmount, &
                  "Ob % ModelCloudAmount",         &
                  Obs % Header % NumObsLocal,      &
                  Obs % ModelCloudAmount)
  Obs % ModelCloudAmount(:) = CX % CloudAmount(:)
END IF    

Obs % Header % ModelForecastLength = IMDI
IF (UmHeader % FixHd(FH_LookupSize2) > 0) THEN ! Some Lookup Tables present
  IF (UmHeader % Lookup (LBFT,1) /= IMDI) THEN ! Forecast Time set to length in mins
    Obs % Header % ModelForecastLength = UMHeader % Lookup (LBFT,1) * 60
  END IF
END IF

IF (Obs % Header % ObsGroup == ObsGroupSBUV .AND. &
    .NOT. Cx % Header % PlevelsA % Present) THEN
  CALL Ops_CXP (CX)
  WRITE (MessageOut, '(A)', ADVANCE = 'NO') 'PlevelsA (SBUV) = '
  DO Ilev = 1, SIZE (Cx % PLevelsA, DIM = 2)
    WRITE (MessageOut, '(F19.7)', ADVANCE = 'NO') Cx % PlevelsA(1,Ilev)
  END DO
  WRITE (MessageOut, '(A)')
END IF

IF (Obs % Header % ObsGroup == ObsGroupSurface .AND. &
    Cx % Header % aerosol_p % Present) THEN
  CALL Ops_Alloc (Cx % Header % Aerosol,  &
                  'Cx % Aerosol',         &
                  Cx % Header % NumLocal, &
                  Cx % Aerosol)
  Cx % Aerosol(:) = Cx % Aerosol_p(:,1)
END IF

IF (GeneralMode >= VerboseMode) THEN
  IF (mype == 0) THEN
    WRITE (MessageOut, '(4A)') ' For ', TRIM (Obs % Header % ObsGroupName), ' in ', RoutineName
    WRITE (MessageOut, '(A)') ' CX structure '
  END IF
  CALL Cx % print
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CxComplete
