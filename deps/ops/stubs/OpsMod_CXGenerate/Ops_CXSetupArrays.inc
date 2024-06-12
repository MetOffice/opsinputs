!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Called by Ops_CXGenerate to set up both the FieldsInfo and CxArrays arrays.
! Also allocates arrays in the Cx structure.
!
! The arrays to be filled within the CX structure are allocated, and an array of
! structures (CXArray_Type) is prepared which points to each field in the cx
! structure.  Also, the positions and number of levels of each field within the
! UM dump is determined.
!
! Inputs:
!     UMHdr: The header from the UM dump, required for field information.
!     ForecastTimes: The forecast times required for time interpolation.
!     Obs: Array of structures containing observations for each obsgroup,
!       required for observation locations.
!     RequiredFields: The stashcodes required for each group (Group,Field)
!     NumLevelsRequired: The number of levels required for each field
!       (Group,Field)
!     ModelTimeUTC: Validity time UTC in the UM dump.
!
! Outputs:
!     CXs: Array of cx structures for each obsgroup, with required arrays
!       allocated.
!     FieldsInfo:
!     CxArrays: Array of structures
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CXSetupArrays (UMHdr,             &
                              ForecastTimes,     &
                              Obs,               &
                              RequiredFields,    &
                              NumLevelsRequired, &
                              CXs,               &
                              CXArrays,          &
                              FieldsInfo,        &
                              BGECall,           &
                              ModelTimeUTC)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE GenMod_ModelIO, ONLY: &
  UM_Header_Type

USE GenMod_UMHeaderConstants, ONLY: &
  FH_ModelVersion

USE OpsMod_AODGeneral, ONLY: &
  NDustBins

USE OpsMod_CXInfo, ONLY: &
  Cx_Type

USE OpsMod_MiscTypes, ONLY: &
  ZCodeSurface,             &
  ZCodeLevelsA,             &
  ZCodeLevelsB

USE OpsMod_ObsGroupInfo, ONLY: &
  ObsGroupAOD

USE OpsMod_ObsInfo, ONLY: &
  Ob_Type

USE OpsMod_Stash

IMPLICIT NONE

! Subroutine arguments:
TYPE (UM_Header_Type), INTENT(IN)     :: UMHdr
INTEGER, INTENT(IN)                   :: ForecastTimes(:)
TYPE (Ob_Type), INTENT(IN)            :: Obs(:)
INTEGER, INTENT(IN)                   :: RequiredFields(:,:)
INTEGER, INTENT(IN)                   :: NumLevelsRequired(:)
TYPE (Cx_Type), TARGET, INTENT(INOUT) :: Cxs(:)
TYPE (CXArray_type), INTENT(INOUT)    :: CXArrays(:,:)
TYPE (FieldInfo_Type), INTENT(INOUT)  :: FieldsInfo(:)
LOGICAL, INTENT(IN)                   :: BGECall
INTEGER, INTENT(IN)                   :: ModelTimeUTC(:)

! Local declarations:
TYPE (Cx_Type), POINTER               :: Cx
TYPE (Cx_Type), TARGET                :: CxDummy
INTEGER                               :: iField
INTEGER                               :: ZCode
INTEGER                               :: NumForecastTimes
INTEGER                               :: DustInd             ! loop over dust fields
INTEGER                               :: StashInd            ! loop over dust stash indices
INTEGER                               :: iGroup
CHARACTER(len=*), PARAMETER           :: RoutineName = 'Ops_CXSetupArrays'

IF (UseTrace) CALL gen_trace_entry (RoutineName)

NumForecastTimes = COUNT (ForecastTimes(:) /= IMDI)

CALL CxDummy % init

DO iGroup = -1, SIZE (Obs)

  IF (iGroup == -1) THEN
    ! Pass to count number of fields required
    Cx => CxDummy
  ELSE IF (iGroup > 0) THEN
    ! passes to set up CX array info
    CX => Cxs(iGroup)
  END IF

  iField = 0

  ! Single level arrays
  !---------------------

  ZCode = ZCodeSurface

  CALL Ops_CXAddField (StashItem_Orog,     &
                       Cx % Header % Orog, &
                       R1 = CX % Orog)
  CALL Ops_CXAddField (StashItem_Pstar,     &
                       Cx % Header % Pstar, &
                       R1 = CX % Pstar)
  CALL Ops_CXAddField (StashItem_P_surface, &
                       Cx % Header % Pstar, &
                       R1 = CX % Pstar)
  CALL Ops_CXAddField (StashCode_T2,     &
                       Cx % Header % T2, &
                       R1 = CX % T2)
  CALL Ops_CXAddField (StashCode_RH2,     &
                       Cx % Header % RH2, &
                       R1 = CX % RH2)
  CALL Ops_CXAddField (StashCode_U10_B_grid, &
                       Cx % Header % U10,    &
                       R1 = CX % U10,        &
                       VectorField = .TRUE.)
  CALL Ops_CXAddField (StashCode_V10_B_grid, &
                       Cx % Header % V10,    &
                       R1 = CX % V10,        &
                       VectorField = .TRUE.)
  CALL Ops_CXAddField (StashCode_U10,        &
                       Cx % Header % U10,    &
                       R1 = CX % U10,        &
                       VectorField = .TRUE.)
  CALL Ops_CXAddField (StashCode_V10,        &
                       Cx % Header % V10,    &
                       R1 = CX % V10,        &
                       VectorField = .TRUE.)
  CALL Ops_CXAddField (StashCode_Vis,     &
                       Cx % Header % Vis, &
                       R1 = CX % Vis)
  CALL Ops_CXAddField (StashCode_Pmsl,     &
                       Cx % Header % Pmsl, &
                       R1 = CX % Pmsl)
  CALL Ops_CXAddField (StashCode_qt2,     &
                       Cx % Header % qt2, &
                       R1 = CX % qt2)
  CALL Ops_CXAddField (StashCode_aerosol,     &
                       Cx % Header % logm, &
                       R1 = CX % logm) ! note called logm not aerosol
  CALL Ops_CXAddField (StashItem_SnowAmount,     &
                       Cx % Header % SnowAmount, &
                       R1 = CX % SnowAmount)
  CALL Ops_CXAddField (StashCode_WAVE_HGHT,     &
                       Cx % Header % WAVE_HGHT, &
                       R1 = CX % WAVE_HGHT)
  CALL Ops_CXAddField (StashCode_WIND_SPED,     &
                       Cx % Header % WIND_SPED, &
                       R1 = CX % WIND_SPED)
  CALL Ops_CXAddField (StashCode_AOD,     &
                       Cx % Header % AOD, &
                       R2 = CX % AOD)
  CALL Ops_CXAddField (StashCode_SurfRainRate_conv,     &
                       Cx % Header % SurfRainRate_conv, &
                       R1 = CX % SurfRainRate_conv)
  CALL Ops_CXAddField (StashCode_SurfSnowRate_conv,     &
                       Cx % Header % SurfSnowRate_conv, &
                       R1 = CX % SurfSnowRate_conv)
  CALL Ops_CXAddField (StashCode_SurfRainRate_LS,     &
                       Cx % Header % SurfRainRate_LS, &
                       R1 = CX % SurfRainRate_LS)
  CALL Ops_CXAddField (StashCode_SurfSnowRate_LS,     &
                       Cx % Header % SurfSnowRate_LS, &
                       R1 = CX % SurfSnowRate_LS)
  CALL Ops_CXAddField (StashCode_PSurfParamA,     &
                       Cx % Header % PSurfParamA, &
                       R1 = CX % PSurfParamA,     &
                       DerivedField = .TRUE.)
  CALL Ops_CXAddField (StashCode_PSurfParamB,     &
                       Cx % Header % PSurfParamB, &
                       R1 = CX % PSurfParamB,     &
                       DerivedField = .TRUE.)
  CALL Ops_CXAddField (StashCode_LapseRate,     &
                       Cx % Header % LapseRate, &
                       R1 = CX % LapseRate,     &
                       DerivedField = .TRUE.)
  CALL Ops_CXAddField (StashCode_CloudAmount,     &
                       Cx % Header % CloudAmount, &
                       R1 = CX % CloudAmount)
  CALL Ops_CXAddField (StashItem_ConvCloudAmount,     &
                       Cx % Header % ConvCloudAmount, &
                       R1 = CX % ConvCloudAmount)
  CALL Ops_CXAddField (StashItem_ConvCloudBase,          &
                       Cx % Header % ConvCloudBaseLevel, &
                       R1 = CX % ConvCloudBaseLevel)
  CALL Ops_CXAddField (StashItem_ConvCloudTop,          &
                       Cx % Header % ConvCloudTopLevel, &
                       R1 = CX % ConvCloudTopLevel)
  CALL Ops_CXAddField (StashCode_SWradiation,     &
                       Cx % Header % SWradiation, &
                       R1 = CX % SWradiation)
  CALL Ops_CXAddField (StashItem_BLheight,     &
                       Cx % Header % BLheight, &
                       R1 = CX % BLheight)
  CALL Ops_CXAddField (StashCode_ObukhovLength,     &
                       Cx % Header % ObukhovLength, &
                       R1 = CX % ObukhovLength)
  CALL Ops_CXAddField (StashCode_FrictionVel,     &
                       Cx % Header % FrictionVel, &
                       R1 = CX % FrictionVel)
  CALL Ops_CXAddField (StashCode_PrecipAcc6hr,     &
                       Cx % Header % PrecipAcc6hr, &
                       R1 = CX % PrecipAcc6hr)
  CALL Ops_CXAddField (StashCode_LowCloudAmount,     &
                       Cx % Header % LowCloudAmount, &
                       R1 = CX % LowCloudAmount)
  CALL Ops_CXAddField (StashCode_MedCloudAmount,     &
                       Cx % Header % MedCloudAmount, &
                       R1 = CX % MedCloudAmount)
  CALL Ops_CXAddField (StashCode_LowCloudBase,     &
                       Cx % Header % LowCloudBase, &
                       R1 = CX % LowCloudBase)
  CALL Ops_CXAddField (StashCode_2p5CloudBase,     &
                       Cx % Header % LowCloudBase, &
                       R1 = CX % LowCloudBase)
  CALL Ops_CXAddField (StashItem_SeaIce,     &
                       Cx % Header % SeaIce, &
                       R1 = CX % SeaIce)
  CALL Ops_CXAddField (StashItem_SST,          &
                       Cx % Header % TSkinSea, &
                       R1 = CX % TSkinSea)
  CALL Ops_CXAddField (StashCode_PM10_AQ,     &
                       Cx % Header % PM10_AQ, &
                       R1 = CX % PM10_AQ)
  CALL Ops_CXAddField (StashCode_PM2p5_AQ,     &
                       Cx % Header % PM2p5_AQ, &
                       R1 = CX % PM2p5_AQ)
  CALL Ops_CXAddField (StashCode_O3_AQ,     &
                       Cx % Header % O3_AQ, &
                       R1 = CX % O3_AQ)
  CALL Ops_CXAddField (StashCode_NO2_AQ,     &
                       Cx % Header % NO2_AQ, &
                       R1 = CX % NO2_AQ)
  CALL Ops_CXAddField (StashCode_CO_AQ,     &
                       Cx % Header % CO_AQ, &
                       R1 = CX % CO_AQ)
  CALL Ops_CXAddField (StashCode_BLtype,     &
                       Cx % Header % BLtype, &
                       R1 = CX % BLtype)

  ! Multiple level arrays, on levelsA (rho levels) in new dynamics
  !----------------------------------------------------------------
  ZCode = ZCodeLevelsA
  ! Extra level for P? May not be a problem, I don't know...
  ! SondeDrift in CX interpolation requires p before other multi-level fields

  CALL Ops_CXAddField (StashItem_p,            &
                       Cx % Header % PLevelsA, &
                       R2 = CX % PLevelsA)
  CALL Ops_CXAddField (StashItem_p_unslanted,            &
                       Cx % Header % PLevelsA_unslanted, &
                       R2 = CX % PLevelsA_unslanted)
  CALL Ops_CXAddField (StashItem_exner,      &
                       Cx % Header % ExnerA, &
                       R2 = CX % ExnerA)
  CALL Ops_CXAddField (StashItem_u,          &
                       Cx % Header % u,      &
                       R2 = CX % u,          &
                       VectorField = .TRUE.)
  CALL Ops_CXAddField (StashItem_u_unslanted,     &
                       Cx % Header % u_unslanted, &
                       R2 = CX % u_unslanted,     &
                       VectorField = .TRUE.)
  CALL Ops_CXAddField (StashItem_v,          &
                       Cx % Header % v,      &
                       R2 = CX % v,          &
                       VectorField = .TRUE.)
  CALL Ops_CXAddField (StashItem_v_unslanted,     &
                       Cx % Header % v_unslanted, &
                       R2 = CX % v_unslanted,     &
                       VectorField = .TRUE.)
  IF (BGECall) THEN
    CALL Ops_CXAddField (StashCode_ozone_p,   &
                         Cx % Header % ozone, &
                         R2 = CX % ozone)
  ELSE
    IF (UMHdr % FixHd(FH_ModelVersion) >= 601) THEN
      StashItem_ozone = StashItem_ozone_new
    ELSE
      StashItem_ozone = StashItem_ozone_old
    END IF
    CALL Ops_CXAddField (StashItem_ozone,     &
                         Cx % Header % ozone, &
                         R2 = CX % ozone)
  END IF
  CALL Ops_CXAddField (StashCode_RichNumber,     &
                       Cx % Header % RichNumber, &
                       R2 = CX % RichNumber)
  CALL Ops_CXAddField (StashCode_SoilMoisture,     &
                       Cx % Header % SoilMoisture, &
                       R2 = CX % SoilMoisture)
  CALL Ops_CXAddField (StashCode_SoilTemp,     &
                       Cx % Header % SoilTemp, &
                       R2 = CX % SoilTemp)

  ! Multiple level arrays, on levelsB (theta levels) in new dynamics
  !------------------------------------------------------------------
  IF (iGroup > 0) THEN
    IF (Cx % Header % NewDynamics) ZCode = ZCodeLevelsB
  END IF

  CALL Ops_CXAddField (StashItem_p_bar,     &
                       Cx % Header % p_bar, &
                       R2 = CX % p_bar)
  CALL Ops_CXAddField (StashItem_exner_bar,  &
                       Cx % Header % ExnerB, &
                       R2 = CX % ExnerB)
  CALL Ops_CXAddField (StashItem_theta,     &
                       Cx % Header % Theta, &
                       R2 = CX % Theta)
  CALL Ops_CXAddField (StashItem_theta_unslanted,     &
                       Cx % Header % Theta_unslanted, &
                       R2 = CX % Theta_unslanted)
  CALL Ops_CXAddField (StashCode_rh,     &
                       Cx % Header % RH, &
                       R2 = CX % rh)
  CALL Ops_CXAddField (StashItem_q,     &
                       Cx % Header % q, &
                       R2 = CX % q)
  CALL Ops_CXAddField (StashItem_q_unslanted,     &
                       Cx % Header % q_unslanted, &
                       R2 = CX % q_unslanted)
  CALL Ops_CXAddField (StashItem_qc,     &
                       Cx % Header % qc, &
                       R2 = CX % qc)
  CALL Ops_CXAddField (StashItem_qcl,     &
                       Cx % Header % qcl, &
                       R2 = CX % qcl)
  CALL Ops_CXAddField (StashItem_qcf,     &
                       Cx % Header % qcf, &
                       R2 = CX % qcf)
  CALL Ops_CXAddField (StashItem_qrain,     &
                       Cx % Header % qrain, &
                       R2 = CX % qrain)
  CALL Ops_CXAddField (StashItem_cloud_bulk,     &
                       Cx % Header % cloud_bulk, &
                       R2 = CX % cloud_bulk)
  CALL Ops_CXAddField (StashItem_Cl,     &
                       Cx % Header % Cl, &
                       R2 = CX % Cl)
  CALL Ops_CXAddField (StashItem_Cf,     &
                       Cx % Header % Cf, &
                       R2 = CX % Cf)
  CALL Ops_CXAddField (StashCode_ql_layer,     &
                       Cx % Header % ql_layer, &
                       R2 = CX % ql_layer)
  CALL Ops_CXAddField (StashCode_qf_layer,     &
                       Cx % Header % qf_layer, &
                       R2 = CX % qf_layer)
  CALL Ops_CXAddField (StashCode_RainRate_layer,     &
                       Cx % Header % RainRate_layer, &
                       R2 = CX % RainRate_layer)
  CALL Ops_CXAddField (StashCode_cloud_layer,     &
                       Cx % Header % cloud_layer, &
                       R2 = CX % cloud_layer)
  CALL Ops_CXAddField (StashCode_cloud_conv,     &
                       Cx % Header % cloud_conv, &
                       R2 = CX % cloud_conv)
  CALL Ops_CXAddField (StashCode_qc_conv,     &
                       Cx % Header % qc_conv, &
                       R2 = CX % qc_conv)
  CALL Ops_CXAddField (StashCode_SO2_AQ,     &
                       Cx % Header % SO2_AQ, &
                       R1 = CX % SO2_AQ)
  CALL Ops_CXAddField (StashCode_u_p_B_grid, &
                       Cx % Header % u,      &
                       R2 = CX % u)
  CALL Ops_CXAddField (StashCode_t_p,   &
                       Cx % Header % t, &
                       R2 = CX % t)
  CALL Ops_CXAddField (StashCode_T,   &
                       Cx % Header % t_theta, &
                       R2 = CX % t_theta)
  CALL Ops_CXAddField (StashCode_rh_p,   &
                       Cx % Header % rh, &
                       R2 = CX % rh)
  CALL Ops_CXAddField (StashItem_w,     &
                       Cx % Header % w, &
                       R2 = CX % w)
  CALL Ops_CXAddField (StashCode_RH_AfterMainCloud, &
                       Cx % Header % RH_AMC,        &
                       R2 = CX % RH_AMC)
  CALL Ops_CXAddField (StashCode_CDNC,     &
                       Cx % Header % CDNC, &
                       R2 = CX % CDNC)
  CALL Ops_CXAddField (StashItem_Aerosol,       &
                       Cx % Header % Aerosol_p, &
                       R2 = CX % Aerosol_p)
  CALL Ops_CXAddField (StashCode_cha,     &
                       Cx % Header % cha, &
                       R1 = CX % cha)
  CALL Ops_CXAddField (StashCode_mss,     &
                       Cx % Header % mss, &
                       R1 = CX % mss)

  IF (Cx % header % ObsGroup == ObsGroupAOD .OR. iGroup <= 0) THEN
    ! dust fields
    DO DustInd = 1, NDustBins
      StashInd = StashItem_dustMin + (DustInd - 1)
      CALL Ops_CXAddField (StashInd,                         &
                           Cx % Header % dustp,              &
                           R2 = CX % dustp(DustInd) % field)
    END DO
  END IF

END DO

CALL CxDummy % deallocate

IF (UseTrace) CALL gen_trace_exit (RoutineName)

CONTAINS

INCLUDE 'Ops_CXAddField.inc'

END SUBROUTINE Ops_CXSetupArrays
