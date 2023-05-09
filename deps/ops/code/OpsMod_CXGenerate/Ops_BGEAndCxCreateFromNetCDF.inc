!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Top level routine to read either the BG or BGE from ocean NetCDF files,
! perform horizontal and time interpolation of a UM dump, fill an array of CX
! structures and complete any BG/BGE specific code required.
!
! Method:
!      0. Tracing and Initialisations
!      1. Set up control information for UM Ffile extraction
!      2. Read BGE or UM dump and Generate BGE or Cx files using top level
!         reading routine Ops_CXGenerate
!      3. Complete any file/obgroup specific info
!-------------------------------------------------------------------------------

SUBROUTINE Ops_BGEAndCxCreateFromNetCDF (Obs,   &
                                         CX,    &
                                         BGECX)

USE GenMod_Control, ONLY: &
    GeneralMode,          &
    DebugMode

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    MessageOut,        &
    UseTrace

USE OpsMod_CXInfo, ONLY: &
    CX_Type

USE OpsMod_DateTime, ONLY:   &
    OpsFn_DateTime_ToMinUTC

USE OpsMod_HorizontalInterp, ONLY:  &
    Ops_HorizontalInterpFromNetCDF

USE OpsMod_MiscTypes, ONLY: &
    ZCodeLevelsA

USE OpsMod_ModelIO, ONLY:            &
    Ops_FindTimeCoordIndices,        &
    Ops_PartitionedFindCoordIndices, &
    Ops_DeallocateOceanNetCDFFields, &
    OceanPresearchGrid,              &
    OceanNetCDFFields

USE OpsMod_ObsGroupInfo, ONLY: &
    ObsGroupOcean,             &
    ObsGroupSurface,           &
    ObsGroupAltim,             &
    ObsGroupSeaIce,            &
    ObsGroupOceanColour,       &
    ObsGroupSatSST

USE OpsMod_ObsInfo, ONLY: &
    OB_type,              &
    Ops_Alloc

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_Type), INTENT(INOUT) :: Obs
TYPE (CX_type), INTENT(INOUT) :: CX
TYPE (CX_type), INTENT(INOUT) :: BGECX

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName = "Ops_BGEAndCxCreateFromNetCDF"
INTEGER, ALLOCATABLE          :: ts(:)
INTEGER, ALLOCATABLE          :: is(:)
INTEGER, ALLOCATABLE          :: js(:)
INTEGER                       :: i
INTEGER                       :: model_time_offset
INTEGER                       :: obs_time_offset
INTEGER                       :: num_levels

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CX % header % ObsGroup = Obs % header % ObsGroup
CALL CX % init
CX % Header % NumLocal = Obs % Header % NumObsLocal
CX % Header % NumTotal = Obs % Header % NumObsTotal
CX % Header % ModelVersion = 0
CX % Header % SubModel = 0
CX % Header % NewDynamics = .TRUE.

BGECX % header % ObsGroup = Obs % header % ObsGroup
CALL BGECX % init
BGECX % Header % NumLocal = Obs % Header % NumObsLocal
BGECX % Header % NumTotal = Obs % Header % NumObsTotal
BGECX % Header % ModelVersion = 0
BGECX % Header % SubModel = 0
BGECX % Header % NewDynamics = .TRUE.

IF (Obs % header % ObsGroup == ObsGroupOcean) THEN
  num_levels = SIZE (OceanNetcdfFields % nav_lev)
ELSE
  num_levels = 3
END IF

IF (Obs % header % ObsGroup == ObsGroupSatSST .OR. &
    Obs % header % ObsGroup == ObsGroupSurface .OR. &
    Obs % header % ObsGroup == ObsGroupOcean) THEN
  CALL Ops_Alloc (CX % header % theta,        &
                  "CX % theta",               &
                  Obs % header % NumObsLocal, &
                  CX % theta,                 &
                  num_levels = num_levels,    &
                  zcode = ZCodeLevelsA)
  CALL Ops_Alloc (BGECX % header % TempOb_Err, &
                  "BGECX % TempOb_Err",        &
                  Obs % header % NumObsLocal,  &
                  BGECX % TempOb_Err,          &
                  num_levels = num_levels,     &
                  zcode = ZCodeLevelsA)
  CALL Ops_Alloc (BGECX % header % theta,     &
                  "BGECX % theta",            &
                  Obs % header % NumObsLocal, &
                  BGECX % theta,              &
                  num_levels = num_levels,    &
                  zcode = ZCodeLevelsA)
  CALL Ops_Alloc (BGECX % header % TempMes_HCor, &
                  "BGECX % TempMes_HCor",        &
                  Obs % header % NumObsLocal,    &
                  BGECX % TempMes_HCor,          &
                  num_levels = num_levels,       &
                  zcode = ZCodeLevelsA)
  CALL Ops_Alloc (BGECX % header % TempSyn_HCor, &
                  "BGECX % TempSyn_HCor",        &
                  Obs % header % NumObsLocal,    &
                  BGECX % TempSyn_HCor,          &
                  num_levels = num_levels,       &
                  zcode = ZCodeLevelsA)
  CALL Ops_Alloc (BGECX % header % TempMes_Var, &
                  "BGECX % TempMes_Var",        &
                  Obs % header % NumObsLocal,   &
                  BGECX % TempMes_Var,          &
                  num_levels = num_levels,      &
                  zcode = ZCodeLevelsA)
  CALL Ops_Alloc (BGECX % header % TempSyn_Var, &
                  "BGECX % TempSyn_Var",        &
                  Obs % header % NumObsLocal,   &
                  BGECX % TempSyn_Var,          &
                  num_levels = num_levels,      &
                  zcode = ZCodeLevelsA)
END IF

IF (Obs % header % ObsGroup == ObsGroupOcean) THEN
  CALL Ops_Alloc (CX % header % salt,         &
                  "CX % salt",                &
                  Obs % header % NumObsLocal, &
                  CX % salt,                  &
                  num_levels = num_levels,    &
                  zcode = ZCodeLevelsA)
  CALL Ops_Alloc (CX % header % PLevelsA,     &
                  "CX % PLevelsA",            &
                  Obs % header % NumObsLocal, &
                  CX % PLevelsA,              &
                  num_levels = num_levels)
  CALL Ops_Alloc (BGECX % header % SalOb_Err, &
                  "BGECX % SalOb_Err",        &
                  Obs % header % NumObsLocal, &
                  BGECX % SalOb_Err,          &
                  num_levels = num_levels,    &
                  zcode = ZCodeLevelsA)
  CALL Ops_Alloc (BGECX % header % SalMes_Var, &
                  "BGECX % SalMes_Var",        &
                  Obs % header % NumObsLocal,  &
                  BGECX % SalMes_Var,          &
                  num_levels = num_levels,     &
                  zcode = ZCodeLevelsA)
  CALL Ops_Alloc (BGECX % header % SalSyn_Var, &
                  "BGECX % SalSyn_Var",        &
                  Obs % header % NumObsLocal,  &
                  BGECX % SalSyn_Var,          &
                  num_levels = num_levels,     &
                  zcode = ZCodeLevelsA)
  CALL Ops_Alloc (BGECX % header % SalMes_HCor,  &
                  "BGECX % SalMes_HCor",         &
                  Obs % header % NumObsLocal,    &
                  BGECX % SalMes_HCor,           &
                  num_levels = num_levels,       &
                  zcode = ZCodeLevelsA)
  CALL Ops_Alloc (BGECX % header % SalSyn_HCor, &
                  "BGECX % SalSyn_HCor",        &
                  Obs % header % NumObsLocal,   &
                  BGECX % SalSyn_HCor,          &
                  num_levels = num_levels,      &
                  zcode = ZCodeLevelsA)
  CALL Ops_Alloc (BGECX % header % Salt,      &
                  "BGECX % Salt",             &
                  Obs % header % NumObsLocal, &
                  BGECX % Salt,               &
                  num_levels = num_levels,    &
                  zcode = ZCodeLevelsA)
  CALL Ops_Alloc (BGECX % header % PLevelsA,  &
                  "BGECX % PLevelsA",         &
                  Obs % header % NumObsLocal, &
                  BGECX % PLevelsA,           &
                  num_levels = num_levels)

  CALL Ops_Alloc (Obs % header % background_temperature,  &
                  "Obs % background_temperature",         &
                  Obs % header % NumObsLocal,             &
                  Obs % background_temperature,           &
                  num_levels = Obs % header % t % NumLev)
  CALL Ops_Alloc (Obs % header % background_salt,            &
                  "Obs % background_salt",                   &
                  Obs % header % NumObsLocal,                &
                  Obs % background_salt,                     &
                  num_levels = Obs % header % Salt % NumLev)

ELSE IF (Obs % header % ObsGroup == ObsGroupSatSST .OR. &
         Obs % header % ObsGroup == ObsGroupSurface) THEN
  CALL Ops_Alloc (CX % header % SST,          &
                  "CX % SST",                 &
                  Obs % header % NumObsLocal, &
                  CX % SST)
  CALL Ops_Alloc (BGECX % header % SST,       &
                  "BGECX % SST",              &
                  Obs % header % NumObsLocal, &
                  BGECX % SST)
  CALL Ops_Alloc (BGECX % header % SSTOb_err, &
                  "BGECX % SSTOb_err",        &
                  Obs % header % NumObsLocal, &
                  BGECX % SSTOb_err)
  CALL Ops_Alloc (BGECX % header % SSTMes_HCor, &
                  "BGECX % SSTMes_HCor",        &
                  Obs % header % NumObsLocal,   &
                  BGECX % SSTMes_HCor)
  CALL Ops_Alloc (BGECX % header % SSTSyn_HCor, &
                  "BGECX % SSTSyn_HCor",        &
                  Obs % header % NumObsLocal,   &
                  BGECX % SSTSyn_HCor)
  CALL Ops_Alloc (BGECX % header % SSTMes_Var, &
                  "BGECX % SSTMes_Var",        &
                  Obs % header % NumObsLocal,  &
                  BGECX % SSTMes_Var)
  CALL Ops_Alloc (BGECX % header % SSTSyn_Var, &
                  "BGECX % SSTSyn_Var",        &
                  Obs % header % NumObsLocal,  &
                  BGECX % SSTSyn_Var)
ELSE IF (Obs % header % ObsGroup == ObsGroupAltim) THEN
  CALL Ops_Alloc (CX % header % SeaSrfcHeight, &
                  "CX % SeaSrfcHeight",        &
                  Obs % header % NumObsLocal,  &
                  CX  % SeaSrfcHeight)
  CALL Ops_Alloc (BGECX % header % SeaSrfcHeight, &
                  "BGECX % SeaSrfcHeight",        &
                  Obs % header % NumObsLocal,     &
                  BGECX % SeaSrfcHeight)
  CALL Ops_Alloc (BGECX % header % MeanSeaHeight, &
                  "BGECX % MeanSeaHeight",        &
                  Obs % header % NumObsLocal,     &
                  BGECX % MeanSeaHeight)
  CALL Ops_Alloc (BGECX % header % SSHOb_err, &
                  "BGECX % SSHOb_err",        &
                  Obs % header % NumObsLocal, &
                  BGECX % SSHOb_err)
  CALL Ops_Alloc (BGECX % header % SSHMes_HCor, &
                  "BGECX % SSHMes_HCor",        &
                  Obs % header % NumObsLocal,   &
                  BGECX % SSHMes_HCor)
  CALL Ops_Alloc (BGECX % header % SSHSyn_HCor, &
                  "BGECX % SSHSyn_HCor",        &
                  Obs % header % NumObsLocal,   &
                  BGECX % SSHSyn_HCor)
  CALL Ops_Alloc (BGECX % header % SSHMes_Var, &
                  "BGECX % SSHMes_Var",        &
                  Obs % header % NumObsLocal,  &
                  BGECX % SSHMes_Var)
  CALL Ops_Alloc (BGECX % header % SSHSyn_Var, &
                  "BGECX % SSHSyn_Var",        &
                  Obs % header % NumObsLocal,  &
                  BGECX % SSHSyn_Var)
ELSE IF (Obs % header % ObsGroup == ObsGroupSeaIce) THEN
  CALL Ops_Alloc (CX % header % SeaIce,       &
                  "CX % SeaIce",              &
                  Obs % header % NumObsLocal, &
                  CX  % SeaIce)
  CALL Ops_Alloc (BGECX % header % SeaIce,    &
                  "BGECX % SeaIce",           &
                  Obs % header % NumObsLocal, &
                  BGECX % SeaIce)
  CALL Ops_Alloc (BGECX % header % SeaIceOb_err, &
                  "BGECX % SeaIceOb_err",        &
                  Obs % header % NumObsLocal,    &
                  BGECX % SeaIceOb_err)
  CALL Ops_Alloc (BGECX % header % SeaIceMes_HCor, &
                  "BGECX % SeaIceMes_HCor",        &
                  Obs % header % NumObsLocal,      &
                  BGECX % SeaIceMes_HCor)
  CALL Ops_Alloc (BGECX % header % SeaIceSyn_HCor, &
                  "BGECX % SeaIceSyn_HCor",        &
                  Obs % header % NumObsLocal,      &
                  BGECX % SeaIceSyn_HCor)
  CALL Ops_Alloc (BGECX % header % SeaIceMes_Var, &
                  "BGECX % SeaIceMes_Var",        &
                  Obs % header % NumObsLocal,     &
                  BGECX % SeaIceMes_Var)
  CALL Ops_Alloc (BGECX % header % SeaIceSyn_Var, &
                  "BGECX % SeaIceSyn_Var",        &
                  Obs % header % NumObsLocal,     &
                  BGECX % SeaIceSyn_Var)
ELSE IF (Obs % header % ObsGroup == ObsGroupOceanColour) THEN

  IF (ALLOCATED (OceanNetCDFFields % schltot)) THEN
    CALL Ops_Alloc (CX % header % CHL,          &
                    "CX % CHL",                 &
                    Obs % header % NumObsLocal, &
                    CX  % CHL)
  END IF
  IF (ALLOCATED (OceanNetCDFFields % skd490tot)) THEN
    CALL Ops_Alloc (CX % header % KD490,        &
                    "CX % KD490",               &
                    Obs % header % NumObsLocal, &
                    CX  % KD490)
  END IF
  IF (ALLOCATED (OceanNetCDFFields % chl_obs_var)) THEN
    CALL Ops_Alloc (BGECX % header % CHLOb_Err, &
                    "BGECX % CHLOb_Err",        &
                    Obs % header % NumObsLocal, &
                    BGECX  % CHLOb_Err)
  END IF
  IF (ALLOCATED (OceanNetCDFFields % lchl_obs_var)) THEN
    CALL Ops_Alloc (BGECX % header % LCHLOb_Err, &
                    "BGECX % LCHLOb_Err",        &
                    Obs % header % NumObsLocal,  &
                    BGECX  % LCHLOb_Err)
  END IF
  IF (ALLOCATED (OceanNetCDFFields % chl_tot_var)) THEN
    CALL Ops_Alloc (BGECX % header % CHL,       &
                    "BGECX % CHL",              &
                    Obs % header % NumObsLocal, &
                    BGECX  % CHL)
  END IF
  IF (ALLOCATED (OceanNetCDFFields % lchl_tot_var)) THEN
    CALL Ops_Alloc (BGECX % header % LCHL,      &
                    "BGECX % LCHL",             &
                    Obs % header % NumObsLocal, &
                    BGECX  % LCHL)
  END IF
  IF (ALLOCATED (OceanNetCDFFields % kd490_obs_var)) THEN
    CALL Ops_Alloc (BGECX % header % KD490Ob_Err, &
                    "BGECX % KD490Ob_Err",        &
                    Obs % header % NumObsLocal,   &
                    BGECX  % KD490Ob_Err)
  END IF
  IF (ALLOCATED (OceanNetCDFFields % lkd490_obs_var)) THEN
    CALL Ops_Alloc (BGECX % header % LKD490Ob_Err, &
                    "BGECX % LKD490Ob_Err",        &
                    Obs % header % NumObsLocal,    &
                    BGECX  % LKD490Ob_Err)
  END IF
  IF (ALLOCATED (OceanNetCDFFields % kd490_tot_var)) THEN
    CALL Ops_Alloc (BGECX % header % KD490,     &
                    "BGECX % KD490",            &
                    Obs % header % NumObsLocal, &
                    BGECX  % KD490)
  END IF
  IF (ALLOCATED (OceanNetCDFFields % lkd490_tot_var)) THEN
    CALL Ops_Alloc (BGECX % header % LKD490,    &
                    "BGECX % LKD490",           &
                    Obs % header % NumObsLocal, &
                    BGECX  % LKD490)
  END IF
END IF

ALLOCATE (ts(obs % header % numobslocal))
ALLOCATE (OceanNetCDFFields % time_counter_adj(OceanNetCDFFields % netcdf_num_bkg_times))

model_time_offset = 60 * OpsFn_DateTime_ToMinUTC (OceanNetCDFFields % ref_bkg_time)
obs_time_offset = 60 * OpsFn_DateTime_ToMinUTC (Obs % header % ValidityTime)

OceanNetCDFFields % time_counter_adj(:) = NINT (OceanNetCDFFields % time_counter(:) + &
                                                model_time_offset - obs_time_offset)

IF (GeneralMode >= DebugMode) THEN
  WRITE (MessageOut, '(A,3(I4,A))') TRIM (RoutineName) // ": ref bkg time: ", &
                                    OceanNetCDFFields % ref_bkg_time % year, "-", &
                                    OceanNetCDFFields % ref_bkg_time % month, "-", &
                                    OceanNetCDFFields % ref_bkg_time % day
  WRITE (MessageOut, '(A,3(I4,A))') TRIM (RoutineName) // ": obs time: ", &
                                    Obs % header % ValidityTime % month, "-", &
                                    Obs % header % ValidityTime % month, "-", &
                                    Obs % header % ValidityTime % day

  WRITE (MessageOut, '(A,I0)') "model offset to 1970-01-01 in seconds: ", model_time_offset
  WRITE (MessageOut, '(A,I0)') "obs offset to 1970-01-01 in seconds: ", obs_time_offset
END IF

CALL Ops_FindTimeCoordIndices (OceanNetCDFFields % time_counter_adj, &
                               Obs % Time,                           &
                               ts)

ALLOCATE (is(obs % header % numobslocal))
ALLOCATE (js(obs % header % numobslocal))

CALL Ops_PartitionedFindCoordIndices (OceanPresearchGrid,          &
                                      OceanNetCDFFields % nav_lon, &
                                      OceanNetCDFFields % nav_lat, &
                                      Obs % Longitude,             &
                                      Obs % Latitude,              &
                                      is,                          &
                                      js)


IF (Obs % header % ObsGroup == ObsGroupOcean) THEN
  DO i = 1, num_levels
    CX % PLevelsA(:,i) = OceanNetCDFFields % nav_lev(i)
    BGECX % PLevelsA(:,i) = OceanNetCDFFields % nav_lev(i)
  END DO
END IF

CALL Ops_HorizontalInterpFromNetCDF (Obs,               &
                                     CX,                &
                                     BGECX,             &
                                     ts,                &
                                     is,                &
                                     js,                &
                                     OceanNetCDFFields)

CALL Ops_DeallocateOceanNetCDFFields (OceanNetCDFFields)

CALL CX % print
CALL BGECX % print

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_BGEAndCxCreateFromNetCDF
