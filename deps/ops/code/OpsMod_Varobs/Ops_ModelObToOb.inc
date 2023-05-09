!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! To take a ModelOb_type structure and put the information in this into an
! Ob_type structure.  This requires the current observations to provide various
! header and general information.
!
! Inputs: The observations and model observations
!
! Outputs: The model observations in an observation structure
!
! Method: The output model observations is assumed to be an uninitialised
! structure.  The routine goes through all the variables in the model ob
! structure, which can be transferred to the ob_type structure (those variables
! which exist in both structures).  The arrays to contain the data in the output
! structure are allocated and the data loaded into these arrays.  The errors and
! PGEs are set to zero.
!
! Note: This routine will need to be updated if the OB_type or ModelOb_Type
! structures are changed.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ModelObToOb (Observations,      & ! The current observation structure (gives header info)
                            ModelOb,           & ! The model observation to be copying
                            ModelObservations, & ! The model observations in the observation structure
                            RepObs)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE OpsMod_GPSRO, ONLY: &
    GPSRO_TPD

USE OpsMod_MiscTypes, ONLY: &
    Element_Type

USE OpsMod_ModelObInfo, ONLY: &
    ModelOB_Type

USE OpsMod_ObsGroupInfo, ONLY: &
    OpsFn_IsClrGeoGroup,       &
    OpsFn_IsSatRadGroup

USE OpsMod_ObsInfo, ONLY: &
    OB_Type,              &
    Ops_Alloc

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_type), INTENT(IN)           :: Observations
TYPE (ModelOB_type), INTENT(IN)      :: ModelOb
TYPE (OB_type), INTENT(OUT)          :: ModelObservations
TYPE (OB_type), OPTIONAL, INTENT(IN) :: RepObs

! Local declarations:
CHARACTER(len=*), PARAMETER          :: RoutineName = "Ops_ModelObToOb"
INTEGER                              :: NumObsLocal ! The number of observations
TYPE (Element_type)                  :: Zero        ! Zero element, used for initialising data arrays

!-------------------------------------------------------------------------------
! 0. Tracing
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

NumObsLocal = ModelOb % header % NumObsLocal
Zero % value = 0
Zero % OBErr = 0
Zero % PGEFinal = 0
Zero % Flags = 0

!-------------------------------------------------------------------------------
! 1. Copy the Observation structure
!-------------------------------------------------------------------------------

CALL ModelObservations % init

ModelObservations % header % ObsGroup = Observations % header % ObsGroup
ModelObservations % header % ObsGroupName = Observations % header % ObsGroupName
ModelObservations % header % NumObsLocal = Observations % header % NumObsLocal
ModelObservations % header % NumObsTotal = Observations % header % NumObsTotal
ModelObservations % header % ValidityTime = ModelOb % header % ValidityTime
ModelObservations % header % ObsRelease = 2
ModelObservations % header % MarineWindAdjusted = Observations % header % MarineWindAdjusted

CALL Ops_Alloc (ModelObservations % Header % ReportFlags, &
                "ModelObservations % ReportFlags",        &
                NumObsLocal,                              &
                ModelObservations % ReportFlags)
ModelObservations % ReportFlags = Observations % ReportFlags

CALL Ops_Alloc (ModelObservations % Header % Latitude, &
                "ModelObservations % Latitude",        &
                NumObsLocal,                           &
                ModelObservations % Latitude)
CALL Ops_Alloc (ModelObservations % Header % Longitude, &
                "ModelObservations % Longitude",        &
                NumObsLocal,                            &
                ModelObservations % Longitude)
CALL Ops_Alloc (ModelObservations % Header % Time, &
                "ModelObservations % Time",        &
                NumObsLocal,                       &
                ModelObservations % Time)
CALL Ops_Alloc (ModelObservations % Header % ObsType, &
                "ModelObservations % ObsType",        &
                NumObsLocal,                          &
                ModelObservations % ObsType)
CALL Ops_Alloc (ModelObservations % Header % SubType, &
                "ModelObservations % SubType",        &
                NumObsLocal,                          &
                ModelObservations % SubType)
IF (PRESENT (RepObs)) THEN
  ModelObservations % Latitude = RepObs % Latitude
  ModelObservations % Longitude = RepObs % Longitude
  ModelObservations % Time = RepObs % Time
  ModelObservations % ObsType = RepObs % ObsType
  ModelObservations % SubType = RepObs % SubType
ELSE
  ModelObservations % Latitude = Observations % Latitude
  ModelObservations % Longitude = Observations % Longitude
  ModelObservations % Time = Observations % Time
  ModelObservations % ObsType = Observations % ObsType
  ModelObservations % SubType = Observations % SubType
END IF

!-------------------------------------------------------------------------------
! 2. Load the model ob header into the output
!-------------------------------------------------------------------------------

! Pressure level information

! The following variables are not available as part of the model_ob
! structure, but are required in the ob_type structure, so we borrow
! the values from the real observations

IF (Observations % header % PlevelsA % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % PLevelsA,    &
                  "ModelObservations % PLevelsA",           &
                  NumObsLocal,                              &
                  ModelObservations % PLevelsA,             &
                  HdrIn = Observations % Header % PLevelsA)
  ModelObservations % PlevelsA = Observations % PlevelsA
END IF
IF (Observations % header % PlevelsB % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % PLevelsB,    &
                  "ModelObservations % PLevelsB",           &
                  NumObsLocal,                              &
                  ModelObservations % PLevelsB,             &
                  HdrIn = Observations % Header % PLevelsB)
  ModelObservations % PlevelsB = Observations % PlevelsB
END IF
IF (Observations % header % SBUVUppLayerP % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % SBUVUppLayerP,    &
                  "ModelObservations % SBUVUppLayerP",           &
                  NumObsLocal,                                   &
                  ModelObservations % SBUVUpplayerP,             &
                  HdrIn = Observations % Header % SBUVUppLayerP)
  ModelObservations % SBUVUppLayerP = Observations % SBUVUppLayerP
END IF
IF (Observations % header % SBUVLowLayerP % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % SBUVLowLayerP,    &
                  "ModelObservations % SBUVLowLayerP",           &
                  NumObsLocal,                                   &
                  ModelObservations % SBUVLowlayerP,             &
                  HdrIn = Observations % Header % SBUVLowLayerP)
  ModelObservations % SBUVLowLayerP = Observations % SBUVLowLayerP
END IF

! Radar geolocation information

! The following variables are not available as part of the model_ob
! structure, but are required in the ob_type structure, so we borrow
! the values from the real observations

IF (Observations % header % RadarBeamElev % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % RadarBeamElev,    &
                  "ModelObservations % RadarBeamElev",           &
                  NumObsLocal,                                   &
                  ModelObservations % RadarBeamElev,             &
                  HdrIn = Observations % Header % RadarBeamElev)
  ModelObservations % RadarBeamElev = Observations % RadarBeamElev
END IF
IF (Observations % header % RadarObRange % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % RadarObRange,    &
                  "ModelObservations % RadarObRange",           &
                  NumObsLocal,                                  &
                  ModelObservations % RadarObRange,             &
                  HdrIn = Observations % Header % RadarObRange)
  ModelObservations % RadarObRange = Observations % RadarObRange
END IF
IF (Observations % header % RadarObAzim % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % RadarObAzim,    &
                  "ModelObservations % RadarObAzim",           &
                  NumObsLocal,                                 &
                  ModelObservations % RadarObAzim,             &
                  HdrIn = Observations % Header % RadarObAzim)
  ModelObservations % RadarObAzim = Observations % RadarObAzim
END IF
IF (Observations % header % RadAltAboveMSL % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % RadAltAboveMSL,    &
                  "ModelObservations % RadAltAboveMSL",           &
                  NumObsLocal,                                    &
                  ModelObservations % RadAltAboveMSL,             &
                  HdrIn = Observations % Header % RadAltAboveMSL)
  ModelObservations % RadAltAboveMSL = Observations % RadAltAboveMSL
END IF
IF (Observations % header % RadNoiseLvl % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % RadNoiseLvl,    &
                  "ModelObservations % RadNoiseLvl",           &
                  NumObsLocal,                                 &
                  ModelObservations % RadNoiseLvl,             &
                  HdrIn = Observations % Header % RadNoiseLvl)
  ModelObservations % RadNoiseLvl = Observations % RadNoiseLvl
END IF
IF (Observations % header % RadFlag % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % RadFlag,    &
                  "ModelObservations % RadFlag",           &
                  NumObsLocal,                             &
                  ModelObservations % RadFlag,             &
                  HdrIn = Observations % Header % RadFlag)
  ModelObservations % RadFlag = Observations % RadFlag
END IF

IF (Observations % header % RadIdent % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % RadIdent,    &
                  "ModelObservations % RadIdent",           &
                  NumObsLocal,                              &
                  ModelObservations % RadIdent,             &
                  HdrIn = Observations % Header % RadIdent)
  ModelObservations % RadIdent = Observations % RadIdent
END IF

!-------------------------------------------------------------------------------
! 2.1 Set all the elements in the ob-type header, for which there is no
!     equivalent in the model_ot structure to absent
!-------------------------------------------------------------------------------

! GPS total zenith delay specific information

IF (Observations % header % GPSTZDelay % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % GPSTZDelay, &
                  "ModelObservations % GPSTZDelay",        &
                  NumObsLocal,                             &
                  ModelObservations % GPSTZDelay)
  ModelObservations % GPSTZDelay = Observations % GPSTZDelay
END IF

!-------------------------------------------------------------------------------
! 3. Load the model ob data into the output
!-------------------------------------------------------------------------------

! First complete the variables which are not of element_type in the observation
! structure

IF (ModelOb % header % BriTemp % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % BriTemp,             &
                  "ModelObservations % BriTemp",                    &
                  NumObsLocal,                                      &
                  ModelObservations % BriTemp,                      &
                  num_levels = ModelOb % header % BriTemp % NumLev)
  ModelObservations % BriTemp = ModelOb % BriTemp
END IF

! Set the CorBriTemp to take data from the model BriTemp

IF (ModelOb % header % BriTemp % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % CorBriTemp,          &
                  "ModelObservations % CorBriTemp",                 &
                  NumObsLocal,                                      &
                  ModelObservations % CorBriTemp,                   &
                  num_levels = ModelOb % header % BriTemp % NumLev)
  ModelObservations % CorBriTemp = ModelOb % BriTemp
END IF

! Surface variables

IF (ModelOb % header % PStar % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % PStar, &
                  "ModelObservations % PStar",        &
                  NumObsLocal,                        &
                  ModelObservations % PStar)
  ModelObservations % PStar = Zero
  ModelObservations % PStar % value = ModelOb % PStar
END IF
IF (ModelOb % header % t2 % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % t2, &
                  "ModelObservations % t2",        &
                  NumObsLocal,                     &
                  ModelObservations % t2)
  ModelObservations % t2 = Zero
  ModelObservations % t2 % value = ModelOb % t2
END IF
IF (ModelOb % header % td2 % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % td2, &
                  "ModelObservations % td2",        &
                  NumObsLocal,                      &
                  ModelObservations % td2)
  ModelObservations % td2 = Zero
  ModelObservations % td2 % value = ModelOb % td2
END IF
IF (ModelOb % header % rh2 % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % rh2, &
                  "ModelObservations % rh2",        &
                  NumObsLocal,                      &
                  ModelObservations % rh2)
  ModelObservations % rh2 = Zero
  ModelObservations % rh2 % value = ModelOb % rh2
END IF
IF (ModelOb % header % u10 % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % u10, &
                  "ModelObservations % u10",        &
                  NumObsLocal,                      &
                  ModelObservations % u10)
  ModelObservations % u10 = Zero
  ModelObservations % u10 % value = ModelOb % u10
END IF
IF (ModelOb % header % v10 % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % v10, &
                  "ModelObservations % v10",        &
                  NumObsLocal,                      &
                  ModelObservations % v10)
  ModelObservations % v10 = Zero
  ModelObservations % v10 % value = ModelOb % v10
END IF
IF (ModelOb % header % vis % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % vis, &
                  "ModelObservations % vis",        &
                  NumObsLocal,                      &
                  ModelObservations % vis)
  ModelObservations % vis = Zero
  ModelObservations % vis % value = ModelOb % vis
END IF
IF (ModelOb % header % logvis % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % logvis, &
                  "ModelObservations % logvis",        &
                  NumObsLocal,                         &
                  ModelObservations % logvis)
  ModelObservations % logvis = Zero
  ModelObservations % logvis % value = ModelOb % logvis
END IF
IF (ModelOb % header % TSkin % Present .AND. &
    .NOT. (OpsFn_IsSatRadGroup (ModelObservations % header % ObsGroup) .AND. .NOT. &
           OpsFn_IsClrGeoGroup (ModelObservations % header % ObsGroup))) THEN
  CALL Ops_Alloc (ModelObservations % Header % tskin, &
                  "ModelObservations % tskin",        &
                  NumObsLocal,                        &
                  ModelObservations % tskin)
  ModelObservations % TSkin = Zero
  ModelObservations % TSkin % value = ModelOb % TSkin
END IF
IF (ModelOb % header % TCWV % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % TCWV, &
                  "ModelObservations % TCWV",        &
                  NumObsLocal,                       &
                  ModelObservations % TCWV)
  ModelObservations % TCWV = Zero
  ModelObservations % TCWV % value = ModelOb % TCWV
END IF
IF (ModelOb % header % LWP % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % LWP, &
                  "ModelObservations % LWP",        &
                  NumObsLocal,                      &
                  ModelObservations % LWP)
  ModelObservations % LWP = Zero
  ModelObservations % LWP % value = ModelOb % LWP
END IF
IF (ModelOb % header % Rainrate % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % Rainrate,             &
                  "ModelObservations % Rainrate",                    &
                  NumObsLocal,                                       &
                  ModelObservations % Rainrate,                      &
                  num_levels = ModelOb % header % Rainrate % NumLev)
  ModelObservations % Rainrate = Zero
  ModelObservations % Rainrate % value = ModelOb % Rainrate
END IF
IF (ModelOb % header % Snowrate % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % Snowrate,             &
                  "ModelObservations % Snowrate",                    &
                  NumObsLocal,                                       &
                  ModelObservations % Snowrate,                      &
                  num_levels = ModelOb % header % Snowrate % NumLev)
  ModelObservations % Snowrate = Zero
  ModelObservations % Snowrate % value = ModelOb % Snowrate
END IF
IF (ModelOb % header % u10AmbWind % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % u10AmbWind,             &
                  "ModelObservations % u10AmbWind",                    &
                  NumObsLocal,                                         &
                  ModelObservations % u10AmbWind,                      &
                  num_levels = ModelOb % header % u10AmbWind % NumLev)
  ModelObservations % u10AmbWind = Zero
  ModelObservations % u10AmbWind % value = ModelOb % u10AmbWind
END IF
IF (ModelOb % header % v10AmbWind % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % v10AmbWind,             &
                  "ModelObservations % v10AmbWind",                    &
                  NumObsLocal,                                         &
                  ModelObservations % v10AmbWind,                      &
                  num_levels = ModelOb % header % v10AmbWind % NumLev)
  ModelObservations % v10AmbWind = Zero
  ModelObservations % v10AmbWind % value = ModelOb % v10AmbWind
END IF
IF (ModelOb % header % AWPriorPCorrect % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % AWPriorPCorrect,             &
                  "ModelObservations % AWPriorPCorrect",                    &
                  NumObsLocal,                                              &
                  ModelObservations % AWPriorPCorrect,                      &
                  num_levels = ModelOb % header % AWPriorPCorrect % NumLev)
  ModelObservations % AWPriorPCorrect = Zero
  ModelObservations % AWPriorPCorrect % value = ModelOb % AWPriorPCorrect
END IF
IF (ModelOb % header % PrWx % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % PrWx, &
                  "ModelObservations % PrWx",        &
                  NumObsLocal,                       &
                  ModelObservations % PrWx)
  ModelObservations % PrWx = ModelOb % PrWx
END IF
IF (ModelOb % header % RainAccum % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % RainAccum, &
                  "ModelObservations % RainAccum",        &
                  NumObsLocal,                            &
                  ModelObservations % RainAccum)
  ModelObservations % RainAccum = Zero
  ModelObservations % RainAccum % value = ModelOb % RainAccum
END IF

! Upper level variables

IF (ModelOb % header % z % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % z,             &
                  "ModelObservations % z",                    &
                  NumObsLocal,                                &
                  ModelObservations % z,                      &
                  num_levels = ModelOb % header % z % NumLev)
  ModelObservations % z = Zero
  ModelObservations % z % value = ModelOb % z
END IF
IF (ModelOb % header % t % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % t,             &
                  "ModelObservations % t",                    &
                  NumObsLocal,                                &
                  ModelObservations % t,                      &
                  num_levels = ModelOb % header % t % NumLev, &
                  zcode = ModelOb % header % t % zcode)
  ModelObservations % t = Zero
  ModelObservations % t % value = ModelOb % t
END IF
IF (ModelOb % header % td % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % td,             &
                  "ModelObservations % td",                    &
                  NumObsLocal,                                 &
                  ModelObservations % td,                      &
                  num_levels = ModelOb % header % td % NumLev, &
                  zcode = ModelOb % header % td % zcode)
  ModelObservations % td = Zero
  ModelObservations % td % value = ModelOb % td
END IF
IF (ModelOb % header % theta % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % theta,             &
                  "ModelObservations % theta",                    &
                  NumObsLocal,                                    &
                  ModelObservations % theta,                      &
                  num_levels = ModelOb % header % theta % NumLev, &
                  zcode = ModelOb % header % theta % zcode)
  ModelObservations % theta = Zero
  ModelObservations % theta % value = ModelOb % theta
END IF
IF (ModelOb % header % rh % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % rh,             &
                  "ModelObservations % rh",                    &
                  NumObsLocal,                                 &
                  ModelObservations % rh,                      &
                  num_levels = ModelOb % header % rh % NumLev, &
                  zcode = ModelOb % header % rh % zcode)
  ModelObservations % rh = Zero
  ModelObservations % rh % value = ModelOb % rh
END IF
IF (ModelOb % header % q % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % q,             &
                  "ModelObservations % q",                    &
                  NumObsLocal,                                &
                  ModelObservations % q,                      &
                  num_levels = ModelOb % header % q % NumLev, &
                  zcode = ModelOb % header % q % zcode)
  ModelObservations % q = Zero
  ModelObservations % q % value = ModelOb % q
END IF
IF (ModelOb % header % u % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % u,             &
                  "ModelObservations % u",                    &
                  NumObsLocal,                                &
                  ModelObservations % u,                      &
                  num_levels = ModelOb % header % u % NumLev, &
                  zcode = ModelOb % header % u % zcode)
  ModelObservations % u = Zero
  ModelObservations % u % value = ModelOb % u
END IF
IF (ModelOb % header % v % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % v,             &
                  "ModelObservations % v",                    &
                  NumObsLocal,                                &
                  ModelObservations % v,                      &
                  num_levels = ModelOb % header % v % NumLev, &
                  zcode = ModelOb % header % v % zcode)
  ModelObservations % v = Zero
  ModelObservations % v % value = ModelOb % v
END IF
IF (ModelOb % header % CLW % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % CLW,             &
                  "ModelObservations % CLW",                    &
                  NumObsLocal,                                  &
                  ModelObservations % CLW,                      &
                  num_levels = ModelOb % header % CLW % NumLev, &
                  zcode = ModelOb % header % CLW % zcode)
  ModelObservations % CLW = Zero
  ModelObservations % CLW % value = ModelOb % CLW
END IF
IF (ModelOb % header % Cloud % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % Cloud,             &
                  "ModelObservations % Cloud",                    &
                  NumObsLocal,                                    &
                  ModelObservations % Cloud,                      &
                  num_levels = ModelOb % header % Cloud % NumLev, &
                  zcode = ModelOb % header % Cloud % zcode)
  ModelObservations % Cloud = Zero
  ModelObservations % Cloud % value = ModelOb % Cloud
END IF
IF (ModelOb % header % Refrac % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % Refrac,             &
                  "ModelObservations % Refrac",                    &
                  NumObsLocal,                                     &
                  ModelObservations % Refrac,                      &
                  num_levels = ModelOb % header % Refrac % NumLev, &
                  zcode = ModelOb % header % Refrac % zcode)
  ModelObservations % Refrac = Zero
  ModelObservations % Refrac % value = ModelOb % Refrac
END IF
IF (ModelOb % header % BendingAngle % Present .AND. .NOT. GPSRO_TPD) THEN
  CALL Ops_Alloc (ModelObservations % Header % BendingAngle,             &
                  "ModelObservations % BendingAngle",                    &
                  NumObsLocal,                                           &
                  ModelObservations % BendingAngle,                      &
                  num_levels = ModelOb % header % BendingAngle % NumLev, &
                  zcode = ModelOb % header % BendingAngle % zcode)
  ModelObservations % BendingAngle = Zero
  ModelObservations % BendingAngle % value = ModelOb % BendingAngle
END IF
IF (ModelOb % header % BendingAngleAll % Present .AND. GPSRO_TPD) THEN
  CALL Ops_Alloc (ModelObservations % Header % BendingAngleAll,             &
                  "ModelObservations % BendingAngleAll",                    &
                  NumObsLocal,                                              &
                  ModelObservations % BendingAngleAll,                      &
                  num_levels = ModelOb % header % BendingAngleAll % NumLev, &
                  zcode = ModelOb % header % BendingAngleAll % zcode)
  ModelObservations % BendingAngleAll = Zero
  ModelObservations % BendingAngleAll % value = ModelOb % BendingAngleAll
END IF
IF (ModelOb % header % ImpactParam % Present .AND. .NOT. GPSRO_TPD) THEN
  CALL Ops_Alloc (ModelObservations % Header % ImpactParam,             &
                  "ModelObservations % ImpactParam",                    &
                  NumObsLocal,                                          &
                  ModelObservations % ImpactParam,                      &
                  num_levels = ModelOb % header % ImpactParam % NumLev, &
                  zcode = ModelOb % header % ImpactParam % zcode)
  ModelObservations % ImpactParam = Zero
  ModelObservations % ImpactParam % value = ModelOb % ImpactParam
END IF
IF (ModelOb % header % ImpactParamAll % Present .AND. GPSRO_TPD) THEN
  CALL Ops_Alloc (ModelObservations % Header % ImpactParamAll,             &
                  "ModelObservations % ImpactParamAll",                    &
                  NumObsLocal,                                             &
                  ModelObservations % ImpactParamAll,                      &
                  num_levels = ModelOb % header % ImpactParamAll % NumLev, &
                  zcode = ModelOb % header % ImpactParamAll % zcode)
  ModelObservations % ImpactParamAll = Zero
  ModelObservations % ImpactParamAll % value = ModelOb % ImpactParamAll
END IF
IF (ModelOb % header % p % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % p,             &
                  "ModelObservations % p",                    &
                  NumObsLocal,                                &
                  ModelObservations % p,                      &
                  num_levels = ModelOb % header % p % NumLev, &
                  zcode = ModelOb % header % p % zcode)
  ModelObservations % p = Zero
  ModelObservations % p % value = ModelOb % p
END IF
IF (ModelOb % header % RadialVelocity % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % RadialVelocSO,              &
                  "ModelObservations % RadialVelocSO",                     &
                  NumObsLocal,                                             &
                  ModelObservations % RadialVelocSO,                       &
                  num_levels = ModelOb % header % RadialVelocity % NumLev, &
                  zcode = ModelOb % header % RadialVelocity % zcode)
  ModelObservations % RadialVelocSO = Zero
  ModelObservations % RadialVelocSO % value = ModelOb % RadialVelocity
  CALL Ops_Alloc (ModelObservations % Header % RadialVelocity,             &
                  "ModelObservations % RadialVelocity",                    &
                  NumObsLocal,                                             &
                  ModelObservations % RadialVelocity,                      &
                  num_levels = ModelOb % header % RadialVelocity % NumLev, &
                  zcode = ModelOb % header % RadialVelocity % zcode)
  ModelObservations % RadialVelocity = Zero
  ModelObservations % RadialVelocity % value = ModelOb % RadialVelocity
END IF
IF (ModelOb % header % Reflectivity % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % ReflectivitySO,           &
                  "ModelObservations % ReflectivitySO",                  &
                  NumObsLocal,                                           &
                  ModelObservations % ReflectivitySO,                    &
                  num_levels = ModelOb % header % Reflectivity % NumLev, &
                  zcode = ModelOb % header % Reflectivity % zcode)
  ModelObservations % ReflectivitySO = Zero
  ModelObservations % ReflectivitySO % value = ModelOb % Reflectivity
END IF
IF (ModelOb % header % ReflectivityR % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % ReflectivityR,             &
                  "ModelObservations % ReflectivityR",                    &
                  NumObsLocal,                                            &
                  ModelObservations % ReflectivityR,                      &
                  num_levels = ModelOb % header % ReflectivityR % NumLev, &
                  zcode = ModelOb % header % ReflectivityR % zcode)
  ModelObservations % ReflectivityR = Zero
  ModelObservations % ReflectivityR % value = ModelOb % ReflectivityR
END IF
IF (ModelOb % header % ReflectivityI % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % ReflectivityI,             &
                  "ModelObservations % ReflectivityI",                    &
                  NumObsLocal,                                            &
                  ModelObservations % ReflectivityI,                      &
                  num_levels = ModelOb % header % ReflectivityI % NumLev, &
                  zcode = ModelOb % header % ReflectivityI % zcode)
  ModelObservations % ReflectivityI = Zero
  ModelObservations % ReflectivityI % value = ModelOb % ReflectivityI
END IF
IF (ModelOb % header % SBUVOzone % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % SBUVOzone,             &
                  "ModelObservations % SBUVOzone",                    &
                  NumObsLocal,                                        &
                  ModelObservations % SBUVOzone,                      &
                  num_levels = ModelOb % header % SBUVOzone % NumLev, &
                  zcode = ModelOb % header % SBUVOzone % zcode)
  ModelObservations % SBUVOzone = Zero
  ModelObservations % SBUVOzone % value = ModelOb % SBUVOzone
END IF
IF (ModelOb % header % AOD % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % AOD,             &
                  "ModelObservations % AOD",                    &
                  NumObsLocal,                                  &
                  ModelObservations % AOD,                      &
                  num_levels = ModelOb % header % AOD % NumLev, &
                  zcode = ModelOb % header % AOD % zcode)
  ModelObservations % AOD = Zero
  ModelObservations % AOD % value = ModelOb % AOD
END IF
IF (ModelOb % header % CeilBackscatter % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % CeilBackscatter,             &
                  "ModelObservations % CeilBackscatter",                    &
                  NumObsLocal,                                              &
                  ModelObservations % CeilBackscatter,                      &
                  num_levels = ModelOb % header % CeilBackscatter % NumLev, &
                  zcode = ModelOb % header % CeilBackscatter % zcode)
  ModelObservations % CeilBackscatter = Zero
  ModelObservations % CeilBackscatter % value = ModelOb % CeilBackscatter
END IF

IF (Observations % header % CeilRange % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % CeilRange,             &
                  "ModelObservations % CeilRange",                    &
                  NumObsLocal,                                        &
                  ModelObservations % CeilRange,                      &
                  num_levels = ModelOb % header % CeilRange % NumLev, &
                  zcode = ModelOb % header % CeilRange % zcode)
  ModelObservations % CeilRange = Observations % CeilRange
END IF
IF (Observations % header % CeilSiteID % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % CeilSiteID,            &
                  "ModelObservations % CeilSiteID",                   &
                  NumObsLocal,                                        &
                  ModelObservations % CeilSiteID,                     &
                  num_levels = ModelOb % header % CeilRange % NumLev, &
                  zcode = ModelOb % header % CeilRange % zcode)
  ModelObservations % CeilSiteID = Observations % CeilSiteID
END IF
IF (Observations % header % CeilScanIdent % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % CeilScanIdent,          &
                  "ModelObservations % CeilScanIdent",                &
                  NumObsLocal,                                        &
                  ModelObservations % CeilScanIdent,                  &
                  num_levels = ModelOb % header % CeilRange % NumLev, &
                  zcode = ModelOb % header % CeilRange % zcode)
  ModelObservations % CeilScanIdent = Observations % CeilScanIdent
END IF
IF (ModelOb % header % HLOSwind % Present) THEN
  CALL Ops_Alloc (ModelObservations % Header % HLOSwind, &
                  "ModelObservations % HLOSwind",        &
                  NumObsLocal,                           &
                  ModelObservations % HLOSwind)
  ModelObservations % HLOSwind = Zero
  ModelObservations % HLOSwind % value = ModelOb % HLOSwind
END IF

!-------------------------------------------------------------------------------
! 5. Tracing and end of routine
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ModelObToOb
