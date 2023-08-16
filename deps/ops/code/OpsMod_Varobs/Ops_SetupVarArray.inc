!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Construct the data array that is written out as one batch of a varobs file.
! Also construct the arrays containing observation positions and times
!
! Inputs: Pre-allocated arrays for varobs data and positions
!
! Outputs: Number of observations for which there are valid data.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_SetupVarArray (ObsArray,          &
                              LatLons_temp,      &
                              Times_temp,        &
                              Family_temp,       &
                              SiteLatLons_temp,  &
                              NumValidVarFields, &
                              VarFields,         &
                              Ob,                &
                              Pge,               &
                              NumVarObsLocal,    &
                              NumRej,            &
                              ObsArraySize,      &
                              RepObs)

USE GenMod_Core, ONLY: &
    gen_fail,          &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI

USE OpsMod_Control, ONLY: &
    ProduceCxVarobsAllObs

USE OpsMod_GPSROInfo, ONLY:   &
    GPSRO_rising_setting_bit

USE OpsMod_CharUtils, ONLY: &
    Ops_StringToReal

USE OpsMod_GPSRO, ONLY: &
    GPSRO_TPD

USE OpsMod_MiscTypes, ONLY: &
    ElementHeader_type,     &
    coord_type

USE OpsMod_ModelObInfo, ONLY: &
    ModelOb_type

USE OpsMod_ObsGroupInfo, ONLY: &
    ObsGroupAircraft,          &
    ObsGroupGPSRO,             &
    ObsGroupSatwind,           &
    ObsGroupScatwind,          &
    ObsGroupSurface

USE OpsMod_ObsInfo, ONLY: &
    OB_type,              &
    FinalRejectReport,    &
    SurplusReport,        &
    Element_type,         &
    LenCallSign

USE OpsMod_ObsTypes, ONLY: &
    ObsTypeTCBogus,        &
    ObsTypeBogus

USE OpsMod_Radar, ONLY: &
    RadFamily,          &
    RadWind_SuperOb

USE OpsMod_SatRad_RTmodel, ONLY: &
    nlevels_strat_varobs

USE OpsMod_Varfields

IMPLICIT NONE

! Subroutine arguments:
REAL, INTENT(INOUT)                     :: ObsArray(:)
REAL, INTENT(INOUT)                     :: LatLons_temp(:)
REAL, INTENT(INOUT)                     :: Times_temp(:)
INTEGER, INTENT(INOUT)                  :: Family_temp(:)
REAL, INTENT(INOUT)                     :: SiteLatLons_temp(:)
INTEGER, INTENT(IN)                     :: NumValidVarFields
INTEGER, INTENT(IN)                     :: VarFields(:)
TYPE (OB_type), INTENT(INOUT)           :: Ob
TYPE (ModelOB_type), INTENT(IN)         :: Pge
INTEGER, INTENT(OUT)                    :: NumVarObsLocal
INTEGER, INTENT(OUT)                    :: NumRej
INTEGER, INTENT(OUT)                    :: ObsArraySize
TYPE (OB_type), OPTIONAL, INTENT(IN)    :: RepObs

! Local declarations::
CHARACTER(len=*), PARAMETER             :: RoutineName = "Ops_SetupVarArray"
CHARACTER(len=80)                       :: ErrorMessage
CHARACTER(len=LenCallSign), ALLOCATABLE :: dummychar(:)
INTEGER                                 :: ObsArrayPointer
INTEGER                                 :: Ivar
INTEGER                                 :: Iob
INTEGER                                 :: Ichr
INTEGER                                 :: VarField
INTEGER                                 :: NumLevs
INTEGER                                 :: ObDataSize
INTEGER                                 :: ObToWrite
INTEGER                                 :: NumGPSHeights ! Required because Ground GPS varobs have both real and element types
TYPE (ElementHeader_type)               :: ObHdrVrbl
TYPE (Element_type), POINTER            :: ObVrblSingle(:)
TYPE (Element_type), POINTER            :: ObVrblMulti(:,:)
TYPE (coord_type), POINTER              :: ObCoordMulti(:,:)
REAL, POINTER                           :: RArrVrbl1d(:)
REAL, POINTER                           :: RArrVrbl2d(:,:)
REAL, POINTER                           :: Pge_1D(:)
REAL, POINTER                           :: Pge_2D(:,:)
INTEGER, POINTER                        :: IArrVrbl1d(:)
INTEGER, POINTER                        :: IArrVrbl2d(:,:)
INTEGER                                 :: NumObsUse
LOGICAL                                 :: UseLevelSubset
INTEGER                                 :: FirstLevel
INTEGER                                 :: LastLevel
LOGICAL, ALLOCATABLE                    :: UseOb(:)
REAL                                    :: ObErrScaling
REAL, ALLOCATABLE                       :: CallSignR(:,:)

!-------------------------------------------------------------------------------
! 0. Tracing
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

NumVarObsLocal = 0
ObsArrayPointer = 0
ObsArraySize = 0
ObDataSize = 0
FirstLevel = 1
ObsArray = RMDI

!-------------------------------------------------------------------------------
! 1. Check values requested
!-------------------------------------------------------------------------------

ALLOCATE (UseOb(Ob % header % NumObsLocal))
UseOb(:) = .FALSE.
DO Iob = 1, Ob % header % NumObsLocal
  IF ((.NOT. BTEST (Ob % ReportFlags(Iob), SurplusReport) .AND. &
       .NOT. BTEST (Ob % ReportFlags(Iob), FinalRejectReport)) &
       .OR. ProduceCxVarobsAllObs) THEN
    UseOb(Iob) = .TRUE.
    NumVarObsLocal = NumVarObsLocal + 1
    IF (PRESENT (RepObs)) THEN
      LatLons_temp(2 * NumVarobsLocal - 1) = RepObs % Latitude(Iob)
      LatLons_temp(2 * NumVarObsLocal) = RepObs % Longitude(Iob)
      Times_temp(NumVarObsLocal) = RepObs % Time(Iob)
    ELSE
      LatLons_temp(2 * NumVarobsLocal - 1) = Ob % Latitude(Iob)
      LatLons_temp(2 * NumVarObsLocal) = Ob % Longitude(Iob)
      Times_temp(NumVarObsLocal) = Ob % Time(Iob)
    END IF
    IF (RadFamily) THEN
      Family_temp(NumVarObsLocal) = Ob % Family(Iob)
    END IF
    IF (Ob % Header % ObsGroup == ObsGroupGPSRO .AND. GPSRO_TPD) THEN
      SiteLatLons_temp(2 * NumVarObsLocal - 1) = Ob % ro_occ_lat(Iob)
      SiteLatLons_temp(2 * NumVarObsLocal) = Ob % ro_occ_lon(Iob)
    END IF
  END IF
END DO

NumObsUse = COUNT (UseOb)
NumRej = Ob % header % NumObsLocal - NumObsUse

ALLOCATE (CallSignR(Ob % header % NumObsLocal, LenCallSign))
CallSignR(:,:) = REAL (ICHAR (" "))

IF (PRESENT (RepObs)) THEN
  IF (RepObs % header % CallSign % Present) THEN
    CALL Ops_StringToReal (RepObs % Callsign, &
                           CallSignR)
  END IF
! For GPSRO, use AAAABBBBBCCCCCCC as callsign in varobs - AAAA is satid, BBBBB is index
! of observation's parent profile and CCCCCCC is the ob id. - needed in VAR for FSO.
! N.B. SatID is already present in CallSign as 'AAAA            ' for other sat-types (and GPSRO)
ELSE IF (Ob % header % ObsGroup == ObsGroupGPSRO .AND. Ob % header % SatId % Present .AND. GPSRO_TPD) THEN
  ALLOCATE (dummychar(Ob % header % NumObsLocal))
  IF (Ob % header % RO_quality % Present) THEN
    DO Iob = 1, Ob % header % NumObsLocal
      IF (BTEST (Ob % RO_quality(iOb), GPSRO_rising_setting_bit)) THEN
        WRITE (dummychar(Iob), '(I4.4,I5.5,I6.6,I1)') Ob % SatId(Iob), Ob % Id(Iob), Iob, 1  ! Rising
      ELSE
        WRITE (dummychar(Iob), '(I4.4,I5.5,I6.6,I1)') Ob % SatId(Iob), Ob % Id(Iob), Iob, 0  ! Setting
      END IF
    END DO
  ELSE
    DO Iob = 1, Ob % header % NumObsLocal
      WRITE (dummychar(Iob), '(I4.4,I5.5,I6.6,I1)') Ob % SatId(Iob), Ob % Id(Iob), Iob, 2  ! Neither
    END DO
  END IF
  CALL Ops_StringToReal (dummychar, &
                         CallSignR)
  DEALLOCATE (dummychar)
ELSE IF (Ob % header % CallSign % Present) THEN
  CALL Ops_StringToReal (Ob % CallSign, &
                         CallSignR)
END IF

DO Ivar = 1, NumValidVarFields

  UseLevelSubset = .FALSE.

  NULLIFY (ObVrblSingle)
  NULLIFY (ObVrblMulti)
  NULLIFY (ObCoordMulti)
  NULLIFY (Pge_1D)
  NULLIFY (Pge_2D)
  NULLIFY (IArrVrbl1d)
  NULLIFY (IArrVrbl2d)
  NULLIFY (RArrVrbl1d)
  NULLIFY (RArrVrbl2d)

  VarField = VarFields(Ivar)
  SELECT CASE (VarField)
    CASE (VarField_pstar)
      ObHdrVrbl = Ob % header % pstar
      ObVrblSingle => Ob % pstar(:)
      Pge_1D => Pge % pstar
    CASE (VarField_theta)
      ObHdrVrbl = Ob % header % theta
      ObVrblMulti => Ob % theta(:,:)
      Pge_2D => Pge % theta
    CASE (VarField_temperature)
      IF (Ob % header % ObsGroup == ObsGroupSurface) THEN
        ObHdrVrbl = Ob % header % t2
        ObVrblSingle => Ob % t2(:)
        Pge_1D => Pge % t2
      ELSE
        ObHdrVrbl = Ob % header % t
        ObVrblMulti => Ob % t(:,:)
        Pge_2D => Pge % t
      END IF
    CASE (VarField_rh)
      IF (Ob % header % ObsGroup == ObsGroupSurface) THEN
        ObHdrVrbl = Ob % header % rh2
        ObVrblSingle => Ob % rh2(:)
        Pge_1D => Pge % rh2
      ELSE
        ObHdrVrbl = Ob % header % rh
        ObVrblMulti => Ob % rh(:,:)
        Pge_2D => Pge % rh
      END IF
    CASE (VarField_u)
      IF (Ob % header % ObsGroup == ObsGroupSurface .OR. &
          Ob % header % ObsGroup == ObsGroupScatwind) THEN
        ObHdrVrbl = Ob % header % u10
        ObVrblSingle => Ob % u10(:)
        Pge_1D => Pge % u10
      ELSE
        ObHdrVrbl = Ob % header % u
        ObVrblMulti => Ob % u(:,:)
        Pge_2D => Pge % u
      END IF
    CASE (VarField_v)
      IF (Ob % header % ObsGroup == ObsGroupSurface .OR. &
          Ob % header % ObsGroup == ObsGroupScatwind) THEN
        ObHdrVrbl = Ob % header % v10
        ObVrblSingle => Ob % v10(:)
        Pge_1D => Pge % u10
      ELSE
        ObHdrVrbl = Ob % header % v
        ObVrblMulti => Ob % v(:,:)
        Pge_2D => Pge % u
      END IF
    CASE (VarField_logvis)
      ObHdrVrbl = Ob % header % logvis
      ObVrblSingle => Ob % logvis(:)
      Pge_1D => Pge % logvis
    CASE (VarField_tcwv)
      ObHdrVrbl = Ob % header % TCWV
      ObVrblSingle => Ob % TCWV(:)
      Pge_1D => Pge % TCWV
    CASE (VarField_windspeed)
      ObHdrVrbl = Ob % header % WindSpeed
      ObVrblSingle => Ob % WindSpeed(:)
      Pge_1D => Pge % WindSpeed
    CASE (VarField_lwp)
      ObHdrVrbl = Ob % header % LWP
      ObVrblSingle => Ob % LWP(:)
      Pge_1D => Pge % LWP
    CASE (VarField_britemp)
      ObHdrVrbl = Ob % header % CorBriTemp
      RArrVrbl2d => Ob % CorBriTemp
    CASE (VarField_gpstzdelay)
      ObHdrVrbl = Ob % header % GPSTZDelay
      ObVrblSingle => Ob % GPSTZDelay(:)
      Pge_1D => Pge % GPSTZDelay
    CASE (VarField_GPS_Station_Height)
      ObHdrVrbl = Ob % header % Zstation
      RArrVrbl1d => Ob % zStation
      ! Workaround- GroundGPS varobs unique in containing mixture of real and
      ! element types
      NumGPSHeights = SIZE (Ob % zStation)
      ALLOCATE (Pge_1D(NumGPSHeights))
      Pge_1D(:) = RMDI  ! Dummy- not used
    CASE (VarField_tskin)
      ObHdrVrbl = Ob % header % Tskin
      ObVrblSingle => Ob % Tskin(:)
    CASE (VarField_mwemiss)
      ObHdrVrbl = Ob % header % MwEmiss
      RArrVrbl2d => Ob % MwEmiss
    CASE (VarField_TCozone)
      ObHdrVrbl = Ob % header % TCozone
      RArrVrbl1d => Ob % TCozone
    CASE (VarField_satzenith)
      ObHdrVrbl = Ob % header % SatZenithAngle
      RArrVrbl1d => Ob % SatZenithAngle
    CASE (VarField_scanpos)
      ObHdrVrbl = Ob % header % ScanPosition
      IArrVrbl1d => Ob % ScanPosition
    CASE (VarField_surface)
      ObHdrVrbl = Ob % header % surface
      IArrVrbl1d => Ob % surface
    CASE (VarField_elevation)
      ObHdrVrbl = Ob % header % elevation
      RArrVrbl1d => Ob % elevation
    CASE (VarField_modelsurface)
      ObHdrVrbl = Ob % header % ModelSurface
      RArrVrbl1d => Ob % ModelSurface
    CASE (VarField_modelorog)
      ObHdrVrbl = Ob % header % ModelOrog
      RArrVrbl1d => Ob % ModelOrog
    CASE (VarField_stratt)
      UseLevelSubset = .TRUE.
      NumLevs = nlevels_strat_varobs
      FirstLevel = Ob % header % t % NumLev - NumLevs + 1
      LastLevel = Ob % header % t % NumLev
      ObHdrVrbl = Ob % header % t
      ObVrblMulti => Ob % t(:,:)
    CASE (VarField_satid)
      ObHdrVrbl = Ob % header % SatId
      IArrVrbl1d => Ob % SatId
    CASE (VarField_satazimth)
      ObHdrVrbl = Ob % header % SatAzimth
      RArrVrbl1d => Ob % SatAzimth
    CASE (VarField_localazimuth)
      ObHdrVrbl = Ob % header % LocalAzimuth
      RArrVrbl1d => Ob % LocalAzimuth
    CASE (VarField_solzenith)
      ObHdrVrbl = Ob % header % SolarZenith
      RArrVrbl1d => Ob % SolarZenith
    CASE (VarField_solazimth)
      ObHdrVrbl = Ob % header % SolarAzimth
      RArrVrbl1d => Ob % SolarAzimth
    CASE (VarField_clw)
      ObHdrVrbl = Ob % header % clw
      ObVrblMulti => Ob % clw(:,:)
    CASE (VarField_iremiss)
      ObHdrVrbl = Ob % header % IREmiss
      RArrVrbl1d => Ob % IREmiss
    CASE (VarField_cloudtopp)
      ObHdrVrbl = Ob % header % CloudTopP
      RArrVrbl1d => Ob % CloudTopP
    CASE (VarField_cloudfrac)
      ObHdrVrbl = Ob % header % CloudFrac
      RArrVrbl1d => Ob % CloudFrac
    CASE (VarField_vnatovpp)
      ObHdrVrbl = Ob % header % VnATOVPP
      IArrVrbl1d => Ob % VnATOVPP
    CASE (VarField_procoption)
      ObHdrVrbl = Ob % header % ATOVSProcOption
      IArrVrbl1d => Ob % ATOVSProcOption
    CASE (VarField_amsusurface)
      ObHdrVrbl = Ob % header % AMSUsurface
      IArrVrbl1d => Ob % AMSUsurface
    CASE (VarField_hirs_temp)
      ObHdrVrbl = Ob % header % HIRS_Temp
      RArrVrbl1d => Ob % HIRS_Temp
    CASE (VarField_amsua1_temp)
      ObHdrVrbl = Ob % header % AMSUa1_Temp
      RArrVrbl1d => Ob % AMSUa1_Temp
    CASE (VarField_amsua2_temp)
      ObHdrVrbl = Ob % header % AMSUa2_Temp
      RArrVrbl1d => Ob % AMSUa2_Temp
    CASE (VarField_amsub_temp)
      ObHdrVrbl = Ob % header % AMSUb_Temp
      RArrVrbl1d => Ob % AMSUb_Temp
    CASE (VarField_cloud)
      ObHdrVrbl = Ob % header % Cloud
      ObVrblMulti => Ob % Cloud(:,:)
      Pge_2D => Pge % Cloud
    CASE (VarField_rainrate)
      ObHdrVrbl = Ob % header % Rainrate
      ObVrblMulti => Ob % Rainrate
      Pge_2D => Pge % Rainrate
    CASE (VarField_snowrate)
      ObHdrVrbl = Ob % header % Snowrate
      ObVrblMulti => Ob % Snowrate
      Pge_2D => Pge % Snowrate
    CASE (VarField_u10ambwind)
      ObHdrVrbl = Ob % header % u10AmbWind
      ObVrblMulti => Ob % u10AmbWind(:,:)
      Pge_2D => Pge % u10AmbWind
    CASE (VarField_v10ambwind)
      ObHdrVrbl = Ob % header % v10AmbWind
      ObVrblMulti => Ob % v10AmbWind(:,:)
      Pge_2D => Pge % u10AmbWind
    CASE (VarField_pcorrect)
      ObHdrVrbl = Ob % header % AWPriorPCorrect
      ObVrblMulti => Ob % AWPriorPcorrect(:,:)
      Pge_2D => Pge % AWPriorPcorrect
    CASE (VarField_Refrac)
      ObHdrVrbl = Ob % header % Refrac
      ObVrblMulti => Ob % Refrac(:,:)
      Pge_2D => Pge % Refrac
    CASE (VarField_z)
      ObHdrVrbl = Ob % header % z
      ObVrblMulti => Ob % z(:,:)
      Pge_2D => Pge % z
    CASE (VarField_BendingAngle)
      IF (GPSRO_TPD) THEN
        ObHdrVrbl = Ob % header % BendingAngleAll
        ObVrblSingle => Ob % BendingAngleAll(:,1)
        Pge_1D => pge % bendingangleAll(:,1)
      ELSE
        ObHdrVrbl = Ob % header % BendingAngle
        ObVrblMulti => Ob % BendingAngle(:,:)
        Pge_2D => pge % bendingangle
      END IF
    CASE (VarField_ImpactParam)
      IF (GPSRO_TPD) THEN
        ObHdrVrbl = Ob % header % ImpactParamAll
        ObVrblSingle => Ob % ImpactParamAll(:,1)
        Pge_1D => Pge % ImpactParamAll(:,1)
      ELSE
        ObHdrVrbl = Ob % header % ImpactParam
        ObVrblMulti => Ob % ImpactParam(:,:)
        Pge_2D => Pge % ImpactParam
      END IF
    CASE (VarField_RO_Rad_Curv)
      ObHdrVrbl = Ob % header % RO_Rad_Curv
      ObVrblSingle => Ob % RO_Rad_Curv(:)
      Pge_1D => Pge % RO_Rad_Curv
    CASE (VarField_RO_geoid_und)
      ObHdrVrbl = Ob % header % RO_geoid_und
      ObVrblSingle => Ob % RO_geoid_und(:)
      Pge_1D => Pge % RO_geoid_und
    CASE (VarField_NumChans)
      ObHdrVrbl = Ob % header % NumChans
      IArrVrbl1d => Ob % NumChans
    CASE (VarField_ChanNum)
      ObHdrVrbl = Ob % header % ChanNum
      IArrVrbl2d => Ob % ChanNum
    CASE (VarField_Emissivity)
      ObHdrVrbl = Ob % header % Emissivity
      RArrVrbl2d => Ob % Emissivity
    CASE (VarField_QCinfo)
      ObHdrVrbl = Ob % header % QCinfo
      IArrVrbl1d => Ob % QCinfo
    CASE (VarField_SBUVozone)
      ObHdrVrbl = Ob % header % SBUVozone
      ObVrblMulti => Ob % SBUVozone(:,:)
      Pge_2D => Pge % SBUVozone
    CASE (VarField_RadialVelocity)
      ObHdrVrbl = Ob % header % RadialVelocSO
      ObVrblMulti => Ob % RadialVelocSO
      Pge_2D => Pge % RadialVelocity
    CASE (VarField_Reflectivity)
       ObHdrVrbl = Ob % header % ReflectivitySO
       ObVrblMulti => Ob % ReflectivitySO
       Pge_2D => Pge % Reflectivity
    CASE (VarField_ReflectivityR)
       ObHdrVrbl = Ob % header % ReflectivityR
       ObVrblMulti => Ob % ReflectivityR
       Pge_2D => Pge % ReflectivityR
    CASE (VarField_ReflectivityI)
       ObHdrVrbl = Ob % header % ReflectivityI
       ObVrblMulti => Ob % ReflectivityI
       Pge_2D => Pge % ReflectivityI
    CASE (VarField_RadarBeamElev)
      ObHdrVrbl = Ob % header % RadarBeamElev
      RArrVrbl2d => Ob % RadarBeamElev
    CASE (VarField_RadarObRange)
      IF (RadWind_SuperOb) THEN
        ObHdrVrbl = Ob % header % RadarObRangSO
        RArrVrbl2d => Ob % RadarObRangSO
      ELSE
        ObHdrVrbl = Ob % header % RadarObRange
        RArrVrbl2d => Ob % RadarObRange
      END IF
    CASE (VarField_RadarObAzim)
      ObHdrVrbl = Ob % header % RadarObAzim
      RArrVrbl2d => Ob % RadarObAzim
    CASE (VarField_RadIdent)
      ObHdrVrbl = Ob % header % RadIdent
      IArrVrbl2d => Ob % RadIdent
    CASE (VarField_RadAltAboveMSL)
      ObHdrVrbl = Ob % header % RadAltAboveMSL
      RArrVrbl2d => Ob % RadAltAboveMSL
    CASE (VarField_RadNoiseLvl)
      ObHdrVrbl = Ob % header % RadNoiseLvl
      RArrVrbl2d => Ob % RadNoiseLvl
    CASE (VarField_RadFlag)
      ObHdrVrbl = Ob % header % RadFlag
      IArrVrbl2d => Ob % RadFlag
    CASE (VarField_AOD)
      ObHdrVrbl = Ob % header % AOD
      ObVrblMulti => Ob % AOD(:,:)
      Pge_2D => Pge % AOD
    CASE (VarField_BriTempVarError)
      ObHdrVrbl = Ob % header % BriTempVarError
      RArrVrbl2d => Ob % BriTempVarError(:,:)
    CASE (VarField_CloudRTError)
      ObHdrVrbl = Ob % header % CloudRTError
      RArrVrbl2d => Ob % CloudRTError(:,:)
    CASE (VarField_CloudRTBias)
      ObHdrVrbl = Ob % header % CloudRTBias
      RArrVrbl2d => Ob % CloudRTBias(:,:)
    CASE (VarField_BiasPredictors)
      ObHdrVrbl = Ob % header % BiasPredictors
      RArrVrbl2d => Ob % BiasPredictors
    CASE (VarField_LevelTime)
      IF (PRESENT (RepObs)) THEN
        ObHdrVrbl = RepObs % header % model_level_time
        RArrVrbl2d => RepObs % model_level_time(:,:)
      ELSE
        ObHdrVrbl = Ob % header % level_time
        RArrVrbl2d => Ob % level_time(:,:)
      END IF
    CASE (VarField_LevelLat)
      IF (PRESENT (RepObs)) THEN
        ObHdrVrbl = RepObs % header % model_level_lat
        RArrVrbl2d => RepObs % model_level_lat(:,:)
      ELSE
        ObHdrVrbl = Ob % header % level_lat
        RArrVrbl2d => Ob % level_lat(:,:)
      END IF
    CASE (VarField_LevelLon)
      IF (PRESENT (RepObs)) THEN
        ObHdrVrbl = RepObs % header % model_level_lon
        RArrVrbl2d => RepObs % model_level_lon(:,:)
      ELSE
        ObHdrVrbl = Ob % header % level_lon
        RArrVrbl2d => Ob % level_lon(:,:)
      END IF
    CASE (VarField_RainAccum)
      ObHdrVrbl = Ob % header % RainAccum
      ObVrblSingle => Ob % RainAccum(:)
      Pge_1D => Pge % RainAccum(:)
    CASE (VarField_CeilBackscatter)
      ObHdrVrbl = Ob % header % CeilBackscatter
      ObVrblMulti => Ob % CeilBackscatter
      Pge_2D => Pge % CeilBackscatter
    CASE (VarField_CeilRange)
      ObHdrVrbl = Ob % header % CeilRange
      RArrVrbl2d => Ob % CeilRange
    CASE (VarField_CeilSiteID)
      ObHdrVrbl = Ob % header % CeilSiteID
      IArrVrbl2d => Ob % CeilSiteID
    CASE (VarField_CeilScanIdent)
      ObHdrVrbl = Ob % header % CeilScanIdent
      IArrVrbl2d => Ob % CeilScanIdent
    CASE (VarField_airqal_consttype)
      ObHdrVrbl = Ob % header % CSNT_TYP
      IArrVrbl1d => Ob % CSNT_TYP
    CASE (VarField_airqal_massdensity)
      ObHdrVrbl = Ob % header % MASS_DNSTY
      ObVrblSingle => Ob % MASS_DNSTY(:)
      Pge_1D => Pge % MASS_DNSTY(:)
    CASE (VarField_airqal_massdensityscale)
      ObHdrVrbl = Ob % header % DCML_SCL_MASS_DNSTY
      IArrVrbl1d => Ob % DCML_SCL_MASS_DNSTY
    CASE (VarField_HLOSwind)
      ObHdrVrbl = Ob % header % HLOSwind
      ObVrblSingle => Ob % HLOSwind(:)
      Pge_1D => Pge % HLOSwind(:)
    CASE (VarField_ProfileNo)
      ObHdrVrbl = Ob % header % ProfileNo
      IArrVrbl1d => Ob % ProfileNo
    CASE (VarField_dWinddT)
      ObHdrVrbl = Ob % header % dWinddT
      RArrVrbl1d => Ob % dWinddT
    CASE (VarField_dWinddP)
      ObHdrVrbl = Ob % header % dWinddP
      RArrVrbl1d => Ob % dWinddP
    CASE (VarField_AzimuthCOG)
      ObHdrVrbl = Ob % header % AzimuthCOG
      RArrVrbl1d => Ob % AzimuthCOG
    CASE (VarField_HeightCOG)
      ObHdrVrbl = Ob % header % HeightCOG
      RArrVrbl1d => Ob % HeightCOG
    CASE DEFAULT
      WRITE (ErrorMessage, '(A,I0)') "Cannot process VarField value ", VarField
      CALL gen_fail (RoutineName,  &
                     ErrorMessage)
  END SELECT

  IF (.NOT. UseLevelSubset) THEN
    NumLevs = ObHdrVrbl % NumLev
    FirstLevel = 1
    LastLevel = NumLevs
  END IF

  ObToWrite = 1
  DO Iob = 1, Ob % header % NumObsLocal

    IF (.NOT. UseOb(Iob)) CYCLE

    ObsArrayPointer = (ObToWrite - 1) * LenObCol ! Start of this ob

    ! Metadata (only included once per observation)
    IF (Ivar == 1) THEN
      IF (PRESENT (RepObs)) THEN
        ObsArray(ObsArrayPointer + 1) = RepObs % Time(Iob) / 60.0
        ObsArray(ObsArrayPointer + 2) = RepObs % ObsType(Iob)
      ELSE
        ObsArray(ObsArrayPointer + 1) = Ob % Time(Iob) / 60.0
        ObsArray(ObsArrayPointer + 2) = Ob % ObsType(Iob)
      END IF

      DO Ichr = 1, LenCallSign
        ObsArray(ObsArrayPointer + Ichr + 2) = CallSignR(Iob,Ichr)
      END DO

      SELECT CASE (Ob % header % ObsGroup)
        CASE (ObsGroupAircraft, ObsGroupSatwind)
          ObsArray(ObsArrayPointer + LenCallSign + 3) = Ob % PlevelsA(Iob,1) % Value
        CASE DEFAULT
          ObsArray(ObsArrayPointer + LenCallSign + 3) = RMDI
      END SELECT
      ObsArray(ObsArrayPointer + LenCallSign + 4) = RMDI ! reserved for observation flag

      ObsArrayPointer = ObsArrayPointer + NumMeta + 1
    ELSE ! Metadata has already been written, so skip this, and any
         ! observation data that has been written
      ObsArrayPointer = ObsArrayPointer + NumMeta + ObDataSize + 1
    END IF ! First field (Ivar == 1)

    ! Observation values
    IF (ASSOCIATED (ObVrblSingle)) THEN
       ObsArray(ObsArrayPointer) = ObVrblSingle(Iob) % Value
    ELSE IF (ASSOCIATED (ObVrblMulti)) THEN
       ObsArray(ObsArrayPointer:ObsArrayPointer + NumLevs - 1) = &
            ObVrblMulti(Iob,FirstLevel:LastLevel) % Value
    ELSE IF (ASSOCIATED (ObCoordMulti)) THEN
       ObsArray(ObsArrayPointer:ObsArrayPointer + NumLevs - 1) = &
            ObCoordMulti(Iob,FirstLevel:LastLevel) % Value
    ELSE IF (ASSOCIATED (RArrVrbl1d)) THEN
       ObsArray(ObsArrayPointer) = RArrVrbl1d(Iob)
    ELSE IF (ASSOCIATED (IArrVrbl1d)) THEN
       ObsArray(ObsArrayPointer) = IArrVrbl1d(Iob)
    ELSE IF (ASSOCIATED (RArrVrbl2d)) THEN
       ObsArray(ObsArrayPointer:ObsArrayPointer + NumLevs - 1) = RArrVrbl2d(Iob,:)
    ELSE IF (ASSOCIATED (IArrVrbl2d)) THEN
       ObsArray(ObsArrayPointer:ObsArrayPointer + NumLevs - 1) = IArrVrbl2d(Iob,:)
    ELSE ! Error
      WRITE (ErrorMessage, '(A,I0)') "No pointer to ObsArray data for varfield ", VarField
      CALL gen_fail (RoutineName,  &
                     ErrorMessage)
    END IF
    ObsArrayPointer = ObsArrayPointer + NumLevs
    IF (NumItem == 3) THEN
      ! Ob Errors and PGE values. Only valid for data in OB_type structure.

      ! Scale observation errors for BOGUS observations
      IF (PRESENT (RepObs)) THEN
        ObErrScaling = 1.0
      ELSE
        IF (Ob % ObsType(Iob) == ObsTypeTCBogus .OR. &
            Ob % ObsType(Iob) == ObsTypeBogus) THEN
          ObErrScaling = BogusObErrScaling_VAR
        ELSE
          ObErrScaling = 1.0
        END IF
      END IF

      IF (ASSOCIATED (ObVrblSingle)) THEN
        ObsArray(ObsArrayPointer) = ObVrblSingle(Iob) % ObErr * ObErrScaling
      ELSE IF (ASSOCIATED (ObVrblMulti)) THEN
        ObsArray(ObsArrayPointer:ObsArrayPointer + NumLevs - 1) = &
               ObVrblMulti(Iob,FirstLevel:LastLevel) % ObErr * ObErrScaling
      END IF

      ObsArrayPointer = ObsArrayPointer + NumLevs

      IF (ASSOCIATED (ObVrblSingle)) THEN
        IF (ASSOCIATED (Pge_1D)) THEN
          ObsArray(ObsArrayPointer) = Pge_1D(Iob)
        END IF
      ELSE
        IF (ASSOCIATED (Pge_2D)) THEN
          ObsArray(ObsArrayPointer:ObsArrayPointer + NumLevs - 1) = Pge_2D(Iob,:)
        END IF
      END IF ! ObVrblSingle

      ObsArrayPointer = ObsArrayPointer + NumLevs

    END IF ! NumItem==3
    ObToWrite = ObToWrite + 1 ! Ready for next ob
  END DO ! Iob

  ObDataSize = ObDataSize + (NumLevs * NumItem)

END DO ! VarField

ObsArraySize = ObsArrayPointer - 1
DEALLOCATE (CallSignR)
DEALLOCATE (UseOb)

!-------------------------------------------------------------------------------
! 2. Tracing and end of routine
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_SetupVarArray
