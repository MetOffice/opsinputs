!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Check that the VarFields namelist input is sensible. For each variable
! requested, calculate maximum space per observation needed.
!
! Inputs: Array of VAR field codes requested.
!
! Outputs: Maximum size of ObsArray required.
!
! Loop through array, checking values are valid. Issue a warning if not.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CheckVarFields (VarFields,         &
                               Ob,                &
                               NumValidVarFields, &
                               ObsArrayMaxSize,   &
                               ElementsPresent,   &
                               ElementsLevels,    &
                               RepObs)

USE GenMod_Core, ONLY: &
    gen_fail,          &
    gen_message,       &
    gen_trace_entry,   &
    gen_trace_exit,    &
    gen_warn,          &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    IMDI

USE OpsMod_GPSRO, ONLY: &
    GPSRO_TPD

USE OpsMod_MiscTypes, ONLY: &
    ElementHeader_type

USE OpsMod_ObsGroupInfo, ONLY: &
    ObsGroupSurface,           &
    ObsGroupScatwind

USE OpsMod_ObsInfo, ONLY: &
    OB_type

USE OpsMod_SatRad_RTmodel, ONLY: &
    nlevels_strat_varobs

USE OpsMod_Varfields

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(INOUT)               :: VarFields(:)
TYPE (OB_type), INTENT(IN)           :: Ob
INTEGER, INTENT(OUT)                 :: NumValidVarFields
REAL, INTENT(INOUT)                  :: ElementsPresent(:)
REAL, INTENT(INOUT)                  :: ElementsLevels(:)
INTEGER, INTENT(OUT)                 :: ObsArrayMaxSize
TYPE (OB_type), OPTIONAL, INTENT(IN) :: RepObs

! Local declarations:
CHARACTER(len=*), PARAMETER          :: RoutineName = "Ops_CheckVarFields"
CHARACTER(len=80)                    :: ErrorMessage
INTEGER                              :: Ivar
INTEGER                              :: VarField
TYPE (ElementHeader_type)            :: ObHdrVrbl
INTEGER                              :: FieldSize
INTEGER                              :: NumLevs
LOGICAL                              :: UseLevelSubset

!-------------------------------------------------------------------------------
! 0. Tracing
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

FieldSize = 0
NumValidVarFields = 0
NumObLev = 0

!-------------------------------------------------------------------------------
! 1. Check values requested
!-------------------------------------------------------------------------------

IF (COUNT (VarFields /= IMDI) == 0) THEN
  CALL gen_fail (RoutineName,                        &
                 "No VarFields have been requested")
END IF

DO Ivar = 1, ActualMaxVarfield

  UseLevelSubset = .FALSE.
  VarField = VarFields(Ivar)
  SELECT CASE (VarField)
    CASE (IMDI)
      CYCLE
    CASE (VarField_pstar)
      ObHdrVrbl = Ob % header % pstar
    CASE (VarField_theta)
      ObHdrVrbl = Ob % header % theta
    CASE (VarField_temperature)
      IF (Ob % header % ObsGroup == ObsGroupSurface) THEN
        ObHdrVrbl = Ob % header % t2
      ELSE
        ObHdrVrbl = Ob % header % t
      END IF
    CASE (VarField_rh)
      IF (Ob % header % ObsGroup == ObsGroupSurface) THEN
        ObHdrVrbl = Ob % header % rh2
      ELSE
        ObHdrVrbl = Ob % header % rh
      END IF
    CASE (VarField_u)
      IF (Ob % header % ObsGroup == ObsGroupSurface .OR. &
          Ob % header % ObsGroup == ObsGroupScatwind) THEN
        ObHdrVrbl = Ob % header % u10
      ELSE
        ObHdrVrbl = Ob % header % u
      END IF
    CASE (VarField_v)
      IF (Ob % header % ObsGroup == ObsGroupSurface .OR. &
          Ob % header % ObsGroup == ObsGroupScatwind) THEN
        ObHdrVrbl = Ob % header % v10
      ELSE
        ObHdrVrbl = Ob % header % v
      END IF
    CASE (VarField_logvis)
      ObHdrVrbl = Ob % header % logvis
    CASE (VarField_tcwv)
      ObHdrVrbl = Ob % header % TCWV
    CASE (VarField_windspeed)
      ObHdrVrbl = Ob % header % WindSpeed
    CASE (VarField_lwp)
      ObHdrVrbl = Ob % header % LWP
    CASE (VarField_britemp)
      ObHdrVrbl = Ob % header % CorBriTemp
    CASE (VarField_tskin)
      ObHdrVrbl = Ob % header % Tskin
    CASE (VarField_gpstzdelay)
      WRITE (ErrorMessage, '(A,I0,2A)') "#Variable ", VarField, &
      " requested in the case..", TRIM (Ob % header % ObsGroupName)
      CALL gen_message (RoutineName,  &
                        ErrorMessage)
      ObHdrVrbl = Ob % header % GPSTZDelay
    CASE (VarField_GPS_Station_Height)
      ObHdrVrbl = Ob % header % Zstation
    CASE (VarField_mwemiss)
      ObHdrVrbl = Ob % header % MwEmiss
    CASE (VarField_TCozone)
      ObHdrVrbl = Ob % header % TCozone
    CASE (VarField_satzenith)
      ObHdrVrbl = Ob % header % SatZenithAngle
    CASE (VarField_scanpos)
      ObHdrVrbl = Ob % header % ScanPosition
    CASE (VarField_surface)
      ObHdrVrbl = Ob % header % surface
    CASE (VarField_elevation)
      ObHdrVrbl = Ob % header % elevation
    CASE (VarField_modelsurface)
      ObHdrVrbl = Ob % header % ModelSurface
    CASE (VarField_modelorog)
      ObHdrVrbl = Ob % header % ModelOrog
    CASE (VarField_stratt)
      UseLevelSubset = .TRUE.
      NumLevs = nlevels_strat_varobs
      ObHdrVrbl = Ob % header % t
    CASE (VarField_satid)
      ObHdrVrbl = Ob % header % SatId
    CASE (VarField_satazimth)
      ObHdrVrbl = Ob % header % SatAzimth
    CASE (VarField_localazimuth)
      ObHdrVrbl = Ob % header % LocalAzimuth
    CASE (VarField_solzenith)
      ObHdrVrbl = Ob % header % SolarZenith
    CASE (VarField_solazimth)
      ObHdrVrbl = Ob % header % SolarAzimth
    CASE (VarField_iremiss)
      ObHdrVrbl = Ob % header % IREmiss
    CASE (VarField_cloudtopp)
      ObHdrVrbl = Ob % header % CloudTopP
    CASE (VarField_cloudfrac)
      ObHdrVrbl = Ob % header % CloudFrac
    CASE (VarField_vnatovpp)
      ObHdrVrbl = Ob % header % VnATOVPP
    CASE (VarField_procoption)
      ObHdrVrbl = Ob % header % ATOVSProcOption
    CASE (VarField_amsusurface)
      ObHdrVrbl = Ob % header % AMSUsurface
    CASE (VarField_hirs_temp)
      ObHdrVrbl = Ob % header % HIRS_Temp
    CASE (VarField_amsua1_temp)
      ObHdrVrbl = Ob % header % AMSUa1_Temp
    CASE (VarField_amsua2_temp)
      ObHdrVrbl = Ob % header % AMSUa2_Temp
    CASE (VarField_amsub_temp)
      ObHdrVrbl = Ob % header % AMSUb_Temp
    CASE (VarField_cloud)
      ObHdrVrbl = Ob % header % Cloud
    CASE (VarField_rainrate)
      ObHdrVrbl = Ob % header % Rainrate
    CASE (VarField_snowrate)
      ObHdrVrbl = Ob % header % Snowrate
    CASE (VarField_u10ambwind)
      ObHdrVrbl = Ob % header % u10AmbWind
    CASE (VarField_v10ambwind)
      ObHdrVrbl = Ob % header % v10AmbWind
    CASE (VarField_pcorrect)
      ObHdrVrbl = Ob % header % AWPriorPCorrect
    CASE (VarField_NumChans)
      ObHdrVrbl = Ob % header % NumChans
    CASE (VarField_ChanNum)
      ObHdrVrbl = Ob % header % ChanNum
    CASE (VarField_Emissivity)
      ObHdrVrbl = Ob % header % Emissivity
    CASE (VarField_QCinfo)
      ObHdrVrbl = Ob % header % QCinfo
    CASE (VarField_SBUVozone)
      ObHdrVrbl = Ob % header % SBUVozone
    CASE (VarField_RadialVelocity)
       ObHdrVrbl = Ob % header % RadialVelocSO
    CASE (VarField_Reflectivity)
      ObHdrVrbl = Ob % header % ReflectivitySO
    CASE (VarField_ReflectivityR)
      ObHdrVrbl = Ob % header % ReflectivityR
    CASE (VarField_ReflectivityI)
      ObHdrVrbl = Ob % header % ReflectivityI
    CASE (VarField_RadarBeamElev)
      ObHdrVrbl = Ob % header % RadarBeamElev
    CASE (VarField_RadarObRange)
      ObHdrVrbl = Ob % header % RadarObRange
    CASE (VarField_RadarObAzim)
      ObHdrVrbl = Ob % header % RadarObAzim
    CASE (VarField_RadIdent)
      ObHdrVrbl = Ob % header % RadIdent
    CASE (VarField_RadAltAboveMSL)
      ObHdrVrbl = Ob % header % RadAltAboveMSL
    CASE (VarField_RadNoiseLvl)
      ObHdrVrbl = Ob % header % RadNoiseLvl
    CASE (VarField_RadFlag)
      ObHdrVrbl = Ob % header % RadFlag
    CASE (VarField_clw)
      ObHdrVrbl = Ob % header % clw
    CASE (VarField_refrac)
      ObHdrVrbl = Ob % header % refrac
    CASE (VarField_z)
      ObHdrVrbl = Ob % header % z
    CASE (VarField_BendingAngle)
      IF (GPSRO_TPD) THEN
        ObHdrVrbl = Ob % header % BendingAngleAll
      ELSE
        ObHdrVrbl = Ob % header % BendingAngle
      END IF
    CASE (VarField_ImpactParam)
      IF (GPSRO_TPD) THEN
        ObHdrVrbl = Ob % header % ImpactParamAll
      ELSE
        ObHdrVrbl = Ob % header % ImpactParam
      END IF
    CASE (VarField_RO_Rad_Curv)
      ObHdrVrbl = Ob % header % RO_Rad_Curv
    CASE (VarField_RO_geoid_und)
      ObHdrVrbl = Ob % header % RO_geoid_und
    CASE (VarField_AOD)
      ObHdrVrbl = Ob % header % AOD
    CASE (VarField_BriTempVarError)
      ObHdrVrbl = Ob % header % BriTempVarError
    CASE (VarField_CloudRTError)
      ObHdrVrbl = Ob % header % CloudRTError
    CASE (VarField_CloudRTBias)
      ObHdrVrbl = Ob % header % CloudRTBias
    CASE (VarField_BiasPredictors)
      ObHdrVrbl = Ob % header % BiasPredictors
    CASE (VarField_LevelTime)
      IF (PRESENT (RepObs)) THEN
        ObHdrVrbl = RepObs % header % model_level_time
      ELSE
        ObHdrVrbl = Ob % header % level_time
      END IF
    CASE (VarField_LevelLat)
      IF (PRESENT (RepObs)) THEN
        ObHdrVrbl = RepObs % header % model_level_lat
      ELSE
        ObHdrVrbl = Ob % header % level_lat
      END IF
    CASE (VarField_LevelLon)
      IF (PRESENT (RepObs)) THEN
        ObHdrVrbl = RepObs % header % model_level_lon
      ELSE
        ObHdrVrbl = Ob % header % level_lon
      END IF
    CASE (VarField_RainAccum)
      ObHdrVrbl = Ob % header % RainAccum
    CASE (VarField_CeilBackscatter)
      ObHdrVrbl = Ob % header % CeilBackscatter
    CASE (VarField_CeilRange)
      ObHdrVrbl = Ob % header % CeilRange
    CASE (VarField_CeilSiteId)
      ObHdrVrbl = Ob % header % CeilSiteID
    CASE (VarField_CeilScanIdent)
      ObHdrVrbl = Ob % header % CeilScanIdent
    CASE (VarField_airqal_consttype)
      ObHdrVrbl = Ob % header % CSNT_TYP
    CASE (VarField_airqal_massdensity)
      ObHdrVrbl = Ob % header % MASS_DNSTY
    CASE (VarField_airqal_massdensityscale)
      ObHdrVrbl = Ob % header % DCML_SCL_MASS_DNSTY
    CASE (VarField_HLOSwind)
      ObHdrVrbl = Ob % header % HLOSwind
    CASE (VarField_ProfileNo)
      ObHdrVrbl = Ob % header % ProfileNo
    CASE (VarField_dWinddT)
      ObHdrVrbl = Ob % header % dWinddT
    CASE (VarField_dWinddP)
      ObHdrVrbl = Ob % header % dWinddP
    CASE (VarField_AzimuthCOG)
      ObHdrVrbl = Ob % header % AzimuthCOG
    CASE (VarField_HeightCOG)
      ObHdrVrbl = Ob % header % HeightCOG
    CASE DEFAULT
      WRITE (ErrorMessage, '(A,I0)') "VarField code not recognised ", VarField
      CALL gen_warn (RoutineName,  &
                     ErrorMessage)
      CYCLE
  END SELECT

  IF (.NOT. ObHdrVrbl % Present) THEN
    WRITE (ErrorMessage, '(A,I0,2A)') "Variable ", VarField, &
      " requested but not present for ", TRIM (Ob % header % ObsGroupName)
    CALL gen_message (RoutineName,  &
                      ErrorMessage)
    CYCLE
  END IF

  NumValidVarFields = NumValidVarFields + 1
  VarFields(NumValidVarFields) = VarField
  IF (.NOT. UseLevelSubset) NumLevs = ObHdrVrbl % NumLev
  FieldSize = FieldSize + (NumItem * NumLevs)

  ElementsPresent(VarField) = NumValidVarFields
  ElementsLevels(VarField) = NumLevs

  ! NumObLev (written to integer header) set to be maximum value of NumLevs
  NumObLev = MAX (NumObLev,NumLevs)

END DO

IF (ANY (Varfields(1:NumValidVarFields) == VarField_stratt)) THEN
  ! This value is not used for ATOVS radiances.
  NumObLev = 0
END IF

IF (NumValidVarFields == 0) THEN
  WRITE (ErrorMessage, '(A,I0)') "No valid VarFields requested for obsgroup ", Ob % header % ObsGroup
  CALL gen_fail (RoutineName,  &
                 ErrorMessage)
END IF

NumObVariables = NumValidVarFields
LenObCol = NumMeta + FieldSize
ObsArrayMaxSize = Ob % header % NumObsLocal * LenObCol

!-------------------------------------------------------------------------------
! 2. Tracing and end of routine
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CheckVarFields
