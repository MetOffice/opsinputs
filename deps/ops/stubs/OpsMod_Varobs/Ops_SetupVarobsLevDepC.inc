!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Set up the level dependent constant part of the varobs header.
!
! Inputs: Fortran90 structures containing (i) the observation information, (ii)
!         the Cx header information, (iii) the varobs header information.
!
! Outputs: The Fortran90 structure containing the varobs header information,
!          with modified level dependent constants.
!
! The section of the level dependent constants array which holds the observation
! level information is set using information from the observation structure
! (Observations). The section which holds the model AK, BK, AKH, BKH values is
! copied directly from the Cx header information structure.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_SetupVarobsLevDepC (Observations, &
                                   Varobs)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI

USE OpsMod_ObsGroupInfo, ONLY: &
    ObsGroupSurface,           &
    ObsGroupScatwind,          &
    ObsGroupScatwindChosen,    &
    ObsGroupSatwind,           &
    ObsGroupAircraft,          &
    ObsGroupSonde,             &
    ObsGroupATOVS,             &
    ObsGroupTOVS,              &
    ObsGroupGroundGPS,         &
    ObsGroupGroundLidar,       &
    ObsGroupAMSUB,             &
    ObsGroupSSMIS,             &
    ObsGroupAIRS,              &
    ObsGroupCrIS,              &
    ObsGroupIASI,              &
    ObsGroupSBUV,              &
    ObsGroupRadar,             &
    ObsGroupRadarZ,            &
    ObsGroupGPSRO,             &
    ObsGroupSEVIRIClr,         &
    ObsGroupSEVIRIASR,         &
    ObsGroupFCIClr,            &
    ObsGroupFCIASR,            &
    ObsGroupMVIRIClr,          &
    ObsGroupGOESImClr,         &
    ObsGroupMTSATImClr,        &
    ObsGroupAHIASR,            &
    ObsGroupAHIClr,            &
    ObsGroupABIClr,            &
    ObsGroupGeoCloud,          &
    ObsGroupAMSR,              &
    ObsGroupSatTCWV,           &
    ObsGroupPrecip,            &
    ObsGroupAOD,               &
    ObsGroupATMS,              &
    ObsGroupSAPHIR,            &
    ObsGroupIN3DIClr,          &
    ObsGroupIN3DS,             &
    ObsGroupSurfaceCloud,      &
    ObsGroupMWSFY3B,           &
    ObsGroupRainAccum,         &
    ObsGroupMWRI,              &
    ObsGroupMWSFY3,            &
    ObsGroupGMIlow,            &
    ObsGroupGMIhigh,           &
    ObsGroupHLOSWIND,          &
    ObsGroupHIRAS,             &
    ObsGroupOceanWinds,        &
    ObsGroupGIIRSLW,           &
    ObsGroupGIIRSMW

USE OpsMod_ObsInfo, ONLY: &
    OB_type

USE OpsMod_VarobsLib, ONLY: &
    varobs_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_type), INTENT(IN)        :: Observations
TYPE (varobs_type), INTENT(INOUT) :: Varobs

! Local declarations:
CHARACTER(len=*), PARAMETER       :: RoutineName = "Ops_SetupVarobsLevDepC"
INTEGER                           :: NumLevelLevs      ! For VAR header
INTEGER                           :: Jlev              ! Loop counters

!-------------------------------------------------------------------------------
! 0. Tracing
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-------------------------------------------------------------------------------
! 1. Assign level dependent constant header values.
!    AK, BK, AKH, and BKH values are already set in array
!-------------------------------------------------------------------------------

! Initialise variables

! ObsLevelType is described in UMDP_P3:
! 0=single, 1=pressure, 2=model, 3=layer mean, 4=pressure levels (boundaries)

SELECT CASE (Observations % header % ObsGroup)

  CASE (ObsGroupSurface)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupScatwind, ObsGroupScatwindChosen)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupSatwind)

    ObsLevelType = 1.0
    NumLevelLevs = 1

  CASE (ObsGroupAircraft)

    ObsLevelType = 1.0
    NumLevelLevs = 1

  CASE (ObsGroupSonde)

    ObsLevelType = 2.0
    NumLevelLevs = NumObLev

  CASE (ObsGroupATOVS)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupTOVS)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupGroundGPS)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupAIRS)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupCrIS)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupIASI)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupAMSUB)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupSSMIS)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupSEVIRIClr, ObsGroupSEVIRIASR)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupFCIClr, ObsGroupFCIASR)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupSAPHIR)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupMVIRIClr)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupGOESImClr)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupIN3DIClr)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupIN3DS)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupMTSATImClr)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupAHIClr, ObsGroupAHIASR)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupABIClr)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupGeoCloud)

    ObsLevelType = 2.0
    NumLevelLevs = NumObLev

  CASE (ObsGroupGroundLidar)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupSBUV)

    ObsLevelType = 4.0
    NumLevelLevs = NumObLev + 1

  CASE (ObsGroupRadar)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupRadarZ)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupGPSRO)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupAMSR)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupGMIlow, ObsGroupGMIhigh)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupSatTCWV)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupPrecip)

    ObsLevelType = 0.0   ! precip data only, single level
    NumLevelLevs = 1

  CASE (ObsGroupAOD)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupRainAccum)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupATMS)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupSurfaceCloud)

    ObsLevelType = 2.0
    NumLevelLevs = NumObLev

  CASE (ObsGroupMWSFY3B)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupMWSFY3)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupMWRI)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupHLOSWIND)
  
      ObsLevelType = 0.0
      NumLevelLevs = NumObLev

  CASE (ObsGroupHIRAS)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupOceanWinds)

    ObsLevelType = 0.0
    NumLevelLevs = 1

  CASE (ObsGroupGIIRSLW, ObsGroupGIIRSMW)

    ObsLevelType = 0.0
    NumLevelLevs = 1

END SELECT

! Initialise

varobs % LevDepC(:) = RMDI

varobs % LevDepC(1) = ObsLevelType
varobs % LevDepC(2) = NumLevelLevs

IF (NumLevelLevs > 1) THEN
  DO Jlev = 1, NumLevelLevs
    IF (ObsLevelType == 2) THEN
      varobs % LevDepC(Jlev + 2) = Jlev
    ELSE
      IF(Observations % header % ObsGroup == ObsGroupSBUV) THEN
        IF(Jlev == 1) THEN
          varobs % LevDepC(Jlev + 2) = Observations % SBUVUppLayerP(1,Jlev)
        ELSE
          varobs % LevDepC(Jlev + 2) = Observations % SBUVLowLayerP(1,Jlev - 1)
        END IF
      ELSE
        varobs % LevDepC(Jlev + 2) = Observations % PlevelsA(1,Jlev) % Value
      END IF
    END IF
  END DO
END IF

!-------------------------------------------------------------------------------
! 2. Tracing and end of routine
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_SetupVarobsLevDepC
