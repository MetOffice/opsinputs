!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Reads the operator switches which determine the observation groups to process.
! The operator switches are obtained by a series of calls to ops_env_is_true
! which read the switches in from environment variables.
!
! Outputs:
!   A list of observation groups to process.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ReadObsGroupSwitches (obs_group_list)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    IMDI

USE OpsMod_EnvUtils, ONLY: &
    ops_env_is_true

USE OpsMod_ObsGroupInfo

IMPLICIT NONE

! Subroutine arguments:
INTEGER, POINTER            :: obs_group_list(:)

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_ReadObsGroupSwitches"
LOGICAL, ALLOCATABLE        :: using(:)
INTEGER                     :: i
INTEGER                     :: j

!------------------------------------------------------------------------------
! 0. Tracing
!------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-------------------------------------------------------------------------------
! 1. Get observation switches
!-------------------------------------------------------------------------------

ALLOCATE (using(max_obs_group_num))
using = .FALSE.

Using(ObsGroupSurface) = ops_env_is_true ("OPS_SURFACE")
Using(ObsGroupSonde) = ops_env_is_true ("OPS_SONDE")
Using(ObsGroupSatwind) = ops_env_is_true ("OPS_SATWIND")
Using(ObsGroupSatSST) = ops_env_is_true ("OPS_SATSST")
Using(ObsGroupAircraft) = ops_env_is_true ("OPS_AIRCRAFT")
Using(ObsGroupAltim) = ops_env_is_true ("OPS_ALTIM")
Using(ObsGroupScatwind) = ops_env_is_true ("OPS_SCATWIND")
Using(ObsGroupOcean) = ops_env_is_true ("OPS_OCEANSOUND")
Using(ObsGroupSSMIS) = ops_env_is_true ("OPS_SSMIS")
Using(ObsGroupGroundGPS) = ops_env_is_true ("OPS_GROUNDGPS")
Using(ObsGroupGroundLidar) = ops_env_is_true ("OPS_GROUNDLIDAR")
Using(ObsGroupATOVS) = ops_env_is_true ("OPS_ATOVS")
Using(ObsGroupTOVS) = ops_env_is_true ("OPS_TOVS")
Using(ObsGroupAMSUB) = ops_env_is_true ("OPS_AMSUB")
Using(ObsGroupATMS) = ops_env_is_true ("OPS_ATMS")
Using(ObsGroupAIRS) = ops_env_is_true ("OPS_AIRS")
Using(ObsGroupIASI) = ops_env_is_true ("OPS_IASI")
Using(ObsGroupSEVIRIClr) = ops_env_is_true ("OPS_SEVIRICLR")
Using(ObsGroupSEVIRIASR) = ops_env_is_true ("OPS_SEVIRIASR")
Using(ObsGroupFCIClr) = ops_env_is_true ("OPS_FCICLR")
Using(ObsGroupFCIASR) = ops_env_is_true ("OPS_FCIASR")
Using(ObsGroupMVIRIClr) = ops_env_is_true ("OPS_MVIRICLR")
Using(ObsGroupGOESImClr) = ops_env_is_true ("OPS_GOESIMCLR")
Using(ObsGroupCOMSMIClr) = ops_env_is_true ("OPS_COMSMICLR")
Using(ObsGroupMTSATImClr) = ops_env_is_true ("OPS_MTSATIMCLR")
Using(ObsGroupAHIClr) = ops_env_is_true ("OPS_AHICLR")
Using(ObsGroupAHIASR) = ops_env_is_true ("OPS_AHIASR")
Using(ObsGroupABIClr) = ops_env_is_true ("OPS_ABICLR")
Using(ObsGroupGPSRO) = ops_env_is_true ("OPS_GPSRO")
Using(ObsGroupSBUV) = ops_env_is_true ("OPS_SBUV")
Using(ObsGroupRadar) = ops_env_is_true ("OPS_RADWIND")
Using(ObsGroupGeoCloud) = ops_env_is_true ("OPS_GEOCLOUD")
Using(ObsGroupRadarZ) = ops_env_is_true ("OPS_RADREFL")
Using(ObsGroupRadarN) = ops_env_is_true ("OPS_RADREFRAC")
Using(ObsGroupSeaIce) = ops_env_is_true ("OPS_SEAICE")
Using(ObsGroupOceanColour) = ops_env_is_true ("OPS_OCEANCOLOUR")
Using(ObsGroupAMSR) = ops_env_is_true ("OPS_AMSR")
Using(ObsGroupAOD) = ops_env_is_true ("OPS_AOD")
Using(ObsGroupSatTCWV) = ops_env_is_true ("OPS_SATTCWV")
Using(ObsGroupPrecip) = ops_env_is_true ("OPS_PRECIP")
Using(ObsGroupCrIS) = ops_env_is_true ("OPS_CRIS")
Using(ObsGroupSurfaceCloud) = ops_env_is_true ("OPS_SURFACECLOUD")
Using(ObsGroupMWSFY3B) = ops_env_is_true ("OPS_MWSFY3B")
Using(ObsGroupRainAccum) = ops_env_is_true ("OPS_RAINACCUM")
Using(ObsGroupMWSFY3) = ops_env_is_true ("OPS_MWSFY3")
Using(ObsGroupMWRI) = ops_env_is_true ("OPS_MWRI")
Using(ObsGroupSAPHIR) = ops_env_is_true ("OPS_SAPHIR")
Using(ObsGroupIN3DIClr) = ops_env_is_true ("OPS_IN3DICLR")
Using(ObsGroupIN3DS) = ops_env_is_true ("OPS_IN3DS")
Using(ObsGroupGMIlow) = ops_env_is_true ("OPS_GMILOW")
Using(ObsGroupGMIhigh) = ops_env_is_true ("OPS_GMIHIGH")
Using(ObsGroupHLOSWIND) = ops_env_is_true ("OPS_HLOSWIND")
Using(ObsGroupHIRAS) = ops_env_is_true ("OPS_HIRAS")
Using(ObsGroupOceanWinds) = ops_env_is_true ("OPS_OCEANWINDS")
Using(ObsGroupGIIRSLW) = ops_env_is_true ("OPS_GIIRSLW")
Using(ObsGroupGIIRSMW) = ops_env_is_true ("OPS_GIIRSMW")

ALLOCATE (obs_group_list(COUNT (using)))
obs_group_list = IMDI
j = 1
DO i = 1, max_obs_group_num
  IF (using(i)) THEN
    obs_group_list(j) = i
    j = j + 1
  END IF
END DO

!-------------------------------------------------------------------------------
! 2. Tracing and end of routine
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ReadObsGroupSwitches
