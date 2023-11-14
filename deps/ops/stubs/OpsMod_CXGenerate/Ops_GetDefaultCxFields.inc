!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Set default CxFields for an obsgroup.
!
! Inputs:
!     Obsgroup
!
! Outputs:
!     Default CxFields
!-------------------------------------------------------------------------------

SUBROUTINE Ops_GetDefaultCxfields (ObsGroup,  &
                                   CxFields)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE OpsMod_Ancil

USE OpsMod_AODGeneral, ONLY: &
  NDustBins

USE OpsMod_Control, ONLY: &
  ModelType,              &
  ModelType_SST,          &
  read_cx_twice

USE OpsMod_ObsGroupInfo

USE OpsMod_Stash

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)         :: ObsGroup
INTEGER, INTENT(OUT)        :: Cxfields(:)

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_GetDefaultCxfields"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

Cxfields = IMDI

SELECT CASE (ObsGroup)
  CASE (ObsGroupABIClr)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,              &
                       StashItem_SST,StashItem_SeaIce,StashItem_orog,          &
                       StashItem_qcl,StashItem_p,StashItem_p_surface,          &
                       StashCode_u10,StashCode_v10,StashCode_t2,StashCode_rh2, &
                       StashCode_pmsl/)
  CASE (ObsGroupAHIASR)
    CxFields(1:15) = (/StashItem_theta,StashItem_q,StashItem_qcf,              &
                       StashItem_SST,StashItem_SeaIce,StashItem_orog,          &
                       StashItem_qcl,StashItem_p,StashItem_p_surface,          &
                       StashCode_u10,StashCode_v10,StashCode_t2,StashCode_rh2, &
                       StashCode_pmsl,StashItem_Cloud_Bulk/)
  CASE (ObsGroupAHIClr)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,              &
                       StashItem_SST,StashItem_SeaIce,StashItem_orog,          &
                       StashItem_qcl,StashItem_p,StashItem_p_surface,          &
                       StashCode_u10,StashCode_v10,StashCode_t2,StashCode_rh2, &
                       StashCode_pmsl/)
  CASE (ObsGroupAircraft)
    CxFields(1:10) = (/StashItem_u,StashItem_v,StashItem_theta,StashItem_q,      &
                      StashItem_qcf,StashItem_SeaIce,StashItem_orog,             &
                      StashItem_qcl,StashItem_p,StashItem_p_Surface/)
  CASE (ObsGroupAIRS)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,StashItem_SST,  &
                       StashItem_SeaIce,StashItem_orog,StashItem_qcl,            &
                       StashItem_p,StashItem_p_Surface,StashCode_u10,            &
                       StashCode_v10,StashCode_t2,StashCode_rh2,StashCode_pmsl/)
  CASE (ObsGroupAltim)
    CxFields(1:1) = (/AncilCode_SeaSrfcHeight/)
  CASE (ObsGroupAMSR)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,StashItem_SST,  &
                       StashItem_SeaIce,StashItem_orog,StashItem_qcl,            &
                       StashItem_p,StashItem_p_Surface,StashCode_u10,            &
                       StashCode_v10,StashCode_t2,StashCode_rh2,StashCode_pmsl/)
  CASE (ObsGroupGMIlow)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,StashItem_SST,&
                       StashItem_SeaIce,StashItem_orog,StashItem_qcl,&
                       StashItem_p,StashItem_p_Surface,StashCode_u10,&
                       StashCode_v10,StashCode_t2,StashCode_rh2,StashCode_pmsl/)

  CASE (ObsGroupGMIhigh)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,StashItem_SST,  &
                       StashItem_SeaIce,StashItem_orog,StashItem_qcl,            &
                       StashItem_p,StashItem_p_Surface,StashCode_u10,            &
                       StashCode_v10,StashCode_t2,StashCode_rh2,StashCode_pmsl/)
  CASE (ObsGroupAMSUB)
    CxFields(1:13) = (/StashItem_theta,StashItem_q,StashItem_SST, &
                       StashItem_SeaIce,StashItem_orog,StashItem_qcl, &
                       StashItem_p,StashItem_p_surface,StashCode_u10, &
                       StashCode_v10,StashCode_t2,StashCode_rh2,StashCode_pmsl/)
  CASE (ObsGroupATOVS)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,StashItem_SST,  &
                       StashItem_SeaIce,StashItem_orog,StashItem_qcl,            &
                       StashItem_p,StashItem_p_Surface,StashCode_u10,            &
                       StashCode_v10,StashCode_t2,StashCode_rh2,StashCode_pmsl/)
  CASE (ObsGroupATMS)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,StashItem_SST,  &
                       StashItem_SeaIce,StashItem_orog,StashItem_qcl,            &
                       StashItem_p,StashItem_p_Surface,StashCode_u10,            &
                       StashCode_v10,StashCode_t2,StashCode_rh2,StashCode_pmsl/)
  CASE (ObsGroupCOMSMIClr)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,              &
                       StashItem_SST,StashItem_SeaIce,StashItem_orog,          &
                       StashItem_qcl,StashItem_p,StashItem_p_surface,          &
                       StashCode_u10,StashCode_v10,StashCode_t2,StashCode_rh2, &
                       StashCode_pmsl/)
  CASE (ObsGroupCrIS)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,StashItem_SST,  &
                       StashItem_SeaIce,StashItem_orog,StashItem_qcl,            &
                       StashItem_p,StashItem_p_Surface,StashCode_u10,            &
                       StashCode_v10,StashCode_t2,StashCode_rh2,StashCode_pmsl/)
  CASE (ObsGroupMWRI)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,StashItem_SST, &
                       StashItem_SeaIce,StashItem_orog,StashItem_qcl,           &
                       StashItem_p,StashItem_p_Surface,StashCode_u10,           &
                       StashCode_v10,StashCode_t2,StashCode_rh2,StashCode_pmsl/)
  CASE (ObsGroupMWSFY3B)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,StashItem_SST,  &
                       StashItem_SeaIce,StashItem_orog,StashItem_qcl,            &
                       StashItem_p,StashItem_p_Surface,StashCode_u10,            &
                       StashCode_v10,StashCode_t2,StashCode_rh2,StashCode_pmsl/)
  CASE (ObsGroupGeoCloud)
    CxFields(1:17) = (/StashItem_theta,StashItem_q,StashItem_qcf,StashItem_SST, &
                       StashItem_SeaIce,StashItem_orog,StashItem_qcl,           &
                       StashItem_cloud_bulk,StashItem_p,StashItem_p_surface,    &
                       StashCode_u10,StashCode_v10,StashCode_t2,StashCode_rh2,  &
                       StashCode_cloud_layer,StashCode_CloudAmount, StashCode_pmsl/)
  CASE (ObsGroupGOESImClr)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,              &
                       StashItem_SST,StashItem_SeaIce,StashItem_orog,          &
                       StashItem_qcl,StashItem_p,StashItem_p_surface,          &
                       StashCode_u10,StashCode_v10,StashCode_t2,StashCode_rh2, &
                       StashCode_pmsl/)
  CASE (ObsGroupGPSRO)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,StashItem_SST,  &
                       StashItem_SeaIce,StashItem_orog,StashItem_qcl,            &
                       StashItem_p,StashItem_p_Surface,StashCode_u10,            &
                       StashCode_v10,StashCode_t2,StashCode_rh2,StashCode_pmsl/)
  CASE (ObsGroupGroundGPS)
    CxFields(1:13) = (/StashItem_theta,StashItem_q,StashItem_qcf,StashItem_SST,      &
                       StashItem_SeaIce,StashItem_orog,StashItem_qcl,StashItem_p,    &
                       StashItem_p_Surface,StashCode_u10,StashCode_v10,StashCode_t2, &
                       StashCode_rh2/)
  CASE (ObsGroupGroundLidar)
    CxFields(1:10) = (/StashItem_qrain,StashItem_qcf,StashItem_qcl,StashItem_aerosol, &
                      StashCode_CDNC,StashItem_orog,StashItem_theta,StashItem_p,      &
                      StashItem_exner_bar,StashCode_RH_AfterMainCloud/)
  CASE (ObsGroupHLOSWIND)
    CxFields(1:5) = (/StashItem_u,StashItem_v,StashItem_orog,StashItem_p, &
                      StashItem_p_Surface/)
  CASE (ObsGroupIASI)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,StashItem_SST,  &
                       StashItem_SeaIce,StashItem_orog,StashItem_qcl,            &
                       StashItem_p,StashItem_p_Surface,StashCode_u10,            &
                       StashCode_v10,StashCode_t2,StashCode_rh2,StashCode_pmsl/)
  CASE (ObsGroupIN3DIClr)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,              &
                       StashItem_SST,StashItem_SeaIce,StashItem_orog,          &
                       StashItem_qcl,StashItem_p,StashItem_p_surface,          &
                       StashCode_u10,StashCode_v10,StashCode_t2,StashCode_rh2, &
                       StashCode_pmsl/)
  CASE (ObsGroupIN3DS)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,              &
                       StashItem_SST,StashItem_SeaIce,StashItem_orog,          &
                       StashItem_qcl,StashItem_p,StashItem_p_surface,          &
                       StashCode_u10,StashCode_v10,StashCode_t2,StashCode_rh2, &
                       StashCode_pmsl/)
  CASE (ObsGroupSatSST)
    CxFields(1:1) = (/AncilItem_SST/)
  CASE (ObsGroupAOD)
    IF (NDustBins == 2) THEN
      CxFields(1:4) = (/StashItem_p,StashItem_p_surface, &
                        StashItem_dust1,StashItem_dust2/)
    ELSE IF (NDustBins == 6) THEN
      CxFields(1:8)  = (/StashItem_p,StashItem_p_surface, &
                         StashItem_dust1,StashItem_dust2, &
                         StashItem_dust3,StashItem_dust4, &
                         StashItem_dust5,StashItem_dust6/)
    END IF
  CASE (ObsGroupMTSATImClr)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,              &
                       StashItem_SST,StashItem_SeaIce,StashItem_orog,          &
                       StashItem_qcl,StashItem_p,StashItem_p_surface,          &
                       StashCode_u10,StashCode_v10,StashCode_t2,StashCode_rh2, &
                       StashCode_pmsl/)
  CASE (ObsGroupSAPHIR)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,StashItem_SST,  &
                       StashItem_SeaIce,StashItem_orog,StashItem_qcl,            &
                       StashItem_p,StashItem_p_Surface,StashCode_u10,            &
                       StashCode_v10,StashCode_t2,StashCode_rh2,StashCode_pmsl/)
  CASE (ObsGroupMVIRIClr)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,              &
                       StashItem_SST,StashItem_SeaIce,StashItem_orog,          &
                       StashItem_qcl,StashItem_p,StashItem_p_surface,          &
                       StashCode_u10,StashCode_v10,StashCode_t2,StashCode_rh2, &
                       StashCode_pmsl/)
  CASE (ObsGroupMWSFY3)
      CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,StashItem_SST,  &
                         StashItem_SeaIce,StashItem_orog,StashItem_qcl,            &
                         StashItem_p,StashItem_p_Surface,StashCode_u10,            &
                         StashCode_v10,StashCode_t2,StashCode_rh2,StashCode_pmsl/)
  CASE (ObsGroupOcean)
    CxFields(1:2) = (/AncilCode_theta_ocean,AncilCode_salt/)
  CASE (ObsGroupOceanWinds)
    CxFields(1:7) = (/StashItem_SST,StashItem_SeaIce,StashItem_orog, &
                      StashItem_p,StashItem_p_Surface,               &
                      StashCode_u10,StashCode_v10/)
  CASE (ObsGroupRadar)
    CxFields(1:4) = (/StashItem_u,StashItem_v,StashItem_orog,StashItem_w/)
  CASE (ObsGroupRadarZ)
    CxFields(1:8) = (/StashItem_theta,StashItem_q,StashItem_qcf,StashItem_orog, &
                      StashItem_p, StashItem_qcl, StashItem_exner,StashItem_qrain/)
  CASE (ObsGroupRadarN)
    CxFields(1:8) = (/StashItem_theta,StashItem_q,StashItem_qcf,StashItem_orog,&
                      StashItem_p,StashItem_p_Surface,StashCode_t2, &
                      StashCode_rh2/)
  CASE (ObsGroupSatTCWV)
    CxFields(1:7) = (/StashItem_theta,StashItem_q,                 &
                      StashItem_SeaIce,StashItem_orog,StashItem_p, &
                      StashItem_p_surface,StashCode_CloudAmount/)
  CASE (ObsGroupSatwind)
    CxFields(1:7) = (/StashItem_u,StashItem_v,StashItem_theta,StashItem_SeaIce, &
                      StashItem_orog,StashItem_p,StashItem_p_Surface/)
  CASE (ObsGroupSBUV)
    CxFields(1:7) = (/StashItem_ozone_old,StashItem_p,StashCode_t2, &
                      StashCode_rh2,StashCode_t_p,StashCode_rh_p,StashCode_pmsl/)
  CASE (ObsGroupScatwind, ObsGroupScatwindChosen)
    CxFields(1:12) = (/StashItem_u,StashItem_v,StashItem_theta, StashItem_SST, &
                       StashItem_SeaIce,StashItem_orog,StashItem_p,            &
                       StashItem_p_Surface,StashCode_u10,StashCode_v10,        &
                       StashCode_ObukhovLength,StashCode_FrictionVel/)
  CASE (ObsGroupSeaIce)
    CxFields(1:1) = (/AncilCode_SeaIce/)
  CASE (ObsGroupOceanColour)
    CxFields(1:2) = (/AncilCode_CHL,AncilCode_KD490/)
  CASE (ObsGroupSEVIRIClr)
    CxFields(1:15) = (/StashItem_theta,StashItem_q,StashItem_qcf,StashItem_SST,  &
                       StashItem_SeaIce,StashItem_orog,StashItem_qcl,            &
                       StashItem_p,StashItem_p_Surface,StashCode_u10,            &
                       StashCode_v10,StashCode_t2,StashCode_rh2,StashCode_pmsl,  &
                       StashItem_Cloud_Bulk/)
  CASE (ObsGroupSEVIRIASR)
    CxFields(1:15) = (/StashItem_theta,StashItem_q,StashItem_qcf,StashItem_SST,  &
                       StashItem_SeaIce,StashItem_orog,StashItem_qcl,            &
                       StashItem_p,StashItem_p_Surface,StashCode_u10,            &
                       StashCode_v10,StashCode_t2,StashCode_rh2,StashCode_pmsl,  &
                       StashItem_Cloud_Bulk/)
  CASE (ObsGroupSonde)
    IF (read_cx_twice) THEN
      CxFields(1:10) = (/StashItem_u,StashItem_v,StashItem_theta,StashItem_q,      &
                         StashItem_qcf,StashItem_SeaIce,StashItem_orog,            &
                         StashItem_qcl,StashItem_p,StashItem_p_Surface/)
    ELSE
      CxFields(1:15) = (/StashItem_u,StashItem_v,StashItem_theta,StashItem_q,      &
                         StashItem_qcf,StashItem_SeaIce,StashItem_orog,            &
                         StashItem_qcl,StashItem_p,StashItem_p_Surface,            &
                         StashItem_theta_unslanted,StashItem_u_unslanted,          &
                         StashItem_v_unslanted,StashItem_q_unslanted,              &
                         StashItem_p_unslanted/)
    END IF
  CASE (ObsGroupSSMIS)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,StashItem_SST,  &
                       StashItem_SeaIce,StashItem_orog,StashItem_qcl,            &
                       StashItem_p,StashItem_p_Surface,StashCode_u10,            &
                       StashCode_v10,StashCode_t2,StashCode_rh2,StashCode_pmsl/)
  CASE (ObsGroupSurface)
    IF (ModelType /= ModelType_SST) THEN
      CxFields(1:34) = (/StashItem_u,StashItem_v,StashItem_theta,StashItem_q,    &
                         StashItem_qcf,StashItem_SnowAmount,StashItem_SST,       &
                         StashItem_BLHeight,StashItem_SeaIce,StashItem_orog,     &
                         StashItem_aerosol,StashItem_qcl,StashItem_p,            &
                         StashItem_p_Surface,StashCode_SWradiation,              &
                         StashCode_RichNumber,StashCode_u10,StashCode_v10,       &
                         StashCode_t2,StashCode_rh2,StashCode_vis,StashCode_qt2, &
                         StashCode_ObukhovLength, StashCode_FrictionVel,         &
                         StashCode_SoilMoisture,StashCode_SoilTemp,              &
                         StashCode_LowCloudAmount,StashCode_MedCloudAmount,      &
                         StashCode_CloudAmount, StashCode_LowCloudBase,          &
                         StashCode_pmsl,StashCode_PSurfParamA,                   &
                         StashCode_PSurfParamB,StashCode_LapseRate/)
    END IF
  CASE (ObsGroupSurfaceCloud)
    CxFields(1:10) = (/StashItem_theta,StashItem_q,StashItem_qcf,              &
                       StashItem_SeaIce,StashItem_orog,StashItem_qcl,          &
                       StashItem_p,StashItem_p_Surface, StashCode_cloud_layer, &
                       StashCode_CloudAmount/)
  CASE (ObsGroupTOVS)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,StashItem_SST,  &
                       StashItem_SeaIce,StashItem_orog,StashItem_qcl,            &
                       StashItem_p,StashItem_p_Surface,StashCode_u10,            &
                       StashCode_v10,StashCode_t2,StashCode_rh2,StashCode_pmsl/)

  CASE (ObsGroupHIRAS)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,StashItem_SST,  &
                       StashItem_SeaIce,StashItem_orog,StashItem_qcl,            &
                       StashItem_p,StashItem_p_Surface,StashCode_u10,            &
                       StashCode_v10,StashCode_t2,StashCode_rh2,StashCode_pmsl/)

  CASE (ObsGroupGIIRSLW, ObsGroupGIIRSMW)
    CxFields(1:14) = (/StashItem_theta,StashItem_q,StashItem_qcf,StashItem_SST,  &
                       StashItem_SeaIce,StashItem_orog,StashItem_qcl,            &
                       StashItem_p,StashItem_p_Surface,StashCode_u10,            &
                       StashCode_v10,StashCode_t2,StashCode_rh2,StashCode_pmsl/)

END SELECT

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_GetDefaultCxfields
