!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Set default varfields for an obsgroup.
!
! Inputs: Obsgroup
!
! Outputs: Default varfields
!-------------------------------------------------------------------------------

SUBROUTINE Ops_GetDefaultVarfields (ObsGroup,  &
                                    Varfields)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE OpsMod_ObsGroupInfo

USE OpsMod_RadarZ, ONLY: &
  RadRefl_DirectAssim

USE OpsMod_SatRad_SetUp, ONLY: &
  VarBC,                       &
  CloudErrorModel,             &
  RTTOV_CloudSwitch

USE OpsMod_Varfields

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)         :: ObsGroup
INTEGER, INTENT(OUT)        :: Varfields(:)

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_GetDefaultVarfields"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

Varfields = IMDI

SELECT CASE (ObsGroup)
  CASE (ObsGroupABIClr)
    Varfields(1:12) = (/Varfield_britemp,Varfield_satid,Varfield_satzenith, &
                        Varfield_solzenith,Varfield_tskin,Varfield_tcozone, &
                        Varfield_surface,Varfield_numchans,Varfield_channum, &
                        Varfield_stratt,Varfield_iremiss,Varfield_qcinfo/)
  CASE (ObsGroupAircraft)
    Varfields(1:4) = (/Varfield_theta,Varfield_u,Varfield_v,Varfield_rh/)
  CASE (ObsGroupAHIASR)
    Varfields(1:12) = (/Varfield_britemp,Varfield_satid,Varfield_satzenith, &
                        Varfield_solzenith,Varfield_tskin,Varfield_tcozone, &
                        Varfield_surface,Varfield_numchans,Varfield_channum, &
                        Varfield_stratt,Varfield_iremiss,Varfield_qcinfo/)
  CASE (ObsGroupAHIClr)
    Varfields(1:12) = (/Varfield_britemp,Varfield_satid,Varfield_satzenith, &
                        Varfield_solzenith,Varfield_tskin,Varfield_tcozone, &
                        Varfield_surface,Varfield_numchans,Varfield_channum, &
                        Varfield_stratt,Varfield_iremiss,Varfield_qcinfo/)
  CASE (ObsGroupAIRS)
    Varfields(1:12) = (/Varfield_tskin,Varfield_tcozone,Varfield_satzenith, &
                        Varfield_surface,Varfield_stratt,Varfield_satid, &
                        Varfield_numchans,Varfield_channum,Varfield_britemp, &
                        Varfield_emissivity,Varfield_solzenith, &
                        Varfield_qcinfo/)
    IF (.NOT. RTTOV_CloudSwitch) THEN
      Varfields(13:14) = (/Varfield_cloudtopp, Varfield_cloudfrac/)
    END IF
  CASE (ObsGroupAMSR)
    Varfields(1:13) = (/VarField_britemp,VarField_tskin,VarField_mwemiss, &
                        VarField_satzenith,VarField_surface, Varfield_stratt, &
                        Varfield_numchans,Varfield_channum,VarField_satid, &
                        VarField_tcozone,VarField_solzenith,VarField_QCinfo, &
                        VarField_clw/)
  CASE (ObsGroupAMSUB)
    Varfields(1:11) = (/VarField_britemp,VarField_tskin,VarField_mwemiss, &
                        VarField_satzenith,VarField_surface, &
                        Varfield_numchans,Varfield_channum,VarField_satid, &
                        VarField_solzenith,VarField_QCinfo,VarField_clw/)
  CASE (ObsGroupATOVS)
    Varfields(1:15) = (/Varfield_britemp,Varfield_satid,Varfield_satzenith, &
                        Varfield_solzenith,Varfield_tskin,Varfield_tcozone, &
                        Varfield_surface,Varfield_numchans,Varfield_channum, &
                        Varfield_stratt,Varfield_iremiss,Varfield_mwemiss, &
                        Varfield_qcinfo,Varfield_BriTempVarError,Varfield_clw/)
  CASE (ObsGroupTOVS)
    Varfields(1:15) = (/Varfield_britemp,Varfield_satid,Varfield_satzenith, &
                        Varfield_solzenith,Varfield_tskin,Varfield_tcozone, &
                        Varfield_surface,Varfield_numchans,Varfield_channum, &
                        Varfield_stratt,Varfield_iremiss,Varfield_mwemiss, &
                        Varfield_qcinfo,Varfield_BriTempVarError,Varfield_clw/)
  CASE (ObsGroupCOMSMIClr)
    Varfields(1:12) = (/Varfield_britemp,Varfield_satid,Varfield_satzenith, &
                        Varfield_solzenith,Varfield_tskin,Varfield_tcozone, &
                        Varfield_surface,Varfield_numchans,Varfield_channum, &
                        Varfield_stratt,Varfield_iremiss,Varfield_qcinfo/)
  CASE (ObsGroupCrIS)
    Varfields(1:12) = (/Varfield_tskin,Varfield_tcozone,Varfield_satzenith, &
                        Varfield_surface,Varfield_stratt,Varfield_satid, &
                        Varfield_numchans,Varfield_channum,Varfield_britemp, &
                        Varfield_emissivity,Varfield_solzenith, &
                        Varfield_qcinfo/)
    IF (.NOT. RTTOV_CloudSwitch) THEN
      Varfields(13:14) = (/Varfield_cloudtopp, Varfield_cloudfrac/)
    END IF
  CASE (ObsGroupHIRAS)
    Varfields(1:12) = (/Varfield_tskin,Varfield_tcozone,Varfield_satzenith, &
                        Varfield_surface,Varfield_stratt,Varfield_satid, &
                        Varfield_numchans,Varfield_channum,Varfield_britemp, &
                        Varfield_emissivity,Varfield_solzenith, &
                        Varfield_qcinfo/)
    IF (.NOT. RTTOV_CloudSwitch) THEN
      Varfields(13:14) = (/Varfield_cloudtopp, Varfield_cloudfrac/)
    END IF
  CASE (ObsGroupGIIRSLW, ObsGroupGIIRSMW)
    Varfields(1:12) = (/Varfield_tskin,Varfield_tcozone,Varfield_satzenith, &
                        Varfield_surface,Varfield_stratt,Varfield_satid, &
                        Varfield_numchans,Varfield_channum,Varfield_britemp, &
                        Varfield_emissivity,Varfield_solzenith, &
                        Varfield_qcinfo/)
    IF (.NOT. RTTOV_CloudSwitch) THEN
      Varfields(13:14) = (/Varfield_cloudtopp, Varfield_cloudfrac/)
    END IF
  CASE (ObsGroupMWSFY3B)
    Varfields(1:13) = (/VarField_britemp,VarField_tskin,Varfield_mwemiss, &
                        VarField_satzenith,VarField_surface, Varfield_stratt, &
                        Varfield_numchans,Varfield_channum,VarField_satid, &
                        VarField_tcozone,VarField_solzenith,VarField_QCinfo, &
                        VarField_clw/)
  CASE (ObsGroupMWRI)
    Varfields(1:13) = (/VarField_britemp,VarField_tskin,Varfield_mwemiss, &
                        VarField_satzenith,VarField_surface, Varfield_stratt, &
                        Varfield_numchans,Varfield_channum,VarField_satid, &
                        VarField_tcozone,VarField_solzenith,VarField_QCinfo, &
                        VarField_clw/)
  CASE (ObsGroupGMIlow)
    Varfields(1:13) = (/VarField_britemp,VarField_tskin,Varfield_mwemiss, &
                        VarField_satzenith,VarField_surface, Varfield_stratt, &
                        Varfield_numchans,Varfield_channum,VarField_satid, &
                        VarField_tcozone,VarField_solzenith,VarField_QCinfo, &
                        VarField_clw/)
  CASE (ObsGroupGMIhigh)
    Varfields(1:13) = (/VarField_britemp,VarField_tskin,Varfield_mwemiss, &
                        VarField_satzenith,VarField_surface, Varfield_stratt, &
                        Varfield_numchans,Varfield_channum,VarField_satid, &
                        VarField_tcozone,VarField_solzenith,VarField_QCinfo, &
                        VarField_clw/)
  CASE (ObsGroupGeoCloud)
    Varfields(1:1) = (/Varfield_cloud/)
  CASE (ObsGroupGOESImClr)
    Varfields(1:12) = (/Varfield_britemp,Varfield_satid,Varfield_satzenith, &
                        Varfield_solzenith,Varfield_tskin,Varfield_tcozone, &
                        Varfield_surface,Varfield_numchans,Varfield_channum, &
                        Varfield_stratt,Varfield_iremiss,Varfield_qcinfo/)
  CASE (ObsGroupGPSRO)
    Varfields(1:4) = (/Varfield_bendingangle,Varfield_impactparam,Varfield_ro_rad_curv, &
                       Varfield_ro_geoid_und/)
  CASE (ObsGroupGroundGPS)
    Varfields(1:2) = (/Varfield_gpstzdelay,Varfield_gps_station_height/)
  CASE (ObsGroupGroundLidar)
    Varfields(1:4) = (/VarField_CeilBackscatter,VarField_CeilRange, &
                      VarField_CeilSiteID,VarField_CeilScanIdent/)
  CASE (ObsGroupHLOSWIND)
    Varfields(1:6) = (/Varfield_HLOSwind,Varfield_ProfileNo,VarField_dWinddT,   &
                       VarField_dWinddP,VarField_AzimuthCOG,VarField_HeightCOG/)
  CASE (ObsGroupIASI)
    Varfields(1:12) = (/Varfield_tskin,Varfield_tcozone,Varfield_satzenith, &
                        Varfield_surface,Varfield_stratt,Varfield_satid, &
                        Varfield_numchans,Varfield_channum,Varfield_britemp, &
                        Varfield_emissivity,Varfield_solzenith, &
                        Varfield_qcinfo/)
    IF (.NOT. RTTOV_CloudSwitch) THEN
      Varfields(13:14) = (/Varfield_cloudtopp, Varfield_cloudfrac/)
    END IF
  CASE (ObsGroupIN3DIClr)
    Varfields(1:12) = (/Varfield_britemp,Varfield_satid,Varfield_satzenith, &
                        Varfield_solzenith,Varfield_tskin,Varfield_tcozone, &
                        Varfield_surface,Varfield_numchans,Varfield_channum, &
                        Varfield_stratt,Varfield_iremiss,Varfield_qcinfo/)
  CASE (ObsGroupIN3DS)
    Varfields(1:12) = (/Varfield_britemp,Varfield_satid,Varfield_satzenith, &
                        Varfield_solzenith,Varfield_tskin,Varfield_tcozone, &
                        Varfield_surface,Varfield_numchans,Varfield_channum, &
                        Varfield_stratt,Varfield_iremiss,Varfield_qcinfo/)
  CASE (ObsGroupAOD)
    Varfields(1:1) = (/Varfield_AOD/)
  CASE (ObsGroupMTSATImClr)
    Varfields(1:12) = (/Varfield_britemp,Varfield_satid,Varfield_satzenith, &
                        Varfield_solzenith,Varfield_tskin,Varfield_tcozone, &
                        Varfield_surface,Varfield_numchans,Varfield_channum, &
                        Varfield_stratt,Varfield_iremiss,Varfield_qcinfo/)
  CASE (ObsGroupSAPHIR)
    Varfields(1:13) = (/VarField_britemp,VarField_tskin,VarField_mwemiss, &
                        VarField_satzenith,VarField_surface, Varfield_stratt, &
                        Varfield_numchans,Varfield_channum,VarField_satid, &
                        VarField_tcozone,VarField_solzenith,VarField_QCinfo, &
                        VarField_clw/)
  CASE (ObsGroupMVIRIClr)
    Varfields(1:12) = (/Varfield_britemp,Varfield_satid,Varfield_satzenith, &
                        Varfield_solzenith,Varfield_tskin,Varfield_tcozone, &
                        Varfield_surface,Varfield_numchans,Varfield_channum, &
                        Varfield_stratt,Varfield_iremiss,Varfield_qcinfo/)
  CASE (ObsGroupMWSFY3)
    Varfields(1:13) = (/VarField_britemp,VarField_tskin,VarField_mwemiss, &
                        VarField_satzenith,VarField_surface, Varfield_stratt, &
                        Varfield_numchans,Varfield_channum,VarField_satid, &
                        VarField_tcozone,VarField_solzenith,VarField_QCinfo, &
                        VarField_clw/)
  CASE (ObsGroupOceanWinds)
    Varfields(1:1) = (/Varfield_windspeed/)
  CASE (ObsGroupPrecip)
    Varfields(1:1) = (/Varfield_rainrate/)
  CASE (ObsGroupRadar)
    Varfields(1:6) = (/VarField_RadialVelocity, VarField_RadarBeamElev, &
                       VarField_RadarObRange,VarField_RadarObAzim, &
                       VarField_RadIdent,VarField_RadAltAboveMSL/)
  CASE (ObsGroupRadarN)
    Varfields(1:6) = (/VarField_Refractivity, VarField_RadarBeamElev, &
                       VarField_RadarObRange,VarField_RadarObAzim, &
                       VarField_RadIdent,VarField_RadAltAboveMSL/)
  CASE (ObsGroupRadarZ)
    IF  (RadRefl_DirectAssim) THEN
       Varfields(1:7) = (/VarField_Reflectivity,VarField_RadarBeamElev, &
                          VarField_RadarObRange,VarField_RadarObAzim, &
                          VarField_RadIdent,VarField_RadAltAboveMSL, &
                          VarField_RadFlag/)
    ELSE
       Varfields(1:2) = (/VarField_temperature,VarField_rh/)
    END IF
  CASE (ObsGroupSatTCWV)
    Varfields(1:1) = (/Varfield_tcwv/)
  CASE (ObsGroupSatwind)
    Varfields(1:2) = (/Varfield_u,Varfield_v/)
  CASE (ObsGroupSBUV)
    Varfields(1:1) = (/VarField_SBUVozone/)
  CASE (ObsGroupScatwind)
    Varfields(1:3) = (/Varfield_u10ambwind,varfield_v10ambwind,Varfield_pcorrect/)
  CASE (ObsGroupSEVIRIClr)
    Varfields(1:12) = (/Varfield_britemp,Varfield_satid,Varfield_satzenith, &
                        Varfield_solzenith,Varfield_tskin,Varfield_tcozone, &
                        Varfield_surface,Varfield_numchans,Varfield_channum, &
                        Varfield_stratt,Varfield_iremiss,Varfield_qcinfo/)
  CASE (ObsGroupSEVIRIASR)
    Varfields(1:12) = (/Varfield_britemp,Varfield_satid,Varfield_satzenith, &
                        Varfield_solzenith,Varfield_tskin,Varfield_tcozone, &
                        Varfield_surface,Varfield_numchans,Varfield_channum, &
                        Varfield_stratt,Varfield_iremiss,Varfield_qcinfo/)
  CASE (ObsGroupSonde)
    Varfields(1:4) = (/Varfield_theta,Varfield_u,Varfield_v,Varfield_rh/)
  CASE (ObsGroupSSMIS)
    Varfields(1:10) = (/Varfield_britemp,Varfield_satid,Varfield_satzenith, &
                        Varfield_surface,Varfield_numchans,Varfield_channum, &
                        Varfield_stratt,Varfield_qcinfo,Varfield_clw, &
                        Varfield_mwemiss/)
  CASE (ObsGroupSurface)
    Varfields(1:6) = (/Varfield_pstar,Varfield_rh,Varfield_temperature, &
                       Varfield_u,Varfield_v,Varfield_logvis/)
  CASE (ObsGroupSurfaceCloud)
    Varfields(1:1) = (/Varfield_cloud/)
  CASE (ObsGroupATMS)
    Varfields(1:13) = (/VarField_britemp,VarField_tskin,VarField_mwemiss, &
                        VarField_satzenith,VarField_surface, Varfield_stratt, &
                        Varfield_numchans,Varfield_channum,VarField_satid, &
                        VarField_tcozone,VarField_solzenith,VarField_QCinfo, &
                        VarField_clw/)
  CASE (ObsGroupRainAccum)
    Varfields(1:1) = (/Varfield_RainAccum/)
END SELECT

IF (VarBC .AND. &
    (ObsGroup /= ObsGroupGeoCloud)) THEN !potentially any group other than GeoCloud can
                                         !include predictors for bias correction. GeoCloud
                                         !uses predictors for the relevant satellite.
  Varfields(COUNT (Varfields(:) /= IMDI) + 1) = VarField_BiasPredictors
END IF

IF (CloudErrorModel) THEN
  ! Only include these Varfields if this feature is used.
  ! This could be made into a default in future.
  Varfields(COUNT (Varfields(:) /= IMDI) + 1) = Varfield_CloudRTError
  Varfields(COUNT (Varfields(:) /= IMDI) + 1) = Varfield_CloudRTBias
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_GetDefaultVarfields
