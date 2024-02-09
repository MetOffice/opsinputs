!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Setting up ODB report types for obs.
!-------------------------------------------------------------------------------

FUNCTION OpsFn_ObsToReportType (Obs,      &
                                ob_start, &
                                ob_end,   &
                                session,  &
                                RepObs) RESULT (report_types)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  gen_warn,            &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE OpsMod_ObsGroupInfo

USE OpsMod_ObsInfo, ONLY: &
  ob_type

USE OpsMod_ObsTypes

USE OpsMod_ODBInfo, ONLY:        &
  fail_with_unrecognised_satids

USE OpsMod_ODBReportTypeMap, ONLY: &
  have_read_map,                   &
  map,                             &
  Ops_ReadODBReportTypeMap

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

USE OpsMod_Satids, ONLY:    &
  SatId_Aqua,               &
  SatId_CFOSAT,             &
  SatId_ChampNOFS,          &
  SatId_Coriolis,           &
  SatId_Cosmic1,            &
  SatId_Cosmic2,            &
  SatId_Cosmic2_E1,         &
  SatId_Cosmic2_E2,         &
  SatId_Cosmic2_E3,         &
  SatId_Cosmic2_E4,         &
  SatId_Cosmic2_E5,         &
  SatId_Cosmic2_E6,         &
  SatId_Cosmic3,            &
  SatId_Cosmic4,            &
  SatId_Cosmic5,            &
  SatId_Cosmic6,            &
  SatId_Cryosat2,           &
  SatId_Cryosat2_metdb_tmp, &
  SatId_DMSPF11,            &
  SatId_DMSPF16,            &
  SatId_DMSPF17,            &
  SatId_DMSPF18,            &
  SatId_DMSPF19,            &
  SatId_ENVISAT,            &
  SatId_ERS1,               &
  SatId_ERS2,               &
  SatId_FY2C,               &
  SatId_FY2D,               &
  SatId_FY2E,               &
  SatId_FY2G,               &
  SatId_FY2H,               &
  SatId_FY3C,               &
  SatId_FY3D,               &
  SatId_FY3E,               &
  SatId_FY4A,               &
  SatId_FY4B,               &
  SatId_GeoKompsat2A,       &
  SatId_GOES11,             &
  SatId_GOES13,             &
  SatId_GOES14,             &
  SatId_GOES15,             &
  SatId_GOES16,             &
  SatId_GOES17,             &
  SatId_GOES18,             &
  SatId_GOES19,             &
  SatId_GraceA,             &
  SatId_GraceB,             &
  SatId_Himawari8,          &
  SatId_Himawari9,          &
  SatId_HY2B,               &
  SatId_HY2C,               &
  SatId_MT,                 &
  SatId_INSAT3D,            &
  SatId_INSAT3DR,           &
  SatId_INSAT3DS,           &
  SatId_ISS,                &
  SatId_JASON1,             &
  SatId_JASON2,             &
  SatId_JASON3,             &
  SatId_KOMPSAT5,           &
  SatId_LeoGeo,             &
  SatId_Meteosat7,          &
  SatId_Meteosat8,          &
  SatId_Meteosat9,          &
  SatId_Meteosat10,         &
  SatId_Meteosat11,         &
  SatId_Meteosat12,         &
  SatId_MetopA,             &
  SatId_MetopB,             &
  SatId_MetopC,             &
  SatId_MixedMetop,         &
  SatId_MSG3,               &
  SatId_MTSAT1R,            &
  SatId_MTSAT2,             &
  SatId_NOAA14,             &
  SatId_NOAA15,             &
  SatId_NOAA16,             &
  SatId_NOAA17,             &
  SatId_NOAA18,             &
  SatId_NOAA19,             &
  SatId_NOAA20,             &
  SatId_NOAA21,             &
  SatId_OceanSat2,          &
  SatId_OceanSat3,          &
  SatId_OceanSat3A,         &
  SatId_PAZ,                &
  SatId_SARAL,              &
  SatId_ScatSat1,           &
  SatId_Sentinel3A,         &
  SatId_Sentinel3B,         &
  SatId_Spire,              &
  SatId_SuomiNPP,           &
  SatId_TandemX,            &
  SatId_Terra,              &
  SatId_TerraSARX,          &
  SatId_TRMM

IMPLICIT NONE

! Function arguments:
TYPE (ob_type), INTENT(IN)                   :: Obs
INTEGER, INTENT(IN)                          :: ob_start
INTEGER, INTENT(IN)                          :: ob_end
TYPE (ODBSession_type), OPTIONAL, INTENT(IN) :: session
TYPE (ob_type), OPTIONAL, INTENT(IN)         :: RepObs

! Function result:
INTEGER                                      :: report_types(ob_end - ob_start + 1)

! Local declarations:
INTEGER                                      :: i
INTEGER                                      :: j
CHARACTER(len=*), PARAMETER                  :: RoutineName = "OpsFn_ObsToReportType"
CHARACTER(len=200)                           :: message
INTEGER                                      :: unrecognized_satids(10)
INTEGER                                      :: number_of_unrecognized_satids
LOGICAL                                      :: found_report_type

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (.NOT. have_setup_reporttypes) CALL Ops_SetupODBReportTypes (session)

IF (.NOT. have_read_map) CALL Ops_ReadODBReportTypeMap

report_types = IMDI
unrecognized_satids = IMDI
number_of_unrecognized_satids = 0

SELECT CASE (Obs % Header % ObsGroup)
  CASE (ObsGroupABIClr)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_GOES16) THEN
          report_types(i - ob_start + 1) = rep_type_goes_16_geos_rad
        ELSE IF (Obs % Satid(i) == SatId_GOES17) THEN
          report_types(i - ob_start + 1) = rep_type_goes_17_geos_rad
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupABIClr .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupAHIASR)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_Himawari8) THEN
          report_types(i - ob_start + 1) = rep_type_himawari_8_rad
        ELSE IF (Obs % Satid(i) == SatId_Himawari9) THEN
          report_types(i - ob_start + 1) = rep_type_himawari_9_rad
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupAHIASR .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupAHIClr)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_Himawari8) THEN
          report_types(i - ob_start + 1) = rep_type_himawari_8_rad
        ELSE IF (Obs % Satid(i) == SatId_Himawari9) THEN
          report_types(i - ob_start + 1) = rep_type_himawari_9_rad
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupAHIClr .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupAircraft)
    IF (Obs % Header % ObsType % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % ObsType(i) == ObsTypeAirep) THEN
          report_types(i - ob_start + 1) = rep_type_airep
        ELSE IF (Obs % ObsType(i) == ObsTypeAmdar) THEN
          report_types(i - ob_start + 1) = rep_type_amdar
        ELSE IF (Obs % ObsType(i) == ObsTypeTamdar) THEN
          report_types(i - ob_start + 1) = rep_type_tamdar
        ELSE IF (Obs % ObsType(i) == ObsTypeBogus) THEN
          report_types(i - ob_start + 1) = rep_type_bogus
        ELSE IF (Obs % ObsType(i) == ObsTypeTCBogus) THEN
          report_types(i - ob_start + 1) = rep_type_tcbogus
        ELSE IF (Obs % ObsType(i) == ObsTypeModeS) THEN
          report_types(i - ob_start + 1) = rep_type_modes
        ELSE IF (Obs % ObsType(i) == ObsTypeEmaddc) THEN
          report_types(i - ob_start + 1) = rep_type_emaddc
        ELSE
          WRITE (message, '(A,I0,A,I0)') "Unknown obs type ", Obs % ObsType(i), " for ob ", i
          CALL gen_warn (RoutineName, &
                         message)
        END IF
      END DO
    END IF
  CASE (ObsGroupAIRS)
    report_types = rep_type_aqua_airs_rad
  CASE (ObsGroupAltim)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_ERS1) THEN
          report_types(i - ob_start + 1) = rep_type_ers_1_wave
        ELSE IF (Obs % Satid(i) == SatId_ERS2) THEN
          report_types(i - ob_start + 1) = rep_type_ers_2_wave
        ELSE IF (Obs % Satid(i) == SatId_JASON1) THEN
          report_types(i - ob_start + 1) = rep_type_jason_1_wave
        ELSE IF (Obs % Satid(i) == SatId_JASON2) THEN
          report_types(i - ob_start + 1) = rep_type_jason_2_wave
        ELSE IF (Obs % Satid(i) == SatId_JASON3) THEN
          report_types(i - ob_start + 1) = rep_type_jason_3_wave
        ELSE IF (Obs % Satid(i) == SatId_Envisat) THEN
          report_types(i - ob_start + 1) = rep_type_envisat_wave
        ELSE IF (Obs % Satid(i) == SatId_SARAL) THEN
          report_types(i - ob_start + 1) = rep_type_saral_wave
        ELSE IF (Obs % Satid(i) == SatId_Sentinel3A) THEN
          report_types(i - ob_start + 1) = rep_type_sentinel_3a_wave
        ELSE IF (Obs % Satid(i) == SatId_Cryosat2 .OR. Obs % Satid(i) == SatId_Cryosat2_metdb_tmp) THEN
          report_types(i - ob_start + 1) = rep_type_cryosat_2_wave
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupAltim .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupAMSR)
    report_types = rep_type_amsr2_rad
  CASE (ObsGroupAMSUB)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_NOAA15) THEN
          report_types(i - ob_start + 1) = rep_type_noaa_15_amsub_rad
        ELSE IF (Obs % Satid(i) == SatId_NOAA16) THEN
          report_types(i - ob_start + 1) = rep_type_noaa_16_amsub_rad
        ELSE IF (Obs % Satid(i) == SatId_NOAA17) THEN
          report_types(i - ob_start + 1) = rep_type_noaa_17_amsub_rad
        ELSE IF (Obs % Satid(i) == SatId_NOAA18) THEN
          report_types(i - ob_start + 1) = rep_type_noaa_18_amsub_rad
        ELSE IF (Obs % Satid(i) == SatId_NOAA19) THEN
          report_types(i - ob_start + 1) = rep_type_noaa_19_mhs_rad
        ELSE IF (Obs % Satid(i) == SatId_MetopA) THEN
          report_types(i - ob_start + 1) = rep_type_metop_a_mhs_rad
        ELSE IF (Obs % Satid(i) == SatId_MetopB) THEN
          report_types(i - ob_start + 1) = rep_type_metop_b_mhs_rad
        ELSE IF (Obs % Satid(i) == SatId_MetopC) THEN
          report_types(i - ob_start + 1) = rep_type_metop_c_mhs_rad
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupAMSUB .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupAOD)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_METEOSAT9) THEN
          report_types(i - ob_start + 1) = rep_type_meteosat_9_aod
        ELSE IF (Obs % Satid(i) == SatId_METEOSAT10) THEN
          report_types(i - ob_start + 1) = rep_type_meteosat_10_aod
        ELSE IF (Obs % Satid(i) == SatId_Aqua) THEN
          report_types(i - ob_start + 1) = rep_type_aqua_modis_aod
        ELSE IF (Obs % Satid(i) == SatId_Terra) THEN
          report_types(i - ob_start + 1) = rep_type_terra_modis_aod
        ELSE IF (Obs % Satid(i) == SatId_Meteosat11) THEN
          report_types(i - ob_start + 1) = rep_type_meteosat_11_aod
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupAOD .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupATMS)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_SuomiNPP) THEN
          report_types(i - ob_start + 1) = rep_type_snpp_atms
        ELSE IF (Obs % Satid(i) == SatId_NOAA20) THEN
          report_types(i - ob_start + 1) = rep_type_noaa_20_atms
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupATMS .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupATOVS)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_MetopA) THEN
          report_types(i - ob_start + 1) = rep_type_atovs_metop_a
        ELSE IF (Obs % Satid(i) == SatId_MetopB) THEN
          report_types(i - ob_start + 1) = rep_type_atovs_metop_b
        ELSE IF (Obs % Satid(i) == SatId_MetopC) THEN
          report_types(i - ob_start + 1) = rep_type_atovs_metop_c
        ELSE IF (Obs % Satid(i) == SatId_NOAA15) THEN
          report_types(i - ob_start + 1) = rep_type_atovs_noaa_15
        ELSE IF (Obs % Satid(i) == SatId_NOAA18) THEN
          report_types(i - ob_start + 1) = rep_type_atovs_noaa_18
        ELSE IF (Obs % Satid(i) == SatId_NOAA19) THEN
          report_types(i - ob_start + 1) = rep_type_atovs_noaa_19
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupATOVS .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupCrIS)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_SuomiNPP) THEN
          report_types(i - ob_start + 1) = rep_type_snpp_cris
        ELSE IF (Obs % Satid(i) == SatId_NOAA20) THEN
          report_types(i - ob_start + 1) = rep_type_noaa_20_cris
        ELSE IF (Obs % Satid(i) == SatId_FY3D) THEN
          report_types(i - ob_start + 1) = rep_type_fy3d_cris
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupCrIS .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupCOMSMIClr)
    report_types = rep_type_kma_geos_rad
  CASE (ObsGroupFCIClr, ObsGroupFCIASR)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_Meteosat12) THEN
          report_types(i - ob_start + 1) = rep_type_meteosat_12_geos_rad
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF ((map(j,1) == ObsGroupFCIClr .AND. map(j,2) == Obs % Satid(i)) .OR. &
                (map(j,1) == ObsGroupFCIASR .AND. map(j,2) == Obs % Satid(i))) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupMWSFY3B)
    report_types = rep_type_fy3b
  CASE (ObsGroupGeoCloud)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_Himawari8) THEN
          report_types(i - ob_start + 1) = rep_type_himawari_8_rad
        ELSE IF (Obs % Satid(i) == SatId_Himawari9) THEN
          report_types(i - ob_start + 1) = rep_type_himawari_9_rad
        ELSE IF (Obs % Satid(i) == SatId_METEOSAT8) THEN
          report_types(i - ob_start + 1) = rep_type_meteosat_8_geos_rad
        ELSE IF (Obs % Satid(i) == SatId_METEOSAT9) THEN
          report_types(i - ob_start + 1) = rep_type_meteosat_9_geos_rad
        ELSE IF (Obs % Satid(i) == SatId_METEOSAT10) THEN
          report_types(i - ob_start + 1) = rep_type_meteosat_10_geos_rad
        ELSE IF (Obs % Satid(i) == SatId_METEOSAT11) THEN
          report_types(i - ob_start + 1) = rep_type_meteosat_11_geos_rad
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupGeoCloud .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupGMIHigh)
    report_types = rep_type_gmi_rad_high
  CASE (ObsGroupGMILow)
    report_types = rep_type_gmi_rad_low
  CASE (ObsGroupGOESImClr)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_GOES13) THEN
          report_types(i - ob_start + 1) = rep_type_goes_13_geos_rad
        ELSE IF (Obs % Satid(i) == SatId_GOES14) THEN
          report_types(i - ob_start + 1) = rep_type_goes_14_geos_rad
        ELSE IF (Obs % Satid(i) == SatId_GOES15) THEN
          report_types(i - ob_start + 1) = rep_type_goes_15_geos_rad
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupGOESImClr .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupGPSRO)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_MetopB) THEN
          report_types(i - ob_start + 1) = rep_type_metop_b_gpsro
        ELSE IF (Obs % Satid(i) == SatId_MetopA) THEN
          report_types(i - ob_start + 1) = rep_type_metop_a_gpsro
        ELSE IF (Obs % Satid(i) == SatId_MetopC) THEN
          report_types(i - ob_start + 1) = rep_type_metop_c_gpsro
        ELSE IF (Obs % Satid(i) == SatId_TerraSARX) THEN
          report_types(i - ob_start + 1) = rep_type_terrasar_x_gpsro
        ELSE IF (Obs % Satid(i) == SatId_TandemX) THEN
          report_types(i - ob_start + 1) = rep_type_tandem_x_gpsro
        ELSE IF (Obs % Satid(i) == SatId_GraceA) THEN
          report_types(i - ob_start + 1) = rep_type_grace_a_gpsro
        ELSE IF (Obs % Satid(i) == SatId_GraceB) THEN
          report_types(i - ob_start + 1) = rep_type_grace_b_gpsro
        ELSE IF (Obs % Satid(i) == SatId_Cosmic1) THEN
          report_types(i - ob_start + 1) = rep_type_cosmic_1_gpsro
        ELSE IF (Obs % Satid(i) == SatId_Cosmic2) THEN
          report_types(i - ob_start + 1) = rep_type_cosmic_2_gpsro
        ELSE IF (Obs % Satid(i) == SatId_Cosmic3) THEN
          report_types(i - ob_start + 1) = rep_type_cosmic_3_gpsro
        ELSE IF (Obs % Satid(i) == SatId_Cosmic4) THEN
          report_types(i - ob_start + 1) = rep_type_cosmic_4_gpsro
        ELSE IF (Obs % Satid(i) == SatId_Cosmic5) THEN
          report_types(i - ob_start + 1) = rep_type_cosmic_5_gpsro
        ELSE IF (Obs % Satid(i) == SatId_Cosmic6) THEN
          report_types(i - ob_start + 1) = rep_type_cosmic_6_gpsro
        ELSE IF (Obs % Satid(i) == SatId_ChampNOFS) THEN
          report_types(i - ob_start + 1) = rep_type_cnofs_gpsro
        ELSE IF (Obs % Satid(i) == SatId_FY3C) THEN
          report_types(i - ob_start + 1) = rep_type_fy3c_gpsro
        ELSE IF (Obs % Satid(i) == SatId_FY3D) THEN
          report_types(i - ob_start + 1) = rep_type_fy3d_gpsro
        ELSE IF (Obs % Satid(i) == SatId_MT) THEN
          report_types(i - ob_start + 1) = rep_type_megha_tropiques_gpsro
        ELSE IF (Obs % Satid(i) == SatId_KOMPSAT5) THEN
          report_types(i - ob_start + 1) = rep_type_kompsat_5_gpsro
        ELSE IF (Obs % Satid(i) == SatId_PAZ) THEN
          report_types(i - ob_start + 1) = rep_type_paz_gpsro
        ELSE IF (Obs % Satid(i) == SatId_Cosmic2_E1) THEN
          report_types(i - ob_start + 1) = rep_type_cosmic2_e1_gpsro
        ELSE IF (Obs % Satid(i) == SatId_Cosmic2_E2) THEN
          report_types(i - ob_start + 1) = rep_type_cosmic2_e2_gpsro
        ELSE IF (Obs % Satid(i) == SatId_Cosmic2_E3) THEN
          report_types(i - ob_start + 1) = rep_type_cosmic2_e3_gpsro
        ELSE IF (Obs % Satid(i) == SatId_Cosmic2_E4) THEN
          report_types(i - ob_start + 1) = rep_type_cosmic2_e4_gpsro
        ELSE IF (Obs % Satid(i) == SatId_Cosmic2_E5) THEN
          report_types(i - ob_start + 1) = rep_type_cosmic2_e5_gpsro
        ELSE IF (Obs % Satid(i) == SatId_Cosmic2_E6) THEN
          report_types(i - ob_start + 1) = rep_type_cosmic2_e6_gpsro
        ELSE IF (Obs % Satid(i) == SatId_Spire) THEN
          report_types(i - ob_start + 1) = rep_type_spire_gpsro
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupGPSRO .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupGroundGPS)
    report_types = rep_type_ground_gps
  CASE (ObsGroupGroundLidar)
    report_types = rep_type_ceilometer_lidar
  CASE (ObsGroupHIRAS)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_FY3D) THEN
          report_types(i - ob_start + 1) = rep_type_fy3_d_hiras_rad 
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupHIRAS .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupGIIRSLW, ObsGroupGIIRSMW)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_FY4A) THEN
          report_types(i - ob_start + 1) = rep_type_giirs_fy4a
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupGIIRSLW .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            ELSE IF (map(j,1) == ObsGroupGIIRSMW .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupHLOSWIND)
    report_types = rep_type_aladin_hloswind
  CASE (ObsGroupIASI)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_MetopB) THEN
          report_types(i - ob_start + 1) = rep_type_metop_b_iasi_rad
        ELSE IF (Obs % Satid(i) == SatId_MetopA) THEN
          report_types(i - ob_start + 1) = rep_type_metop_a_iasi_rad
        ELSE IF (Obs % Satid(i) == SatId_MetopC) THEN
          report_types(i - ob_start + 1) = rep_type_metop_c_iasi_rad
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupIASI .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupIN3DIClr)
    report_types = rep_type_insat_3d_imager
  CASE (ObsGroupIN3DS)
    report_types = rep_type_insat_3d_sounder
  CASE (ObsGroupSAPHIR)
    report_types = rep_type_saphir_rad
  CASE (ObsGroupMTSATImClr)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_MTSAT1R) THEN
          report_types(i - ob_start + 1) = rep_type_mtsat_1r_geos_rad
        ELSE IF (Obs % Satid(i) == SatId_MTSAT2) THEN
          report_types(i - ob_start + 1) = rep_type_mtsat_2r_geos_rad
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupMTSATImClr .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupMVIRIClr)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_Meteosat7) THEN
          report_types(i - ob_start + 1) = rep_type_meteosat_7_geos_rad
        ELSE IF (Obs % Satid(i) == SatId_Meteosat9) THEN
          report_types(i - ob_start + 1) = rep_type_meteosat_9_geos_rad
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupMVIRIClr .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupMWRI)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_FY3C) THEN
          report_types(i - ob_start + 1) = rep_type_mwri_fy3c
        ELSE IF (Obs % Satid(i) == SatId_FY3D) THEN
          report_types(i - ob_start + 1) = rep_type_mwri_fy3d
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupMWRI .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupMWSFY3)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_FY3C) THEN
          report_types(i - ob_start + 1) = rep_type_mwts2_fy3c
        ELSE IF (Obs % Satid(i) == SatId_FY3D) THEN
          report_types(i - ob_start + 1) = rep_type_mwts2_fy3d
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupMWSFY3 .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupOcean)
    IF (Obs % Header % ObsType % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % ObsType(i) == ObsTypeBathy) THEN
          report_types(i - ob_start + 1) = rep_type_dribu_bathy
        ELSE IF (Obs % ObsType(i) == ObsTypeArgo .OR. &
                 Obs % ObsType(i) == ObsTypeArgoBufr .OR. &
                 Obs % ObsType(i) == ObsTypeTesac) THEN
          report_types(i - ob_start + 1) = rep_type_dribu_tesac
        ELSE IF (Obs % ObsType(i) == ObsTypeBuoyProf) THEN
          report_types(i - ob_start + 1) = rep_type_dribu
        ELSE IF (Obs % ObsType(i) == ObsTypeBuoyBufr) THEN
          report_types(i - ob_start + 1) = rep_type_moored_buoy_bufr
        ELSE IF (Obs % ObsType(i) == ObsTypeOceanFB) THEN
          report_types(i - ob_start + 1) = rep_type_dribu
        ELSE IF (Obs % ObsType(i) == ObsTypeOceanRE) THEN
          report_types(i - ob_start + 1) = rep_type_dribu
        ELSE IF (Obs % ObsType(i) == ObsTypeOceanTS) THEN
          report_types(i - ob_start + 1) = rep_type_dribu
        ELSE IF (Obs % ObsType(i) == ObsTypeCopTSMO) THEN
          report_types(i - ob_start + 1) = rep_type_moored_buoy
        ELSE
          WRITE (message, '(A,I0,A,I0)') "Unknown obs type ", Obs % ObsType(i), " for ob ", i
          CALL gen_warn (RoutineName, &
                         message)
        END IF
      END DO
    END IF
  CASE (ObsGroupOceanColour)
    report_types = rep_type_oceancolour
  CASE (ObsGroupOceanWinds)
    report_types = rep_type_oceanwinds
  CASE (ObsGroupPrecip)
    report_types = rep_type_ground_based_radar
  CASE (ObsGroupRadar)
    report_types = rep_type_radar_scan
  CASE (ObsGroupRadarN)
    report_types = rep_type_radar_scan
  CASE (ObsGroupRainAccum)
    report_types = rep_type_radar_scan
  CASE (ObsGroupRadarZ)
    report_types = rep_type_ground_based_radar
  CASE (ObsGroupSatSST)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_MetopB) THEN
          report_types(i - ob_start + 1) = rep_type_metop_b_satsst
        ELSE IF (Obs % Satid(i) == SatId_MetopC) THEN
          report_types(i - ob_start + 1) = rep_type_metop_c_satsst
        ELSE IF (Obs % Satid(i) == SatId_Meteosat9) THEN
          report_types(i - ob_start + 1) = rep_type_meteosat_9_satsst
        ELSE IF (Obs % Satid(i) == SatId_Envisat) THEN
          report_types(i) = rep_type_envisat_satsst
        ELSE IF (Obs % Satid(i) == SatId_NOAA18) THEN
          report_types(i - ob_start + 1) = rep_type_noaa_18_satsst
        ELSE IF (Obs % Satid(i) == SatId_NOAA19) THEN
          report_types(i - ob_start + 1) = rep_type_noaa_19_satsst
        ELSE IF (Obs % Satid(i) == SatId_GOES13) THEN
          report_types(i - ob_start + 1) = rep_type_goes_13_satsst
        ELSE IF (Obs % Satid(i) == SatId_TRMM) THEN
          report_types(i - ob_start + 1) = rep_type_trmm_satsst
        ELSE IF (Obs % Satid(i) == SatId_MSG3) THEN
          report_types(i - ob_start + 1) = rep_type_msg3_satsst
        ELSE IF (Obs % Satid(i) == SatId_DMSPF11) THEN
          report_types(i - ob_start + 1) = rep_type_dmspf11_satsst
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupSatSST .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupSatTCWV)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_Envisat) THEN
          report_types = rep_type_envisat_meris_tcwv
        ELSE IF (Obs % Satid(i) == SatId_Sentinel3A) THEN
          report_types(i - ob_start + 1) = rep_type_sentinel_3a_olci_tcwv
        ELSE IF (Obs % Satid(i) == SatId_Sentinel3B) THEN
          report_types(i - ob_start + 1) = rep_type_sentinel_3b_olci_tcwv
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupScatwind .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupSatwind)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_MetopB) THEN
          report_types(i - ob_start + 1) = rep_type_metop_b_amv
        ELSE IF (Obs % Satid(i) == SatId_MetopA) THEN
          report_types(i - ob_start + 1) = rep_type_metop_a_amv
        ELSE IF (Obs % Satid(i) == SatId_MetopC) THEN
          report_types(i - ob_start + 1) = rep_type_metop_c_amv
        ELSE IF (Obs % Satid(i) == SatId_Meteosat7) THEN
          report_types(i - ob_start + 1) = rep_type_meteosat_7_amv
        ELSE IF (Obs % Satid(i) == SatId_Meteosat8) THEN
          report_types(i - ob_start + 1) = rep_type_meteosat_8_amv
        ELSE IF (Obs % Satid(i) == SatId_Meteosat9) THEN
          report_types(i - ob_start + 1) = rep_type_meteosat_9_amv
        ELSE IF (Obs % Satid(i) == SatId_Meteosat10) THEN
          report_types(i - ob_start + 1) = rep_type_meteosat_10_amv
        ELSE IF (Obs % Satid(i) == SatId_Meteosat11) THEN
          report_types(i - ob_start + 1) = rep_type_meteosat_11_amv
        ELSE IF (Obs % Satid(i) == SatId_MTSAT1R) THEN
          report_types(i - ob_start + 1) = rep_type_mtsat_1r_amv
        ELSE IF (Obs % Satid(i) == SatId_MTSAT2) THEN
          report_types(i - ob_start + 1) = rep_type_mtsat_2_amv
        ELSE IF (Obs % Satid(i) == SatId_NOAA15) THEN
          report_types(i - ob_start + 1) = rep_type_noaa_15_amv
        ELSE IF (Obs % Satid(i) == SatId_NOAA16) THEN
          report_types(i - ob_start + 1) = rep_type_noaa_16_amv
        ELSE IF (Obs % Satid(i) == SatId_NOAA18) THEN
          report_types(i - ob_start + 1) = rep_type_noaa_18_amv
        ELSE IF (Obs % Satid(i) == SatId_NOAA19) THEN
          report_types(i - ob_start + 1) = rep_type_noaa_19_amv
        ELSE IF (Obs % Satid(i) == SatId_NOAA20) THEN
          report_types(i - ob_start + 1) = rep_type_noaa_20_amv
        ELSE IF (Obs % Satid(i) == SatId_NOAA21) THEN
          report_types(i - ob_start + 1) = rep_type_noaa_21_amv
        ELSE IF (Obs % Satid(i) == SatId_GOES11) THEN
          report_types(i - ob_start + 1) = rep_type_goes_11_amv
        ELSE IF (Obs % Satid(i) == SatId_GOES13) THEN
          report_types(i - ob_start + 1) = rep_type_goes_13_amv
        ELSE IF (Obs % Satid(i) == SatId_GOES14) THEN
          report_types(i - ob_start + 1) = rep_type_goes_14_amv
        ELSE IF (Obs % Satid(i) == SatId_GOES15) THEN
          report_types(i - ob_start + 1) = rep_type_goes_15_amv
        ELSE IF (Obs % Satid(i) == SatId_GOES16) THEN
          report_types(i - ob_start + 1) = rep_type_goes_16_amv
        ELSE IF (Obs % Satid(i) == SatId_GOES17) THEN
          report_types(i - ob_start + 1) = rep_type_goes_17_amv
        ELSE IF (Obs % Satid(i) == SatId_GOES18) THEN
          report_types(i - ob_start + 1) = rep_type_goes_18_amv
        ELSE IF (Obs % Satid(i) == SatId_GOES19) THEN
          report_types(i - ob_start + 1) = rep_type_goes_19_amv
        ELSE IF (Obs % Satid(i) == SatId_Himawari8) THEN
          report_types(i - ob_start + 1) = rep_type_himawari_8_amv
        ELSE IF (Obs % Satid(i) == SatId_Himawari9) THEN
          report_types(i - ob_start + 1) = rep_type_himawari_9_amv
        ELSE IF (Obs % Satid(i) == SatId_MT) THEN
          report_types(i - ob_start + 1) = rep_type_imd_amv
        ELSE IF (Obs % Satid(i) == SatId_FY2C) THEN
          report_types(i - ob_start + 1) = rep_type_fy2c_amv
        ELSE IF (Obs % Satid(i) == SatId_FY2D) THEN
          report_types(i - ob_start + 1) = rep_type_fy2d_amv
        ELSE IF (Obs % Satid(i) == SatId_FY2E) THEN
          report_types(i - ob_start + 1) = rep_type_fy2e_amv
        ELSE IF (Obs % Satid(i) == SatId_FY2G) THEN
          report_types(i - ob_start + 1) = rep_type_fy2g_amv
        ELSE IF (Obs % Satid(i) == SatId_FY2H) THEN
          report_types(i - ob_start + 1) = rep_type_fy2h_amv
        ELSE IF (Obs % Satid(i) == SatId_FY4A) THEN
          report_types(i - ob_start + 1) = rep_type_fy4a_amv
        ELSE IF (Obs % Satid(i) == SatId_FY4B) THEN
          report_types(i - ob_start + 1) = rep_type_fy4b_amv
        ELSE IF (Obs % Satid(i) == SatId_Terra) THEN
          IF (Obs % SubType(i) == ObsTypeStereomv) THEN
            report_types(i - ob_start + 1) = rep_type_stereo_mv
          ELSE
            report_types(i - ob_start + 1) = rep_type_terra_modis_amv
          END IF
        ELSE IF (Obs % Satid(i) == SatId_Aqua) THEN
          report_types(i - ob_start + 1) = rep_type_aqua_modis_amv
        ELSE IF (Obs % Satid(i) == SatId_LeoGeo) THEN
          report_types(i - ob_start + 1) = rep_type_cimss_leogeo_amv
        ELSE IF (Obs % Satid(i) == SatId_MixedMetop) THEN
          report_types(i - ob_start + 1) = rep_type_combined_metop_amv
        ELSE IF (Obs % Satid(i) == SatId_INSAT3D) THEN
          report_types(i - ob_start + 1) = rep_type_insat_3d_amv
        ELSE IF (Obs % Satid(i) == SatId_INSAT3DR) THEN
          report_types(i - ob_start + 1) = rep_type_insat_3dr_amv
        ELSE IF (Obs % Satid(i) == SatId_INSAT3DS) THEN
          report_types(i - ob_start + 1) = rep_type_insat_3ds_amv
        ELSE IF (Obs % Satid(i) == SatId_SuomiNPP) THEN
          report_types(i - ob_start + 1) = rep_type_suomi_npp_amv
        ELSE IF (Obs % Satid(i) == SatId_GeoKompsat2A) THEN
          report_types(i - ob_start + 1) = rep_type_geokompsat_2a_amv
        ELSE IF (Obs % Satid(i) == SatId_Sentinel3A) THEN
          report_types(i - ob_start + 1) = rep_type_sentinel_3a_amv
        ELSE IF (Obs % Satid(i) == SatId_Sentinel3B) THEN
          report_types(i - ob_start + 1) = rep_type_sentinel_3b_amv
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupSatwind .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          IF (.NOT. found_report_type) CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupSBUV)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_NOAA16) THEN
          report_types(i - ob_start + 1) = rep_type_noaa_16_sbuv
        ELSE IF (Obs % Satid(i) == SatId_NOAA17) THEN
          report_types(i - ob_start + 1) = rep_type_noaa_17_sbuv
        ELSE IF (Obs % Satid(i) == SatId_NOAA18) THEN
          report_types(i - ob_start + 1) = rep_type_noaa_18_sbuv
        ELSE IF (Obs % Satid(i) == SatId_NOAA19) THEN
          report_types(i - ob_start + 1) = rep_type_noaa_19_sbuv
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupSBUV .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupScatwind, ObsGroupScatwindChosen)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_MetopB) THEN
          report_types(i - ob_start + 1) = rep_type_metop_b_ascat
        ELSE IF (Obs % Satid(i) == SatId_MetopA) THEN
          report_types(i - ob_start + 1) = rep_type_metop_a_ascat
        ELSE IF (Obs % Satid(i) == SatId_MetopC) THEN
          report_types(i - ob_start + 1) = rep_type_metop_c_ascat
        ELSE IF (Obs % Satid(i) == SatId_Coriolis) THEN
          report_types(i - ob_start + 1) = rep_type_coriolis_windsat
        ELSE IF (Obs % Satid(i) == SatId_OceanSat2) THEN
          report_types(i - ob_start + 1) = rep_type_oceansat_2_ascat
        ELSE IF (Obs % Satid(i) == SatId_OceanSat3) THEN
          report_types(i - ob_start + 1) = rep_type_oceansat_3_ascat
        ELSE IF (Obs % Satid(i) == SatId_OceanSat3A) THEN
          report_types(i - ob_start + 1) = rep_type_oceansat_3a_ascat
        ELSE IF (Obs % Satid(i) == SatId_ISS) THEN
          report_types(i - ob_start + 1) = rep_type_iss_ascat
        ELSE IF (Obs % Satid(i) == SatId_ScatSat1) THEN
          report_types(i - ob_start + 1) = rep_type_scatsat1_ascat
        ELSE IF (Obs % Satid(i) == SatId_HY2B) THEN
          report_types(i - ob_start + 1) = rep_type_hy2b_ascat
        ELSE IF (Obs % Satid(i) == SatId_HY2C) THEN
          report_types(i - ob_start + 1) = rep_type_hy2c_ascat
        ELSE IF (Obs % Satid(i) == SatId_FY3E) THEN
          report_types(i - ob_start + 1) = rep_type_fy3e_ascat
        ELSE IF (Obs % Satid(i) == SatId_CFOSAT) THEN
          report_types(i - ob_start + 1) = rep_type_cfosat_ascat
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF ((map(j,1) == ObsGroupScatwind .AND. map(j,2) == Obs % Satid(i)) .OR. &
                (map(j,1) == ObsGroupScatwindChosen .AND. map(j,2) == Obs % Satid(i))) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupSeaIce)
    report_types = rep_type_osseaice
  CASE (ObsGroupSEVIRIClr, ObsGroupSEVIRIASR)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_Meteosat8) THEN
          report_types(i - ob_start + 1) = rep_type_meteosat_8_geos_rad
        ELSE IF (Obs % Satid(i) == SatId_Meteosat9) THEN
          report_types(i - ob_start + 1) = rep_type_meteosat_9_geos_rad
        ELSE IF (Obs % Satid(i) == SatId_Meteosat10) THEN
          report_types(i - ob_start + 1) = rep_type_meteosat_10_geos_rad
        ELSE IF (Obs % Satid(i) == SatId_Meteosat11) THEN
          report_types(i - ob_start + 1) = rep_type_meteosat_11_geos_rad
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF ((map(j,1) == ObsGroupSEVIRIClr .AND. map(j,2) == Obs % Satid(i)) .OR. &
                (map(j,1) == ObsGroupSEVIRIASR .AND. map(j,2) == Obs % Satid(i))) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupSonde)
    IF (Obs % Header % ObsType % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % ObsType(i) == ObsTypeDropsonde) THEN
          report_types(i - ob_start + 1) = rep_type_dropsond
        ELSE IF (Obs % ObsType(i) == ObsTypeSonde .OR. Obs % ObsType(i) == ObsTypeTSTSonde) THEN
          IF (PRESENT (RepObs)) THEN
            SELECT CASE (RepObs % SondeReportType(i))
              CASE (3)
                report_types(i - ob_start + 1) = rep_type_bufr_ship_temp
              CASE DEFAULT
                report_types(i - ob_start + 1) = rep_type_bufr_land_temp
            END SELECT
          ELSE
            SELECT CASE (Obs % SondeReportType(i))
              CASE (3)
                report_types(i - ob_start + 1) = rep_type_bufr_ship_temp
              CASE DEFAULT
                report_types(i - ob_start + 1) = rep_type_bufr_land_temp
            END SELECT
          END IF
        ELSE IF (Obs % ObsType(i) == ObsTypePilot .OR. &
                 Obs % ObsType(i) == ObstypePilotLand) THEN
          report_types(i - ob_start + 1) = rep_type_land_pilot
        ELSE IF (Obs % ObsType(i) == ObsTypePilotMobile) THEN
          report_types(i - ob_start + 1) = rep_type_mobile_pilot
        ELSE IF (Obs % ObsType(i) == ObsTypePilotShip) THEN
          report_types(i - ob_start + 1) = rep_type_ship_pilot
        ELSE IF (Obs % ObsType(i) == ObsTypeTemp .OR. &
                 Obs % ObsType(i) == ObstypeTempLand) THEN
          report_types(i - ob_start + 1) = rep_type_land_temp
        ELSE IF (Obs % ObsType(i) == ObsTypeTempMobile) THEN
          report_types(i - ob_start + 1) = rep_type_mobile_temp
        ELSE IF (Obs % ObsType(i) == ObsTypeTempShip) THEN
          report_types(i - ob_start + 1) = rep_type_ship_temp
        ELSE IF (Obs % ObsType(i) == ObsTypeWindProf) THEN
          report_types(i - ob_start + 1) = rep_type_american_wind_prof_1
        ELSE
          WRITE (message, '(A,I0,A,I0)') "Unknown obs type ", Obs % ObsType(i), " for ob ", i
          CALL gen_warn (RoutineName, &
                         message)
        END IF
      END DO
    END IF
  CASE (ObsGroupSSMIS)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_DMSPF16) THEN
          report_types(i - ob_start + 1) = rep_type_dmsp_16_ssmis_rad
        ELSE IF (Obs % Satid(i) == SatId_DMSPF17) THEN
          report_types(i - ob_start + 1) = rep_type_dmsp_17_ssmis_rad
        ELSE IF (Obs % Satid(i) == SatId_DMSPF18) THEN
          report_types(i - ob_start + 1) = rep_type_dmsp_18_ssmis_rad
        ELSE IF (Obs % Satid(i) == SatId_DMSPF19) THEN
          report_types(i - ob_start + 1) = rep_type_dmsp_19_ssmis_rad
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupSSMIS .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE (ObsGroupSurface, ObsGroupSurfaceCloud)
    IF (Obs % Header % ObsType % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % ObsType(i) == ObsTypeBuoy .OR. &
            Obs % ObsType(i) == ObsTypeDrfBuoySpOb .OR. &
            Obs % ObsType(i) == ObsTypeDrifter .OR. &
            Obs % ObsType(i) == ObsTypeMooredBuoy .OR. &
            Obs % ObsType(i) == ObsTypeMrdBuoySpOb) THEN
          report_types(i - ob_start + 1) = rep_type_dribu
        ELSE IF (Obs % ObsType(i) == ObsTypeMooredBuoyBufr) THEN
          report_types(i - ob_start + 1) = rep_type_moored_buoy_bufr
        ELSE IF (Obs % ObsType(i) == ObsTypeDrifterBuoyBufr) THEN
          report_types(i - ob_start + 1) = rep_type_drifting_buoy_bufr
        ELSE IF (Obs % ObsType(i) == ObsTypeMetar .OR. &
                 Obs % ObsType(i) == ObsTypeMetarManual .OR. &
                 Obs % ObsType(i) == ObsTypeMetarAuto) THEN
          report_types(i - ob_start + 1) = rep_type_metar
        ELSE IF (Obs % ObsType(i) == ObsTypePlatsRiggs .OR. &
                 Obs % ObsType(i) == ObsTypeShip .OR. &
                 Obs % ObsType(i) == ObsTypeShipAuto .OR. &
                 Obs % ObsType(i) == ObsTypeShipDrifter .OR. &
                 Obs % ObsType(i) == ObsTypeShipMrdBuoy .OR. &
                 Obs % ObsType(i) == ObsTypeShpDrftSpOb .OR. &
                 Obs % ObsType(i) == ObsTypeShpMrdBySpOb) THEN
          IF (Obs % SubType(i) == ObsTypeShipBufr) THEN
            report_types(i - ob_start + 1) = rep_type_bufr_ship
          ELSE
            report_types(i - ob_start + 1) = rep_type_auto_ship
          END IF
        ELSE IF (Obs % ObsType(i) == ObsTypeShipManual) THEN
          IF (Obs % SubType(i) == ObsTypeShipBufr) THEN
            report_types(i - ob_start + 1) = rep_type_bufr_ship
          ELSE
            report_types(i - ob_start + 1) = rep_type_ship
          END IF
        ELSE IF (Obs % ObsType(i) == ObsTypeSynop .OR. &
                 Obs % ObsType(i) == ObstypeSynopAuto) THEN
          report_types(i - ob_start + 1) = rep_type_auto_land_synop
        ELSE IF (Obs % ObsType(i) == ObsTypeSynopManual) THEN
          report_types(i - ob_start + 1) = rep_type_manual_land_synop
        ELSE IF (Obs % ObsType(i) == ObsTypeSynopMob) THEN
          report_types(i - ob_start + 1) = rep_type_mobile_land_synop
        ELSE IF (Obs % ObsType(i) == ObsTypeBathy) THEN
          report_types(i - ob_start + 1) = rep_type_dribu_bathy
        ELSE IF (Obs % ObsType(i) == ObsTypeBogus) THEN
          report_types(i - ob_start + 1) = rep_type_bogus
        ELSE IF (Obs % ObsType(i) == ObsTypeOpenRoad) THEN
          report_types(i - ob_start + 1) = rep_type_roadside_sensor
        ELSE IF (Obs % ObsType(i) == ObsTypeAirQal) THEN
          report_types(i - ob_start + 1) = rep_type_air_quality
        ELSE IF (Obs % ObsType(i) == ObsTypeAirQalEU) THEN
          report_types(i - ob_start + 1) = rep_type_air_quality
        ELSE IF (Obs % ObsType(i) == ObsTypeSferics) THEN
          report_types(i - ob_start + 1) = rep_type_lightning_report
        ELSE IF (Obs % ObsType(i) == ObsTypeSrew) THEN
          report_types(i - ob_start + 1) = rep_type_rainfall_accumulation_1h
        ELSE IF (Obs % ObsType(i) == ObsTypeWavenet) THEN
          report_types(i - ob_start + 1) = rep_type_ocean_wave
        ELSE IF (Obs % ObsType(i) == ObsTypeAtdnet) THEN
          report_types(i - ob_start + 1) = rep_type_atdnet
        ELSE IF (Obs % ObsType(i) == ObsTypeWOW) THEN
          report_types(i - ob_start + 1) = rep_type_wow
        ELSE IF (Obs % ObsType(i) == ObsTypeTCBogus) THEN
          report_types(i - ob_start + 1) = rep_type_tcbogus
        ELSE IF (Obs % ObsType(i) == ObsTypeSynopBufr) THEN
          report_types(i - ob_start + 1) = rep_type_bufr_land_synop
        ELSE
          WRITE (message, '(A,I0,A,I0)') "Unknown obs type ", Obs % ObsType(i), " for ob ", i
          CALL gen_warn (RoutineName, &
                         message)
        END IF
      END DO
    END IF
  CASE (ObsGroupTOVS)
    IF (Obs % Header % Satid % Present) THEN
      DO i = ob_start, ob_end
        IF (Obs % Satid(i) == SatId_NOAA14) THEN
          report_types(i - ob_start + 1) = rep_type_tovs
        ELSE
          found_report_type = .FALSE.
          DO j = 1, SIZE (map, DIM = 1)
            IF (map(j,1) == ObsGroupTOVS .AND. map(j,2) == Obs % Satid(i)) THEN
              report_types(i - ob_start + 1) = map(j,3)
              found_report_type = .TRUE.
              EXIT
            END IF
          END DO
          CALL update_unrecognized_satids (Obs % Satid(i))
        END IF
      END DO
    END IF
  CASE DEFAULT
    WRITE (message, '(A,I0)') "Unknown obs group ", Obs % Header % ObsGroup
    CALL gen_fail (RoutineName, &
                   message)
END SELECT

IF (number_of_unrecognized_satids > 0) THEN
  WRITE (message, '(A,10(1X,I0))') "Unrecognized satellite identifiers (maximum of 10):", &
                                   unrecognized_satids(1:MIN (SIZE (unrecognized_satids), number_of_unrecognized_satids))
  IF (fail_with_unrecognised_satids) THEN
    CALL gen_fail (RoutineName, &
                   message)
  ELSE
    CALL gen_warn (RoutineName, &
                   message)
  END IF
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

CONTAINS

SUBROUTINE update_unrecognized_satids (satid)

! Subroutine arguments:
INTEGER, INTENT(IN) :: satid

IF (number_of_unrecognized_satids == 0) THEN
  number_of_unrecognized_satids = number_of_unrecognized_satids + 1
  unrecognized_satids(number_of_unrecognized_satids) = satid
ELSE IF (number_of_unrecognized_satids > 0 .AND. number_of_unrecognized_satids < SIZE (unrecognized_satids)) THEN
  IF (.NOT. ANY (unrecognized_satids(1:number_of_unrecognized_satids) == satid)) THEN
    number_of_unrecognized_satids = number_of_unrecognized_satids + 1
    unrecognized_satids(number_of_unrecognized_satids) = satid
  END IF
END IF

END SUBROUTINE update_unrecognized_satids

END FUNCTION OpsFn_ObsToReportType
