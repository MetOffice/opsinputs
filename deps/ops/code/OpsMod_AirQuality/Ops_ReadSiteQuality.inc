!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Reads a AIRQAL site quality data.
!
! In / Outputs:
!     Obs
!
! Assigns values to Obs % SITE_QAL obtained from Surface_AIRQSITEQAL.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ReadSiteQuality (Obs)

USE GenMod_Core, ONLY: &
    gen_warn,          &
    gen_fail,          &
    gen_trace_entry,   &
    gen_trace_exit,    &
    StatusWarning,     &
    UseTrace

USE OpsMod_EnvUtils, ONLY: &
    ops_get_env

USE OpsMod_IOUtils, ONLY: &
    Ops_CloseFile,        &
    Ops_OpenPathRead

USE OpsMod_Kinds, ONLY: &
    Integer_Default

USE OpsMod_ObsInfo, ONLY: &
    OB_type,              &
    SiteQualityFlag

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_type), INTENT(INOUT) :: Obs

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName = "Ops_ReadSiteQuality"
CHARACTER(len=8)              :: stationid
CHARACTER(len=3)              :: site
CHARACTER(len=3)              :: area
INTEGER                       :: SiteQalUnit
INTEGER                       :: iOb
INTEGER(kind=integer_default) :: ErrStat_io
REAL                          :: o3
REAL                          :: no2
REAL                          :: pm10
REAL                          :: nox
REAL                          :: no
REAL                          :: so2
REAL                          :: co
REAL                          :: pm2p5
INTEGER                       :: siterej
REAL                          :: lat
REAL                          :: lon

!-----------------------------------------------------------------------------
! 0. Initialisation and tracing.
!-----------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-----------------------------------------------------------------------------
! 1. Read the namelist to obtain extraction info.
!-----------------------------------------------------------------------------

CALL Ops_OpenPathRead ("OPS_STATIONLIST_DIR",                 &
                       SiteQalUnit,                           &
                       filename = "Surface_AIRQSITEQAL",      &
                       status_if_env_not_set = StatusWarning, &
                       status_if_open_failed = StatusWarning)

CALL ops_get_env ("OPS_SITEQUALITY_REJ",     &
                  siterej,                   &
                  result_if_env_not_set = 6)

IF (SiteQalUnit < 0) THEN
  CALL gen_warn (RoutineName,                                     &
                 'Warning, Air quality stations file not found.')
ELSE IF (siterej >= 0 .AND. siterej <= 10) THEN
  READ (SiteQalUnit, *) ! Skip header

  DO
    READ (SiteQalUnit,*, IOSTAT = ErrStat_io)                            &
          stationid, site, area, lat, lon, o3, no2, pm10, nox, no, so2,  &
          co, pm2p5

    IF (ErrStat_io < 0) THEN
      EXIT
    ELSE IF (ErrStat_io > 0) THEN
      CALL gen_fail (RoutineName,                          &
                     'Problem reading line in Surface_AIRQSITEQAL')
    END IF

    DO iOb = 1, Obs % Header % NumObsLocal
      IF (stationid == ADJUSTL (Obs % GEMS_AQTY_OBSG_STTN(iOb))) THEN
        SELECT CASE (Obs % CSNT_TYP(iOb))
          CASE (SO2_CSNT)
            Obs % SITE_QAL(iOb) = so2
            IF (so2 >= siterej) Obs % SO2_DNSTY(iob) % Flags = IBSET (Obs % SO2_DNSTY(iob) % Flags, SiteQualityFlag)
          CASE (PM10_CSNT)
            Obs % SITE_QAL(iOb) = pm10
            IF (pm10 >= siterej) Obs % PM10_DNSTY(iob) % Flags = IBSET (Obs % PM10_DNSTY(iob) % Flags, SiteQualityFlag)
          CASE (PM2p5_CSNT)
            Obs % SITE_QAL(iOb) = pm2p5
            IF (pm2p5 >= siterej) Obs % PM2p5_DNSTY(iob) % Flags = IBSET (Obs % PM2p5_DNSTY(iob) % Flags, SiteQualityFlag)
          CASE (O3_CSNT)
            Obs % SITE_QAL(iOb) = o3
            IF (o3 >= siterej) Obs % O3_DNSTY(iob) % Flags = IBSET (Obs % O3_DNSTY(iob) % Flags, SiteQualityFlag)
          CASE (NO2_CSNT)
            Obs % SITE_QAL(iOb) = no2
            IF (no2 >= siterej) Obs % NO2_DNSTY(iob) % Flags = IBSET (Obs % NO2_DNSTY(iob) % Flags, SiteQualityFlag)
          CASE (CO_CSNT)
            Obs % SITE_QAL(iOb) = co
            IF (co >= siterej) Obs % CO_DNSTY(iob) % Flags = IBSET (Obs % CO_DNSTY(iob) % Flags, SiteQualityFlag)
        END SELECT
      END IF
    END DO
  END DO

  CALL Ops_CloseFile (SiteQalUnit)

ELSE
  CALL gen_fail (RoutineName,                          &
                 'siterej must be a value of 0 - 10.')

END IF

!-------------------------------------------------------------------------------
! 2. Error reporting, tracing, and end of routine
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ReadSiteQuality
