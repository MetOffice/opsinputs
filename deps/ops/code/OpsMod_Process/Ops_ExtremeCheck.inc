!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Checks for impossible values in reports.
!
! Method:
!   Observed values are checked to see whether they are ridiculous.
!   If so, the permanent reject flag is set.
!
! Inputs:
!   Obs - Structure containing data about observations
!
! Outputs:
!   Obs - With permanent reject flags set appropriately
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ExtremeCheck (Obs)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  RMDI

USE OpsMod_AirQuality, ONLY: &
  SO2_CSNT,                  &
  PM10_CSNT,                 &
  PM2p5_CSNT,                &
  O3_CSNT,                   &
  NO2_CSNT,                  &
  CO_CSNT

USE OpsMod_ObsInfo, ONLY: &
  OB_type,                &
  FinalRejectFlag,        &
  ExtremeValueFlag

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_type), INTENT(INOUT) :: Obs

! Local declarations:
REAL                          :: ObsVal
INTEGER                       :: i           ! Loop variable over observations
INTEGER                       :: klev        ! Loop variable over observations
INTEGER                       :: NumObsLocal ! Total number of observations
INTEGER                       :: iflags
CHARACTER(len=*),PARAMETER    :: RoutineName = 'Ops_ExtremeCheck'

IF (UseTrace) CALL gen_trace_entry (RoutineName)

NumObsLocal = Obs % header % NumObsLocal

iflags = IOR (IBSET (0, FinalRejectFlag), IBSET (0, ExtremeValueFlag))

DO i = 1, NumObsLocal

  !-------------------------------------
  ! 1. Check for pressure less than zero
  !-------------------------------------

  IF (Obs % header % Pmsl % Present) THEN
    IF (Obs % Pmsl(i) % Value /= RMDI) THEN
      IF (Obs % Pmsl(i) % Value <= 0.0) Obs % Pmsl(i) % Flags = IOR (Obs % Pmsl(i) % Flags, iflags)
    END IF
  END IF

  IF (Obs % header % Pstandard % Present) THEN
    IF (Obs % Pstandard(i) % Value /= RMDI) THEN
      IF (Obs % Pstandard(i) % Value <= 0.0) Obs % Pstandard(i) % Flags = IOR (Obs % Pstandard(i) % Flags, iflags)
    END IF
  END IF

  IF (Obs % header % Pstation % Present) THEN
    IF (Obs % Pstation(i) % Value /= RMDI) THEN
      IF (Obs % Pstation(i) % Value <= 0.0) Obs % Pstation(i) % Flags = IOR (Obs % Pstation(i) % Flags, iflags)
    END IF
  END IF

  !-------------------------------------------------
  ! 2. Check for temperature less than absolute zero
  !-------------------------------------------------

  IF (Obs % header % t2 % Present) THEN
    IF (Obs % t2(i) % Value /= RMDI) THEN
      IF (Obs % t2(i) % Value <= 0.0) Obs % t2(i) % Flags = IOR (Obs % t2(i) % Flags, iflags)
    END IF
  END IF

  IF (Obs % header % SST % Present) THEN
    IF (Obs % SST(i) % Value /= RMDI) THEN
      IF (Obs % SST(i) % Value <= 0.0) Obs % SST(i) % Flags = IOR (Obs % SST(i) % Flags, iflags)
    END IF
  END IF

  IF (Obs % header % t % Present) THEN
    DO klev = 1,Obs % header % t % NumLev
      IF (Obs % t(i,klev) % Value /= RMDI) THEN
        IF (Obs % t(i,klev) % Value <= 0.0) Obs % t(i,klev) % Flags = IOR (Obs % t(i,klev) % Flags, iflags)
      END IF
    END DO
  END IF

  !-----------------------------------------------------------------------------
  ! 3. Check for wind speed greater than zero & direction outside permitted
  !    range
  !-----------------------------------------------------------------------------

  IF (Obs % header % ddd10 % Present .AND. &
      Obs % header % fff10 % Present) THEN
    IF (Obs % fff10(i) % Value /= RMDI) THEN
      IF (Obs % fff10(i) % Value < 0.0) THEN
        Obs % fff10(i) % Flags = IOR (Obs % fff10(i) % Flags, iflags)
        Obs % ddd10(i) % Flags = IOR (Obs % ddd10(i) % Flags, iflags)
      END IF
    END IF
    IF (Obs % ddd10(i) % Value /= RMDI) THEN
      IF (Obs % ddd10(i) % Value < 0.0 .OR. &
          Obs % ddd10(i) % Value > 360.0) THEN
        Obs % ddd10(i) % Flags = IOR (Obs % ddd10(i) % Flags, iflags)
        Obs % fff10(i) % Flags = IOR (Obs % fff10(i) % Flags, iflags)
      END IF
    END IF
  END IF

  IF (Obs % header % WindSpeed % Present) THEN
    IF (Obs % WindSpeed(i) % Value /= RMDI) THEN
      IF (Obs % WindSpeed(i) % Value < 0.0) Obs % WindSpeed(i) % Flags = IOR (Obs % WindSpeed(i) % Flags, iflags)
    END IF
  END IF

  IF (Obs % header % ddd % Present .AND. &
      Obs % header % fff % Present) THEN
    DO klev = 1, Obs % header % ddd % NumLev
      IF (Obs % fff(i,klev) % Value /= RMDI) THEN
        IF (Obs % fff(i,klev) % Value < 0.0) THEN
          Obs % fff(i,klev) % Flags = IOR (Obs % fff(i,klev) % Flags, iflags)
          Obs % ddd(i,klev) % Flags = IOR (Obs % ddd(i,klev) % Flags, iflags)
        END IF
      END IF
      IF (Obs % ddd(i,klev) % Value /= RMDI) THEN
        IF (Obs % ddd(i,klev) % Value < 0.0 .OR. &
            Obs % ddd(i,klev) % Value > 360.0) THEN
          Obs % ddd(i,klev) % Flags = IOR (Obs % ddd(i,klev) % Flags, iflags)
          Obs % fff(i,klev) % Flags = IOR (Obs % fff(i,klev) % Flags, iflags)
        END IF
      END IF
    END DO

  END IF

  !---------------------------------------------------------
  ! 4. Check for dew point less than or equal to temperature
  !---------------------------------------------------------

  IF (Obs % header % t2 % Present .AND. &
      Obs % header % td2 % Present) THEN
    IF (Obs % td2(i) % Value /= RMDI .AND. &
        Obs % t2(i) % Value /= RMDI) THEN
      IF (Obs % td2(i) % Value > Obs % t2(i) % Value) Obs % td2(i) % Flags =  IOR (Obs % td2(i) % Flags, iflags)
    END IF
  END IF

  IF (Obs % header % td % Present .AND. &
      Obs % header % t % Present) THEN
    DO klev = 1, Obs % header % td % NumLev
      IF (Obs % td(i,klev) % Value /= RMDI .AND. &
          Obs % t(i,klev) % Value /= RMDI) THEN
        IF (Obs % td(i,klev) % Value > Obs % t(i,klev) % Value) THEN
          Obs % td(i,klev) % Flags = IOR (Obs % td(i,klev) % Flags, iflags)
        END IF
      END IF
    END DO

  END IF

 !----------------------------------------------------------------------------------
 ! 6. Check AIRQALEU species densities (Value references in #474 converted to kg/m3)
 !----------------------------------------------------------------------------------

  IF (Obs % header % CSNT_TYP % Present) THEN
    ObsVal = Obs % MASS_DNSTY(i) % Value * 10.0 ** Obs % DCML_SCL_MASS_DNSTY(i)
    SELECT CASE (Obs % CSNT_TYP(i))
      CASE (SO2_CSNT)
        IF (ObsVal > 1.e-5 .OR. ObsVal < 0.0) THEN
          Obs % MASS_DNSTY(i) % Flags = IOR (Obs % MASS_DNSTY(i) % Flags, iflags)
        END IF
      CASE (PM10_CSNT)
        IF (ObsVal > 1e-4 .OR. ObsVal < 0.0) THEN
          Obs % MASS_DNSTY(i) % Flags = IOR (Obs % MASS_DNSTY(i) % Flags, iflags)
        END IF
      CASE (PM2p5_CSNT)
        IF (ObsVal > 2.5e-6 .OR. ObsVal < 0.0) THEN
          Obs % MASS_DNSTY(i) % Flags = IOR (Obs % MASS_DNSTY(i) % Flags, iflags)
        END IF
      CASE (O3_CSNT)
        IF (ObsVal > 2.e-6 .OR. ObsVal < 0.0) THEN
          Obs % MASS_DNSTY(i) % Flags = IOR (Obs % MASS_DNSTY(i) % Flags, iflags)
        END IF
      CASE (NO2_CSNT)
        IF (ObsVal > 1.e-6 .OR. ObsVal < 0.0) THEN
          Obs % MASS_DNSTY(i) % Flags = IOR (Obs % MASS_DNSTY(i) % Flags, iflags)
        END IF
      CASE (CO_CSNT)
        IF (ObsVal > 1.e-4 .OR. ObsVal < 0.0) THEN
          Obs % MASS_DNSTY(i) % Flags = IOR (Obs % MASS_DNSTY(i) % Flags, iflags)
        END IF
    END SELECT
  END IF
END DO

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END  SUBROUTINE Ops_ExtremeCheck
