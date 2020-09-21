!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Calculates relative humidity (RH_ice) from dew point temperature,
! or converts RH_water to RH_ice.
!
! Inputs: Obs - Observational data with dew point or RH_water
!
! Outputs: Obs  -  Observational data with relative humidity calculated.
!
! Method: -
!
! Saturated specific humidity at the dew point (ie w.r.t water), and saturated
! specific humidity (w.r.t ice below 0C, using routine Ops_Qsat for) at the air
! temperature are calculated.  The dew point conversion uses routine Ops_QsatDew
! (the Sonntag 1994 formula) it used to use Qsat_Wat (older Goff-Gratch formula).
! Relative humidity is then calculated using :
!
!       RH = (QSAT(DEW POINT)/QSAT(DRY BULB))*100
!
! For some temperatures (e.g. when dew point = temperature), supersaturation
! w.r.t ice may occur. CapSupersat controls whether upper air relative humidity
! is capped at 100%.  If the pressure, dew point or temperature take extreme
! values or are missing, the relative humidity is set to missing data.
!
!-------------------------------------------------------------------------------

SUBROUTINE Ops_TdToRH (Obs)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI,                                &
    ZERODEGC

USE OpsMod_MiscTypes, ONLY: &
    ZcodeSurface,           &
    ZcodeLevelsA

USE OpsMod_ObsInfo, ONLY: &
    OB_type,              &
    RHreportFlag,         &
    ExtremeValueFlag,     &
    ops_alloc,            &
    ops_dealloc

USE OpsMod_ObsTypes, ONLY: &
    ObsTypeBogus

USE OpsMod_Process, ONLY: &
    CapSupersat

USE OpsMod_Utilities, ONLY: &
    Ops_Qsat,               &
    Ops_QsatDew

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_type), INTENT(INOUT) :: Obs  ! Contains data on obs

! Local declarations:
CHARACTER(len=*),PARAMETER    :: RoutineName = 'Ops_TdToRH'
INTEGER                       :: NumObsLocal     ! Number of observations on this PE
INTEGER                       :: NumLev          ! Number of levels
INTEGER                       :: Iobs
INTEGER                       :: Lev             ! Loop variables
LOGICAL                       :: RHpreset        ! RH already set up
REAL                          :: Press(1)        ! Pressure at observation level
REAL                          :: Tempd(1)        ! Dry bulb temperature at observation level
REAL                          :: DewPoint(1)     ! Wet bulb temperature at observation level
REAL                          :: Smrd(1)         ! Saturated specific humidity wrt ice at dry bulb temperature
REAL                          :: Smrw(1)         ! Saturated specific humidity wrt water at wet bulb (dew point) temperature
REAL                          :: RH              ! Relative humidity
REAL                          :: RHCorr          ! Relative humidity correction
LOGICAL                       :: td_extreme_value_flag

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!------------------------
! 1.   Set Initial values
!------------------------

NumObsLocal = Obs % header % NumObsLocal

!------------------------------------
! 2. Calculate surface RH if possible
!------------------------------------

IF (Obs % header % td2 % Present .AND. &
    Obs % header % t2 % Present) THEN

  !--------------------------------
  ! 2.1 Calculate relative humidity
  !--------------------------------

  IF (.NOT. Obs % header % rh2 % Present) THEN
    CALL ops_alloc (Obs % Header % RH2, &
                    "Obs % RH2",        &
                    NumObsLocal,        &
                    Obs % RH2)
    Obs % rh2(:) % OBErr = Obs % td2(:) % OBErr
    Obs % rh2(:) % PGEFinal = Obs % td2(:) % PGEFinal
    Obs % rh2(:) % Flags = Obs % td2(:) % Flags
  END IF

  DO Iobs = 1,NumObsLocal

    IF (Obs % header % Pstar % Present) THEN
      Press(1) = Obs % Pstar(Iobs) % Value
    ELSE
      Press(1) = RMDI
    END IF
    IF (Press(1) <= 0.0) Press(1) = 100000.0

    ! There is very little sensitivity of calculated RH to P
    ! (less than 0.1% to change from 1000 to 800 hPa)

    Tempd(1) = Obs % t2(Iobs) % Value
    DewPoint(1) = Obs % td2(Iobs) % Value

    !  Dew point reported (most stations)
    IF (Press(1) /= RMDI .AND.                                       &
        Tempd(1) /= RMDI .AND.                                       &
        DewPoint(1) > 1.0 .AND.                                      &
        Press(1) > 0.0 .AND.                                         &
        .NOT. BTEST (Obs % t2(Iobs) % Flags, ExtremeValueFlag) .AND. &
        .NOT. BTEST (Obs % td2(Iobs) % Flags, ExtremeValueFlag)) THEN
      CALL Ops_Qsat (Smrd,  & ! OUT - saturated specific humidity w.r.t ice
                     Tempd, & ! IN - temperature in Kelvins
                     Press, & ! IN - pressure in Pascals
                     1)       ! IN - number values calculated
      CALL Ops_QsatDew (Smrw(1),     &  ! OUT - saturated q wrt water
                        DewPoint(1), &  ! IN - dew point temperature in K
                        Press(1))       ! IN - pressure in Pascals
      IF (Smrw(1) > 0 .AND. Smrd(1) > 0) THEN
        RH = (Smrw(1) / Smrd(1)) * 100.0
      ELSE
        RH = RMDI
      END IF
      IF (RH < 0 .AND. RH /= RMDI) RH = 0
      Obs % rh2(Iobs) % Value = RH

    !  RH reported (small minority of stations)
    ELSE IF (Obs % rh2(Iobs) % Value /= RMDI) THEN
      Obs % rh2(Iobs) % Flags = IBSET (Obs % rh2(Iobs) % Flags, RHreportFlag)

      !  Convert from RH_water to RH_ice for temperatures below freezing
      IF (Press(1) > 0.0 .AND. &
          Tempd(1) > 1.0 .AND. Tempd(1) < ZeroDegC) THEN
        CALL Ops_Qsat (Smrd,   & ! OUT - saturated specific humidity w.r.t ice
                       Tempd,  & ! IN - air temperature in Kelvin
                       Press,  & ! IN - pressure in Pascals
                       1)        ! IN - number values calculated
        CALL Ops_QsatDew (Smrw(1),  &  ! OUT - saturated q wrt water
                          Tempd(1), &  ! IN - dew point temperature in K
                          Press(1))     ! IN - pressure in Pascals
        RH = Obs % rh2(Iobs) % Value * Smrw(1) / Smrd(1)
        RHCorr = RH - Obs % rh2(Iobs) % Value
        ! Have to store as correction to make sure MetDB has original value
        IF (Obs % RH2(Iobs) % CorValue /= RMDI) THEN
          Obs % RH2(Iobs) % CorValue = Obs % RH2(Iobs) % CorValue + RHCorr
        ELSE
          Obs % RH2(Iobs) % CorValue = RHCorr
        END IF
        Obs % rh2(Iobs) % Value = RH
      ELSE IF (Tempd(1) == RMDI) THEN
        Obs % rh2(Iobs) % Value = RMDI
      END IF
    ELSE
      Obs % rh2(Iobs) % Value = RMDI
    END IF
  END DO

  !---------------------------
  ! 2.2 Set header information
  !---------------------------

  Obs % header % rh2 % Present = .TRUE.
  Obs % header % rh2 % Zcode = ZcodeSurface
  Obs % header % rh2 % NumLev = 1

  !-----------------------------------
  ! 2.3 Deallocate space for dew point
  !-----------------------------------

  CALL ops_dealloc (Obs % Header % td2, &
                    "Obs % td2",        &
                    Obs % td2)
END IF

!--------------------------------------
! 3. Calculate upper air RH if possible
!--------------------------------------

IF ((Obs % header % td % Present .OR. Obs % header % rh % Present) .AND. &
    Obs % header % t % Present) THEN

  !--------------------------------
  ! 3.1 Calculate relative humidity
  !--------------------------------

  NumLev = Obs % header % t % NumLev
  RHpreset = Obs % header % rh % Present
  IF (.NOT. Obs % header % rh % Present) THEN
    CALL ops_alloc (Obs % Header % RH,        &
                    "Obs % RH",               &
                    NumObsLocal,              &
                    Obs % RH,                 &
                    HdrIn = Obs % Header % T)
    Obs % header % rh % Zcode = ZcodeLevelsA
  END IF

  DO Iobs = 1, NumObsLocal
    DO Lev = 1, NumLev
      IF (Obs % header % PlevelsA % Present) THEN
        Press(1) = Obs % PlevelsA(Iobs,Lev) % Value
      ELSE
        Press(1) = RMDI
      END IF
      Tempd(1) = Obs % t(Iobs,Lev) % Value
      IF (Obs % header % td % Present) THEN
        DewPoint(1) = Obs % td(Iobs,Lev) % Value
        td_extreme_value_flag = BTEST (Obs % td(Iobs,Lev) % Flags, ExtremeValueFlag)
      ELSE
        DewPoint(1) = RMDI
        td_extreme_value_flag = .FALSE.
      END IF
      IF (Press(1) /= RMDI .AND.                                          &
          Tempd(1) /= RMDI .AND.                                          &
          DewPoint(1) > 1.0 .AND.                                         &
          Press(1) > 0.0 .AND.                                            &
          .NOT. BTEST (Obs % t(Iobs,Lev) % Flags, ExtremeValueFlag) .AND. &
          .NOT. td_extreme_value_flag) THEN
        CALL Ops_Qsat (Smrd,  & ! OUT - saturated specific humidity w.r.t ice
                       Tempd, & ! IN - temperature in Kelvins
                       Press, & ! IN - pressure in Pascals
                       1)       ! IN - number values calculated
        CALL Ops_QsatDew (Smrw(1),     &  ! OUT - saturated q wrt water
                          DewPoint(1), &  ! IN - dew point temperature in K
                          Press(1))       ! IN - pressure in Pascals

        IF (Smrw(1) > 0 .AND. Smrd(1) > 0) THEN
          RH = (Smrw(1) / Smrd(1)) * 100.0
        ELSE
          RH = RMDI
        END IF
        IF (RH < 0 .AND. RH /= RMDI) RH = 0
        IF (CapSupersat .AND. RH > 100.0 .AND. RH /= RMDI) RH = 100.0
        Obs % rh(Iobs,Lev) % Value = RH
        IF (.NOT. RHpreset) THEN
          Obs % rh(Iobs,Lev) % OBErr = Obs % td(Iobs,Lev) % OBErr
          Obs % rh(Iobs,Lev) % PGEFinal = Obs % td(Iobs,Lev) % PGEFinal
          Obs % rh(Iobs,Lev) % Flags = Obs % td(Iobs,Lev) % Flags
        END IF

      ELSE IF (Obs % rh(Iobs,Lev) % Value /= RMDI .AND. &
               Obs % ObsType(Iobs) /= ObsTypeBogus) THEN

        !  Convert from RH_water to RH_ice for temperatures below freezing
        IF (Press(1) > 0.0 .AND. &
            Tempd(1) > 1.0 .AND. Tempd(1) < ZeroDegC) THEN
          CALL Ops_Qsat (Smrd,  & ! OUT - saturated specific humidity w.r.t ice
                         Tempd, & ! IN - air temperature in Kelvin
                         Press, & ! IN - pressure in Pascals
                         1)       ! IN - number values calculated
          CALL Ops_QsatDew (Smrw(1),  &  ! OUT - saturated q wrt water
                            Tempd(1), &  ! IN - dew point temperature in K
                            Press(1))     ! IN - pressure in Pascals
          RH = Obs % rh(Iobs,Lev) % Value * Smrw(1) / Smrd(1)
          RHCorr = RH - Obs % rh(Iobs,Lev) % Value
          ! Have to store as correction to make sure MetDB has original value
          IF (Obs % rh(Iobs,Lev) % CorValue /= RMDI) THEN
            Obs % rh(Iobs,Lev) % CorValue = Obs % rh(Iobs,Lev) % CorValue + RHCorr
          ELSE
            Obs % rh(Iobs,Lev) % CorValue = RHCorr
          END IF
          Obs % rh(Iobs,Lev) % Value = RH
        ELSE IF (Tempd(1) == RMDI) THEN
          Obs % rh(Iobs,Lev) % Value = RMDI
        END IF
      END IF
    END DO
  END DO

END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END  SUBROUTINE Ops_TdToRH
