!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Calculates relative humidity from mixing ratio.
!
! Inputs: Obs - Observational data with mixing ratio.
!
! Outputs: Obs  -  Observational data with relative humidity calculated.
!
! Saturated specific humidity (w.r.t ice) at the air temperature is obtained
! using the routine Ops_Qsat which computes it above water for temperatures greater
! than 0 and above ice below zero.  Relative humidity is then calculated using :
!
!       RH = (Mixing_Ratio/QSAT(DRY BULB))*100
!
!       Values less than 0% are set to 0%, values greater than 100% are set to
!       100%.
!
!       If the pressure, mixing ratio or temperature are missing, the relative
!       humidity is set to missing data.
!
!       The DerivedFromMixRatioFlag is set for all relative humidity values
!       derived in this routine.
!
!       If not already set up RH observation error is set to that of T for now,
!       will be further defined later.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_MixingRatioToRH (Obs)

USE GenMod_Core, ONLY: &
    gen_fail,          &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI

USE OpsMod_MiscTypes, ONLY: &
    ZcodeLayersA,           &
    ZcodeLayersB,           &
    ZcodeLevelsA,           &
    ZcodeLevelsB

USE OpsMod_ObsInfo, ONLY:    &
    DerivedFromMixRatioFlag, &
    ExtremeValueFlag,        &
    OB_type,                 &
    Ops_Alloc

USE OpsMod_Process, ONLY: &
    CapSupersat

USE OpsMod_Utilities, ONLY: &
    Ops_Qsat

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_type), INTENT(INOUT) :: Obs  ! Contains data on obs

! Local declarations:
CHARACTER(len=40)             :: ErrorMessage    ! ErrorMessage
CHARACTER(len=*), PARAMETER   :: RoutineName = 'Ops_MixingRatioToRH'
INTEGER                       :: NumObsLocal     ! Number of observations on this PE
INTEGER                       :: NumLev          ! Number of levels
INTEGER                       :: Iobs
INTEGER                       :: Lev             ! Loop variables
LOGICAL                       :: RHpreset        ! RH already set up
REAL                          :: Press(1)        ! Pressure at observation level
REAL                          :: Tempd(1)        ! Dry bulb temperature at observation level
REAL                          :: MixRat(1)       ! Wet bulb temperature at observation level
REAL                          :: Smrd(1)         ! Saturated mixing ratio w.r.t ice at dry
REAL                          :: RH              ! Relative humidity

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!------------------------
! 1.   Set Initial values
!------------------------

NumObsLocal = Obs % header % NumObsLocal

!--------------------------------------
! 3. Calculate upper air RH if possible
!--------------------------------------

IF (Obs % header % t % Present .AND. &
    Obs % header % MixingRatio % Present) THEN

  NumLev = Obs % header % MixingRatio % NumLev
  RHpreset = Obs % header % rh % Present
  IF (.NOT. Obs % header % rh % Present) THEN
    CALL Ops_Alloc (Obs % Header % rh,                &
                    "Obs % rh",                       &
                    NumObsLocal,                      &
                    Obs % rh,                         &
                    num_levels = NumLev,              &
                    zcode = Obs % header % t % Zcode)
  END IF

  IF ((Obs % header % t % Zcode == ZcodeLayersA .OR. &
       Obs % header % t % Zcode == ZcodeLayersB .OR. &
       Obs % header % t % Zcode == ZcodeLevelsB) .AND. &
       .NOT. Obs % header % PlevelsB % Present) THEN
    ErrorMessage = 'PlevelsB missing'
    CALL gen_fail (RoutineName,  &
                   ErrorMessage)
  END IF
  IF (Obs % header % t % Zcode == ZcodeLevelsA .AND. &
      .NOT. Obs % header % PlevelsA % Present) THEN
    ErrorMessage = 'PlevelsA missing'
    CALL gen_fail (RoutineName,  &
                   ErrorMessage)
  END IF

  DO Iobs = 1, NumObsLocal
    DO Lev = 1, NumLev

      ! layer value between levelsB (OD)
      ! option for OD Sondes not actually used since the Sonde code sets to
      ! ZcodeLevelsA and overwrites PLevelsA with SQRT(PB(L)*PB(L+1))
      IF (Obs % header % t % Zcode == ZcodeLayersB) THEN
        IF (Obs % PlevelsB(Iobs,Lev) % Value /= RMDI .AND. &
            Obs % PlevelsB(Iobs,Lev + 1) % Value /= RMDI) THEN
          Press(1) = SQRT (Obs % PlevelsB(Iobs,Lev) % Value  * &
                           Obs % PlevelsB(Iobs,Lev + 1) % Value)
        ELSE
          Press(1) = RMDI
        END IF
      ! spot value at levelA (alternate OD)
      ELSE IF (Obs % header % t % Zcode == ZcodeLevelsA) THEN
        Press(1) = Obs % PlevelsA(Iobs,Lev) % Value
      ! layer value between levelsA (Sonde - New Dynamics) OR spot value at
      ! levelB
      ELSE IF (Obs % header % t % Zcode == ZcodeLevelsB .OR. &
               Obs % header % t % Zcode == ZcodeLayersA) THEN
        Press(1) = Obs % PlevelsB(Iobs,Lev) % Value
      END IF

      Tempd(1) = Obs % t(Iobs,Lev) % Value
      MixRat(1) = Obs % MixingRatio(Iobs,Lev) % Value
      IF (Press(1) /= RMDI .AND.                                   &
          Tempd(1) /= RMDI .AND.                                   &
          MixRat(1) /= RMDI .AND.                                  &
          Press(1) > 0.0 .AND.                                     &
          .NOT. BTEST (Obs % t(Iobs,Lev) % Flags, ExtremeValueFlag)) THEN
        CALL Ops_Qsat (Smrd,  & ! OUT - saturated vapour pressure w.r.t water for Tempd > 0  w.r.t. ice for Tempd < 0
                       Tempd, & ! IN - temperature in Kelvins
                       Press, & ! IN - pressure in Pascals
                       1)       ! IN - number values calculated
        IF (MixRat(1) >= 0 .AND. Smrd(1) > 0) THEN
          RH = (MixRat(1) / Smrd(1)) * 100.0
        ELSE
          RH = RMDI
        END IF
        IF (RH < 0 .AND. RH /= RMDI) RH = 0
        IF (CapSupersat .AND. RH > 100.0 .AND. RH /= RMDI) RH = 100.0

        Obs % rh(Iobs,Lev) % Value = RH
        IF (.NOT. RHpreset) THEN
          Obs % rh(Iobs,Lev) % OBErr = Obs % t(Iobs,Lev) % OBErr
          Obs % rh(Iobs,Lev) % PGEFinal = Obs % t(Iobs,Lev) % PGEFinal
          Obs % rh(Iobs,Lev) % Flags = Obs % t(Iobs,Lev) % Flags
        END IF
        Obs % rh(Iobs,Lev) % Flags = IBSET (Obs % rh(Iobs,Lev) % Flags, DerivedFromMixRatioFlag)
      END IF
    END DO
  END DO

END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END  SUBROUTINE Ops_MixingRatioToRH
