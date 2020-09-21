!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Calculates theta from temperature.
!
! Inputs: Obs - Observational data with temperature
!
! Outputs: Obs  -  Observational data with theta calculated.
!
! Simple calculation of theta using :
!
!                        k
!       THETA = (1000/P*)  * TEMPERATURE  (where k=R/Cp)
!
! P* is used in the calculation because t2 has been adjusted to Z*
!-------------------------------------------------------------------------------

SUBROUTINE Ops_TtoTheta (Obs)

USE GenMod_Core, ONLY: &
    gen_fail,          &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    Pref,                                &
    RMDI,                                &
    Kappa

USE OpsMod_MiscTypes, ONLY: &
    ZcodeLayersA,           &
    ZcodeLayersB,           &
    ZcodeLevelsA,           &
    ZcodeLevelsB

USE OpsMod_ObsInfo, ONLY: &
    ExtremeValueFlag,     &
    OB_type,              &
    Ops_Alloc

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_type),INTENT(INOUT) :: Obs  ! Contains data on obs

! Local declarations:
CHARACTER(len=*), PARAMETER  :: RoutineName = 'Ops_TtoTheta'
INTEGER                      :: NumObsLocal     ! Number of observations on this PE
INTEGER                      :: NumLev          ! Number of levels
INTEGER                      :: Iobs
INTEGER                      :: Lev             ! Loop variables
REAL                         :: Press           ! Pressure at observation level
REAL                         :: Temp            ! Temperature at observation level
REAL                         :: TObErr          ! Ob. error at observation level

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!------------------------
! 1.   Set Initial values
!------------------------

NumObsLocal = Obs % header % NumObsLocal

!-----------------------------------------
! 3. Calculate upper air theta if possible
!-----------------------------------------

IF (Obs % header % t % Present) THEN

  NumLev = Obs % header % t % NumLev
  CALL Ops_Alloc (Obs % Header % theta, &
                  "Obs % theta",        &
                  NumObsLocal,          &
                  Obs % theta,          &
                  num_levels = NumLev)

  IF ((Obs % header % t % Zcode == ZcodeLayersA .OR. &
       Obs % header % t % Zcode == ZcodeLayersB .OR. &
       Obs % header % t % Zcode == ZcodeLevelsB) .AND. &
      .NOT. Obs % header % PlevelsB % Present) THEN
    CALL gen_fail (RoutineName,        &
                   'PlevelsB missing')
  END IF
  IF (Obs % header % t % Zcode == ZcodeLevelsA .AND. &
      .NOT. Obs % header % PlevelsA % Present) THEN
    CALL gen_fail (RoutineName,        &
                   'PlevelsA missing')
  END IF

  DO Iobs = 1, NumObsLocal
    DO Lev = 1, NumLev
      Press = RMDI
      ! layer value between levelsB (OD)
      ! option for OD Sondes not actually used since the Sonde code sets to
      ! ZcodeLevelsA and overwrites PLevelsA with SQRT(PB(L)*PB(L+1))
      IF (Obs % header % t % Zcode == ZcodeLayersB) THEN
        IF (Obs % PlevelsB(Iobs,Lev) % Value /= RMDI .AND.  &
            Obs % PlevelsB(Iobs,Lev + 1) % Value /= RMDI) THEN
          Press = SQRT (Obs % PlevelsB(Iobs,Lev) % Value * &
                        Obs % PlevelsB(Iobs,Lev + 1) % Value)
        END IF
      ELSE IF (Obs % header % t % Zcode == ZcodeLevelsA) THEN
         Press = Obs % PlevelsA(Iobs,Lev) % Value
      ELSE IF (Obs % header % t % Zcode == ZcodeLevelsB .OR. &
               Obs % header % t % Zcode == ZcodeLayersA) THEN
         Press = Obs % PlevelsB(Iobs,Lev) % Value
      END IF

      Temp = Obs % t(Iobs,Lev) % Value
      TObErr = Obs % t(Iobs,Lev) % ObErr
      IF (Press /= RMDI .AND. &
          Press > 0.0 .AND.   &
          Temp /= RMDI .AND.  &
          (.NOT. BTEST (Obs % t(Iobs,Lev) % Flags,ExtremeValueFlag))) THEN

        Obs % theta(Iobs,Lev) % Value = ((Pref / Press) ** Kappa) * Temp
        Obs % theta(Iobs,Lev) % ObErr = ((Pref / Press) ** Kappa) * TObErr

      ELSE
        Obs % theta(Iobs,Lev) % Value = RMDI
        Obs % theta(Iobs,Lev) % ObErr = RMDI
      END IF
      Obs % theta(Iobs,Lev) % PGEFinal = Obs % t(Iobs,Lev) % PGEFinal
      Obs % theta(Iobs,Lev) % Flags = Obs % t(Iobs,Lev) % Flags
    END DO
  END DO

  !---------------------------
  ! 3.2 Set header information
  !---------------------------

  Obs % header % theta % Present = .TRUE.
  Obs % header % theta % Zcode = Obs % header % t % Zcode
  Obs % header % theta % NumLev = NumLev

END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END  SUBROUTINE Ops_TtoTheta
