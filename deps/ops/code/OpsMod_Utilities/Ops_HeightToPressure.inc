!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Converts heights to pressures using the ICAO atmosphere.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_HeightToPressure (Heights,   &
                                 Npoints,   &
                                 Pressures)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    g,                                   &
    RMDI,                                &
    R

USE OpsMod_Constants, ONLY: &
    Lapse_RateL,            &
    Lapse_RateU,            &
    Press_Bot,              &
    Press_Mid,              &
    Press_Top,              &
    Temp_Bot,               &
    Temp_Top,               &
    Gpm1,                   &
    Gpm2

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)        :: Npoints           ! Number of heights to be converted
REAL, INTENT(IN)           :: Heights(Npoints)  ! Heights (gpm)to be converted
REAL, INTENT(OUT)          :: Pressures(Npoints)! Calculated pressures (Pascals)

! Local declarations:
CHARACTER(len=*),PARAMETER :: RoutineName = 'Ops_HeightToPressure'
INTEGER                    :: I               ! Loop variable
REAL                       :: RepT_Bot        ! Reciprical of bottom temperature
REAL                       :: RepT_Top        ! Reciprical of top temperature
REAL                       :: G_over_R        ! g/R
REAL                       :: ZP1             ! Exponent used for calculation of pressure in bottom layer
REAL                       :: ZP2             ! Exponent used for calculation of pressure in top layer
REAL                       :: GPM             ! Height in geopotential metres
REAL                       :: Pressure        ! Calculated pressure

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!------------------------
! 1.   Set Initial values
!------------------------

RepT_Bot = 1.0 / Temp_Bot
RepT_Top = 1.0 / Temp_Top
G_over_R = g / R
ZP1 = G_over_R / Lapse_RateL
ZP2 = G_over_R / Lapse_RateU

DO I = 1,Npoints

  GPM = Heights(I)

  IF (GPM == RMDI) THEN
    Pressure = RMDI
  ELSE IF (GPM < -5000.0) THEN
    Pressure = RMDI
  ELSE IF (GPM < Gpm1) THEN
    ! Heights up to 11,000 GPM
    Pressure = Lapse_RateL * GPM * RepT_Bot
    Pressure = (1.0 - Pressure) ** ZP1
    Pressure = 100.0 * Pressure * Press_Bot
  ELSE IF (GPM < Gpm2) THEN
    ! Heights between 11,000 and 20,000 GPM
    Pressure = G_over_R * (GPM - Gpm1) * RepT_Top
    Pressure = LOG (Press_Mid) - Pressure
    Pressure = 100.0 * EXP (Pressure)
  ELSE
    ! Heights above 20,000 GPM
    Pressure = Lapse_RateU * RepT_Top * (GPM - Gpm2)
    Pressure = 100.0 * Press_Top * ((1.0 - Pressure)) ** ZP2
  END IF

  Pressures(I) = Pressure

END DO

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END  SUBROUTINE Ops_HeightToPressure
