!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Converts pressures to heights using the ICAO atmosphere.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_PressureToHeight (Pressures, &
                                 Npoints,   &
                                 Heights)

USE GenMod_Core,ONLY: &
    gen_trace_entry,  &
    gen_trace_exit,   &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    G,                                   &
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
REAL, INTENT(OUT)          :: Heights(Npoints)  ! Calculated heights (gpm)
REAL, INTENT(IN)           :: Pressures(Npoints)! Pressures (Pascals) to be converted

! Local declarations:
CHARACTER(len=*),PARAMETER :: RoutineName = 'Ops_PressureToHeight'
INTEGER                    :: I               ! Loop variable
REAL                       :: RepT_Bot        ! Reciprical of bottom temperature
REAL                       :: RepT_Top        ! Reciprical of top temperature
REAL                       :: G_over_R        ! g/R
REAL                       :: ZP1             ! Exponent used for calculation of pressure in bottom layer
REAL                       :: ZP2             ! Exponent used for calculation of pressure in top layer
REAL                       :: Press_Bot_hPa   ! Variable to allow for units of hPa in constants file
REAL                       :: Press_Mid_hPa   ! Variable to allow for units of hPa in constants file
REAL                       :: Press_Top_hPa   ! Variable to allow for units of hPa in constants file
REAL                       :: GPM             ! Calculated height in geopotential metres
REAL                       :: Pressure        ! Input pressure in hPa

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!------------------------
! 1.   Set Initial values
!------------------------

RepT_Bot = 1.0 / Temp_Bot
RepT_Top = 1.0 / Temp_Top
G_over_R = g / R
ZP1 = G_over_R / Lapse_RateL
ZP2 = G_over_R / Lapse_RateU
Press_Bot_hPa = Press_Bot * 100.0
Press_Mid_hPa = Press_Mid * 100.0
Press_Top_hPa = Press_Top * 100.0

DO I = 1, Npoints

  Pressure = Pressures(I)

  IF (Pressure == RMDI) THEN
    GPM = RMDI
  ELSE IF (Pressure <= 0.0) THEN
    GPM = RMDI
  ELSE IF (Pressure > Press_Mid_hPa) THEN
    Pressure = Pressure / Press_Bot_hPa
    Pressure = 1.0 - (Pressure ** (1.0 / ZP1))
    GPM = Pressure / (Lapse_RateL * RepT_Bot)
  ELSE IF (Pressure <= Press_Mid_hPa .AND. Pressure > Press_Top_hPa) THEN
    Pressure = LOG (Pressure)
    Pressure = LOG (Press_Mid_hPa) - Pressure
    GPM = (Pressure / (G_over_R * RepT_Top)) + Gpm1
  ELSE
    Pressure = 1.0 - ((Pressure / Press_Top_hPa) ** (1.0 / ZP2))
    GPM = (Pressure / (Lapse_RateU * RepT_Top)) + Gpm2
  END IF

  Heights(I) = GPM

END DO

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END  SUBROUTINE Ops_PressureToHeight
