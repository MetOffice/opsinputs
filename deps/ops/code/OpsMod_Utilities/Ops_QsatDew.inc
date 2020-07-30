!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Calculates saturated specific humidity (or vapour pressure) w.r.t water.
!
! Inputs: T (dew point), P
!
! Outputs: QS
!
! Method: -
!       Calculation using the Sonntag (1994) formula.
!       Only one value at a time (unlike QSAT).
!-------------------------------------------------------------------------------

SUBROUTINE Ops_QsatDew (QS, &
                        T,  &
                        P)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    Epsilon,                             &
    ZERODEGC

IMPLICIT NONE

! Subroutine arguments:
REAL, INTENT(OUT)           :: QS     ! Saturated specific humidity
                                      ! or saturated vapour pressure (if P<0)
REAL, INTENT(IN)            :: T      ! Temperature (dew point)
REAL, INTENT(IN)            :: P      ! Pressure

! Local declarations:
REAL                        :: FSUBW    ! Enhancement factor (see below)
REAL                        :: Psat     ! Saturation vapour pressure (Pa)
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_ QsatDew"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! Source: Eqn 7, Sonntag, D., Advancements in the field of hygrometry,
! Meteorol. Zeitschrift, N. F., 3, 51-66, 1994.
!  Most radiosonde manufacturers use Wexler, or Hyland and Wexler
!  or Sonntag formulations, which are all very similar
!  (Holger Vomel, pers. comm., 2011)

IF (T > 0.0) THEN
  Psat = EXP (-6096.9385 / T +       & ! Output Psat in Pa
              21.2409642 -           & ! see note below eq 7
              2.711193E-2 * T +      &
              1.673952E-5 * T ** 2 + &
              2.433502 * ALOG (T))
ELSE
  Psat = 0.0
END IF

! Psat above is the saturation vapour pressure of pure water vapour
! FSUBW (~ 1.005 at 1000 hPa) is the enhancement factor needed for moist air
! (eg eqns 20, 22 of Sonntag, but for consistency with QSAT the formula
! below is from eqn A4.6 of Adrian Gill's book)

FSUBW = 1.0 + 1.0E-8 * P * (4.5 + 6.0E-4 * (T - ZERODEGC) * (T - ZERODEGC))
Psat = Psat * FSUBW

IF (P <= 0.0) THEN

  QS = Psat

ELSE
  ! Now calculate QS, eqn 29 of Sonntag.  (With fix at low pressure)

  QS = (Epsilon * Psat) / (MAX (P,Psat) - (1.0 - Epsilon) * Psat)
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_QsatDew
