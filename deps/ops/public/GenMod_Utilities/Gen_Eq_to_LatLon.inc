!-----------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-----------------------------------------------------------------------
! Calculates latitude and longitude on standard grid from input arrays
! of latitude and longitude on equatorial latitude-longitude (eq) grid
! used in regional models. Both input and output latitudes and
! longitudes are in degrees.
!
! The transformation formulae are described in unified model on-line
! documentation paper S1.
!-----------------------------------------------------------------------

SUBROUTINE Gen_Eq_to_LatLon (PHI_EQ,      &
                             LAMBDA_EQ,   &
                             PHI,         &
                             LAMBDA,      &
                             PHI_POLE,    &
                             LAMBDA_POLE, &
                             POINTS)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  Pi_Over_180,                           &
  Recip_Pi_Over_180

IMPLICIT NONE

! Subroutine arguments:
INTEGER                     :: POINTS            ! Number of points to be processed
REAL                        :: PHI(POINTS)       ! Latitude
REAL                        :: LAMBDA(POINTS)    ! Longitude (0 =< LON < 360)
REAL                        :: LAMBDA_EQ(POINTS) ! Longitude in equatorial lat-lon coords
REAL                        :: PHI_EQ(POINTS)    ! Latitude in equatorial lat-lon coords
REAL                        :: PHI_POLE          ! Latitude of equatorial lat-lon pole
REAL                        :: LAMBDA_POLE       ! Longitude of equatorial lat-lon pole

! Local declarations:
REAL                        :: E_LAMBDA
REAL                        :: E_PHI
REAL                        :: A_LAMBDA
REAL                        :: ARG
REAL                        :: A_PHI
REAL                        :: SIN_PHI_POLE
REAL                        :: COS_PHI_POLE
REAL                        :: TERM1
REAL                        :: TERM2
REAL, PARAMETER             :: SMALL = 1.0E-6
REAL                        :: LAMBDA_ZERO
INTEGER                     :: I
CHARACTER(len=*), PARAMETER :: RoutineName = "Gen_Eq_to_LatLon"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! 1. Initialise local constants

! Latitude of zeroth meridian

LAMBDA_ZERO = LAMBDA_POLE + 180.0

! Sine and cosine of latitude of eq pole

IF (PHI_POLE >= 0.0) THEN
  SIN_PHI_POLE = SIN (PI_OVER_180 * PHI_POLE)
  COS_PHI_POLE = COS (PI_OVER_180 * PHI_POLE)
ELSE
  SIN_PHI_POLE = -SIN (PI_OVER_180 * PHI_POLE)
  COS_PHI_POLE = -COS (PI_OVER_180 * PHI_POLE)
END IF

! 2. Transform from equatorial to standard latitude-longitude

DO I = 1, POINTS

  ! Scale eq longitude to range -180 to +180 degs

  E_LAMBDA = LAMBDA_EQ(I)
  IF (E_LAMBDA > 180.0) E_LAMBDA = E_LAMBDA - 360.0
  IF (E_LAMBDA < -180.0) E_LAMBDA = E_LAMBDA + 360.0

  ! Convert eq latitude & longitude to radians

  E_LAMBDA = PI_OVER_180 * E_LAMBDA
  E_PHI = PI_OVER_180 * PHI_EQ(I)

  ! Compute latitude using equation (4.7)

  ARG = COS_PHI_POLE * COS (E_LAMBDA) * COS (E_PHI) + SIN (E_PHI) * SIN_PHI_POLE
  ARG = MIN (ARG, 1.0)
  ARG = MAX (ARG, -1.0)
  A_PHI = ASIN (ARG)
  PHI(I) = RECIP_PI_OVER_180 * A_PHI

  ! Compute longitude using equation (4.8)

  TERM1 = COS (E_PHI) * COS (E_LAMBDA) * SIN_PHI_POLE - SIN (E_PHI) * COS_PHI_POLE
  TERM2 = COS (A_PHI)
  IF (TERM2 < SMALL) THEN
    A_LAMBDA = 0.0
  ELSE
    ARG = TERM1 / TERM2
    ARG = MIN (ARG, 1.0)
    ARG = MAX (ARG, -1.0)
    A_LAMBDA = RECIP_PI_OVER_180 * ACOS (ARG)
    A_LAMBDA = SIGN (A_LAMBDA, E_LAMBDA)
    A_LAMBDA = A_LAMBDA + LAMBDA_ZERO
  END IF

  ! Scale longitude to range 0 to 360 degs

  IF (A_LAMBDA >= 360.0) A_LAMBDA = A_LAMBDA - 360.0
  IF (A_LAMBDA < 0.0) A_LAMBDA = A_LAMBDA + 360.0
  LAMBDA(I) = A_LAMBDA

END DO

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Gen_Eq_to_LatLon
