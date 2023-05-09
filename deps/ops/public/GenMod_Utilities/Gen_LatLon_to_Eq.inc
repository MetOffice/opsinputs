!-----------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-----------------------------------------------------------------------
! Calculates latitude and longitude on equatorial latitude-longitude
! (eq) grid used in regional models from input arrays of latitude and
! longitude on standard grid. Both input and output latitudes and
! longitudes are in degrees.
!-----------------------------------------------------------------------

SUBROUTINE Gen_LatLon_to_Eq (phi,         &
                             lambda,      &
                             phi_eq,      &
                             lambda_eq,   &
                             phi_pole,    &
                             lambda_pole)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  Pi_Over_180,                           &
  Recip_Pi_Over_180

IMPLICIT NONE

! Subroutine arguments:
REAL, INTENT(IN)            :: phi(:)       ! Latitude
REAL, INTENT(IN)            :: lambda(:)    ! Longitude
REAL, INTENT(OUT)           :: lambda_eq(:) ! Longitude in equatorial lat-lon coords
REAL, INTENT(OUT)           :: phi_eq(:)    ! Latitude in equatorial lat-lon coords
REAL, INTENT(IN)            :: phi_pole     ! Latitude of equatorial lat-lon pole
REAL, INTENT(IN)            :: lambda_pole  ! Longitude of equatorial lat-lon pole

! Local declarations:
REAL                        :: a_lambda
REAL                        :: a_phi
REAL                        :: e_lambda
REAL                        :: arg
REAL                        :: e_phi
REAL                        :: sin_phi_pole
REAL                        :: cos_phi_pole
REAL                        :: term1
REAL                        :: term2
REAL                        :: lambda_zero
REAL                        :: lambda_pole_local
INTEGER                     :: i
REAL, PARAMETER             :: small = 1.0E-6
CHARACTER(len=*), PARAMETER :: routine_name = "Gen_LatLon_to_Eq"

IF (UseTrace) CALL gen_trace_entry (routine_name)

! 1. Initialise local constants

! Scale lambda pole to range -180 to 180 degs

lambda_pole_local = lambda_pole
IF (lambda_pole_local > 180.0) lambda_pole_local = lambda_pole_local - 360.0

! Latitude of zeroth meridian

lambda_zero = lambda_pole_local + 180.0

! Sine and cosine of latitude of eq pole

IF (phi_pole >= 0.0) THEN
  sin_phi_pole = SIN (PI_OVER_180 * phi_pole)
  cos_phi_pole = COS (PI_OVER_180 * phi_pole)
ELSE
  sin_phi_pole = -SIN (PI_OVER_180 * phi_pole)
  cos_phi_pole = -COS (PI_OVER_180 * phi_pole)
END IF

! 2. Transform from standard to equatorial latitude-longitude

DO i = 1, SIZE (phi)

  ! Scale longitude to range -180 to +180 degs

  a_lambda = lambda(i) - lambda_zero
  IF (a_lambda > 180.0) a_lambda = a_lambda - 360.0
  IF (a_lambda <= -180.0) a_lambda = a_lambda + 360.0

  ! Convert latitude & longitude to radians

  a_lambda = PI_OVER_180 * a_lambda
  a_phi = PI_OVER_180 * phi(i)

  ! Compute eq latitude using equation (4.4)

  arg = -cos_phi_pole * COS (a_lambda) * COS (a_phi) + SIN (a_phi)*  sin_phi_pole
  arg = MIN (arg, 1.0)
  arg = MAX (arg,-1.0)
  E_phi = ASIN (arg)
  phi_eq(i) = RECIP_PI_OVER_180 * E_phi

  ! Compute eq longitude using equation (4.6)

  term1 = COS (a_phi) * COS (a_lambda) * sin_phi_pole + SIN (a_phi) * cos_phi_pole
  term2 = COS (E_phi)
  IF (term2 < small) THEN
    e_lambda = 0.0
  ELSE
    arg = term1 / term2
    arg = MIN (arg, 1.0)
    arg = MAX (arg, -1.0)
    e_lambda = RECIP_PI_OVER_180 * ACOS (arg)
    e_lambda = SIGN (e_lambda, a_lambda)
  END IF

  ! Scale longitude to range 0 to 360 degs

  IF (e_lambda >= 360.0) e_lambda = e_lambda - 360.0
  IF (e_lambda < 0.0) e_lambda = e_lambda + 360.0
  lambda_eq(i) = e_lambda

END DO

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE Gen_LatLon_to_Eq
