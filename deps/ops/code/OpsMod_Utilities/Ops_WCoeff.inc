!-----------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-----------------------------------------------------------------------
! Calculates coefficients used to translate u and v components of wind
! between equatorial (eq) latitude-longitude grid and standard
! latitude-longitude grid (or visa versa).  Input latitudes and
! longitudes are in degrees.
!----------------------------------------------------------------------

SUBROUTINE Ops_WCoeff (COEFF1,      &
                       COEFF2,      &
                       LAMBDA,      &
                       LAMBDA_EQ,   &
                       PHI_POLE,    &
                       LAMBDA_POLE, &
                       POINTS)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    Pi_Over_180

IMPLICIT NONE

! Subroutine arguments:
INTEGER                     :: POINTS            ! Number of points to be processed
REAL                        :: COEFF1(POINTS)    ! Coefficient of rotation no 1
REAL                        :: COEFF2(POINTS)    ! Coefficient of rotation no 2
REAL                        :: LAMBDA(POINTS)    ! Longitude
REAL                        :: LAMBDA_EQ(POINTS) ! Longitude in equatorial lat-lon coords
REAL                        :: PHI_POLE          ! Latitude of equatorial lat-lon pole
REAL                        :: LAMBDA_POLE       ! Longitude of equatorial lat-lon pole

! Local declarations:
REAL                        :: A_LAMBDA
REAL                        :: E_LAMBDA
REAL                        :: SIN_E_LAMBDA
REAL                        :: SIN_PHI_POLE
REAL                        :: C1
REAL                        :: C2
REAL                        :: LAMBDA_ZERO
INTEGER                     :: I
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_WCoeff"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! 0.1 Check for special case of no rotation (or complete N-S rotation)

IF (ABS (PHI_POLE) < 90) THEN       ! Normal rotation

  ! 1. Initialise local constants

  ! Longitude of zeroth meridian

  LAMBDA_ZERO = LAMBDA_POLE + 180

  ! Sine and cosine of latitude of eq pole

  SIN_PHI_POLE = SIN (PI_OVER_180 * PHI_POLE)

  ! 2. Evaluate translation coefficients

  DO I = 1, POINTS

    ! Actual longitude converted to radians

    A_LAMBDA = PI_OVER_180 * (LAMBDA(I) - LAMBDA_ZERO)

    ! Convert eq longitude to radians and take sine

    E_LAMBDA = LAMBDA_EQ(I) * PI_OVER_180
    SIN_E_LAMBDA = SIN (E_LAMBDA)

    ! Formulae used are from eqs (4.19) and (4.21)

    C1 = SIN (A_LAMBDA) * SIN_E_LAMBDA * SIN_PHI_POLE + COS (A_LAMBDA) * COS (E_LAMBDA)
    COEFF1(I) = C1
    C2 = SQRT (1 - C1 * C1)
    COEFF2(I) = SIGN (C2, SIN_E_LAMBDA)

  END DO

ELSE       ! Special case: no rotation (or complete N-S rotation)

  C1 = SIGN (1.0, PHI_POLE)           ! =1.0 if no rotation
  DO I = 1, POINTS
    COEFF1(I) = C1
    COEFF2(I) = 0
  END DO

END IF      ! End of test for special case of no rotation

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_WCoeff
