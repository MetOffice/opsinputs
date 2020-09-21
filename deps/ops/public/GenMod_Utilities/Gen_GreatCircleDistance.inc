!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Find the great circle distance between 2 points
!-------------------------------------------------------------------------------

ELEMENTAL FUNCTION Gen_GreatCircleDistance (Lat1, &
                                            Lon1, &
                                            Lat2, &
                                            Lon2)

USE GenMod_MiscUMScienceConstants, ONLY: &
  earth_radius,                          &
  pi_over_180

IMPLICIT NONE

! Function arguments:
REAL, INTENT(IN) :: Lat1   ! The latitude of the first point
REAL, INTENT(IN) :: Lon1   ! The longitude of the first point
REAL, INTENT(IN) :: Lat2   ! The latitude of the second point
REAL, INTENT(IN) :: Lon2   ! The longitude of the second point

! Function result:
REAL             :: Gen_GreatCircleDistance  ! The great circle distance

! Local declarations:
REAL             :: Argument

Argument = SIN (pi_over_180 * Lat1) * SIN (pi_over_180 * Lat2) + &
           COS (pi_over_180 * Lat1) * COS (pi_over_180 * Lat2) * &
           COS (pi_over_180 * Lon1 - pi_over_180 * Lon2)

Gen_GreatCircleDistance = Earth_Radius * ACOS (MAX (MIN (1.0, Argument), -1.0)) / 1000

END FUNCTION Gen_GreatCircleDistance
