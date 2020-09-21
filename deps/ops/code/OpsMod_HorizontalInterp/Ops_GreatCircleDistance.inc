!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Great circle distance between pts (lat1,lon1) (lat2,lon2).
!-------------------------------------------------------------------------------

REAL FUNCTION Ops_GreatCircleDistance (pa1, &
                                       pa2, &
                                       pb1, &
                                       pb2, &
                                       pc1, &
                                       pc2)

! Function arguments:
REAL, INTENT(IN) :: pa1   !  sin(lat1)
REAL, INTENT(IN) :: pa2   !  sin(lat2)
REAL, INTENT(IN) :: pb1   !  cos(lat1) * cos(lon1)
REAL, INTENT(IN) :: pb2   !  cos(lat2) * cos(lon2)
REAL, INTENT(IN) :: pc1   !  cos(lat1) * sin(lon1)
REAL, INTENT(IN) :: pc2   !  cos(lat2) * sin(lon2)

Ops_GreatCircleDistance = ASIN (SQRT (1.0 - (pa1 * pa2 + pb1 * pb2 + pc1 * pc2) ** 2))

END FUNCTION Ops_GreatCircleDistance

