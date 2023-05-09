!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Convert lat/lon to XYZ coordinates.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_LatLon_to_XYZ (lat, &
                              lon, &
                              x,   &
                              y,   &
                              z)

USE OpsMod_Constants, ONLY: &
    deg2rad

IMPLICIT NONE

! Subroutine arguments:
REAL, INTENT(IN)  :: lat
REAL, INTENT(IN)  :: lon
REAL, INTENT(OUT) :: x
REAL, INTENT(OUT) :: y
REAL, INTENT(OUT) :: z

! Local declarations:
REAL              :: coslat

coslat = COS (lat * deg2rad)
x = COS (lon * deg2rad) * coslat
y = SIN (lon * deg2rad) * coslat
z = SIN (lat * deg2rad)

END SUBROUTINE Ops_LatLon_to_XYZ
