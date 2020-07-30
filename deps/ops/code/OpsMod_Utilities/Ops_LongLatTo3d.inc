!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Description: This subroutine converts a position given by the Latitude and
! Longitude in degrees, to a 3-d vector giving the position in cartesian
! coordinates.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_LongLatTo3d (Long, &
                            Lat,  &
                            v)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    Earth_Radius,                        &
    RECIP_PI_OVER_180

IMPLICIT NONE

! Subroutine arguments:
REAL, INTENT(IN)            :: long
REAL, INTENT(IN)            :: lat
REAL, INTENT(OUT)           :: v(3)

! Local declarations:
REAL                        :: latrad
REAL                        :: longrad
REAL                        :: rplan
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_LongLatTo3d"

!----------------------------------
! 0. Tracing and initialisations
!----------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

LongRad = Long / RECIP_PI_OVER_180
LatRad = Lat / RECIP_PI_OVER_180
rplan = Earth_Radius * COS (latrad)
v = (/SIN (Longrad) * rplan, COS (Longrad) * rplan, SIN (latrad) * Earth_Radius/)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_LongLatTo3d
