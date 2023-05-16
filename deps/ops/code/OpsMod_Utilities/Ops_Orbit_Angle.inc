!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Compute angular position in orbit.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_Orbit_Angle (year,        &
                            day_of_year, &
                            secs_of_day, &
                            pos_vector,  &
                            vel_vector,  &
                            angle)

USE OpsMod_Constants, ONLY: &
    deg2rad,                &
    rad2deg

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI,                                & ! Real Missing data indicator.
    PI                                     ! 3.1415926....

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN) :: year           ! Start year
INTEGER, INTENT(IN) :: day_of_year    ! Start day
INTEGER, INTENT(IN) :: secs_of_day    ! Seconds of day since midnight
REAL, INTENT(IN)    :: pos_vector(3)  ! metres
REAL, INTENT(IN)    :: vel_vector(3)  ! m/s
REAL, INTENT(OUT)   :: angle

! Local declarations:
REAL, PARAMETER     :: ecliptic_npole_RA_hours = 18.0
REAL, PARAMETER     :: ecliptic_npole_dec = 1.16152   ! 66.55 deg
REAL                :: hour_of_day
REAL                :: orbitplane_vector(3)
REAL                :: solarplane_vector(3)
REAL                :: refvector(3)
REAL                :: d
REAL                :: m
REAL                :: lst
REAL                :: ha
REAL                :: jday

!-------------------------------------------------------------------------------
! Initialise output as missing data
!-------------------------------------------------------------------------------

angle = RMDI

hour_of_day = secs_of_day / 3600.0
jday = INT ((year - 2001) * 365.25D0) + day_of_year + 364.5D0 + hour_of_day / 24.0

! Local sidereal time and hour angle for ecliptic N pole, hence x,y,z vector

lst = 100.46 + 0.985647352 * jday + hour_of_day * 15           ! lst in deg
ha = lst * deg2rad - 2 * PI * ecliptic_npole_RA_hours / 24.0   ! ha in radians
solarplane_vector(1) = COS (ha) * COS (ecliptic_npole_dec)
solarplane_vector(2) = -SIN (ha) * COS (ecliptic_npole_dec)
solarplane_vector(3) = SIN (ecliptic_npole_dec)

! Compute the orbit plane

CALL Ops_CrossProduct (pos_vector,        &
                       vel_vector,        &
                       orbitplane_vector)

! Define a reference vector where the orbit plane and solar plane intersect

CALL Ops_CrossProduct (solarplane_vector, &
                       orbitplane_vector, &
                       refvector)

! Find the angle between the position (p) of the satellite and the reference vector (r)
! p.r = |p||r|*cos(angle) therefore,  angle = acos(p.r/|p||r|)  thus,  p.r/|p||r| <= 1

d = Ops_DotProduct (pos_vector, refvector)
m = Ops_VectorMod (pos_vector) * Ops_VectorMod (refvector)
IF (ABS (d) > ABS (m)) THEN
  angle = RMDI
ELSE
  angle = ACOS (d/m) * rad2deg
END IF

! Finally resolve the sign ambiguity

IF (Ops_DotProduct (solarplane_vector, pos_vector) < 0) THEN
  angle = -angle + 360
END IF

END SUBROUTINE Ops_Orbit_Angle
