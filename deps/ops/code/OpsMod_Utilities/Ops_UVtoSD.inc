!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Generate speed and direction from wind U and V.
!-------------------------------------------------------------------------------

ELEMENTAL SUBROUTINE Ops_UVtoSD (u,         &
                                 v,         &
                                 speed,     &
                                 direction)

USE GenMod_MiscUMScienceConstants, ONLY: &
  PI,                                    &
  RMDI

IMPLICIT NONE

! Subroutine arguments:
REAL, INTENT(IN)  :: u
REAL, INTENT(IN)  :: v
REAL, INTENT(OUT) :: speed
REAL, INTENT(OUT) :: direction

IF (u /= RMDI .AND. v /= RMDI) THEN
  speed = SQRT (u ** 2 + v ** 2)
  IF (v == 0 .AND. u == 0) THEN
    direction = 0
  ELSE
    direction = MOD(270 - ATAN2 (v, u) * 180 / PI, 360.0)
  END IF
ELSE
  speed = RMDI
  direction = RMDI
END IF

END SUBROUTINE Ops_UVtoSD
