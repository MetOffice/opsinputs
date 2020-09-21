!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Determine whether a point P(x,y) lies within or on the boundary of a
! quadrangle (ABCD) of any shape on a plane.
!
! Check if the vectorial products PA x PC, PB x PA, PC x PD, and PD x PB are all
! negative.
!-------------------------------------------------------------------------------

LOGICAL FUNCTION Ops_InQuadrilateral (px,  &
                                      py,  &
                                      pxv, &
                                      pyv)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

IMPLICIT NONE

! Function arguments:
REAL, INTENT(IN)            :: px        ! (lon) of the point P(x,y) 
REAL, INTENT(IN)            :: py        ! (lat) of the point P(x,y)               
REAL, INTENT(IN)            :: pxv(4)
REAL, INTENT(IN)            :: pyv(4)    ! (lon, lat) of the surrounding cell

! Local declarations:
REAL                        :: zst1
REAL                        :: zst2
REAL                        :: zst3
REAL                        :: zst4
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_InQuadrilateral"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-----------------------------------------------------------------------
! Test to see if the point is within the cell
!-----------------------------------------------------------------------

Ops_InQuadrilateral = .FALSE.

zst1 = (px - pxv(1)) * (py - pyv(4)) - (py - pyv(1)) * (px - pxv(4))

IF (zst1 <= 0.0) THEN
  zst2 = (px - pxv(4)) * (py - pyv(3)) - (py - pyv(4)) * (px - pxv(3))
  IF (zst2 <= 0.0) THEN
    zst3 = (px - pxv(3)) * (py - pyv(2)) - (py - pyv(3)) * (px - pxv(2))
    IF (zst3 <= 0.0) THEN
      zst4 = (px - pxv(2)) * (py - pyv(1)) - (py - pyv(2)) * (px - pxv(1))
      IF (zst4 <= 0.0) Ops_InQuadrilateral = .TRUE.
    END IF
  END IF
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END FUNCTION Ops_InQuadrilateral
