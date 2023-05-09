!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Vector mod of 3-element vectors.
!-------------------------------------------------------------------------------

FUNCTION Ops_VectorMod (a) RESULT (vector_mod)

IMPLICIT NONE

! Function arguments:
REAL, INTENT(IN) :: a(3)

! Function result:
REAL             :: vector_mod

vector_mod = SQRT (a(1) * a(1) + a(2) * a(2) + a(3) * a(3))

END FUNCTION Ops_VectorMod
