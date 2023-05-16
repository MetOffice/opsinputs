!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Scalar product of 3-element vectors.
!-------------------------------------------------------------------------------

FUNCTION Ops_DotProduct (A, &
                         B) RESULT (dotprod)

IMPLICIT NONE

! Function arguments:
REAL, INTENT(IN) :: A(3)
REAL, INTENT(IN) :: B(3)

! Function result:
REAL             :: dotprod

dotprod = A(1) * B(1) + A(2) * B(2) + A(3) * B(3)

END FUNCTION Ops_DotProduct
