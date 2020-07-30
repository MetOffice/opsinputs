!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Vector product of 3-element vectors.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CrossProduct (A, &
                             B, &
                             C)

IMPLICIT NONE

! Subroutine arguments:
REAL, INTENT(IN)  :: A(3)
REAL, INTENT(IN)  :: B(3)
REAL, INTENT(OUT) :: C(3)

C(1) = A(2) * B(3) - A(3) * B(2)
C(2) = A(3) * B(1) - A(1) * B(3)
C(3) = A(1) * B(2) - A(2) * B(1)

END SUBROUTINE Ops_CrossProduct
