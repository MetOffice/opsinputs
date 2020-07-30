!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Divide a number into n shares, accounting for remainders
!-------------------------------------------------------------------------------

PURE FUNCTION Ops_Share (total, num) RESULT (shares)

IMPLICIT NONE

! Function arguments:
INTEGER, INTENT(IN) :: total
INTEGER, INTENT(IN) :: num

! Function result
INTEGER             :: shares(num)

! Local declarations:
INTEGER             :: i

shares = total / num
DO i = 1, MODULO (total, num)
  shares(i) = shares(i) + 1
END DO

END FUNCTION Ops_Share
