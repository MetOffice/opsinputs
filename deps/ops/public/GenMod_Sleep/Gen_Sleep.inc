!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Sleep for a decimal number of seconds.
!
! This procedure uses a spin wait:
!
!   <= 0.0:  Does not sleep.
!   > 0.0:   Sleeps for this number of seconds.
!-------------------------------------------------------------------------------

SUBROUTINE Gen_Sleep (sleep_time)

IMPLICIT NONE

! Subroutine arguments:
REAL, INTENT(IN) :: sleep_time

CALL Gen_SpinWait (sleep_time)

END SUBROUTINE Gen_Sleep
