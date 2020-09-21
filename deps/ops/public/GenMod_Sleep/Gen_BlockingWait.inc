!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Sleep for a decimal number of seconds.
!
! Uses a blocking wait:
!
!   <= 0.0: Does not sleep.
!   > 0.0:  Sleeps for this number of seconds.
!-------------------------------------------------------------------------------

SUBROUTINE Gen_BlockingWait (sleep_time)

USE, INTRINSIC :: ISO_C_BINDING, ONLY: &
  C_INT,                               &
  C_LONG

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)  :: sleep_time

! Local declarations:
INTEGER(kind=C_INT)  :: sleep_time_seconds
INTEGER(kind=C_LONG) :: return_code

sleep_time_seconds = sleep_time
CALL Gen_SleepWrapper (sleep_time_seconds, &
                       return_code)
IF (return_code /= 0) THEN
  WRITE (0, '(A,I0)') "Gen_SleepWrapper returned with code ", return_code
END IF

END SUBROUTINE Gen_BlockingWait
