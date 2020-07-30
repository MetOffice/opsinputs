!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Sleep for a decimal number of seconds.
!
! Implements a spin wait:
!
!   <= 0.0:  Writes an error message - does not sleep.
!   > 0.0:   Sleeps for this number of seconds.
!-------------------------------------------------------------------------------

SUBROUTINE Gen_SpinWait (sleep_time)

IMPLICIT NONE

! Subroutine arguments:
REAL, INTENT(IN) :: sleep_time

! Local declarations:
INTEGER          :: start_time
INTEGER          :: curr_time
INTEGER          :: prev_time
INTEGER          :: rate
INTEGER          :: wrap
INTEGER          :: countmax

wrap = 0

CALL SYSTEM_CLOCK (COUNT = start_time,   &
                   COUNT_RATE = rate,    &
                   COUNT_MAX = countmax)

curr_time = start_time
prev_time = start_time

IF (sleep_time > 0.0) THEN
  IF (rate > 0) THEN
    DO WHILE ((curr_time - start_time) / REAL (rate) < sleep_time)
      CALL SYSTEM_CLOCK (COUNT = curr_time)

      IF (curr_time < prev_time) THEN
        wrap = wrap + 1
      END IF

      prev_time = curr_time
      curr_time = curr_time + wrap * countmax
    END DO
  END IF
END IF

END SUBROUTINE Gen_SpinWait
