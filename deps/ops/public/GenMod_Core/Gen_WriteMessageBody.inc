!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Output the body part of a message for a procedure.
!-------------------------------------------------------------------------------

SUBROUTINE Gen_WriteMessageBody (Message,  &
                                 unit_out, &
                                 len_max,  &
                                 is_local)

USE GenMod_Control, ONLY: &
  mype

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN) :: Message(:)
INTEGER, INTENT(IN)          :: unit_out
INTEGER, INTENT(IN)          :: len_max
LOGICAL, INTENT(IN)          :: is_local

! Local declarations:
INTEGER                      :: i
INTEGER                      :: iB, iE   ! loop counters
INTEGER                      :: len_mesg ! length of message

!$OMP CRITICAL
DO i = 1, SIZE (Message)
  ! Wrap message, if its length exceeds the maximum length
  iB = 0
  iE = 0
  len_mesg = LEN (Message(i))

  DO
    iB = iE + 1
    iE = iB + len_max - 1

    IF (iE > len_mesg) iE = len_mesg

    IF (is_local) THEN
       WRITE (unit_out, '(A,I0,A)') '[', mype, ': ' // TRIM (Message(i)(iB:iE)) // ']'

    ELSE
      WRITE (unit_out, '(A)') TRIM (Message(i)(iB:iE))
    END IF

    IF (iE >= LEN_TRIM (Message(i))) EXIT
  END DO
END DO
!$OMP END CRITICAL

END SUBROUTINE Gen_WriteMessageBody
