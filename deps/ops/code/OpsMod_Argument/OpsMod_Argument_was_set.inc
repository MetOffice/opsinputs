!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Test if argument was actually set.
!-------------------------------------------------------------------------------

FUNCTION OpsMod_Argument_was_set (args, arg) RESULT (was_set)

USE GenMod_Core, ONLY: &
  gen_fail

IMPLICIT NONE

! Function arguments:
CLASS (arguments_type), INTENT(IN) :: args
CHARACTER(len=*), INTENT(IN)       :: arg

! Function result:
LOGICAL                            :: was_set

! Local declarations:
INTEGER                            :: i
LOGICAL                            :: found
CHARACTER(len=200)                 :: message

found = .FALSE.
IF (ALLOCATED (args % args)) THEN
  DO i = 1, SIZE (args % args)
    IF (args % args(i) % short_opt == arg .OR. args % args(i) % long_opt == arg) THEN
      found = .TRUE.
      was_set = args % args(i) % was_set
      EXIT
    END IF
  END DO
END IF

IF (.NOT. found) THEN
  WRITE (message, '(2A)') "Invalid argument ", TRIM (arg)
  CALL gen_fail ("was_set", message)
END IF

END FUNCTION OpsMod_Argument_was_set
