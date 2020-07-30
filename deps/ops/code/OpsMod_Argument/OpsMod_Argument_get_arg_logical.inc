!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Retrieve a logical argument.
!-------------------------------------------------------------------------------

SUBROUTINE OpsMod_Argument_get_arg_logical (args, &
                                            arg,  &
                                            val)

USE GenMod_Core, ONLY: &
  gen_fail

IMPLICIT NONE

! Subroutine arguments:
CLASS (arguments_type), INTENT(IN) :: args
CHARACTER(len=*), INTENT(IN)       :: arg
LOGICAL, INTENT(OUT)               :: val

! Local declarations:
INTEGER                            :: i
LOGICAL                            :: found
CHARACTER(len=200)                 :: message

found = .FALSE.
IF (ALLOCATED (args % args)) THEN
  DO i = 1, SIZE (args % args)
    IF (args % args(i) % short_opt == arg .OR. args % args(i) % long_opt == arg) THEN
      found = .TRUE.
      IF (args % args(i) % type == type_logical) THEN
        val = args % args(i) % logical_val
        EXIT
      ELSE
        WRITE (message, '(A,I0,2A)') "Invalid type: ", args % args(i) % type, " for arg ", TRIM (arg)
        CALL gen_fail ("get_arg_logical", message)
      END IF
    END IF
  END DO
END IF

IF (.NOT. found) THEN
  WRITE (message, '(2A)') "Invalid argument ", TRIM (arg)
  CALL gen_fail ("get_arg_logical", message)
END IF

END SUBROUTINE OpsMod_Argument_get_arg_logical