!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Print usage text based on arguments definitions.
!-------------------------------------------------------------------------------

SUBROUTINE OpsMod_Argument_usage (args,      &
                                  prog_name)

IMPLICIT NONE

! Subroutine arguments:
CLASS (arguments_type), INTENT(IN) :: args
CHARACTER(len=*), INTENT(IN)       :: prog_name

! Local declarations:
CHARACTER(len=256), ALLOCATABLE    :: strings(:)
INTEGER                            :: i

ALLOCATE (strings(2 * SIZE (args % args) + 1))
strings = ""
strings(1) = TRIM (prog_name) // ":"
DO i = 1, SIZE (args % args)
  IF (args % args(i) % short_opt /= "" .AND. args % args(i) % long_opt /= "") THEN
    strings(2*i) = TRIM (args % args(i) % short_opt) // ", " // args % args(i) % long_opt
  ELSE IF (args % args(i) % short_opt /= "") THEN
    strings(2*i) = args % args(i) % short_opt
  ELSE
    strings(2*i) = args % args(i) % long_opt
  END IF
  strings(2*i + 1) = args % args(i) % description
END DO

DO i = 1, SIZE (strings)
  WRITE (0, '(A)') TRIM (strings(i))
END DO

END SUBROUTINE OpsMod_Argument_usage
