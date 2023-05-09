!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Splits "input" into a character array, using the characters in
! "separator_set" as delimiters. Returns the "output" as a pointer to the
! character array. If "separator_set" is omitted, splits on whitespace.
!-------------------------------------------------------------------------------

SUBROUTINE Gen_StringSplit (input,         &
                            separator_set, &
                            output)

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)           :: input
CHARACTER(len=*), INTENT(IN), OPTIONAL :: separator_set
CHARACTER(len=*), POINTER              :: output(:)

! Local declarations:
CHARACTER(len=LEN (input))             :: working_output(LEN_TRIM (input) / 2 + 1)
INTEGER                                :: i
INTEGER                                :: current_pos
INTEGER                                :: pos
INTEGER                                :: n_output

current_pos = 1
n_output = 0

scan_input: DO
  IF (PRESENT (separator_set)) THEN
    pos = SCAN (input(current_pos:), separator_set)
  ELSE
    pos = SCAN (input(current_pos:), ' ')
  END IF
  SELECT CASE (pos)
    CASE (0)
      n_output = n_output + 1
      working_output(n_output) = input(current_pos :)
      EXIT scan_input
    CASE (1)
      IF (current_pos == LEN (input)) EXIT scan_input
    CASE DEFAULT
      n_output = n_output + 1
      working_output(n_output) = input(current_pos : current_pos + pos - 2)
  END SELECT
  current_pos = current_pos + pos
END DO scan_input

ALLOCATE (output(n_output))
DO i = 1, n_output
  output(i) = working_output(i)
END DO

END SUBROUTINE Gen_StringSplit
