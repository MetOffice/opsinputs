!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Copies the contents of in_string character by character into out_array.
!-------------------------------------------------------------------------------

SUBROUTINE ops_join_string_array (strings,        &
                                  output_string,  &
                                  separator,      &
                                  trim_separator, &
                                  trim_strings)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)           :: strings(:)
CHARACTER(len=*), INTENT(OUT)          :: output_string
CHARACTER(len=*), OPTIONAL, INTENT(IN) :: separator
LOGICAL, OPTIONAL, INTENT(IN)          :: trim_separator
LOGICAL, OPTIONAL, INTENT(IN)          :: trim_strings

! Local declarations:
LOGICAL                                :: local_trim_separator
LOGICAL                                :: local_trim_strings
INTEGER                                :: i
CHARACTER(len=*), PARAMETER            :: routine_name = "ops_join_string_array"
INTEGER                                :: string_position

IF (UseTrace) CALL gen_trace_entry (routine_name)

IF (PRESENT (trim_separator)) THEN
  local_trim_separator = trim_separator
ELSE
  local_trim_separator = .TRUE.
END IF

IF (PRESENT (trim_strings)) THEN
  local_trim_strings = trim_strings
ELSE
  local_trim_strings = .TRUE.
END IF

IF (SIZE (strings) > 0) THEN
  IF (local_trim_strings) THEN
    output_string = TRIM (strings(1))
    string_position = LEN_TRIM (strings(1))
  ELSE
    output_string = strings(1)
    string_position = LEN (strings(1))
  END IF
  DO i = 2, SIZE (strings)
    IF (PRESENT (separator)) THEN
      IF (local_trim_separator .AND. local_trim_strings) THEN
        output_string(string_position +1:) = TRIM (separator) // TRIM (strings(i))
        string_position = string_position + LEN_TRIM (separator) + LEN_TRIM (strings(i))
      ELSE IF (local_trim_separator) THEN
        output_string(string_position +1:) = TRIM (separator) // strings(i)
        string_position = string_position + LEN_TRIM (separator) + LEN (strings(i))
      ELSE IF (local_trim_strings) THEN
        output_string(string_position +1:) = separator // TRIM (strings(i))
        string_position = string_position + LEN (separator) + LEN_TRIM (strings(i))
      ELSE
        output_string(string_position +1:) = separator // strings(i)
        string_position = string_position + LEN (separator) + LEN (strings(i))
      END IF
    ELSE
      IF (local_trim_strings) THEN
        output_string(string_position +1:) = TRIM (strings(i))
        string_position = string_position + LEN_TRIM (strings(i))
      ELSE
        output_string(string_position +1:) = strings(i)
        string_position = string_position + LEN (strings(i))
      END IF
    END IF
  END DO
ELSE
  output_string = ""
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_join_string_array
