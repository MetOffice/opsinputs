!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Copies the contents of in_string character by character into out_array.
!-------------------------------------------------------------------------------

SUBROUTINE ops_string_to_char_array (in_string, out_array)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN) :: in_string
CHARACTER, INTENT(OUT)       :: out_array(:)

! Local declarations:
CHARACTER(len=*), PARAMETER  :: routine_name = "ops_string_to_char_array"
INTEGER                      :: i

IF (UseTrace) CALL gen_trace_entry (routine_name)

out_array = ""
DO i = 1, LEN_TRIM (in_string)
  out_array(i) = in_string(i:i)
END DO

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_string_to_char_array
