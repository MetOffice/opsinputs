!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Contains wrapper routines for some portio2a functions.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_Buffin_Wrapper_char (unit,                  &
                                    array,                 &
                                    maxlen,                &
                                    actual_length,         &
                                    status_if_end_of_file)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_message,         &
  gen_trace_entry,     &
  gen_trace_exit,      &
  gen_warn,            &
  StatusFatal,         &
  StatusWarning,       &
  UseTrace

USE OpsMod_Kinds, ONLY: &
  integer64,            &
  real64

USE OpsMod_UMInterface, ONLY: &
  ops_buffin8

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)            :: unit
CHARACTER(len=*), INTENT(OUT)  :: array(:)
INTEGER, INTENT(IN)            :: maxlen
INTEGER, OPTIONAL, INTENT(OUT) :: actual_length
INTEGER, OPTIONAL, INTENT(IN)  :: status_if_end_of_file

! Local declarations:
INTEGER(kind=integer64)        :: local_unit
CHARACTER(len=1)               :: local_array(SIZE (array) * LEN (array))
INTEGER(kind=integer64)        :: local_maxlen
INTEGER(kind=integer64)        :: local_length
REAL(kind=real64)              :: local_status
CHARACTER(len=300)             :: messages(4)
CHARACTER(len=*), PARAMETER    :: routine_name = "Ops_Buffin_Wrapper_char"
INTEGER                        :: local_status_if_end_of_file

IF (UseTrace) CALL gen_trace_entry (routine_name)

local_unit = unit
local_maxlen = maxlen * LEN (array)

IF (PRESENT (status_if_end_of_file)) THEN
  local_status_if_end_of_file = status_if_end_of_file
ELSE IF (PRESENT (actual_length)) THEN
  local_status_if_end_of_file = StatusWarning
ELSE
  local_status_if_end_of_file = StatusFatal
END IF

local_array = ""

CALL ops_buffin8 (local_unit,   &
                  local_array,  &
                  local_maxlen, &
                  local_length, &
                  local_status)

IF (NINT (local_status) /= -1 .OR. local_maxlen /= local_length) THEN
  IF (NINT (local_status) == 0 .AND. local_status_if_end_of_file /= StatusFatal) THEN
    WRITE (messages(1), '(A)') "ops_buffin end of file reached"
    WRITE (messages(2), '(A,I0)') "unit = ", unit
    WRITE (messages(3), '(A,I0)') "maxlen = ", maxlen
    WRITE (messages(4), '(A,I0)') "length = ", local_length
    IF (local_status_if_end_of_file == StatusWarning) THEN
      CALL gen_message (routine_name,  &
                        messages(1:4))
    ELSE
      CALL gen_warn (routine_name,  &
                     messages(1:4))
    END IF
  ELSE
    WRITE (messages(1), '(A,I0)') "ops_buffin failed with status = ", NINT (local_status)
    WRITE (messages(2), '(A,I0)') "unit = ", unit
    WRITE (messages(3), '(A,I0)') "maxlen = ", maxlen
    WRITE (messages(4), '(A,I0)') "length = ", local_length
    CALL gen_fail (routine_name,  &
                   messages(1:4))
  END IF
END IF

IF (PRESENT (actual_length)) actual_length = local_length
array = TRANSFER (local_array, array)

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE Ops_Buffin_Wrapper_char
