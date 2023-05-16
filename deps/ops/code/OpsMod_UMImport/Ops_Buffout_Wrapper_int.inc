!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Contains wrapper routines for some portio2a functions.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_Buffout_Wrapper_int (unit,    &
                                    array,   &
                                    maxlen,  &
                                    success)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  gen_warn,            &
  UseTrace

USE OpsMod_Kinds, ONLY: &
  integer64,            &
  real64

USE OpsMod_UMInterface, ONLY: &
  ops_buffout

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)                 :: unit
INTEGER(kind=integer64), INTENT(IN) :: array(:)
INTEGER, INTENT(IN)                 :: maxlen
LOGICAL, OPTIONAL, INTENT(OUT)      :: success

! Local declarations:
INTEGER(kind=integer64)             :: local_unit
INTEGER(kind=integer64)             :: local_array(SIZE (array))
INTEGER(kind=integer64)             :: local_maxlen
INTEGER(kind=integer64)             :: local_length
REAL(kind=real64)                   :: local_status
CHARACTER(len=300)                  :: messages(4)
CHARACTER(len=*), PARAMETER         :: routine_name = "Ops_Buffout_Wrapper_int"

IF (UseTrace) CALL gen_trace_entry (routine_name)

local_unit = unit
local_maxlen = maxlen
local_array = array

CALL ops_buffout (local_unit,   &
                  local_array,  &
                  local_maxlen, &
                  local_length, &
                  local_status)

IF (NINT (local_status) /= -1 .OR. local_length /= maxlen) THEN
  WRITE (messages(1), '(A,I0)') "ops_buffout failed with status = ", NINT (local_status)
  WRITE (messages(2), '(A,I0)') "unit = ", unit
  WRITE (messages(3), '(A,I0)') "maxlen = ", maxlen
  WRITE (messages(4), '(A,I0)') "length = ", local_length
  IF (PRESENT (success)) THEN
    CALL gen_warn (routine_name,  &
                   messages(1:4))
    success = .FALSE.
  ELSE
    CALL gen_fail (routine_name,  &
                   messages(1:4))
  END IF
ELSE
  IF (PRESENT (success)) THEN
    success = .TRUE.
  END IF
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE Ops_Buffout_Wrapper_int
