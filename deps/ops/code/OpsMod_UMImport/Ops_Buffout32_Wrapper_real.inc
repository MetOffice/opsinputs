!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Contains wrapper routines for some portio2a functions.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_Buffout32_Wrapper_real (unit,   &
                                       array,  &
                                       maxlen)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE OpsMod_Kinds, ONLY: &
  integer64,            &
  real64

USE OpsMod_UMInterface, ONLY: &
  ops_buffo32

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)           :: unit
REAL(kind=real64), INTENT(IN) :: array(:)
INTEGER, INTENT(IN)           :: maxlen

! Local declarations:
INTEGER(kind=integer64)       :: local_unit
REAL(kind=real64)             :: local_array(SIZE (array))
INTEGER(kind=integer64)       :: local_maxlen
INTEGER(kind=integer64)       :: local_length
REAL(kind=real64)             :: local_status
CHARACTER(len=300)            :: messages(4)
CHARACTER(len=*), PARAMETER   :: routine_name = "Ops_Buffout32_Wrapper_real"

IF (UseTrace) CALL gen_trace_entry (routine_name)

local_unit = unit
local_maxlen = 2 * maxlen
local_array = array

CALL ops_buffo32 (local_unit,   &
                  local_array,  &
                  local_maxlen, &
                  local_length, &
                  local_status)

IF (NINT (local_status) /= -1 .OR. local_length /= local_maxlen) THEN
  WRITE (messages(1), '(A,I0)') "ops_buffout failed with status = ", NINT (local_status)
  WRITE (messages(2), '(A,I0)') "unit = ", unit
  WRITE (messages(3), '(A,I0)') "maxlen = ", maxlen
  WRITE (messages(4), '(A,I0)') "length = ", local_length
  CALL gen_fail (routine_name,  &
                 messages(1:4))
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE Ops_Buffout32_Wrapper_real
