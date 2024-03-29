!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Contains wrapper routines for some portio2a functions.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_Buffin_Wrapper_int32 (unit,   &
                                     array,  &
                                     maxlen)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE OpsMod_Kinds, ONLY: &
  integer32,            &
  integer64,            &
  real64

USE OpsMod_UMInterface, ONLY: &
  ops_buffin

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)                  :: unit
INTEGER(kind=integer32), INTENT(OUT) :: array(:)
INTEGER, INTENT(IN)                  :: maxlen

! Local declarations:
INTEGER(kind=integer64)              :: local_unit
INTEGER(kind=integer64)              :: local_array(SIZE (array))
INTEGER(kind=integer64)              :: local_maxlen
INTEGER(kind=integer64)              :: local_length
REAL(kind=real64)                    :: local_status
CHARACTER(len=300)                   :: messages(4)
CHARACTER(len=*), PARAMETER          :: routine_name = "Ops_Buffin_Wrapper_int32"

IF (UseTrace) CALL gen_trace_entry (routine_name)

local_unit = unit
local_maxlen = maxlen

CALL ops_buffin (local_unit,   &
                 local_array,  &
                 local_maxlen, &
                 local_length, &
                 local_status)

IF (NINT (local_status) /= -1 .OR. local_maxlen /= local_length) THEN
  WRITE (messages(1), '(A,I0)') "ops_buffin failed with status = ", NINT (local_status)
  WRITE (messages(2), '(A,I0)') "unit = ", unit
  WRITE (messages(3), '(A,I0)') "maxlen = ", maxlen
  WRITE (messages(4), '(A,I0)') "length = ", local_length
  CALL gen_fail (routine_name,  &
                 messages(1:4))
END IF

array = local_array

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE Ops_Buffin_Wrapper_int32
