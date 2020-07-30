!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Contains wrapper routines for some portio2a functions.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_Setpos_Wrapper (unit,         &
                               word_address)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE OpsMod_Kinds, ONLY: &
  integer64

USE OpsMod_UMInterface, ONLY: &
  ops_setpos

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)         :: unit
INTEGER, INTENT(IN)         :: word_address

! Local declarations:
INTEGER(kind=integer64)     :: local_unit
INTEGER(kind=integer64)     :: local_word_address
INTEGER(kind=integer64)     :: err
CHARACTER(len=300)          :: messages(3)
CHARACTER(len=*), PARAMETER :: routine_name = "Ops_Setpos_Wrapper"

IF (UseTrace) CALL gen_trace_entry (routine_name)

local_unit = unit
local_word_address = word_address

CALL ops_setpos (local_unit,         &
                 local_word_address, &
                 err)

IF (err /= 0) THEN
  WRITE (messages(1), '(A,I0)') "ops_setpos failed with err = ", err
  WRITE (messages(2), '(A,I0)') "unit = ", unit
  WRITE (messages(3), '(A,I0)') "word_address = ", word_address
  CALL gen_fail (routine_name,  &
                 messages(1:3))
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE Ops_Setpos_Wrapper
