!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for odb_close.
!-------------------------------------------------------------------------------

#ifndef NO_ODB
SUBROUTINE ops_odb_close (session, &
                          save)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE odb_module, ONLY: &
  odb_close

USE OpsMod_ODBKinds, ONLY: &
  odb_int

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(IN) :: session
LOGICAL, OPTIONAL, INTENT(IN)      :: save

! Local declarations:
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_close"
INTEGER                            :: return_code
LOGICAL(kind=odb_int)              :: local_save
CHARACTER(len=200)                 :: message

IF (UseTrace) CALL gen_trace_entry (routine_name)

IF (PRESENT (save)) THEN
  local_save = save
ELSE
  local_save = .TRUE.
END IF

return_code = odb_close (session % handle, save = local_save)
IF (return_code < 0) THEN
  WRITE (message, '(A,I0)') "Failure in odb_close, rc = ", return_code
  CALL gen_fail (routine_name, &
                 message)
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_odb_close

#else

SUBROUTINE ops_odb_close (session, &
                          save)

USE GenMod_Core, ONLY: &
  gen_fail

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(IN) :: session
LOGICAL, OPTIONAL, INTENT(IN)      :: save

! Local declarations:
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_close"

CALL gen_fail (routine_name,                                   &
               "ODB support not compiled for this executable")

END SUBROUTINE ops_odb_close

#endif
