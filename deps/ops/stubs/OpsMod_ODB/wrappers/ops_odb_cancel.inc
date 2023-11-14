!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for odb_cancel.
!-------------------------------------------------------------------------------

#ifndef NO_ODB
SUBROUTINE ops_odb_cancel (session, &
                           query)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE odb_module, ONLY: &
  odb_cancel

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(IN) :: session
CHARACTER(len=*), INTENT(IN)       :: query

! Local declarations:
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_cancel"
INTEGER                            :: return_code
CHARACTER(len=200)                 :: message

IF (UseTrace) CALL gen_trace_entry (routine_name)

return_code = odb_cancel (session % handle, TRIM (query))
IF (return_code < 0) THEN
  WRITE (message, '(A,I0)') "Failure in odb_cancel, rc = ", return_code
  CALL gen_fail (routine_name, &
                 message)
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_odb_cancel

#else

SUBROUTINE ops_odb_cancel (session, &
                           query)

USE GenMod_Core, ONLY: &
  gen_fail

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(IN) :: session
CHARACTER(len=*), INTENT(IN)       :: query

! Local declarations:
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_cancel"

CALL gen_fail (routine_name,                                    &
                "ODB support not compiled for this executable")

END SUBROUTINE ops_odb_cancel

#endif
