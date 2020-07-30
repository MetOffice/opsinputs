!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for odb_open.
!-------------------------------------------------------------------------------

#ifndef NO_ODB
SUBROUTINE ops_odb_open (session, &
                         mode)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE odb_module, ONLY: &
  odb_open

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(INOUT) :: session
CHARACTER(len=*), INTENT(IN)          :: mode

! Local declarations:
CHARACTER(len=*), PARAMETER           :: routine_name = "ops_odb_open"
CHARACTER(len=200)                    :: message

IF (UseTrace) CALL gen_trace_entry (routine_name)

session % handle = odb_open (TRIM (session % dbname), TRIM (mode), session % npools, session % npools_old)
IF (session % handle < 0) THEN
  WRITE (message, '(A,I0)') "Failure in odb_open, rc = ", session % handle
  CALL gen_fail (routine_name, &
                 message)
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_odb_open

#else

SUBROUTINE ops_odb_open (session, &
                         mode)

USE GenMod_Core, ONLY: &
  gen_fail

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(INOUT) :: session
CHARACTER(len=*), INTENT(IN)          :: mode

! Local declarations:
CHARACTER(len=*), PARAMETER           :: routine_name = "ops_odb_open"

CALL gen_fail (routine_name,                                   &
               "ODB support not compiled for this executable")

END SUBROUTINE ops_odb_open

#endif
