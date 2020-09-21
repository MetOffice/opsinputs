!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for odb_getnames that returns a list of table names.
!-------------------------------------------------------------------------------

SUBROUTINE ops_odb_get_table_names (session, &
                                    tables)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE odb_module, ONLY: &
  odb_getnames

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(IN) :: session
CHARACTER(len=*), INTENT(OUT)      :: tables(:)

! Local declarations:
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_get_table_names"
INTEGER                            :: return_code
CHARACTER(len=200)                 :: message

IF (UseTrace) CALL gen_trace_entry (routine_name)

return_code = odb_getnames (session % handle, "*", "table", outnames = tables)
IF (return_code /= SIZE (tables)) THEN
  WRITE (message, '(A,I0,A,I0)') "Invalid number of tables returned from odb_getnames, expected ", &
                                 SIZE (tables), " got ", return_code
  CALL gen_fail (routine_name, &
                 message)
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_odb_get_table_names
