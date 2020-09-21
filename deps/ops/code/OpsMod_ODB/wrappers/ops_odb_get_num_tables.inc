!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for odb_getnames that returns the numbers of tables in the odb.
!-------------------------------------------------------------------------------

FUNCTION ops_odb_get_num_tables (session)

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

! Function arguments:
TYPE (ODBSession_type), INTENT(IN) :: session

! Function result:
INTEGER                            :: ops_odb_get_num_tables

! Local declarations:
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_get_num_tables"
CHARACTER(len=200)                 :: message

IF (UseTrace) CALL gen_trace_entry (routine_name)

ops_odb_get_num_tables = odb_getnames (session % handle, "*", "table")
IF (ops_odb_get_num_tables < 0) THEN
  WRITE (message, '(A,I0)') "Failure in odb_getnames while obtaining number of tables, rc = ", &
                            ops_odb_get_num_tables
  CALL gen_fail (routine_name, &
                 message)
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END FUNCTION ops_odb_get_num_tables
