!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for odb_getnames that returns the number of columns in a table.
!-------------------------------------------------------------------------------

#ifndef NO_ODB
FUNCTION ops_odb_get_num_columns (session, &
                                  table)

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
CHARACTER(len=*), INTENT(IN)       :: table

! Function result:
INTEGER                            :: ops_odb_get_num_columns

! Local declarations:
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_get_num_columns"
CHARACTER(len=200)                 :: message

IF (UseTrace) CALL gen_trace_entry (routine_name)

ops_odb_get_num_columns = odb_getnames (session % handle, TRIM (table), "name")
IF (ops_odb_get_num_columns < 0) THEN
  WRITE (message, '(A,I0)') "Invalid number of columns returned from odb_getnames for table " // &
                               TRIM (table) // ", rc = ", ops_odb_get_num_columns
  CALL gen_fail (routine_name, &
                 message)
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END FUNCTION ops_odb_get_num_columns

#else

FUNCTION ops_odb_get_num_columns (session, &
                                  table)

USE GenMod_Core, ONLY: &
  gen_fail

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Function arguments:
TYPE (ODBSession_type), INTENT(IN) :: session
CHARACTER(len=*), INTENT(IN)       :: table

! Function result:
INTEGER                            :: ops_odb_get_num_columns

! Local declarations:
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_get_num_columns"

ops_odb_get_num_columns = 0

CALL gen_fail (routine_name,                                   &
               "ODB support not compiled for this executable")

END FUNCTION ops_odb_get_num_columns

#endif
