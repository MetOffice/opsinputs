!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for odb_getnames that returns a list of columns names for a table.
!-------------------------------------------------------------------------------

#ifndef NO_ODB
SUBROUTINE ops_odb_get_column_names (session, &
                                     table,   &
                                     columns)

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
CHARACTER(len=*), INTENT(IN)       :: table
CHARACTER(len=*), INTENT(OUT)      :: columns(:)

! Local declarations:
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_get_column_names"
INTEGER                            :: return_code
CHARACTER(len=200)                 :: message

IF (UseTrace) CALL gen_trace_entry (routine_name)

return_code = odb_getnames (session % handle, TRIM (table), "name", outnames = columns)
IF (return_code /= SIZE (columns)) THEN
  WRITE (message, '(A,I0,A,I0)') "Wrong number of columns from odb_getnames for table " // &
                                 TRIM (table) // ", expected ", SIZE (columns), ", got ", return_code
  CALL gen_fail (routine_name, &
                 message)
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_odb_get_column_names

#else

SUBROUTINE ops_odb_get_column_names (session, &
                                     table,   &
                                     columns)

USE GenMod_Core, ONLY: &
  gen_fail

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(IN) :: session
CHARACTER(len=*), INTENT(IN)       :: table
CHARACTER(len=*), INTENT(OUT)      :: columns(:)

! Local declarations:
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_get_column_names"

CALL gen_fail (routine_name,                                   &
               "ODB support not compiled for this executable")

END SUBROUTINE ops_odb_get_column_names

#endif
