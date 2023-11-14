!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for odb_varindex, returns true if column_name is available in query.
!-------------------------------------------------------------------------------

#ifndef NO_ODB
FUNCTION ops_odb_is_column_available (session,     &
                                      query,       &
                                      column_name)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE odb_module, ONLY: &
  odb_varindex

USE OpsMod_ODBKinds, ONLY: &
  odb_int

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Function arguments:
TYPE (ODBSession_type), INTENT(IN) :: session
CHARACTER(len=*), INTENT(IN)       :: query
CHARACTER(len=*), INTENT(IN)       :: column_name

! Function result:
LOGICAL                            :: ops_odb_is_column_available

! Local declarations:
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_is_column_available"
INTEGER                            :: return_code
INTEGER(kind=odb_int)              :: tmp_column_index(1)

IF (UseTrace) CALL gen_trace_entry (routine_name)

return_code = odb_varindex (session % handle, TRIM (query), (/TRIM (column_name)/), tmp_column_index)
IF (return_code > 0) THEN
  ops_odb_is_column_available = .TRUE.
ELSE
  ops_odb_is_column_available = .FALSE.
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END FUNCTION ops_odb_is_column_available

#else

FUNCTION ops_odb_is_column_available (session,     &
                                      query,       &
                                      column_name)

USE GenMod_Core, ONLY: &
  gen_fail

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Function arguments:
TYPE (ODBSession_type), INTENT(IN) :: session
CHARACTER(len=*), INTENT(IN)       :: query
CHARACTER(len=*), INTENT(IN)       :: column_name

! Function result:
LOGICAL                            :: ops_odb_is_column_available

! Local declarations:
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_is_column_available"

ops_odb_is_column_available = .FALSE.

CALL gen_fail (routine_name,                                   &
               "ODB support not compiled for this executable")

END FUNCTION ops_odb_is_column_available

#endif
