!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for odb_varindex, scalar version.
!-------------------------------------------------------------------------------

#ifndef NO_ODB
SUBROUTINE ops_odb_varindex_scalar (session,      &
                                    query,        &
                                    column_name,  &
                                    column_index)

USE GenMod_Core, ONLY: &
  gen_fail,            &
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

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(IN) :: session
CHARACTER(len=*), INTENT(IN)       :: query
CHARACTER(len=*), INTENT(IN)       :: column_name
INTEGER, INTENT(OUT)               :: column_index

! Local declarations:
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_varindex_scalar"
INTEGER                            :: return_code
INTEGER(kind=odb_int)              :: tmp_column_index(1)
CHARACTER(len=200)                 :: message

IF (UseTrace) CALL gen_trace_entry (routine_name)

return_code = odb_varindex (session % handle, TRIM (query), (/TRIM (column_name)/), tmp_column_index)
IF (return_code <= 0) THEN
  WRITE (message, '(A,I0)') "Invalid status returned from ODB_varindex, query " // TRIM (query) // &
                            " variable " // TRIM (column_name) // " rc = ", return_code
  CALL gen_fail (routine_name, &
                 message)
END IF
column_index = tmp_column_index(1)

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_odb_varindex_scalar

#else

SUBROUTINE ops_odb_varindex_scalar (session,      &
                                    query,        &
                                    column_name,  &
                                    column_index)

USE GenMod_Core, ONLY: &
  gen_fail

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(IN) :: session
CHARACTER(len=*), INTENT(IN)       :: query
CHARACTER(len=*), INTENT(IN)       :: column_name
INTEGER, INTENT(OUT)               :: column_index

! Local declarations:
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_varindex_scalar"

CALL gen_fail (routine_name,                                   &
               "ODB support not compiled for this executable")

END SUBROUTINE ops_odb_varindex_scalar

#endif
