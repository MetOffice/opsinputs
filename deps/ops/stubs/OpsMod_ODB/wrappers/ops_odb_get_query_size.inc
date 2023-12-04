!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for odb_select that returns the numbers of rows and columns in a
! query.
!-------------------------------------------------------------------------------

#ifndef NO_ODB
SUBROUTINE ops_odb_get_query_size (session,           &
                                   query,             &
                                   number_of_rows,    &
                                   number_of_columns, &
                                   query_strings,     &
                                   query_values,      &
                                   verbose_output,    &
                                   pool_number)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE odb_module, ONLY: &
  odb_select

USE OpsMod_ODBKinds, ONLY: &
  odb_int,                 &
  odb_real

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(IN)        :: session
CHARACTER(len=*), INTENT(IN)              :: query
INTEGER, INTENT(OUT)                      :: number_of_rows
INTEGER, INTENT(OUT)                      :: number_of_columns
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: query_strings(:)
REAL(kind=odb_real), OPTIONAL, INTENT(IN) :: query_values(:)
LOGICAL, OPTIONAL, INTENT(IN)             :: verbose_output
INTEGER, OPTIONAL, INTENT(IN)             :: pool_number

! Local declarations:
CHARACTER(len=*), PARAMETER               :: routine_name = "ops_odb_get_query_size"
INTEGER                                   :: return_code
INTEGER(kind=odb_int)                     :: number_of_rows_local
INTEGER(kind=odb_int)                     :: number_of_columns_local
CHARACTER(len=200)                        :: message
INTEGER(kind=odb_int)                     :: local_pool_number
LOGICAL(kind=odb_int)                     :: local_inform_progress

IF (UseTrace) CALL gen_trace_entry (routine_name)

IF (PRESENT (pool_number)) THEN
  local_pool_number = pool_number
ELSE
  local_pool_number = odb_all_pools
END IF

IF (PRESENT (verbose_output)) THEN
  local_inform_progress = verbose_output
ELSE
  local_inform_progress = .FALSE.
END IF

return_code = odb_select (session % handle, TRIM (query), number_of_rows_local, number_of_columns_local, &
                          setvars = query_strings, values = query_values, &
                          inform_progress = local_inform_progress, poolno = local_pool_number)
IF (return_code < 0) THEN
  WRITE (message, '(A,I0)') "Failure in odb_select for query " // TRIM (query) // ", rc = ", return_code
  CALL gen_fail (routine_name, &
                 message)
END IF
number_of_rows = number_of_rows_local
number_of_columns = number_of_columns_local

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_odb_get_query_size

#else

SUBROUTINE ops_odb_get_query_size (session,           &
                                   query,             &
                                   number_of_rows,    &
                                   number_of_columns, &
                                   query_strings,     &
                                   query_values,      &
                                   verbose_output,    &
                                   pool_number)

USE GenMod_Core, ONLY: &
  gen_fail

USE OpsMod_ODBKinds, ONLY: &
  odb_real

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(IN)        :: session
CHARACTER(len=*), INTENT(IN)              :: query
INTEGER, INTENT(OUT)                      :: number_of_rows
INTEGER, INTENT(OUT)                      :: number_of_columns
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: query_strings(:)
REAL(kind=odb_real), OPTIONAL, INTENT(IN) :: query_values(:)
LOGICAL, OPTIONAL, INTENT(IN)             :: verbose_output
INTEGER, OPTIONAL, INTENT(IN)             :: pool_number

! Local declarations:
CHARACTER(len=*), PARAMETER               :: routine_name = "ops_odb_get_query_size"

CALL gen_fail (routine_name,                                   &
               "ODB support not compiled for this executable")

END SUBROUTINE ops_odb_get_query_size

#endif
