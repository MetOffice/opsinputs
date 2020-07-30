!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for odb_getsize.
!-------------------------------------------------------------------------------

#ifndef NO_ODB
SUBROUTINE ops_odb_getsize (session,           &
                            query,             &
                            number_of_rows,    &
                            number_of_columns, &
                            pool_number)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE odb_module, ONLY: &
  odb_getsize

USE OpsMod_ODBKinds, ONLY: &
  odb_int

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(IN) :: session
CHARACTER(len=*), INTENT(IN)       :: query
INTEGER, OPTIONAL, INTENT(OUT)     :: number_of_rows
INTEGER, OPTIONAL, INTENT(OUT)     :: number_of_columns
INTEGER, OPTIONAL, INTENT(IN)      :: pool_number

! Local declarations:
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_getsize"
INTEGER                            :: return_code
INTEGER(kind=odb_int)              :: local_number_of_rows
INTEGER(kind=odb_int)              :: local_number_of_columns
CHARACTER(len=200)                 :: message
INTEGER(kind=odb_int)              :: local_pool_number

IF (UseTrace) CALL gen_trace_entry (routine_name)

IF (PRESENT (pool_number)) THEN
  local_pool_number = pool_number
ELSE
  local_pool_number = odb_all_pools
END IF

return_code = odb_getsize (session % handle, TRIM (query), local_number_of_rows, local_number_of_columns, &
                           poolno = local_pool_number)
IF (return_code < 0) THEN
  WRITE (message, '(A,I0)') "Failure in odb_getsize for query " // TRIM (query) // ", rc = ", return_code
  CALL gen_fail (routine_name, &
                 message)
END IF
IF (PRESENT (number_of_rows)) number_of_rows = local_number_of_rows
IF (PRESENT (number_of_columns)) number_of_columns = local_number_of_columns

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_odb_getsize

#else

SUBROUTINE ops_odb_getsize (session,           &
                            query,             &
                            number_of_rows,    &
                            number_of_columns, &
                            pool_number)

USE GenMod_Core, ONLY: &
  gen_fail

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(IN) :: session
CHARACTER(len=*), INTENT(IN)       :: query
INTEGER, OPTIONAL, INTENT(OUT)     :: number_of_rows
INTEGER, OPTIONAL, INTENT(OUT)     :: number_of_columns
INTEGER, OPTIONAL, INTENT(IN)      :: pool_number

! Local declarations:
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_getsize"

CALL gen_fail (routine_name,                                   &
               "ODB support not compiled for this executable")

END SUBROUTINE ops_odb_getsize

#endif
