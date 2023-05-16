!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for odb_get.
!-------------------------------------------------------------------------------

#ifndef NO_ODB
SUBROUTINE ops_odb_get (session,     &
                        query,       &
                        data,        &
                        pool_number)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE odb_module, ONLY: &
  odb_get

USE OpsMod_ODBKinds, ONLY: &
  odb_int,                 &
  odb_real

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(IN) :: session
CHARACTER(len=*), INTENT(IN)       :: query
REAL(kind=odb_real), INTENT(INOUT) :: data(:,0:)
INTEGER, OPTIONAL, INTENT(IN)      :: pool_number

! Local declarations:
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_get"
INTEGER                            :: return_code
INTEGER(kind=odb_int)              :: number_of_rows
CHARACTER(len=200)                 :: message
INTEGER(kind=odb_int)              :: local_pool_number

IF (UseTrace) CALL gen_trace_entry (routine_name)

IF (PRESENT (pool_number)) THEN
  local_pool_number = pool_number
ELSE
  local_pool_number = odb_all_pools
END IF

number_of_rows = SIZE (data, dim = 1)

return_code = odb_get (session % handle, TRIM (query), data, number_of_rows, poolno = local_pool_number)
IF (return_code < 0) THEN
  WRITE (message, '(A,I0)') "Failure in odb_get for query " // TRIM (query) // ", rc = ", return_code
  CALL gen_fail (routine_name, &
                 message)
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_odb_get

#else

SUBROUTINE ops_odb_get (session,     &
                        query,       &
                        data,        &
                        pool_number)

USE GenMod_Core, ONLY: &
  gen_fail

USE OpsMod_ODBKinds, ONLY: &
  odb_real

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(IN) :: session
CHARACTER(len=*), INTENT(IN)       :: query
REAL(kind=odb_real), INTENT(INOUT) :: data(:,0:)
INTEGER, OPTIONAL, INTENT(IN)      :: pool_number

! Local declarations:
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_get"

CALL gen_fail (routine_name,                                   &
               "ODB support not compiled for this executable")

END SUBROUTINE ops_odb_get

#endif
