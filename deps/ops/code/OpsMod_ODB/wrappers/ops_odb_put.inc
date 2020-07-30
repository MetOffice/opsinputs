!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for odb_put.
!-------------------------------------------------------------------------------

#ifndef NO_ODB
SUBROUTINE ops_odb_put (session,     &
                        query,       &
                        data,        &
                        pool_number)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE odb_module, ONLY: &
  odb_put

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
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_put"
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

number_of_rows = SIZE (data,dim = 1)

IF (number_of_rows > 0) THEN
  return_code = odb_put (session % handle, TRIM (query), data, number_of_rows, poolno = local_pool_number)
  IF (return_code <= 0) THEN
    WRITE (message, '(A,I0,A,I0)') "Failure in odb_put, query = " // TRIM (query) // &
                                   ", putting ", number_of_rows, " row, rc = ", return_code
    CALL gen_fail (routine_name, &
                   message)
  END IF
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_odb_put

#else

SUBROUTINE ops_odb_put (session,     &
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
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_put"

CALL gen_fail (routine_name,                                   &
               "ODB support not compiled for this executable")

END SUBROUTINE ops_odb_put

#endif
