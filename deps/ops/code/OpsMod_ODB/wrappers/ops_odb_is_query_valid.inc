!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for odb_addview that checks whether a query is valid or not.
!-------------------------------------------------------------------------------

#ifndef NO_ODB
FUNCTION ops_odb_is_query_valid (session, &
                                 query)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE odb_module, ONLY: &
  odb_addview

USE OpsMod_ODBKinds, ONLY: &
  odb_int

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Function arguments:
TYPE (ODBSession_type), INTENT(IN) :: session
CHARACTER(len=*), INTENT(IN)       :: query

! Function result:
LOGICAL                            :: ops_odb_is_query_valid

! Local declarations:
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_is_query_valid"

IF (UseTrace) CALL gen_trace_entry (routine_name)

IF (query(1:1) == "@") THEN  ! Query is a table.
  ops_odb_is_query_valid = .TRUE.
ELSE
  ops_odb_is_query_valid = odb_addview (session % handle, TRIM (query), abort = .FALSE._odb_int) > 0
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END FUNCTION ops_odb_is_query_valid

#else

FUNCTION ops_odb_is_query_valid (session, &
                                 query)

USE GenMod_Core, ONLY: &
  gen_fail

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Function arguments:
TYPE (ODBSession_type), INTENT(IN) :: session
CHARACTER(len=*), INTENT(IN)       :: query

! Function result:
LOGICAL                            :: ops_odb_is_query_valid

! Local declarations:
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_is_query_valid"

ops_odb_is_query_valid = .FALSE.

CALL gen_fail (routine_name,                                   &
               "ODB support not compiled for this executable")

END FUNCTION ops_odb_is_query_valid

#endif
