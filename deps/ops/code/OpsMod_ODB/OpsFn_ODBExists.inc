!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Tests to see whether an ODB exists.
!-------------------------------------------------------------------------------

FUNCTION OpsFn_ODBExists (dbname)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  MaxFileNameLen,      &
  UseTrace

USE OpsMod_EnvUtils, ONLY: &
  ops_get_env

USE OpsMod_Kinds, ONLY: &
  logical_default

IMPLICIT NONE

! Function arguments:
CHARACTER(len=*), INTENT(IN)  :: dbname

! Function result:
LOGICAL                       :: OpsFn_ODBExists

! Local declarations:
CHARACTER(len=MaxFileNameLen) :: path
LOGICAL(kind=logical_default) :: exists
CHARACTER(len=*), PARAMETER   :: RoutineName = "OpsFn_ODBExists"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL ops_get_env ("ODB_DATAPATH_" // TRIM (dbname), &
                  path)

IF (path /= "") THEN
  INQUIRE (FILE = TRIM (path) // "/1/desc", EXIST = exists)
  OpsFn_ODBExists = exists
ELSE
  CALL gen_fail (RoutineName,                                  &
                 "ODB_DATAPATH_" // TRIM (dbname) // " unset")
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END FUNCTION OpsFn_ODBExists
