!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Place a lock directory in the ODB directory to prevent concurrent reads by OPS
! or reading of corrupted ODB.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_LockODB

USE GenMod_Control, ONLY: &
  mype

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  MaxFileNameLen,      &
  UseTrace

USE OpsMod_DirectoryUtils, ONLY: &
  Ops_MakeDirectory,             &
  OpsFn_DirectoryExists

USE OpsMod_EnvUtils, ONLY: &
  ops_get_env

IMPLICIT NONE

! Local declarations:
CHARACTER(len=MaxFileNameLen)      :: odb_src_path
CHARACTER(len=MaxFileNameLen + 10) :: message
CHARACTER(len=*), PARAMETER        :: RoutineName = "Ops_LockODB"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (mype == 0) THEN
  CALL ops_get_env ("ODB_SRCPATH_ECMA", &
                    odb_src_path)
  IF (OpsFn_DirectoryExists (TRIM (odb_src_path) // "/" // ".lock")) THEN
    message = TRIM (odb_src_path) // " is locked"
    CALL gen_fail (RoutineName, &
                   message)
  END IF

  CALL Ops_MakeDirectory (TRIM (odb_src_path) // "/" // ".lock")
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_LockODB
