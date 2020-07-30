!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Remove a lock directory in the ODB directory to allow reads by OPS again.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_UnlockODB

USE GenMod_Control, ONLY: &
  mype

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  MaxFileNameLen,      &
  UseTrace

USE OpsMod_DirectoryUtils, ONLY: &
  Ops_RemoveDirectory

USE OpsMod_EnvUtils, ONLY: &
  ops_get_env

IMPLICIT NONE

! Local declarations:
CHARACTER(len=MaxFileNameLen) :: odb_src_path
CHARACTER(len=*), PARAMETER   :: RoutineName = "Ops_UnlockODB"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (mype == 0) THEN
  CALL ops_get_env ("ODB_SRCPATH_ECMA", &
                    odb_src_path)

  CALL Ops_RemoveDirectory (TRIM (odb_src_path) // "/" // ".lock")
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_UnlockODB
