!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Open a new binary file specified by an environment variable.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_OpenCEnvNew (env_var,               &
                            unit,                  &
                            filename,              &
                            status_if_env_not_set, &
                            status_if_open_failed)

USE GenMod_Core, ONLY: &
  MaxFileNameLen,      &
  StatusFatal

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)           :: env_var
INTEGER, INTENT(OUT)                   :: Unit
CHARACTER(len=*), OPTIONAL, INTENT(IN) :: filename
INTEGER, OPTIONAL, INTENT(IN)          :: status_if_env_not_set
INTEGER, OPTIONAL, INTENT(IN)          :: status_if_open_failed

! Local declarations:
INTEGER                                :: local_env_set_status
INTEGER                                :: local_status_if_open_failed
CHARACTER(len=MaxFileNameLen)          :: local_filename

IF (PRESENT (status_if_env_not_set)) THEN
  local_env_set_status = status_if_env_not_set
ELSE
  local_env_set_status = StatusFatal
END IF

IF (PRESENT (filename)) THEN
  local_filename = filename
ELSE
  local_filename = ""
END IF

IF (PRESENT (status_if_open_failed)) THEN
  local_status_if_open_failed = status_if_open_failed
ELSE
  local_status_if_open_failed = StatusFatal
END IF

CALL Ops_OpenCEnv_inner (env_var,                     &
                         unit,                        &
                         local_filename,              &
                         local_env_set_status,        &
                         local_status_if_open_failed, &
                         read_write)

END SUBROUTINE Ops_OpenCEnvNew
