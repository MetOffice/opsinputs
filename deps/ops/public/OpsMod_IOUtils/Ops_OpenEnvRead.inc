!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Open a text file specified by an environment variable for reading.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_OpenEnvRead (env_var,               &
                            unit,                  &
                            filename,              &
                            recl,                  &
                            access,                &
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
INTEGER, OPTIONAL, INTENT(IN)          :: recl
CHARACTER(len=*), OPTIONAL, INTENT(IN) :: access
INTEGER, OPTIONAL, INTENT(IN)          :: status_if_env_not_set
INTEGER, OPTIONAL, INTENT(IN)          :: status_if_open_failed

! Local declarations:
INTEGER                                :: local_env_set_status
INTEGER                                :: local_status_if_open_failed
INTEGER                                :: local_recl
CHARACTER(len=MaxFileNameLen)          :: local_filename
CHARACTER(len=12)                      :: local_access

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

IF (PRESENT (recl)) THEN
  local_recl = recl
ELSE
  local_recl = default_recl
END IF

IF (PRESENT (status_if_open_failed)) THEN
  local_status_if_open_failed = status_if_open_failed
ELSE
  local_status_if_open_failed = StatusFatal
END IF

IF (PRESENT (access)) THEN
  local_access = access
ELSE
  local_access = "SEQUENTIAL"
END IF

CALL Ops_OpenEnv_inner (env_var,                     &
                        unit,                        &
                        local_filename,              &
                        local_recl,                  &
                        local_env_set_status,        &
                        local_status_if_open_failed, &
                        "READ",                      &
                        "ASIS",                      &
                        "OLD",                       &
                        local_access)

END SUBROUTINE Ops_OpenEnvRead
