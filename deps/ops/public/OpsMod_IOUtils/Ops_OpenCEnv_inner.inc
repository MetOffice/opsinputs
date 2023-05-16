!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Internal routine for opening a binary file specified by an environment
! variable.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_OpenCEnv_inner (env_var,               &
                               unit,                  &
                               filename,              &
                               status_if_env_not_set, &
                               status_if_open_failed, &
                               read_or_write_arg)

USE GenMod_Core, ONLY: &
  gen_error_report,    &
  gen_fail,            &
  gen_message,         &
  MaxFileNameLen,      &
  StatusFatal,         &
  StatusOK,            &
  StatusWarning

USE GenMod_Control, ONLY: &
  VerboseMode,            &
  GeneralMode

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE OpsMod_EnvUtils, ONLY: &
  ops_get_env

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)  :: env_var
INTEGER, INTENT(OUT)          :: Unit
CHARACTER(len=*), INTENT(IN)  :: filename
INTEGER, INTENT(IN)           :: status_if_env_not_set
INTEGER, INTENT(IN)           :: status_if_open_failed
INTEGER, INTENT(IN)           :: read_or_write_arg

! Local declarations:
INTEGER                       :: get_env_rc
CHARACTER(len=MaxFileNameLen) :: result
CHARACTER(len=200)            :: Message
CHARACTER(len=*), PARAMETER   :: RoutineName = "Ops_OpenCEnv_inner"

CALL ops_get_env (env_var,                                &
                  result,                                 &
                  status_if_env_not_set = StatusOK,       &
                  status_if_res_too_long = StatusWarning, &
                  get_env_return_code = get_env_rc)

IF (result == "") THEN
  SELECT CASE (status_if_env_not_set)
    CASE (StatusFatal, StatusWarning)
      WRITE (Message, '(A,I0)') "Unable to read " // TRIM (env_var) // ", ops_get_env rc = ", get_env_rc
      CALL gen_error_report (RoutineName,           &
                             (/Message/),           &
                             status_if_env_not_set)
    CASE (StatusOK)
      IF (GeneralMode >= VerboseMode) THEN
        WRITE (Message, '(A)') TRIM (env_var) // " not set"
        CALL gen_message (RoutineName, &
                          Message)
      END IF
    CASE DEFAULT
      WRITE (Message, '(A,I0)') "Invalid value for env set status: ", status_if_env_not_set
      CALL gen_fail (RoutineName, &
                     Message)
  END SELECT
  Unit = IMDI
ELSE
  IF (filename /= "") result = TRIM (result) // "/" // TRIM (filename)
  CALL Ops_OpenCFile_inner (result,                &
                            Unit,                  &
                            status_if_open_failed, &
                            read_or_write_arg)
END IF

END SUBROUTINE Ops_OpenCEnv_inner
