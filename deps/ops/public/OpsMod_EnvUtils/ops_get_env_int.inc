!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Read an integer from an environment variable
!-------------------------------------------------------------------------------

SUBROUTINE ops_get_env_int (environment_variable,       &
                            result,                     &
                            status_if_env_not_set,      &
                            result_if_env_not_set,      &
                            status_if_env_not_readable)

USE GenMod_Control, ONLY: &
  DebugMode,              &
  GeneralMode

USE GenMod_Core, ONLY: &
  gen_error_report,    &
  gen_fail,            &
  StatusFatal,         &
  StatusOK

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE OpsMod_Kinds, ONLY: &
  integer_default

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)  :: environment_variable
INTEGER, INTENT(OUT)          :: result
INTEGER, OPTIONAL, INTENT(IN) :: status_if_env_not_set
INTEGER, OPTIONAL, INTENT(IN) :: result_if_env_not_set
INTEGER, OPTIONAL, INTENT(IN) :: status_if_env_not_readable

! Local declarations:
CHARACTER(len=*), PARAMETER   :: routine_name = "ops_get_env_int"
INTEGER                       :: status
CHARACTER(len=500)            :: error_message
INTEGER                       :: local_status_if_env_not_set
CHARACTER(len=100)            :: result_string
INTEGER                       :: local_result_if_env_not_set
INTEGER                       :: local_status_if_env_not_readable
INTEGER(kind=integer_default) :: io_stat

IF (PRESENT (status_if_env_not_set)) THEN
  local_status_if_env_not_set = status_if_env_not_set
ELSE
  IF (PRESENT (result_if_env_not_set)) THEN
    local_status_if_env_not_set = StatusOK
  ELSE
    local_status_if_env_not_set = StatusFatal
  END IF
END IF

IF (PRESENT (result_if_env_not_set)) THEN
  local_result_if_env_not_set = result_if_env_not_set
ELSE
  local_result_if_env_not_set = IMDI
END IF

IF (PRESENT (status_if_env_not_readable)) THEN
  local_status_if_env_not_readable = status_if_env_not_readable
ELSE
  local_status_if_env_not_readable = StatusFatal
END IF

result_string = ""
status = IMDI

CALL get_environment_variable (environment_variable,  &
                               value = result_string, &
                               status = status)
IF (status /= 0) THEN
  SELECT CASE (status)
    CASE (-1)
      WRITE (error_message, '(A,I0)') "Result variable too short for environment variable " // &
                                      TRIM (environment_variable) // ", result length = ", LEN (result_string)
      CALL gen_fail (routine_name,  &
                     error_message)
    CASE (1)
      WRITE (error_message, '(A,I0)') "Environment variable " // TRIM (environment_variable) // " hasn't been set"
      IF (local_status_if_env_not_set == StatusFatal .OR. GeneralMode >= DebugMode) THEN
        CALL gen_error_report (routine_name,                &
                               (/error_message/),           &
                               local_status_if_env_not_set)
      END IF
    CASE (2)
      WRITE (error_message, '(A,I0)') "Environment variables are not supported on this system"
      CALL gen_fail (routine_name,  &
                     error_message)
    CASE DEFAULT
      WRITE (error_message, '(A,I0)') "Unknown status code from get_environment_variable"
      CALL gen_fail (routine_name,  &
                     error_message)
  END SELECT
  result = local_result_if_env_not_set
ELSE
  READ (result_string, *, IOSTAT = io_stat) result
  IF (io_stat /= 0) THEN
    WRITE (error_message, '(A,I0)') "Unable to read " // TRIM (environment_variable) // ", iostat = ", io_stat
    CALL gen_error_report (routine_name,                     &
                           (/error_message/),                &
                           local_status_if_env_not_readable)
    result = result_if_env_not_set
  END IF
END IF

END SUBROUTINE ops_get_env_int
