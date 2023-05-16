!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Read an character string from an environment variable
!-------------------------------------------------------------------------------

SUBROUTINE ops_get_env_character (environment_variable,   &
                                  result,                 &
                                  status_if_env_not_set,  &
                                  status_if_res_too_long, &
                                  result_if_env_not_set,  &
                                  get_env_return_code)

USE GenMod_Control, ONLY: &
  DebugMode,              &
  GeneralMode

USE GenMod_Core, ONLY: &
  gen_error_report,    &
  gen_fail,            &
  StatusFatal,         &
  StatusOK,            &
  StatusWarning

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)           :: environment_variable
CHARACTER(len=*), INTENT(OUT)          :: result
INTEGER, OPTIONAL, INTENT(IN)          :: status_if_env_not_set
INTEGER, OPTIONAL, INTENT(IN)          :: status_if_res_too_long
CHARACTER(len=*), OPTIONAL, INTENT(IN) :: result_if_env_not_set
INTEGER, OPTIONAL, INTENT(OUT)         :: get_env_return_code

! Local declarations:
CHARACTER(len=*), PARAMETER            :: routine_name = "ops_get_env_character"
INTEGER                                :: status
CHARACTER(len=500)                     :: error_message(2)
INTEGER                                :: local_status_if_env_not_set
INTEGER                                :: local_status_if_res_too_long
CHARACTER(len=5000)                    :: local_result_if_env_not_set

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
  local_result_if_env_not_set = ""
END IF

IF (PRESENT (status_if_res_too_long)) THEN
  local_status_if_res_too_long = status_if_res_too_long
ELSE
  local_status_if_res_too_long = StatusFatal
END IF

result = ""
status = IMDI

CALL get_environment_variable (environment_variable, &
                               value = result,       &
                               status = status)

IF (status /= 0) THEN
  SELECT CASE (status)
    CASE (-1)

      IF (local_status_if_res_too_long == StatusFatal .OR. &
          local_status_if_res_too_long == StatusWarning .OR. &
          GeneralMode >= DebugMode) THEN
        WRITE (error_message(1), '(A,I0)') "Result variable too short for environment variable " // &
                                           TRIM (environment_variable) // ", result length = ", LEN (result)
        IF (local_status_if_res_too_long == StatusFatal) THEN
          CALL gen_fail (routine_name,     &
                         error_message(1))
        ELSE
          WRITE (error_message(2), '(A)') "Result has been truncated"
          CALL gen_error_report (routine_name,                 &
                                 error_message(1:2),           &
                                 local_status_if_res_too_long)
        END IF
      END IF
    CASE (1)

      WRITE (error_message(1), '(A,I0)') "Environment variable " // TRIM (environment_variable) // " hasn't been set"
      IF (local_status_if_env_not_set == StatusFatal) THEN
        CALL gen_error_report (routine_name,                &
                               error_message(1:1),          &
                               local_status_if_env_not_set)
      ELSE IF (local_status_if_env_not_set == StatusWarning .OR. &
               GeneralMode >= DebugMode) THEN
        WRITE (error_message(2), '(2A)') "Using default value: ", TRIM (local_result_if_env_not_set)
        CALL gen_error_report (routine_name,                &
                               error_message(1:2),          &
                               local_status_if_env_not_set)
      END IF
    CASE (2)

      WRITE (error_message(1), '(A,I0)') "Environment variables are not supported on this system"
      CALL gen_fail (routine_name,     &
                     error_message(1))
    CASE DEFAULT
      WRITE (error_message(1), '(A,I0)') "Unknown status code from get_environment_variable"
      CALL gen_fail (routine_name,     &
                     error_message(1))
  END SELECT

  result = local_result_if_env_not_set
END IF

IF (PRESENT (get_env_return_code)) THEN
  get_env_return_code = status
END IF

END SUBROUTINE ops_get_env_character
