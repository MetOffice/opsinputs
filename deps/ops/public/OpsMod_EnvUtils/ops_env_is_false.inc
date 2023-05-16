!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Reads an environment variable; returne "true" if variable contains a "truthy"
! value, false otherwise.  Truthy means containing upper or lower case Y or T.
!-------------------------------------------------------------------------------

FUNCTION ops_env_is_false (env_var,               &
                           result_if_env_not_set)

USE GenMod_Core, ONLY: &
  MaxFileNameLen,      &
  StatusOK

USE OpsMod_Kinds, ONLY: &
  logical64

IMPLICIT NONE

! Function arguments:
CHARACTER(len=*), INTENT(IN)                  :: env_var
LOGICAL(kind=logical64), OPTIONAL, INTENT(IN) :: result_if_env_not_set

! Function result:
LOGICAL                                       :: ops_env_is_false

! Local declarations:
CHARACTER(len=MaxFileNameLen)                 :: env_val

!----------------------------------
! 0. Tracing and initialisations
!----------------------------------

ops_env_is_false = .FALSE.

CALL ops_get_env (env_var,                           &
                  env_val,                           &
                  status_if_env_not_set = StatusOK,  &
                  status_if_res_too_long = StatusOK)
IF (env_val /= "") THEN
  IF (SCAN (env_val, "NnFf") /= 0) THEN
    ops_env_is_false = .TRUE.
  END IF
ELSE
  IF (PRESENT (result_if_env_not_set)) THEN
    ops_env_is_false = result_if_env_not_set
  END IF
END IF

END FUNCTION ops_env_is_false
