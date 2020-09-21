!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Returns true if an environment variable has been set, false otherwise.
!-------------------------------------------------------------------------------

FUNCTION ops_env_is_set (env_var)

USE GenMod_Core, ONLY: &
  MaxFileNameLen,      &
  StatusOK

IMPLICIT NONE

! Function arguments:
CHARACTER(len=*), INTENT(IN)  :: env_var

! Function result:
LOGICAL                       :: ops_env_is_set

! Local declarations:
CHARACTER(len=MaxFileNameLen) :: env_val
INTEGER                       :: rc

!----------------------------------
! 0. Tracing and initialisations
!----------------------------------

ops_env_is_set = .FALSE.

CALL ops_get_env (env_var,                           &
                  env_val,                           &
                  status_if_env_not_set = StatusOK,  &
                  status_if_res_too_long = StatusOK, &
                  get_env_return_code = rc)

ops_env_is_set = rc /= 1

END FUNCTION ops_env_is_set
