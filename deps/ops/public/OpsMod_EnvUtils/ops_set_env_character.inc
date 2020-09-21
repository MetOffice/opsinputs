!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Sets an environment variable to a value.  Any environment variable set this
! way will only be effective for the duration of the program; it will not
! persist once the program has ended.
!-------------------------------------------------------------------------------

SUBROUTINE ops_set_env_character (environment_variable, &
                                  value,                &
                                  overwrite)

USE GenMod_Core, ONLY: &
  gen_fail

USE, INTRINSIC :: ISO_C_BINDING, ONLY: &
  C_INT,                               &
  C_NULL_CHAR

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)  :: environment_variable
CHARACTER(len=*), INTENT(IN)  :: value
LOGICAL, OPTIONAL, INTENT(IN) :: overwrite

! Local declarations:
INTEGER                       :: rc
CHARACTER(len=200)            :: error_message
CHARACTER(len=*), PARAMETER   :: RoutineName = "ops_set_env_character"
INTEGER(kind=C_INT)           :: local_overwrite

! Interface blocks:
INTERFACE
  FUNCTION setenv (envname, envval, overwrite) BIND (C,NAME = "setenv")
    USE, INTRINSIC :: ISO_C_BINDING,  ONLY: &
      C_CHAR,                               &
      C_INT
    CHARACTER(kind=C_CHAR)     :: envname(*)
    CHARACTER(kind=C_CHAR)     :: envval(*)
    INTEGER(kind=C_INT), VALUE :: overwrite
    INTEGER(kind=C_INT)        :: setenv
  END FUNCTION setenv
END INTERFACE

IF (PRESENT (overwrite)) THEN
  IF (overwrite) THEN
    local_overwrite = 1
  ELSE
    local_overwrite = 0
  END IF
ELSE
  local_overwrite = 1
END IF

rc = setenv (TRIM (environment_variable) // C_NULL_CHAR, &
             TRIM (value) // C_NULL_CHAR,                &
             local_overwrite)
IF (rc /= 0) THEN
  WRITE (error_message, '(A,I0)') "setenv failed with rc = ", rc
  CALL gen_fail (RoutineName,   &
                 error_message)
END IF

END SUBROUTINE ops_set_env_character
