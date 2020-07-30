!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Read an array of character strings from an environment variable
!-------------------------------------------------------------------------------

SUBROUTINE ops_get_env_character_array (environment_variable,  &
                                        result,                &
                                        status_if_env_not_set)

USE GenMod_Core, ONLY: &
  Gen_StringSplit

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)  :: environment_variable
CHARACTER(len=*), POINTER     :: result(:)
INTEGER, OPTIONAL, INTENT(IN) :: status_if_env_not_set

! Local declarations:
CHARACTER(len=16000)          :: local_result

NULLIFY (result)

CALL ops_get_env_character (environment_variable,                          &
                            local_result,                                  &
                            status_if_env_not_set = status_if_env_not_set)

IF (local_result /= "") THEN
  CALL Gen_StringSplit (local_result, &
                        ' ',          &
                        result)
END IF

END SUBROUTINE ops_get_env_character_array
