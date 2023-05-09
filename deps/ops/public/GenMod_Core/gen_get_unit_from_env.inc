!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Opens a file defined in env_list - a list of environment variables - or uses
! file_name_default if present and available, and all environment variables in
! the list are unset (or the first one is blank.)
! A list of environment variables is used rather than just one so that calling
! routines can maintain backward compatibility for a period of time when
! migrating to new environment names.
! The unit number is assigned to file_unit on success.
! Caller should call Gen_FreeUnit(file_unit) when it is done with the file_unit.
! If no file is located, -1 is assigned to file_unit.
!-------------------------------------------------------------------------------

SUBROUTINE gen_get_unit_from_env (file_unit,         &
                                  env_list,          &
                                  file_name_default)

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(OUT)                   :: file_unit
CHARACTER(len=*), INTENT(IN)           :: env_list(:)
CHARACTER(len=*), INTENT(IN), OPTIONAL :: file_name_default

! Local declarations:
CHARACTER(len=*), PARAMETER            :: THIS = 'gen_get_unit_from_env'
CHARACTER(len=MaxFileNameLen)          :: file_name
INTEGER                                :: i
INTEGER                                :: i_file_name_len
INTEGER                                :: i_status
LOGICAL                                :: is_exist

file_unit = -1
file_name = ''
DO i = 1, SIZE (env_list)
  CALL gen_get_env (env_list(i),     &
                    i_file_name_len, &
                    file_name,       &
                    i_status)
  IF (i_status == 0) EXIT
  IF (i_status == -1) THEN
    CALL gen_fail (THIS,                                             &
                   TRIM (env_list(i)) // ': file name is too long.')
  END IF
END DO

IF (TRIM (file_name) == '' .AND. PRESENT (file_name_default)) THEN
  IF (LEN(file_name) < LEN(file_name_default)) THEN
    CALL gen_fail (THIS,                                                           &
                   TRIM (file_name_default) // ': default file name is too long.')
  END IF
  INQUIRE (FILE = file_name_default, EXIST = is_exist)
  IF (is_exist) file_name = file_name_default
END IF

IF (TRIM (file_name) /= '') THEN
  CALL gen_get_unit (file_unit)
  OPEN (file_unit, FILE = file_name, ACTION = 'READ', STATUS = 'OLD')
END IF

END SUBROUTINE gen_get_unit_from_env
