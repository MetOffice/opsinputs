!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Open a text file specified by an environment variable for reading.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_OpenPathRead (env_var,               &
                             unit,                  &
                             filename,              &
                             recl,                  &
                             status_if_env_not_set, &
                             status_if_open_failed)

USE GenMod_Control, ONLY: &
  GeneralMode,            &
  VerboseMode

USE GenMod_Core, ONLY: &
  gen_error_report,    &
  gen_fail,            &
  gen_message,         &
  Gen_StringSplit,     &
  MaxFileNameLen,      &
  StatusFatal,         &
  StatusOK,            &
  StatusWarning

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE OpsMod_EnvUtils, ONLY: &
  ops_get_env

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)       :: env_var
INTEGER, INTENT(OUT)               :: Unit
CHARACTER(len=*), INTENT(IN)       :: filename
INTEGER, OPTIONAL, INTENT(IN)      :: recl
INTEGER, OPTIONAL, INTENT(IN)      :: status_if_env_not_set
INTEGER, OPTIONAL, INTENT(IN)      :: status_if_open_failed

! Local declarations:
INTEGER                            :: local_status_if_open_failed
INTEGER                            :: local_recl
CHARACTER(len=10000)               :: path
CHARACTER(len=10000), POINTER      :: paths(:)
INTEGER                            :: i
CHARACTER(len=MaxFileNameLen + 32) :: Message
CHARACTER(len=*), PARAMETER        :: routine_name = "Ops_OpenPathRead"

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

CALL ops_get_env (env_var,                                       &
                  path,                                          &
                  status_if_env_not_set = status_if_env_not_set)

CALL Gen_StringSplit (path,  &
                      ":",   &
                      paths)

unit = IMDI
DO i = 1, SIZE (paths)
  CALL Ops_OpenFile_inner (TRIM (paths(i)) // "/" // TRIM (filename), &
                           unit,                                      &
                           local_recl,                                &
                           StatusOK,                                  &
                           "READ",                                    &
                           "REWIND",                                  &
                           "OLD",                                     &
                           "FORMATTED",                               &
                           "SEQUENTIAL")
  IF (unit > 0) THEN
    IF (GeneralMode >= VerboseMode) THEN
      WRITE (Message, '(A)') "Filename " // TRIM (filename) // " has been found in directory " // TRIM (paths(i))
      CALL gen_message (routine_name, &
                        Message)
    END IF
    EXIT
  END IF
END DO

IF (unit <= 0) THEN
  SELECT CASE (local_status_if_open_failed)
    CASE (StatusWarning, StatusFatal)
      WRITE (Message, '(A)') 'Failure to open file: ' // TRIM (filename) // " in any directory."
      CALL gen_error_report (routine_name,                &
                             (/Message/),                 &
                             local_status_if_open_failed)
    CASE (StatusOK)
      IF (GeneralMode >= VerboseMode) THEN
        WRITE (Message, '(A)') "File " // TRIM (filename) // " could not be opened"
        CALL gen_message (routine_name, &
                          Message)
      END IF
    CASE DEFAULT
      WRITE (Message, '(A,I0)') "Invalid value for status_if_open_failed: ", local_status_if_open_failed
      CALL gen_fail (routine_name, &
                     Message)
  END SELECT
END IF

DEALLOCATE (paths)

END SUBROUTINE Ops_OpenPathRead
