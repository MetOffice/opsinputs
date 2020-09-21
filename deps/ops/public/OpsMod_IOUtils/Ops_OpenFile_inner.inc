!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Internal file opening routine.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_OpenFile_inner (filename,              &
                               unit,                  &
                               recl,                  &
                               status_if_open_failed, &
                               action,                &
                               position,              &
                               status,                &
                               form,                  &
                               access)

USE GenMod_Control, ONLY: &
  GeneralMode,            &
  VerboseMode

USE GenMod_Core, ONLY: &
  gen_error_report,    &
  gen_fail,            &
  gen_free_unit,       &
  gen_get_unit,        &
  gen_message,         &
  MaxFileNameLen,      &
  StatusFatal,         &
  StatusOK,            &
  StatusWarning

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE OpsMod_Kinds, ONLY: &
  integer_default

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)       :: filename
INTEGER, INTENT(OUT)               :: Unit
INTEGER, INTENT(IN)                :: recl
INTEGER, INTENT(IN)                :: status_if_open_failed
CHARACTER(len=*), INTENT(IN)       :: action
CHARACTER(len=*), INTENT(IN)       :: position
CHARACTER(len=*), INTENT(IN)       :: status
CHARACTER(len=*), INTENT(IN)       :: form
CHARACTER(len=*), INTENT(IN)       :: access

! Local declarations:
INTEGER(kind=integer_default)      :: open_status
CHARACTER(len=MaxFileNameLen + 32) :: Message
CHARACTER(len=*), PARAMETER        :: routine_name = "Ops_OpenFile_inner"

! Find a free unit
CALL gen_get_unit (Unit)

! Open the file.  Note that specifying RECL with access = "STREAM" is illegal,
! so we have to check the access argument.
!---------------

IF (access == "STREAM") THEN
  OPEN (Unit,                 &
        FILE = filename,      &
        ACTION = action,      &
        ACCESS = access,      &
        STATUS = status,      &
        FORM = "UNFORMATTED", &
        IOSTAT = open_status)
ELSE
  IF (access == "DIRECT") THEN
    OPEN (Unit,                 &
          FILE = filename,      &
          ACTION = action,      &
          ACCESS = access,      &
          STATUS = status,      &
          RECL = recl,          &
          FORM = form,          &
          IOSTAT = open_status)
  ELSE
    OPEN (Unit,                 &
          FILE = filename,      &
          ACTION = action,      &
          ACCESS = access,      &
          POSITION = position,  &
          STATUS = status,      &
          RECL = recl,          &
          FORM = form,          &
          IOSTAT = open_status)
  END IF
END IF

IF (open_status /= 0) THEN
  SELECT CASE (status_if_open_failed)
    CASE (StatusWarning, StatusFatal)
      WRITE (Message, '(A,I0)') 'Failure to open file: ' // TRIM (filename) // " rc = ", open_status
      CALL gen_error_report (routine_name,          &
                             (/Message/),           &
                             status_if_open_failed)
    CASE (StatusOK)
      IF (GeneralMode > VerboseMode) THEN
        WRITE (Message, '(A)') "File " // TRIM (filename) // " could not be opened"
        CALL gen_message (routine_name, &
                          Message)
      END IF
    CASE DEFAULT
      WRITE (Message, '(A,I0)') "Invalid value for status_if_open_failed: ", status_if_open_failed
      CALL gen_fail (routine_name, &
                     Message)
  END SELECT
  CALL gen_free_unit (Unit)
  Unit = IMDI
END IF

END SUBROUTINE Ops_OpenFile_inner
