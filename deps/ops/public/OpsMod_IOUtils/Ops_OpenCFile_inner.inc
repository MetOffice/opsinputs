!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Internal routine for opening a binary file.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_OpenCFile_inner (filename,              &
                                unit,                  &
                                status_if_open_failed, &
                                read_or_write_arg)

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

USE GenMod_Control, ONLY: &
  VerboseMode,            &
  GeneralMode

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE OpsMod_Kinds, ONLY: &
  integer64

USE OpsMod_UMInterface, ONLY: &
  ops_file_open

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)       :: filename
INTEGER, INTENT(OUT)               :: Unit
INTEGER, INTENT(IN)                :: status_if_open_failed
INTEGER, INTENT(IN)                :: read_or_write_arg

! Local declarations:
INTEGER(kind=integer64)            :: open_status
INTEGER(kind=integer64)            :: len_filename
CHARACTER(len=MaxFileNameLen + 32) :: Message
CHARACTER(len=*), PARAMETER        :: RoutineName = "Ops_OpenCFile_inner"
INTEGER(kind=integer64)            :: local_unit
INTEGER(kind=integer64)            :: local_read_or_write_arg

! Find a free unit
CALL gen_get_unit (unit,           &
                   CUnit = .TRUE.)

! Open the file
!---------------

len_filename = LEN_TRIM (filename)

local_unit = unit
local_read_or_write_arg = read_or_write_arg

CALL ops_file_open (local_unit,                                 &
                    filename,                                   &
                    len_filename,                               &
                    local_read_or_write_arg,                    &
                    INT (explicit_file_name, KIND = integer64), &
                    open_status)

IF (open_status /= 0) THEN
  SELECT CASE (status_if_open_failed)
    CASE (StatusWarning, StatusFatal)
      WRITE (Message, '(A,I0)') 'Failure to open file: ' // TRIM (filename) // " rc = ", open_status
      CALL gen_error_report (RoutineName,           &
                             (/Message/),           &
                             status_if_open_failed)
    CASE (StatusOK)
      IF (GeneralMode > VerboseMode) THEN
        WRITE (Message, '(A)') "File " // TRIM (filename) // " could not be opened"
        CALL gen_message (RoutineName, &
                          Message)
      END IF
    CASE DEFAULT
      WRITE (Message, '(A,I0)') "Invalid value for status_if_open_failed: ", status_if_open_failed
      CALL gen_fail (RoutineName, &
                     Message)
  END SELECT
  CALL gen_free_unit (Unit,           &
                      CUnit = .TRUE.)
  Unit = IMDI
END IF

END SUBROUTINE Ops_OpenCFile_inner
