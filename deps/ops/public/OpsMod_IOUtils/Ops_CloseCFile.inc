!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Close a binary file.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CloseCFile (unit,     &
                           filename)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_free_unit,       &
  MaxFileNameLen

USE OpsMod_Kinds, ONLY: &
  integer64

USE OpsMod_UMInterface, ONLY: &
  ops_file_close

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(INOUT)                 :: Unit
CHARACTER(len=*), OPTIONAL, INTENT(IN) :: filename

! Local declarations:
INTEGER(kind=integer64)                :: close_status
CHARACTER(len=*), PARAMETER            :: RoutineName = "Ops_CloseCFile"
CHARACTER(len=100)                     :: ErrorMessage
CHARACTER(len=MaxFileNameLen)          :: local_filename
INTEGER(kind=integer64)                :: len_filename

! Close the file
!---------------
IF (PRESENT (filename)) THEN
  local_filename = filename
ELSE
  local_filename = ""
END IF

len_filename = LEN_TRIM (local_filename)

! Unfortunately ops_file_close does not always set this, so it needs to be initialised.
close_status = 0

CALL ops_file_close (INT (Unit, KIND = integer64),               &
                     local_filename,                             &
                     len_filename,                               &
                     INT (explicit_file_name, KIND = integer64), &
                     INT (delete_file_no, KIND = integer64),     &
                     close_status)
IF (close_status /= 0) THEN
  WRITE (ErrorMessage, '(A,I0)') "Error in ops_file_close: ", close_status
  CALL gen_fail (RoutineName,  &
                 ErrorMessage)
END IF

CALL gen_free_unit (Unit,           &
                    CUnit = .TRUE.)

END SUBROUTINE Ops_CloseCFile
