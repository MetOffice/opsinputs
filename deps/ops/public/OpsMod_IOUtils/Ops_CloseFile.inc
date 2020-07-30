!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Close a text file.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CloseFile (unit,        &
                          delete_file)

USE GenMod_Core, ONLY: &
  gen_free_unit

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(INOUT)        :: Unit
LOGICAL, OPTIONAL, INTENT(IN) :: delete_file

! Local declarations:
CHARACTER(len=6)              :: local_status

IF (PRESENT (delete_file)) THEN
  IF (delete_file) THEN
    local_status = "DELETE"
  ELSE
    local_status = "KEEP"
  END IF
ELSE
  local_status = "KEEP"
END IF

! Close the file
!---------------

CLOSE (Unit, STATUS = local_status)

CALL gen_free_unit (Unit)

END SUBROUTINE Ops_CloseFile
