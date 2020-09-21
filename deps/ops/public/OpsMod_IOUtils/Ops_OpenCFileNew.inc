!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Open a new binary file.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_OpenCFileNew (filename,              &
                             unit,                  &
                             status_if_open_failed)

USE GenMod_Core, ONLY: &
  StatusFatal

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)  :: filename
INTEGER, INTENT(OUT)          :: Unit
INTEGER, OPTIONAL, INTENT(IN) :: status_if_open_failed

! Local declarations:
INTEGER                       :: local_status_if_open_failed

IF (PRESENT (status_if_open_failed)) THEN
  local_status_if_open_failed = status_if_open_failed
ELSE
  local_status_if_open_failed = StatusFatal
END IF

CALL Ops_OpenCFile_inner (filename,                    &
                          Unit,                        &
                          local_status_if_open_failed, &
                          read_write)

END SUBROUTINE Ops_OpenCFileNew
