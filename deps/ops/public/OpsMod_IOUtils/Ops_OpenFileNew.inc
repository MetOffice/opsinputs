!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Open a new text file.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_OpenFileNew (filename,              &
                            unit,                  &
                            recl,                  &
                            status_if_open_failed, &
                            formatted,             &
                            access)

USE GenMod_Core, ONLY: &
  StatusFatal

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)           :: filename
INTEGER, INTENT(OUT)                   :: Unit
INTEGER, OPTIONAL, INTENT(IN)          :: recl
INTEGER, OPTIONAL, INTENT(IN)          :: status_if_open_failed
LOGICAL, OPTIONAL, INTENT(IN)          :: formatted
CHARACTER(len=*), OPTIONAL, INTENT(IN) :: access

! Local declarations:
INTEGER                                :: local_status_if_open_failed
INTEGER                                :: local_recl
CHARACTER(len=20)                      :: local_formatted
CHARACTER(len=20)                      :: local_access

IF (PRESENT (status_if_open_failed)) THEN
  local_status_if_open_failed = status_if_open_failed
ELSE
  local_status_if_open_failed = StatusFatal
END IF

IF (PRESENT (recl)) THEN
  local_recl = recl
ELSE
  local_recl = default_recl
END IF

IF (PRESENT (formatted)) THEN
  IF (formatted) THEN
    local_formatted = "FORMATTED"
  ELSE
    local_formatted = "UNFORMATTED"
  END IF
ELSE
  local_formatted = "FORMATTED"
END IF

IF (PRESENT (access)) THEN
  local_access = access
ELSE
  local_access = 'SEQUENTIAL'
END IF

CALL Ops_OpenFile_inner (filename,                    &
                         Unit,                        &
                         local_recl,                  &
                         local_status_if_open_failed, &
                         "WRITE",                     &
                         "ASIS",                      &
                         "REPLACE",                   &
                         local_formatted,             &
                         local_access)

END SUBROUTINE Ops_OpenFileNew
