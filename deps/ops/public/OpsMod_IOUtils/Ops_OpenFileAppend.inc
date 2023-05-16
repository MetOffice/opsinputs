!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Open a text file for appending.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_OpenFileAppend (filename,              &
                               unit,                  &
                               recl,                  &
                               status_if_open_failed)

USE GenMod_Core, ONLY: &
  StatusFatal

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)  :: filename
INTEGER, INTENT(OUT)          :: Unit
INTEGER, OPTIONAL, INTENT(IN) :: recl
INTEGER, OPTIONAL, INTENT(IN) :: status_if_open_failed

! Local declarations:
INTEGER                       :: local_status_if_open_failed
INTEGER                       :: local_recl

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

CALL Ops_OpenFile_inner (filename,                    &
                         Unit,                        &
                         local_recl,                  &
                         local_status_if_open_failed, &
                         "READWRITE",                 &
                         "APPEND",                    &
                         "UNKNOWN",                   &
                         "FORMATTED",                 &
                         "SEQUENTIAL")

END SUBROUTINE Ops_OpenFileAppend
