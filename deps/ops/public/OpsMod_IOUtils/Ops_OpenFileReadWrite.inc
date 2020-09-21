!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Open a text file for reading and writing.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_OpenFileReadWrite (filename,              &
                                  unit,                  &
                                  recl,                  &
                                  status_if_open_failed)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  StatusFatal,         &
  UseTrace

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)  :: filename
INTEGER, INTENT(OUT)          :: Unit
INTEGER, OPTIONAL, INTENT(IN) :: recl
INTEGER, OPTIONAL, INTENT(IN) :: status_if_open_failed

! Local declarations:
INTEGER                       :: local_status_if_open_failed
INTEGER                       :: local_recl
CHARACTER(len=*), PARAMETER   :: RoutineName = "Ops_OpenFileReadWrite"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

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
                         "REWIND",                    &
                         "UNKNOWN",                   &
                         "FORMATTED",                 &
                         "SEQUENTIAL")

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_OpenFileReadWrite
