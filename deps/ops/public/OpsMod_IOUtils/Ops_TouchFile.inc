!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Emulates a "touch" shell command by opening and then closing a file.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_TouchFile (filename,         &
                          status_if_failed)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)  :: filename
INTEGER, OPTIONAL, INTENT(IN) :: status_if_failed

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName = "Ops_TouchFile"
INTEGER                       :: Unit

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL Ops_OpenFileNew (filename,                                 &
                      Unit,                                     &
                      status_if_open_failed = status_if_failed)

CALL Ops_CloseFile (Unit)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_TouchFile
