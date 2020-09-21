!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Closes an ODB.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CloseODB (h,    &
                         save)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE ops_odb_wrappers, ONLY: &
  ops_odb_close

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(INOUT) :: h
LOGICAL, OPTIONAL, INTENT(IN)         :: save

! Local declarations:
CHARACTER(len=*), PARAMETER           :: RoutineName = "Ops_CloseODB"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL ops_odb_close (h,    &
                    save)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CloseODB
