!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Control calculation of Obs Penalty, and its gradient w.r.t. PF.
!
! Force syncronisation of PEs, timing the wait to RoutineName//"_Sync_"//Tag
! This enables the time lost due to load imbalance (e.g. in obs processing
! in Var) to be measured, rather than going arbitrarily to the next
! syncromisation point which might be in an unrelated routine.
! The use of Gen_TraceSync is described in VTDP 20.
!
! A pseudo subroutine name is made up from the RoutineName argument;
! the time spent synchonising will appear against that name in Trace output.
!-------------------------------------------------------------------------------

SUBROUTINE gen_trace_sync (RoutineName, &
                           gid,         &
                           Tag)

USE OpsMod_Gcom, ONLY: &
    gcg_gsync

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)           :: RoutineName
INTEGER, INTENT(IN)                    :: gid
CHARACTER(len=*), INTENT(IN), OPTIONAL :: Tag

! Local declarations:
INTEGER                                :: info
CHARACTER(len=TraceNameLen)            :: LocalRoutineName

IF (.NOT. UseTrace) RETURN

IF (PRESENT (Tag)) THEN
  LocalRoutineName = RoutineName // "_Sync_" // Tag
ELSE
  LocalRoutineName = RoutineName // "_Sync"
END IF

CALL gen_trace_entry (LocalRoutineName)

CALL gcg_gsync (gid,  &
                info)

CALL gen_trace_exit (LocalRoutineName)

END SUBROUTINE gen_trace_sync
