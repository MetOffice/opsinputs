!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Rotates Cx winds to produce true wind component values.
!
! Inputs: Cx - Cx structure with wind components wrt model grid.
!
! Outputs: Cx  - Same structure but with wind components wrt true lat-long
!
! Method: - Standard UM rotation routines.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_UnRotateWinds (Cx)

USE GenMod_Core,ONLY: &
    gen_message,      &
    gen_trace_entry,  &
    gen_trace_exit,   &
    UseTrace

USE OpsMod_CXInfo, ONLY: &
    Cx_type

USE OpsMod_ObsInfo, ONLY: &
    Ops_Alloc

USE OpsMod_Utilities, ONLY: &
    Ops_WCoeff,             &
    Ops_WEq_to_ll

IMPLICIT NONE

! Subroutine arguments:
TYPE (Cx_type), INTENT(INOUT) :: Cx          ! Contains data on obs

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName = 'Ops_UnRotateWinds'

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (Cx % Header % u10 % present .AND. Cx % header % u10_orig % present .AND. Cx % header % NumLocal > 0) THEN
  Cx % u10 = Cx % u10_orig
  Cx % v10 = Cx % v10_orig
END IF

IF (Cx % header % u % present .AND. Cx % header % u_orig % present .AND. Cx % header % NumLocal > 0) THEN
  Cx % u = Cx % u_orig
  Cx % v = Cx % v_orig
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END  SUBROUTINE Ops_UnRotateWinds
