!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Initialise a CX structure.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CXInit (CX)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE OpsMod_MiscTypes, ONLY: &
    ActionInit

IMPLICIT NONE

! Subroutine arguments:
CLASS (CX_type), INTENT(INOUT) :: CX   ! Model equivalent of OB

! Local declarations:
CHARACTER(len=*), PARAMETER    :: RoutineName = "Ops_CXInit"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL Ops_CXGlobalAction (CX,            &
                         ActionInit)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CXInit
