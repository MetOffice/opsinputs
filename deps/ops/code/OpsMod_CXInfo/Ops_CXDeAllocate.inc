!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Deallocate space for CX (ModelColumn)
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CXDeAllocate (CX)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE OpsMod_MiscTypes, ONLY: &
    ActionDealloc

IMPLICIT NONE

! Subroutine arguments:
CLASS (CX_type), INTENT(INOUT) :: CX   ! Model equivalent of OB

! Local declarations:
CHARACTER(len=*), PARAMETER    :: RoutineName = "Ops_CXDeAllocate"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (ASSOCIATED (CX % Header % AK)) DEALLOCATE (CX % Header % AK)
IF (ASSOCIATED (CX % Header % BK)) DEALLOCATE (CX % Header % BK)
IF (ASSOCIATED (CX % Header % Depths)) DEALLOCATE (CX % Header % Depths)
IF (ASSOCIATED (CX % Header % AKH)) DEALLOCATE (CX % Header % AKH)
IF (ASSOCIATED (CX % Header % BKH)) DEALLOCATE (CX % Header % BKH)
IF (ASSOCIATED (CX % Header % EtaTheta)) DEALLOCATE (CX % Header % EtaTheta)
IF (ASSOCIATED (CX % Header % EtaRho)) DEALLOCATE (CX % Header % EtaRho)
IF (ASSOCIATED (CX % Id)) DEALLOCATE (CX % Id)
IF (ASSOCIATED (CX % Header % EtaP)) DEALLOCATE (CX % Header % EtaP)

CALL Ops_CXGlobalAction (CX,            &
                         ActionDealloc)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CXDeAllocate
