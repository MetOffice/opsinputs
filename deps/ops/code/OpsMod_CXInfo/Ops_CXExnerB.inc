!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Allocates and calculates pressure on B levels in the CX structure.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CXExnerB(CX)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    PRef,                                &
    Kappa

IMPLICIT NONE

! Subroutine arguments:
TYPE (CX_Type), INTENT(INOUT) :: CX

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName = "Ops_CXExnerB"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (CX % Header % PLevelsB % Present .AND. &
    .NOT. CX % Header % ExnerB % Present) THEN
  ALLOCATE (CX % ExnerB(SIZE (CX % PLevelsB, DIM = 1), SIZE (CX % PLevelsB, DIM = 2)))
  CX % Header % ExnerB = CX % Header % PLevelsB
  CX % ExnerB = (CX % PlevelsB(:,:) /pref) ** kappa
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CXExnerB
