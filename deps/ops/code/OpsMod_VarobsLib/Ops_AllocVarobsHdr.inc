!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Allocate space for a varobs header.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_AllocVarobsHdr (varobs)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_UMHeaderConstants, ONLY: &
    FH_ColDepCSize1,                &
    FH_ColDepCSize2,                &
    FH_IntCSize,                    &
    FH_LevDepCSize1,                &
    FH_LevDepCSize2,                &
    FH_LookupSize1,                 &
    FH_LookupSize2,                 &
    FH_RealCSize

IMPLICIT NONE

! Subroutine arguments:
CLASS (varobs_type), INTENT(INOUT) :: varobs

! Local declarations:
CHARACTER(len=*), PARAMETER        :: RoutineName = "Ops_AllocVarobsHdr"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

ALLOCATE (varobs % IntC(varobs % fixhd(FH_IntCSize)))
ALLOCATE (varobs % RealC(varobs % fixhd(FH_RealCSize)))
ALLOCATE (varobs % LevDepC(varobs % fixhd(FH_LevDepCSize1) * varobs % fixhd(FH_LevDepCSize2)))
ALLOCATE (varobs % ColDepC(varobs % fixhd(FH_ColDepCSize1) * varobs % fixhd(FH_ColDepCSize2)))
ALLOCATE (varobs % Lookup(varobs % fixhd(FH_LookupSize1),varobs % fixhd(FH_LookupSize2)))

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_AllocVarobsHdr
