!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Spread a varobs header between processors.
! Broadcast all parts from PE 0 to all others.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_SpreadVarobsHdr (varobs)

USE GenMod_Control, ONLY: &
    mype,                 &
    GlobalGroup

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_ModelIO, ONLY: &
    LenFixHd

USE GenMod_UMHeaderConstants, ONLY: &
    FH_ColDepCSize1,                &
    FH_ColDepCSize2,                &
    FH_IntCSize,                    &
    FH_LevDepCSize1,                &
    FH_LevDepCSize2,                &
    FH_LookupSize1,                 &
    FH_LookupSize2,                 &
    FH_RealCSize

USE OpsMod_Gcom, ONLY: &
    gcg_ibcast,        &
    gcg_rbcast

IMPLICIT NONE

! Subroutine arguments:
CLASS (varobs_type), INTENT(INOUT) :: varobs   ! varobs header

! Local declarations:
CHARACTER(len=*), PARAMETER        :: RoutineName = "Ops_SpreadVarobsHdr"
INTEGER                            :: istat

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-------------------------------------------------------------------------------
! Section 1.    Allocate & read in Fixed-Length Header from Model dump file
!-------------------------------------------------------------------------------

CALL gcg_ibcast (0,              &
                 LenFixHd,       &
                 0,              &
                 GlobalGroup,    &
                 istat,          &
                 varobs % FixHd)

IF (mype /= 0) THEN
  CALL varobs % alloc
END IF

CALL gcg_ibcast (1,                           &
                 varobs % FixHd(FH_IntCSize), &
                 0,                           &
                 GlobalGroup,                 &
                 istat,                       &
                 varobs % IntC)

CALL gcg_rbcast (2,                            &
                 varobs % FixHd(FH_RealCSize), &
                 0,                            &
                 GlobalGroup,                  &
                 istat,                        &
                 varobs % RealC)

CALL gcg_rbcast (3,                                                                 &
                 varobs % FixHd(FH_LevDepCSize1) * varobs % FixHd(FH_LevDepCSize2), &
                 0,                                                                 &
                 GlobalGroup,                                                       &
                 istat,                                                             &
                 varobs % LevDepC)

CALL gcg_rbcast (5,                                                                 &
                 varobs % FixHd(FH_ColDepCSize1) * varobs % FixHd(FH_ColDepCSize2), &
                 0,                                                                 &
                 GlobalGroup,                                                       &
                 istat,                                                             &
                 varobs % ColDepC)

CALL gcg_ibcast (8,                                                               &
                 varobs % FixHd(FH_LookupSize1) * varobs % FixHd(FH_LookupSize2), &
                 0,                                                               &
                 GlobalGroup,                                                     &
                 istat,                                                           &
                 varobs % Lookup)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_SpreadVarobsHdr
