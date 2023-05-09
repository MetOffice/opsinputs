!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Write out a varobs header.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_WriteVarobsHdr (varobs)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_ModelIO, ONLY:   &
    LenFixHd,               &
    Ops_SetDumpfileAddress

USE GenMod_UMHeaderConstants, ONLY: &
    FH_ColDepCSize1,                &
    FH_ColDepCSize2,                &
    FH_ColDepCStart,                &
    FH_IntCSize,                    &
    FH_IntCStart,                   &
    FH_LevDepCSize1,                &
    FH_LevDepCSize2,                &
    FH_LevDepCStart,                &
    FH_LookupSize1,                 &
    FH_LookupSize2,                 &
    FH_LookupStart,                 &
    FH_RealCSize,                   &
    FH_RealCStart

USE OpsMod_Portio_Wrappers, ONLY: &
    Ops_Buffout_Wrapper,          &
    Ops_Setpos_Wrapper

IMPLICIT NONE

! Subroutine arguments:
CLASS (varobs_type), INTENT(INOUT) :: varobs

! Local declarations:
CHARACTER(len=*), PARAMETER        :: RoutineName = "Ops_WriteVarobsHdr"
INTEGER                            :: WordAddress        ! Position on file, used in ops_setpos

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-------------------------------------------------------------------------------
!  Section 1.   Write out a UM header to a Model dump file
!-------------------------------------------------------------------------------

WordAddress = 0
CALL ops_setpos_wrapper (varobs % unit_num, &
                         WordAddress)

CALL Ops_SetDumpfileAddress (varobs % fixhd,                 &
                             LenFixHd,                       &
                             varobs % Lookup,                &
                             varobs % fixhd(FH_LookupSize1), &
                             varobs % fixhd(FH_LookupSize2))

! fixed length header

CALL Ops_Buffout_Wrapper (varobs % unit_num, &
                          varobs % fixhd,    &
                          LenFixHd)

! integer constants

IF (varobs % fixhd(FH_IntCStart) > 0) THEN
  CALL Ops_Buffout_Wrapper (varobs % unit_num,           &
                            varobs % IntC,               &
                            varobs % fixhd(FH_IntCSize))
END IF

! real constants

IF (varobs % fixhd(FH_RealCStart) > 0) THEN
  CALL Ops_Buffout_Wrapper (varobs % unit_num,            &
                            varobs % RealC,               &
                            varobs % fixhd(FH_RealCSize))
END IF

! level dependent constants

IF (varobs % fixhd(FH_LevDepCStart) > 0) THEN
  CALL Ops_Buffout_Wrapper (varobs % unit_num,                                                 &
                            varobs % LevDepC,                                                  &
                            varobs % fixhd(FH_LevDepCSize1) * varobs % fixhd(FH_LevDepCSize2))
END IF

! column dependent constants

IF (varobs % fixhd(FH_ColDepCStart) > 0) THEN
  CALL Ops_Buffout_Wrapper (varobs % unit_num,                                                 &
                            varobs % ColDepC,                                                  &
                            varobs % fixhd(FH_ColDepCSize1) * varobs % fixhd(FH_ColDepCSize2))
END IF

! lookup table

IF (varobs % fixhd(FH_LookupStart) > 0) THEN
  CALL Ops_Buffout_Wrapper (varobs % unit_num,              &
                            varobs % Lookup,                &
                            varobs % fixhd(FH_LookupSize1), &
                            varobs % fixhd(FH_LookupSize2))
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_WriteVarobsHdr
