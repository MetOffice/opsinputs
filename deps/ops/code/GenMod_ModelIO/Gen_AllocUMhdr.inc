!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Allocate space for a header from a UM dump.
! Extract and set lengths of all arrays, then ALLOCATE the arrays.
!-------------------------------------------------------------------------------

SUBROUTINE Gen_AllocUMhdr (UMhdr)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_UMHeaderConstants, ONLY: &
    FH_ColDepCSize1,                &
    FH_ColDepCSize2,                &
    FH_ColDepCStart,                &
    FH_CompFldI1Size,               &
    FH_CompFldI1Start,              &
    FH_CompFldI2Size,               &
    FH_CompFldI2Start,              &
    FH_CompFldI3Size,               &
    FH_CompFldI3Start,              &
    FH_ExtraCSize,                  &
    FH_ExtraCStart,                 &
    FH_FldsOfCSize1,                &
    FH_FldsOfCSize2,                &
    FH_FldsOfCStart,                &
    FH_IntCSize,                    &
    FH_IntCStart,                   &
    FH_LevDepCSize1,                &
    FH_LevDepCSize2,                &
    FH_LevDepCStart,                &
    FH_LookupSize1,                 &
    FH_LookupSize2,                 &
    FH_LookupStart,                 &
    FH_RealCSize,                   &
    FH_RealCStart,                  &
    FH_RowDepCSize1,                &
    FH_RowDepCSize2,                &
    FH_RowDepCStart

IMPLICIT NONE

! Subroutine arguments:
CLASS (UM_header_type), INTENT(INOUT) :: UMhdr     ! Header from UM dump

! Local declarations:
CHARACTER(len=*), PARAMETER           :: RoutineName = "Gen_AllocUMhdr"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

NULLIFY (UMhdr % IntC)
NULLIFY (UMhdr % RealC)
NULLIFY (UMhdr % LevDepC)
NULLIFY (UMhdr % RowDepC)
NULLIFY (UMhdr % ColDepC)
NULLIFY (UMhdr % FldsOfC)
NULLIFY (UMhdr % ExtraC)
NULLIFY (UMhdr % CompFldI1)
NULLIFY (UMhdr % CompFldI2)
NULLIFY (UMhdr % CompFldI3)
NULLIFY (UMhdr % Lookup)

IF (UMhdr % FixHd(FH_IntCStart) > 0) THEN
  ALLOCATE (UMhdr % IntC(UMhdr % FixHd(FH_IntCSize)))
END IF
IF (UMhdr % FixHd(FH_RealCStart) > 0) THEN
  ALLOCATE (UMhdr % RealC(UMhdr % FixHd(FH_RealCSize)))
END IF
IF (UMhdr % FixHd(FH_LevDepCStart) > 0) THEN
  ALLOCATE (UMhdr % LevDepC(UMhdr % FixHd(FH_LevDepCSize1) * UMhdr % FixHd(FH_LevDepCSize2)))
END IF
IF (UMhdr % FixHd(FH_RowDepCStart) > 0) THEN
  ALLOCATE (UMhdr % RowDepC(UMhdr % FixHd(FH_RowDepCSize1) * UMhdr % FixHd(FH_RowDepCSize2)))
END IF
IF (UMhdr % FixHd(FH_ColDepCStart) > 0) THEN
  ALLOCATE (UMhdr % ColDepC(UMhdr % FixHd(FH_ColDepCSize1) * UMhdr % FixHd(FH_ColDepCSize2)))
END IF
IF (UMhdr % FixHd(FH_FldsOfCStart) > 0) THEN
  ALLOCATE (UMhdr % FldsOfC(UMhdr % FixHd(FH_FldsOfCSize1) * UMhdr % FixHd(FH_FldsOfCSize2)))
END IF
IF (UMhdr % FixHd(FH_ExtraCStart) > 0) THEN
  ALLOCATE (UMhdr % ExtraC(UMhdr % FixHd(FH_ExtraCSize)))
END IF
IF (UMhdr % FixHd(FH_CompFldI1Start) > 0) THEN
  ALLOCATE (UMhdr % CompFldI1(UMhdr % FixHd(FH_CompFldI1Size)))
END IF
IF (UMhdr % FixHd(FH_CompFldI2Start) > 0) THEN
  ALLOCATE (UMhdr % CompFldI2(UMhdr % FixHd(FH_CompFldI2Size)))
END IF
IF (UMhdr % FixHd(FH_CompFldI3Start) > 0) THEN
  ALLOCATE (UMhdr % CompFldI3(UMhdr % FixHd(FH_CompFldI3Size)))
END IF
IF (UMhdr % FixHd(FH_LookupStart) > 0) THEN
  ALLOCATE (UMhdr % Lookup(UMhdr % FixHd(FH_LookupSize1),UMhdr % FixHd(FH_LookupSize2)))
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Gen_AllocUMhdr
