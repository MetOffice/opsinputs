!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Deallocate space used by a UM dump header.
!-------------------------------------------------------------------------------

SUBROUTINE Gen_DeallocUMhdr (UMhdr)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

IMPLICIT NONE

! Subroutine arguments:
CLASS (UM_header_type), INTENT(INOUT) :: UMhdr  ! Header from UM dump

! Local declarations:
CHARACTER(len=*), PARAMETER           :: RoutineName = "Gen_DeallocUMhdr"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (ASSOCIATED (UMhdr % IntC)) DEALLOCATE (UMhdr % IntC)
IF (ASSOCIATED (UMhdr % RealC)) DEALLOCATE (UMhdr % RealC)
IF (ASSOCIATED (UMhdr % LevDepC)) DEALLOCATE (UMhdr % LevDepC)
IF (ASSOCIATED (UMhdr % RowDepC)) DEALLOCATE (UMhdr % RowDepC)
IF (ASSOCIATED (UMhdr % ColDepC)) DEALLOCATE (UMhdr % ColDepC)
IF (ASSOCIATED (UMhdr % FldsOfC)) DEALLOCATE (UMhdr % FldsOfC)
IF (ASSOCIATED (UMhdr % ExtraC)) DEALLOCATE (UMhdr % ExtraC)
IF (ASSOCIATED (UMhdr % CompFldI1)) DEALLOCATE (UMhdr % CompFldI1)
IF (ASSOCIATED (UMhdr % CompFldI2)) DEALLOCATE (UMhdr % CompFldI2)
IF (ASSOCIATED (UMhdr % CompFldI3)) DEALLOCATE (UMhdr % CompFldI3)
IF (ASSOCIATED (UMhdr % Lookup)) DEALLOCATE (UMhdr % Lookup)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Gen_DeallocUMhdr
