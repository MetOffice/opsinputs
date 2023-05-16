!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Copy an ObHeader_Type from InputHeader to OutputHeader.
!
! Inputs : InputHeader  - Input ObHeader_Type
!
! Outputs: OutputHeader - Output ObHeader_Type
!
! Simple copy for all non-pointer elements.  Pointer elements allocated in
! OutputHeader before explicit copying to avoid hidden aliasing.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CopyObTypeHeader (InputHeader, &
                                 OutputHeader)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

IMPLICIT NONE

! Subroutine arguments:
TYPE (ObHeader_Type), INTENT(IN)  :: InputHeader
TYPE (ObHeader_Type), INTENT(OUT) :: OutputHeader

! Local declarations:
CHARACTER(len=*), PARAMETER       :: RoutineName = "Ops_CopyObTypeHeader"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

OutputHeader = InputHeader
IF (ASSOCIATED (InputHeader % ObsPerBatchPerPE)) THEN
  ALLOCATE (OutputHeader % ObsPerBatchPerPE(SIZE (InputHeader % ObsPerBatchPerPE, DIM = 1), &
                                    0:SIZE (InputHeader % ObsPerBatchPerPE, DIM = 2) - 1))
  OutputHeader % ObsPerBatchPerPE = InputHeader % ObsPerBatchPerPE
END IF

IF (ASSOCIATED (InputHeader % ObsPerPE)) THEN
  ALLOCATE (OutputHeader % ObsPerPE(0:SIZE (InputHeader % ObsPerPE) - 1))
  OutputHeader % ObsPerPE = InputHeader % ObsPerPE
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CopyObTypeHeader
