!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Sets up the index of observations to print.
!
! Inputs:
!   NumObsLocal: the number of observations on this processor
!
! Outputs:
!   Module array ObPrintIndex, allocated and set up.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_SetObPrintIndex (NumObsLocal)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)         :: NumObsLocal

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_SetObPrintIndex"
INTEGER, PARAMETER          :: DefaultCol = 7   ! default if no list specified
INTEGER                     :: Gap      ! Gap between printed columns
INTEGER                     :: JOb      ! loop count over Ob columns
INTEGER                     :: NumPrint ! Number of ob Columns printed

IF (UseTrace) CALL gen_trace_entry (RoutineName)

NumPrint = MIN (NumObsLocal, DefaultCol)

! setup mask and other work space
ALLOCATE (ObPrintIndex(NumPrint))

Gap = (NumObsLocal - 1) / NumPrint + 1
IF ((1 + (NumPrint - 1) * Gap) > NumObsLocal) THEN
  Gap = 1
END IF
DO Job = 1, NumPrint
  ObPrintIndex(Job) = 1 + (Job - 1) * Gap
END DO

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_SetObPrintIndex
