!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Spreads a 1d array of integers in an ob type structure.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_SpreadReal1D (Obs,       &
                             TotalReal, &
                             LocalReal)

USE GenMod_Control, ONLY: &
    nproc,                &
    mype

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE OpsMod_ObsInfo, ONLY: &
    OB_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_type), INTENT(IN)  :: Obs
REAL, INTENT(IN)            :: TotalReal(:)
REAL, INTENT(INOUT)         :: LocalReal(:)

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_SpreadReal1D"
INTEGER                     :: Batch
INTEGER                     :: BatchSize
INTEGER                     :: i
INTEGER                     :: LocalCounter
INTEGER                     :: TotalCounter

IF (UseTrace) CALL gen_trace_entry (RoutineName)

LocalCounter = 0
TotalCounter = 0

IF (gather_by_batch) THEN
  DO batch = 1, Obs % Header % NumCXBatches
    DO i = 0,nproc - 1
      BatchSize = Obs % header % ObsPerBatchPerPE(batch,i)
      IF (mype == i) THEN
        LocalReal(LocalCounter + 1 : LocalCounter + Batchsize) = &
                       TotalReal(TotalCounter + 1:TotalCounter + Batchsize)
        LocalCounter = LocalCounter + Batchsize
      END IF
      TotalCounter = TotalCounter + Batchsize
    END DO
  END DO
ELSE
  DO i = 0,nproc - 1
    BatchSize = Obs % header % ObsPerPE(i)
    IF (mype == i) THEN
      LocalReal(LocalCounter + 1 : LocalCounter + Batchsize) = &
                     TotalReal(TotalCounter + 1:TotalCounter + Batchsize)
      LocalCounter = LocalCounter + Batchsize
    END IF
    TotalCounter = TotalCounter + Batchsize
  END DO
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END  SUBROUTINE Ops_SpreadReal1D
