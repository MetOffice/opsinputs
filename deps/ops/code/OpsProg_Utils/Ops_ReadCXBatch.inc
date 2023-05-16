!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Reads a single batch from a CX file.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ReadCXBatch (Header,     &
                            BatchNum,   &
                            BatchOfObs)

USE GenMod_CLookAdd, ONLY: &
  LBNPT,                   &
  LBROW

USE GenMod_MiscUMScienceConstants, ONLY: &
  RMDI

USE GenMod_ModelIO, ONLY: &
  UM_Header_Type

IMPLICIT NONE

! Subroutine arguments:
TYPE (UM_Header_Type), INTENT(IN) :: Header
INTEGER, INTENT(IN)               :: BatchNum
REAL, POINTER                     :: BatchOfObs(:,:)

! Local declarations:
INTEGER                           :: NumCols
INTEGER                           :: LenCol
REAL, ALLOCATABLE                 :: DummyArray(:)

NumCols = Header % Lookup(LBNPT, BatchNum)
LenCol = Header % Lookup(LBROW, BatchNum)

ALLOCATE (BatchOfObs(LenCol, NumCols))

IF (LenCol * NumCols > 0) THEN
  ALLOCATE (DummyArray(LenCol * NumCols))
  DummyArray = RMDI
  CALL Header % read_field (BatchNum,   &
                            DummyArray)
  BatchOfObs = RESHAPE (DummyArray, (/LenCol,NumCols/))
  DEALLOCATE (DummyArray)
END IF

END SUBROUTINE Ops_ReadCXBatch
