!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Does something with ocean dumps.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_UncompressDump (NoExpRows,  &
                               NoExpCols,  &
                               NoLevs,     &
                               NoSeg,      &
                               LenFld,     &
                               IndxCmp,    &
                               IndxExp,    &
                               IndxToRows, &
                               NoCmp,      &
                               FieldIndex, &
                               LCyc,       &
                               FldCmp,     &
                               FldExp)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  RMDI

USE OpsMod_Kinds, ONLY: &
  real64

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)           :: NoExpRows  ! Length of row dependent constants
INTEGER, INTENT(IN)           :: NoExpCols  ! Length of column dependent constants
INTEGER, INTENT(IN)           :: NoLevs     ! Length of level dependednt constants
INTEGER, INTENT(IN)           :: NoSeg      ! Total number of sea segments in compressed field
INTEGER, INTENT(IN)           :: LenFld     ! Number of sea points on this level
INTEGER, INTENT(IN)           :: IndxCmp(NoSeg) ! Position in compressed array of start of each segment
INTEGER, INTENT(IN)           :: IndxExp(NoSeg) ! Position in expanded array of start of each segment
INTEGER, INTENT(IN)           :: IndxToRows(NoExpRows * NoLevs) ! Contains number of first/next segment for each row/level
INTEGER, INTENT(IN)           :: NoCmp          ! Total no of pts in 3D compressed array
INTEGER, INTENT(IN)           :: FieldIndex     ! Index of field passed in
LOGICAL, INTENT(IN)           :: LCyc           ! T => FldExp cyclic in W-E
REAL(kind=real64), INTENT(IN) :: FldCmp(LenFld) ! Data on compressed grid
REAL, INTENT(INOUT)           :: FldExp(:)      ! Data on expanded grid

! Local declarations:
INTEGER                       :: IJ             ! Index for loop over all pts in field
INTEGER                       :: J              ! Index for loop over rows in field
INTEGER                       :: IncCyc         ! Extra columns on cyclic grid
INTEGER                       :: ISegSt         ! Index for first segment for this field
INTEGER                       :: IstCmpM1       ! Index of 1st pt in compressed field minus 1
INTEGER                       :: IstExpM1       ! Index of 1st pt in expanded field minus 1
INTEGER                       :: JPt            ! Point index number
INTEGER                       :: NoSegRow       ! Number of segments in row
INTEGER                       :: ISeg           ! Index for loop over segments in row
INTEGER                       :: IPtSeg         ! Segment index number
INTEGER                       :: LenSeg         ! Number of grid points in current segment
INTEGER                       :: IPtExp         ! Pointer to location in 3d expanded field
INTEGER                       :: IPtExpCyc      ! Pointer to location in 2d expanded field
INTEGER                       :: IPtCmp         ! Pointer to location in compressed field
INTEGER                       :: IncRow         ! No of pts in cyclic grid before this row
INTEGER                       :: ICount         ! Index for loop over pts in segment
CHARACTER(len=*), PARAMETER   :: RoutineName = "Ops_UncompressDump"

!----------------------------------
! 0. Tracing and initialisations
!----------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! 1. Preliminaries

DO IJ = 1, NoExpCols * NoExpRows
  FldExp(IJ) = RMDI
END DO

! 1.1 Set the number of extra columns for cyclic overlap

IF (LCyc) THEN
  IncCyc = 2
ELSE
  IncCyc = 0
END IF

! 1.2 Set offsets for compressed and expanded grids for this level

ISegSt = IndxToRows(NoExpRows * (FieldIndex - 1) + 1)
IstCmpM1 = IndxCmp(IsegSt) - 1
IstExpM1 = (FieldIndex - 1) * NoExpRows * (NoExpCols - IncCyc)

! 2. Loop over rows (index J)

! 2.1 Start loop over rows and define the pointer to the row

DO J  = 1, NoExpRows
  JPt = (FieldIndex - 1) * NoExpRows + J

  ! 2.2 Calculate the number of sea segments in the row

  IF (JPt == NoLevs * NoExpRows) THEN
    NoSegRow = NoSeg - IndxToRows(JPt) + 1
  ELSE
    NoSegRow = IndxToRows(JPt + 1) - IndxToRows(JPt)
  END IF

  ! 2.3 Start loop over sea segments and define pointer to segment
  DO ISeg = 1, NoSegRow
    IPtSeg = IndxToRows(JPt) + ISeg - 1

    ! 2.4 Calculate the length of the present sea segment

    IF (IPtSeg < NoSeg) THEN
      LenSeg = IndxCmp(IPtSeg + 1) - IndxCmp(IPtSeg)
    ELSE
      LenSeg = NoCmp - IndxCmp(IPtSeg) + 1
    END IF

    ! 2.5 Calculate FldExp for all points in the segment

    DO ICount = 1, LenSeg
      IPtExp = IndxExp(IPtSeg) + ICount - 1
      IPtExpCyc = IPtExp - IStExpM1 + IncCyc * (J - 1)
      IPtCmp = IndxCmp(IPtSeg) + ICount - 1
      FldExp(IPtExpCyc) = FldCmp(IPtCmp - IStCmpM1)
    END DO   !  index ICOUNT

  END DO ! index ISEG

END DO  ! index J

!  End loop over rows

! 3. Put in cyclic points if necessary

IF (LCyc)THEN
   DO J = 1, NoExpRows
     IncRow = (J - 1) * NoExpCols
     FldExp(IncRow + NoExpCols - 1) = FldExp(IncRow + 1)
     FldExp(IncRow + NoExpCols) = FldExp(IncRow + 2)
   END DO
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_UncompressDump
