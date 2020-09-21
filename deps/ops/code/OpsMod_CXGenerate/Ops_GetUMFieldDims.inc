!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Gets the dimensions from one level of one field from a UM dump
!
! - The size of the field is determined.
!
! Inputs:
!     UMHdr: UM Header
!     StashCode: The stashcode of the required field in dump.
!     Level: The level of the field required.
!     Time: The forecast time of the field required.
!     Vector: Optional logical indicating whether field is
!       a component of a vector field.
!     ModelTimeUTC: Validity time UTC in the UM dump.
!     AddBLField: Optional Integer offset for Boundary Layer (I think?)
!     extend: Optional logical defines if the grid is extend
!
! Outputs:
!     FieldDims: Integer array with two elements [PointsInRow, PointsInCol]
!-------------------------------------------------------------------------------

SUBROUTINE Ops_GetUMFieldDims (FieldDims,    &
                               UMHdr,        &
                               StashCode,    &
                               Level,        &
                               Time,         &
                               ModelTimeUTC, &
                               AddBLField,   &
                               extend)

USE GenMod_CLookAdd, ONLY: &
  LBROW,                   &
  LBNPT,                   &
  LBFT,                    &
  ITEM_CODE

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  gen_warn,            &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE GenMod_ModelIO, ONLY: &
  UM_header_type

USE GenMod_UMHeaderConstants, ONLY: &
  FH_ColDepCSize1,                  &
  FH_RowDepCSize1,                  &
  FH_RowDepCStart,                  &
  FH_SubModel,                      &
  FH_SubModel_Ocean,                &
  FH_HorizGrid_Global,              &
  FH_HorizGrid,                     &
  FH_GridStagger,                   &
  FH_GridStagger_EndGame

USE OpsMod_HorizontalInterp, ONLY: &
  num_extend_points

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(OUT)              :: FieldDims(2)
TYPE (UM_Header_Type), INTENT(IN) :: UMHdr
INTEGER, INTENT(IN)               :: StashCode
INTEGER, OPTIONAL, INTENT(IN)     :: Level
INTEGER, OPTIONAL, INTENT(IN)     :: Time
INTEGER, OPTIONAL, INTENT(IN)     :: ModelTimeUTC(:)
INTEGER, OPTIONAL, INTENT(IN)     :: AddBLField   ! is THIS 2nd boundary level pressure
LOGICAL, OPTIONAL, INTENT(IN)     :: extend

! Local declarations:
INTEGER                           :: PointsInRow
INTEGER                           :: PointsInCol
INTEGER                           :: iField
INTEGER                           :: Field
INTEGER                           :: NLev
LOGICAL                           :: LOcean
LOGICAL                           :: LGlobal
LOGICAL                           :: LEndGame
LOGICAL                           :: extend_local
INTEGER                           :: level_local
CHARACTER(len=80)                 :: ErrMess
CHARACTER(len=*), PARAMETER       :: RoutineName = 'Ops_GetUMFieldDims'

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (PRESENT (extend)) THEN
  extend_local = extend
ELSE
  extend_local = .TRUE.
END IF

Field = IMDI
NLev = 0

IF (PRESENT (Time)) THEN
  DO iField = 1, UMHdr % NumFlds
    IF (UMHdr % LookUp(ITEM_CODE,iField) == StashCode .AND. &
        (ModelTimeUTC(iField) == Time .OR. UMHdr % Lookup(LBFT,iField) == Time)) THEN
      IF (NLev == 0) Field = iField
      NLev = NLev + 1
    END IF
  END DO
ELSE
  DO iField = 1, UMHdr % NumFlds
    IF (UMHdr % LookUp(ITEM_CODE,iField) == StashCode) THEN
      Field = iField
      EXIT
    END IF
  END DO
END IF

! Add 1 for reading a BL" pressure field
IF (PRESENT (AddBLField) .AND. Field /= IMDI) Field = Field + AddBLField

IF (Field == IMDI) THEN
  WRITE (ErrMess, '(A,I0,A)') 'Field with stashcode ', StashCode, &
    ' not available for reading from dump.'
  CALL gen_fail (RoutineName, &
                 ErrMess)
END IF

IF (PRESENT (level)) THEN
  IF (Level > NLev) THEN
    WRITE (ErrMess, '(A,I0,A,I0,A,I0,A)') 'Level ', Level, &
      ' of field ', StashCode, ' requested, when only ', NLev, ' levels available.'
    CALL gen_warn (RoutineName, &
                   ErrMess)
    GOTO 9999
  END IF
  level_local = level
ELSE
  level_local = 1
END IF


LOcean = UMHdr % FixHd(FH_SubModel) == FH_SubModel_Ocean
LGlobal = UMHdr % FixHd(FH_HorizGrid) == FH_HorizGrid_Global
LEndGame = UMHdr % FixHd(FH_GridStagger) == FH_GridStagger_EndGame

! Work out the size of the array
IF ((LOcean .OR. UMhdr % FixHd(FH_RowDepCStart) > 0) .AND. &
    .NOT. (LEndGame .AND. .NOT. LGlobal)) THEN
  PointsInRow = UMHdr % FixHd(FH_ColDepCSize1)
  PointsInCol = UMHdr % FixHd(FH_RowDepCSize1)
ELSE
  PointsInRow = UMHdr % LookUp(LBNPT,Field + Level_local - 1)
  PointsInCol = UMHdr % LookUp(LBROW,Field + Level_local - 1)
END IF

IF (extend_local) THEN
  PointsInRow = PointsInRow + 2 * num_extend_points
  PointsInCol = PointsInCol + 2 * num_extend_points
END IF

! Below is not required for fields
FieldDims = [PointsInRow,PointsInCol]

9999 CONTINUE

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_GetUMFieldDims
