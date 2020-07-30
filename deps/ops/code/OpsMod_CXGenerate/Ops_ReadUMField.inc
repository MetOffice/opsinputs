!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Reads one level of one field from a UM dump into a 2-d array.
!
! - Gen_ReadField is called returning a 1-d array.
! - There are two options: i) read and bcast or ii) read on all PE's. This is
!   controlled with optional logical varible
! - The data is uncompressed if necessary (ocean data),
!       using Ops_UncompressDump.
! - The array is reshaped to 2-d, and extended using
!       Ops_HorizontalExtend.
!
! Inputs:
!     UMHdr: UM Header
!     StashCode: The stashcode of the required field in dump.
!     Level: The level of the field required.
!     Time: The forecast time of the field required.
!     ModelTimeUTC: Validity time UTC in the UM dump.
!     Vector: Optional logical indicating whether field is
!       a component of a vector field.
!     IntField: Optional logical indicating whether this field
!       is stored as an integer in the dump.
!     AddBLField: Optional Integer offset for Boundary Layer (I think?)
!     ReadOnAllPEs: Optional logical (default: false) that determines if the
!     D1array is: i) read on PE0 and broadcast to all others (false) or, ii) read on each PE (true)
!
! Outputs:
!     Result: 2D Real array representing a single level of the model field
!
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ReadUMField (UMHdr,        &
                            StashCode,    &
                            Result,       &
                            Level,        &
                            Time,         &
                            ModelTimeUTC, &
                            Vector,       &
                            IntField,     &
                            AddBLField,   &
                            ReadOnAllPEs)

USE GenMod_CLookAdd, ONLY: &
  LBLREC,                  &
  LBFT,                    &
  ITEM_CODE,               &
  LBPACK,                  &
  BDY,                     &
  BZY

USE GenMod_Control, ONLY: &
  MyPE

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  gen_warn,            &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI,                                  &
  RMDI,                                  &
  Pi

USE GenMod_ModelIO, ONLY: &
  UM_header_type

USE GenMod_UMHeaderConstants, ONLY: &
  FH_ColDepCSize1,                  &
  FH_RowDepCSize1,                  &
  FH_LevDepCSize1,                  &
  FH_SubModel,                      &
  FH_SubModel_Ocean,                &
  FH_HorizGrid_LamWrapEq,           &
  FH_HorizGrid_LamWrap,             &
  FH_HorizGrid_Global,              &
  FH_HorizGrid,                     &
  IC_NoSeaPts,                      &
  FH_CompFldI1Size

USE OpsMod_Control, ONLY: &
  mpi_group

USE OpsMod_Gcom, ONLY: &
  gcg_rbcast

USE OpsMod_HorizontalInterp, ONLY: &
  num_extend_points

USE OpsMod_Kinds, ONLY: &
  real32,               &
  real64

IMPLICIT NONE

! Subroutine arguments:
TYPE (UM_Header_Type), INTENT(IN)    :: UMHdr
INTEGER, INTENT(IN)                  :: StashCode
REAL(kind=real32), INTENT(INOUT)     :: Result(:,:)
INTEGER, OPTIONAL, INTENT(IN)        :: Level
INTEGER, OPTIONAL, INTENT(IN)        :: Time
INTEGER, OPTIONAL, INTENT(IN)        :: ModelTimeUTC(:)
LOGICAL, OPTIONAL, INTENT(IN)        :: Vector       ! whether this is a vector component
LOGICAL, OPTIONAL, INTENT(IN)        :: IntField     ! Whether this is an integer field
INTEGER, OPTIONAL, INTENT(IN)        :: AddBLField   ! is THIS 2nd boundary level pressure
LOGICAL, OPTIONAL, INTENT(IN)        :: ReadOnAllPEs

! Local declarations:
INTEGER                              :: PointsInRow
INTEGER                              :: PointsInCol
LOGICAL                              :: WgdosExpand
INTEGER                              :: AnInteger ! for the transfer function.
INTEGER                              :: IStat
INTEGER                              :: iField
INTEGER                              :: Field
INTEGER                              :: NLev
INTEGER                              :: len_data
LOGICAL                              :: LOcean
LOGICAL                              :: LCyclic
LOGICAL                              :: LGlobal
LOGICAL                              :: LVector
REAL                                 :: DPhiIn
REAL                                 :: PhiOrigin
REAL                                 :: AReal  ! for the transfer function
REAL, ALLOCATABLE                    :: FldExp(:) ! Uncompressed ocean dump fields
REAL(kind=real64), ALLOCATABLE, SAVE :: D1Array(:)
CHARACTER(len=80)                    :: ErrMess
CHARACTER(len=*), PARAMETER          :: RoutineName = 'Ops_ReadUMField'
INTEGER                              :: level_local
LOGICAL                              :: ReadOnAllPEs_local
LOGICAL                              :: ReadField
LOGICAL                              :: BcastField

IF (UseTrace) CALL gen_trace_entry (RoutineName)

Field = IMDI
NLev = 0
AnInteger = 0
AReal = 0.0

IF (PRESENT (ReadOnAllPEs)) THEN
  ReadOnAllPEs_local = ReadOnAllPEs
ELSE
  ReadOnAllPEs_local = .FALSE.
END IF

IF (ReadOnAllPEs_local) THEN
  ReadField = .TRUE.
  BcastField = .FALSE.
ELSE
  ReadField = .FALSE.
  BcastField = .TRUE.
  IF (mype == 0) THEN
    ReadField = .TRUE.
  END IF
END IF

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

! Work out the size of the array
PointsInRow = SIZE (Result, DIM = 1) - 2 * num_extend_points
PointsInCol = SIZE (Result, DIM = 2) - 2 * num_extend_points

! Only allocate if not already allocated or if the size of the array changes
IF (.NOT. ALLOCATED (D1Array)) then
  ALLOCATE (D1Array(PointsInRow * PointsInCol))
ELSE IF (SIZE (D1Array) /= PointsInRow * PointsInCol) then
  DEALLOCATE (D1Array)
  ALLOCATE (D1Array(PointsInRow * PointsInCol))
END IF

WgdosExpand = .FALSE.
D1Array = RMDI
IF (MODULO (UMHdr % LookUp(LBPACK,Field + Level_local - 1), 10) /= 0) WgdosExpand = .TRUE.

IF (ReadField) THEN

  CALL UMHdr % read_field (Field + Level_local - 1, & ! in
                           D1Array(:))                ! out

END IF

IF (BcastField) THEN
  len_data = SIZE (D1Array)
  CALL gcg_rbcast (1,         &
                   len_data,  &
                   0,         &
                   mpi_group, &
                   istat,     &
                   D1Array)
END IF

IF (PRESENT (IntField)) THEN
  IF (IntField) D1Array = REAL (TRANSFER (D1Array, AnInteger, SIZE (D1Array)))
END IF

! If model dump is an ocean dump,
! uncompress the land points if neccessary
!------------------------------------------

IF (LOcean .AND. WgdosExpand) THEN ! Compressed data

  LCyclic = UMHdr % FixHd(FH_HorizGrid) == FH_HorizGrid_LamWrap .OR. &
            UMHdr % FixHd(FH_HorizGrid) == FH_HorizGrid_LamWrapEq

  ALLOCATE (FldExp(UMHdr % FixHd(FH_RowDepCSize1) * UMHdr % FixHd(FH_ColDepCSize1)))

  CALL Ops_UncompressDump (UMHdr % FixHd(FH_RowDepCSize1),                 &
                           UMHdr % FixHd(FH_ColDepCSize1),                 &
                           UMHdr % FixHd(FH_LevDepCSize1),                 &
                           UMHdr % FixHd(FH_CompFldI1Size),                &
                           UMHdr % LookUp(LBLREC,Field + Level_local - 1), &
                           UMHdr % CompFldI1(:),                           &
                           UMHdr % CompFldI2(:),                           &
                           UMHdr % CompFldI3(:),                           &
                           UMHdr % IntC(IC_NoSeaPts),                      &
                           Level,                                          &
                           LCyclic,                                        &
                           D1Array,                                        &
                           FldExp(:))

  D1Array(:) = FldExp(:)
  DEALLOCATE (FldExp)

END IF

! Reshape 1-D array and put into ModelDump structure
!----------------------------------------------------

LVector = .FALSE.
IF (PRESENT (Vector)) LVector = Vector

DPhiIn = TRANSFER (UMHdr % LookUp(BDY,Field + Level_local - 1), AReal) * Pi / 180.0
PhiOrigin  = TRANSFER (UMHdr % LookUp(BZY,Field + Level_local - 1), AReal) * Pi / 180.0 + DPhiIn

! Reshape and copy the D1Array to the Result array
Result(num_extend_points + 1:num_extend_points + PointsInRow, &
       num_extend_points + 1:num_extend_points + PointsInCol) &
       = RESHAPE (D1Array, (/PointsInRow,PointsInCol/))

CALL Ops_HorizontalExtend (1 - num_extend_points, & ! in
                           LGlobal,               & ! in
                           LVector,               & ! in
                           PhiOrigin,             & ! in
                           DPhiIn,                & ! in
                           Result)                  ! inout

9999 CONTINUE

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ReadUMField
