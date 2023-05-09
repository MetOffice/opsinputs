!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! To establish model grid boundaries.
!
! Inputs : UM header from model dump
!
! Outputs: Model grid boundaries
!
! The UM header is interrogated and the boundaries of the model area are passed
! back to calling routine.  The boundaries are RELATIVE TO THE GRID'S
! ORIENTATION.
!
! ASSUMES ALL LONGITUDES BETWEEN +/- 180 DEGREES
!-------------------------------------------------------------------------------

SUBROUTINE Ops_DefineModelArea (domain, &
                                Deltax, &
                                Deltay)

USE GenMod_Control, ONLY: &
    GeneralMode,          &
    DebugMode,            &
    VerboseMode,          &
    mype

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    MessageOut,        &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    IMDI,                                &
    RMDI

USE GenMod_UMHeaderConstants, ONLY: &
    IC_XLen,                        &
    IC_YLen,                        &
    RC_LatSpacing,                  &
    RC_LongSpacing,                 &
    FH_HorizGrid,                   &
    FH_HorizGrid_Global,            &
    FH_HorizGrid_LamNoWrap,         &
    FH_HorizGrid_LamNoWrapEq,       &
    FH_HorizGrid_LamWrap,           &
    FH_SubModel,                    &
    FH_SubModel_Atmos,              &
    FH_SubModel_Ocean,              &
    RC_FirstLat,                    &
    RC_FirstLong,                   &
    FH_GridStagger,                 &
    FH_GridStagger_ArakawaC,        &
    FH_GridStagger_EndGame,         &
    FH_RowDepCStart

USE OpsMod_Control, ONLY: &
    ModelEastBound,       &
    ModelNorthBound,      &
    ModelSouthBound,      &
    ModelWestBound

IMPLICIT NONE

! Subroutine arguments:
TYPE (DomainInfo_type), INTENT(IN) :: domain
REAL, INTENT(OUT)                  :: DeltaY   ! longitude spacing of background error file
REAL, INTENT(OUT)                  :: DeltaX   ! latitude spacing of background error file

! Local declarations:
CHARACTER(len=*), PARAMETER        :: RoutineName = "Ops_DefineModelArea"
INTEGER                            :: Nrows
INTEGER                            :: Ncols
REAL                               :: TempLat

!-------------------------------------------------------------------------------
! 0. Tracing and initialisation
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-------------------------------------------------------------------------------
! 1. Determine boundaries of model grid
!-------------------------------------------------------------------------------

Nrows = IMDI
Ncols = IMDI

IF (domain % header(1) % FixHd(FH_HorizGrid) == FH_HorizGrid_Global .OR. &
    domain % header(1) % FixHd(FH_HorizGrid) == IMDI) THEN
  !-----------------------------------------------------------------------------
  !  1.1. Global grid - set boundaries to maximum
  !-----------------------------------------------------------------------------

  IF (GeneralMode >= DebugMode .AND. mype == 0) WRITE (MessageOut, '(A)') 'Global model'

  Nrows = domain % header(1) % IntC(IC_YLen)
  Ncols = domain % header(1) % IntC(IC_XLen)
  DeltaX = domain % header(1) % RealC(RC_LongSpacing)
  DeltaY = domain % header(1) % RealC(RC_LatSpacing)
  ModelNorthBound = 90.0
  ModelSouthBound = -90.0
  ModelWestBound = -180.0
  ModelEastBound = 180.0

ELSE IF (domain % header(1) % FixHd(FH_HorizGrid) == FH_HorizGrid_LamNoWrap .AND. &
         domain % header(1) % FixHd(FH_Submodel) /= FH_Submodel_Atmos) THEN
  !-----------------------------------------------------------------------------
  !  1.2. LAM without wrap, e.g. WAVE fieldsfile.
  !-----------------------------------------------------------------------------

  IF (GeneralMode >= DebugMode) WRITE (MessageOut, '(A)') 'LAM without wrap, e.g. WAVE fieldsfile'

  DeltaX = domain % header(1) % RealC(RC_LongSpacing)
  DeltaY = domain % header(1) % RealC(RC_LatSpacing)
  Nrows = domain % header(1) % IntC(IC_YLen)
  Ncols = domain % header(1) % IntC(IC_XLen)
  ModelNorthBound = domain % header(1) % RealC(RC_FirstLat)
  ModelSouthBound = ModelNorthBound + DeltaY * (Nrows - 1)
  ModelWestBound = domain % header(1) % RealC(RC_FirstLong)
  IF (domain % header(1) % RealC(RC_FirstLong) >= 180.0) THEN
    ModelWestBound = ModelWestBound - 360.0
  END IF
  ModelEastBound = ModelWestBound + DeltaX * (Ncols - 1)

  ! OCEAN dumps go from South to North
  IF (domain % header(1) % Fixhd(FH_SubModel) == FH_SubModel_Ocean) THEN
    IF (GeneralMode >= DebugMode) WRITE (MessageOut, '(A)') 'OCEAN dump'
    ModelSouthBound = domain % header(1) % RealC(RC_FirstLat)
    ModelNorthBound = ModelSouthBound + DeltaY * (Nrows - 1)
  END IF

ELSE IF (domain % header(1) % FixHd(FH_HorizGrid) == FH_HorizGrid_LamWrap) THEN
  !-----------------------------------------------------------------------------
  !  1.2. LAM with wrap, e.g. Ocean dump. To account for wind grid,
  !       shrink size by half a gridlength.
  !-----------------------------------------------------------------------------

  IF (GeneralMode >= DebugMode .AND. mype == 0) WRITE (MessageOut, '(A)') 'LAM with wrap, e.g. Ocean dump'

  ModelWestBound = -180.0
  ModelEastBound = 180.0
  DeltaX = domain % header(1) % RealC(RC_LongSpacing)
  DeltaY = domain % header(1) % RealC(RC_LatSpacing)
  Nrows = domain % header(1) % IntC(IC_YLen)
  Ncols = domain % header(1) % IntC(IC_XLen)
  ModelNorthBound = domain % header(1) % RealC(RC_FirstLat)
  ModelSouthBound = ModelNorthBound - DeltaY * (Nrows - 1)

  ! OCEAN dumps go from South to North
  IF (domain % header(1) % Fixhd (FH_SubModel) == FH_SubModel_Ocean) THEN
    ModelSouthBound = domain % header(1) % RealC(RC_FirstLat)
    ModelNorthBound = ModelSouthBound + DeltaY * (Nrows - 1)
  END IF

ELSE IF (domain % header(1) % FixHd(FH_HorizGrid) == FH_HorizGrid_LamNoWrapEq .OR. &
         (domain % header(1) % FixHd(FH_HorizGrid) == FH_HorizGrid_LamNoWrap .AND. &
          domain % header(1) % FixHd(FH_Submodel) == FH_Submodel_Atmos)) THEN

  !-----------------------------------------------------------------------------
  !  1.3.1 Rotated LAM without wrap, e.g. Mesoscale model. To account for wind
  !        grid, shrink size by half a gridlength all round.
  !-----------------------------------------------------------------------------

  IF (GeneralMode >= DebugMode .AND. mype == 0) WRITE (MessageOut, '(A)') 'Rotated LAM without wrap'

  Nrows = domain % header(1) % IntC(IC_YLen)
  Ncols = domain % header(1) % IntC(IC_XLen)

  IF (domain % variable_resolution) THEN
    ModelNorthBound = MINVAL (domain % header(1) % RowDepC(1:domain % header(1) % IntC(IC_YLen))) + &
                               (domain % header(1) % RowDepC(2) - domain % header(1) % RowDepC(1)) / 2
    ModelSouthBound = MAXVAL (domain % header(1) % RowDepC(1:domain % header(1) % IntC(IC_YLen))) - &
                               (domain % header(1) % RowDepC(2) - domain % header(1) % RowDepC(1)) / 2
    ModelWestBound = MINVAL (domain % header(1) % ColDepC(1:domain % header(1) % IntC(IC_XLen))) + &
                               (domain % header(1) % ColDepC(2) - domain % header(1) % ColDepC(1)) / 2
    ModelEastBound = MAXVAL (domain % header(1) % ColDepC(1:domain % header(1) % IntC(IC_XLen))) - &
                               (domain % header(1) % ColDepC(2) - domain % header(1) % ColDepC(1)) / 2
    DeltaX = RMDI
    DeltaY = RMDI

  ELSE
    DeltaY = domain % header(1) % RealC(RC_LatSpacing)
    ModelNorthBound = domain % header(1) % RealC(RC_FirstLat)
    DeltaX = domain % header(1) % RealC(RC_LongSpacing)
    ModelWestBound = domain % header(1) % RealC(RC_FirstLong)

    IF (domain % header(1) % Fixhd(FH_GridStagger) == FH_GridStagger_ArakawaC .OR. &
        domain % header(1) % Fixhd(FH_GridStagger) == FH_GridStagger_EndGame) THEN
      DeltaY = -1.0 * DeltaY
    END IF

    ModelNorthBound = ModelNorthBound - 0.5 * DeltaY
    ModelWestBound = ModelWestBound + 0.5 * DeltaX
    ModelSouthBound = ModelNorthBound - DeltaY * (Nrows - 2)
    ModelEastBound = ModelWestBound + DeltaX * (Ncols - 1)

  END IF

  IF (ModelEastBound >= 180.0) ModelEastBound = ModelEastBound - 360.0
  IF (ModelWestBound >= 180.0) ModelWestBound = ModelWestBound - 360.0

  ! Ensure that northern boundary *is* north of the southern boundary
  IF (ModelNorthBound < ModelSouthBound) THEN
    TempLat = ModelSouthBound
    ModelSouthBound = ModelNorthBound
    ModelNorthBound = TempLat
  END IF

END IF

!-------------------------------------------------------------------------------
! 2. Print grid details if required
!-------------------------------------------------------------------------------

IF (GeneralMode >= VerboseMode .AND. mype == 0) THEN
  WRITE (MessageOut, '(A)') 'MODEL GRID DETAILS FROM FIELDSFILE'
  WRITE (MessageOut, '(A)') '============================'
  WRITE (MessageOut, '(A,F8.3)') 'Northlat :', ModelNorthBound
  WRITE (MessageOut, '(A,F8.3)') 'Southlat :', ModelSouthBound
  WRITE (MessageOut, '(A,F8.3)') 'WestLong :', ModelWestBound
  WRITE (MessageOut, '(A,F8.3)') 'EastLong :', ModelEastBound
  WRITE (MessageOut, '(A,I0)') 'Nrows    :', Nrows
  WRITE (MessageOut, '(A,I0)') 'Ncols    :', Ncols
  IF (domain % header(1) % FixHd(FH_RowDepCStart) <= 0 .OR. &
      domain % header(1) % FixHd(FH_SubModel) == FH_SubModel_Ocean) THEN
    WRITE (MessageOut, '(A,F8.3)') 'DeltaX   :', DeltaX
    WRITE (MessageOut, '(A,F8.3)') 'DeltaY   :', DeltaY
  END IF
  WRITE (MessageOut, '(A)') ' '
END IF

!-------------------------------------------------------------------------------
! 4. End of routine
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_DefineModelArea
