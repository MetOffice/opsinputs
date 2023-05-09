!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Read and interpolate distance from coast field also calculate solar zenith
! angle.
!
! Calculate solar zenith angle
! Obtain COASTDIST file name and read field
! Interpolate to observation locations
! Tidy up
!
! Inputs:
!     Obs - Observational data with positions
!
! Outputs:
!     Obs - Observational data with CoastDist set
!     Note that CoastDist is +ve for points at sea, -ve for inland points
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CoastZenith (Obs)

USE GenMod_CLookAdd, ONLY: &
    BDX,                   &
    BDY,                   &
    BZX,                   &
    BZY

USE GenMod_Control, ONLY: &
    DebugMode,            &
    mype

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    gen_warn,          &
    MessageOut,        &
    StatusOK,          &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI

USE OpsMod_Constants, ONLY: &
    deg2rad

USE OpsMod_FieldRead, ONLY: &
    Ops_GetUMFieldDims,     &
    Ops_ReadUMField

USE OpsMod_HorizontalInterp, ONLY: &
    Ops_HorizontalInterp

USE OpsMod_IOUtils, ONLY: &
    Ops_CloseCFile

USE OpsMod_Kinds, ONLY: &
    real32

USE OpsMod_ModelIO, ONLY:         &
    DomainInfo_type,              &
    Ops_FieldsFile_ReadHeaderEnv

USE OpsMod_ObsInfo, ONLY: &
    OB_Type,              &
    OutOfAreaReport

USE OpsMod_ObTypeUtils, ONLY: &
    Ops_Solar_Zenith

USE OpsMod_Process, ONLY:       &
    ProcessMode,                &
    SolarZenithIgnoreOutOfArea

USE OpsMod_Stash, ONLY:  &
    StashCode_CoastDist

USE OpsMod_Utilities, ONLY: &
    Ops_WorkingCoordinates

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_type), INTENT(INOUT)  :: Obs     ! Contains data on obs

! Local declarations:
CHARACTER(len=*), PARAMETER    :: RoutineName = 'Ops_CoastZenith'
CHARACTER(len=80)              :: ErrMess(2)
INTEGER                        :: NumSel
INTEGER                        :: FieldDims(2)
REAL(kind=real32), ALLOCATABLE :: CoastDistField(:,:)
REAL                           :: DLambda
REAL                           :: DPhi
REAL                           :: LambdaOrigin
REAL                           :: PhiOrigin
REAL                           :: x
REAL, ALLOCATABLE              :: LatTemp(:)
REAL, ALLOCATABLE              :: LonTemp(:)
REAL, ALLOCATABLE              :: LatRot(:)
REAL, ALLOCATABLE              :: LonRot(:)
REAL, ALLOCATABLE              :: DistTemp(:)
TYPE (DomainInfo_type)         :: coast_dist

! Initialise

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!----------------------------------
!  1. Calculate solar zenith angles
!----------------------------------

CALL Ops_Solar_Zenith (Obs,                        &
                       Obs % SolarZenith,          &  ! Out : Solar Zenith in degrees
                       SolarZenithIgnoreOutOfArea)

!---------------------------------------
! 2. Open coastdist file
!---------------------------------------

CALL Ops_FieldsFile_ReadHeaderEnv (coast_dist,                       &
                                   "OPS_COASTDIST",                  &
                                   status_if_env_not_set = StatusOK)

IF (.NOT. ALLOCATED (coast_dist % header)) THEN
  ErrMess(1) = "OPS_COASTDIST is not set"
  ErrMess(2) = "Distance from coast check will not be applied"
  CALL gen_warn (RoutineName,  & ! in
                 ErrMess(1:2))   ! in
  GOTO 9999
END IF

IF (ProcessMode >= DebugMode .AND. mype == 0) THEN
  WRITE (MessageOut, '(2A)') RoutineName, ' Reading CoastDist file header'
END IF

!---------------------------
! 3. Read in CoastDist field
!---------------------------

CALL Ops_GetUMFieldDims (FieldDims,              &
                         coast_dist % header(1), &
                         StashCode_CoastDist)
ALLOCATE (CoastDistField(FieldDims(1),FieldDims(2)))

CALL Ops_ReadUMField (coast_dist % header(1), &
                      StashCode_CoastDist,    &
                      CoastDistField)

!--------------------------------------------------------
! 4. Interpolate CoastDist field (= field 1) then tidy up
!--------------------------------------------------------

x = 0.0
IF (coast_dist % variable_resolution) THEN  ! Variable resolution grid
  LambdaOrigin = coast_dist % header(1) % ColDepC(1)
  PhiOrigin = coast_dist % header(1) % RowDepC(1)
  DLambda = 0.0      ! undefined for variable grid
  DPhi = 0.0      ! -ditto-
ELSE     ! regular grid
  DLambda = TRANSFER (coast_dist % header(1) % Lookup(BDX,1),x) * deg2rad
  DPhi = TRANSFER (coast_dist % header(1) % Lookup(BDY,1),x) * deg2rad
  LambdaOrigin = TRANSFER (coast_dist % header(1) % Lookup(BZX,1),x) * deg2rad + DLambda
  PhiOrigin = TRANSFER (coast_dist % header(1) % Lookup(BZY,1),x) * deg2rad + DPhi
END IF

! Remove any 'out of area' reports (set in Ops_CheckObsValidity)

NumSel = COUNT (.NOT. BTEST (Obs % ReportFlags(:), OutOfAreaReport))

ALLOCATE (LatTemp(NumSel))
ALLOCATE (LonTemp(NumSel))
ALLOCATE (DistTemp(NumSel))
ALLOCATE (LatRot(NumSel))
ALLOCATE (LonRot(NumSel))

LatTemp(:) = PACK (Obs % Latitude(:), .NOT. BTEST (Obs % ReportFlags(:), OutOfAreaReport))
LonTemp(:) = PACK (Obs % Longitude(:), .NOT. BTEST (Obs % ReportFlags(:), OutOfAreaReport))

! Rotate coordinates if required
CALL Ops_WorkingCoordinates (LatTemp,                &
                             LonTemp,                &
                             coast_dist % pole_lat,  &
                             coast_dist % pole_lon,  &
                             coast_dist % grid_type, &
                             LatRot,                 &
                             LonRot)

! Convert to radians
LonRot(:) = LonRot(:) * deg2rad
LatRot(:) = LatRot(:) * deg2rad

! Perform interpolation
CALL Ops_HorizontalInterp (coast_dist,          &
                           LambdaOrigin,        &
                           PhiOrigin,           &
                           DLambda,             &
                           DPhi,                &
                           LonRot(:),           &
                           LatRot(:),           &
                           CoastDistField(:,:), &
                           DistTemp(:),         &
                           StashCode_CoastDist)

Obs % CoastDist(:) = UNPACK (DistTemp(:), .NOT. BTEST (Obs % ReportFlags(:), OutOfAreaReport), RMDI)

CALL Ops_CloseCFile (coast_dist % header(1) % UnitNum)

!------------------------------------------
! 5. End
!------------------------------------------

9999 CONTINUE       ! Skip to here if error

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CoastZenith
