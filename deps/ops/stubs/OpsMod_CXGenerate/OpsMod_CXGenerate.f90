!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Contains shared variables required for cx generation
!-------------------------------------------------------------------------------

MODULE OpsMod_CXGenerate

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE OpsMod_HorizontalInterp, ONLY: &
  interp_type_linear_coast

IMPLICIT NONE

SAVE

! Public declarations:
TYPE CXArray_Type  ! Holds information about each array in the cx structure
  INTEGER          :: NumLevels = IMDI   ! The number of levels required for the array
  REAL, POINTER    :: R1(:) => NULL ()   ! Points to the array to be filled with data
  REAL, POINTER    :: R2(:,:) => NULL () ! Points to the array to be filled with data
END TYPE CXArray_Type

TYPE LocInfo_Type ! Holds information about the obs locations for an obsgroup
  REAL, POINTER    :: Phi(:) => NULL()           ! \ Lat, long in radians.
  REAL, POINTER    :: Lambda(:) => NULL()        ! / (possibly in rotated grid)
  REAL, POINTER    :: SPhi(:,:) => NULL()        ! \  As above, but for each model level
  REAL, POINTER    :: SLambda(:,:) => NULL()     ! /  Eg radiosonde slant ascent
  REAL, POINTER    :: STime(:,:) => NULL()       ! Time for each model level
  INTEGER, POINTER :: Index(:) => NULL()         ! Index pointing to each ob in time order
  INTEGER, POINTER :: FirstInWindow(:) => NULL() ! first ob within time window
  INTEGER, POINTER :: LastInWindow(:) => NULL()  ! last ob within time window
  LOGICAL, POINTER :: UseLevel(:,:) => NULL()    ! use data from this level?
END TYPE LocInfo_Type

TYPE FieldInfo_Type ! Holds information about each field
  ! The position of the field in the dump, for each forecast time
  INTEGER, POINTER :: StartPositions(:) => NULL ()
  INTEGER          :: StashCode = IMDI          ! The stashcode of this field
  INTEGER          :: NumLevelsAvailable = IMDI ! The number of levels of this field in the dump
  LOGICAL          :: IntField = .FALSE.        ! Whether this field is stored as integer in the dump
  LOGICAL          :: VectorField = .FALSE.     ! Whether this field is a component of a vector
  LOGICAL          :: DerivedField = .FALSE.    ! Whether this field is to be derived from other fields
  LOGICAL          :: FieldSetup = .FALSE.
END TYPE FieldInfo_Type

! Used to distinguish between background files and background error create files
INTEGER, PARAMETER :: dump_type_bg = 1
INTEGER, PARAMETER :: dump_type_bgerr = 2
INTEGER            :: CxLevels = 30
INTEGER            :: BgerrLevels = 30

! Ob type specific control options for extraction
INTEGER, PARAMETER :: MaxModelCodes = 100
INTEGER, PARAMETER :: MaxForecastPeriods = 7
INTEGER            :: ForecastPeriods(MaxForecastPeriods) = IMDI
INTEGER            :: BGE_ForecastPeriod = 6
INTEGER            :: SSTBackground_Age = IMDI
LOGICAL            :: UseValidityTimes = .FALSE.
INTEGER            :: cx_interp_method = interp_type_linear_coast
INTEGER            :: cx_no_interpolation(MaxModelCodes) = IMDI
INTEGER            :: LevelLapse = 1        ! If <= 0 then climatological LapseRate used
REAL               :: ZVarMinLapse = 100.0  ! Minimum height variance for Lapse calculation
INTEGER            :: xRadiusLapse = 4
INTEGER            :: yRadiusLapse = 4
REAL               :: seaice_theshold_for_sst = 0.9

! Switch to control how model fields are accessed
LOGICAL            :: SharedMemory = .FALSE.          ! Read all required fields into shared memory
LOGICAL            :: SharedMemoryTimeSlice = .FALSE. ! Read single time slices when using shared memory
LOGICAL            :: ReadOnAllPEs = .FALSE.          ! Read fields on all PE's

CONTAINS

INCLUDE 'Ops_BGEandCxCreate.inc'
INCLUDE 'Ops_BGEAndCxCreateFromNetCDF.inc'
INCLUDE 'Ops_BGEComplete.inc'
INCLUDE 'Ops_CalcTLapse.inc'
INCLUDE 'Ops_CreateCxUMHdr.inc'
INCLUDE 'Ops_CxComplete.inc'
INCLUDE 'Ops_CXGenerate.inc'
INCLUDE 'Ops_CXPerformInterp.inc'
INCLUDE 'Ops_CXPreproLocations.inc'
INCLUDE 'Ops_CXSetup.inc'
INCLUDE 'Ops_CXSetupArrays.inc'
INCLUDE 'Ops_CXSetupArraysAncil.inc'
INCLUDE 'Ops_CXSlantPressure.inc'
INCLUDE 'Ops_DeriveField.inc'
INCLUDE 'Ops_GetDefaultBgerrFields.inc'
INCLUDE 'Ops_GetDefaultCxFields.inc'
INCLUDE 'Ops_GetReqDims.inc'
INCLUDE 'Ops_GetUMFieldAvailable.inc'
INCLUDE 'Ops_InitFieldDataShared.inc'
INCLUDE 'Ops_LocateUMField.inc'
INCLUDE 'Ops_ReadCXControlNL.inc'
INCLUDE 'Ops_ReadFieldsControlNL.inc'
INCLUDE 'Ops_ReadModelShared.inc'

END MODULE OpsMod_CXGenerate
