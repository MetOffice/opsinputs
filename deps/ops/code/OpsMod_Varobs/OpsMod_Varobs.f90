!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Declarations relating to varobs generation.
!-------------------------------------------------------------------------------

MODULE OpsMod_Varobs

USE OpsMod_ObsGroupInfo, ONLY: &
  max_obs_group_num

USE OpsMod_ObsInfo, ONLY: &
  LenCallsign

IMPLICIT NONE

SAVE

! Public declarations:

! Global Parameters
INTEGER              :: NumMeta            ! Actual number of metadata values
INTEGER              :: NumItem            ! Actual number of observation items
INTEGER              :: NumObVariables    ! Actual number of ob variables
INTEGER              :: NumObLev          ! Number of observation levels
REAL                 :: ObsLevelType      ! Atmosphere obs level type
INTEGER              :: LenObCol          ! Length of an ob column
INTEGER, PARAMETER   :: NumMetaMax = LenCallsign + 6 ! Max num of metadata values
INTEGER, PARAMETER   :: NumItemMax = 3     ! Maximum number of observation items
INTEGER, PARAMETER   :: NumObPItemMax = 1  ! Maximum number of ob pressure items
INTEGER, PARAMETER   :: NumObPItem = 0     ! Actual number of ob pressure items
INTEGER, PARAMETER   :: NumCxPItemMax = 2  ! Maximum number of Cx pressure items
INTEGER, PARAMETER   :: NumCxPItem = 0     ! Actual number of Cx pressure items
INTEGER, PARAMETER   :: NumCxSfVarMax = 8  ! Maximum number of Cx sfce variables
INTEGER, PARAMETER   :: NumCxSfVar = 8     ! Actual number of Cx sfce variables
INTEGER, PARAMETER   :: NumCxUaVarMax = 12 ! Maximum number of Cx uair variables
INTEGER, PARAMETER   :: NumCxUaVar = 4     ! Actual number of Cx uair variables

! Parameters to set scaling applied to bogus observation errors
REAL, PARAMETER      :: BogusObErrScaling_VAR = 0.5 ! For VAR

INTEGER              :: VarobsVersionNumber = 6

INTEGER, PARAMETER   :: AssimDataFormat_VAR = 2
INTEGER, PARAMETER   :: AssimDataFormat_ModelOb = 3

CONTAINS

INCLUDE 'Ops_CheckVarFields.inc'
INCLUDE 'Ops_CreateVarobs.inc'
INCLUDE 'Ops_GetDefaultVarfields.inc'
INCLUDE 'Ops_ModelObToOb.inc'
INCLUDE 'Ops_ReadVarobsControlNL.inc'
INCLUDE 'Ops_SetupVarArray.inc'
INCLUDE 'Ops_SetupVarobsColDepC.inc'
INCLUDE 'Ops_SetupVarobsFixhd.inc'
INCLUDE 'Ops_SetupVarobsIntC.inc'
INCLUDE 'Ops_SetupVarobsLevDepC.inc'
INCLUDE 'Ops_SetupVarobsLookup.inc'
INCLUDE 'Ops_SetupVarobsRealC.inc'
INCLUDE 'Ops_VarobPGEs.inc'
INCLUDE 'Ops_WriteVarobs.inc'

END MODULE OpsMod_Varobs
