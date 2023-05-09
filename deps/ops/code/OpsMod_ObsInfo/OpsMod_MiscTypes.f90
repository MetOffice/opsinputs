!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! This module currently contains:
! Items common to both OpsMod_ObsInfo and VarMod_ObsInfo
! It is USEd by both OpsMod_ObsInfo and VarMod_ObsInfo
!
! a) Derived data types for CX columns
! b) Selected Global PARAMETERs
!-------------------------------------------------------------------------------

MODULE OpsMod_MiscTypes

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI,                                  &
  RMDI

USE OpsMod_Constants, ONLY: &
  PGEMDI

IMPLICIT NONE

SAVE

! Public declarations:

! Possible settings of Zcode in ElementHeader_type
INTEGER, PARAMETER :: ZcodeUnused = -1      ! => no level details
INTEGER, PARAMETER :: ZcodeSurface = 0      ! => surface
INTEGER, PARAMETER :: ZcodeLevelsA = 1      ! => data is at levels A
INTEGER, PARAMETER :: ZcodeLevelsB = 2      ! => data is at levels B
INTEGER, PARAMETER :: ZcodeLayersA = 3      ! => is for layers between levels A
INTEGER, PARAMETER :: ZcodeLayersB = 4      ! => is for layers between levels B
INTEGER, PARAMETER :: Zcodep = 5            ! => PF data at p levels
INTEGER, PARAMETER :: Zcodetheta = 6        ! => PF data at theta levels
INTEGER, PARAMETER :: Zcodechannel = 7      ! => data is for radiance channels
INTEGER, PARAMETER :: ZcodeDepths = 8       ! => data is on ocean levels
INTEGER, PARAMETER :: ZcodeLaneNumber = 9   ! => data is in road side lanes
INTEGER, PARAMETER :: ZcodeImpactParam = 10 ! => data is on impact parameters

TYPE ElementHeader_type
  LOGICAL :: Present = .FALSE.     ! whether element is present or not (see below)
  INTEGER :: NumLev = IMDI         ! Number of levels of data
  INTEGER :: Zcode = IMDI          ! Level type (see below)
END TYPE ElementHeader_type

!-------------------------------------------------------------------
!  Element data type used in OB_type
!-----------------------------------
TYPE Element_type
  REAL             :: Value = RMDI
  REAL             :: CorValue = RMDI
  REAL             :: OBErr = RMDI
  REAL             :: PGEFinal = PGEMDI
  INTEGER          :: Flags = 0
END TYPE Element_type

!-------------------------------------------------------------------
!  Coord data type used in OB_type
!  The current code does not make any allowance for possible errors in the
!  observation position, so no space is provided to characterise such errors.
!  A derived type is used so that a future version can add OBErr etc if desired.
!----------------------------------
TYPE coord_type
  REAL             :: Value = RMDI
  INTEGER          :: Flags = 0
END TYPE coord_type

! Global action control

INTEGER, PARAMETER :: ActionInit = 1
INTEGER, PARAMETER :: ActionPrint1 = 2
INTEGER, PARAMETER :: ActionPrint2 = 3
INTEGER, PARAMETER :: ActionDealloc = 4
INTEGER, POINTER   :: ObPrintIndex(:)

CONTAINS

INCLUDE 'Ops_SetObPrintIndex.inc'

END MODULE OpsMod_MiscTypes
