!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Module containing routines to read observations from the ObStore file into the
! Ob_Type structure. Also contains some other stray routines.
!-------------------------------------------------------------------------------

MODULE OpsMod_Process

USE GenMod_Control, ONLY: &
  DiagnosticMode

USE OpsMod_ObsInfo, ONLY: &
  Ob_Type

USE GenMod_ModelIO, ONLY: &
  UM_header_type

IMPLICIT NONE

SAVE

! Public declarations:

LOGICAL          :: VerRun = .FALSE.
INTEGER          :: ProcessMode = DiagnosticMode

INTEGER          :: TrackOptions = 0 ! 0 simultaneous obs checked in main loop
                                     ! 1 deferred check for simultaneous obs
                                     !   + possible reinstatement of flagged obs
                                     ! Add 2 to check "Fast/Short" tracks
LOGICAL          :: PerformDuplicateCheck = .TRUE.  ! Aircraft, Ocean
LOGICAL          :: PerformTrackCheck = .TRUE.  ! Aircraft, Ocean
LOGICAL          :: PerformStuckCheck = .FALSE. ! Surf
LOGICAL          :: PerformSuperobbing = .TRUE.  ! SST,      Ocean
LOGICAL          :: PerformBackgroundCheck = .TRUE.
LOGICAL          :: PerformBuddyCheck = .TRUE.
LOGICAL          :: PerformSondeConsist = .TRUE.   ! Perform consistency check
LOGICAL          :: CapSupersat = .TRUE. ! Cap RHice values to 100%
LOGICAL          :: LogHeight = .TRUE.   ! Indicates if logarithm of
                                         ! height readings is to be taken during vertical
                                         ! interpolation
INTEGER          :: FOAMCovStats = 1  ! use of MesVar, HCor etc
                                      ! 0 old cov stats in QC, old ACobs
                                      ! 1 new cov stats in QC, old ACobs
                                      ! 2 new cov stats in QC, new ACobs
                                      ! 11,12 as 1,2 but with extra factor for climate bk
LOGICAL          :: PerformLogBackgroundCheck = .FALSE.   ! Indicates if background
                                                          ! check should be performed on the
                                                          ! logarithm of the observations
REAL             :: SuperobHours = 24.0   ! superob window in hours
REAL             :: observation_error_multiplier = 1.0
REAL             :: ThinningDLat = 0.2   ! thinning latitude spacing degrees
REAL             :: ThinningDLon = 0.2   ! thinning longitude spacing degrees
REAL             :: ThinningDTime = 3600 ! thinning timescale in seconds
REAL             :: ThinningDz = 10.0    ! thinning depth spacing in metres

LOGICAL          :: SolarZenithIgnoreOutOfArea = .TRUE.
LOGICAL          :: userttov13defaults = .FALSE.

CONTAINS

INCLUDE 'Ops_CheckPrevUsed.inc'
INCLUDE 'Ops_ExtremeCheck.inc'
INCLUDE 'Ops_SetupModelLevelObs.inc'

END MODULE OpsMod_Process
