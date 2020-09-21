!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Common control variables.
!-------------------------------------------------------------------------------

MODULE GenMod_Control

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI,                                  &
  RMDI

USE, INTRINSIC :: ISO_C_BINDING, ONLY: &
  C_INT64_T

IMPLICIT NONE

INTEGER, PARAMETER                 :: LenDocumentationURL = 100
CHARACTER(len=LenDocumentationURL) :: DocumentationURL = '.'
LOGICAL                            :: ProduceHTML = .TRUE.
INTEGER                            :: mype = 0 ! This processor's number:  0 <= mype < NPROC.
INTEGER                            :: nproc = 1  ! Number of processors
INTEGER                            :: GlobalGroup = IMDI ! Never used if not running MPI

! The following are the allowed settings of GeneralMode:

INTEGER, PARAMETER                 :: OperationalMode = 0
INTEGER, PARAMETER                 :: QuietMode = 0
INTEGER, PARAMETER                 :: ProductionMode  = 10 ! for non-operational production
INTEGER, PARAMETER                 :: NormalMode = 10      ! for normal running
INTEGER, PARAMETER                 :: DiagnosticMode  = 20 ! for program development
INTEGER, PARAMETER                 :: DebugMode = 30       ! for testing and debugging
INTEGER, PARAMETER                 :: VerboseMode = 40     ! for detailed program tracing
INTEGER                            :: GeneralMode = DiagnosticMode ! mode of run
INTEGER                            :: GenMode = DiagnosticMode     ! mode of GEN code.

! Config namelist options:
INTEGER                            :: WarningsMax = IMDI
LOGICAL                            :: GCOMBitRep = .FALSE.
REAL                               :: ErrorSleep = 2.5          ! Seconds to sleep when exiting program on error
LOGICAL                            :: WarningToError = .FALSE.  ! Convert calls to gen_warn to calls to gen_fail
LOGICAL                            :: StatsAppend = .FALSE.     ! Allow capability to revert to old behaviour

INTEGER                            :: UM_SECTOR_SIZE = 2048

! Dummy values to initialise data
REAL                               :: RUDI = RMDI / 10 ! Real Undefined Data Indicator
INTEGER                            :: IUDI = IMDI + 10 ! Integer Undefined Data Indicator

#ifdef SET_L_UDI
LOGICAL                            :: L_RUDI = .TRUE. ! Should we use rudi/iudi
LOGICAL                            :: L_IUDI = .TRUE.
#else
LOGICAL                            :: L_RUDI = .FALSE. ! Should we use rudi/iudi
LOGICAL                            :: L_IUDI = .FALSE.
#endif

CONTAINS

INCLUDE 'GenFn_UMSectorLen.inc'

END MODULE GenMod_Control
