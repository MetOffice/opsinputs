!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Constants used by utility programs.
!-------------------------------------------------------------------------------

MODULE OpsMod_UtilsProgs

IMPLICIT NONE

SAVE

! Public declarations:
INTEGER            :: MaxNumObs = 10
INTEGER            :: MaxNumLevels = 5
INTEGER            :: MaxVarsPerLine = 10
INTEGER            :: ACNumMeta = 5
INTEGER, PARAMETER :: ModelObType = 68
INTEGER, PARAMETER :: MaxLenCommandLine = 500

CONTAINS

INCLUDE 'Ops_PrintFixedHeader.inc'

END MODULE OpsMod_UtilsProgs
