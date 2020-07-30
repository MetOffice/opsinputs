!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Contains utilities for manipulating ob_type structure elements.
!-------------------------------------------------------------------------------

MODULE OpsMod_ObTypeUtils

IMPLICIT NONE

SAVE

CONTAINS

INCLUDE 'Ops_FlightLevel.inc'
INCLUDE 'Ops_LCL.inc'
INCLUDE 'Ops_MixingRatioToRH.inc'
INCLUDE 'Ops_RotateWinds.inc'
INCLUDE 'Ops_Solar_Zenith.inc'
INCLUDE 'Ops_TdToRH.inc'
INCLUDE 'Ops_TtoTheta.inc'
INCLUDE 'Ops_UnRotateWinds.inc'
INCLUDE 'Ops_UV.inc'

END MODULE OpsMod_ObTypeUtils
