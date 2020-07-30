!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Module variables and procedures used for air quality processing.
!-------------------------------------------------------------------------------

MODULE OpsMod_AirQuality

IMPLICIT NONE

SAVE

! AIRQAL CSNT_TYP constants:
INTEGER, PARAMETER :: SO2_CSNT = 8
INTEGER, PARAMETER :: PM10_CSNT = 27
INTEGER, PARAMETER :: PM2p5_CSNT = 26
INTEGER, PARAMETER :: O3_CSNT = 0
INTEGER, PARAMETER :: NO2_CSNT = 5
INTEGER, PARAMETER :: CO_CSNT = 4

CONTAINS

INCLUDE 'Ops_ReadSiteQuality.inc'

END MODULE OpsMod_AirQuality
