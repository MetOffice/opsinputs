!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Miscellaneous utilities.
!-------------------------------------------------------------------------------

MODULE GenMod_Utilities

INTERFACE Gen_DateToMinUTC
  MODULE PROCEDURE Gen_DateToMinUTC_scalar
  MODULE PROCEDURE Gen_DateToMinUTC_array
END INTERFACE

INTERFACE Gen_MinUTCToDate
  MODULE PROCEDURE Gen_MinUTCToDate_scalar
  MODULE PROCEDURE Gen_MinUTCToDate_array
END INTERFACE

INTEGER, PARAMETER :: MinutesInDay=1440
INTEGER, PARAMETER :: MinutesInYear=525600

INTEGER :: MonthTab(12) = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)

CONTAINS

INCLUDE 'Gen_DateToMinUTC_scalar.inc'
INCLUDE 'Gen_MinUTCToDate_scalar.inc'
INCLUDE 'Gen_DateToMinUTC_array.inc'
INCLUDE 'Gen_MinUTCToDate_array.inc'

INCLUDE 'Gen_Eq_to_LatLon.inc'
INCLUDE 'Gen_LatLon_to_Eq.inc'
INCLUDE 'Gen_GreatCircleDistance.inc'

END MODULE GenMod_Utilities
