!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! This module DateTime_type and related constants and procedures.
!-------------------------------------------------------------------------------

MODULE OpsMod_DateTime

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

IMPLICIT NONE

SAVE

! Public declarations:

!--------------------------------------
! DataTime data type
!--------------------------------------

TYPE DateTime_type
  INTEGER          :: year = IMDI
  INTEGER          :: month = IMDI
  INTEGER          :: day = IMDI
  INTEGER          :: hour = IMDI
  INTEGER          :: minute = IMDI
  INTEGER          :: second = IMDI
  INTEGER          :: diff_from_utc = IMDI
END TYPE DateTime_type

INTEGER, PARAMETER :: minutes_in_day = 1440
INTEGER, PARAMETER :: minutes_in_year = 525600
INTEGER, PARAMETER :: seconds_in_day = minutes_in_day * 60
INTEGER, PARAMETER :: seconds_in_hour = 3600

INTEGER, PARAMETER :: month_table(12) = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)

INTERFACE OpsFn_DateTime_Create
  MODULE PROCEDURE OpsFn_DateTime_CreateFromMinUTC
  MODULE PROCEDURE OpsFn_DateTime_CreateFromVars
  MODULE PROCEDURE OpsFn_DateTime_FromMinUTCArray
END INTERFACE OpsFn_DateTime_Create

INTERFACE OpsFn_DateTime_ToMinUTC
  MODULE PROCEDURE OpsFn_DateTime_ToMinUTCArray
  MODULE PROCEDURE OpsFn_DateTime_ToMinUTCDateTime
  MODULE PROCEDURE OpsFn_DateTime_ToMinUTCVars
  MODULE PROCEDURE OpsFn_DateTime_ToMinUTCVarsArray
END INTERFACE OpsFn_DateTime_ToMinUTC

CONTAINS

INCLUDE 'OpsFn_DateTime_Create.inc'
INCLUDE 'OpsFn_DateTime_now.inc'
INCLUDE 'OpsFn_DateTime_ToMinUTC.inc'

END MODULE OpsMod_DateTime
