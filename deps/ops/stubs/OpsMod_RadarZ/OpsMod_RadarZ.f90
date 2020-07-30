!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Declares variables and parameters associated with radarZ obs group
!-------------------------------------------------------------------------------

MODULE OpsMod_RadarZ

IMPLICIT NONE

SAVE

! Radar reflectivity information
LOGICAL            :: RadRefl_DirectAssim = .FALSE. ! DirectAssim flag, false - indirect assimilation

END MODULE OpsMod_RadarZ

