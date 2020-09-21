!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Declares variables and parameters associated with radar obs group.
!-------------------------------------------------------------------------------

MODULE OpsMod_Radar

IMPLICIT NONE

SAVE

! needed for Family and new parallelisation in VAR
LOGICAL              :: RadFamily = .FALSE.

! Control process namelist
! ------------------------
LOGICAL                   :: RadWind_SuperOb =.FALSE. ! SuperObbing flag
LOGICAL                   :: RadPerformSuperObservation = .FALSE.

END MODULE OpsMod_Radar
