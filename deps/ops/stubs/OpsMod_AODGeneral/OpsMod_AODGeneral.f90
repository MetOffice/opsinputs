!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! AOD code and variables used by both OPS and VAR.
!-------------------------------------------------------------------------------

MODULE OpsMod_AODGeneral

IMPLICIT NONE

SAVE

! Public declarations:

INTEGER :: NDustBins = 2        ! Number of model dust bins 2:Global, 6:CAM
INTEGER :: NAODWaves = 1        ! Number of AOD obs wavelengths

END MODULE OpsMod_AODGeneral
