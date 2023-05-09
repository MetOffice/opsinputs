!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Contains constants for use with RTTOV
!-------------------------------------------------------------------------------

MODULE OpsMod_SatRad_RTmodel

IMPLICIT NONE

SAVE

!Copy of nlevels_strat which isn't a pointer to be used after SatRad
INTEGER :: nlevels_strat_varobs

END MODULE OpsMod_SatRad_RTmodel
