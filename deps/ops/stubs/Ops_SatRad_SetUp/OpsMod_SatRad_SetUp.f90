!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Namelist definitions and data file information for satellite radiance
! processing.
!-------------------------------------------------------------------------------

MODULE OpsMod_SatRad_SetUp

IMPLICIT NONE

SAVE

!----------------------------------------------------
!Note defaults for the following namelist variables
!are declared in Ops_SatRad_InitNLVariables.f90
!----------------------------------------------------

!---------------------------------
!5. Processing option namelists
!---------------------------------
!These namelists contain information that will apply to all satellites in a
!given run. All variables must be given default values, in
!Ops_SatRad_InitNLVariables.f90, but some of these will be overwritten in
!Ops_SatRad_SetUpControl.f90 before the namelist is read in because we need
!different settings for different observation types.

!If adding any switches to these namelists, remember to update
!Ops_SatRad_InitNLVariables and also Ops_SatRad_SetUpControl if there are any
!observation specific issues to consider.

!----
!5.1) General Processing Options
!----

LOGICAL :: VarBC                   !master switch in OPS (not used in VAR)
LOGICAL :: CloudErrorModel         !Use IR cloud error model
LOGICAL :: RTTOV_CloudSwitch       !Activates cloudy simulation for RTTOV-9 onwards

END MODULE OpsMod_SatRad_SetUp
