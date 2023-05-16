!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Module to contain variables and routines specific to Ground GPS.
!-------------------------------------------------------------------------------

MODULE OpsMod_GroundGPS

IMPLICIT NONE

SAVE

! Public declarations:

!-------------------------------------------------------------------------------
! 1. Process control namelist parameters
!-------------------------------------------------------------------------------

! GroundGPSOperator 2 is the generic refractivity forward operator for
! Ground Based GNSS Zenith Total Delay.
! This is the newest version of the operator and is used in both the
! Global and the UKV model as of 04/05/2022. In operations this value
! is controlled in the ops_process_groundgps/rose-app.conf. 

INTEGER :: GroundGPSOperator = 2

END MODULE OpsMod_GroundGPS
