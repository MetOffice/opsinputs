!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Contains generic Quality Control code:
!   low-level background check routines
!   buddy check: sorting, pairing and actual buddy check routines
!-------------------------------------------------------------------------------

MODULE OpsMod_QC

IMPLICIT NONE

SAVE

! Public declarations:
INTEGER           :: QcMode = 0                    ! QC output level - as for GeneralMode

! Background check
REAL              :: PGECrit = 0.5                 ! Rejection limit
REAL              :: ExpArgMax = 80.0              ! Max argument for EXP function

! Following values are for ocean model (track check also Surf/Sonde)
REAL             :: TrackDistTol = 20.0           ! km  Minimum displacement to check
REAL             :: MaxShipSpeed = 15.0           ! m/s MaxSpeed for ship track check
REAL             :: MaxBuoySpeed = 2.0            ! m/s MaxSpeed for buoy track check

! Scaling factors for horizontal correlation scale

! 1st box:  Box(1) <= latitude <= Box(2), Box(3) <= longitude <= Box(4)

INTEGER          :: HistBeforeSynop =  0          ! (27 suggested if used)
INTEGER          :: HistBeforeShip = 51           ! need longer window for ships
INTEGER          :: HistBeforeBuoy = 27
INTEGER          :: HistBeforeTemp = 51           ! need longer window for tempships
INTEGER          :: HistBeforeOcean = 51
INTEGER          :: HistAfterMax = 9              ! atmos, use 15 for Ocean?
INTEGER          :: NStuckTol = 6                 ! Stuck flags if at least NStuckTol
REAL             :: StuckTolP = 12.0              ! identical values spanning at least
REAL             :: StuckTolTRH = 12.0            ! StuckTol? hours
REAL             :: StuckTolWind = 24.0
REAL             :: StuckTolSST = 36.0
LOGICAL          :: IgnoreStuckCalm = .FALSE.     ! Ignore stuck calm wind speeds?

END MODULE OpsMod_QC
