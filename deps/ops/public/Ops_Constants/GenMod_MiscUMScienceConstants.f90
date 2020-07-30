!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Miscellaneous science constants from the UM.
!-------------------------------------------------------------------------------
MODULE GenMod_MiscUMScienceConstants

IMPLICIT NONE

!*L------------------COMDECK C_O_DG_C-----------------------------------
! ZERODEGC IS CONVERSION BETWEEN DEGREES CELSIUS AND KELVIN
! TFS IS TEMPERATURE AT WHICH SEA WATER FREEZES
! TM IS TEMPERATURE AT WHICH FRESH WATER FREEZES AND ICE MELTS

Real, Parameter :: ZeroDegC = 273.15
Real, Parameter :: TFS      = 271.35
Real, Parameter :: TM       = 273.15

!*----------------------------------------------------------------------

!*L------------------COMDECK C_R_CP-------------------------------------

! R IS GAS CONSTANT FOR DRY AIR
! CP IS SPECIFIC HEAT OF DRY AIR AT CONSTANT PRESSURE
! PREF IS REFERENCE SURFACE PRESSURE

Real, Parameter  :: R      = 287.05
Real, Parameter  :: CP     = 1005.
Real, Parameter  :: Kappa  = R/CP
Real, Parameter  :: Pref   = 100000.

! Reference surface pressure = PREF
Real, Parameter  :: P_zero = Pref
!*----------------------------------------------------------------------

!*L------------------COMDECK C_PI---------------------------------------

! Pi
Real, Parameter :: Pi                 = 3.14159265358979323846

! Conversion factor degrees to radians
Real, Parameter :: Pi_Over_180        = Pi/180.0

! Conversion factor radians to degrees
Real, Parameter :: Recip_Pi_Over_180  = 180.0/Pi

!*----------------------------------------------------------------------

Real, Parameter    :: RMDI_PP  = -1.0E+30

! Old real missing data indicator (-32768.0)
Real, Parameter    :: RMDI_OLD = -32768.0

! New real missing data indicator (-2**30)
Real, Parameter    :: RMDI     = -32768.0*32768.0

! Integer missing data indicator
Integer, Parameter :: IMDI     = -32768
!*----------------------------------------------------------------------
! C_LHEAT start

! latent heat of condensation of water at 0degc
REAL,PARAMETER:: LC=2.501E6

 ! latent heat of fusion at 0degc
REAL,PARAMETER:: LF=0.334E6

! C_LHEAT end
!*L------------------COMDECK C_G----------------------------------------
! G IS MEAN ACCEL DUE TO GRAVITY AT EARTH'S SURFACE

Real, Parameter :: G = 9.80665

!*----------------------------------------------------------------------
!*L------------------COMDECK C_EPSLON-----------------------------------
! EPSILON IS RATIO OF MOLECULAR WEIGHTS OF WATER AND DRY AIR

Real, Parameter :: Epsilon   = 0.62198
Real, Parameter :: C_Virtual = 1./Epsilon-1.

!*----------------------------------------------------------------------

!*L------------------COMDECK C_A----------------------------------------

! Mean radius of Earth in m.
Real, Parameter  :: Earth_Radius = 6371229.

!*----------------------------------------------------------------------

END MODULE GenMod_MiscUMScienceConstants
