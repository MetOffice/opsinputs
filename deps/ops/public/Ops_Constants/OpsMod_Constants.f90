!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Module to store physical constants and other global parameters.
!-------------------------------------------------------------------------------

MODULE OpsMod_Constants

USE GenMod_MiscUMScienceConstants, ONLY: &
    Pi,                                  &
    ZERODEGC

IMPLICIT NONE

SAVE

! Public declarations:

!-------------------------------------------------------------------------------
! 1. Define constants
!-------------------------------------------------------------------------------

REAL, PARAMETER              :: Root2 = 1.414213562373095 ! square root of 2
REAL, PARAMETER              :: Tc_to_Tk = ZERODEGC       ! Celsius to Kelvins conversion
REAL, PARAMETER              :: Lapse_RateL = 6.5E-03     ! Lapse rate for levels up to up to 11,000 gpm
REAL, PARAMETER              :: Lapse_RateU = -1.0E-03    ! Lapse rate for levels up to up to 11,000 gpm
REAL, PARAMETER              :: Nalpha = 77.6             ! Refractivity constant a
REAL, PARAMETER              :: Nbeta  = 3.73E5           ! Refractivity constant b
REAL, PARAMETER              :: Press_Bot = 1013.25       ! Assumed surface pressure
REAL, PARAMETER              :: Press_Mid = 226.32        ! Assumed pressure at 11,000 gpm
REAL, PARAMETER              :: Press_Top = 54.7487       ! Assumed pressure at 20,000 gpm
REAL, PARAMETER              :: Press_Trop = 200.0        ! Assumed pressure of tropapause
REAL, PARAMETER              :: Temp_Bot = 288.15         ! Surface temperature
REAL, PARAMETER              :: Temp_Top = 216.65         ! Temperature of isothermal layer
REAL, PARAMETER              :: Gpm1 = 11000.0            ! Height limit (gpm) for assumed lower lapse rate
REAL, PARAMETER              :: Gpm2 = 20000.0            ! Height (gpm) of top of isothermal layer
REAL, PARAMETER              :: Feet_to_ms = 0.3048       ! Conversion factor for feet to m/s
REAL, PARAMETER              :: Knots_to_ms = .514444     ! Knots to m/s conversion factor
REAL, PARAMETER              :: MetdbMDI = -9999999.0     ! Metdb missing data indicator
REAL, PARAMETER              :: PPF = 1000.0              ! PGE packing factor
REAL, PARAMETER              :: PGEMDI = 1.111            ! Missing data for packed PGEs
REAL, PARAMETER              :: Eta_Ref = 0.795
REAL, PARAMETER              :: Sea_Freeze = 271.15       ! freezing point of sea water
REAL, PARAMETER              :: deg2rad = Pi / 180.0      ! Convert degrees to radians
REAL, PARAMETER              :: rad2deg = 180.0 / Pi      ! Convert radians to degrees

! constants relating to WGS-84 ellipsoid and gravity above ellipsoid
REAL, PARAMETER              :: ecc = 0.081819            ! eccentricity
REAL, PARAMETER              :: k_somig = 1.931853E-3     ! Somigliana's constant
REAL, PARAMETER              :: g_equat = 9.7803253359    ! equatorial gravity (ms-2)
REAL, PARAMETER              :: a_earth = 6378.137E3      ! semi-major axis of earth (m)
REAL, PARAMETER              :: flatt = 0.003352811       ! flattening
REAL, PARAMETER              :: m_ratio= 0.003449787      ! gravity ratio

! Maximum values of RH_ice for temperatures 0 to -40 deg C
! (100*qsat(water)/qsat(ice))
REAL, PARAMETER             :: rhmax(0:40) =  &
   (/ 100.00, 100.98, 101.97, 102.96, 103.97, &
      104.99, 106.01, 107.05, 108.10, 109.16, &
      110.23, 111.31, 112.40, 113.51, 114.62, &
      115.75, 116.88, 118.03, 119.19, 120.36, &
      121.54, 122.74, 123.94, 125.15, 126.38, &
      127.62, 128.87, 130.12, 131.39, 132.67, &
      133.96, 135.26, 136.58, 137.90, 139.23, &
      140.57, 141.92, 143.27, 144.64, 146.02, 147.40 /)

CHARACTER(len=16), PARAMETER :: CMDI = 'XXXXXXXX'

END MODULE OpsMod_Constants
