!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Provides constants related to horizontal interpolation.
!-------------------------------------------------------------------------------

MODULE OpsMod_HorizontalInterp

IMPLICIT NONE

SAVE

! Public declaration:

REAL              :: rpi = 3.141592653589793           !: pi
REAL              :: rad = 3.141592653589793 / 180.0   !: degrees to radians conversion

INTEGER, PARAMETER :: num_extend_points = 3    ! extra points for extended model field

INTEGER, PARAMETER :: interp_type_linear_coast = 1
INTEGER, PARAMETER :: interp_type_nearest = 2

CONTAINS

INCLUDE 'OpsFn_Apply2DOceanWeights.inc'
INCLUDE 'OpsFn_ApplyProfileOceanWeights.inc'
INCLUDE 'OpsFn_ApplySurfaceOceanWeights.inc'
INCLUDE 'Ops_BilinearRemappingWeights.inc'
INCLUDE 'Ops_GreatCircleDistance.inc'
INCLUDE 'Ops_GreatCircleDistWeights.inc'
INCLUDE 'Ops_HorizontalGridStretch.inc'
INCLUDE 'Ops_HorizontalInterp.inc'
INCLUDE 'Ops_HorizontalInterp_OceanWeights.inc'
INCLUDE 'Ops_HorizontalInterp_Weights.inc'
INCLUDE 'Ops_HorizontalInterpFromNetCDF.inc'
INCLUDE 'Ops_HorizontalInterpLinearCoast.inc'
INCLUDE 'Ops_HorizontalInterpNearest.inc'
INCLUDE 'Ops_HorizontalInterpProfile.inc'
INCLUDE 'Ops_TimeInterp_OceanWeights.inc'

END MODULE OpsMod_HorizontalInterp
