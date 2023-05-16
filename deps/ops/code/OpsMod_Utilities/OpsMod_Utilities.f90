!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Contains low-level utility routines.
!-------------------------------------------------------------------------------

MODULE OpsMod_Utilities

IMPLICIT NONE

SAVE

! Public declarations:

CONTAINS

INCLUDE 'Ops_Cholesky.inc'
INCLUDE 'Ops_CrossProduct.inc'
INCLUDE 'Ops_DivideIntoBatches.inc'
INCLUDE 'Ops_DotProduct.inc'
INCLUDE 'Ops_FindNextNamelist.inc'
INCLUDE 'Ops_HeightToPressure.inc'
INCLUDE 'Ops_LatLon_to_XYZ.inc'
INCLUDE 'Ops_LEQWildCard.inc'
INCLUDE 'Ops_LinearInterp.inc'
INCLUDE 'Ops_LongLatTo3d.inc'
INCLUDE 'Ops_Orbit_Angle.inc'
INCLUDE 'Ops_Qsat.inc'
INCLUDE 'Ops_QsatDew.inc'
INCLUDE 'Ops_QsatWat.inc'
INCLUDE 'Ops_Refractivity.inc'
INCLUDE 'Ops_RefractivityDeriv.inc'
INCLUDE 'Ops_RotateCoordinates.inc'
INCLUDE 'Ops_SatOrbitAngle.inc'
INCLUDE 'Ops_Share.inc'
INCLUDE 'Ops_ThinObs4D.inc'
INCLUDE 'Ops_UVtoSD.inc'
INCLUDE 'Ops_VectorMod.inc'
INCLUDE 'Ops_WCoeff.inc'
INCLUDE 'Ops_WEq_to_ll.inc'
INCLUDE 'Ops_WorkingCoordinates.inc'

END MODULE OpsMod_Utilities
