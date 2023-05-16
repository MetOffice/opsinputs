!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Procedures to perform linear, cubic Lagrange or quintic lagrange vertical
! interpolations on 3D fields.
!-------------------------------------------------------------------------------

MODULE OpsMod_VerticalInterp

IMPLICIT NONE

SAVE

CONTAINS

INCLUDE "Ops_VerticalInterp.inc"
INCLUDE "Ops_VerticalInterp1D.inc"
INCLUDE "Ops_VerticalInterpCubic.inc"
INCLUDE "Ops_VerticalInterpLinWts.inc"
INCLUDE "Ops_VerticalInterpLinear.inc"
INCLUDE "Ops_VerticalInterpQuintic.inc"
INCLUDE 'Ops_VertInterpCxUV2ObHeight.inc'

END MODULE OpsMod_VerticalInterp
