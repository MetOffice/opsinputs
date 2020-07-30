!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Interfaces for some MPP obs utilities.
!-------------------------------------------------------------------------------

MODULE OpsMod_GatherSpread

IMPLICIT NONE

SAVE

LOGICAL :: gather_by_batch = .TRUE.

! Interface blocks:

INTERFACE Ops_Gather
  MODULE PROCEDURE Ops_GatherChar1D
  MODULE PROCEDURE Ops_GatherInteger1D
  MODULE PROCEDURE Ops_GatherInteger2D
  MODULE PROCEDURE Ops_GatherReal1D
  MODULE PROCEDURE Ops_GatherReal2D
END INTERFACE

INTERFACE Ops_Spread
  MODULE PROCEDURE Ops_SpreadInteger1D
  MODULE PROCEDURE Ops_SpreadInteger2D
  MODULE PROCEDURE Ops_SpreadReal1D
  MODULE PROCEDURE Ops_SpreadReal2D
END INTERFACE

CONTAINS

#include "Ops_GatherChar1D.inc"
#include "Ops_GatherInteger1D.inc"
#include "Ops_GatherInteger2D.inc"
#include "Ops_GatherReal1D.inc"
#include "Ops_GatherReal2D.inc"

INCLUDE 'Ops_SpreadInteger1D.inc'
INCLUDE 'Ops_SpreadInteger2D.inc'
INCLUDE 'Ops_SpreadReal1D.inc'
INCLUDE 'Ops_SpreadReal2D.inc'

END MODULE OpsMod_GatherSpread
