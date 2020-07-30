!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Module to contain sorting routines for use in the OPS
!-------------------------------------------------------------------------------

MODULE OpsMod_Sort

IMPLICIT NONE

SAVE

! Interface blocks:

INTERFACE Ops_RealSearch
  MODULE PROCEDURE Ops_RealSearchIndex
  MODULE PROCEDURE Ops_RealSearchNoIndex
END INTERFACE

INTERFACE Ops_Sort
  MODULE PROCEDURE Ops_RealSortQuick
  MODULE PROCEDURE Ops_IntegerSortQuick
  MODULE PROCEDURE Ops_CharSortQuick
END INTERFACE

CONTAINS

INCLUDE 'Ops_CharSearch.inc'
INCLUDE 'Ops_CharSort.inc'
INCLUDE 'Ops_CharSortQuick.inc'
INCLUDE 'Ops_InitSort.inc'
INCLUDE 'Ops_InitSortMask.inc'
INCLUDE 'Ops_IntegerSearch.inc'
INCLUDE 'Ops_IntegerSort.inc'
INCLUDE 'Ops_IntegerSortQuick.inc'
INCLUDE 'Ops_OneCharSort.inc'
INCLUDE 'Ops_RealSort.inc'
INCLUDE 'Ops_RealSortQuick.inc'
INCLUDE 'Ops_RealSearchIndex.inc'
INCLUDE 'Ops_RealSearchNoIndex.inc'

END MODULE OpsMod_Sort
