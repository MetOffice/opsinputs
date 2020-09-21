!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Module containing procedures for CX output.
!-------------------------------------------------------------------------------

MODULE OpsMod_ModelColumnIO

IMPLICIT NONE

SAVE

CONTAINS

INCLUDE 'Ops_CloseModelColumn.inc'
INCLUDE 'Ops_MaxCXIndexes.inc'
INCLUDE 'Ops_OpenModelColumn.inc'
INCLUDE 'Ops_SetupCxHeader.inc'
INCLUDE 'Ops_SetupModelColumn.inc'
INCLUDE 'Ops_SetupModelColumn1pe.inc'
INCLUDE 'Ops_WriteModelColumn.inc'
INCLUDE 'Ops_WriteModelColumn1pe.inc'
INCLUDE 'Ops_WriteOutVarCx.inc'
INCLUDE 'Ops_WriteOutVarCx1pe.inc'

END MODULE OpsMod_ModelColumnIO
