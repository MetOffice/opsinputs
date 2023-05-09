!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Contains Ops_ReadUMField and a few helper routines.
!-------------------------------------------------------------------------------

MODULE OpsMod_FieldRead

IMPLICIT NONE

SAVE

CONTAINS

INCLUDE 'Ops_GetUMFieldDims.inc'
INCLUDE 'Ops_HorizontalExtend.inc'
INCLUDE 'Ops_ReadUMField.inc'
INCLUDE 'Ops_UncompressDump.inc'

END MODULE OpsMod_FieldRead
