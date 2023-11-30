!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Mapping of obsgroup, satid and report types.
!-------------------------------------------------------------------------------

MODULE OpsMod_ODBReportTypeMap

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

IMPLICIT NONE

SAVE

INTEGER, ALLOCATABLE :: map(:,:)
LOGICAL              :: have_read_map = .FALSE.

CONTAINS

INCLUDE 'Ops_ReadODBReportTypeMap.inc'

END MODULE OpsMod_ODBReportTypeMap
