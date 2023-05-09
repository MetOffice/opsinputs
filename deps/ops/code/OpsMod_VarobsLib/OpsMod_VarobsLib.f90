!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Definition of varobs_type.
!-------------------------------------------------------------------------------

MODULE OpsMod_VarobsLib

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE GenMod_ModelIO, ONLY: &
  LenFixhd

IMPLICIT NONE

SAVE

! Public declarations:

TYPE varobs_type
  INTEGER                            :: fixhd(LenFixHd)
  INTEGER, ALLOCATABLE               :: intc(:)
  REAL, ALLOCATABLE                  :: realc(:)
  REAL, ALLOCATABLE                  :: coldepc(:)
  REAL, ALLOCATABLE                  :: levdepc(:)
  INTEGER, ALLOCATABLE               :: lookup(:,:)
  INTEGER                            :: unit_num = IMDI
  INTEGER                            :: num_data = IMDI
  INTEGER                            :: num_meta = IMDI
CONTAINS
  PROCEDURE                          :: alloc => Ops_AllocVarobsHdr
  PROCEDURE                          :: dealloc => Ops_DeallocVarobsHdr
  PROCEDURE                          :: get_varfields => Ops_GetVarfieldsInfo
  PROCEDURE                          :: read_batch => Ops_ReadVarobsBatch
  PROCEDURE                          :: read_head => Ops_ReadVarobsHdr
  PROCEDURE                          :: spread_head => Ops_SpreadVarobsHdr
  PROCEDURE                          :: write_batch => Ops_WriteVarobsBatch
  PROCEDURE                          :: write_head => Ops_WriteVarobsHdr
END TYPE varobs_type

CONTAINS

INCLUDE 'Ops_AllocVarobsHdr.inc'
INCLUDE 'Ops_DeallocVarobsHdr.inc'
INCLUDE 'Ops_GetVarfieldsInfo.inc'
INCLUDE 'Ops_ReadVarobsBatch.inc'
INCLUDE 'Ops_ReadVarobsHdr.inc'
INCLUDE 'Ops_SpreadVarobsHdr.inc'
INCLUDE 'Ops_WriteVarobsBatch.inc'
INCLUDE 'Ops_WriteVarobsHdr.inc'

END MODULE OpsMod_VarobsLib
