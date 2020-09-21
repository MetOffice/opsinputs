!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! UM header type declaration.
!-------------------------------------------------------------------------------
MODULE GenMod_ModelIO

IMPLICIT NONE

! Public declarations:

INTEGER, PARAMETER :: LenFixHd = 256   ! Length of Fixed Length Header.

TYPE UM_header_type
  INTEGER          :: NumFlds          ! Number of data fields.
  INTEGER          :: MaxFldSize       ! Maximum (unpacked) size of field.
  INTEGER          :: UnitNum          ! Unit number associated with UM dump.
  INTEGER          :: FixHd(LenFixHd)  ! Fixed length header.
  INTEGER, POINTER :: IntC(:)      => NULL() ! Integer Constants array.
  INTEGER, POINTER :: CompFldI1(:) => NULL() ! Compressed Field Index array 1.
  INTEGER, POINTER :: CompFldI2(:) => NULL() ! Compressed Field Index array 2.
  INTEGER, POINTER :: CompFldI3(:) => NULL() ! Compressed Field Index array 3.
  INTEGER, POINTER :: Lookup(:,:)  => NULL() ! Lookup table.
  REAL, POINTER    :: RealC(:)     => NULL() ! Real Constants array.
  REAL, POINTER    :: LevDepC(:)   => NULL() ! Level Dependent Constants array.
  REAL, POINTER    :: RowDepC(:)   => NULL() ! Row Dependent Constants array.
  REAL, POINTER    :: ColDepC(:)   => NULL() ! Column Dependent Constants array.
  REAL, POINTER    :: FldsOfC(:)   => NULL() ! Field Dependent Constants array.
  REAL, POINTER    :: ExtraC(:)    => NULL() ! Extra Constants array.
CONTAINS
  PROCEDURE        :: alloc => Gen_AllocUMhdr
  PROCEDURE        :: check_lookup => Ops_CheckLookup
  PROCEDURE        :: dealloc => Gen_DeallocUMhdr
  PROCEDURE        :: max_field_size => Gen_CalcMaxFldSize
  PROCEDURE        :: print_lookup => Ops_PrintLookup
  PROCEDURE        :: read => Gen_ReadUMhdr
  PROCEDURE        :: read_field => Gen_ReadField
  PROCEDURE        :: spread => Gen_SpreadUMhdr
  PROCEDURE        :: write_field => Gen_WriteField
  PROCEDURE        :: write => Gen_WriteUMhdr
END TYPE UM_header_type

CONTAINS

INCLUDE 'Gen_AllocUMhdr.inc'
INCLUDE 'Gen_DeallocUMhdr.inc'
INCLUDE 'Gen_ReadField.inc'
INCLUDE 'Gen_CalcMaxFldSize.inc'
INCLUDE 'Gen_ReadUMhdr.inc'
INCLUDE 'Gen_WriteField.inc'
INCLUDE 'Gen_WriteUMhdr.inc'
INCLUDE 'Ops_CheckLookup.inc'
INCLUDE 'Ops_PrintLookup.inc'
INCLUDE 'Ops_SetDumpfileAddress.inc'

#include "Gen_SpreadUMhdr.inc"

END MODULE GenMod_ModelIO
