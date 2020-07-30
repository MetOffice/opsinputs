!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Contains shared_memory object for storeing the enitire model field. It has
! the SAVE attribute so that it remians in memory until it destroyed.
!
! Included in the module are two public routines:
! 1) OPS_InitFieldData - inilise and read the model state into memory
! Inputs: 
!   domain: contains the infomation required to get the header data and read the
!       fields.
!   FieldsInfo: information about fields in UM dump, prepared in 
!       Ops_CXSetupArrays
!   CxArrays: used to get the number of levels in each required field
!   ForecastTimes: define the time slices to be read
!   ModelTimeUTC: Validity time UTC in the UM dump.
! Output:
!   shared_memory: object created and available via SAVE attribute
! OPS standards state this should be cammel case:  FieldDataShared
!
! Included in the module are two private routines:
! 1) GetReqDims - used by OPS_InitFieldData to get the dimensions of the
! shared memory
! 2) ReadModelShared - used by OPS_InitFieldData to read the data into the 
! shared memory
!-------------------------------------------------------------------------------

MODULE OpsMod_FieldDataShared

USE OpsMod_SharedMemory, ONLY: &
    SharedMemory_type

IMPLICIT NONE

SAVE

TYPE (SharedMemory_type) :: FieldDataShared

CONTAINS

INCLUDE 'Ops_FinalFieldDataShared.inc'

END MODULE OpsMod_FieldDataShared
