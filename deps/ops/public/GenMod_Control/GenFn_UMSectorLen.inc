!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Returns the length of data needed to write data of length "in_len" for UM
! well-formed IO based on UM_SECTOR_SIZE.
!-------------------------------------------------------------------------------

PURE FUNCTION GenFn_UMSectorLen (in_len)

IMPLICIT NONE

! Function arguments:
INTEGER, INTENT(IN) :: in_len

! Function result:
INTEGER             :: GenFn_UMSectorLen

GenFn_UMSectorLen = ((in_len + UM_SECTOR_SIZE) / UM_SECTOR_SIZE) * UM_SECTOR_SIZE

END FUNCTION GenFn_UMSectorLen
