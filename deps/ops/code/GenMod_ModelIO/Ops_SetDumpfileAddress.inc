!-----------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-----------------------------------------------------------------------
! To set the LBEGIN and LBNREC fields in the LOOKUP Headers for VN 16
! Type Dumpfiles - addressed by location and length which are rounded up
! the 'UM_SECTOR_SIZE' to make Well-Formed I/O Requests.
!-------------------------------------------------------------------

SUBROUTINE Ops_SetDumpfileAddress (fixhd,       &
                                   len_fixhd,   &
                                   lookup,      &
                                   len1_lookup, &
                                   len2_lookup)

USE GenMod_CLookAdd, ONLY: &
  LBEGIN,                  &
  LBLREC,                  &
  LBNREC,                  &
  LBPACK,                  &
  LBYR

USE GenMod_Control, ONLY: &
  um_sector_size

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_UMHeaderConstants, ONLY: &
  FH_DataStart

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)         :: len_fixhd                       ! Length of fixed length header
INTEGER, INTENT(INOUT)      :: fixhd(len_fixhd)                ! Fixed length header
INTEGER, INTENT(IN)         :: len1_lookup                     ! 1st dim of lookup
INTEGER, INTENT(IN)         :: len2_lookup                     ! 2nd dim of lookup
INTEGER, INTENT(INOUT)      :: lookup(len1_lookup,len2_lookup) ! lookup tables

! Local declarations:
INTEGER                     :: disk_length                     ! current data length on disk
INTEGER                     :: i                               ! Loop Index
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_SetDumpfileAddress"
INTEGER                     :: disk_address                    ! Current rounded disk address and final data length

IF (fixhd(FH_DataStart) < 0) RETURN

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! check that the initial data address has been rounded up
! to a sector boundary - REMEMBER all the code removes
! one from this address because addresses start at zero.

IF ((fixhd(FH_DataStart) - 1) /= (((fixhd(FH_DataStart) - 1) + um_sector_size - 1) / um_sector_size) * um_sector_size) THEN

  ! round up the initial disk address

  fixhd(FH_DataStart) = (((fixhd(FH_DataStart) - 1) + um_sector_size - 1) / um_sector_size) * um_sector_size + 1

END IF

! find the initial data location on disk

disk_address = fixhd(FH_DataStart) - 1

! loop over all the entries and alter the addresses and lengths

DO i = 1, len2_lookup

  ! check for a PP type file with an incomplete lookup table

  IF (lookup(LBYR,i) == -99) GOTO 9999

  ! check for packing to 32-bits

  IF (lookup(LBPACK,i) - ((lookup(LBPACK,i) / 10) * 10) == 2) THEN
    disk_length = (lookup(LBLREC,i) + 1) / 2
  ELSE
    disk_length = lookup(LBLREC,i)
  END IF

  ! round up the length to a number of sectors

  disk_length = ((disk_length + um_sector_size - 1) / um_sector_size) * um_sector_size

  ! set the disk address

  lookup(LBEGIN,i) = disk_address

  ! set the disk length

  lookup(LBNREC,i) = disk_length

  ! increment the disk address

  disk_address = disk_address + lookup(LBNREC,i)

END DO

! escape for PP type files
9999 CONTINUE

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_SetDumpfileAddress
