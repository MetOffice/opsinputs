!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Contains parameters for global access in Ops_GPSRO_Process
!-------------------------------------------------------------------------------

MODULE OpsMod_GPSROInfo

IMPLICIT NONE

SAVE

! Public declarations:

! The PCD is an integer whose bits contain information on the occultation
! The meaning of each bit is specified in the ROPP I/O user guide
! The guide lists PCD_rising as number three.  The numbers seen by OPS count
! from start at the bottom and count from zero.
INTEGER, PARAMETER :: GPSRO_rising_setting_bit = 13  ! Bit which says whether
                                                     ! the occultation is
                                                     ! rising or setting

END MODULE OpsMod_GPSROInfo
