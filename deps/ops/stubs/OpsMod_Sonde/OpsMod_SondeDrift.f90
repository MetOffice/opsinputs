!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Module to contain code specific to radiosonde observations drifting.
!-------------------------------------------------------------------------------

MODULE OpsMod_SondeDrift

IMPLICIT NONE

SAVE

INTEGER              :: SondeDrift = 0      ! 0 do not account for drift
                                            ! 1 account for BUFR sonde drift
                                            ! 2 account for BUFR and TEMP sonde drift, calculating the latter based on wind profiles
END MODULE OpsMod_SondeDrift
