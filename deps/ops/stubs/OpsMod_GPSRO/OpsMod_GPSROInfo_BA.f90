!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Contains parameters for global access in Ops_GPSRO_Process
!-------------------------------------------------------------------------------

MODULE OpsMod_GPSROInfo_BA

IMPLICIT NONE

SAVE

! Public declarations:

LOGICAL              :: GPSRO_pseudo_ops = .TRUE.  ! .TRUE. is pseudo-levels turned on.
LOGICAL              :: GPSRO_vert_interp_ops = .TRUE.  ! .TRUE. is interp of ln(P) instead of Exner.

END MODULE OpsMod_GPSROInfo_BA
