!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Contains generic Quality Control code:
!   low-level background check routines
!   buddy check: sorting, pairing and actual buddy check routines
!-------------------------------------------------------------------------------

MODULE OpsMod_QCBackground

IMPLICIT NONE

SAVE

! Public declarations:
REAL :: ObErrMult = 1.0               ! multiplication factor for ObErr
REAL :: BackgrErrMult = 1.0           ! multiplication factor for BackgrErr
REAL :: BogusErrMult                  ! Set to SQRT(3.0) in Ops_ReadQcNL Bogus obs: mult. factor for BackgrErr
REAL :: SDiffCrit = 15.0 ** 2         ! do not use in buddy check if SDiff > SDiffCrit
REAL :: Bogus_PdBad_p = 0.00043       ! Pa**-1
REAL :: Bogus_PdBad_uv10 = 0.00005    ! (m/s)**-2
REAL :: Bogus_PdBad_t = 0.01          ! K**-1
REAL :: Bogus_PdBad_rh = 0.01         !
REAL :: Bogus_PdBad_uv = 0.00005      ! (m/s)**-2

CONTAINS

INCLUDE 'Ops_QcBackgr1s.inc'
INCLUDE 'Ops_QcBackgr1v.inc'
INCLUDE 'Ops_QcBackgr2s.inc'

END MODULE OpsMod_QCBackground
