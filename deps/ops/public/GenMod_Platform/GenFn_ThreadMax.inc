!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Discover maxmium number of threads in current ream. Seperate routine
! created as in some environments omp_get_max_threads can return integer*4
! into an integer*8 routine.
!-------------------------------------------------------------------------------

INTEGER FUNCTION GenFn_ThreadMax ()

!$ USE OpsMod_Kinds, ONLY: &
!$   integer32

IMPLICIT NONE

! Local declarations
!$ INTEGER(kind=integer32) :: omp_get_max_threads

! Do not trace

GenFn_ThreadMax = 1
!$ GenFn_ThreadMax = omp_get_max_threads ()

END FUNCTION GenFn_ThreadMax
