!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Discover number of threads in current ream. Seperate routine created as in
! some environments omp_get_num_threads can return integer*4 into an
! integer*8 routine.
!-------------------------------------------------------------------------------

INTEGER FUNCTION GenFn_ThreadCount ()

!$ USE OpsMod_Kinds, ONLY: &
!$   integer32

IMPLICIT NONE

! Local declarations
!$ INTEGER(kind=integer32) :: omp_get_num_threads

! Do not trace

GenFn_ThreadCount = 1
!$ GenFn_ThreadCount = omp_get_num_threads ()

END FUNCTION GenFn_ThreadCount
