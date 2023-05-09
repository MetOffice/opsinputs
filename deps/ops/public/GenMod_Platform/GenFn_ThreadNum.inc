!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Discover current thread number. Seperate routine created as in some
! environments omp_get_thread_num can return integer*4 into an integer*8
! routine.
!-------------------------------------------------------------------------------

INTEGER FUNCTION GenFn_ThreadNum ()

!$ USE OpsMod_Kinds, ONLY: &
!$   integer32

IMPLICIT NONE

! Local declarations
!$ INTEGER(kind=integer32) :: omp_get_thread_num

! Do not trace

GenFn_ThreadNum = 0
!$ GenFn_ThreadNum = omp_get_thread_num ()

END FUNCTION GenFn_ThreadNum
