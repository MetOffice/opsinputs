!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Returns the value of the random seed.
!-------------------------------------------------------------------------------

FUNCTION OpsFn_GetRandomSeed ()

IMPLICIT NONE

! Function result:
INTEGER                     :: OpsFn_GetRandomSeed

OpsFn_GetRandomSeed = initializer

END FUNCTION OpsFn_GetRandomSeed
