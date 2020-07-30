!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Application of the horizontal interpolation weights.
!
! i=x=lambda, j=y=phi
!
! The four model columns surrounding the lat lon of interest are specified:
!                          x i-1,j              x i, j
!                            _corner(4)         _corner(3)
!                            _weight(4)         _weight(3)
!                            *mp                *pp
!
!
!                                 * lon,lat
!
!                          x i-1,j-1            x i,j-1
!                            _corner(1)         _corner(2)
!                            _weight(1)         _weight(2)
!                            *mm                *pm
!
! The weights and corner variables are listed in a counter-clockwise 
! direction, or the variables are suffixed with m and p.
!-------------------------------------------------------------------------------

REAL FUNCTION OpsFn_Apply2DOceanWeights (weights, &
                                         field,   &
                                         is,      &
                                         js,      &
                                         i)

IMPLICIT NONE

! Function arguments:
REAL, INTENT(IN)    :: weights(:,:)
REAL, INTENT(IN)    :: field(:,:)
INTEGER, INTENT(IN) :: is(:)
INTEGER, INTENT(IN) :: js(:)
INTEGER, INTENT(IN) :: i

! Local declarations:
REAL                :: field_at_ob
REAL                :: weight_mm
REAL                :: weight_pm
REAL                :: weight_pp
REAL                :: weight_mp

weight_mm = weights(1,i)
weight_pm = weights(2,i)
weight_pp = weights(3,i)
weight_mp = weights(4,i)

field_at_ob = (weight_mm * field(is(i) - 1,js(i) - 1) + &
               weight_pm * field(is(i),js(i) - 1) + &
               weight_pp * field(is(i),js(i)) + &
               weight_mp * field(is(i) - 1,js(i))) / &
              (weight_pp + weight_pm + weight_mp + weight_mm)

OpsFn_Apply2DOceanWeights = field_at_ob

END FUNCTION OpsFn_Apply2DOceanWeights
