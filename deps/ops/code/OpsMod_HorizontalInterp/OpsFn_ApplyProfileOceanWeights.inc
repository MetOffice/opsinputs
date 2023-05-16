!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
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

REAL FUNCTION OpsFn_ApplyProfileOceanWeights (weights,       &
                                              t_weights,     &
                                              model_columns, &
                                              i,             &
                                              z)
IMPLICIT NONE

! Function arguments:
REAL, INTENT(IN)    :: weights(:,:)
REAL, INTENT(IN)    :: t_weights(:,:)
REAL, INTENT(IN)    :: model_columns(:,:,:)
INTEGER, INTENT(IN) :: i
INTEGER, INTENT(IN) :: z

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

field_at_ob = t_weights(1,i) * (weight_mm * model_columns(1,1,z) + &
                                weight_pm * model_columns(1,2,z) + &
                                weight_pp * model_columns(1,3,z) + &
                                weight_mp * model_columns(1,4,z)) / &
                               (weight_pp + weight_pm + weight_mp + weight_mm)

IF (t_weights(2,i) > 0) THEN
  field_at_ob = field_at_ob + t_weights(2,i) * (weight_mm * model_columns(2,1,z) + &
                                                weight_pm * model_columns(2,2,z) + &
                                                weight_pp * model_columns(2,3,z) + &
                                                weight_mp * model_columns(2,4,z)) / &
                                               (weight_pp + weight_pm + weight_mp + weight_mm)
END IF

OpsFn_ApplyProfileOceanWeights = field_at_ob

END FUNCTION OpsFn_ApplyProfileOceanWeights
