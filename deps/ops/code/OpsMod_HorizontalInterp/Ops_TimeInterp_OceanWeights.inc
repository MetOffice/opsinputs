!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Calculate the linear time interpolation weights.
!
! Use the t_indices array to identify the first left-of-interval model time
! coordinate. t_indicies is calculated in a separate routine.
!
! This time interpolation does not extrapolate, and reverts to nearest neighbour
! interpolation for observations with times smaller than the minimum time or
! larger than the maximum. This is handled by setting t_weights(2,:)=0.
!
!-------------------------------------------------------------------------------

SUBROUTINE Ops_TimeInterp_OceanWeights (t_model,   &
                                        t_obs,     &
                                        t_indices, &
                                        t_weights)

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN) :: t_model(:)
REAL, INTENT(IN)    :: t_obs(:)
INTEGER, INTENT(IN) :: t_indices(:)
REAL, INTENT(INOUT) :: t_weights(:,:)

! Local declarations:
INTEGER             :: index_num
REAL                :: model_before
REAL                :: model_after
REAL                :: model_diff
REAL                :: ob_time

DO index_num = 1, SIZE (t_obs)
  ob_time = t_obs(index_num)
  model_before = t_model(t_indices(index_num))
  IF (t_indices(index_num) == SIZE (t_model) .OR. &
      t_obs(index_num) <= model_before) THEN
    t_weights(1,index_num) = 1
    t_weights(2,index_num) = 0
  ELSE
    model_after = t_model(t_indices(index_num) + 1)
    model_diff = model_after - model_before
    t_weights(1,index_num) = (model_after - ob_time) / model_diff
    t_weights(2,index_num) = (ob_time - model_before) / model_diff
  END IF
END DO

END SUBROUTINE Ops_TimeInterp_OceanWeights
