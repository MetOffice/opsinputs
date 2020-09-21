!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Identify the first left-of-interval model time coordinate. 
!
!-------------------------------------------------------------------------------

SUBROUTINE Ops_FindTimeCoordIndices (t_model,   &
                                     t_obs,     &
                                     t_indices)

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)    :: t_model(:)
REAL, INTENT(IN)       :: t_obs(:)
INTEGER, INTENT(INOUT) :: t_indices(:)

! Local declarations:
INTEGER                :: index_num
INTEGER                :: curr_index
REAL                   :: ob_time

t_indices(:) = 0
DO index_num = 1, SIZE (t_indices)
  ob_time = t_obs(index_num)
  curr_index = 1
  DO WHILE (t_indices(index_num) == 0)
    IF (t_model(curr_index) >= ob_time) THEN
      IF (curr_index == 1) THEN
        t_indices(index_num) = curr_index
      ELSE
        t_indices(index_num) = curr_index - 1
      END IF
    ELSE IF (curr_index == SIZE (t_model)) THEN
      t_indices(index_num) = curr_index
    END IF
    curr_index = curr_index + 1
  END DO
END DO

END SUBROUTINE Ops_FindTimeCoordIndices
