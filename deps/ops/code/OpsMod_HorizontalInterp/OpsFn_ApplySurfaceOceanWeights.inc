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

REAL FUNCTION OpsFn_ApplySurfaceOceanWeights (weights,      &
                                              field,        &
                                              is,           &
                                              js,           &
                                              i,            &
                                              ts_in,        &
                                              t_weights_in)

USE GenMod_Core, ONLY: &
  gen_fail

IMPLICIT NONE

! Function arguments:
REAL, INTENT(IN)              :: weights(:,:)
REAL, INTENT(IN)              :: field(:,:,:)
INTEGER, INTENT(IN)           :: is(:)
INTEGER, INTENT(IN)           :: js(:)
INTEGER, INTENT(IN)           :: i
INTEGER, OPTIONAL, INTENT(IN) :: ts_in(:)
REAL, OPTIONAL, INTENT(IN)    :: t_weights_in(:,:)

! Local declarations:
REAL                          :: field_at_ob
REAL                          :: weight_mm
REAL                          :: weight_pm
REAL                          :: weight_pp
REAL                          :: weight_mp
INTEGER                       :: t_before
INTEGER                       :: t_after
REAL                          :: t_weight_before
REAL                          :: t_weight_after
CHARACTER(len=*), PARAMETER   :: RoutineName = 'OpsFn_ApplySurfaceOceanWeights'
CHARACTER(len=80)             :: ErrorMessage

weight_mm = weights(1,i)
weight_pm = weights(2,i)
weight_pp = weights(3,i)
weight_mp = weights(4,i)

IF (PRESENT (ts_in)) THEN
  t_before = ts_in(i)
  t_after = ts_in(i) + 1
ELSE
  t_before = 1
  t_after = 1
END IF

IF (PRESENT (t_weights_in)) THEN
  t_weight_before = t_weights_in(1,i)
  t_weight_after = t_weights_in(2,i)
ELSE
  t_weight_before = 1
  t_weight_after = 0
END IF

IF (t_before == 0) THEN
  WRITE (ErrorMessage, '(A)') "earlier-than-ob t coord cannot be zero! STOPPING"
  CALL gen_fail (RoutineName,  &
                 ErrorMessage)
END IF

field_at_ob = t_weight_before * (weight_pp * field(is(i),js(i),t_before) + &
                                 weight_pm * field(is(i),js(i) - 1,t_before) + &
                                 weight_mp * field(is(i) - 1,js(i),t_before) + &
                                 weight_mm * field(is(i) - 1,js(i) - 1,t_before)) / &
                                (weight_pp + weight_pm + weight_mp + weight_mm)

IF (t_weight_after > 0) THEN
  field_at_ob = field_at_ob + t_weight_after * (weight_pp * field(is(i),js(i),t_after) + &
                                                weight_pm * field(is(i),js(i) - 1,t_after) + &
                                                weight_mp * field(is(i) - 1,js(i),t_after) + &
                                                weight_mm * field(is(i) - 1,js(i) - 1,t_after)) / &
                                               (weight_pp + weight_pm + weight_mp + weight_mm)
END IF

OpsFn_ApplySurfaceOceanWeights = field_at_ob

END FUNCTION OpsFn_ApplySurfaceOceanWeights
