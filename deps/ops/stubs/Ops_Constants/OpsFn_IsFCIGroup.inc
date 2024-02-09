!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Function that returns .TRUE. if given obs group is a FCI obs group, false
! if not.
!-------------------------------------------------------------------------------

ELEMENTAL FUNCTION OpsFn_IsFCIGroup (obsgroup)

! Function arguments:
INTEGER, INTENT(IN) :: obsgroup

! Function result:
LOGICAL             :: OpsFn_IsFCIGroup

IF (obsgroup == ObsGroupFCIClr .OR. &
    obsgroup == ObsGroupFCIASR) THEN
  OpsFn_IsFCIGroup = .TRUE.
ELSE
  OpsFn_IsFCIGroup = .FALSE.
END IF

END FUNCTION OpsFn_IsFCIGroup
