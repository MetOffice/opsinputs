!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Function that returns .TRUE. if given obs group is a SEVIRI obs group, false
! if not.
!-------------------------------------------------------------------------------

ELEMENTAL FUNCTION OpsFn_IsSEVIRIGroup (obsgroup)

! Function arguments:
INTEGER, INTENT(IN) :: obsgroup

! Function result:
LOGICAL             :: OpsFn_IsSEVIRIGroup

IF (obsgroup == ObsGroupSEVIRIClr .OR. &
    obsgroup == ObsGroupSEVIRIASR) THEN
  OpsFn_IsSEVIRIGroup = .TRUE.
ELSE
  OpsFn_IsSEVIRIGroup = .FALSE.
END IF

END FUNCTION OpsFn_IsSEVIRIGroup
