!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Function that returns .TRUE. if given obs group is a AHI obs group, false
! if not.
!-------------------------------------------------------------------------------

ELEMENTAL FUNCTION OpsFn_IsAHIGroup (obsgroup)

! Function arguments:
INTEGER, INTENT(IN) :: obsgroup

! Function result:
LOGICAL             :: OpsFn_IsAHIGroup

IF (obsgroup == ObsGroupAHIClr .OR. &
    obsgroup == ObsGroupAHIASR) THEN
  OpsFn_IsAHIGroup = .TRUE.
ELSE
  OpsFn_IsAHIGroup = .FALSE.
END IF

END FUNCTION OpsFn_IsAHIGroup
