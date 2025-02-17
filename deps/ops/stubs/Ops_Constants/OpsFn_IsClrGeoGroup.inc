!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Function that returns .TRUE. if given obs group is a SatRad group, false if
! not.
!-------------------------------------------------------------------------------

ELEMENTAL FUNCTION OpsFn_IsClrGeoGroup (obsgroup)

! Function arguments:
INTEGER, INTENT(IN) :: obsgroup

! Function result:
LOGICAL             :: OpsFn_IsClrGeoGroup

IF (obsgroup == ObsGroupABIClr .OR. &
    obsgroup == ObsGroupAHIClr .OR. &
    obsgroup == ObsGroupCOMSMIClr .OR. &
    obsgroup == ObsGroupFCIClr .OR. &
    obsgroup == ObsGroupGOESImClr .OR. &
    obsgroup == ObsGroupIN3DIClr .OR. &
    obsgroup == ObsGroupMTSATImClr .OR. &
    obsgroup == ObsGroupMVIRIClr .OR. &
    obsgroup == ObsGroupSEVIRIClr) THEN
  OpsFn_IsClrGeoGroup = .TRUE.
ELSE
  OpsFn_IsClrGeoGroup = .FALSE.
END IF

END FUNCTION OpsFn_IsClrGeoGroup
