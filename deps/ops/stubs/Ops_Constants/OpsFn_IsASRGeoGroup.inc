!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Function that returns .TRUE. if given obs group is a SatRad group, false if
! not.
!-------------------------------------------------------------------------------

ELEMENTAL FUNCTION OpsFn_IsASRGeoGroup (obsgroup)

! Function arguments:
INTEGER, INTENT(IN) :: obsgroup

! Function result:
LOGICAL             :: OpsFn_IsASRGeoGroup

IF (obsgroup == ObsGroupAHIASR .OR. &
    obsgroup == ObsGroupSEVIRIASR) THEN
  OpsFn_IsASRGeoGroup = .TRUE.
ELSE
  OpsFn_IsASRGeoGroup = .FALSE.
END IF

END FUNCTION OpsFn_IsASRGeoGroup
