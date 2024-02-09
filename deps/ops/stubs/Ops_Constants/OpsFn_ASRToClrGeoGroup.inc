!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Function that returns the Clr ObsGroup which corresponds to a given ASR
! ObsGroup
!-------------------------------------------------------------------------------

ELEMENTAL FUNCTION OpsFn_ASRToClrGeoGroup (obsgroup)

! Function arguments:
INTEGER, INTENT(IN) :: obsgroup

! Function result:
INTEGER             :: OpsFn_ASRToClrGeoGroup

SELECT CASE (obsgroup)
  CASE (ObsGroupAHIASR)
    OpsFn_ASRToClrGeoGroup = ObsGroupAHIClr
  CASE (ObsGroupFCIASR)
    OpsFn_ASRToClrGeoGroup = ObsGroupFCIClr
  CASE (ObsGroupSEVIRIASR)
    OpsFn_ASRToClrGeoGroup = ObsGroupSEVIRIClr
  CASE DEFAULT
    OpsFn_ASRToClrGeoGroup = obsgroup
END SELECT

END FUNCTION OpsFn_ASRToClrGeoGroup
