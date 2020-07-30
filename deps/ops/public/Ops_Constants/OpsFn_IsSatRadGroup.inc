!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Function that returns .TRUE. if given obs group is a SatRad group, false if
! not.
!-------------------------------------------------------------------------------

ELEMENTAL FUNCTION OpsFn_IsSatRadGroup (obsgroup)

! Function arguments:
INTEGER, INTENT(IN) :: obsgroup

! Function result:
LOGICAL             :: OpsFn_IsSatRadGroup

IF (obsgroup == ObsGroupABIClr .OR. &
    obsgroup == ObsGroupAHIASR .OR. &
    obsgroup == ObsGroupAHIClr .OR. &
    obsgroup == ObsGroupAIRS .OR. &
    obsgroup == ObsGroupAMSR .OR. &
    obsgroup == ObsGroupAMSUB .OR. &
    obsgroup == ObsGroupATMS .OR. &
    obsgroup == ObsGroupATOVS .OR. &
    obsgroup == ObsGroupTOVS .OR. &
    obsgroup == ObsGroupCOMSMIClr .OR. &
    obsgroup == ObsGroupCRIS .OR. &
    obsgroup == ObsGroupHIRAS .OR. &
    obsgroup == ObsGroupGeoCloud .OR. &
    obsgroup == ObsGroupGOESImClr .OR. &
    obsgroup == ObsGroupIASI .OR. &
    obsgroup == ObsGroupIN3DIClr .OR. &
    obsgroup == ObsGroupIN3DS .OR. &
    obsgroup == ObsGroupMWSFY3B .OR. &
    obsgroup == ObsGroupMTSATImClr .OR. &
    obsgroup == ObsGroupSAPHIR .OR. &
    obsgroup == ObsGroupMVIRIClr .OR. &
    obsgroup == ObsGroupMWSFY3 .OR. &
    obsgroup == ObsGroupMWRI .OR. &
    obsgroup == ObsGroupSEVIRIClr .OR. &
    obsgroup == ObsGroupSEVIRIASR .OR. &
    obsgroup == ObsGroupGMIlow .OR. &
    obsgroup == ObsGroupGMIhigh .OR. &
    obsgroup == ObsGroupSSMIS .OR. &
    obsgroup == ObsGroupGIIRSLW .OR. &
    obsgroup == ObsGroupGIIRSMW) THEN

  OpsFn_IsSatRadGroup = .TRUE.
ELSE
  OpsFn_IsSatRadGroup = .FALSE.
END IF

END FUNCTION OpsFn_IsSatRadGroup
