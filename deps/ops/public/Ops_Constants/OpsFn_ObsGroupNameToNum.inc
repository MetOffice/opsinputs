!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Convert an obs group name to a number.
!-------------------------------------------------------------------------------

ELEMENTAL FUNCTION OpsFn_ObsGroupNameToNum (ObsGroupName)

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

IMPLICIT NONE

! Function arguments:
CHARACTER(len=*), INTENT(IN) :: ObsGroupName

! Function result:
INTEGER                      :: OpsFn_ObsGroupNameToNum

SELECT CASE (ObsGroupName)
  CASE ("ABIClr")
    OpsFn_ObsGroupNameToNum = ObsGroupABIClr
  CASE ("AHIASR")
    OpsFn_ObsGroupNameToNum = ObsGroupAHIASR
  CASE ("AHIClr")
    OpsFn_ObsGroupNameToNum = ObsGroupAHIClr
  CASE ("Aircraft")
    OpsFn_ObsGroupNameToNum = ObsGroupAircraft
  CASE ("AIRS")
    OpsFn_ObsGroupNameToNum = ObsGroupAIRS
  CASE ("Altimeter")
    OpsFn_ObsGroupNameToNum = ObsGroupAltim
  CASE ("AMSR")
    OpsFn_ObsGroupNameToNum = ObsGroupAMSR
  CASE ("AMSUB")
    OpsFn_ObsGroupNameToNum = ObsGroupAMSUB
  CASE ("ATMS")
    OpsFn_ObsGroupNameToNum = ObsGroupATMS
  CASE ("ATOVS")
    OpsFn_ObsGroupNameToNum = ObsGroupATOVS
  CASE ("TOVS")
    OpsFn_ObsGroupNameToNum = ObsGroupTOVS
  CASE ("COMSMIClr")
    OpsFn_ObsGroupNameToNum = ObsGroupCOMSMIClr
  CASE ("CrIS")
    OpsFn_ObsGroupNameToNum = ObsGroupCrIS
  CASE ("HIRAS")
    OpsFn_ObsGroupNameToNum = ObsGroupHIRAS
  CASE ("MWSFY3B")
    OpsFn_ObsGroupNameToNum = ObsGroupMWSFY3B
  CASE ("GeoCloud")
    OpsFn_ObsGroupNameToNum = ObsGroupGeoCloud
  CASE ("GIIRSLW")
    OpsFn_ObsGroupNameToNum = ObsGroupGIIRSLW
  CASE ("GIIRSMW")
    OpsFn_ObsGroupNameToNum = ObsGroupGIIRSMW
  CASE("GMIhigh")
    OpsFn_ObsGroupNameToNum = ObsGroupGMIhigh
  CASE("GMIlow")
    OpsFn_ObsGroupNameToNum = ObsGroupGMIlow
  CASE ("GOESImClr")
    OpsFn_ObsGroupNameToNum = ObsGroupGOESImClr
  CASE ("GPSRO")
    OpsFn_ObsGroupNameToNum = ObsGroupGPSRO
  CASE ("GroundGPS")
    OpsFn_ObsGroupNameToNum = ObsGroupGroundGPS
  CASE ("GroundLidar")
    OpsFn_ObsGroupNameToNum = ObsGroupGroundLidar
  CASE ("HLOSwind")
    OpsFn_ObsGroupNameToNum = ObsGroupHLOSwind
  CASE ("IASI")
    OpsFn_ObsGroupNameToNum = ObsGroupIASI
  CASE ("IN3DIClr")
    OpsFn_ObsGroupNameToNum = ObsGroupIN3DIClr
  CASE ("IN3DS")
    OpsFn_ObsGroupNameToNum = ObsGroupIN3DS
  CASE ("AOD")
    OpsFn_ObsGroupNameToNum = ObsGroupAOD
  CASE ("MTSATImClr")
    OpsFn_ObsGroupNameToNum = ObsGroupMTSATImClr
  CASE ("SAPHIR")
    OpsFn_ObsGroupNameToNum = ObsGroupSAPHIR
  CASE ("MVIRIClr")
    OpsFn_ObsGroupNameToNum = ObsGroupMVIRIClr
  CASE ("MWRI")
    OpsFn_ObsGroupNameToNum = ObsGroupMWRI
  CASE ("MWSFY3")
    OpsFn_ObsGroupNameToNum = ObsGroupMWSFY3
  CASE ("OceanSound")
    OpsFn_ObsGroupNameToNum = ObsGroupOcean
  CASE ("OceanWinds")
    OpsFn_ObsGroupNameToNum = ObsGroupOceanWinds
  CASE ("Precip")
    OpsFn_ObsGroupNameToNum = ObsGroupPrecip
  CASE ("Radar")
    OpsFn_ObsGroupNameToNum = ObsGroupRadar
  CASE ("RadarN")
    OpsFn_ObsGroupNameToNum = ObsGroupRadarN
  CASE ("RadarZ")
    OpsFn_ObsGroupNameToNum = ObsGroupRadarZ
  CASE ("RainAccum")
    OpsFn_ObsGroupNameToNum = ObsGroupRainAccum
  CASE ("SatSST")
    OpsFn_ObsGroupNameToNum = ObsGroupSatSST
  CASE ("SatTCWV")
    OpsFn_ObsGroupNameToNum = ObsGroupSatTCWV
  CASE ("Satwind")
    OpsFn_ObsGroupNameToNum = ObsGroupSatwind
  CASE ("SBUV")
    OpsFn_ObsGroupNameToNum = ObsGroupSBUV
  CASE ("Scatwind")
    OpsFn_ObsGroupNameToNum = ObsGroupScatwind
  CASE ("ScatwindChosen")
    OpsFn_ObsGroupNameToNum = ObsGroupScatwindChosen
  CASE ("SeaIce")
    OpsFn_ObsGroupNameToNum = ObsGroupSeaIce
  CASE ("OceanColour")
    OpsFn_ObsGroupNameToNum = ObsGroupOceanColour
  CASE ("SEVIRIASR")
    OpsFn_ObsGroupNameToNum = ObsGroupSEVIRIASR
  CASE ("SEVIRIClr")
    OpsFn_ObsGroupNameToNum = ObsGroupSEVIRIClr
  CASE ("Sonde")
    OpsFn_ObsGroupNameToNum = ObsGroupSonde
  CASE ("SSMIS")
    OpsFn_ObsGroupNameToNum = ObsGroupSSMIS
  CASE ("Surface")
    OpsFn_ObsGroupNameToNum = ObsGroupSurface
  CASE ("SurfaceCloud")
    OpsFn_ObsGroupNameToNum = ObsGroupSurfaceCloud
  CASE DEFAULT
    OpsFn_ObsGroupNameToNum = IMDI
END SELECT

END FUNCTION OpsFn_ObsGroupNameToNum
