!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Convert an obs group number to a name.
!-------------------------------------------------------------------------------

ELEMENTAL FUNCTION OpsFn_ObsGroupNumToName (ObsGroup)

IMPLICIT NONE

! Function arguments:
INTEGER, INTENT(IN)         :: ObsGroup

! Function result:
CHARACTER(len=12)           :: OpsFn_ObsGroupNumToName

SELECT CASE (ObsGroup)
  CASE (ObsGroupABIClr)
    OpsFn_ObsGroupNumToName = "ABIClr"
  CASE (ObsGroupAHIASR)
    OpsFn_ObsGroupNumToName = "AHIASR"
  CASE (ObsGroupAHIClr)
    OpsFn_ObsGroupNumToName = "AHIClr"
  CASE (ObsGroupAircraft)
    OpsFn_ObsGroupNumToName = "Aircraft"
  CASE (ObsGroupAIRS)
    OpsFn_ObsGroupNumToName = "AIRS"
  CASE (ObsGroupHIRAS)
    OpsFn_ObsGroupNumToName = "HIRAS"
  CASE (ObsGroupHLOSwind)
    OpsFn_ObsGroupNumToName = "HLOSwind"
  CASE (ObsGroupAltim)
    OpsFn_ObsGroupNumToName = "Altimeter"
  CASE (ObsGroupAMSR)
    OpsFn_ObsGroupNumToName = "AMSR"
  CASE (ObsGroupAMSUB)
    OpsFn_ObsGroupNumToName = "AMSUB"
  CASE (ObsGroupATMS)
    OpsFn_ObsGroupNumToName = "ATMS"
  CASE (ObsGroupATOVS)
    OpsFn_ObsGroupNumToName = "ATOVS"
  CASE (ObsGroupTOVS)
    OpsFn_ObsGroupNumToName = "TOVS"
  CASE (ObsGroupCOMSMIClr)
    OpsFn_ObsGroupNumToName = "COMSMIClr"
  CASE (ObsGroupCrIS)
    OpsFn_ObsGroupNumToName = "CrIS"
  CASE (ObsGroupMWSFY3B)
    OpsFn_ObsGroupNumToName = "MWSFY3B"
  CASE (ObsGroupGeoCloud)
    OpsFn_ObsGroupNumToName = "GeoCloud"
  CASE (ObsGroupGIIRSLW)
    OpsFn_ObsGroupNumToName = "GIIRSLW"
  CASE (ObsGroupGIIRSMW)
    OpsFn_ObsGroupNumToName = "GIIRSMW"
  CASE (ObsGroupGOESImClr)
    OpsFn_ObsGroupNumToName = "GOESImClr"
  CASE (ObsGroupGMIhigh)
    OpsFn_ObsGroupNumToName = "GMIhigh"
  CASE (ObsGroupGMIlow)
    OpsFn_ObsGroupNumToName = "GMIlow"
  CASE (ObsGroupGPSRO)
    OpsFn_ObsGroupNumToName = "GPSRO"
  CASE (ObsGroupGroundGPS)
    OpsFn_ObsGroupNumToName = "GroundGPS"
  CASE (ObsGroupGroundLidar)
    OpsFn_ObsGroupNumToName = "GroundLidar"
  CASE (ObsGroupIASI)
    OpsFn_ObsGroupNumToName = "IASI"
  CASE (ObsGroupIN3DIClr)
    OpsFn_ObsGroupNumToName = "IN3DIClr"
  CASE (ObsGroupIN3DS)
    OpsFn_ObsGroupNumToName = "IN3DS"
  CASE (ObsGroupAOD)
    OpsFn_ObsGroupNumToName = "AOD"
  CASE (ObsGroupMTSATImClr)
    OpsFn_ObsGroupNumToName = "MTSATImClr"
  CASE (ObsGroupSAPHIR)
    OpsFn_ObsGroupNumToName = "SAPHIR"
  CASE (ObsGroupMVIRIClr)
    OpsFn_ObsGroupNumToName = "MVIRIClr"
  CASE (ObsGroupMWSFY3)
    OpsFn_ObsGroupNumToName = "MWSFY3"
  CASE (ObsGroupMWRI)
    OpsFn_ObsGroupNumToName = "MWRI"
  CASE (ObsGroupOcean)
    OpsFn_ObsGroupNumToName = "OceanSound"
  CASE (ObsGroupOceanWinds)
    OpsFn_ObsGroupNumToName = "OceanWinds"
  CASE (ObsGroupPrecip)
    OpsFn_ObsGroupNumToName = "Precip"
  CASE (ObsGroupRadar)
    OpsFn_ObsGroupNumToName = "Radar"
  CASE (ObsGroupRadarN)
    OpsFn_ObsGroupNumToName = "RadarN"
  CASE (ObsGroupRadarZ)
    OpsFn_ObsGroupNumToName = "RadarZ"
  CASE (ObsGroupRainAccum)
    OpsFn_ObsGroupNumToName = "RainAccum"
  CASE (ObsGroupSatSST)
    OpsFn_ObsGroupNumToName = "SatSST"
  CASE (ObsGroupSatTCWV)
    OpsFn_ObsGroupNumToName = "SatTCWV"
  CASE (ObsGroupSatwind)
    OpsFn_ObsGroupNumToName = "Satwind"
  CASE (ObsGroupSBUV)
    OpsFn_ObsGroupNumToName = "SBUV"
  CASE (ObsGroupScatwind)
    OpsFn_ObsGroupNumToName = "Scatwind"
  CASE (ObsGroupScatwindChosen)
    OpsFn_ObsGroupNumToName = "ScatwindChosen"
  CASE (ObsGroupSeaIce)
    OpsFn_ObsGroupNumToName = "SeaIce"
  CASE (ObsGroupOceanColour)
    OpsFn_ObsGroupNumToName = "OceanColour"
  CASE (ObsGroupSEVIRIClr)
    OpsFn_ObsGroupNumToName = "SEVIRIClr"
  CASE (ObsGroupSEVIRIASR)
    OpsFn_ObsGroupNumToName = "SEVIRIASR"
  CASE (ObsGroupSonde)
    OpsFn_ObsGroupNumToName = "Sonde"
  CASE (ObsGroupSSMIS)
    OpsFn_ObsGroupNumToName = "SSMIS"
  CASE (ObsGroupSurface)
    OpsFn_ObsGroupNumToName = "Surface"
  CASE (ObsGroupSurfaceCloud)
    OpsFn_ObsGroupNumToName = "SurfaceCloud"
  CASE DEFAULT
    OpsFn_ObsGroupNumToName = "Unknown"
END SELECT

END FUNCTION OpsFn_ObsGroupNumToName
