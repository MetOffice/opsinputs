!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Maps subtype names to subtype numbers.
!-------------------------------------------------------------------------------

FUNCTION Ops_SubTypeNameToNum (name) RESULT (num)

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

IMPLICIT NONE

! Function arguments:
CHARACTER(len=*), INTENT(IN) :: name

! Function result:
INTEGER                      :: num

num = IMDI

SELECT CASE (name)
  CASE ("ABICSR")
    num = ObsTypeABICSR
  CASE ("AATSR")
    num = ObsTypeAATSRUnkn
  CASE ("AIREPS")
    num = ObsTypeAirep
  CASE ("AIRQAL")
    num = ObsTypeAirqal
  CASE ("AIRQALEU")
    num = ObsTypeAirqalEU
  CASE ("AIRS")
    num = ObsTypeAIRS
  CASE ("AIRSL")
    num = ObsTypeAIRSL
  CASE ("AIRSMS")
    num = ObsTypeAIRSMS
  CASE ("AIRSRR")
    num = ObsTypeAIRSRR
  CASE ("AIRSWF")
    num = ObsTypeAIRSWF
  CASE ("ALADIN")
    num = ObsTypeALADIN
  CASE ("AMDARS")
    num = ObsTypeAmdar
  CASE ("AMSR2")
    num = ObsTypeAMSR2
  CASE ("AMSUB")
    num = ObsTypeAMSUB
  CASE ("AMV")
    num = ObsTypeAMV
  CASE ("ARGO", "ARGOB")
    num = ObsTypeArgoBufr
  CASE ("ASCAT")
    num = ObsTypeASCAT
  CASE ("ASCATHR")
    num = ObsTypeASCATHR
  CASE ("ATDNET")
    num = ObsTypeATDNET
  CASE ("ATMS")
    num = ObsTypeATMS
  CASE ("ATMSHR")
    num = ObsTypeATMSHR
  CASE ("ATOVSG")
    num = ObsTypeATOVS_G
  CASE ("ATOVSL")
    num = ObsTypeATOVS_L
  CASE ("TOVSG")
    num = ObsTypeTOVS_G
  CASE ("TOVSL")
    num = ObsTypeTOVS_L
  CASE ("BATHY")
    num = ObsTypeBathy
  CASE ("BOGUS")
    num = ObsTypeBogus
  CASE ("BUOY")
    num = ObsTypeBuoy
  CASE ("BUOYB")
    num = ObsTypeBuoyBufr
  CASE ("BUOYPROF")
    num = ObsTypeBuoyprof
  CASE ("CMAWINDS")
    num = ObsTypeCMAWINDS
  CASE ("COMSCSR")
    num = ObsTypeCOMSCSR
  CASE ("COPTSMO")
    num = ObsTypeCopTSMO
  CASE ("CRIMSS")
    num = ObsTypeCrIS
  CASE ("CRIMSSHR")
    num = ObsTypeCrISHR
  CASE ("DROPSOND")
    num = ObsTypeDropsonde
  CASE ("DRWIND")
    num = ObsTypeRadwind
  CASE ("EMADDC")
    num = ObsTypeEmaddc
  CASE ("ESACMW")
    num = ObsTypeESACMW
  CASE ("ESACSR")
    num = ObsTypeMVIRICSR
  CASE ("ESACSWVW")
    num = ObsTypeESACSWVW
  CASE ("ESAHRVW")
    num = ObsTypeEsahRVW
  CASE ("ESAHRWVW")
    num = ObsTypeESAHRWVW
  CASE ("ESAURA")
    num = ObsTypeEsaura
  CASE ("ESAUWA")
    num = ObsTypeEsauwa
  CASE ("ESAUWI")
    num = ObsTypeESAUWI
  CASE ("GAUGE")
    num = ObsTypeGauge
  CASE ("GHRSST")
    num = ObsTypeGHRSST
  CASE ("GIIRS")
    num = ObsTypeGIIRS
  CASE ("GMI")
    num = ObsTypeGMIlow
  CASE ("GOESAMW")
    num = ObsTypeGoesAMW
  CASE ("GOESBUFR")
    num = ObsTypeGOESBUFR
  CASE ("GOESCSR")
    num = ObsTypeGOESImCSR
  CASE ("GOESVIS")
    num = ObsTypeGoesVIS
  CASE ("GOESWV")
    num = ObsTypeGOESWV
  CASE ("GPSIWV")
    num = ObsTypeGPSIWV
  CASE ("GPSRO")
    num = ObsTypeGPSRO
  CASE ("HRSEAICE")
    num = ObsTypeHRSeaIce
  CASE ("IASIG")
    num = ObsTypeIASI_G
  CASE ("IASIHR")
    num = ObsTypeIASI_H
  CASE ("IASIL")
    num = ObsTypeIASI_L
  CASE ("IMDWINDS")
    num = ObsTypeIMDWINDS
  CASE ("IN3DCSR")
    num = ObsTypeIN3DImCSR
  CASE ("IN3DSNDR")
    num = ObsTypeIN3DSdCSR
  CASE ("HIMCSR")
    num = ObsTypeAHICSR
  CASE ("HIMRAD")
    num = ObsTypeAHIASR
  CASE ("JMACSR")
    num = ObsTypeMTSATImCSR
  CASE ("JMAWINDS")
    num = ObsTypeJMAWINDS
  CASE ("KMAWINDS")
    num = ObsTypeKMAWINDS
  CASE ("LIDAR")
    num = ObsTypeGroundLidar
  CASE ("LNDSYB")
    num = ObsTypeSynopBufr
  CASE ("LNDSYN")
    num = ObsTypeSynop
  CASE ("MERIS")
    num = ObsTypeMeris
  CASE ("METARS")
    num = ObsTypeMetar
  CASE ("MOBSYN")
    num = ObsTypeSynopMob
  CASE ("MODE-S")
    num = ObsTypeModeS
  CASE ("MODIS")
    num = ObsTypeModis
  CASE ("MSGCSR")
    num = ObsTypeSeviriEUM
  CASE ("MSGAOD")
    num = ObsTypeMSGAOD
  CASE ("MSGCTP")
    num = ObsTypeSeviriCTP
  CASE ("MSGCTPFD")
    num = ObsTypeSeviriCTPFD
  CASE ("MSGCTPUK")
    num = ObsTypeSeviriCTPUK
  CASE ("MSGRAD")
    num = ObsTypeSeviriAuto
  CASE ("MSGRADFD")
    num = ObsTypeSeviriAutoFD
  CASE ("MSGRADUK")
    num = ObsTypeSeviriAutoUK
  CASE ("MSGWINDS")
    num = ObsTypeMSGWINDS
  CASE ("MWHS")
    num = ObsTypeMWHSFY3
  CASE ("MWRI")
    num = ObsTypeMWRI
  CASE ("MWTS")
    num = ObsTypeMWSFY3
  CASE ("OCEANCOL")
    num = ObsTypeOceanColour
  CASE ("OCEANFB")
    num = ObsTypeOceanFB
  CASE ("OCEANRE")
    num = ObsTypeOceanRE
  CASE ("OCEANTS")
    num = ObsTypeOceanTS
  CASE ("OCNWINDS")
    num = ObsTypeOCNWINDS
  CASE ("OLCI")
    num = ObsTypeOLCI
  CASE ("OPENROAD")
    num = ObsTypeOpenRoad
  CASE ("OSSEAICE")
    num = ObsTypeSeaIce
  CASE ("OSSEAICN")
    num = ObsTypeSeaIceN
  CASE ("OZONESAT")
    num = ObsTypeSBUVOzone
  CASE ("PAOBS")
    num = 40700
  CASE ("PILOT")
    num = ObsTypePilot
  CASE ("RADOP")
    num = ObsTypeRadopWind
  CASE ("RADOP2")
    num = ObsTypeRadop2Wind
  CASE ("RADRF")
    num = ObsTypeRadRefl
  CASE ("RADREFL")
    num = ObsTypeRadRefl2
  CASE ("RADREFR")
    num = ObsTypeRadRefr
  CASE ("RADRRATE")
    num = ObsTypePrecip
  CASE ("RADVEL")
    num = ObsTypeRadVel
  CASE ("RPBOGUS")
    num = 40500
  CASE ("SALTSSH")
    num = ObsTypeSaltSSH
  CASE ("SALTSSHS")
    num = ObsTypeSaltSSHS
  CASE ("SALTSSHT")
    num = ObsTypeSaltSSHT
  CASE ("SAPHIR")
    num = ObsTypeSAPHIR
  CASE ("SATAOD")
    num = ObsTypeSATAOD
  CASE ("SATOB")
    num = ObsTypeSatob
  CASE ("SEAWINDS")
    num = ObsTypeSeawinds
  CASE ("SFERICS")
    num = ObsTypeSFERICS
  CASE ("SHPSYB")
    num = ObsTypeShipBufr
  CASE ("SHPSYN")
    num = ObsTypeShip
  CASE ("SONDE")
    num = ObsTypeSonde
  CASE ("SREW")
    num = ObsTypeSREW
  CASE ("SSMIS")
    num = ObsTypeSSMIS
  CASE ("SSTBOGUS")
    num = 40200
  CASE ("STEREOMV")
    num = ObsTypeStereomv
  CASE ("TAMDAR")
    num = ObsTypeTamdar
  CASE ("TCBOGUS")
    num = ObsTypeTCBogus
  CASE ("TCRTEMP")
    num = 22700
  CASE ("TEMP")
    num = ObsTypeTemp
  CASE ("TESAC")
    num = ObsTypeTesac
  CASE ("THBOGUS")
    num = 40600
  CASE ("TSTSONDE")
    num = ObsTypeTstSonde
  CASE ("UKMOSST")
    num = ObsTypeUKMOSSTu
  CASE ("UKWINDS")
    num = ObsTypeUKWINDS
  CASE ("WAVENET")
    num = ObsTypeWavenet
  CASE ("WINDSAT")
    num = ObsTypeWindsat
  CASE ("WINPRO")
    num = ObsTypeWindProf
  CASE ("WOW")
    num = ObsTypeWOW
  CASE ("ASCATCO")
    num = ObsTypeASCATCO
END SELECT

END FUNCTION Ops_SubTypeNameToNum
