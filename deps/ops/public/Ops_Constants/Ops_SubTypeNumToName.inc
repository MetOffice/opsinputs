!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Maps subtype numbers to subtype names.
!-------------------------------------------------------------------------------

FUNCTION Ops_SubTypeNumToName (num) RESULT (name)

IMPLICIT NONE

! Function arguments:
INTEGER, INTENT(IN) :: num

! Function result:
CHARACTER(len=8)    :: name

name = ""

SELECT CASE (num)
  CASE (ObsTypeAATSRUnkn)
    name = "AATSR"
  CASE (ObsTypeABICSR)
    name = "ABICSR"
  CASE (ObsTypeAHIASR)
    name = "HIMRAD"
  CASE (ObsTypeAHICSR)
    name = "HIMCSR"
  CASE (ObsTypeAirep)
    name = "AIREPS"
  CASE (ObsTypeAirqal)
    name = "AIRQAL"
  CASE (ObsTypeAirqalEU)
    name = "AIRQALEU"
  CASE (ObsTypeAIRS)
    name = "AIRS"
  CASE (ObsTypeAIRSL)
    name = "AIRSL"
  CASE (ObsTypeAIRSMS)
    name = "AIRSMS"
  CASE (ObsTypeAIRSRR)
    name = "AIRSRR"
  CASE (ObsTypeAIRSWF)
    name = "AIRSWF"
  CASE (ObsTypeALADIN)
    name = "ALADIN"
  CASE (ObsTypeAmdar)
    name = "AMDARS"
  CASE (ObsTypeAMSR2)
    name = "AMSR2"
  CASE (ObsTypeAMV)
    name = "AMV"
  CASE (ObsTypeAMSUB)
    name = "AMSUB"
  CASE (ObsTypeArgoBufr)
    name = "ARGOB"
  CASE (ObsTypeASCAT)
    name = "ASCAT"
  CASE (ObsTypeASCATHR)
    name = "ASCATHR"
  CASE (ObsTypeATDNET)
    name = "ATDNET"
  CASE (ObsTypeATMS)
    name = "ATMS"
  CASE (ObsTypeATMSHR)
    name = "ATMSHR"
  CASE (ObsTypeATOVS_G)
    name = "ATOVSG"
  CASE (ObsTypeATOVS_L)
    name = "ATOVSL"
  CASE (ObsTypeTOVS_G)
    name = "TOVSG"
  CASE (ObsTypeTOVS_L)
    name = "TOVSL"
  CASE (ObsTypeBathy)
    name = "BATHY"
  CASE (ObsTypeBogus)
    name = "BOGUS"
  CASE (ObsTypeBuoy)
    name = "BUOY"
  CASE (ObsTypeBuoyBufr)
    name = "BUOYB"
  CASE (ObsTypeBuoyprof)
    name = "BUOYPROF"
  CASE (ObsTypeCMAWINDS)
    name = "CMAWINDS"
  CASE (ObsTypeCOMSCSR)
    name = "COMSCSR"
  CASE (ObsTypeCopTSMO)
    name = "COPTSMO"
  CASE (ObsTypeCrIS)
    name = "CRIMSS"
  CASE (ObsTypeCrISHR)
    name = "CRIMSSHR"
  CASE (ObsTypeHIRAS, ObsTypeHIRASFSR)
    name = "CRIMSS"
  CASE (ObsTypeHIRASHR, ObsTypeHIRASFSRHR)
    name = "CRIMSSHR"
  CASE (ObsTypeDropsonde)
    name = "DROPSOND"
  CASE (ObsTypeRadwind)
    name = "DRWIND"
  CASE (ObsTypeEmaddc)
    name = "EMADDC"
  CASE (ObsTypeESACMW)
    name = "ESACMW"
  CASE (ObsTypeMVIRICSR)
    name = "ESACSR"
  CASE (ObsTypeESACSWVW)
    name = "ESACSWVW"
  CASE (ObsTypeEsahRVW)
    name = "ESAHRVW"
  CASE (ObsTypeESAHRWVW)
    name = "ESAHRWVW"
  CASE (ObsTypeEsaura)
    name = "ESAURA"
  CASE (ObsTypeEsauwa)
    name = "ESAUWA"
  CASE (ObsTypeESAUWI)
    name = "ESAUWI"
  CASE (ObsTypeGHRSST)
    name = "GHRSST"
  CASE (ObsTypeGIIRS)
    name = "GIIRS"
  CASE (ObsTypeGMIlow, ObsTypeGMIhigh)
    name = "GMI"
  CASE (ObsTypeGoesAMW)
    name = "GOESAMW"
  CASE (ObsTypeGOESBUFR)
    name = "GOESBUFR"
  CASE (ObsTypeGOESImCSR)
    name = "GOESCSR"
  CASE (ObsTypeGoesVIS)
    name = "GOESVIS"
  CASE (ObsTypeGOESWV)
    name = "GOESWV"
  CASE (ObsTypeGpsiwv)
    name = "Gpsiwv"
  CASE (ObsTypeGPSRO)
    name = "GPSRO"
  CASE (ObsTypeGroundLidar)
    name = "LIDAR"
  CASE (ObsTypeHRSeaIce)
    name = "HRSEAICE"
  CASE (ObsTypeIASI_G)
    name = "IASIG"
  CASE (ObsTypeIASI_H)
    name = "IASIHR"
  CASE (ObsTypeIASI_L)
    name = "IASIL"
  CASE (ObsTypeIMDWINDS)
    name = "IMDWINDS"
  CASE (ObsTypeIN3DImCSR)
    name = "IN3DCSR"
  CASE (ObsTypeIN3DSdCSR)
    name = "IN3DSNDR"
  CASE (ObsTypeMTSATImCSR)
    name = "JMACSR"
  CASE (ObsTypeJMAWINDS)
    name = "JMAWINDS"
  CASE (ObsTypeKMAWINDS)
    name = "KMAWINDS"
  CASE (ObsTypeSynopBufr)
    name = "LNDSYB"
  CASE (ObsTypeSynop)
    name = "LNDSYN"
  CASE (ObsTypeMeris)
    name = "MERIS"
  CASE (ObsTypeMetar)
    name = "METARS"
  CASE (ObsTypeSAPHIR)
    name = "SAPHIR"
  CASE (ObsTypeSynopMob)
    name = "MOBSYN"
  CASE (ObsTypeModes)
    name = "MODE-S"
  CASE (ObsTypeModis)
    name = "MODIS"
  CASE (ObsTypeSeviriEUM)
    name = "MSGCSR"
  CASE (ObsTypeMSGAOD)
    name = "MSGAOD"
  CASE (ObsTypeSeviriCTP)
    name = "MSGCTP"
  CASE (ObsTypeSeviriCTPFD)
    name = "MSGCTPFD"
  CASE (ObsTypeSeviriCTPUK)
    name = "MSGCTPUK"
  CASE (ObsTypeSeviriAuto)
    name = "MSGRAD"
  CASE (ObsTypeSeviriAutoFD)
    name = "MSGRADFD"
  CASE (ObsTypeSeviriAutoUK)
    name = "MSGRADUK"
  CASE (ObsTypeMSGWINDS)
    name = "MSGWINDS"
  CASE (ObsTypeMWSFY3)
    name = "MWTS"
  CASE (ObsTypeMWHSFY3)
    name = "MWHS"
  CASE (ObsTypeMWRI)
    name = "MWRI"
  CASE (ObsTypeOceanColour)
    name = "OCEANCOL"
  CASE (ObsTypeOceanFB)
    name = "OCEANFB"
  CASE (ObsTypeOceanRE)
    name = "OCEANRE"
  CASE (ObsTypeOceanTS)
    name = "OCEANTS"
  CASE (ObsTypeOCNWINDS)
    name = "OCNWINDS"
  CASE (ObsTypeOLCI)
    name = "OLCI"
  CASE (ObsTypeOpenRoad)
    name = "OPENROAD"
  CASE (ObsTypeSeaIce)
    name = "OSSEAICE"
  CASE (ObsTypeSeaIceN)
    name = "OSSEAICN"
  CASE (ObsTypeSBUVOzone)
    name = "OZONESAT"
  CASE (40700)
    name = "PAOBS"
  CASE (ObsTypePilot)
    name = "PILOT"
  CASE (ObsTypeRadopWind)
    name = "RADOP"
  CASE (ObsTypeRadop2Wind)
    name = "RADOP2"
  CASE (ObsTypeRadRefl)
    name = "RADRF"
  CASE (ObsTypeRadRefl2)
    name = "RADREFL"
  CASE (ObsTypeRadRefr)
    name = "RADREFR"
  CASE (ObsTypePrecip)
    name = "RADRRATE"
  CASE (ObsTypeRadVel)
    name = "RADVEL"
  CASE (ObsTypeGauge)
    name = "GAUGE"
  CASE (40500)
    name = "RPBOGUS"
  CASE (ObsTypeSaltSSH)
    name = "SALTSSH"
  CASE (ObsTypeSaltSSHT)
    name = "SALTSSHT"
  CASE (ObsTypeSaltSSHS)
    name = "SALTSSHS"
  CASE (ObsTypeSATAOD)
    name = "SATAOD"
  CASE (ObsTypeSatob)
    name = "SATOB"
  CASE (ObsTypeSeawinds)
    name = "SEAWINDS"
  CASE (ObsTypeSFERICS)
    name = "SFERICS"
  CASE (ObsTypeShip)
    name = "SHPSYN"
  CASE (ObsTypeShipBufr)
    name = "SHPSYB"
  CASE (ObsTypeSonde)
    name = "SONDE"
  CASE (ObsTypeSREW)
    name = "SREW"
  CASE (ObsTypeSSMIS)
    name = "SSMIS"
  CASE (40200)
    name = "SSTBOGUS"
  CASE (ObsTypeStereomv)
    name = "STEREOMV"
  CASE (ObsTypeTamdar)
    name = "TAMDAR"
  CASE (ObsTypeTCBogus)
    name = "TCBOGUS"
  CASE (22700)
    name = "TCRTEMP"
  CASE (ObsTypeTemp)
    name = "TEMP"
  CASE (ObsTypeTesac)
    name = "TESAC"
  CASE (40600)
    name = "THBOGUS"
  CASE (ObsTypeTstSonde)
    name = "TSTSONDE"
  CASE (ObsTypeUKMOSSTu)
    name = "UKMOSST"
  CASE (ObsTypeUKWINDS)
    name = "UKWINDS"
  CASE (ObsTypeWavenet)
    name = "WAVENET"
  CASE (ObsTypeWindsat)
    name = "WINDSAT"
  CASE (ObsTypeWindProf)
    name = "WINPRO"
  CASE (ObsTypeWOW)
    name = "WOW"
  CASE (ObsTypeASCATCO)
    name = "ASCATCO"
END SELECT

END FUNCTION Ops_SubTypeNumToName
