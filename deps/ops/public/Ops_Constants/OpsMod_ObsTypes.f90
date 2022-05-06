!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Possible settings of Obs type.
!-------------------------------------------------------------------------------

MODULE OpsMod_ObsTypes

IMPLICIT NONE

SAVE

! Public declarations:

! Surface data
INTEGER, PARAMETER :: ObsTypeSynop       = 10100  ! => generic synop
INTEGER, PARAMETER :: ObsTypeSynopManual = 10101  ! => non-automatic synop
INTEGER, PARAMETER :: ObsTypeSynopAuto   = 10102  ! => automatic synop
INTEGER, PARAMETER :: ObsTypeShip        = 10200  ! => generic ship
INTEGER, PARAMETER :: ObsTypeShipManual  = 10201  ! => non-automatic ship
INTEGER, PARAMETER :: ObsTypeShipAuto    = 10202  ! => automatic ship
INTEGER, PARAMETER :: ObsTypePlatsRiggs  = 10204  ! => platform/rigg
INTEGER, PARAMETER :: ObsTypeShipMrdBuoy = 10210  ! => moored buoy in ship
                                                   !    code
INTEGER, PARAMETER :: ObsTypeShpMrdBySpOb= 10211  ! => moored buoy super ob
                                                   !    from ship code reports
INTEGER, PARAMETER :: ObsTypeShipDrifter = 10212  ! => drifting buoy in ship
                                                   !    code
INTEGER, PARAMETER :: ObsTypeShpDrftSpOb = 10213  ! => drifting buoy super ob
                                                   !    from ship code reports
INTEGER, PARAMETER :: ObsTypeBuoy        = 10300  ! => generic buoy
INTEGER, PARAMETER :: ObsTypeMooredBuoy  = 10310  ! => moored buoy in buoy
                                                   !    code
INTEGER, PARAMETER :: ObsTypeMrdBuoySpOb = 10311  ! => moored buoy super ob
INTEGER, PARAMETER :: ObsTypeDrifter     = 10312  ! => drifting buoy in buoy
                                                   !    code
INTEGER, PARAMETER :: ObsTypeDrfBuoySpOb = 10313  ! => drifting buoy super ob
INTEGER, PARAMETER :: ObsTypePrecip      = 10400  ! => Radar precipitation
INTEGER, PARAMETER :: ObsTypeGauge       = 10401  ! => Rain gauge accumulation
INTEGER, PARAMETER :: ObsTypeSrew        = 10500  ! => SREW precipitation

INTEGER, PARAMETER :: ObsTypeRadWind     = 10601  ! => Radar radial winds
INTEGER, PARAMETER :: ObsTypeRadRefl     = 10602  ! => Radar reflectivity
INTEGER, PARAMETER :: ObsTypeRadTRH      = 10603  ! => Radar 1DVar
INTEGER, PARAMETER :: ObsTypeRadopWind   = 10604  ! => Radar radial winds
INTEGER, PARAMETER :: ObsTypeRadop2Wind  = 10605  ! => Radar radial winds (RADOP2)
INTEGER, PARAMETER :: ObsTypeRadRefr     = 10606  ! => Radar radial refractivity
INTEGER, PARAMETER :: ObsTypeRadVel      = 10607  ! => Radar radial winds
INTEGER, PARAMETER :: ObsTypeRadRefl2    = 10608  ! => Radar reflectivity (ODIM HDF5)
INTEGER, PARAMETER :: ObsTypeWavenet     = 10700  ! => Wavenet ocean wave spectra
INTEGER, PARAMETER :: ObsTypeSynopMob    = 10800  ! => mobile synop
INTEGER, PARAMETER :: ObsTypeOpenRoad    = 10900  ! => OPENROAD data
INTEGER, PARAMETER :: ObsTypeAirQal      = 11000  ! => Air quality ob
INTEGER, PARAMETER :: ObsTypeAirQalEU    = 11001  ! => Air quality EU ob
INTEGER, PARAMETER :: ObsTypeMetar       = 11100  ! => generic metar
INTEGER, PARAMETER :: ObsTypeMetarManual = 11101  ! => non-automatic metar
INTEGER, PARAMETER :: ObsTypeMetarAuto   = 11102  ! => automatic metar
INTEGER, PARAMETER :: ObsTypeSferics     = 11200  ! => SFERICS lightning strikes
INTEGER, PARAMETER :: ObsTypeATDNET      = 11300  ! => SFERICS lightning strikes
INTEGER, PARAMETER :: ObsTypeWOW         = 11400  ! => WOW observations
INTEGER, PARAMETER :: ObsTypeGroundLidar = 11501  ! => Lidarnet ceilometer backscatter
INTEGER, PARAMETER :: ObsTypeSynopBufr   = 11600  ! => generic synop

INTEGER, PARAMETER :: ObsTypeBuoyBufr        = 11700  ! => buoy bufr
INTEGER, PARAMETER :: ObsTypeMooredBuoyBufr  = 11701  ! => moored buoy bufr
INTEGER, PARAMETER :: ObsTypeDrifterBuoyBufr = 11702  ! => drifting buoy bufr

INTEGER, PARAMETER :: ObsTypeShipBufr   = 11800  ! => BUFR ship

! Satellite data
INTEGER, PARAMETER :: ObsTypeEsaura       = 20100   ! => ERS UARS(wave height)
INTEGER, PARAMETER :: ObsTypeEsauraSpOb   = 20101   ! => ERS UARS super ob
INTEGER, PARAMETER :: ObsTypeEsauwa       = 20200   ! => ERS ESA spectral wave
INTEGER, PARAMETER :: ObsTypeSeaWinds     = 20300   ! => Ku Band  scat winds
INTEGER, PARAMETER :: ObsTypeAATSRUnkn    = 20400   ! => ERS AATSR SST (unknown)
INTEGER, PARAMETER :: ObsTypeAATSRDay     = 20401   ! => ERS AATSR SST (day)
INTEGER, PARAMETER :: ObsTypeAATSRNight   = 20402   ! => ERS AATSR SST (night)
INTEGER, PARAMETER :: ObsTypeEsauwi       = 20500   ! => ERS scat winds

INTEGER, PARAMETER :: ObsTypeUKMOSSTu     = 20600   ! UKMOSST unknown
INTEGER, PARAMETER :: ObsTypeUKMOSSTd     = 20601   ! UKMOSST daytime
INTEGER, PARAMETER :: ObsTypeUKMOSSTn     = 20602   ! UKMOSST nighttime
INTEGER, PARAMETER :: ObsTypeUKMOSSTt     = 20603   ! UKMOSST twilight
INTEGER, PARAMETER :: ObsTypeALADIN       = 20700   ! => HLOS winds from Aeolus(ALADIN)

INTEGER, PARAMETER :: ObsTypeSatob        = 21000   ! => Satob
                                                     ! ** Satob SSTs **
INTEGER, PARAMETER :: ObsTypeNoaaPo       = 21001   ! => NOAA plar orbitor
INTEGER, PARAMETER :: ObsTypeMeteoSat     = 21002   ! => MeteoSat SST satob
INTEGER, PARAMETER :: ObsTypeHimawari     = 21003   ! => Himawari SST satob
INTEGER, PARAMETER :: ObsTypeIndSat       = 21004   ! => Indian SST satob
INTEGER, PARAMETER :: ObsTypeGoesE        = 21005   ! => GOES E SST satob
INTEGER, PARAMETER :: ObsTypeGoesW        = 21006   ! => GOES W SST satob
INTEGER, PARAMETER :: ObsTypeAuoSat2      = 21007   ! => AutoSat 2 SST

                                                     ! ** Satwinds **
INTEGER, PARAMETER :: ObsTYPEAMV          = 21010   ! => Combined AMV data
INTEGER, PARAMETER :: ObsTypeSatobIR      = 21021   ! => Satob - Infrared
INTEGER, PARAMETER :: ObsTypeSatobVIS     = 21022   ! => Satob - Visible
INTEGER, PARAMETER :: ObsTypeSatobWV      = 21023   ! => Satob - Water Vapour
INTEGER, PARAMETER :: ObsTypeSatobMC      = 21024   ! => Satob - Multi-channel
INTEGER, PARAMETER :: ObsTypeGoesamw      = 22000   ! => HD GOES Satob, infrared
INTEGER, PARAMETER :: ObsTypeGoesvis      = 22002   ! => HD GOES Satob, visible
INTEGER, PARAMETER :: ObsTypeGoeswv       = 22003   ! => HD GOES Satob, water vapour
INTEGER, PARAMETER :: ObsTypeEsahrvw      = 22100   ! => EUMETSAT - visible
INTEGER, PARAMETER :: ObsTypeEsacmw       = 22200   ! => EUMETSAT - combined (LR)
INTEGER, PARAMETER :: ObsTypeEsacmwir     = 22201   ! => EUMETSAT - infrared (LR)
INTEGER, PARAMETER :: ObsTypeEsacmwvis    = 22202   ! => EUMETSAT - visible (LR)
INTEGER, PARAMETER :: ObsTypeEsacmwwv     = 22203   ! => EUMETSAT - water vapour (LR)
INTEGER, PARAMETER :: ObsTypeEsacswvw     = 22300   ! => EUMETSAT - clear sky water vapour
INTEGER, PARAMETER :: ObsTypeEsahrwvw     = 22400   ! => EUMETSAT - cloudy water vapour
INTEGER, PARAMETER :: ObsTypeGoesbufr     = 22500   ! => NESDIS - unknown channel
INTEGER, PARAMETER :: ObsTypeGoesbufrIR   = 22501   ! => NESDIS - infrared 10.8
INTEGER, PARAMETER :: ObsTypeGoesbufrVIS  = 22502   ! => NESDIS - visible
INTEGER, PARAMETER :: ObsTypeGoesbufrWV   = 22503   ! => NESDIS - cloudy water vapour
INTEGER, PARAMETER :: ObsTypeGoesbufrCSWV = 22505   ! => NESDIS - clear sky water vapour
INTEGER, PARAMETER :: ObsTypeGoesbufrWV62 = 22531   ! => NESDIS - cloudy water vapour 6.2
INTEGER, PARAMETER :: ObsTypeGoesbufrWV73 = 22532   ! => NESDIS - cloudy water vapour 7.3
INTEGER, PARAMETER :: ObsTypeGoesbufrCSWV62 = 22551   ! => NESDIS - clear sky water vapour 6.2
INTEGER, PARAMETER :: ObsTypeGoesbufrCSWV73 = 22552   ! => NESDIS - clear sky water vapour 7.3
INTEGER, PARAMETER :: ObsTypeGoesbufrIR38 = 22511   ! => NESDIS - infrared 3.8
INTEGER, PARAMETER :: ObsTypeGoesbufrIR16 = 22515   ! => NESDIS - infrared 1.6
INTEGER, PARAMETER :: ObsTypeModis        = 22600   ! => MODIS - unknown channel
INTEGER, PARAMETER :: ObsTypeModisIR      = 22601   ! => MODIS - infrared 10.8
INTEGER, PARAMETER :: ObsTypeModisWV      = 22603   ! => MODIS - cloudy water vapour
INTEGER, PARAMETER :: ObsTypeModisMIXWV   = 22604   ! => MODIS - mixed water vapour
INTEGER, PARAMETER :: ObsTypeModisCSWV    = 22605   ! => MODIS - clear sky water vapour
INTEGER, PARAMETER :: ObsTypeModisIR38    = 22611   ! => MODIS - infrared 3.8
INTEGER, PARAMETER :: ObsTypeModisIR16    = 22615   ! => MODIS - infrared 1.6
INTEGER, PARAMETER :: ObsTypeJmawinds     = 23500   ! => JMA - unknown channel
INTEGER, PARAMETER :: ObsTypeJmawindsIR   = 23501   ! => JMA - infrared
INTEGER, PARAMETER :: ObsTypeJmawindsVIS  = 23502   ! => JMA - visible
INTEGER, PARAMETER :: ObsTypeJmawindsWV   = 23503   ! => JMA - cloudy water vapour
INTEGER, PARAMETER :: ObsTypeJmawindsCSWV = 23505   ! => JMA - clear sky water vapour
INTEGER, PARAMETER :: ObsTypeJmawindsMIXWV= 23507   ! => JMA - mixed water vapour
INTEGER, PARAMETER :: ObsTypeJmawindsIR38 = 23511   ! => JMA - infrared 3.8
INTEGER, PARAMETER :: ObsTypeJmawindsWV62 = 23531   ! => JMA - cloudy water vapour 6.2
INTEGER, PARAMETER :: ObsTypeJmawindsWV73 = 23532   ! => JMA - cloudy water vapour 7.3
INTEGER, PARAMETER :: ObsTypeJmawindsCSWV62 = 23551   ! => JMA - clear sky water vapour 6.2
INTEGER, PARAMETER :: ObsTypeJmawindsCSWV73 = 23552   ! => JMA - clear sky water vapour 7.3
INTEGER, PARAMETER :: ObsTypeMsgwinds     = 23600   ! => MSG - unknown channel
INTEGER, PARAMETER :: ObsTypeMsgIR38      = 23611   ! => MSG - infrared 3.8
INTEGER, PARAMETER :: ObsTypeMsgIR87      = 23612   ! => MSG - infrared 8.7
INTEGER, PARAMETER :: ObsTypeMsgIR108     = 23613   ! => MSG - infrared 10.8
INTEGER, PARAMETER :: ObsTypeMsgOZONE     = 23614   ! => MSG - infrared 9.7
INTEGER, PARAMETER :: ObsTypeMsgHRVIS     = 23621   ! => MSG - high res. visible
INTEGER, PARAMETER :: ObsTypeMsgVIS06     = 23622   ! => MSG - visible 0.6
INTEGER, PARAMETER :: ObsTypeMsgVIS08     = 23623   ! => MSG - visible 0.8
INTEGER, PARAMETER :: ObsTypeMsgWV62      = 23631   ! => MSG - cloudy water vapour 6.2
INTEGER, PARAMETER :: ObsTypeMsgWV73      = 23632   ! => MSG - cloudy water vapour 7.3
INTEGER, PARAMETER :: ObsTypeMsgCSWV62    = 23651   ! => MSG - clear sky water vapour 6.2
INTEGER, PARAMETER :: ObsTypeMsgCSWV73    = 23652   ! => MSG - clear sky water vapour 7.3
INTEGER, PARAMETER :: ObsTypeCmawinds     = 24900   ! => CMA - unknown channel
INTEGER, PARAMETER :: ObsTypeCmawindsIR   = 24901   ! => CMA - infrared
INTEGER, PARAMETER :: ObsTypeCmawindsVIS  = 24902   ! => CMA - visible
INTEGER, PARAMETER :: ObsTypeCmawindsWV   = 24903   ! => CMA - cloudy water vapour
INTEGER, PARAMETER :: ObsTypeCmawindsCSWV = 24905   ! => CMA - clear sky water vapour
INTEGER, PARAMETER :: ObsTypeCmawindsMIXWV= 24907   ! => CMA - mixed water vapour
INTEGER, PARAMETER :: ObsTypeCmawindsIR38 = 24911   ! => CMA - infrared 3.8
INTEGER, PARAMETER :: ObsTypeImdwinds     = 25200   ! => IMD - unknown channel
INTEGER, PARAMETER :: ObsTypeImdwindsIR   = 25201   ! => IMD - infrared
INTEGER, PARAMETER :: ObsTypeImdwindsVIS  = 25202   ! => IMD - visible
INTEGER, PARAMETER :: ObsTypeImdwindsWV   = 25203   ! => IMD - cloudy WV
INTEGER, PARAMETER :: ObsTypeImdwindsCSWV = 25205   ! => IMD - clear sky WV
INTEGER, PARAMETER :: ObsTypeImdwindsMIXWV= 25207   ! => IMD - mixed WV
INTEGER, PARAMETER :: ObsTypeImdwindsIR38 = 25211   ! => IMD - infrared 3.8
INTEGER, PARAMETER :: ObsTypeKmawinds     = 25900   ! => KMA - unknown channel
INTEGER, PARAMETER :: ObsTypeKmawindsIR   = 25901   ! => KMA - infrared
INTEGER, PARAMETER :: ObsTypeKmawindsVIS  = 25902   ! => KMA - visible
INTEGER, PARAMETER :: ObsTypeKmawindsWV   = 25903   ! => KMA - cloudy WV
INTEGER, PARAMETER :: ObsTypeKmawindsCSWV = 25905   ! => KMA - clear sky WV
INTEGER, PARAMETER :: ObsTypeKmawindsMIXWV= 25907   ! => KMA - mixed WV
INTEGER, PARAMETER :: ObsTypeKmawindsIR38 = 25911   ! => KMA - infrared 3.8

INTEGER, PARAMETER :: ObsTypeUkwinds      = 26400   ! => UK MSG - unknown channel
INTEGER, PARAMETER :: ObsTypeUkIR38       = 26411   ! => UK MSG - infrared 3.8
INTEGER, PARAMETER :: ObsTypeUkIR87       = 26412   ! => UK MSG - infrared 8.7
INTEGER, PARAMETER :: ObsTypeUkIR108      = 26413   ! => UK MSG - infrared 10.8
INTEGER, PARAMETER :: ObsTypeUkIR120      = 26415   ! => UK MSG - infrared 12.0
INTEGER, PARAMETER :: ObsTypeUkOZONE      = 26414   ! => UK MSG - infrared 9.7
INTEGER, PARAMETER :: ObsTypeUkHRVIS      = 26421   ! => UK MSG - high res. visible
INTEGER, PARAMETER :: ObsTypeUkVIS06      = 26422   ! => UK MSG - visible 0.6
INTEGER, PARAMETER :: ObsTypeUkVIS08      = 26423   ! => UK MSG - visible 0.8
INTEGER, PARAMETER :: ObsTypeUkWV62       = 26431   ! => UK MSG - cloudy water vapour 6.2
INTEGER, PARAMETER :: ObsTypeUkWV73       = 26432   ! => UK MSG - cloudy water vapour 7.3
INTEGER, PARAMETER :: ObsTypeUkCSWV62     = 26451   ! => UK MSG - clear sky water vapour 6.2
INTEGER, PARAMETER :: ObsTypeUkCSWV73     = 26452   ! => UK MSG - clear sky water vapour 7.3
INTEGER, PARAMETER :: ObsTypeStereomv     = 27100   ! => STEREOMV - unknown type
INTEGER, PARAMETER :: ObsTypeStereomvVIS06= 27122   ! => STEREOMV - MISR winds (VIS 0.6)

INTEGER, PARAMETER :: ObsTypeTOVS_G       = 21100   ! ** global TOVS **
INTEGER, PARAMETER :: ObsTypeTOVS_G2      = 21101   ! => products only
INTEGER, PARAMETER :: ObsTypeTOVS_G1D     = 21102   ! => radiances/products
INTEGER, PARAMETER :: ObsTypeTOVS_G1E     = 21103   ! => G1D with NESDIS rad's
INTEGER, PARAMETER :: ObsTypeTOVS_L       = 21200   ! ** local TOVS **
INTEGER, PARAMETER :: ObsTypeTOVS_L2      = 21201   ! => products only
INTEGER, PARAMETER :: ObsTypeTOVS_L1D     = 21202   ! => radiances/products

INTEGER, PARAMETER :: ObsTypeATOVS_G      = 21400   ! ** global ATOVS **
INTEGER, PARAMETER :: ObsTypeATOVS_G2     = 21401   ! => products only
INTEGER, PARAMETER :: ObsTypeATOVS_G1D    = 21402   ! => radiances/products
INTEGER, PARAMETER :: ObsTypeATOVS_L      = 21500   ! ** local ATOVS **
INTEGER, PARAMETER :: ObsTypeATOVS_L2     = 21501   ! => products only
INTEGER, PARAMETER :: ObsTypeATOVS_L1D    = 21502   ! => radiances/products
INTEGER, PARAMETER :: ObsTypeMWSFY3B      = 21450   ! => Global FY3B as ATOVS subtype

INTEGER, PARAMETER :: ObsTypeGpsiwv      = 21700   ! => Ground GPS

INTEGER, PARAMETER :: ObsTypeATMS         = 21800   ! => ATMS radiances
INTEGER, PARAMETER :: ObsTypeATMSHR       = 21900   ! => High res ATMS radiances

INTEGER, PARAMETER :: ObsTypeAIRS         = 22800   ! => AIRS Radiances

INTEGER, PARAMETER :: ObsTypeGPSRO        = 22900   ! => GPSRO

INTEGER, PARAMETER :: ObsTypeSAltSSH      = 23000   ! => Altimeter SSH
INTEGER, PARAMETER :: ObsTypeSAltSSHT     = 23001   ! => Altimeter SSH (real time)
INTEGER, PARAMETER :: ObsTypeSAltSSHS     = 23002   ! => Altimeter SSH with extra variables

INTEGER, PARAMETER :: ObsTypeSSMIS        = 23100   ! => SSMI/S

INTEGER, PARAMETER :: ObsTypeAMSUB        = 23200   ! => full res AMSUB

INTEGER, PARAMETER :: ObsTypeSBUVozone    = 23700   ! => SBUV ozone

INTEGER, PARAMETER :: ObsTypeAIRSRR       = 23900   ! => AIRS Reconstructed Radiances

INTEGER, PARAMETER :: ObsTypeAIRSWF       = 24000   ! => AIRS Warmest FOV Radiances
INTEGER, PARAMETER :: ObsTypeIASI_G       = 24100   ! => Global IASI
INTEGER, PARAMETER :: ObsTypeIASI_L       = 24200   ! => Local  IASI
INTEGER, PARAMETER :: ObsTypeASCAT        = 24300   ! => ASCAT BUFR
INTEGER, PARAMETER :: ObsTypeGHRSST       = 24400   ! => GHRSST
INTEGER, PARAMETER :: ObsTypeSEVIRIEUM    = 24500   ! => SEVIRI EUMETSAT
                                                      !    radiance
INTEGER, PARAMETER :: ObsTypeSEVIRIAUTO   = 24600   ! => SEVIRI AUTOSAT
                                                      !    radiance
INTEGER, PARAMETER :: ObsTypeSEVIRIAUTOUK = 24610   ! => SEVIRI AUTOSAT
                                                      !    radiance for UK area
INTEGER, PARAMETER :: ObsTypeSEVIRIAUTOFD = 24650   ! => SEVIRI AUTOSAT
                                                      !    radiance for full disc
INTEGER, PARAMETER :: ObsTypeSEVIRICTP    = 24660   ! => SEVIRI AUTOSAT
                                                      !    MSGCTP data
INTEGER, PARAMETER :: ObsTypeSEVIRICTPUK  = 24670   ! => SEVIRI AUTOSAT
                                                      !    MSGCTP data for UK area
INTEGER, PARAMETER :: ObsTypeSEVIRICTPFD  = 24680   ! => SEVIRI AUTOSAT
                                                      !    MSGCTP data for full disc
INTEGER, PARAMETER :: ObsTypeAIRSMS       = 24700   ! => AIRS Clearest MODIS FOV
INTEGER, PARAMETER :: ObsTypeWINDSAT      = 24800   ! => WINDSAT BUFR
INTEGER, PARAMETER :: ObsTypeAMSR2        = 25000   ! => AMSR2 radiances
INTEGER, PARAMETER :: ObsTypeAIRSL        = 25100   ! => AIRS locally received data
INTEGER, PARAMETER :: ObsTypeIASI_H       = 25300   ! => High Res IASI
INTEGER, PARAMETER :: ObsTypeMERIS        = 25400   ! => MERIS
INTEGER, PARAMETER :: ObsTypeOLCI         = 25410   ! => OLCI
INTEGER, PARAMETER :: ObsTypeASCATHR      = 25500   ! => HiRes ASCAT BUFR
INTEGER, PARAMETER :: ObsTypeMSGAOD       = 25600   ! => SEVIRI AOD
INTEGER, PARAMETER :: ObsTypeMVIRICSR     = 25700   ! => MVIRI radiances
INTEGER, PARAMETER :: ObsTypeGOESImCSR    = 25800   ! => GOES Imager radiances
                                                    ! 259xx defined further up
INTEGER, PARAMETER :: ObsTypeCrIS         = 26000   ! => CrIS+ATMS (global, thinned)
INTEGER, PARAMETER :: ObsTypeCrISFSR      = 26001   ! => CrIS FSR +ATMS (global, thinned)

INTEGER, PARAMETER :: ObsTypeCRISHR       = 26100   ! => CrIS+ATMS (UK, full res)
INTEGER, PARAMETER :: ObsTypeCRISFSRHR    = 26101   ! => CrIS FSR +ATMS (UK, full res)

INTEGER, PARAMETER :: ObsTypeCOMSCSR      = 26200   ! => COMS Imager radiances
INTEGER, PARAMETER :: ObsTypeMTSATImCSR   = 26300   ! => MTSAT Imager radiances
INTEGER, PARAMETER :: ObsTypeAHICSR       = 26310   ! => AHI radiances
INTEGER, PARAMETER :: ObsTypeAHIASR       = 26320   ! => AHI all-sky radiances
INTEGER, PARAMETER :: ObsTypeABICSR       = 26330   ! => ABI (GOES-16 onwards) radiances
INTEGER, PARAMETER :: ObsTypeSATAOD       = 26600   ! => Satellite AOD (Polar)
INTEGER, PARAMETER :: ObsTypeASCATCO      = 26700   ! => ASCAT coastal winds
INTEGER, PARAMETER :: ObsTypeIN3DImCSR    = 26800   ! => INSAT3D Imager radiances
INTEGER, PARAMETER :: ObsTypeIN3DSdCSR    = 26900   ! => INSAT3D Sounder radiances
INTEGER, PARAMETER :: ObsTypeMWSFY3       = 27000   ! => FY-3C/D MWTS-2 and MWHS-2 radiances
INTEGER, PARAMETER :: ObsTypeMWHSFY3      = 27001   ! => MWHS-2 radiances full resolution

                                                    ! 271xx defined (satwinds - StereoMV)
INTEGER, PARAMETER :: ObsTypeOCNWINDS     = 27200   ! => Satellite ocean wind speed
INTEGER, PARAMETER :: ObsTypeSAPHIR       = 28000   ! => MT SAPHIR radiances
INTEGER, PARAMETER :: ObsTypeMWRI         = 29000   ! => MWRI radiances
INTEGER, PARAMETER :: ObsTypeGMIlow       = 29100   ! => GMI low freq channels
INTEGER, PARAMETER :: ObsTypeGMIhigh      = 29200   ! => GMI high freq channels

INTEGER, PARAMETER :: ObsTypeHIRAS        = 29300   ! => HIRAS 
INTEGER, PARAMETER :: ObsTypeHIRASFSR     = 29301   ! => HIRAS FSR 
INTEGER, PARAMETER :: ObsTypeHIRASHR      = 29302   ! => HIRAS (UK)
INTEGER, PARAMETER :: ObsTypeHIRASFSRHR   = 29303   ! => HIRAS FSR (UK)

INTEGER, PARAMETER :: ObsTypeGIIRS        = 29400   ! => GIIRS

! Aircraft data
INTEGER, PARAMETER :: ObsTypeAmdar        = 30100  ! => Amdar
INTEGER, PARAMETER :: ObsTypeAirep        = 30200  ! => airep
INTEGER, PARAMETER :: ObsTypeTamdar       = 30300  ! => Tamdar
INTEGER, PARAMETER :: ObsTypeModeS        = 30500  ! => MODE-S
INTEGER, PARAMETER :: ObsTypeEmaddc       = 30600  ! => EMADDC

! Bogus data
INTEGER, PARAMETER :: ObsTypeTCBogus      = 40100  ! => TC Bogus
INTEGER, PARAMETER :: ObsTypeBogus        = 40300  ! => Surface or sonde bogus

! Atmospheric profile data
INTEGER, PARAMETER :: ObsTypeTemp         = 50100   ! => generic temp
INTEGER, PARAMETER :: ObsTypeTempLand     = 50101   ! => temp
INTEGER, PARAMETER :: ObsTypeTempShip     = 50102   ! => temp ship
INTEGER, PARAMETER :: ObsTypeTempMobile   = 50103   ! => temp mobile
INTEGER, PARAMETER :: ObsTypePilot        = 50200   ! => generic pilot
INTEGER, PARAMETER :: ObsTypePilotLand    = 50201   ! => pilot
INTEGER, PARAMETER :: ObsTypePilotShip    = 50202   ! => pilot ship
INTEGER, PARAMETER :: ObsTypePilotMobile  = 50203   ! => pilot mobile
INTEGER, PARAMETER :: ObsTypeDropSonde    = 50300   ! => drop sonde
INTEGER, PARAMETER :: ObsTypeWindProf     = 50400   ! => wind profiler
INTEGER, PARAMETER :: ObsTypeSonde        = 50500   ! => sonde (BUFR)
INTEGER, PARAMETER :: ObsTypeTSTSonde     = 50501   ! => sonde (BUFR)

! Ocean profiles
INTEGER, PARAMETER :: ObsTypeBathy        = 60100   ! => Bathy
INTEGER, PARAMETER :: ObsTypeTesac        = 60200   ! => Tesac
INTEGER, PARAMETER :: ObsTypeArgo         = 60201   ! => ARGO floats
INTEGER, PARAMETER :: ObsTypeArgoBufr     = 60202   ! => ARGO floats (BUFR)
INTEGER, PARAMETER :: ObsTypeBuoyProf     = 60300   ! => Buoy profiles
INTEGER, PARAMETER :: ObsTypeOceanFB      = 60404   ! => NetCDF Ocean Ferrybox
INTEGER, PARAMETER :: ObsTypeOceanRE      = 60402   ! => NetCDF Ocean EXRE0206 EXRE0186
INTEGER, PARAMETER :: ObsTypeOceanTS      = 60403   ! => NetCDF Ocean FNCM FAC8862 FNHO FHZI HOWN VHW5167
INTEGER, PARAMETER :: ObsTypeCopTSMO      = 60405   ! => NetCDF Ocean moorings time series from Copernicus Marine Service

! Sea Ice
INTEGER, PARAMETER :: ObsTypeSeaIce       = 60500   ! => Sea ice concentration (OSSEAICE)
INTEGER, PARAMETER :: ObsTypeHRSeaIce     = 60600   ! => Sea ice concentration (HRSEAICE)
INTEGER, PARAMETER :: ObsTypeSeaIceN      = 60700   ! => Sea ice concentration (OSSEAICN)

! Ocean colour
INTEGER, PARAMETER :: ObsTypeOceanColour  = 61000   ! => Ocean colour

CONTAINS

INCLUDE 'Ops_SubTypeNameToNum.inc'
INCLUDE 'Ops_SubTypeNumToName.inc'

END MODULE OpsMod_ObsTypes
