!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Possible settings of ObsGroup in OBheader_type, CXheader_type.  We do not
! delete obs group numbers from here even if support is removed from the code.
! This is to ensure that the numbers remain consistent so that e.g. old obstores
! can be handled.
!
! For all changes please make sure that all the include'ed subroutines at the
! bottom are updated
!-------------------------------------------------------------------------------

MODULE OpsMod_ObsGroupInfo

IMPLICIT NONE

SAVE

! Public declarations:

INTEGER, PARAMETER :: ObsGroupSurface      =   1 ! => Conventional surface reports
INTEGER, PARAMETER :: ObsGroupScatwind     =   2 ! => ERS Scatterometer winds
INTEGER, PARAMETER :: ObsGroupSatwind      =   3 ! => cloud track winds
INTEGER, PARAMETER :: ObsGroupAircraft     =   4 ! => aircraft reports
INTEGER, PARAMETER :: ObsGroupSonde        =   5 ! => radiosonde reports
INTEGER, PARAMETER :: ObsGroupATOVS        =   7 ! => ATOVS through 1DVAR
INTEGER, PARAMETER :: ObsGroupAltim        =  10 ! => ERS Altimeter data
INTEGER, PARAMETER :: ObsGroupOcean        =  11 ! => Bathys and Tesacs
INTEGER, PARAMETER :: ObsGroupSatSST       =  12 ! => Satellite SST measurements
INTEGER, PARAMETER :: ObsGroupGroundGPS    =  14 ! => GroundGPS satellite obs
INTEGER, PARAMETER :: ObsGroupAIRS         =  16 ! => AIRS+AMSU radiances
INTEGER, PARAMETER :: ObsGroupGPSRO        =  18 ! => GPSRO data
INTEGER, PARAMETER :: ObsGroupSSMIS        =  19 ! => SSMI/S through 1DVAR
INTEGER, PARAMETER :: ObsGroupAMSUB        =  20 ! => AMSUB at full res
INTEGER, PARAMETER :: ObsGroupSBUV         =  23 ! => SBUV ozone density
INTEGER, PARAMETER :: ObsGroupRadar        =  24 ! => Radar winds & rain rate
INTEGER, PARAMETER :: ObsGroupIASI         =  26 ! => IASI plus AMSU
INTEGER, PARAMETER :: ObsGroupSEVIRIClr    =  27 ! => SEVIRI Clear data
INTEGER, PARAMETER :: ObsGroupGeoCloud     =  28 ! => AUTOSAT/SEVIRI cloud products
INTEGER, PARAMETER :: ObsGroupAMSR         =  29 ! => AMSR  data
INTEGER, PARAMETER :: ObsGroupSeaIce       =  30 ! => Sea ice data
INTEGER, PARAMETER :: ObsGroupRadarZ       =  31 ! => Radar reflectivity
INTEGER, PARAMETER :: ObsGroupSatTCWV      =  32 ! => Satellite TCWV obs
INTEGER, PARAMETER :: ObsGroupPrecip       =  33 ! => radar surface precip rate
INTEGER, PARAMETER :: ObsGroupAOD          =  34 ! => AOD (MSG, MODIS)
INTEGER, PARAMETER :: ObsGroupMVIRIClr     =  35 ! => MVIRI Clear data
INTEGER, PARAMETER :: ObsGroupGOESImClr    =  36 ! => GOES Imager Clear data
INTEGER, PARAMETER :: ObsGroupABIClr       =  37 ! => ABI (GOES-16 onwards) radiances
INTEGER, PARAMETER :: ObsGroupATMS         =  38 ! => ATMS radiances
INTEGER, PARAMETER :: ObsGroupCrIS         =  39 ! => CrIS+ATMS radiances
INTEGER, PARAMETER :: ObsGroupCOMSMIClr    =  40 ! => COMS M Imager Clear data
INTEGER, PARAMETER :: ObsGroupMTSATImClr   =  41 ! => MTSAT Imager Clear data
INTEGER, PARAMETER :: ObsGroupSurfaceCloud =  42 ! => Surface cloud reports
INTEGER, PARAMETER :: ObsGroupMWSFY3B      =  43 ! => FY3B data
INTEGER, PARAMETER :: ObsGroupMWSFY3       =  44 ! => FY3C/D microwave sounder data
INTEGER, PARAMETER :: ObsGroupSAPHIR       =  45 ! => SAPHIR data
INTEGER, PARAMETER :: ObsGroupRainAccum    =  46 ! => Rain accumulation data
INTEGER, PARAMETER :: ObsGroupIN3DIClr     =  47 ! => INSAT3D Imager Clear Sky data
INTEGER, PARAMETER :: ObsGroupIN3DS        =  48 ! => INSAT3D Sounder Clear Sky data
INTEGER, PARAMETER :: ObsGroupRadarN       =  49 ! => Radar refractivity
INTEGER, PARAMETER :: ObsGroupGroundLidar  =  50 ! => Ground-based lidar/ceilometers
INTEGER, PARAMETER :: ObsGroupAHIClr       =  51 ! => AHI (Himawari) Clear Sky data
INTEGER, PARAMETER :: ObsGroupTOVS         =  52 ! => TOVS through 1DVAR
INTEGER, PARAMETER :: ObsGroupSEVIRIASR    =  53 ! => SEVIRI all-sky radiances
INTEGER, PARAMETER :: ObsGroupAHIASR       =  54 ! => AHI all-sky radiances
INTEGER, PARAMETER :: ObsGroupMWRI         =  55 ! => MWRI data
INTEGER, PARAMETER :: ObsGroupGMIlow       =  56 ! => GMI low freq channels
INTEGER, PARAMETER :: ObsGroupGMIhigh      =  57 ! => GMI high freq channels
INTEGER, PARAMETER :: ObsGroupHLOSwind     =  58 ! => ALADIN-AEOLUS HLOS wind
INTEGER, PARAMETER :: ObsGroupOceanColour  =  59 ! => Ocean Colour Data
INTEGER, PARAMETER :: ObsGroupHIRAS        =  60 ! => HIRAS 
INTEGER, PARAMETER :: ObsGroupOceanWinds   =  61 ! => Satellite wind speeds
INTEGER, PARAMETER :: ObsGroupGIIRSLW      =  62 ! => GIIRS LW radiances
INTEGER, PARAMETER :: ObsGroupGIIRSMW      =  63 ! => GIIRS MW radiances
INTEGER, PARAMETER :: ObsGroupScatwindChosen =  64 ! => Scatterometer winds (single solution)
INTEGER, PARAMETER :: ObsGroupFCIASR       =  65 ! => FCI all-sky radiances
INTEGER, PARAMETER :: ObsGroupFCIClr       =  66 ! => FCI clear-sky radiances
INTEGER, PARAMETER :: max_obs_group_num    =  66 ! Number of observation groups

CONTAINS

INCLUDE 'OpsFn_ASRToClrGeoGroup.inc'
INCLUDE 'OpsFn_IsSatRadGroup.inc'
INCLUDE 'OpsFn_IsAHIGroup.inc'
INCLUDE 'OpsFn_IsFCIGroup.inc'
INCLUDE 'OpsFn_IsSEVIRIGroup.inc'
INCLUDE 'OpsFn_IsASRGeoGroup.inc'
INCLUDE 'OpsFn_IsClrGeoGroup.inc'
INCLUDE 'OpsFn_IsModelLevelGroup.inc'
INCLUDE 'OpsFn_ObsGroupNameToNum.inc'
INCLUDE 'OpsFn_ObsGroupNumToName.inc'

END MODULE OpsMod_ObsGroupInfo
