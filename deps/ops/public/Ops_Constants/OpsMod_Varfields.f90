!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Module to store varfields.
!
! *** Please remember that all additions to the list  ***
! *** of varfields need to be added to OTDP16_table1. ***
!
! Redundant varfields are not removed to ensure that numbers remain constant for
! compatibility with old varobs files.
!-------------------------------------------------------------------------------

MODULE OpsMod_Varfields

IMPLICIT NONE

SAVE

! Public declarations:
INTEGER, PARAMETER :: NumObVarMax = 83

INTEGER, PARAMETER :: VarField_pstar = 1
INTEGER, PARAMETER :: VarField_temperature = 2
INTEGER, PARAMETER :: VarField_rh = 3
INTEGER, PARAMETER :: VarField_u = 4
INTEGER, PARAMETER :: VarField_v = 5
INTEGER, PARAMETER :: VarField_logvis = 6
INTEGER, PARAMETER :: VarField_tcwv = 7
INTEGER, PARAMETER :: VarField_windspeed = 8
INTEGER, PARAMETER :: VarField_lwp = 9
INTEGER, PARAMETER :: VarField_britemp = 10
INTEGER, PARAMETER :: VarField_tskin = 11
INTEGER, PARAMETER :: VarField_gpstzdelay = 12
INTEGER, PARAMETER :: VarField_cloud = 15
INTEGER, PARAMETER :: VarField_rainrate = 16
INTEGER, PARAMETER :: VarField_mwemiss = 17
INTEGER, PARAMETER :: VarField_tcozone = 18
INTEGER, PARAMETER :: VarField_satzenith = 19
INTEGER, PARAMETER :: VarField_scanpos = 20
INTEGER, PARAMETER :: VarField_surface = 21
INTEGER, PARAMETER :: VarField_elevation = 22
INTEGER, PARAMETER :: VarField_modelsurface = 23
INTEGER, PARAMETER :: VarField_modelorog = 24
INTEGER, PARAMETER :: VarField_chanword_ir = 25
INTEGER, PARAMETER :: VarField_chanword_mw = 26
INTEGER, PARAMETER :: VarField_stratt = 27
INTEGER, PARAMETER :: VarField_satid = 28
INTEGER, PARAMETER :: VarField_satazimth = 29
INTEGER, PARAMETER :: VarField_localazimuth = 30
INTEGER, PARAMETER :: VarField_solzenith = 31
INTEGER, PARAMETER :: VarField_solazimth = 32
INTEGER, PARAMETER :: VarField_iremiss = 34
INTEGER, PARAMETER :: VarField_cloudtopp = 35
INTEGER, PARAMETER :: VarField_cloudfrac = 36
INTEGER, PARAMETER :: VarField_vnatovpp = 39
INTEGER, PARAMETER :: VarField_procoption = 40
INTEGER, PARAMETER :: VarField_amsusurface = 44
INTEGER, PARAMETER :: VarField_hirs_temp = 45
INTEGER, PARAMETER :: VarField_amsua1_temp = 46
INTEGER, PARAMETER :: VarField_amsua2_temp = 47
INTEGER, PARAMETER :: VarField_amsub_temp = 48
INTEGER, PARAMETER :: VarField_u10ambwind = 51
INTEGER, PARAMETER :: VarField_v10ambwind = 52
INTEGER, PARAMETER :: VarField_pcorrect   = 53
INTEGER, PARAMETER :: VarField_NumChans = 54
INTEGER, PARAMETER :: VarField_ChanNum = 55
INTEGER, PARAMETER :: VarField_Emissivity = 57
INTEGER, PARAMETER :: VarField_QCinfo = 58
INTEGER, PARAMETER :: VarField_refrac = 59
INTEGER, PARAMETER :: VarField_z = 60
INTEGER, PARAMETER :: VarField_SBUVozone  = 61
INTEGER, PARAMETER :: VarField_GeoBriTemp = 62
INTEGER, PARAMETER :: VarField_RadialVelocity = 63
INTEGER, PARAMETER :: VarField_RadarBeamElev = 64
INTEGER, PARAMETER :: VarField_RadarObRange = 65
INTEGER, PARAMETER :: VarField_RadarObAzim  = 66
INTEGER, PARAMETER :: VarField_GPS_Station_Height = 67
INTEGER, PARAMETER :: VarField_clw = 68
INTEGER, PARAMETER :: VarField_RadIdent = 69
INTEGER, PARAMETER :: VarField_Reflectivity = 70
INTEGER, PARAMETER :: VarField_BendingAngle = 71
INTEGER, PARAMETER :: VarField_ImpactParam = 72
INTEGER, PARAMETER :: VarField_RO_Rad_Curv = 73
INTEGER, PARAMETER :: VarField_RO_geoid_und = 74
INTEGER, PARAMETER :: VarField_RadAltAboveMSL = 75
INTEGER, PARAMETER :: VarField_BriTempVarError = 76
INTEGER, PARAMETER :: VarField_AOD = 77
INTEGER, PARAMETER :: VarField_Theta = 78
INTEGER, PARAMETER :: VarField_RadNoiseLvl = 79
INTEGER, PARAMETER :: VarField_BiasPredictors = 80
INTEGER, PARAMETER :: VarField_LevelTime = 81
INTEGER, PARAMETER :: VarField_LevelLat = 82
INTEGER, PARAMETER :: VarField_LevelLon = 83
INTEGER, PARAMETER :: VarField_RainAccum = 84
INTEGER, PARAMETER :: VarField_CloudRTError = 85
INTEGER, PARAMETER :: VarField_CloudRTBias = 86
INTEGER, PARAMETER :: VarField_Refractivity = 87
INTEGER, PARAMETER :: VarField_snowrate = 88
INTEGER, PARAMETER :: VarField_ReflectivityR = 89
INTEGER, PARAMETER :: VarField_ReflectivityI = 90
INTEGER, PARAMETER :: VarField_CeilBackscatter = 91
INTEGER, PARAMETER :: VarField_CeilRange = 92
INTEGER, PARAMETER :: VarField_CeilSiteID = 93
INTEGER, PARAMETER :: VarField_CeilScanIdent = 94
INTEGER, PARAMETER :: VarField_airqal_consttype = 95
INTEGER, PARAMETER :: VarField_airqal_massdensity = 96
INTEGER, PARAMETER :: VarField_airqal_massdensityscale = 97
INTEGER, PARAMETER :: VarField_HLOSwind = 98
INTEGER, PARAMETER :: VarField_ProfileNo = 99
INTEGER, PARAMETER :: VarField_dWinddT = 100
INTEGER, PARAMETER :: VarField_dWinddP = 101
INTEGER, PARAMETER :: VarField_AzimuthCOG = 102
INTEGER, PARAMETER :: VarField_HeightCOG = 103
INTEGER, PARAMETER :: VarField_RadFlag = 104

INTEGER, PARAMETER :: ActualMaxVarfield = 104

CONTAINS

INCLUDE 'OpsFn_VarFieldNumToName.inc'

END MODULE OpsMod_Varfields
