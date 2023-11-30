MODULE OpsMod_BufrDescriptors

!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! DESCRIPTION
!   Contains BUFR descriptors for use in routines for associated data file
!   generation.
!
!-------------------------------------------------------------------------------

IMPLICIT NONE

SAVE

! Public declarations:

INTEGER, PARAMETER :: BD_CallSign           =   1005
INTEGER, PARAMETER :: BD_CldCost            =   2222
INTEGER, PARAMETER :: BD_Year               =   4001
INTEGER, PARAMETER :: BD_Month              =   4002
INTEGER, PARAMETER :: BD_Day                =   4003
INTEGER, PARAMETER :: BD_Hour               =   4004
INTEGER, PARAMETER :: BD_Minute             =   4005
INTEGER, PARAMETER :: BD_Second             =   4006
INTEGER, PARAMETER :: BD_ForecastTime       =   4015
INTEGER, PARAMETER :: BD_Lat                =   5001
INTEGER, PARAMETER :: BD_LatitudeHiRes      =   5001
INTEGER, PARAMETER :: BD_Latitude           =   5002
INTEGER, PARAMETER :: BD_ChanNums           =   5042
INTEGER, PARAMETER :: BD_CoastDist          =   5215
INTEGER, PARAMETER :: BD_Long               =   6001
INTEGER, PARAMETER :: BD_LongitudeHiRes     =   6001
INTEGER, PARAMETER :: BD_Longitude          =   6002
INTEGER, PARAMETER :: BD_EstStnHtP          =   7001
INTEGER, PARAMETER :: BD_PlevelsA           =   7004
INTEGER, PARAMETER :: BD_ModelSurfaceHeight =   7007
INTEGER, PARAMETER :: BD_SolarZenith        =   7025
INTEGER, PARAMETER :: BD_AnemoHeight        =   7194
INTEGER, PARAMETER :: BD_Depth              =   7062
INTEGER, PARAMETER :: BD_ModelSurfaceType   =   8204
INTEGER, PARAMETER :: BD_P                  =  10004
INTEGER, PARAMETER :: BD_Zstd               =  10007
INTEGER, PARAMETER :: BD_Z                  =  10008
INTEGER, PARAMETER :: BD_Hawson             =  10192
INTEGER, PARAMETER :: BD_SurPres            =  10193
INTEGER, PARAMETER :: BD_BLheight           =  10194
INTEGER, PARAMETER :: BD_HeightRange        =  10195
INTEGER, PARAMETER :: BD_WindSpd            =  11002
INTEGER, PARAMETER :: BD_U                  =  11003
INTEGER, PARAMETER :: BD_V                  =  11004
INTEGER, PARAMETER :: BD_10mWindDirCor      =  11011
INTEGER, PARAMETER :: BD_10mWind            =  11012
INTEGER, PARAMETER :: BD_10mWindSpdCor      =  11012
INTEGER, PARAMETER :: BD_InvRichNumber      =  11201
INTEGER, PARAMETER :: BD_T                  =  12001
INTEGER, PARAMETER :: BD_2mT                =  12004
INTEGER, PARAMETER :: BD_SoilTemp           =  12030
INTEGER, PARAMETER :: BD_Tskin              =  12061
INTEGER, PARAMETER :: BD_BriTemp            =  12063
INTEGER, PARAMETER :: BD_BriHirs            =  12063
INTEGER, PARAMETER :: BD_Theta              =  12193
INTEGER, PARAMETER :: BD_LapseRate          =  12194
INTEGER, PARAMETER :: BD_CLW                =  13002
INTEGER, PARAMETER :: BD_RH                 =  13003
INTEGER, PARAMETER :: BD_TCWV               =  13016
INTEGER, PARAMETER :: BD_PrecipAcc6hr       =  13021
INTEGER, PARAMETER :: BD_SnowAmount         =  13199
INTEGER, PARAMETER :: BD_SoilMoisture       =  13203
INTEGER, PARAMETER :: BD_SpecIREmiss        =  14021
INTEGER, PARAMETER :: BD_Radiance           =  14045
INTEGER, PARAMETER :: BD_SWradiation        =  14062
INTEGER, PARAMETER :: BD_MwEmiss            =  14193
INTEGER, PARAMETER :: BD_IrEmiss            =  14200
INTEGER, PARAMETER :: BD_Ozone              =  15001
INTEGER, PARAMETER :: BD_ZTD                =  15031
INTEGER, PARAMETER :: BD_ZWD                =  15035
INTEGER, PARAMETER :: BD_Refrac             =  15036
INTEGER, PARAMETER :: BD_BendingAngle       =  15037
INTEGER, PARAMETER :: BD_Vis                =  20001
INTEGER, PARAMETER :: BD_Cloud              =  20010
INTEGER, PARAMETER :: BD_LowCloudAmount     =  20011
INTEGER, PARAMETER :: BD_MedCloudAmount     =  20011
INTEGER, PARAMETER :: BD_LowCloudBase       =  20013
INTEGER, PARAMETER :: BD_LWP                =  21031
INTEGER, PARAMETER :: BD_WindRPCD           =  21067
INTEGER, PARAMETER :: BD_WindIndex          =  21102
INTEGER, PARAMETER :: BD_WindQual           =  21109
INTEGER, PARAMETER :: BD_ScatModC           =  21119
INTEGER, PARAMETER :: BD_UWIProductConf     =  21197
INTEGER, PARAMETER :: BD_ConeWind           =  21251
INTEGER, PARAMETER :: BD_WindRetSkill       =  21252
INTEGER, PARAMETER :: BD_FrictionVel        =  20202
INTEGER, PARAMETER :: BD_ObukhovLength      =  20205
INTEGER, PARAMETER :: BD_SeaTemp            =  22043
INTEGER, PARAMETER :: BD_SeaSurfTemp        =  22049
INTEGER, PARAMETER :: BD_Salt               =  22062
INTEGER, PARAMETER :: BD_WaveHght           =  22070
INTEGER, PARAMETER :: BD_SeaIce             =  22194
INTEGER, PARAMETER :: BD_OceanRo            =  22195
INTEGER, PARAMETER :: BD_SeaHght            =  22196
INTEGER, PARAMETER :: BD_ChannelHIRS        =  25045
INTEGER, PARAMETER :: BD_ChannelAMSA        =  25048
INTEGER, PARAMETER :: BD_ChannelAMSB        =  25049
INTEGER, PARAMETER :: BD_SoftwareIdentifier =  25060
INTEGER, PARAMETER :: BD_ScatCoef           =  25206
INTEGER, PARAMETER :: BD_Niter              =  25213
INTEGER, PARAMETER :: BD_GrodyP1            =  25216
INTEGER, PARAMETER :: BD_GrodyP2            =  25217
INTEGER, PARAMETER :: BD_GrodyP3            =  25218
INTEGER, PARAMETER :: BD_GrodyP4            =  25219
INTEGER, PARAMETER :: BD_ObTimeDiff         =  26003
INTEGER, PARAMETER :: BD_Levels             =  31001
INTEGER, PARAMETER :: BD_OBErrRat           =  33193
INTEGER, PARAMETER :: BD_ErrRat             =  33193
INTEGER, PARAMETER :: BD_PGE                =  33202
INTEGER, PARAMETER :: BD_ReportPGE          =  33202
INTEGER, PARAMETER :: BD_ModVNo             =  55002
INTEGER, PARAMETER :: BD_ModelVersion       =  55002
INTEGER, PARAMETER :: BD_VnATOVS            =  55007
INTEGER, PARAMETER :: BD_OPSVersion         =  55007
INTEGER, PARAMETER :: BD_ExpId              =  55008
INTEGER, PARAMETER :: BD_ModelId            =  55009
INTEGER, PARAMETER :: BD_ReportFlags        =  55016
INTEGER, PARAMETER :: BD_ModelObsType       =  55017
INTEGER, PARAMETER :: BD_QCflags1           =  55018
INTEGER, PARAMETER :: BD_QCflags2           =  55019
INTEGER, PARAMETER :: BD_QCflags3           =  55020
INTEGER, PARAMETER :: BD_QCflags            =  55021
INTEGER, PARAMETER :: BD_QCflagW2           =  55022
INTEGER, PARAMETER :: BD_Version            =  55027
INTEGER, PARAMETER :: BD_EndChangeWidth     = 201000
INTEGER, PARAMETER :: BD_EndChangeScale     = 202000
INTEGER, PARAMETER :: BD_FXXYYY             = 999999
INTEGER, PARAMETER :: BD_MwEmi_p            = 999999
INTEGER, PARAMETER :: BD_VertCoor           = 999999

END MODULE OpsMod_BufrDescriptors
