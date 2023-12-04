!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! This module currently contains CX indexes.
!-------------------------------------------------------------------------------

MODULE OpsMod_CXIndexes

IMPLICIT NONE

SAVE

! Public declarations:

! for cx (surface)
INTEGER, PARAMETER :: IndexCxorog               = 1
INTEGER, PARAMETER :: IndexCxpstar              = 2
INTEGER, PARAMETER :: IndexCxt2                 = 3
INTEGER, PARAMETER :: IndexCxrh2                = 4
INTEGER, PARAMETER :: IndexCxu10                = 5
INTEGER, PARAMETER :: IndexCxv10                = 6
INTEGER, PARAMETER :: IndexCxModelSurface       = 7
INTEGER, PARAMETER :: IndexCxvis                = 8
INTEGER, PARAMETER :: IndexCxWaveHeight         = 9
INTEGER, PARAMETER :: IndexCxSeaHeight          = 10
INTEGER, PARAMETER :: IndexCxSST                = 11
INTEGER, PARAMETER :: IndexCxTskinSea           = 13
INTEGER, PARAMETER :: IndexCxTropPres           = 14
INTEGER, PARAMETER :: IndexCxpmsl               = 16
INTEGER, PARAMETER :: IndexCxSeaIce             = 17
INTEGER, PARAMETER :: IndexCxWAVE_HGHT          = 18
INTEGER, PARAMETER :: IndexCxWIND_SPED          = 19
INTEGER, PARAMETER :: IndexCxqt2                = 20
INTEGER, PARAMETER :: IndexCxaerosol            = 21
INTEGER, PARAMETER :: IndexCxPSurfParamA        = 22
INTEGER, PARAMETER :: IndexCxPSurfParamB        = 23
INTEGER, PARAMETER :: IndexCxCloudAmount        = 24
INTEGER, PARAMETER :: IndexCxConvCloudAmount    = 25
INTEGER, PARAMETER :: IndexCxConvCloudBaseLevel = 26
INTEGER, PARAMETER :: IndexCxConvCloudTopLevel  = 27
INTEGER, PARAMETER :: IndexCxSurfRainRate_conv  = 28
INTEGER, PARAMETER :: IndexCxSurfSnowRate_conv  = 29
INTEGER, PARAMETER :: IndexCxSeaSrfcHeight      = 30
INTEGER, PARAMETER :: IndexCxMeanSeaHeight      = 31
INTEGER, PARAMETER :: IndexCxSnowAmount         = 42
INTEGER, PARAMETER :: IndexCxSurfRainRate_LS    = 43
INTEGER, PARAMETER :: IndexCxSurfSnowRate_LS    = 44
INTEGER, PARAMETER :: IndexCxSWradiation        = 45
INTEGER, PARAMETER :: IndexCxBLheight           = 46
INTEGER, PARAMETER :: IndexCxPrecipAcc6hr       = 47
INTEGER, PARAMETER :: IndexCxLowCloudAmount     = 48
INTEGER, PARAMETER :: IndexCxMedCloudAmount     = 49
INTEGER, PARAMETER :: IndexCxLowCloudBase       = 50
INTEGER, PARAMETER :: IndexCxObukhovLength      = 56
INTEGER, PARAMETER :: IndexCxFrictionVel        = 57
INTEGER, PARAMETER :: IndexCxLapseRate          = 58
INTEGER, PARAMETER :: IndexCxSO2_AQ             = 59
INTEGER, PARAMETER :: IndexCxPM10_AQ            = 60
INTEGER, PARAMETER :: IndexCxPM2p5_AQ           = 61
INTEGER, PARAMETER :: IndexCxO3_AQ              = 62
INTEGER, PARAMETER :: IndexCxNO2_AQ             = 63
INTEGER, PARAMETER :: IndexCxCO_AQ              = 64
INTEGER, PARAMETER :: IndexCxBLtype             = 65
INTEGER, PARAMETER :: IndexCXSFSize             = 65

! for cx (upper level)
INTEGER, PARAMETER :: IndexCxtheta              = 1
INTEGER, PARAMETER :: IndexCxrh                 = 2
INTEGER, PARAMETER :: IndexCxu                  = 3
INTEGER, PARAMETER :: IndexCxv                  = 4
INTEGER, PARAMETER :: IndexCxq                  = 5
INTEGER, PARAMETER :: IndexCxqc                 = 6
INTEGER, PARAMETER :: IndexCxcloud              = 7
INTEGER, PARAMETER :: IndexCxSalt               = 8
INTEGER, PARAMETER :: IndexCxt                  = 9
INTEGER, PARAMETER :: IndexCxql_layer           = 10
INTEGER, PARAMETER :: IndexCxp                  = 11
INTEGER, PARAMETER :: IndexCxozone              = 12
INTEGER, PARAMETER :: IndexCxqf_layer           = 13
INTEGER, PARAMETER :: IndexCxRainRate_layer     = 14
INTEGER, PARAMETER :: IndexCxcloud_layer        = 15
INTEGER, PARAMETER :: IndexCxcloud_conv         = 16
INTEGER, PARAMETER :: IndexCxqc_conv            = 17
INTEGER, PARAMETER :: IndexCxrhtotal            = 18
INTEGER, PARAMETER :: IndexCxqcf                = 29
INTEGER, PARAMETER :: IndexCxqcl                = 30
INTEGER, PARAMETER :: IndexCxcloud_bulk         = 31
INTEGER, PARAMETER :: IndexCxw                  = 32
INTEGER, PARAMETER :: IndexCxp_bar              = 33
INTEGER, PARAMETER :: IndexCxCf                 = 34
INTEGER, PARAMETER :: IndexCxCl                 = 35
INTEGER, PARAMETER :: IndexCxRichNumber         = 36
INTEGER, PARAMETER :: IndexCxSoilMoisture       = 37
INTEGER, PARAMETER :: IndexCxSoilTemp           = 38
INTEGER, PARAMETER :: IndexCxExnerA             = 39
INTEGER, PARAMETER :: IndexCxqrain              = 40
INTEGER, PARAMETER :: IndexCxDust1              = 41
INTEGER, PARAMETER :: IndexCxDust2              = 42
INTEGER, PARAMETER :: IndexCxDust3              = 43
INTEGER, PARAMETER :: IndexCxDust4              = 44
INTEGER, PARAMETER :: IndexCxDust5              = 45
INTEGER, PARAMETER :: IndexCxDust6              = 46
INTEGER, PARAMETER :: IndexCxAOD                = 47
INTEGER, PARAMETER :: IndexCxaerosol_p          = 48
INTEGER, PARAMETER :: IndexCxCDNC               = 49
INTEGER, PARAMETER :: IndexCxRH_AMC             = 50
INTEGER, PARAMETER :: IndexCXUASize             = 50

END MODULE OpsMod_CXIndexes
