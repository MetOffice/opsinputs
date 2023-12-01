!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Module to store satellite identifiers.
!-------------------------------------------------------------------------------

MODULE OpsMod_SatIds

IMPLICIT NONE

SAVE

! Public declarations:

!-------------------------------------------------------------------------------
! 1. Define constants
!-------------------------------------------------------------------------------

INTEGER, PARAMETER  :: SatId_ERS1               = 1
INTEGER, PARAMETER  :: SatId_ERS2               = 2
INTEGER, PARAMETER  :: SatId_MetopB             = 3
INTEGER, PARAMETER  :: SatId_MetopA             = 4
INTEGER, PARAMETER  :: SatId_MetopC             = 5
INTEGER, PARAMETER  :: SatId_TerraSARX          = 42
INTEGER, PARAMETER  :: SatId_TandemX            = 43
INTEGER, PARAMETER  :: SatId_PAZ                = 44
INTEGER, PARAMETER  :: SatId_SMOS               = 46
INTEGER, PARAMETER  :: SatId_Cryosat2           = 47
INTEGER, PARAMETER  :: SatId_Aeolus             = 48
INTEGER, PARAMETER  :: SatId_Meteosat7          = 54
INTEGER, PARAMETER  :: SatId_Meteosat8          = 55
INTEGER, PARAMETER  :: SatId_Meteosat9          = 56
INTEGER, PARAMETER  :: SatId_Meteosat10         = 57
INTEGER, PARAMETER  :: SatId_Envisat            = 60
INTEGER, PARAMETER  :: SatId_Sentinel3A         = 61
INTEGER, PARAMETER  :: SatId_Sentinel3B         = 65
INTEGER, PARAMETER  :: SatId_Meteosat11         = 70
INTEGER, PARAMETER  :: SatId_MSG3               = 73
INTEGER, PARAMETER  :: SatId_GCOMW1             = 122
INTEGER, PARAMETER  :: SatId_MTSAT1R            = 171
INTEGER, PARAMETER  :: SatId_MTSAT2             = 172
INTEGER, PARAMETER  :: SatId_Himawari8          = 173
INTEGER, PARAMETER  :: SatId_Himawari9          = 174
INTEGER, PARAMETER  :: SatId_NOAA8              = 200
INTEGER, PARAMETER  :: SatId_NOAA9              = 201
INTEGER, PARAMETER  :: SatId_NOAA10             = 202
INTEGER, PARAMETER  :: SatId_NOAA11             = 203
INTEGER, PARAMETER  :: SatId_NOAA12             = 204
INTEGER, PARAMETER  :: SatId_NOAA14             = 205
INTEGER, PARAMETER  :: SatId_NOAA15             = 206
INTEGER, PARAMETER  :: SatId_NOAA16             = 207
INTEGER, PARAMETER  :: SatId_NOAA17             = 208
INTEGER, PARAMETER  :: SatId_NOAA18             = 209
INTEGER, PARAMETER  :: SatId_Landsat4           = 221
INTEGER, PARAMETER  :: SatId_Landsat7           = 222
INTEGER, PARAMETER  :: SatId_NOAA19             = 223
INTEGER, PARAMETER  :: SatId_SuomiNPP           = 224
INTEGER, PARAMETER  :: SatId_NOAA20             = 225
INTEGER, PARAMETER  :: SatId_NOAA21             = 226
INTEGER, PARAMETER  :: SatId_DMSPF11            = 244
INTEGER, PARAMETER  :: SatId_DMSPF12            = 245
INTEGER, PARAMETER  :: SatId_DMSPF13            = 246
INTEGER, PARAMETER  :: SatId_DMSPF14            = 247
INTEGER, PARAMETER  :: SatId_DMSPF15            = 248
INTEGER, PARAMETER  :: SatId_DMSPF16            = 249
INTEGER, PARAMETER  :: SatId_GOES11             = 255
INTEGER, PARAMETER  :: SatId_GOES12             = 256
INTEGER, PARAMETER  :: SatId_GOES13             = 257
INTEGER, PARAMETER  :: SatId_GOES14             = 258
INTEGER, PARAMETER  :: SatId_GOES15             = 259
INTEGER, PARAMETER  :: SatId_JASON1             = 260
INTEGER, PARAMETER  :: SatId_JASON2             = 261
INTEGER, PARAMETER  :: SatId_JASON3             = 262
INTEGER, PARAMETER  :: SatId_Spire              = 269
INTEGER, PARAMETER  :: SatId_GOES16             = 270
INTEGER, PARAMETER  :: SatId_GOES17             = 271
INTEGER, PARAMETER  :: SatId_GOES18             = 272
INTEGER, PARAMETER  :: SatId_GOES19             = 273
INTEGER, PARAMETER  :: SatId_TRMM               = 282
INTEGER, PARAMETER  :: SatId_Coriolis           = 283
INTEGER, PARAMETER  :: SatId_DMSPF17            = 285
INTEGER, PARAMETER  :: SatId_DMSPF18            = 286
INTEGER, PARAMETER  :: SatId_DMSPF19            = 287
INTEGER, PARAMETER  :: SatId_GPM                = 288
INTEGER, PARAMETER  :: SatId_Kalpana1           = 410
INTEGER, PARAMETER  :: SatId_OceanSat2          = 421
INTEGER, PARAMETER  :: SatId_ScatSat1           = 422
INTEGER, PARAMETER  :: SatId_OceanSat3          = 423
INTEGER, PARAMETER  :: SatId_OceanSat3A         = 424
INTEGER, PARAMETER  :: SatId_MT                 = 440
INTEGER, PARAMETER  :: SatId_SARAL              = 441
INTEGER, PARAMETER  :: SatId_INSAT3A            = 470
INTEGER, PARAMETER  :: SatId_INSAT3D            = 471
INTEGER, PARAMETER  :: SatId_INSAT3E            = 472
INTEGER, PARAMETER  :: SatId_INSAT3DR           = 473
INTEGER, PARAMETER  :: SatId_INSAT3DS           = 474
INTEGER, PARAMETER  :: SatId_HY2B               = 503
INTEGER, PARAMETER  :: SatId_HY2C               = 504
INTEGER, PARAMETER  :: SatId_FY2C               = 513
INTEGER, PARAMETER  :: SatId_FY2D               = 514
INTEGER, PARAMETER  :: SatId_FY2E               = 515
INTEGER, PARAMETER  :: SatId_FY2F               = 516
INTEGER, PARAMETER  :: SatId_FY2G               = 517
INTEGER, PARAMETER  :: SatId_FY2H               = 518
INTEGER, PARAMETER  :: SatId_FY3B               = 521
INTEGER, PARAMETER  :: SatId_FY3C               = 522
INTEGER, PARAMETER  :: SatId_FY3D               = 523
INTEGER, PARAMETER  :: SatId_FY3E               = 524
INTEGER, PARAMETER  :: SatId_FY4A               = 530
INTEGER, PARAMETER  :: SatId_FY4B               = 531
INTEGER, PARAMETER  :: SatId_NOAA6              = 706
INTEGER, PARAMETER  :: SatId_NOAA7              = 707
INTEGER, PARAMETER  :: SatId_TIROSN             = 708
INTEGER, PARAMETER  :: SatId_GraceA             = 722
INTEGER, PARAMETER  :: SatId_GraceB             = 723
INTEGER, PARAMETER  :: SatId_Cosmic1            = 740
INTEGER, PARAMETER  :: SatId_Cosmic2            = 741
INTEGER, PARAMETER  :: SatId_Cosmic3            = 742
INTEGER, PARAMETER  :: SatId_Cosmic4            = 743
INTEGER, PARAMETER  :: SatId_Cosmic5            = 744
INTEGER, PARAMETER  :: SatId_Cosmic6            = 745
INTEGER, PARAMETER  :: SatId_Cosmic2_E1         = 750
INTEGER, PARAMETER  :: SatId_Cosmic2_E2         = 751
INTEGER, PARAMETER  :: SatId_Cosmic2_E3         = 752
INTEGER, PARAMETER  :: SatId_Cosmic2_E4         = 753
INTEGER, PARAMETER  :: SatId_Cosmic2_E5         = 754
INTEGER, PARAMETER  :: SatId_Cosmic2_E6         = 755
INTEGER, PARAMETER  :: SatId_Terra              = 783
INTEGER, PARAMETER  :: SatId_Aqua               = 784
INTEGER, PARAMETER  :: SatId_ChampNOFS          = 786
INTEGER, PARAMETER  :: SatId_ISS                = 801
INTEGER, PARAMETER  :: SatId_CFOSAT             = 802
INTEGER, PARAMETER  :: SatId_COMS1              = 810
INTEGER, PARAMETER  :: SatId_GeoKompsat2A       = 811
INTEGER, PARAMETER  :: SatId_KOMPSAT5           = 825
INTEGER, PARAMETER  :: SatId_MixedMetop         = 852
INTEGER, PARAMETER  :: SatId_LeoGeo             = 854
INTEGER, PARAMETER  :: SatId_Cryosat2_metdb_tmp = 1005

END MODULE OpsMod_SatIds
