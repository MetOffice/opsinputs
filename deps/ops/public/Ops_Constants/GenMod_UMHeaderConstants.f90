!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! UM header stuff from the UM.
!-------------------------------------------------------------------------------
MODULE GenMod_UMHeaderConstants

IMPLICIT NONE

! Fixed length header constants

INTEGER,PARAMETER :: FH_Version      = 1
INTEGER,PARAMETER :: FH_SubModel     = 2
INTEGER,PARAMETER :: FH_VertCoord    = 3
INTEGER,PARAMETER :: FH_HorizGrid    = 4
INTEGER,PARAMETER :: FH_Dataset      = 5
INTEGER,PARAMETER :: FH_RunId        = 6
INTEGER,PARAMETER :: FH_ExptNo       = 7
INTEGER,PARAMETER :: FH_CalendarType = 8
INTEGER,PARAMETER :: FH_GridStagger  = 9
INTEGER,PARAMETER :: FH_AncilDataId  = 10
INTEGER,PARAMETER :: FH_ProjNo       = 11
INTEGER,PARAMETER :: FH_ModelVersion = 12
INTEGER,PARAMETER :: FH_ObsFileType = 14

INTEGER,PARAMETER :: FH_DTYear    = 21
INTEGER,PARAMETER :: FH_DTMonth   = 22
INTEGER,PARAMETER :: FH_DTDay     = 23
INTEGER,PARAMETER :: FH_DTHour    = 24
INTEGER,PARAMETER :: FH_DTMinute  = 25
INTEGER,PARAMETER :: FH_DTSecond  = 26
INTEGER,PARAMETER :: FH_DTDayNo   = 27

INTEGER,PARAMETER :: FH_VTYear    = 28
INTEGER,PARAMETER :: FH_VTMonth   = 29
INTEGER,PARAMETER :: FH_VTDay     = 30
INTEGER,PARAMETER :: FH_VTHour    = 31
INTEGER,PARAMETER :: FH_VTMinute  = 32
INTEGER,PARAMETER :: FH_VTSecond  = 33
INTEGER,PARAMETER :: FH_VTDayNo   = 34

INTEGER,PARAMETER :: FH_CTYear    = 35
INTEGER,PARAMETER :: FH_CTMonth   = 36
INTEGER,PARAMETER :: FH_CTDay     = 37
INTEGER,PARAMETER :: FH_CTHour    = 38
INTEGER,PARAMETER :: FH_CTMinute  = 39
INTEGER,PARAMETER :: FH_CTSecond  = 40
INTEGER,PARAMETER :: FH_CTDayNo   = 41

INTEGER,PARAMETER :: FH_IntCStart      = 100
INTEGER,PARAMETER :: FH_IntCSize       = 101
INTEGER,PARAMETER :: FH_RealCStart     = 105
INTEGER,PARAMETER :: FH_RealCSize      = 106
INTEGER,PARAMETER :: FH_LevDepCStart   = 110
INTEGER,PARAMETER :: FH_LevDepCSize1   = 111
INTEGER,PARAMETER :: FH_LevDepCSize2   = 112
INTEGER,PARAMETER :: FH_RowDepCStart   = 115
INTEGER,PARAMETER :: FH_RowDepCSize1   = 116
INTEGER,PARAMETER :: FH_RowDepCSize2   = 117
INTEGER,PARAMETER :: FH_ColDepCStart   = 120
INTEGER,PARAMETER :: FH_ColDepCSize1   = 121
INTEGER,PARAMETER :: FH_ColDepCSize2   = 122
INTEGER,PARAMETER :: FH_FldsOfCStart   = 125
INTEGER,PARAMETER :: FH_FldsOfCSize1   = 126
INTEGER,PARAMETER :: FH_FldsOfCSize2   = 127
INTEGER,PARAMETER :: FH_ExtraCStart    = 130
INTEGER,PARAMETER :: FH_ExtraCSize     = 131
INTEGER,PARAMETER :: FH_HistStart      = 135
INTEGER,PARAMETER :: FH_HistSize       = 136
INTEGER,PARAMETER :: FH_CompFldI1Start = 140
INTEGER,PARAMETER :: FH_CompFldI1Size  = 141
INTEGER,PARAMETER :: FH_CompFldI2Start = 142
INTEGER,PARAMETER :: FH_CompFldI2Size  = 143
INTEGER,PARAMETER :: FH_CompFldI3Start = 144
INTEGER,PARAMETER :: FH_CompFldI3Size  = 145
INTEGER,PARAMETER :: FH_LookupStart    = 150
INTEGER,PARAMETER :: FH_LookupSize1    = 151
INTEGER,PARAMETER :: FH_LookupSize2    = 152
INTEGER,PARAMETER :: FH_NumProgFields  = 153
INTEGER,PARAMETER :: FH_DataStart      = 160
INTEGER,PARAMETER :: FH_DataSize       = 161
INTEGER,PARAMETER :: FH_MaxDataSize    = 162

INTEGER,PARAMETER :: FH_SubModel_Atmos = 1
INTEGER,PARAMETER :: FH_SubModel_Ocean = 2
INTEGER,PARAMETER :: FH_SubModel_Wave  = 4

INTEGER,PARAMETER :: FH_VertCoord_Hybrid   = 1
INTEGER,PARAMETER :: FH_VertCoord_Sigma    = 2
INTEGER,PARAMETER :: FH_VertCoord_Pressure = 3
INTEGER,PARAMETER :: FH_VertCoord_Depth    = 4
INTEGER,PARAMETER :: FH_VertCoord_CP       = 5
INTEGER,PARAMETER :: FH_VertCoord_Wave     = 6

INTEGER,PARAMETER :: FH_HorizGrid_Global      = 0
INTEGER,PARAMETER :: FH_HorizGrid_NH          = 1
INTEGER,PARAMETER :: FH_HorizGrid_SH          = 2
INTEGER,PARAMETER :: FH_HorizGrid_LamNoWrap   = 3
INTEGER,PARAMETER :: FH_HorizGrid_LamWrap     = 4
INTEGER,PARAMETER :: FH_HorizGrid_Eq          = 100
INTEGER,PARAMETER :: FH_HorizGrid_LamNoWrapEq = 103
INTEGER,PARAMETER :: FH_HorizGrid_LamWrapEq   = 104

INTEGER,PARAMETER :: FH_GridStagger_ArakawaB = 2
INTEGER,PARAMETER :: FH_GridStagger_ArakawaC = 3
INTEGER,PARAMETER :: FH_GridStagger_EndGame  = 6

INTEGER,PARAMETER :: FH_Dataset_InstDump = 1
INTEGER,PARAMETER :: FH_Dataset_MeanDump = 2
INTEGER,PARAMETER :: FH_Dataset_FF       = 3
INTEGER,PARAMETER :: FH_Dataset_Ancil    = 4
INTEGER,PARAMETER :: FH_Dataset_Boundary = 5
INTEGER,PARAMETER :: FH_Dataset_ACOBS    = 6
INTEGER,PARAMETER :: FH_Dataset_VAROBS   = 7
INTEGER,PARAMETER :: FH_Dataset_CX       = 8
INTEGER,PARAMETER :: FH_Dataset_COV      = 9
INTEGER,PARAMETER :: FH_Dataset_OBSTORE  = 10

INTEGER,PARAMETER :: FH_ObsFileType_Atmos = 1
INTEGER,PARAMETER :: FH_ObsFileType_Ocean = 2
INTEGER,PARAMETER :: FH_ObsFileType_SST   = 3
INTEGER,PARAMETER :: FH_ObsFileType_Wave  = 4

INTEGER, PARAMETER :: IC_TorTheta        = 1 !location in header
INTEGER, PARAMETER :: IC_TorTheta_T      = 1 !value of above if T
INTEGER, PARAMETER :: IC_TorTheta_Theta  = 2 !value of above if Theta
INTEGER, PARAMETER :: IC_ShipWind        = 2 !location in header
INTEGER, PARAMETER :: IC_ShipWind_10m    = 1 !value if all winds at 10m
INTEGER, PARAMETER :: IC_NoInstDumps     = 3
INTEGER, PARAMETER :: IC_GroundGPSOperator = 3 !location in header
INTEGER, PARAMETER :: IC_GroundGPSOperatorChoice = 1 ! value if new operator chosen
INTEGER, PARAMETER :: IC_GroundGPSOperatorGeneric = 2 ! value if newer generic refrac operator chosen
INTEGER, PARAMETER :: IC_GPSRO_Operator_pseudo = 4        !location in header
INTEGER, PARAMETER :: IC_GPSRO_Operator_pseudo_choice = 1 ! value if new operator chosen
INTEGER, PARAMETER :: IC_GPSRO_Operator_press = 5         !location in header
INTEGER, PARAMETER :: IC_GPSRO_Operator_press_Choice = 1  ! value if new operator chosen
INTEGER, PARAMETER :: IC_XLen          = 6
INTEGER, PARAMETER :: IC_YLen          = 7
INTEGER, PARAMETER :: IC_PLevels       = 8
INTEGER, PARAMETER :: IC_WetLevels     = 9
INTEGER, PARAMETER :: IC_NoSoilLevels  = 10
INTEGER, PARAMETER :: IC_NoCloudLevels = 11 ! ATMOS only
INTEGER, PARAMETER :: IC_NoSeaPts      = 11 ! OCEAN only
INTEGER, PARAMETER :: IC_NoTracerLevels = 12
INTEGER, PARAMETER :: IC_BLevels       = 13
INTEGER, PARAMETER :: IC_TimeWindowStart = 14 ! Obstores only
INTEGER, PARAMETER :: IC_TimeWindowEnd = 15   ! Obstores only
INTEGER, PARAMETER :: IC_NoPassiveTracers = 14
INTEGER, PARAMETER :: IC_NoFieldTypes  = 15
INTEGER, PARAMETER :: IC_MDI           = 21
INTEGER, PARAMETER :: IC_FirstConstantRhoLevel = 24
INTEGER, PARAMETER :: IC_NumLandPoints = 25
INTEGER, PARAMETER :: IC_NumOzoneLevs  = 26
INTEGER, PARAMETER :: IC_NumObsTotal   = 28
INTEGER, PARAMETER :: IC_LenObCol      = 29
INTEGER, PARAMETER :: IC_LenCxCol      = 30 ! Varobs, not acobs
INTEGER, PARAMETER :: IC_ObsGroup      = 31 ! "
INTEGER, PARAMETER :: IC_ObsRelease    = 32 ! "
INTEGER, PARAMETER :: IC_NumMetaMax    = 33 ! "
INTEGER, PARAMETER :: IC_NumItemMax    = 34 ! "
INTEGER, PARAMETER :: IC_NumObVarMax   = 35
INTEGER, PARAMETER :: IC_NumObPItemMax = 36
INTEGER, PARAMETER :: IC_NumCxPItemMax = 37
INTEGER, PARAMETER :: IC_NumCxSFVarMax = 38
INTEGER, PARAMETER :: IC_NumCxUaVarMax = 39
INTEGER, PARAMETER :: IC_NumMeta       = 40
INTEGER, PARAMETER :: IC_NumItem       = 41
INTEGER, PARAMETER :: IC_NumObVar      = 42
INTEGER, PARAMETER :: IC_NumObPItem    = 43
INTEGER, PARAMETER :: IC_NumCxPItem    = 44
INTEGER, PARAMETER :: IC_NumCxSfVar    = 45
INTEGER, PARAMETER :: IC_NumCxUaVar    = 46
INTEGER, PARAMETER :: IC_NumObLev      = 47
INTEGER, PARAMETER :: IC_NumCxLev      = 48
INTEGER, PARAMETER :: IC_NumVarBatches = 49

! Acobs files only:
INTEGER, PARAMETER :: IC_TimeWindowBefore = 30
INTEGER, PARAMETER :: IC_TimeWindowAfter  = 31
INTEGER, PARAMETER :: IC_NumACTypes       = 32
INTEGER, PARAMETER :: IC_NumOceanTypes    = 33

INTEGER, PARAMETER :: RC_LongSpacing = 1
INTEGER, PARAMETER :: RC_LatSpacing  = 2
INTEGER, PARAMETER :: RC_FirstLat    = 3
INTEGER, PARAMETER :: RC_FirstLong   = 4
INTEGER, PARAMETER :: RC_PoleLat     = 5
INTEGER, PARAMETER :: RC_PoleLong    = 6
INTEGER, PARAMETER :: RC_z_ModelTop  = 16

INTEGER, PARAMETER :: CC_Meta_Latitude  = 1 ! Used in varobs
INTEGER, PARAMETER :: CC_Meta_Longitude = 2 !      "
INTEGER, PARAMETER :: CC_Meta_Time      = 3 !      "
INTEGER, PARAMETER :: CC_Meta_Type      = 4 !      "
INTEGER, PARAMETER :: CC_Meta_Call      = 5 !      "
INTEGER, PARAMETER :: CC_Meta_Level     = 6 !      "
INTEGER, PARAMETER :: CC_Meta_RepPGE    = 7 !      "

INTEGER, PARAMETER :: CC_Item_Value = 1 ! Used in varobs
INTEGER, PARAMETER :: CC_Item_Error = 2 !      "
INTEGER, PARAMETER :: CC_Item_PGE   = 3 !      "

END MODULE GenMod_UMHeaderConstants
