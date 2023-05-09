!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Count the number of surface and upper air CX indexes to be written to a CX
! file, based on presence, or lack therof, of elements in the CX header.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_MaxCXIndexes (CX,              &
                             MaxSurfaceIndex, &
                             MaxUairIndex)

USE GenMod_Control, ONLY: &
    DebugMode,            &
    GeneralMode

USE GenMod_Core, ONLY: &
    gen_message,       &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    IMDI

USE OpsMod_AODGeneral, ONLY: &
    NDustBins

USE OpsMod_CXIndexes

USE OpsMod_CXInfo, ONLY: &
    Cx_type

USE OpsMod_MiscTypes, ONLY: &
    ElementHeader_type

USE OpsMod_ObsGroupInfo, ONLY: &
    ObsGroupGroundLidar,       &
    ObsGroupSurface

IMPLICIT NONE

! Subroutine arguments:
TYPE (Cx_type), INTENT (IN) :: Cx
INTEGER, INTENT(OUT)        :: MaxSurfaceIndex
INTEGER, INTENT(OUT)        :: MaxUairIndex

! Local declarations:
INTEGER                     :: Ivar
TYPE (ElementHeader_type)   :: CxHdrVrbl
CHARACTER(len=200)          :: message
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_MaxCXIndexes"
INTEGER                     :: IndexCxDustMin   ! Min Dust Cx Index
INTEGER                     :: IndexCxDustMax   ! Max Dust Cx Index

IF (UseTrace) CALL gen_trace_entry (RoutineName)

MaxSurfaceIndex = IMDI
MaxUairIndex = IMDI

DO Ivar = 1, IndexCXSFSize

  SELECT CASE (Ivar)
    CASE (IndexCxorog)
      CxHdrVrbl = Cx % Header % Orog
    CASE (IndexCxpstar)
      CxHdrVrbl = Cx % Header % pstar
    CASE (IndexCxt2)
      CxHdrVrbl = Cx % Header % t2
    CASE (IndexCxrh2)
      CxHdrVrbl = Cx % Header % rh2
    CASE (IndexCxu10)
      CxHdrVrbl = Cx % Header % u10
    CASE (IndexCxv10)
      CxHdrVrbl = Cx % Header % v10
    CASE (IndexCxvis)
      CxHdrVrbl = Cx % Header % vis
    CASE (IndexCxWaveHeight)
      CxHdrVrbl = Cx % Header % WaveHeight
    CASE (IndexCxWAVE_HGHT)
      CxHdrVrbl = Cx % Header % WAVE_HGHT
    CASE (IndexCxWIND_SPED)
      CxHdrVrbl = Cx % Header % WIND_SPED
    CASE (IndexCxSeaHeight)
      CxHdrVrbl = Cx % Header % SeaHeight
    CASE (IndexCxSST)
      CxHdrVrbl = Cx % Header % SST
    CASE (IndexCxTskinSea)
      CxHdrVrbl = Cx % Header % TskinSea
    CASE (IndexCxTropPres)
      CxHdrVrbl = Cx % Header % TropPres
    CASE (IndexCxpmsl)
      CxHdrVrbl = Cx % Header % pmsl
    CASE (IndexCxSeaIce)
      CxHdrVrbl = Cx % Header % SeaIce
    CASE (IndexCxSnowAmount)
      CxHdrVrbl = Cx % Header % SnowAmount
    CASE (IndexCxqt2)
      CxHdrVrbl = Cx % Header % qt2
    CASE (IndexCxaerosol)
      IF (Cx % Header % ObsGroup == ObsGroupSurface) THEN
        CxHdrVrbl = Cx % Header % aerosol
      END IF
    CASE (IndexCxPsurfParamA)
      CxHdrVrbl = Cx % Header % PSurfParamA
    CASE (IndexCxPSurfParamB)
      CxHdrVrbl = Cx % Header % PSurfParamB
    CASE (IndexCxLapseRate)
      CxHdrVrbl = Cx % Header % LapseRate
    CASE (IndexCxCloudAmount)
      CxHdrVrbl = Cx % Header % CloudAmount
    CASE (IndexCxConvCloudAmount)
      CxHdrVrbl = Cx % Header % ConvCloudAmount
    CASE (IndexCxConvCloudBaseLevel)
      CxHdrVrbl = Cx % Header % ConvCloudBaseLevel
    CASE (IndexCxConvCloudTopLevel)
      CxHdrVrbl = Cx % Header % ConvCloudTopLevel
    CASE (IndexCxSurfRainRate_conv)
      CxHdrVrbl = Cx % Header % SurfRainRate_conv
    CASE (IndexCxSurfSnowRate_conv)
      CxHdrVrbl = Cx % Header % SurfSnowRate_conv
    CASE (IndexCxSeaSrfcHeight)
      CxHdrVrbl = Cx % Header % SeaSrfcHeight
    CASE (IndexCxMeanSeaHeight)
      CxHdrVrbl = Cx % Header % MeanSeaHeight
    CASE (IndexCxSurfRainRate_LS)
      CxHdrVrbl = Cx % Header % SurfRainRate_LS
    CASE (IndexCxSurfSnowRate_LS)
      CxHdrVrbl = Cx % Header % SurfSnowRate_LS
    CASE (IndexCxSWradiation)
      CxHdrVrbl = Cx % Header % SWradiation
    CASE (IndexCxBLheight)
      CxHdrVrbl = Cx % Header % BLheight
    CASE (IndexCxObukhovLength)
      CxHdrVrbl = Cx % Header % ObukhovLength
    CASE (IndexCxFrictionVel)
      CxHdrVrbl = Cx % Header % FrictionVel
    CASE (IndexCxPrecipAcc6hr)
      CxHdrVrbl = Cx % Header % PrecipAcc6hr
    CASE (IndexCxLowCloudAmount)
      CxHdrVrbl = Cx % Header % LowCloudAmount
    CASE (IndexCxMedCloudAmount)
      CxHdrVrbl = Cx % Header % MedCloudAmount
    CASE (IndexCxLowCloudBase)
      CxHdrVrbl = Cx % Header % LowCloudBase
    CASE (IndexCxSO2_AQ)
      CxHdrVrbl = Cx % Header % SO2_AQ
    CASE (IndexCxPM10_AQ)
      CxHdrVrbl = Cx % Header % PM10_AQ
    CASE (IndexCxPM2p5_AQ)
      CxHdrVrbl = Cx % Header % PM2p5_AQ
    CASE (IndexCxO3_AQ)
      CxHdrVrbl = Cx % Header % O3_AQ
    CASE (IndexCxNO2_AQ)
      CxHdrVrbl = Cx % Header % NO2_AQ
    CASE (IndexCxCO_AQ)
      CxHdrVrbl = Cx % Header % CO_AQ
    CASE (IndexCxBLtype)
      CxHdrVrbl = Cx % Header % BLtype
    CASE DEFAULT
      CYCLE
  END SELECT

  IF (CxHdrVrbl % Present) THEN
    MaxSurfaceIndex = IVar
  END IF

END DO

IF (GeneralMode >= DebugMode) THEN
  WRITE (message, '(A,I0)') "Max surface index = ", MaxSurfaceIndex
  CALL gen_message (RoutineName, &
                    message)
END IF

! Dynamically adjust DustIndex Min and Max values

IndexCxDustMin = IndexCxDust1
IndexCxDustMax = IndexCxDustmin + NDustBins - 1

DO Ivar = 1, IndexCXUASize

  SELECT CASE (Ivar)
    CASE (IndexCxtheta)
      CxHdrVrbl = Cx % Header % theta
    CASE (IndexCxrh)
      CxHdrVrbl = Cx % Header % rh
    CASE (IndexCxu)
      CxHdrVrbl = Cx % Header % u
    CASE (IndexCxv)
      CxHdrVrbl = Cx % Header % v
    CASE (IndexCxw)
      CxHdrVrbl = Cx % Header % w
    CASE (IndexCxq)
      CxHdrVrbl = Cx % Header % q
    CASE (IndexCxqc)
      CxHdrVrbl = Cx % Header % qc
    CASE (IndexCxp_bar)
      CxHdrVrbl = Cx % Header % p_bar
    CASE (IndexCxcloud)
      CxHdrVrbl = Cx % Header % cloud
    CASE (IndexCxql_layer)
      CxHdrVrbl = Cx % Header % ql_layer
    CASE (IndexCxP)
      CxHdrVrbl = Cx % Header % PlevelsA
    CASE (IndexCxsalt)
      CxHdrVrbl = Cx % Header % salt
    CASE (IndexCxt)
      CxHdrVrbl = Cx % Header % t
    CASE (IndexCxqf_layer)
      CxHdrVrbl = Cx % Header % qf_layer
    CASE (IndexCxRainRate_layer)
      CxHdrVrbl = Cx % Header % RainRate_layer
    CASE (IndexCxcloud_layer)
      CxHdrVrbl = Cx % Header % cloud_layer
    CASE (IndexCxcloud_conv)
      CxHdrVrbl = Cx % Header % cloud_conv
    CASE (IndexCxqc_conv)
      CxHdrVrbl = Cx % Header % qc_conv
    CASE (IndexCxOzone)
      CxHdrVrbl = Cx % Header % ozone
    CASE (IndexCxqcf)
      CxHdrVrbl = Cx % Header % qcf
    CASE (IndexCxqcl)
      CxHdrVrbl = Cx % Header % qcl
    CASE (IndexCxqrain)
      CxHdrVrbl = Cx % Header % qrain
    CASE (IndexCxcloud_bulk)
      CxHdrVrbl = Cx % Header % cloud_bulk
    CASE (IndexCxaerosol_p)
      IF (Cx % Header % ObsGroup == ObsGroupGroundLidar) THEN
        CxHdrVrbl = Cx % Header % aerosol_p
      END IF
    CASE (IndexCxCDNC)
      CxHdrVrbl = Cx % Header % CDNC
    CASE (IndexCxRH_AMC)
      CxHdrVrbl = Cx % Header % RH_AMC
    CASE (IndexCxCl)
      CxHdrVrbl = Cx % Header % Cl
    CASE (IndexCxCf)
      CxHdrVrbl = Cx % Header % Cf
    CASE (IndexCxExnerA)
      CxHdrVrbl = Cx % Header % ExnerA
    CASE (IndexCxRichNumber)
      CxHdrVrbl = Cx % Header % RichNumber
    CASE (IndexCxSoilMoisture)
      CxHdrVrbl = Cx % Header % SoilMoisture
    CASE (IndexCxSoilTemp)
      CxHdrVrbl = Cx % Header % SoilTemp
    CASE (IndexCxDust1, IndexCxDust2, &
          IndexCxDust3, IndexCxDust4, &
          IndexCxDust5, IndexCxDust6)
      IF (Ivar >= IndexCxDustmin .AND. Ivar <= IndexCxDustMax) THEN
        CxHdrVrbl = Cx % Header % dustp
      ELSE
        CxHdrVrbl % Present = .FALSE.
      END IF
    CASE DEFAULT
      CYCLE
  END SELECT

  IF (CxHdrVrbl % Present) THEN
    MaxUairIndex = IVar
  END IF

END DO

IF (GeneralMode >= DebugMode) THEN
  WRITE (message, '(A,I0)') "Max upper air index = ", MaxUairIndex
  CALL gen_message (RoutineName, &
                    message)
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_MaxCXIndexes
