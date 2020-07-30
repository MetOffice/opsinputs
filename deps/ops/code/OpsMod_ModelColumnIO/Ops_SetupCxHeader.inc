!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Write out model columns (Cx data) to a dump.  The Cx data arrays are assumed
! to be present and required to be output, if within the Cx structure's header
! they have a present = .true.
!
! Method:
!     Set up Cx file UM header.
!     Set up the Cx file data array(s) from those present as arguments.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_SetupCxHeader (Cx,              &
                              Cx_UMhdr,        &
                              CxLevels,        &
                              MaxSurfaceIndex, &
                              MaxUairIndex)

USE GenMod_Control, ONLY: &
    GeneralMode,          &
    DebugMode,            &
    mype

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    MessageOut,        &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
   IMDI

USE GenMod_ModelIO, ONLY: &
    UM_header_type

USE GenMod_UMHeaderConstants, ONLY: &
    FH_ColDepCSize1,                &
    IC_NumObstotal,                 &
    IC_LenObCol,                    &
    IC_LenCxCol,                    &
    IC_ObsRelease,                  &
    IC_NumMetaMax,                  &
    IC_NumItemMax,                  &
    IC_NumObVarMax,                 &
    IC_NumObPItemMax,               &
    IC_NumCxPItemMax,               &
    IC_NumCxSFVarMax,               &
    IC_NumCxUaVarMax,               &
    IC_NumMeta,                     &
    IC_NumItem,                     &
    IC_NumObVar,                    &
    IC_NumObPItem,                  &
    IC_NumCxPItem,                  &
    IC_NumCxSfVar,                  &
    IC_NumCxUaVar,                  &
    IC_NumObLev,                    &
    IC_NumCxLev

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
TYPE (Cx_type), INTENT (IN)           :: Cx          ! Cx structure
TYPE (UM_header_type), INTENT (INOUT) :: Cx_UMhdr    ! Cx UM header
INTEGER, INTENT (IN)                  :: CxLevels
INTEGER, INTENT(IN)                   :: MaxSurfaceIndex
INTEGER, INTENT(IN)                   :: MaxUairIndex

! Local declarations:
CHARACTER(len=*), PARAMETER           :: RoutineName = "Ops_SetupCxHeader"
INTEGER                               :: NumCxSfVar       ! Number of Surface Data items
INTEGER                               :: PntColDepC       ! Point to Column-Dependent Constants
INTEGER                               :: Len1ColDepC
INTEGER                               :: NumCxUAVar
INTEGER                               :: UpperDataLength
INTEGER                               :: IndexCxDustMin   ! Min Dust Cx Index
INTEGER                               :: IndexCxDustMax   ! Max Dust Cx Index
TYPE (ElementHeader_type)             :: CxHdrVrbl
INTEGER                               :: Ivar

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!------------------------------------------------------------------------------
!  Section 1.  Set up Cx_UMhdr % ColDepC (with pointers to Cx data)
!------------------------------------------------------------------------------

Cx_UMhdr % ColDepC(:) = IMDI   ! Initialise to missing data

Len1ColDepC = Cx_UMhdr % FixHd(FH_ColDepCSize1)

! Pointers to Surface variables:
NumCxSfVar = 0
PntColDepC = 5 * Cx_UMhdr % FixHd(FH_ColDepCSize1) ! Point to end of 5th 'row'

SurfVrbl: DO Ivar = 1, IndexCXSFSize

  CxHdrVrbl = ElementHeader_type()
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
    NumCxSfVar = NumCxSfVar + 1
    Cx_UMhdr % ColDepC(PntColDepC + IVar) = NumCxSfVar
  END IF

END DO SurfVrbl

IF (GeneralMode >= DebugMode .AND. mype == 0) THEN
  WRITE (MessageOut, '(A,I0)') "SetupCxHeader: NumCxSfVar = ", NumCxSfVar
END IF

! Pointers to multi-level variables:
UpperDataLength = 0
PntColDepC = 6 * Len1ColDepC          ! Point to end of 6th 'row'
NumCxUAVar = 0

! Dynamically adjust DustIndex Min and Max values
IndexCxDustMin = IndexCxDust1
IndexCxDustMax = IndexCxDustmin + NDustBins - 1

UairVrbl: DO Ivar = 1, IndexCXUASize

  CxHdrVrbl = ElementHeader_type()
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
      CxHdrVrbl = Cx % Header % PLevelsA
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
      CYCLE UairVrbl
  END SELECT

  IF (CxHdrVrbl % Present) THEN
    NumCXUAVar = NumCxUAVar + 1
    Cx_UMhdr % ColDepC(PntColDepC + Ivar) = UpperDataLength + 1
    Cx_UMhdr % ColDepC(PntColDepC + Len1ColDepC + Ivar) = CxHdrVrbl % NumLev

    UpperDataLength = UpperDataLength + CxHdrVrbl % NumLev
  END IF

END DO UairVrbl

IF (GeneralMode >= DebugMode .AND. mype == 0) THEN
  WRITE (MessageOut, '(A,I0)') "SetupCxHeader: NumCxUAVar = ", NumCxUAVar
END IF

!-------------------------------------------------------------------------------
!  Section 2.  Set up Integer Constants
!-------------------------------------------------------------------------------

Cx_UMhdr % IntC(IC_NumObstotal) =  0
Cx_UMhdr % IntC(IC_LenObCol) =  0
Cx_UMhdr % IntC(IC_LenCxCol) = NumCxSfVar + UpperDataLength
Cx_UMhdr % IntC(IC_ObsRelease) = 1
Cx_UMhdr % IntC(IC_NumMetaMax) = 0
Cx_UMhdr % IntC(IC_NumItemMax) = 0
Cx_UMhdr % IntC(IC_NumObVarMax) = 0
Cx_UMhdr % IntC(IC_NumObPItemMax) = 0
Cx_UMhdr % IntC(IC_NumCxPItemMax) = 0
Cx_UMhdr % IntC(IC_NumCxSFVarMax) = MaxSurfaceIndex
Cx_UMhdr % IntC(IC_NumCxUaVarMax) = MaxUairIndex
Cx_UMhdr % IntC(IC_NumMeta) = 0
Cx_UMhdr % IntC(IC_NumItem) = 0
Cx_UMhdr % IntC(IC_NumObVar) = 0
Cx_UMhdr % IntC(IC_NumObPItem) = 0
Cx_UMhdr % IntC(IC_NumCxPItem) = 0
IF (Cx % Header % SST % Present) THEN
  Cx_UMhdr % IntC(IC_NumCxPItem) = IMDI
END IF
Cx_UMhdr % IntC(IC_NumCxSfVar) = NumCxSfVar
Cx_UMhdr % IntC(IC_NumCxUaVar) = NumCxUaVar
Cx_UMhdr % IntC(IC_NumObLev) = 0
Cx_UMhdr % IntC(IC_NumCxLev) = CxLevels

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_SetupCxHeader
