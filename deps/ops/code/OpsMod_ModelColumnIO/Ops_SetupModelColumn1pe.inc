!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Sets up an array containing CX data to be written to a file.  For each of the
! indices in OpsMod_CXIndexes the data in the CX structure "Cx" is copied to
! CxData if it is present.  The integer constants are also updated with the
! data lengths.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_SetupModelColumn1pe (NumCx,    &
                                    Cx,       &
                                    Cx_UMhdr, &
                                    CxData,   &
                                    batch)

USE GenMod_Core, ONLY: &
    gen_fail,          &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_ModelIO, ONLY: &
    UM_header_type

USE GenMod_UMHeaderConstants, ONLY : &
    FH_DataSize,                     &
    IC_LenCxCol,                     &
    IC_NumObsTotal,                  &
    IC_NumVarBatches

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
INTEGER, INTENT (IN)                  :: NumCx(:)    ! Number of Cx columns
TYPE (Cx_type), INTENT(IN)            :: Cx
TYPE (UM_header_type), INTENT (INOUT) :: Cx_UMhdr    ! Cx UM header
REAL, INTENT(INOUT)                   :: CxData(:,:) ! 1D array for output of data
INTEGER, INTENT(IN)                   :: batch

! Local declarations:
INTEGER                               :: LenCx
INTEGER                               :: LenBatch
INTEGER                               :: Pntr
INTEGER                               :: LastCx
INTEGER                               :: Ivar
TYPE (ElementHeader_type)             :: CxHdrVrbl
REAL, POINTER                         :: CxVrblSurf(:)
REAL, POINTER                         :: CxVrblUair(:,:)
INTEGER                               :: Icx
INTEGER                               :: Pos
INTEGER                               :: ilev
CHARACTER(len=80)                     :: ErrMess(2)
INTEGER                               :: DustInd
INTEGER                               :: IndexCxDustMin
INTEGER                               :: IndexCxDustMax
CHARACTER(len=*), PARAMETER           :: RoutineName = "Ops_SetupModelColumn1pe"
INTEGER                               :: start_of_batch

!------------------------------------------------------------------------------
!  Section 0.1:  Tracing and Initialisation
!------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

LenCx = Cx_UMhdr % IntC(IC_LenCxCol)         ! Length of each Cx column
LenBatch = NumCx(batch) * LenCx           ! Length of this batch
Cx_UMhdr % FixHd(FH_DataSize) = LenBatch
Cx_UMhdr % MaxFldSize = MAX (Cx_UMhdr % MaxFldSize, LenBatch)
start_of_batch = SUM (NumCx(1:batch - 1))

!-------------------------------------------------------------------------------
!  Section 1:  Copy CX info to CxData
!-------------------------------------------------------------------------------

!-----------------------------------------------------------------------------
!  Section 1.1  Increment the number of obs and batches
!-----------------------------------------------------------------------------

Cx_UMhdr % IntC(IC_NumObsTotal) = SUM (NumCx(:))
Cx_UMhdr % IntC(IC_NumVarBatches) = SIZE (NumCx)

!-----------------------------------------------------------------------------
!  Section 1.3  Set up the Surface Data
!-----------------------------------------------------------------------------
Pntr = 0
LastCx = LenCx * (NumCx(batch) - 1)   ! Point before last Cx column

SurfVrbl: DO Ivar = 1, IndexCXSFSize
  CxHdrVrbl = ElementHeader_type()
  SurfVrbl2: SELECT CASE (Ivar)
    CASE (IndexCxorog)
      CxHdrVrbl = Cx % Header % orog
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % orog(:)
    CASE (IndexCxpstar)
      CxHdrVrbl = Cx % Header % pstar
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % pstar(:)
    CASE (IndexCxrh2)
      CxHdrVrbl = Cx % Header % rh2
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % rh2(:)
    CASE (IndexCxu10)
      CxHdrVrbl = Cx % Header % u10
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % u10(:)
    CASE (IndexCxv10)
      CxHdrVrbl = Cx % Header % v10
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % v10(:)
    CASE (IndexCxvis)
      CxHdrVrbl = Cx % Header % vis
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % vis(:)
    CASE (IndexCxWaveHeight)
      CxHdrVrbl = Cx % Header % WaveHeight
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % WaveHeight(:)
    CASE (IndexCxWAVE_HGHT)
      CxHdrVrbl = Cx % Header % WAVE_HGHT
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % WAVE_HGHT(:)
    CASE (IndexCxWIND_SPED)
      CxHdrVrbl = Cx % Header % WIND_SPED
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % WIND_SPED(:)
    CASE (IndexCxSeaHeight)
      CxHdrVrbl = Cx % Header % SeaHeight
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % SeaHeight(:)
    CASE (IndexCxSST)
      CxHdrVrbl = Cx % Header % SST
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % SST(:)
    CASE (IndexCxTskinSea)
      CxHdrVrbl = Cx % Header % TskinSea
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % TskinSea(:)
    CASE (IndexCxTropPres)
      CxHdrVrbl = Cx % Header % TropPres
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % TropPres(:)
    CASE (IndexCxt2)
      CxHdrVrbl = Cx % Header % t2
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % t2(:)
    CASE (IndexCxpmsl)
      CxHdrVrbl = Cx % Header % pmsl
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % pmsl(:)
    CASE (IndexCxSeaIce)
      CxHdrVrbl = Cx % Header % SeaIce
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % SeaIce(:)
    CASE (IndexCxSnowAmount)
      CxHdrVrbl = Cx % Header % SnowAmount
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % SnowAmount(:)
    CASE (IndexCxqt2)
      CxHdrVrbl = Cx % Header % qt2
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % qt2(:)
    CASE (IndexCxaerosol)
      IF (Cx % Header % ObsGroup == ObsGroupSurface) THEN
        CxHdrVrbl = Cx % Header % aerosol
        IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
        CxVrblSurf => Cx % aerosol(:)
      END IF
    CASE (IndexCxPSurfParamA)
      CxHdrVrbl = Cx % Header % PSurfParamA
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % PSurfParamA(:)
    CASE (IndexCxPSurfParamB)
      CxHdrVrbl = Cx % Header % PSurfParamB
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % PSurfParamB(:)
    CASE (IndexCxLapseRate)
      CxHdrVrbl = Cx % Header % LapseRate
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % LapseRate(:)
    CASE (IndexCxCloudAmount)
      CxHdrVrbl = Cx % Header % CloudAmount
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % CloudAmount(:)
    CASE (IndexCxConvCloudAmount)
      CxHdrVrbl = Cx % Header % ConvCloudAmount
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % ConvCloudAmount(:)
    CASE (IndexCxConvCloudBaseLevel)
      CxHdrVrbl = Cx % Header % ConvCloudBaseLevel
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % ConvCloudBaseLevel(:)
    CASE (IndexCxConvCloudTopLevel)
      CxHdrVrbl = Cx % Header % ConvCloudTopLevel
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % ConvCloudTopLevel(:)
    CASE (IndexCxSurfRainRate_conv)
      CxHdrVrbl = Cx % Header % SurfRainRate_conv
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % SurfRainRate_conv(:)
    CASE (IndexCxSurfSnowRate_conv)
      CxHdrVrbl = Cx % Header % SurfSnowRate_conv
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % SurfSnowRate_conv(:)
    CASE (IndexCxSurfRainRate_LS)
      CxHdrVrbl = Cx % Header % SurfRainRate_LS
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % SurfRainRate_LS(:)
    CASE (IndexCxSurfSnowRate_LS)
      CxHdrVrbl = Cx % Header % SurfSnowRate_LS
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % SurfSnowRate_LS(:)
    CASE (IndexCxSeaSrfcHeight)
      CxHdrVrbl = Cx % Header % SeaSrfcHeight
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % SeaSrfcHeight(:)
    CASE (IndexCxMeanSeaHeight)
      CxHdrVrbl = Cx % Header % MeanSeaHeight
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % MeanSeaHeight(:)
    CASE (IndexCxSWradiation)
      CxHdrVrbl = Cx % Header % SWradiation
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % SWradiation(:)
    CASE (IndexCxBLheight)
      CxHdrVrbl = Cx % Header % BLheight
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % BLheight(:)
    CASE (IndexCxObukhovLength)
      CxHdrVrbl = Cx % Header % ObukhovLength
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % ObukhovLength(:)
    CASE (IndexCxFrictionVel)
      CxHdrVrbl = Cx % Header % FrictionVel
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % FrictionVel(:)
    CASE (IndexCxPrecipAcc6hr)
      CxHdrVrbl = Cx % Header % PrecipAcc6hr
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % PrecipAcc6hr(:)
    CASE (IndexCxLowCloudAmount)
      CxHdrVrbl = Cx % Header % LowCloudAmount
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % LowCloudAmount(:)
    CASE (IndexCxMedCloudAmount)
      CxHdrVrbl = Cx % Header % MedCloudAmount
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % MedCloudAmount(:)
    CASE (IndexCxLowCloudBase)
      CxHdrVrbl = Cx % Header % LowCloudBase
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % LowCloudBase(:)
    CASE (IndexCxSo2_AQ)
      CxHdrVrbl = Cx % Header % So2_AQ
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % So2_AQ(:)
    CASE (IndexCxPM10_AQ)
      CxHdrVrbl = Cx % Header % PM10_AQ
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % PM10_AQ(:)
    CASE (IndexCxPM2p5_AQ)
      CxHdrVrbl = Cx % Header % PM2p5_AQ
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % PM2p5_AQ(:)
    CASE (IndexCxO3_AQ)
      CxHdrVrbl = Cx % Header % O3_AQ
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % O3_AQ(:)
    CASE (IndexCxNo2_AQ)
      CxHdrVrbl = Cx % Header % No2_AQ
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % No2_AQ(:)
    CASE (IndexCxCO_AQ)
      CxHdrVrbl = Cx % Header % CO_AQ
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % CO_AQ(:)
    CASE (IndexCxBLtype)
      CxHdrVrbl = Cx % Header % BLtype
      IF (.NOT. CxHdrVrbl % Present) CYCLE SurfVrbl
      CxVrblSurf => Cx % BLtype(:)
    CASE DEFAULT
      ! As the Cx Index numbers need not be contiguous, this is not
      ! a problem
      CYCLE
  END SELECT SurfVrbl2

  IF (CxHdrVrbl % Present) THEN
    Pntr = Pntr + 1              ! Point to next Cx column
    CxData(batch,Pntr:Pntr + LastCx:LenCx) = CxVrblSurf(start_of_batch + 1:start_of_batch + NumCx(batch))
  END IF
END DO SurfVrbl

!-----------------------------------------------------------------------------
!  Section 1.4  Set up the multi-level data
!-----------------------------------------------------------------------------
IndexCxDustMin = IndexCxDust1
IndexCxDustMax = IndexCxDustMin + NDustBins - 1

UairVrbl: DO Ivar = 1, IndexCXUASize

  CxHdrVrbl = ElementHeader_type()
  UairVrbl2: SELECT CASE (Ivar)
    CASE (IndexCxtheta)
      CxHdrVrbl = Cx % Header % theta
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % theta(:,:)
    CASE (IndexCxrh)
      CxHdrVrbl = Cx % Header % rh
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % rh(:,:)
    CASE (IndexCxu)
      CxHdrVrbl = Cx % Header % u
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % u(:,:)
    CASE (IndexCxv)
      CxHdrVrbl = Cx % Header % v
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % v(:,:)
    CASE (IndexCxw)
      CxHdrVrbl = Cx % Header % w
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % w(:,:)
    CASE (IndexCxq)
      CxHdrVrbl = Cx % Header % q
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % q(:,:)
    CASE (IndexCxqc)
      CxHdrVrbl = Cx % Header % qc
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % qc(:,:)
    CASE (IndexCxp_bar)
      CxHdrVrbl = Cx % Header % p_bar
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % p_bar(:,:)
    CASE (IndexCxcloud)
      CxHdrVrbl = Cx % Header % cloud
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % cloud(:,:)
    CASE (IndexCxql_layer)
      CxHdrVrbl = Cx % Header % ql_layer
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % ql_layer(:,:)
    CASE (IndexCxP)
      CxHdrVrbl = Cx % Header % PlevelsA
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % PlevelsA(:,:)
    CASE (IndexCxSalt)
      CxHdrVrbl = Cx % Header % Salt
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % Salt(:,:)
    CASE (IndexCxt)
      CxHdrVrbl = Cx % Header % t
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % t(:,:)
    CASE (IndexCxqf_layer)
      CxHdrVrbl = Cx % Header % qf_layer
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % qf_layer(:,:)
    CASE (IndexCxRainRate_layer)
      CxHdrVrbl = Cx % Header % RainRate_layer
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % RainRate_layer(:,:)
    CASE (IndexCxcloud_conv)
      CxHdrVrbl = Cx % Header % cloud_conv
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % cloud_conv(:,:)
    CASE (IndexCxqc_conv)
      CxHdrVrbl = Cx % Header % qc_conv
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % qc_conv(:,:)
    CASE (IndexCxcloud_layer)
      CxHdrVrbl = Cx % Header % cloud_layer
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % cloud_layer(:,:)
    CASE (IndexCxOzone)
      CxHdrVrbl = Cx % Header % ozone
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % ozone(:,:)
    CASE (IndexCxqcf)
      CxHdrVrbl = Cx % Header % qcf
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % qcf(:,:)
    CASE (IndexCxqcl)
      CxHdrVrbl = Cx % Header % qcl
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % qcl(:,:)
    CASE (IndexCxcloud_bulk)
      CxHdrVrbl = Cx % Header % cloud_bulk
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % cloud_bulk(:,:)
    CASE (IndexCxaerosol_p)
      IF (Cx % Header % ObsGroup == ObsGroupGroundLidar) THEN
        CxHdrVrbl = Cx % Header % aerosol_p
        IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
        CxVrblUair => Cx % aerosol_p(:,:)
      END IF
    CASE (IndexCxCDNC)
      CxHdrVrbl = Cx % Header % CDNC
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % CDNC(:,:)
    CASE (IndexCxRH_AMC)
      CxHdrVrbl = Cx % Header % RH_AMC
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % RH_AMC(:,:)
    CASE (IndexCxCl)
      CxHdrVrbl = Cx % Header % Cl
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % Cl(:,:)
    CASE (IndexCxCf)
      CxHdrVrbl = Cx % Header % Cf
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % Cf(:,:)
    CASE (IndexCxqrain)
      CxHdrVrbl = Cx % Header % qrain
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % qrain(:,:)
    CASE (IndexCxExnerA)
      CxHdrVrbl = Cx % Header % ExnerA
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % ExnerA(:,:)
    CASE (IndexCxRichNumber)
      CxHdrVrbl = Cx % Header % RichNumber
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % RichNumber(:,:)
    CASE (IndexCxSoilMoisture)
      CxHdrVrbl = Cx % Header % SoilMoisture
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % SoilMoisture(:,:)
    CASE (IndexCxSoilTemp)
      CxHdrVrbl = Cx % Header % SoilTemp
      IF (.NOT. CxHdrVrbl % Present) CYCLE UairVrbl
      CxVrblUair => Cx % SoilTemp(:,:)
    CASE (IndexCxDust1, IndexCxDust2, &
          IndexCxDust3, IndexCxDust4, &
          IndexCxDust5, IndexCxDust6)
      CxHdrVrbl = Cx % Header % dustp
      IF (Ivar > IndexCxDustMax .OR. .NOT. CxHdrVrbl % Present) THEN
        CxHdrVrbl % Present = .FALSE.
        CYCLE UairVrbl
      ENDIF
      DustInd = Ivar - IndexCxDustMin + 1
      CxVrblUair => Cx % dustp(DustInd) % field(:,:)

    CASE DEFAULT
      ! As the Cx Index numbers need not be contiguous, this is not
      ! a problem
      CYCLE UairVrbl
  END SELECT UairVrbl2

  IF (CxHdrVrbl % Present)  THEN
    DO ICx = 1, NumCx(batch)
      Pos = Pntr + 1 + (ICx - 1) * LenCx   ! Point to next Cx column
      DO Ilev = 1, CxHdrVrbl % NumLev
        CxData(batch,Pos) = CxVrblUair(start_of_batch + ICx,Ilev)
        Pos = Pos + 1                      ! Point to next Cx column
      END DO
    END DO
    NULLIFY (CxVrblUair)
    Pntr = Pntr + CxHdrVrbl % NumLev
  END IF

END DO UairVrbl

!-----------------------------------------------------------------------------
!  Section 1.5  Sanity check
!-----------------------------------------------------------------------------

IF (Pntr /= LenCx) THEN
  WRITE (ErrMess(1), '(A,2(1X,I0))') ' Final value of Pntr /= LenCx :', Pntr, LenCx
  ErrMess(2) = 'i.e. number of elements different to length of each Cx column'
  CALL gen_fail (RoutineName,  &
                 ErrMess(1:2))
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_SetupModelColumn1pe
