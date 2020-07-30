!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
!-------------------------------------------------------------------------------
! Performs action defined by ActionArg on all array elements of CX.  Calls
! Ops_ObsAction for every array element.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CXGlobalAction (CX,        &
                               ActionArg)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI

USE OpsMod_AODGeneral, ONLY: &
    NDustBins

USE OpsMod_MiscTypes, ONLY: &
    ElementHeader_type,     &
    ActionDealloc,          &
    ActionInit

USE OpsMod_ObsGroupInfo, ONLY: &
    ObsGroupAOD

USE OpsMod_ObsInfo, ONLY: &
    Ops_ObsAction

IMPLICIT NONE

! Subroutine arguments:
TYPE (CX_type)              :: CX
INTEGER, INTENT(IN)         :: ActionArg

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_CXGlobalAction"
INTEGER                     :: DustInd     ! loop over dust fields
CHARACTER                   :: DustIndStr
TYPE (ElementHeader_Type)   :: DumHdr

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! Update NDustBins from Namelist file

IF (ActionArg == ActionInit) THEN
  NULLIFY (Cx % Header % AK)
  NULLIFY (Cx % Header % BK)
  NULLIFY (Cx % Header % Depths)
  NULLIFY (Cx % Header % AKH)
  NULLIFY (Cx % Header % BKH)
  NULLIFY (Cx % Header % EtaTheta)
  NULLIFY (Cx % Header % EtaRho)
  NULLIFY (Cx % Header % EtaP)
  NULLIFY (Cx % Id)
  Cx % Header % PoleLat = RMDI
  Cx % Header % PoleLong = RMDI
  ALLOCATE (Cx % dustp(NDustBins))
END IF

CALL Ops_ObsAction (Cx % Header % PlevelsA, &
                    'CX % PLevelsA',        &
                    ActionArg,              &
                    Real2 = CX % PLevelsA)
CALL Ops_ObsAction (Cx % Header % PlevelsB, &
                    'CX % PlevelsB',        &
                    ActionArg,              &
                    Real2 = CX % PlevelsB)
CALL Ops_ObsAction (Cx % Header % orog, &
                    'CX % orog',        &
                    ActionArg,          &
                    Real1 = CX % orog)
CALL Ops_ObsAction (Cx % Header % pmsl, &
                    'CX % pmsl',        &
                    ActionArg,          &
                    Real1 = CX % pmsl)
CALL Ops_ObsAction (Cx % Header % pstar, &
                    'CX % pstar',        &
                    ActionArg,           &
                    Real1 = CX % pstar)
CALL Ops_ObsAction (Cx % Header % PSurfParamA, &
                    'CX % PSurfParamA',        &
                    ActionArg,                 &
                    Real1 = CX % PSurfParamA)
CALL Ops_ObsAction (Cx % Header % PSurfParamB, &
                    'CX % PSurfParamB',        &
                    ActionArg,                 &
                    Real1 = CX % PSurfParamB)
CALL Ops_ObsAction (Cx % Header % LapseRate, &
                    'CX % LapseRate',        &
                    ActionArg,               &
                    Real1 = CX % LapseRate)
CALL Ops_ObsAction (Cx % Header % t2, &
                    'CX % t2',        &
                    ActionArg,        &
                    Real1 = CX % t2)
CALL Ops_ObsAction (Cx % Header % rh2, &
                    'CX % rh2',        &
                    ActionArg,         &
                    Real1 = CX % rh2)
CALL Ops_ObsAction (CX % Header % u10, &
                    'CX % u10',        &
                    ActionArg,         &
                    Real1 = CX % u10)
CALL Ops_ObsAction (Cx % Header % v10, &
                    'CX % v10',        &
                    ActionArg,         &
                    Real1 = CX % v10)
CALL Ops_ObsAction (CX % Header % u10_orig, &
                    'CX % u10_orig',        &
                    ActionArg,              &
                    Real1 = CX % u10_orig)
CALL Ops_ObsAction (Cx % Header % v10_orig, &
                    'CX % v10_orig',        &
                    ActionArg,              &
                    Real1 = CX % v10_orig)
CALL Ops_ObsAction (Cx % Header % CDNC, &
                    'CX % CDNC',        &
                    ActionArg,          &
                    Real2 = CX % CDNC)
CALL Ops_ObsAction (Cx % Header % CloudAmount, &
                    'CX % CloudAmount',        &
                    ActionArg,                 &
                    Real1 = CX % CloudAmount)
CALL Ops_ObsAction (Cx % Header % ConvCloudAmount, &
                    'CX % ConvCloudAmount',        &
                    ActionArg,                     &
                    Real1 = CX % ConvCloudAmount)
CALL Ops_ObsAction (Cx % Header % ConvCloudBaseLevel, &
                    'CX % ConvCloudBaseLevel',        &
                    ActionArg,                        &
                    Real1 = CX % ConvCloudBaseLevel)
CALL Ops_ObsAction (Cx % Header % ConvCloudTopLevel, &
                    'CX % ConvCloudTopLevel',        &
                    ActionArg,                       &
                    Real1 = CX % ConvCloudTopLevel)
CALL Ops_ObsAction (Cx % Header % SurfRainRate_LS, &
                    'CX % SurfRainRate_LS',        &
                    ActionArg,                     &
                    Real1 = CX % SurfRainRate_LS)
CALL Ops_ObsAction (Cx % Header % SurfSnowRate_LS, &
                    'CX % SurfSnowRate_LS',        &
                    ActionArg,                     &
                    Real1 = CX % SurfSnowRate_LS)
CALL Ops_ObsAction (Cx % Header % SurfRainRate_conv, &
                    'CX % SurfRainRate_conv',        &
                    ActionArg,                       &
                    Real1 = CX % SurfRainRate_conv)
CALL Ops_ObsAction (Cx % Header % SurfSnowRate_conv, &
                    'CX % SurfSnowRate_conv',        &
                    ActionArg,                       &
                    Real1 = CX % SurfSnowRate_conv)
CALL Ops_ObsAction (Cx % Header % vis, &
                    'CX % vis',        &
                    ActionArg,         &
                    Real1 = CX % vis)
CALL Ops_ObsAction (Cx % Header % WaveHeight, &
                    'CX % WaveHeight',        &
                    ActionArg,                &
                    Real1 = CX % WaveHeight)
CALL Ops_ObsAction (Cx % Header % WAVE_HGHT, &
                    'CX % WAVE_HGHT',        &
                    ActionArg,               &
                    Real1 = CX % WAVE_HGHT)
CALL Ops_ObsAction (Cx % Header % WIND_SPED, &
                    'CX % WIND_SPED',        &
                    ActionArg,               &
                    Real1 = CX % WIND_SPED)
CALL Ops_ObsAction (Cx % Header % SeaHeight, &
                    'CX % SeaHeight',        &
                    ActionArg,               &
                    Real1 = CX % SeaHeight)
CALL Ops_ObsAction (Cx % Header % SST, &
                    'CX % SST',        &
                    ActionArg,         &
                    Real1 = CX % SST)
CALL Ops_ObsAction (Cx % Header % TskinSea, &
                    'CX % TskinSea',        &
                    ActionArg,              &
                    Real1 = CX % TskinSea)
CALL Ops_ObsAction (Cx % Header % SeaIce, &
                    'CX % SeaIce',        &
                    ActionArg,            &
                    Real1 = CX % SeaIce)
CALL Ops_ObsAction (Cx % Header % CHL, &
                    'CX % CHL',        &
                    ActionArg,         &
                    Real1 = CX % CHL)
CALL Ops_ObsAction (Cx % Header % KD490, &
                    'CX % KD490',        &
                    ActionArg,           &
                    Real1 = CX % KD490)
CALL Ops_ObsAction (Cx % Header % LCHL, &
                    'CX % LCHL',        &
                    ActionArg,         &
                    Real1 = CX % LCHL)
CALL Ops_ObsAction (Cx % Header % LKD490, &
                    'CX % LKD490',        &
                    ActionArg,            &
                    Real1 = CX % LKD490)
CALL Ops_ObsAction (Cx % Header % SnowAmount, &
                    'CX % SnowAmount',        &
                    ActionArg,                &
                    Real1 = CX % SnowAmount)
CALL Ops_ObsAction (Cx % Header % TropPres, &
                    'CX % TropPres',        &
                    ActionArg,              &
                    Real1 = CX % TropPres)
CALL Ops_ObsAction (Cx % Header % qt2, &
                    'CX % qt2',        &
                    ActionArg,         &
                    Real1 = CX % qt2)
CALL Ops_ObsAction (Cx % Header % logm, &
                    'CX % logm',        &
                    ActionArg,          &
                    Real1 = CX % logm)
CALL Ops_ObsAction (Cx % Header % aerosol, &
                    'CX % aerosol',        &
                    ActionArg,             &
                    Real1 = CX % aerosol)
CALL Ops_ObsAction (Cx % Header % aerosol_p, &
                    'CX % aerosol_p',        &
                    ActionArg,               &
                    Real2 = CX % aerosol_p)
CALL Ops_ObsAction (Cx % Header % t, &
                    'CX % t',        &
                    ActionArg,       &
                    Real2 = CX % t)
CALL Ops_ObsAction (Cx % Header % theta, &
                    'CX % theta',        &
                    ActionArg,           &
                    Real2 = CX % theta)
CALL Ops_ObsAction (Cx % Header % rh, &
                    'CX % rh',        &
                    ActionArg,        &
                    Real2 = CX % rh)
CALL Ops_ObsAction (Cx % Header % rhtotal, &
                    'CX % rhtotal',        &
                    ActionArg,             &
                    Real2 = CX % rhtotal)
CALL Ops_ObsAction (Cx % Header % q, &
                    'CX % q',        &
                    ActionArg,       &
                    Real2 = CX % q)
CALL Ops_ObsAction (Cx % Header % qc, &
                    'CX % qc',        &
                    ActionArg,        &
                    Real2 = CX % qc)
CALL Ops_ObsAction (Cx % Header % p_bar, &
                    'CX % p_bar',        &
                    ActionArg,           &
                    Real2 = CX % p_bar)
CALL Ops_ObsAction (Cx % Header % ozone, &
                    'CX % ozone',        &
                    ActionArg,           &
                    Real2 = CX % ozone)
IF (Cx % header % ObsGroup == ObsGroupAOD) THEN
  DO DustInd = 1, NDustBins
    WRITE (DustIndStr, '(i1)') DustInd
    IF (DustInd < NDustBins) THEN
      DumHdr = Cx % Header % dustp
      CALL Ops_ObsAction (DumHdr,                              &
                          'CX % dust' // DustIndStr,           &
                          ActionArg,                           &
                          Real2 = CX % dustp(DustInd) % field)
    ELSE
      CALL Ops_ObsAction (Cx % Header % dustp,                 &
                          'CX % dust' // DustIndStr,           &
                          ActionArg,                           &
                          Real2 = CX % dustp(DustInd) % field)
    END IF
  END DO
END IF
CALL Ops_ObsAction (Cx % Header % AOD, &
                    'CX % AOD',        &
                    ActionArg,         &
                    Real2 = CX % AOD)
CALL Ops_ObsAction (Cx % Header % SeaSrfcHeight, &
                    'CX % SeaSrfcHeight',        &
                    ActionArg,                   &
                    Real1 = CX % SeaSrfcHeight)
CALL Ops_ObsAction (Cx % Header % MeanSeaHeight, &
                    'CX % MeanSeaHeight',        &
                    ActionArg,                   &
                    Real1 = CX % MeanSeaHeight)
CALL Ops_ObsAction (Cx % header % u, &
                    'CX % u',        &
                    ActionArg,       &
                    Real2 = CX % u)
CALL Ops_ObsAction (Cx % Header % v, &
                    'CX % v',        &
                    ActionArg,       &
                    Real2 = CX % v)
CALL Ops_ObsAction (Cx % header % u_orig, &
                    'CX % u_orig',        &
                    ActionArg,            &
                    Real2 = CX % u_orig)
CALL Ops_ObsAction (Cx % Header % v_orig, &
                    'CX % v_orig',        &
                    ActionArg,            &
                    Real2 = CX % v_orig)
CALL Ops_ObsAction (Cx % Header % w, &
                    'CX % w',        &
                    ActionArg,       &
                    Real2 = CX % w)
CALL Ops_ObsAction (Cx % Header % cloud, &
                    'CX % cloud',        &
                    ActionArg,           &
                    Real2 = CX % cloud)
CALL Ops_ObsAction (Cx % Header % ql_layer, &
                    'CX % ql_layer',        &
                    ActionArg,              &
                    Real2 = CX % ql_layer)
CALL Ops_ObsAction (Cx % Header % qf_layer, &
                    'CX % qf_layer',        &
                    ActionArg,              &
                    Real2 = CX % qf_layer)
CALL Ops_ObsAction (Cx % Header % RainRate_layer, &
                    'CX % RainRate_layer',        &
                    ActionArg,                    &
                    Real2 = CX % RainRate_layer)
CALL Ops_ObsAction (Cx % Header % cloud_layer, &
                    'CX % cloud_layer',        &
                    ActionArg,                 &
                    Real2 = CX % cloud_layer)
CALL Ops_ObsAction (Cx % Header % cloud_conv, &
                    'CX % cloud_conv',        &
                    ActionArg,                &
                    Real2 = CX % cloud_conv)
CALL Ops_ObsAction (Cx % Header % qc_conv, &
                    'CX % qc_conv',        &
                    ActionArg,             &
                    Real2 = CX % qc_conv)
CALL Ops_ObsAction (Cx % Header % qcf, &
                    'CX % qcf',        &
                    ActionArg,         &
                    Real2 = CX % qcf)
CALL Ops_ObsAction (Cx % Header % qcl, &
                    'CX % qcl',        &
                    ActionArg,         &
                    Real2 = CX % qcl)
CALL Ops_ObsAction (Cx % Header % qrain, &
                    'CX % qrain',        &
                    ActionArg,           &
                    Real2 = CX % qrain)
CALL Ops_ObsAction (Cx % Header % Cf, &
                    'CX % Cf',        &
                    ActionArg,        &
                    Real2 = CX % Cf)
CALL Ops_ObsAction (Cx % Header % Cl, &
                    'CX % Cl',        &
                    ActionArg,        &
                    Real2 = CX % Cl)
CALL Ops_ObsAction (Cx % Header % cloud_bulk, &
                    'CX % cloud_bulk',        &
                    ActionArg,                &
                    Real2 = CX % cloud_bulk)
CALL Ops_ObsAction (Cx % Header % RH_AMC, &
                    'CX % RH_AMC',        &
                    ActionArg,            &
                    Real2 = CX % RH_AMC)
CALL Ops_ObsAction (Cx % Header % Salt, &
                    'CX % Salt',        &
                    ActionArg,          &
                    Real2 = CX % Salt)
CALL Ops_ObsAction (Cx % Header % ZA, &
                    'CX % ZA',        &
                    ActionArg,        &
                    Real2 = CX % ZA)
CALL Ops_ObsAction (Cx % Header % ZB, &
                    'CX % ZB',        &
                    ActionArg,        &
                    Real2 = CX % ZB)
CALL Ops_ObsAction (Cx % Header % ExnerA, &
                    'CX % ExnerA',        &
                    ActionArg,            &
                    Real2 = CX % ExnerA)
CALL Ops_ObsAction (Cx % Header % ExnerB, &
                    'CX % ExnerB',        &
                    ActionArg,            &
                    Real2 = CX % ExnerB)
CALL Ops_ObsAction (Cx % Header % SSTMes_Var, &
                    'CX % SSTMes_Var',        &
                    ActionArg,                &
                    Real1 = CX % SSTMes_Var)
CALL Ops_ObsAction (Cx % Header % SSTMes_HCor, &
                    'CX % SSTMes_HCor',        &
                    ActionArg,                 &
                    Real1 = CX % SSTMes_HCor)
CALL Ops_ObsAction (Cx % Header % SSTSyn_Var, &
                    'CX % SSTSyn_Var',        &
                    ActionArg,                &
                    Real1 = CX % SSTSyn_Var)
CALL Ops_ObsAction (Cx % Header % SSTSyn_HCor, &
                    'CX % SSTSyn_HCor',        &
                    ActionArg,                 &
                    Real1 = CX % SSTSyn_HCor)
CALL Ops_ObsAction (Cx % Header % SSTOb_Err, &
                    'CX % SSTOb_Err',        &
                    ActionArg,               &
                    Real1 = CX % SSTOb_Err)
CALL Ops_ObsAction (Cx % Header % SSHMes_Var, &
                    'CX % SSHMes_Var',        &
                    ActionArg,                &
                    Real1 = CX % SSHMes_Var)
CALL Ops_ObsAction (Cx % Header % SSHMes_HCor, &
                    'CX % SSHMes_HCor',        &
                    ActionArg,                 &
                    Real1 = CX % SSHMes_HCor)
CALL Ops_ObsAction (Cx % Header % SSHSyn_Var, &
                    'CX % SSHSyn_Var',        &
                    ActionArg,                &
                    Real1 = CX % SSHSyn_Var)
CALL Ops_ObsAction (Cx % Header % SSHSyn_HCor, &
                    'CX % SSHSyn_HCor',        &
                    ActionArg,                 &
                    Real1 = CX % SSHSyn_HCor)
CALL Ops_ObsAction (Cx % Header % SSHOb_Err, &
                    'CX % SSHOb_Err',        &
                    ActionArg,               &
                    Real1 = CX % SSHOb_Err)
CALL Ops_ObsAction (Cx % Header % TempMes_Var, &
                    'CX % TempMes_Var',        &
                    ActionArg,                 &
                    Real2 = CX % TempMes_Var)
CALL Ops_ObsAction (Cx % Header % TempMes_HCor, &
                    'CX % TempMes_HCor',        &
                    ActionArg,                  &
                    Real2 = CX % TempMes_HCor)
CALL Ops_ObsAction (Cx % Header % TempSyn_Var, &
                    'CX % TempSyn_Var',        &
                    ActionArg,                 &
                    Real2 = CX % TempSyn_Var)
CALL Ops_ObsAction (Cx % Header % TempSyn_HCor, &
                    'CX % TempSyn_HCor',        &
                    ActionArg,                  &
                    Real2 = CX % TempSyn_HCor)
CALL Ops_ObsAction (Cx % Header % TempOb_Err, &
                    'CX % TempOb_Err',        &
                    ActionArg,                &
                    Real2 = CX % TempOb_Err)
CALL Ops_ObsAction (Cx % Header % SalMes_Var, &
                    'CX % SalMes_Var',        &
                    ActionArg,                &
                    Real2 = CX % SalMes_Var)
CALL Ops_ObsAction (Cx % Header % SalMes_HCor, &
                    'CX % SalMes_HCor',        &
                    ActionArg,                 &
                    Real2 = CX % SalMes_HCor)
CALL Ops_ObsAction (Cx % Header % SalSyn_Var, &
                    'CX % SalSyn_Var',        &
                    ActionArg,                &
                    Real2 = CX % SalSyn_Var)
CALL Ops_ObsAction (Cx % Header % SalSyn_HCor, &
                    'CX % SalSyn_HCor',        &
                    ActionArg,                 &
                    Real2 = CX % SalSyn_HCor)
CALL Ops_ObsAction (Cx % Header % SalOb_Err, &
                    'CX % SalOb_Err',        &
                    ActionArg,               &
                    Real2 = CX % SalOb_Err)
CALL Ops_ObsAction (Cx % Header % SWradiation, &
                    'CX % SWradiation',        &
                    ActionArg,                 &
                    Real1 = CX % SWradiation)
CALL Ops_ObsAction (Cx % Header % PrecipAcc6hr, &
                    'CX % PrecipAcc6hr',        &
                    ActionArg,                  &
                    Real1 = CX % PrecipAcc6hr)
CALL Ops_ObsAction (Cx % Header % LowCloudAmount, &
                    'CX % LowCloudAmount',        &
                    ActionArg,                    &
                    Real1 = CX % LowCloudAmount)
CALL Ops_ObsAction (Cx % Header % MedCloudAmount, &
                    'CX % MedCloudAmount',        &
                    ActionArg,                    &
                    Real1 = CX % MedCloudAmount)
CALL Ops_ObsAction (Cx % Header % LowCloudBase, &
                    'CX % LowCloudBase',        &
                    ActionArg,                  &
                    Real1 = CX % LowCloudBase)
CALL Ops_ObsAction (Cx % Header % BLheight, &
                    'CX % BLheight',        &
                    ActionArg,              &
                    Real1 = CX % BLheight)
CALL Ops_ObsAction (Cx % Header % ObukhovLength, &
                    'CX % ObukhovLength',        &
                    ActionArg,                   &
                    Real1 = CX % ObukhovLength)
CALL Ops_ObsAction (Cx % Header % FrictionVel, &
                    'CX % FrictionVel',        &
                    ActionArg,                 &
                    Real1 = CX % FrictionVel)
CALL Ops_ObsAction (Cx % Header % RichNumber, &
                    'CX % RichNumber',        &
                    ActionArg,                &
                    Real2 = CX % RichNumber)
CALL Ops_ObsAction (Cx % Header % SoilMoisture, &
                    'CX % SoilMoisture',        &
                    ActionArg,                  &
                    Real2 = CX % SoilMoisture)
CALL Ops_ObsAction (Cx % Header % SoilTemp, &
                    'CX % SoilTemp',        &
                    ActionArg,              &
                    Real2 = CX % SoilTemp)
CALL Ops_ObsAction (Cx % Header % SeaIceMes_Var, &
                    'CX % SeaIceMes_Var',        &
                    ActionArg,                   &
                    Real1 = CX % SeaIceMes_Var)
CALL Ops_ObsAction (Cx % Header % SeaIceMes_HCor, &
                    'CX % SeaIceMes_HCor',        &
                    ActionArg,                    &
                    Real1 = CX % SeaIceMes_HCor)
CALL Ops_ObsAction (Cx % Header % SeaIceSyn_Var, &
                    'CX % SeaIceSyn_Var',        &
                    ActionArg,                   &
                    Real1 = CX % SeaIceSyn_Var)
CALL Ops_ObsAction (Cx % Header % SeaIceSyn_HCor, &
                    'CX % SeaIceSyn_HCor',        &
                    ActionArg,                    &
                    Real1 = CX % SeaIceSyn_HCor)
CALL Ops_ObsAction (Cx % Header % SeaIceOb_Err, &
                    'CX % SeaIceOb_Err',        &
                    ActionArg,                  &
                    Real1 = CX % SeaIceOb_Err)
CALL Ops_ObsAction (Cx % Header % CHLOb_Err,    &
                    'CX % CHLOb_Err',           &
                    ActionArg,                  &
                    Real1 = CX % CHLOb_Err)
CALL Ops_ObsAction (Cx % Header % KD490Ob_Err, &
                    'CX % KD490Ob_Err',        &
                    ActionArg,                 &
                    Real1 = CX % KD490Ob_Err)
CALL Ops_ObsAction (Cx % Header % SO2_AQ, &
                    'CX % SO2_AQ',        &
                    ActionArg,            &
                    Real1 = CX % SO2_AQ)
CALL Ops_ObsAction (Cx % Header % PM10_AQ, &
                    'CX % PM10_AQ',        &
                    ActionArg,             &
                    Real1 = CX % PM10_AQ)
CALL Ops_ObsAction (Cx % Header % PM2p5_AQ, &
                    'CX % PM2p5_AQ',        &
                    ActionArg,              &
                    Real1 = CX % PM2p5_AQ)
CALL Ops_ObsAction (Cx % Header % O3_AQ, &
                    'CX % O3_AQ',        &
                    ActionArg,           &
                    Real1 = CX % O3_AQ)
CALL Ops_ObsAction (Cx % Header % NO2_AQ, &
                    'CX % NO2_AQ',        &
                    ActionArg,            &
                    Real1 = CX % NO2_AQ)
CALL Ops_ObsAction (Cx % Header % CO_AQ, &
                    'CX % CO_AQ',        &
                    ActionArg,           &
                    Real1 = CX % CO_AQ)
CALL Ops_ObsAction (Cx % Header % BLtype, &
                    'CX % BLtype',        &
                    ActionArg,            &
                    Real1 = CX % BLtype)
CALL Ops_ObsAction (Cx % Header % cha, &
                    'CX % cha',        &
                    ActionArg,         &
                    Real1 = CX % cha)
CALL Ops_ObsAction (Cx % Header % mss, &
                    'CX % mss',        &
                    ActionArg,         &
                    Real1 = CX % mss)

IF (ActionArg == ActionDealloc) THEN
  IF (ASSOCIATED (Cx % dustp)) DEALLOCATE (Cx % dustp)
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CXGlobalAction
