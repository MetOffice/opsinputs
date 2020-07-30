!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Performs action on all array elements of BG.  Calls Ops_ObsAction for every
! array element.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ModelObGlobalAction (BG,  &
                                    sub)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE OpsMod_ObsInfo, ONLY: &
    Ops_ObsAction

IMPLICIT NONE

! Subroutine arguments:
TYPE (ModelOB_type)         :: BG   ! Model equivalent of OB

INTERFACE
SUBROUTINE sub (Hdr,    &
                Name,   &
                Real2,  &
                Real1,  &
                El1,    &
                El2,    &
                Int1,   &
                Int2,   &
                Coord1, &
                Coord2, &
                Char)

USE OpsMod_MiscTypes, ONLY: &
  ElementHeader_Type,       &
  Element_Type,             &
  Coord_Type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ElementHeader_Type)              :: Hdr
CHARACTER(len=*), INTENT(IN)           :: Name
REAL, POINTER, OPTIONAL                :: Real2(:,:)
REAL, POINTER, OPTIONAL                :: Real1(:)
TYPE (Element_Type), POINTER, OPTIONAL :: El1(:)
TYPE (Element_Type), POINTER, OPTIONAL :: El2(:,:)
INTEGER, POINTER, OPTIONAL             :: Int1(:)
INTEGER, POINTER, OPTIONAL             :: Int2(:,:)
TYPE (Coord_Type), POINTER, OPTIONAL   :: Coord1(:)
TYPE (Coord_Type), POINTER, OPTIONAL   :: Coord2(:,:)
CHARACTER(len=*), POINTER, OPTIONAL    :: Char(:)
END SUBROUTINE sub
END INTERFACE

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_ModelOBGlobalAction"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! Surface variables
CALL Ops_ObsAction (BG % Header % Pstandard, &
                    'BG % Pstandard',        &
                    Real1 = BG % Pstandard,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % Pstation, &
                    'BG % Pstation',        &
                    Real1 = BG % Pstation,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % Pmsl, &
                    'BG % Pmsl',        &
                    Real1 = BG % Pmsl,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % pstar, &
                    'BG % pstar',        &
                    Real1 = BG % pstar,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % t2, &
                    'BG % t2',        &
                    Real1 = BG % t2,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % td2, &
                    'BG % td2',        &
                    Real1 = BG % td2,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % rh2, &
                    'BG % rh2',        &
                    Real1 = BG % rh2,  &
                    sub = sub)
CALL Ops_ObsAction (BG % header % ddd10, &
                    'BG % ddd10',        &
                    Real1 = BG % ddd10,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % fff10, &
                    'BG % fff10',        &
                    Real1 = BG % fff10,  &
                    sub = sub)
CALL Ops_ObsAction (BG % header % u10, &
                    'BG % u10',        &
                    Real1 = BG % u10,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % v10, &
                    'BG % v10',        &
                    Real1 = BG % v10,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % vis, &
                    'BG % vis',        &
                    Real1 = BG % vis,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % logvis, &
                    'BG % logvis',        &
                    Real1 = BG % logvis,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % WindSpeed, &
                    'BG % WindSpeed',        &
                    Real1 = BG % WindSpeed,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % WaveHeight, &
                    'BG % WaveHeight',        &
                    Real1 = BG % WaveHeight,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % WAVE_HGHT, &
                    'BG % WAVE_HGHT',        &
                    Real1 = BG % WAVE_HGHT,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % WIND_SPED, &
                    'BG % WIND_SPED',        &
                    Real1 = BG % WIND_SPED,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SeaHeight, &
                    'BG % SeaHeight',        &
                    Real1 = BG % SeaHeight,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SST, &
                    'BG % SST',        &
                    Real1 = BG % SST,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % CHL, &
                    'BG % CHL',        &
                    Real1 = BG % CHL,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % KD490, &
                    'BG % KD490',        &
                    Real1 = BG % KD490,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % LCHL, &
                    'BG % LCHL',        &
                    Real1 = BG % LCHL,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % LKD490, &
                    'BG % LKD490',        &
                    Real1 = BG % LKD490,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % Tskin, &
                    'BG % Tskin',        &
                    Real1 = BG % Tskin,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % TCWV, &
                    'BG % TCWV',        &
                    Real1 = BG % TCWV,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % LWP, &
                    'BG % LWP',        &
                    Real1 = BG % LWP,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % RetLWP, &
                    'BG % RetLWP',        &
                    Real1 = BG % RetLWP,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % GPSTZDelay, &
                    'BG % GPSTZDelay',        &
                    Real1 = BG % GPSTZDelay,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % TotalCloudAmount, &
                    'BG % TotalCloudAmount',        &
                    Real1 = BG % TotalCloudAmount,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % u10AmbWind, &
                    'BG % u10AmbWind',        &
                    Real2 = BG % u10AmbWind,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % v10AmbWind, &
                    'BG % v10AmbWind',        &
                    Real2 = BG % v10AmbWind,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % AWPriorPcorrect, &
                    'BG % AWPriorPcorrect',        &
                    Real2 = BG % AWPriorPcorrect,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SeaSrfcHeight, &
                    'BG % SeaSrfcHeight',        &
                    Real1 = BG % SeaSrfcHeight,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SeaIce, &
                    'BG % SeaIce',        &
                    Real1 = BG % SeaIce,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % PrWx, &
                    'BG % PrWx',        &
                    Real1 = BG % PrWx,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % LwstClod, &
                    'BG % LwstClod',        &
                    Real1 = BG % LwstClod,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % LwstClodType, &
                    'BG % LwstClodType',        &
                    Real1 = BG % LwstClodType,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SSTMes_Var, &
                    'BG % SSTMes_Var',        &
                    Real1 = BG % SSTMes_Var,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SSTMes_HCor, &
                    'BG % SSTMes_HCor',        &
                    Real1 = BG % SSTMes_HCor,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SSTSyn_Var, &
                    'BG % SSTSyn_Var',        &
                    Real1 = BG % SSTSyn_Var,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SSTSyn_HCor, &
                    'BG % SSTSyn_HCor',        &
                    Real1 = BG % SSTSyn_HCor,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SSTOb_Err, &
                    'BG % SSTOb_Err',        &
                    Real1 = BG % SSTOb_Err,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SSHMes_Var, &
                    'BG % SSHMes_Var',        &
                    Real1 = BG % SSHMes_Var,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SSHMes_HCor, &
                    'BG % SSHMes_HCor',        &
                    Real1 = BG % SSHMes_HCor,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SSHSyn_Var, &
                    'BG % SSHSyn_Var',        &
                    Real1 = BG % SSHSyn_Var,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SSHSyn_HCor, &
                    'BG % SSHSyn_HCor',        &
                    Real1 = BG % SSHSyn_HCor,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SSHOb_Err, &
                    'BG % SSHOb_Err',        &
                    Real1 = BG % SSHOb_Err,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SeaIceMes_Var, &
                    'BG % SeaIceMes_Var',        &
                    Real1 = BG % SeaIceMes_Var,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SeaIceMes_HCor, &
                    'BG % SeaIceMes_HCor',        &
                    Real1 = BG % SeaIceMes_HCor,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SeaIceSyn_Var, &
                    'BG % SeaIceSyn_Var',        &
                    Real1 = BG % SeaIceSyn_Var,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SeaIceSyn_HCor, &
                    'BG % SeaIceSyn_HCor',        &
                    Real1 = BG % SeaIceSyn_HCor,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SeaIceOb_Err, &
                    'BG % SeaIceOb_Err',        &
                    Real1 = BG % SeaIceOb_Err,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % CHLOb_Err, &
                    'BG % CHLOb_Err',        &
                    Real1 = BG % CHLOb_Err,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % KD490Ob_Err, &
                    'BG % KD490Ob_Err',        &
                    Real1 = BG % KD490Ob_Err,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % LCHLOb_Err, &
                    'BG % LCHLOb_Err',        &
                    Real1 = BG % LCHLOb_Err,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % LKD490Ob_Err, &
                    'BG % LKD490Ob_Err',        &
                    Real1 = BG % LKD490Ob_Err,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SSTCnfd, &
                    'BG % SSTCnfd',        &
                    Int1 = BG % SSTCnfd,   &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % Reflectivity, &
                    'BG % Reflectivity',        &
                    Real2 = BG % Reflectivity,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % ReflectivityR, &
                    'BG % ReflectivityR',        &
                    Real2 = BG % ReflectivityR,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % ReflectivityI, &
                    'BG % ReflectivityI',        &
                    Real2 = BG % ReflectivityI,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % RadialVelocity, &
                    'BG % RadialVelocity',        &
                    Real2 = BG % RadialVelocity,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % RadRefractivity, &
                    'BG % RadRefractivity',        &
                    Real2 = BG % RadRefractivity,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SnowAmount, &
                    'BG % SnowAmount',        &
                    Real1 = BG % SnowAmount,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % PrecipAcc6hr, &
                    'BG % PrecipAcc6hr',        &
                    Real1 = BG % PrecipAcc6hr,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % BLheight, &
                    'BG % BLheight',        &
                    Real1 = BG % BLheight,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % ObukhovLength, &
                    'BG % ObukhovLength',        &
                    Real1 = BG % ObukhovLength,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % FrictionVel, &
                    'BG % FrictionVel',        &
                    Real1 = BG % FrictionVel,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SWradiation, &
                    'BG % SWradiation',        &
                    Real1 = BG % SWradiation,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % LowCloudAmount, &
                    'BG % LowCloudAmount',        &
                    Real1 = BG % LowCloudAmount,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % MedCloudAmount, &
                    'BG % MedCloudAmount',        &
                    Real1 = BG % MedCloudAmount,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % LowCloudBase, &
                    'BG % LowCloudBase',        &
                    Real1 = BG % LowCloudBase,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SO2_AQ, &
                    'BG % SO2_AQ',        &
                    Real1 = BG % SO2_AQ,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % PM10_AQ, &
                    'BG % PM10_AQ',        &
                    Real1 = BG % PM10_AQ,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % PM2p5_AQ, &
                    'BG % PM2p5_AQ',        &
                    Real1 = BG % PM2p5_AQ,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % O3_AQ, &
                    'BG % O3_AQ',        &
                    Real1 = BG % O3_AQ,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % NO2_AQ, &
                    'BG % NO2_AQ',        &
                    Real1 = BG % NO2_AQ,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % CO_AQ, &
                    'BG % CO_AQ',        &
                    Real1 = BG % CO_AQ,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % MASS_DNSTY, &
                    'BG % MASS_DNSTY',        &
                    Real1 = BG % MASS_DNSTY,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SO2_DNSTY, &
                    'BG % SO2_DNSTY',        &
                    Real1 = BG % SO2_DNSTY,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % PM10_DNSTY, &
                    'BG % PM10_DNSTY',        &
                    Real1 = BG % PM10_DNSTY,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % PM2p5_DNSTY, &
                    'BG % PM2p5_DNSTY',        &
                    Real1 = BG % PM2p5_DNSTY,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % O3_DNSTY, &
                    'BG % O3_DNSTY',        &
                    Real1 = BG % O3_DNSTY,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % NO2_DNSTY, &
                    'BG % NO2_DNSTY',        &
                    Real1 = BG % NO2_DNSTY,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % CO_DNSTY, &
                    'BG % CO_DNSTY',        &
                    Real1 = BG % CO_DNSTY,  &
                    sub = sub)

! upper-level variables
CALL Ops_ObsAction (BG % Header % z, &
                    'BG % z',        &
                    Real2 = BG % z,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % t, &
                    'BG % t',        &
                    Real2 = BG % t,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % td, &
                    'BG % td',        &
                    Real2 = BG % td,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % theta, &
                    'BG % theta',        &
                    Real2 = BG % theta,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % rh, &
                    'BG % rh',        &
                    Real2 = BG % rh,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % q, &
                    'BG % q',        &
                    Real2 = BG % q,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % u, &
                    'BG % u',        &
                    Real2 = BG % u,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % v, &
                    'BG % v',        &
                    Real2 = BG % v,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % w, &
                    'BG % w',        &
                    Real2 = BG % w,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % ddd, &
                    'BG % ddd',        &
                    Real2 = BG % ddd,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % fff, &
                    'BG % fff',        &
                    Real2 = BG % fff,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % Salt, &
                    'BG % Salt',        &
                    Real2 = BG % Salt,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % BriTemp, &
                    'BG % BriTemp',        &
                    Real2 = BG % BriTemp,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % Emissivity, &
                    'BG % Emissivity',        &
                    Real2 = BG % Emissivity,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % CLW, &
                    'BG % CLW',        &
                    Real2 = BG % CLW,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % CIW, &
                    'BG % CIW',        &
                    Real2 = BG % CIW,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % Rainamount, &
                    'BG % Rainamount',        &
                    Real2 = BG % Rainamount,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % Rainrate, &
                    'BG % Rainrate',        &
                    Real2 = BG % Rainrate,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % Snowrate, &
                    'BG % Snowrate',        &
                    Real2 = BG % Snowrate,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % Cloud, &
                    'BG % Cloud',        &
                    Real2 = BG % Cloud,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % ozone, &
                    'BG % ozone',        &
                    Real2 = BG % ozone,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % Refrac, &
                    'BG % Refrac',        &
                    Real2 = BG % Refrac,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % BendingAngle, &
                    'BG % BendingAngle',        &
                    Real2 = BG % BendingAngle,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % BendingAngleOrig, &
                    'BG % BendingAngleOrig',        &
                    Real2 = BG % BendingAngleOrig,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % ImpactParam, &
                    'BG % ImpactParam',        &
                    Real2 = BG % ImpactParam,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % BendingAngleAll, &
                    'BG % BendingAngleAll',        &
                    Real2 = BG % BendingAngleAll,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % ImpactParamAll, &
                    'BG % ImpactParamAll',        &
                    Real2 = BG % ImpactParamAll,  &
                    sub = sub)

CALL Ops_ObsAction (BG % Header % RO_Rad_Curv, &
                    'BG % RO_Rad_Curv',        &
                    Real1 = BG % RO_Rad_Curv,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % RO_geoid_und, &
                    'BG % RO_geoid_und',        &
                    Real1 = BG % RO_geoid_und,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % p, &
                    'BG % p',        &
                    Real2 = BG % p,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % TempMes_Var, &
                    'BG % TempMes_Var',        &
                    Real2 = BG % TempMes_Var,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % TempMes_HCor, &
                    'BG % TempMes_HCor',        &
                    Real2 = BG % TempMes_HCor,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % TempSyn_Var, &
                    'BG % TempSyn_Var',        &
                    Real2 = BG % TempSyn_Var,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % TempSyn_HCor, &
                    'BG % TempSyn_HCor',        &
                    Real2 = BG % TempSyn_HCor,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % TempOb_Err, &
                    'BG % TempOb_Err',        &
                    Real2 = BG % TempOb_Err,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SalMes_Var, &
                    'BG % SalMes_Var',        &
                    Real2 = BG % SalMes_Var,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SalMes_HCor, &
                    'BG % SalMes_HCor',        &
                    Real2 = BG % SalMes_HCor,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SalSyn_Var, &
                    'BG % SalSyn_Var',        &
                    Real2 = BG % SalSyn_Var,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SalSyn_HCor, &
                    'BG % SalSyn_HCor',        &
                    Real2 = BG % SalSyn_HCor,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SalOb_Err, &
                    'BG % SalOb_Err',        &
                    Real2 = BG % SalOb_Err,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SBUVOzone, &
                    'BG % SBUVOzone',        &
                    Real2 = BG % SBUVOzone,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % RichNumber, &
                    'BG % RichNumber',        &
                    Real2 = BG % RichNumber,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SoilMoisture, &
                    'BG % SoilMoisture',        &
                    Real2 = BG % SoilMoisture,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % SoilTemp, &
                    'BG % SoilTemp',        &
                    Real2 = BG % SoilTemp,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % AOD, &
                    'BG % AOD',        &
                    Real2 = BG % AOD,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % RainAccum, &
                    'BG % RainAccum',        &
                    Real1 = BG % RainAccum,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % CeilBackscatter, &
                    'BG % CeilBackscatter',        &
                    Real2 = BG % CeilBackscatter,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % CeilRange, &
                    'BG % CeilRange',        &
                    Real2 = BG % CeilRange,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % aerosol, &
                    'BG % aerosol',        &
                    Real2 = BG % aerosol,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % CDNC, &
                    'BG % CDNC',        &
                    Real2 = BG % CDNC,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % RH_AMC, &
                    'BG % RH_AMC',        &
                    Real2 = BG % RH_AMC,  &
                    sub = sub)
CALL Ops_ObsAction (BG % Header % HLOSwind, &
                    'BG % HLOSwind',        &
                    Real1 = BG % HLOSwind,  &
                    sub = sub)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ModelObGlobalAction
