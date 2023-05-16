!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Allocate space for ModelOB (Model Ob equivalent) using OB header as input to
! determine which fields are likely to be required; copy header and initialise
! to zero.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ModelOBAllocate (OBHeader, &
                                BG)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE OpsMod_ObsInfo, ONLY: &
    OBHeader_type,        &
    Ops_ObsActionInit,    &
    Ops_Alloc

IMPLICIT NONE

! Subroutine arguments:
TYPE (OBHeader_type), INTENT(IN)   :: OBHeader
TYPE (ModelOB_type), INTENT(INOUT) :: BG       ! Model equivalent of OB

! Local declarations:
CHARACTER(len=*), PARAMETER        :: RoutineName = "Ops_ModelOBAllocate"
INTEGER                            :: NObs

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! This routine need not exist: it should be straightforward for allocation to
! occur within the routines that actually fill in the data...

!  Section 1.0: Setup header
!----------------------------

BG % Header % ValidityTime = OBHeader % ValidityTime
BG % Header % NumObsLocal = OBHeader % NumObsLocal
BG % Header % NumObsTotal = OBHeader % NumObsTotal
BG % Header % ObsGroup = OBHeader % ObsGroup
BG % Header % ModelRelease = 0

!  Section 2.0:  Allocate space for each surface variable required
!------------------------------------------------------------------

Nobs = OBHeader % NumObsLocal

! Initialise
CALL Ops_ModelObGlobalAction (BG,                &
                              Ops_ObsActionInit)

CALL Ops_Alloc (BG % Header % Pstandard,      &
                'BG % Pstandard',             &
                NObs,                         &
                HdrIn = OBHeader % Pstandard, &
                Real1 = BG % Pstandard)

CALL Ops_Alloc (BG % Header % Pstation,      &
                'BG % Pstation',             &
                NObs,                        &
                HdrIn = OBHeader % Pstation, &
                Real1 = BG % Pstation)

CALL Ops_Alloc (BG % Header % Pmsl,      &
                'BG % Pmsl',             &
                NObs,                    &
                HdrIn = OBHeader % Pmsl, &
                Real1 = BG % Pmsl)

CALL Ops_Alloc (BG % Header % Pstar,      &
                'BG % Pstar',             &
                NObs,                     &
                HdrIn = OBHeader % Pstar, &
                Real1 = BG % Pstar)

CALL Ops_Alloc (BG % Header % t2,      &
                'BG % t2',             &
                NObs,                  &
                HdrIn = OBHeader % t2, &
                Real1 = BG % t2)

CALL Ops_Alloc (BG % Header % td2,      &
                'BG % td2',             &
                NObs,                   &
                HdrIn = OBHeader % td2, &
                Real1 = BG % td2)

CALL Ops_Alloc (BG % Header % rh2,      &
                'BG % rh2',             &
                NObs,                   &
                HdrIn = OBHeader % rh2, &
                Real1 = BG % rh2)

CALL Ops_Alloc (BG % Header % ddd10,      &
                'BG % ddd10',             &
                NObs,                     &
                HdrIn = OBHeader % ddd10, &
                Real1 = BG % ddd10)

CALL Ops_Alloc (BG % Header % fff10,      &
                'BG % fff10',             &
                NObs,                     &
                HdrIn = OBHeader % fff10, &
                Real1 = BG % fff10)

CALL Ops_Alloc (BG % Header % u10,      &
                'BG % u10',             &
                NObs,                   &
                HdrIn = OBHeader % u10, &
                Real1 = BG % u10)

CALL Ops_Alloc (BG % Header % v10,      &
                'BG % v10',             &
                NObs,                   &
                HdrIn = OBHeader % v10, &
                Real1 = BG % v10)

CALL Ops_Alloc (BG % Header % vis,      &
                'BG % vis',             &
                NObs,                   &
                HdrIn = OBHeader % vis, &
                Real1 = BG % vis)

CALL Ops_Alloc (BG % Header % logvis,      &
                'BG % logvis',             &
                NObs,                      &
                HdrIn = OBHeader % logvis, &
                Real1 = BG % logvis)

CALL Ops_Alloc (BG % Header % WindSpeed,      &
                'BG % WindSpeed',             &
                NObs,                         &
                HdrIn = OBHeader % WindSpeed, &
                Real1 = BG % WindSpeed)

CALL Ops_Alloc (BG % Header % WaveHeight,      &
                'BG % WaveHeight',             &
                NObs,                          &
                HdrIn = OBHeader % WaveHeight, &
                Real1 = BG % WaveHeight)

CALL Ops_Alloc (BG % Header % WAVE_HGHT,      &
                'BG % WAVE_HGHT',             &
                NObs,                         &
                HdrIn = OBHeader % WAVE_HGHT, &
                Real1 = BG % WAVE_HGHT)

CALL Ops_Alloc (BG % Header % WIND_SPED,      &
                'BG % WIND_SPED',             &
                NObs,                         &
                HdrIn = OBHeader % WIND_SPED, &
                Real1 = BG % WIND_SPED)

CALL Ops_Alloc (BG % Header % SeaHeight,      &
                'BG % SeaHeight',             &
                NObs,                         &
                HdrIn = OBHeader % SeaHeight, &
                Real1 = BG % SeaHeight)

CALL Ops_Alloc (BG % Header % SST,      &
                'BG % SST',             &
                NObs,                   &
                HdrIn = OBHeader % SST, &
                Real1 = BG % SST)

CALL Ops_Alloc (BG % Header % Tskin,      &
                'BG % Tskin',             &
                NObs,                     &
                HdrIn = OBHeader % Tskin, &
                Real1 = BG % Tskin)

CALL Ops_Alloc (BG % Header % TCWV,      &
                'BG % TCWV',             &
                NObs,                    &
                HdrIn = OBHeader % TCWV, &
                Real1 = BG % TCWV)

CALL Ops_Alloc (BG % Header % LWP,      &
                'BG % LWP',             &
                NObs,                   &
                HdrIn = OBHeader % LWP, &
                Real1 = BG % LWP)

CALL Ops_Alloc (BG % Header % RetLWP,      &
                'BG % RetLWP',             &
                NObs,                      &
                HdrIn = OBHeader % RetLWP, &
                Real1 = BG % RetLWP)

CALL Ops_Alloc (BG % Header % GPSTZDelay,      &
                'BG % GPSTZDelay',             &
                NObs,                          &
                HdrIn = OBHeader % GPSTZDelay, &
                Real1 = BG % GPSTZDelay)

CALL Ops_Alloc (BG % Header % TotalCloudAmount,      &
                'BG % TotalCloudAmount',             &
                NObs,                                &
                HdrIn = OBHeader % TotalCloudAmount, &
                Real1 = BG % TotalCloudAmount)

CALL Ops_Alloc (BG % Header % u10AmbWind,      &
                'BG % u10AmbWind',             &
                NObs,                          &
                HdrIn = OBHeader % u10AmbWind, &
                Real2 = BG % u10AmbWind)

CALL Ops_Alloc (BG % Header % v10AmbWind,      &
                'BG % v10AmbWind',             &
                NObs,                          &
                HdrIn = OBHeader % v10AmbWind, &
                Real2 = BG % v10AmbWind)

CALL Ops_Alloc (BG % Header % AWPriorPcorrect,      &
                'BG % AWPriorPcorrect',             &
                NObs,                               &
                HdrIn = OBHeader % AWPriorPcorrect, &
                Real2 = BG % AWPriorPcorrect)

CALL Ops_Alloc (BG % Header % SeaSrfcHeight,      &
                'BG % SeaSrfcHeight',             &
                NObs,                             &
                HdrIn = OBHeader % SeaSrfcHeight, &
                Real1 = BG % SeaSrfcHeight)

CALL Ops_Alloc (BG % Header % SeaIce,      &
                'BG % SeaIce',             &
                NObs,                      &
                HdrIn = OBHeader % SeaIce, &
                Real1 = BG % SeaIce)

CALL Ops_Alloc (BG % Header % CHL,      &
                'BG % CHL',             &
                NObs,                      &
                HdrIn = OBHeader % CHL, &
                Real1 = BG % CHL)

CALL Ops_Alloc (BG % Header % KD490,      &
                'BG % KD490',             &
                NObs,                      &
                HdrIn = OBHeader % KD490, &
                Real1 = BG % KD490)

CALL Ops_Alloc (BG % Header % LCHL,      &
                'BG % LCHL',             &
                NObs,                      &
                HdrIn = OBHeader % CHL, &
                Real1 = BG % LCHL)

CALL Ops_Alloc (BG % Header % LKD490,      &
                'BG % LKD490',             &
                NObs,                      &
                HdrIn = OBHeader % KD490, &
                Real1 = BG % LKD490)

CALL Ops_Alloc (BG % Header % MASS_DNSTY,      &
                'BG % MASS_DNSTY',             &
                NObs,                          &
                HdrIn = OBHeader % MASS_DNSTY, &
                Real1 = BG % MASS_DNSTY)

CALL Ops_Alloc (BG % Header % SO2_DNSTY,      &
                'BG % SO2_DNSTY',             &
                NObs,                         &
                HdrIn = OBHeader % SO2_DNSTY, &
                Real1 = BG % SO2_DNSTY)

CALL Ops_Alloc (BG % Header % PM10_DNSTY,      &
                'BG % PM10_DNSTY',             &
                NObs,                          &
                HdrIn = OBHeader % PM10_DNSTY, &
                Real1 = BG % PM10_DNSTY)

CALL Ops_Alloc (BG % Header % PM2p5_DNSTY,      &
                'BG % PM2p5_DNSTY',             &
                NObs,                           &
                HdrIn = OBHeader % PM2p5_DNSTY, &
                Real1 = BG % PM2p5_DNSTY)

CALL Ops_Alloc (BG % Header % O3_DNSTY,      &
                'BG % O3_DNSTY',             &
                NObs,                        &
                HdrIn = OBHeader % O3_DNSTY, &
                Real1 = BG % O3_DNSTY)

CALL Ops_Alloc (BG % Header % NO2_DNSTY,      &
                'BG % NO2_DNSTY',             &
                NObs,                         &
                HdrIn = OBHeader % NO2_DNSTY, &
                Real1 = BG % NO2_DNSTY)

CALL Ops_Alloc (BG % Header % CO_DNSTY,      &
                'BG % CO_DNSTY',             &
                NObs,                        &
                HdrIn = OBHeader % CO_DNSTY, &
                Real1 = BG % CO_DNSTY)

CALL Ops_Alloc (BG % Header % LwstClodType,      &
                'BG % LwstClodType',             &
                NObs,                            &
                HdrIn = OBHeader % LwstClodType, &
                Real1 = BG % LwstClodType)

CALL Ops_Alloc (BG % Header % LwstClod,      &
                'BG % LwstClod',             &
                NObs,                        &
                HdrIn = OBHeader % LwstClod, &
                Real1 = BG % LwstClod)

!  Section 3.0:  Allocate space for each multi level variable required
!----------------------------------------------------------------------

CALL Ops_Alloc (BG % Header % z,      &
                'BG % z',             &
                NObs,                 &
                HdrIn = OBHeader % z, &
                Real2 = BG % z)

CALL Ops_Alloc (BG % Header % t,      &
                'BG % t',             &
                NObs,                 &
                HdrIn = OBHeader % t, &
                Real2 = BG % t)

CALL Ops_Alloc (BG % Header % td,      &
                'BG % td',             &
                NObs,                  &
                HdrIn = OBHeader % td, &
                Real2 = BG % td)

CALL Ops_Alloc (BG % Header % theta,      &
                'BG % theta',             &
                NObs,                     &
                HdrIn = OBHeader % theta, &
                Real2 = BG % theta)

CALL Ops_Alloc (BG % Header % rh,      &
                'BG % rh',             &
                NObs,                  &
                HdrIn = OBHeader % rh, &
                Real2 = BG % rh)

CALL Ops_Alloc (BG % Header % u,      &
                'BG % u',             &
                NObs,                 &
                HdrIn = OBHeader % u, &
                Real2 = BG % u)

CALL Ops_Alloc (BG % Header % v,      &
                'BG % v',             &
                NObs,                 &
                HdrIn = OBHeader % v, &
                Real2 = BG % v)

CALL Ops_Alloc (BG % Header % w,      &
                'BG % w',             &
                NObs,                 &
                HdrIn = OBHeader % w, &
                Real2 = BG % w)

CALL Ops_Alloc (BG % Header % ddd,      &
                'BG % ddd',             &
                NObs,                   &
                HdrIn = OBHeader % ddd, &
                Real2 = BG % ddd)

CALL Ops_Alloc (BG % Header % fff,      &
                'BG % fff',             &
                NObs,                   &
                HdrIn = OBHeader % fff, &
                Real2 = BG % fff)

CALL Ops_Alloc (BG % Header % Salt,      &
                'BG % Salt',             &
                NObs,                    &
                HdrIn = OBHeader % Salt, &
                Real2 = BG % Salt)

CALL Ops_Alloc (BG % Header % BriTemp,      &
                'BG % BriTemp',             &
                NObs,                       &
                HdrIn = OBHeader % BriTemp, &
                Real2 = BG % BriTemp)

CALL Ops_Alloc (BG % Header % Emissivity,   &
                'BG % Emissivity',          &
                NObs,                       &
                HdrIn = ObHeader % BriTemp, &
                Real2 = BG % Emissivity)

CALL Ops_Alloc (BG % Header % q,      &
                'BG % q',             &
                NObs,                 &
                HdrIn = OBHeader % q, &
                Real2 = BG % q)

CALL Ops_Alloc (BG % Header % CLW,      &
                'BG % CLW',             &
                NObs,                   &
                HdrIn = OBHeader % CLW, &
                Real2 = BG % CLW)

CALL Ops_Alloc (BG % Header % CIW,      &
                'BG % CIW',             &
                NObs,                   &
                HdrIn = OBHeader % CIW, &
                Real2 = BG % CIW)

CALL Ops_Alloc (BG % Header % Rainamount,      &
                'BG % Rainamount',             &
                NObs,                          &
                HdrIn = OBHeader % Rainamount, &
                Real2 = BG % Rainamount)

CALL Ops_Alloc (BG % Header % Cloud,      &
                'BG % Cloud',             &
                NObs,                     &
                HdrIn = OBHeader % Cloud, &
                Real2 = BG % Cloud)

CALL Ops_Alloc (BG % Header % Rainrate,      &
                'BG % Rainrate',             &
                NObs,                        &
                HdrIn = OBHeader % Rainrate, &
                Real2 = BG % Rainrate)

CALL Ops_Alloc (BG % Header % Snowrate,      &
                'BG % Snowrate',             &
                NObs,                        &
                HdrIn = OBHeader % Snowrate, &
                Real2 = BG % Snowrate)

CALL Ops_Alloc (BG % Header % ozone,      &
                'BG % ozone',             &
                NObs,                     &
                HdrIn = OBHeader % ozone, &
                Real2 = BG % ozone)

CALL Ops_Alloc (BG % Header % Refrac,      &
                'BG % Refrac',             &
                NObs,                      &
                HdrIn = OBHeader % Refrac, &
                Real2 = BG % Refrac)

CALL Ops_Alloc (BG % Header % BendingAngle,      &
                'BG % BendingAngle',             &
                NObs,                            &
                HdrIn = OBHeader % BendingAngle, &
                Real2 = BG % BendingAngle)

CALL Ops_Alloc (BG % Header % ImpactParam,      &
                'BG % ImpactParam',             &
                NObs,                           &
                HdrIn = OBHeader % ImpactParam, &
                Real2 = BG % ImpactParam)

CALL Ops_Alloc (BG % Header % BendingAngleAll,      &
                'BG % BendingAngleAll',             &
                NObs,                               &
                HdrIn = OBHeader % BendingAngleAll, &
                Real2 = BG % BendingAngleAll)

CALL Ops_Alloc (BG % Header % ImpactParamAll,      &
                'BG % ImpactParamAll',             &
                NObs,                              &
                HdrIn = OBHeader % ImpactParamAll, &
                Real2 = BG % ImpactParamAll)

CALL Ops_Alloc (BG % Header % RO_Rad_Curv,      &
                'BG % RO_Rad_Curv',             &
                NObs,                           &
                HdrIn = OBHeader % RO_Rad_Curv, &
                Real1 = BG % RO_Rad_Curv)

CALL Ops_Alloc (BG % Header % RO_geoid_und,      &
                'BG % RO_geoid_und',             &
                NObs,                            &
                HdrIn = OBHeader % RO_geoid_und, &
                Real1 = BG % RO_geoid_und)

CALL Ops_Alloc (BG % Header % p,      &
                'BG % p',             &
                NObs,                 &
                HdrIn = OBHeader % p, &
                Real2 = BG % p)

CALL Ops_Alloc (BG % Header % SBUVozone,      &
                'BG % SBUVozone',             &
                NObs,                         &
                HdrIn = OBHeader % SBUVozone, &
                Real2 = BG % SBUVozone)

CALL Ops_Alloc (BG % Header % RadialVelocity,      &
                'BG % RadialVelocity',             &
                NObs,                              &
                HdrIn = OBHeader % RadialVelocity, &
                Real2 = BG % RadialVelocity)

CALL Ops_Alloc (BG % Header % RadRefractivity,      &
                'BG % RadRefractivity',             &
                NObs,                               &
                HdrIn = OBHeader % RadRefractivity, &
                Real2 = BG % RadRefractivity)

CALL Ops_Alloc (BG % Header % Reflectivity,      &
                'BG % Reflectivity',             &
                NObs,                            &
                HdrIn = OBHeader % Reflectivity, &
                Real2 = BG % Reflectivity)

CALL Ops_Alloc (BG % Header % ReflectivityR,     &
                'BG % ReflectivityR',            &
                NObs,                            &
                HdrIn = OBHeader % ReflectivityR,&
                Real2 = BG % ReflectivityR)

CALL Ops_Alloc (BG % Header % ReflectivityI,      &
                'BG % ReflectivityI',             &
                NObs,                             &
                HdrIn = OBHeader % ReflectivityI, &
                Real2 = BG % ReflectivityI)

CALL Ops_Alloc (BG % Header % AOD,      &
                'BG % AOD',             &
                NObs,                   &
                HdrIn = OBHeader % AOD, &
                Real2 = BG % AOD)

CALL Ops_Alloc (BG % Header % RainAccum,      &
                'BG % RainAccum',             &
                NObs,                         &
                HdrIn = OBHeader % RainAccum, &
                Real1 = BG % RainAccum)

CALL Ops_Alloc (BG % Header % CeilBackscatter,      &
                'BG % CeilBackscatter',             &
                NObs,                               &
                HdrIn = OBHeader % CeilBackscatter, &
                Real2 = BG % CeilBackscatter)

CALL Ops_Alloc (BG % Header % CeilRange,      &
                'BG % CeilRange',             &
                NObs,                         &
                HdrIn = OBHeader % CeilRange, &
                Real2 = BG % CeilRange)

CALL Ops_Alloc (BG % Header % HLOSwind,      &
                'BG % HLOSwind',             &
                NObs,                        &
                HdrIn = OBHeader % HLOSwind, &
                Real1 = BG % HLOSwind)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ModelOBAllocate
