!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! This module contains:
!
! a) The modelob_type definition
!
! Notes on updating this structure:
!
! 1) Make sure that for every body element there is a corresponding header element
! 2) Check the following routines to see if they need updating:
!     Ops_ModelObGlobalAction
!     Ops_ModelOBAllocate
!     Ops_ModelObToOb
!     Ops_BGVertInterp
!     Ops_BGVEVertInterp
!-------------------------------------------------------------------------------

MODULE OpsMod_ModelObInfo

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE OpsMod_DateTime, ONLY: &
  DateTime_type

USE OpsMod_MiscTypes, ONLY: &
  ElementHeader_type,       &
  Element_Type,             &
  ActionPrint1,             &
  ActionPrint2,             &
  ActionInit,               &
  ActionDeAlloc,            &
  ObPrintIndex

IMPLICIT NONE

SAVE

! Public declarations:

!-------------------------------------------------------------------
! ModelOB Header data types used in ModelOB_type
!------------------------------------------------
! There is a one to one correspondence between the items
! in ModelOBheader and ModelOB
! There is a one to one correspondence between the items
! in ModelOBheader and OBheader except for those variables in ObHeader
! described as MetaData
!
TYPE ModelOBheader_type
  TYPE(DateTime_type)       :: ValidityTime ! Validity time of this file
  INTEGER                   :: ModelRelease = IMDI ! UM release number (eg 401 for 4.1)
  INTEGER                   :: NumObsTotal = IMDI  ! Number of Obs in total
  INTEGER                   :: NumObsLocal = IMDI  ! Number of Obs locally
  INTEGER                   :: ObsGroup = IMDI     ! Code for group of Obs TYPEs
  ! Surface variables
  TYPE (ElementHeader_type) :: Pstandard    ! DPI for Pstandard
  TYPE (ElementHeader_type) :: Pstation     ! DPI for Pstation
  TYPE (ElementHeader_type) :: Pmsl         ! DPI for Pmsl
  TYPE (ElementHeader_type) :: pstar        ! DPI for pstar
  TYPE (ElementHeader_type) :: t2           ! DPI for surface temperature
  TYPE (ElementHeader_type) :: td2          ! DPI for td2
  TYPE (ElementHeader_type) :: rh2          ! DPI for surface humidity
  TYPE (ElementHeader_type) :: ddd10        ! DPI for ddd & fff 10
  TYPE (ElementHeader_type) :: fff10        ! DPI for ddd & fff 10
  TYPE (ElementHeader_type) :: u10          ! DPI for surface wind
  TYPE (ElementHeader_type) :: v10          ! DPI for surface wind
  TYPE (ElementHeader_type) :: vis          ! DPI for vis
  TYPE (ElementHeader_type) :: logvis       ! DPI for surface visibility
  TYPE (ElementHeader_type) :: WindSpeed    ! DPI for WindSpeed
  TYPE (ElementHeader_type) :: WaveHeight   ! DPI for WaveHeight
  TYPE (ElementHeader_type) :: WAVE_HGHT    ! DPI for WaveHeight
  TYPE (ElementHeader_type) :: WIND_SPED    ! DPI for Surface windspeed
  TYPE (ElementHeader_type) :: SeaHeight    ! DPI for Height of Sea surface
  TYPE (ElementHeader_type) :: SST          ! DPI for Sea Surface Temperature
  TYPE (ElementHeader_type) :: Tskin        ! DPI for Surface radiative temp
  TYPE (ElementHeader_type) :: TCWV         ! DPI for Total Column Water Vapour
  TYPE (ElementHeader_type) :: MASS_DNSTY   ! DPI for Mass Density
  TYPE (ElementHeader_type) :: SO2_DNSTY    ! DPI for Mass Density
  TYPE (ElementHeader_type) :: PM10_DNSTY   ! DPI for Mass Density
  TYPE (ElementHeader_type) :: PM2p5_DNSTY  ! DPI for Mass Density
  TYPE (ElementHeader_type) :: O3_DNSTY     ! DPI for Mass Density
  TYPE (ElementHeader_type) :: NO2_DNSTY    ! DPI for Mass Density
  TYPE (ElementHeader_type) :: CO_DNSTY     ! DPI for Mass Density
  TYPE (ElementHeader_type) :: LWP          ! DPI for Liquid Water Path
  TYPE (ElementHeader_type) :: RetLWP       ! DPI for retrieved Liquid Water Path from Bg Tb
  TYPE (ElementHeader_type) :: TotalCloudAmount  ! DPI for Total Cloud Amount
  TYPE (ElementHeader_type) :: u10AmbWind   ! DPI for scat ambiguous winds
  TYPE (ElementHeader_type) :: v10AmbWind   ! DPI for scat ambiguous winds
  TYPE (ElementHeader_type) :: AWPriorPcorrect ! DPI for scat prior prob
  TYPE (ElementHeader_type) :: PrWx         ! DPI for Present Weather
  TYPE (ElementHeader_type) :: LwstClod     ! DPI for Low cloud amount
  TYPE (ElementHeader_type) :: LwstClodType ! DPI for Low cloud type
  TYPE (ElementHeader_type) :: SeaSrfcHeight ! DPI for Altimeter SSH
  TYPE (ElementHeader_type) :: SnowAmount   ! DPI for snow cover
  TYPE (ElementHeader_type) :: BLheight     ! DPI for boundary layer height
  TYPE (ElementHeader_type) :: ObukhovLength ! DPI for Obukhov length
  TYPE (ElementHeader_type) :: FrictionVel   ! DPI for friction velocity
  TYPE (ElementHeader_type) :: SWradiation  ! DPI for downward SW sfc radiation
  TYPE (ElementHeader_type) :: PrecipAcc6hr ! DPI for 6 hrly precip accumulation
  TYPE (ElementHeader_type) :: LowCloudAmount ! DPI for low cloud amount
  TYPE (ElementHeader_type) :: MedCloudAmount ! DPI for medium cloud amount
  TYPE (ElementHeader_type) :: LowCloudBase ! DPI for low cloud base
  TYPE (ElementHeader_type) :: SeaIce       ! DPI for Sea ice
  TYPE (ElementHeader_type) :: CHL           ! DPI for ocean colour
  TYPE (ElementHeader_type) :: KD490         ! DPI for ocean colour
  TYPE (ElementHeader_type) :: LCHL           ! DPI for ocean colour
  TYPE (ElementHeader_type) :: LKD490         ! DPI for ocean colour

  ! following two are used for AATSR
  TYPE (ElementHeader_type) :: SSTCnfd       ! SST confidence

  TYPE (ElementHeader_type) :: SSTMes_Var   ! DPI for mesoscale SST variance
  TYPE (ElementHeader_type) :: SSTMes_HCor  ! DPI for mesoscale SST horizontal
                                            ! correlation
  TYPE (ElementHeader_type) :: SSTSyn_Var   ! DPI for synoptic SST variance
  TYPE (ElementHeader_type) :: SSTSyn_HCor  ! DPI for synoptic SST horizontal
                                            ! correlation
  TYPE (ElementHeader_type) :: SSTOb_Err    ! DPI for observation errors
  TYPE (ElementHeader_type) :: SSHMes_Var   ! DPI for mesoscale SSH variance
  TYPE (ElementHeader_type) :: SSHMes_HCor  ! DPI for mesoscale SSH horizontal
                                            ! correlation
  TYPE (ElementHeader_type) :: SSHSyn_Var   ! DPI for synoptic SSH variance
  TYPE (ElementHeader_type) :: SSHSyn_HCor  ! DPI for synoptic SSH horizontal
                                            ! correlation
  TYPE (ElementHeader_type) :: SSHOb_Err    ! DPI for observation errors

  TYPE (ElementHeader_type) :: SeaIceMes_Var   ! DPI for mesoscale Sea ice variance
  TYPE (ElementHeader_type) :: SeaIceMes_HCor  ! DPI for mesoscale Sea ice horizontal
                                            ! correlation
  TYPE (ElementHeader_type) :: SeaIceSyn_Var   ! DPI for synoptic Sea ice variance
  TYPE (ElementHeader_type) :: SeaIceSyn_HCor  ! DPI for synoptic Sea ice horizontal
                                            ! correlation
  TYPE (ElementHeader_type) :: SeaIceOb_Err    ! DPI for observation errors
  TYPE (ElementHeader_type) :: CHLOb_Err       ! DPI for observation errors
  TYPE (ElementHeader_type) :: KD490Ob_Err     ! DPI for observation errors
  TYPE (ElementHeader_type) :: LCHLOb_Err      ! DPI for observation errors
  TYPE (ElementHeader_type) :: LKD490Ob_Err    ! DPI for observation errors
  TYPE (ElementHeader_type) :: RadarRainrate

  TYPE (ElementHeader_type) :: SO2_AQ
  TYPE (ElementHeader_type) :: PM10_AQ
  TYPE (ElementHeader_type) :: PM2p5_AQ
  TYPE (ElementHeader_type) :: O3_AQ
  TYPE (ElementHeader_type) :: NO2_AQ
  TYPE (ElementHeader_type) :: CO_AQ

  ! upper-level variables,             (NumObs,NumLev)
  ! single level uppper level variables are catered for here with NumLev=1
  TYPE (ElementHeader_type) :: z            ! DPI for height
  TYPE (ElementHeader_type) :: t            ! DPI for temperature
  TYPE (ElementHeader_type) :: td           ! DPI for dew point temp
  TYPE (ElementHeader_type) :: theta        ! DPI for theta
  TYPE (ElementHeader_type) :: rh           ! DPI for rh
  TYPE (ElementHeader_type) :: q            ! DPI for specific humidity
  TYPE (ElementHeader_type) :: ddd          ! DPI for ddd & fff
  TYPE (ElementHeader_type) :: fff          ! DPI for ddd & fff
  TYPE (ElementHeader_type) :: u            ! DPI for wind
  TYPE (ElementHeader_type) :: v            ! DPI for wind
  TYPE (ElementHeader_type) :: w            ! DPI for w component of wind
  TYPE (ElementHeader_type) :: Salt         ! DPI for Salinity
  TYPE (ElementHeader_type) :: BriTemp      ! DPI for brightness temps (K)
  TYPE (ElementHeader_type) :: Emissivity   ! DPI for surface emissivity
  TYPE (ElementHeader_type) :: CLW          ! DPI for cloud liquid water
  TYPE (ElementHeader_type) :: CIW          ! DPI for cloud ice water
  TYPE (ElementHeader_type) :: GPSTZDelay   ! DPI for GPS total zenith delay
  TYPE (ElementHeader_type) :: GPSZWDelay   ! DPI for GPS zenith wet delay
  TYPE (ElementHeader_type) :: Cloud        ! DPI for cloud fraction
  TYPE (ElementHeader_type) :: Rainrate     ! DPI for rainrate
  TYPE (ElementHeader_type) :: Snowrate     ! DPI for rainrate
  TYPE (ElementHeader_type) :: ozone        ! DPI for ozone
  TYPE (ElementHeader_type) :: Refrac       ! DPI for GPSRO refractivity
  TYPE (ElementHeader_type) :: BendingAngle ! DPI for GPSRO bending angle
  TYPE (ElementHeader_type) :: BendingAngleAll ! DPI for GPSRO bending angle
  TYPE (ElementHeader_type) :: BendingAngleOrig ! DPI for GPSRO bending angle
  TYPE (ElementHeader_type) :: ImpactParam  ! DPI for GPSRO bending angle
  TYPE (ElementHeader_type) :: ImpactParamAll  ! DPI for GPSRO bending angle
  TYPE (ElementHeader_type) :: RO_Rad_Curv  ! DPI for GPSRO radius of curvature
  TYPE (ElementHeader_type) :: RO_geoid_und  ! DPI for GPSRO geoid undulation
  TYPE (ElementHeader_type) :: p            ! DPI for pressure
  TYPE (ElementHeader_type) :: TempMes_Var  ! DPI for mesoscale temp variance
  TYPE (ElementHeader_type) :: TempMes_HCor ! DPI for mesoscale temp horizontal
                                            ! correlation
  TYPE (ElementHeader_type) :: TempSyn_Var  ! DPI for synoptic temp. variance
  TYPE (ElementHeader_type) :: TempSyn_HCor ! DPI for synoptic temp. horizontal
                                            ! correlation
  TYPE (ElementHeader_type) :: TempOb_Err   ! DPI for observation errors
  TYPE (ElementHeader_type) :: SalMes_Var   ! DPI for mesoscale salinity
                                            ! variance
  TYPE (ElementHeader_type) :: SalMes_HCor  ! DPI for mesoscale salinity
                                            ! horizontal correlation
  TYPE (ElementHeader_type) :: SalSyn_Var   ! DPI for synoptic salinity
                                            ! variance
  TYPE (ElementHeader_type) :: SalSyn_HCor  ! DPI for synoptic salinity
                                            ! horizontal correlation
  TYPE (ElementHeader_type) :: SalOb_Err    ! DPI for observation errors
  TYPE (ElementHeader_type) :: SBUVozone    ! DPI for SBUV ozone density
  TYPE (ElementHeader_type) :: RichNumber   ! DPI for Richardson number
  TYPE (ElementHeader_type) :: SoilMoisture ! DPI for soil moisture
  TYPE (ElementHeader_type) :: SoilTemp     ! DPI for soil temperature
  TYPE (ElementHeader_type) :: Rainamount   ! DPI for rainamount
  ! Radar variables
  TYPE (ElementHeader_type) :: RadialVelocity ! DPI for Radial velocity ob
  TYPE (ElementHeader_type) :: Reflectivity   ! DPI for Radar reflectivity ob
  TYPE (ElementHeader_type) :: ReflectivityR  ! DPI for Reflectivity from rain ob
  TYPE (ElementHeader_type) :: ReflectivityI  ! DPI for Reflectivity from ice ob
  TYPE (ElementHeader_type) :: AOD            ! DustAOD at pseudo-levels
  TYPE (ElementHeader_type) :: RainAccum      ! Rain accumulation
  TYPE (ElementHeader_type) :: RadRefractivity ! DPI Radar refractivity
  ! Ground based lidar variables and related variables
  TYPE (ElementHeader_type) :: ExnerB           ! DPI for levels B or theta exner
  TYPE (ElementHeader_type) :: orog             ! DPI for orog
  TYPE (ElementHeader_type) :: aerosol          ! Aerosol vertical profile
  TYPE (ElementHeader_type) :: CDNC             ! Cloud droplet number concentration
  TYPE (ElementHeader_type) :: RH_AMC           ! RH after main cloud scheme
  TYPE (ElementHeader_type) :: CeilBackscatter  ! Backscatter profile
  TYPE (ElementHeader_type) :: CeilRange        ! Ceilometer range above instrument
  !HLOSwind information
  TYPE (ElementHeader_type) :: HLOSwind

END TYPE ModelOBheader_type

!-------------------------------------------------------------------
! ModelOB data types
!--------------------
! There is a one to one correspondence between the items
! in ModelOBheader and ModelOB
! There is a one to one correspondence between the items
! in ModelOB and OB except for those variables in Ob
! described as MetaData
!
TYPE ModelOB_type
  TYPE (ModelOBheader_type)  :: header
  ! Surface variables,                   (NumObs)
  REAL, POINTER              :: Pstandard(:) => NULL() ! pressure at Zstandard     Pa
  REAL, POINTER              :: Pstation(:) => NULL() ! pressure at Zstation      Pa
  REAL, POINTER              :: Pmsl(:) => NULL() ! Pmsl                    Pa
  REAL, POINTER              :: pstar(:) => NULL() ! pressure at model surface Pa
  REAL, POINTER              :: t2(:) => NULL() ! 2m temperature            K
  REAL, POINTER              :: td2(:) => NULL() ! 2m dew point temperature  K
  REAL, POINTER              :: rh2(:) => NULL() ! 2m relative humidity      %
  REAL, POINTER              :: ddd10(:) => NULL() ! 10m wind direction      deg
  REAL, POINTER              :: fff10(:) => NULL() ! 10m wind speed          m/s
  REAL, POINTER              :: u10(:) => NULL() ! 10m westerly wind         m/s
  REAL, POINTER              :: v10(:) => NULL() ! 10m southerly wind        m/s
  REAL, POINTER              :: vis(:) => NULL() ! (visibility),        (vis m)
  REAL, POINTER              :: logvis(:) => NULL() ! log10(visibility),   (vis m)
  REAL, POINTER              :: WindSpeed(:) => NULL() ! WindSpeed                 m/s
  REAL, POINTER              :: WaveHeight(:) => NULL() ! WaveHeight                m
  REAL, POINTER              :: WAVE_HGHT(:) => NULL() ! WaveHeight                m
  REAL, POINTER              :: WIND_SPED(:) => NULL() ! Surface windspeed         m/s
  REAL, POINTER              :: SeaHeight(:) => NULL() ! Height of Sea surface     m
  REAL, POINTER              :: SST(:) => NULL() ! Sea Surface Temperature   K
  REAL, POINTER              :: Tskin(:) => NULL() ! Surface radiative temp.   K
  REAL, POINTER              :: TCWV(:) => NULL() ! Total Column Water Vapour
                                               !                      kg/m^2
  REAL, POINTER              :: MASS_DNSTY(:) => NULL()  ! Mass density kg/m3
  REAL, POINTER              :: SO2_DNSTY(:) => NULL()   ! Mass density kg/m3
  REAL, POINTER              :: PM10_DNSTY(:) => NULL()  ! Mass density kg/m3
  REAL, POINTER              :: PM2p5_DNSTY(:) => NULL() ! Mass density kg/m3
  REAL, POINTER              :: O3_DNSTY(:) => NULL()    ! Mass density kg/m3
  REAL, POINTER              :: NO2_DNSTY(:) => NULL()   ! Mass density kg/m3
  REAL, POINTER              :: CO_DNSTY(:) => NULL()    ! Mass density kg/m3

  REAL, POINTER              :: LWP(:) => NULL() ! Liquid Water Path    kg/m^2
  REAL, POINTER              :: RetLWP(:) => NULL() ! retrieved Liquid Water Path   kg/m^2
  REAL, POINTER              :: GPSTZDelay(:) => NULL() ! GPS total zenith delay
  REAL, POINTER              :: GPSZWDelay(:) => NULL() ! GPS zenith wet delay
  REAL, POINTER              :: TotalCloudAmount(:) => NULL() ! Total Cloud Amount
  REAL, POINTER              :: u10Ambwind(:,:) => NULL() ! 10m westerly scat ambiguous
                                                  ! winds
  REAL, POINTER              :: v10AmbWind(:,:) => NULL() ! 10m southerly scat
                                                  ! ambiguous winds
  REAL, POINTER              :: AWPriorPcorrect(:,:) => NULL() ! prob that wind is
                                                      ! correct
  REAL, POINTER              :: SeaSrfcHeight(:) => NULL() ! Altimeter SSH         cm
  REAL, POINTER              :: SeaIce(:) => NULL() ! Sea Ice
  REAL, POINTER              :: CHL(:) => NULL()       ! mass concentration of chlorophyll a 
                                                                   ! in sea water milligram     m-3
  REAL, POINTER              :: KD490(:) => NULL()     ! volume attenuation coefficient of downwelling
  REAL, POINTER              :: LCHL(:) => NULL()       ! mass concentration of chlorophyll a 
                                                                   ! in sea water milligram     m-3
  REAL, POINTER              :: LKD490(:) => NULL()     ! volume attenuation coefficient of downwelling
  REAL, POINTER              :: SSTMes_Var(:) => NULL() ! mesoscale SST variance
  REAL, POINTER              :: SSTMes_HCor(:) => NULL() ! mesoscale SST horizontal
                                                 ! correlation
  REAL, POINTER              :: SSTSyn_Var(:) => NULL() ! synoptic SST variance
  REAL, POINTER              :: SSTSyn_HCor(:) => NULL() ! synoptic SST horizontal
                                                 ! correlation
  REAL, POINTER              :: SSTOb_Err(:) => NULL() ! observation errors
  REAL, POINTER              :: SSHMes_Var(:) => NULL() ! mesoscale SSH variance
  REAL, POINTER              :: SSHMes_HCor(:) => NULL() ! mesoscale SSH horizontal
                                                 ! correlation
  REAL, POINTER              :: SSHSyn_Var(:) => NULL() ! synoptic SSH variance
  REAL, POINTER              :: SSHSyn_HCor(:) => NULL() ! synoptic SSH horizontal
                                                 ! correlation
  REAL, POINTER              :: SSHOb_Err(:) => NULL() ! observation errors

  REAL, POINTER              :: SeaIceMes_Var(:) => NULL() ! mesoscale Sea ice variance
  REAL, POINTER              :: SeaIceMes_HCor(:) => NULL() ! mesoscale Sea ice horizontal
                                                 ! correlation
  REAL, POINTER              :: SeaIceSyn_Var(:) => NULL() ! synoptic Sea ice variance
  REAL, POINTER              :: SeaIceSyn_HCor(:) => NULL() ! synoptic Sea ice horizontal
                                                 ! correlation
  REAL, POINTER              :: SeaIceOb_Err(:) => NULL() ! observation errors
  REAL, POINTER              :: CHLOb_Err(:) => NULL() ! observation errors
  REAL, POINTER              :: KD490Ob_Err(:) => NULL() ! observation errors
  REAL, POINTER              :: LCHLOb_Err(:) => NULL() ! observation errors
  REAL, POINTER              :: LKD490Ob_Err(:) => NULL() ! observation errors

  REAL, POINTER              :: PrWx(:) => NULL() ! Present Weather
  REAL, POINTER              :: LwstClod(:) => NULL() ! Low cloud amount       frac
  REAL, POINTER              :: LwstClodType(:) => NULL() ! Low cloud type
  INTEGER, POINTER           :: SSTCnfd(:) => NULL()  ! SST confidence
  REAL, POINTER              :: SnowAmount(:) => NULL()   ! Snow cover
  REAL, POINTER              :: BLheight(:) => NULL()     ! Boundary layer height
  REAL, POINTER              :: ObukhovLength(:) => NULL() ! Obukhov length
  REAL, POINTER              :: FrictionVel(:) => NULL() ! Friction velocity
  REAL, POINTER              :: SWradiation(:) => NULL()  ! Downward SW sfc radiation
  REAL, POINTER              :: PrecipAcc6hr(:) => NULL() ! 6 hrly precip accumulation
  REAL, POINTER              :: LowCloudAmount(:) => NULL() ! Low cloud amount
  REAL, POINTER              :: MedCloudAmount(:) => NULL() ! Medium cloud amount
  REAL, POINTER              :: LowCloudBase(:) => NULL() ! Low cloud base
  REAL, POINTER              :: RO_Rad_Curv(:) => NULL()    ! GPSRO RoC
  REAL, POINTER              :: RO_geoid_und(:) => NULL()   ! GPSRO geoid
  REAL, POINTER              :: RadarRainRate(:) => NULL()
  REAL, POINTER              :: SO2_AQ(:) => NULL()
  REAL, POINTER              :: PM10_AQ(:) => NULL()
  REAL, POINTER              :: PM2p5_AQ(:) => NULL()
  REAL, POINTER              :: O3_AQ(:) => NULL()
  REAL, POINTER              :: NO2_AQ(:) => NULL()
  REAL, POINTER              :: CO_AQ(:) => NULL()

  ! upper-level variables,             (NumObs,NumLev)
  ! single level upper level variables are catered for here with NumLev=1
  REAL, POINTER              :: z(:,:) => NULL() ! height                    m
  REAL, POINTER              :: t(:,:) => NULL() ! temperature               K
  REAL, POINTER              :: td(:,:) => NULL() ! dewpoint temperature      K
  REAL, POINTER              :: theta(:,:) => NULL() ! potential temperature     K
  REAL, POINTER              :: rh(:,:) => NULL() ! relative humidity         %
  REAL, POINTER              :: q(:,:) => NULL() ! specific humidity         g/Kg
  REAL, POINTER              :: u(:,:) => NULL() ! westerly wind             m/s
  REAL, POINTER              :: v(:,:) => NULL() ! southerly wind            m/s
  REAL, POINTER              :: w(:,:) => NULL() ! vertical wind             m/s
  REAL, POINTER              :: ddd(:,:) => NULL() ! wind direction            deg
  REAL, POINTER              :: fff(:,:) => NULL() ! wind speed                m/s
  REAL, POINTER              :: Salt(:,:) => NULL() ! salinity                  %o
  REAL, POINTER              :: BriTemp(:,:) => NULL() ! brightness temps          K
  REAL, POINTER              :: Emissivity(:,:) => NULL() ! surface emissivity
  REAL, POINTER              :: CLW(:,:) => NULL() ! Cloud liquid water
  REAL, POINTER              :: CIW(:,:) => NULL() ! Cloud ice water
  REAL, POINTER              :: Cloud(:,:) => NULL() ! cloud fraction (0 to 1)
  REAL, POINTER              :: Rainrate(:,:) => NULL() ! rainrate              kg/m^2/s
  REAL, POINTER              :: Snowrate(:,:) => NULL() ! rainrate              kg/m^2/s
  REAL, POINTER              :: ozone(:,:) => NULL() ! ozone (kg/kg)
  REAL, POINTER              :: Refrac(:,:) => NULL() ! refractivity
  REAL, POINTER              :: BendingAngle(:,:) => NULL() ! GPSRO bending angle
  REAL, POINTER              :: BendingAngleAll(:,:) => NULL() ! GPSRO bending angle
  REAL, POINTER              :: BendingAngleOrig(:,:) => NULL() ! GPSRO bending angle
  REAL, POINTER              :: ImpactParam(:,:) => NULL()   ! GPSRO impact parameter
  REAL, POINTER              :: ImpactParamAll(:,:) => NULL()   ! GPSRO impact parameter
  REAL, POINTER              :: p(:,:) => NULL() ! pressure
  REAL, POINTER              :: TempMes_Var(:,:) => NULL() ! mesoscale temp variance
  REAL, POINTER              :: TempMes_HCor(:,:) => NULL() ! mesoscale temp horizontal
                                                    ! correlation
  REAL, POINTER              :: TempSyn_Var(:,:) => NULL() ! synoptic temp variance
  REAL, POINTER              :: TempSyn_HCor(:,:) => NULL() ! synoptic temp horizontal
                                                    ! correlation
  REAL, POINTER              :: TempOb_Err(:,:) => NULL() ! observation errors
  REAL, POINTER              :: SalMes_Var(:,:) => NULL() ! mesoscale salinity
                                                   ! variance
  REAL, POINTER              :: SalMes_HCor(:,:) => NULL() ! mesoscale salinity
                                                   ! horizontal correlation
  REAL, POINTER              :: SalSyn_Var(:,:) => NULL() ! synoptic salinity variance
  REAL, POINTER              :: SalSyn_HCor(:,:) => NULL() ! synoptic salinity
                                                   ! horizontal correlation
  REAL, POINTER              :: SalOb_Err(:,:) => NULL() ! observation errors
  REAL, POINTER              :: SBUVozone(:,:) => NULL() ! SBUV ozone density (kg/m**2)
  REAL, POINTER              :: RichNumber(:,:) => NULL()   ! DPI for Richardson number
  REAL, POINTER              :: SoilMoisture(:,:) => NULL() ! DPI for soil moisture
  REAL, POINTER              :: SoilTemp(:,:) => NULL()     ! DPI for soil temperature
  REAL, POINTER              :: Rainamount(:,:) => NULL() ! rainamount          kg/kg
  ! Radar variables
  REAL, POINTER              :: RadialVelocity(:,:) => NULL() ! Radial velocity ob
  REAL, POINTER              :: Reflectivity(:,:) => NULL()   ! Reflectivity ob
  REAL, POINTER              :: ReflectivityR(:,:) => NULL()   ! ReflectivityR ob
  REAL, POINTER              :: ReflectivityI(:,:) => NULL()   ! ReflectivityI ob
  REAL, POINTER              :: AOD(:,:) => NULL()            ! Aerosol Optical Depth
  REAL, POINTER              :: RainAccum(:) => NULL()        ! Rain accumulation
  REAL, POINTER              :: RadRefractivity(:,:) => NULL() ! Radar refractivity
  ! Ground based lidar variables and related variables
  REAL, POINTER              :: ExnerB(:,:) => NULL()          ! Exner pressure on theta-levels
  REAL, POINTER              :: orog(:) => NULL()              ! orography
  REAL, POINTER              :: aerosol(:,:) => NULL()         ! Aerosol vertical profile
  REAL, POINTER              :: CDNC(:,:) => NULL()            ! Cloud droplet number concentration
  REAL, POINTER              :: RH_AMC(:,:) => NULL()          ! RH after main cloud scheme
  REAL, POINTER              :: CeilBackscatter(:,:) => NULL() ! Backscatter profile
  REAL, POINTER              :: CeilRange(:,:) => NULL()       ! Ceilometer range above instrument
  ! HLOSwind information
  REAL, POINTER              :: HLOSwind(:) => NULL()
CONTAINS
  PROCEDURE                  :: init => Ops_InitModelOb
  PROCEDURE                  :: deallocate => Ops_ModelOBDeAllocate
  PROCEDURE                  :: print => Ops_ModelOBPrint
END TYPE ModelOB_type

CONTAINS

INCLUDE 'Ops_InitModelOb.inc'
INCLUDE 'Ops_ModelOBAllocate.inc'
INCLUDE 'Ops_ModelOBDeAllocate.inc'
INCLUDE 'Ops_ModelObGlobalAction.inc'
INCLUDE 'Ops_ModelOBPrint.inc'

END MODULE OpsMod_ModelObInfo
