!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! This module contains:
!
! a) The cx_type definition
!
! Notes on updating this structure:
!
! 1) Make sure that for every body element there is a corresponding header element
! 2) Check the following routines to see if they need updating:
!         Ops_CXGlobalAction
!         Ops_BGVertInterp
!         Ops_BGVEVertInterp
!-------------------------------------------------------------------------------

MODULE OpsMod_CXInfo

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI,                                  &
  RMDI

USE OpsMod_DateTime, ONLY: &
  DateTime_type

USE OpsMod_MiscTypes, ONLY: &
  ElementHeader_type

IMPLICIT NONE

SAVE

! Public declarations:

!-------------------------------------------------------------------
! CX header types used in CX_type
!-----------------------------------
! There is a one to one correspondence between the items
! in CXheader and CX
!
TYPE CXheader_type
  LOGICAL                   :: Rotated = .FALSE.      ! True if lat & long, & u,v wind components are for rotated grid
  REAL                      :: PoleLat = RMDI      ! Lat of grid pole
  REAL                      :: PoleLong = RMDI     ! Long of grid pole
  REAL                      :: StartLat = RMDI     ! Northmost rotated latitude
  REAL                      :: StartLong = RMDI    ! Westmost rotated longitude
  REAL                      :: LatInterval = RMDI  ! Latitude grid spacing
  REAL                      :: LongInterval = RMDI ! Longitude grid spacing
  INTEGER                   :: NumLat = IMDI      ! Number of latitude points
  INTEGER                   :: NumLong = IMDI      ! Number of longitude points
  INTEGER                   :: NumLocal = IMDI     ! Number of Cx columns on this PE
  INTEGER                   :: NumTotal = IMDI     ! Number of Cx columns overall
  INTEGER                   :: ObsGroup = IMDI     ! Code for group of Obs Types
  INTEGER                   :: NumWetLev = IMDI    ! number of Wet Levels
  INTEGER                   :: LevelAboveBL = IMDI ! The first level above the boundary layer

  ! Vertical coordinate constants:
  REAL, POINTER             :: Depths(:) => NULL() ! Depths array for ocean
  REAL, POINTER             :: AK(:) => NULL()     ! Old dynamics vertical coord info
  REAL, POINTER             :: BK(:) => NULL()
  REAL, POINTER             :: AKH(:) => NULL()
  REAL, POINTER             :: BKH(:) => NULL()

  ! New Dynamics
  REAL, POINTER             :: EtaTheta(:) => NULL()  ! New dynamics vertical coord info
  REAL, POINTER             :: EtaRho(:) => NULL()
  REAL, POINTER             :: EtaP(:) => NULL()
  INTEGER                   :: NumLevelsBL = IMDI
  INTEGER                   :: FirstConstantRhoLevel = IMDI
  INTEGER                   :: VertCoord = IMDI
  INTEGER                   :: ModelVersion = IMDI
  INTEGER                   :: NumLevels = IMDI
  REAL                      :: ZModelTop = RMDI
  REAL                      :: PModelTop = RMDI ! isn't this redundant?
  LOGICAL                   :: NewDynamics = .FALSE.
  INTEGER                   :: SubModel = IMDI

  TYPE (DateTime_type)      :: ValidityTime ! Validity time of this file
  TYPE (ElementHeader_type) :: PlevelsA   ! DPI for levels A or rho pressures
  TYPE (ElementHeader_type) :: PlevelsA_unslanted   ! DPI for levels A or rho pressures
  TYPE (ElementHeader_type) :: PlevelsB   ! DPI for levels B or theta pressures
  TYPE (ElementHeader_type) :: ZA         ! DPI for levels A or rho heights
  TYPE (ElementHeader_type) :: ZB         ! DPI for levels B or theta heights
  TYPE (ElementHeader_type) :: ExnerA     ! DPI for levels A or rho exner
  TYPE (ElementHeader_type) :: ExnerB     ! DPI for levels B or theta exner
  TYPE (ElementHeader_type) :: orog       ! DPI for orog

  !  Var ACTIVE variables
  TYPE (ElementHeader_type) :: pstar      ! DPI for pstar
  TYPE (ElementHeader_type) :: t2         ! DPI for surface temperature
  TYPE (ElementHeader_type) :: rh2        ! DPI for surface moisture
  TYPE (ElementHeader_type) :: u10        ! DPI for 10m wind or windspeed err.
  TYPE (ElementHeader_type) :: v10        ! DPI for 10m wind or windspeed err.
  TYPE (ElementHeader_type) :: u10_orig   ! DPI for 10m wind or windspeed err.
  TYPE (ElementHeader_type) :: v10_orig   ! DPI for 10m wind or windspeed err.
  TYPE (ElementHeader_type) :: qt2        ! DPI for surface total water
  TYPE (ElementHeader_type) :: logm       ! DPI for surface aerosol
  TYPE (ElementHeader_type) :: u          ! DPI for wind or wind speed error
  TYPE (ElementHeader_type) :: u_unslanted          ! DPI for wind or wind speed error
  TYPE (ElementHeader_type) :: v          ! DPI for wind or wind speed error
  TYPE (ElementHeader_type) :: v_unslanted          ! DPI for wind or wind speed error
  TYPE (ElementHeader_type) :: u_orig     ! DPI for wind or wind speed error
  TYPE (ElementHeader_type) :: v_orig     ! DPI for wind or wind speed error
  TYPE (ElementHeader_type) :: w          ! DPI for vertical wind component
  TYPE (ElementHeader_type) :: theta      ! DPI for theta
  TYPE (ElementHeader_type) :: theta_unslanted      ! DPI for theta
  TYPE (ElementHeader_type) :: t          ! DPI for temperature
  TYPE (ElementHeader_type) :: q          ! DPI for q
  TYPE (ElementHeader_type) :: q_unslanted          ! DPI for q
  TYPE (ElementHeader_type) :: qc         ! DPI for qc
  TYPE (ElementHeader_type) :: rh         ! DPI for rh
  TYPE (ElementHeader_type) :: rhtotal    ! DPI for total relative humidity
  TYPE (ElementHeader_type) :: ozone      ! DPI for ozone
  TYPE (ElementHeader_type) :: dustp      ! DPI for dust
  TYPE (ElementHeader_type) :: AOD        ! DPI for spectral AOD (dust)
  TYPE (ElementHeader_type) :: p          ! DPI for pressure
  TYPE (ElementHeader_type) :: p_bar      ! DPI for pressure at theta levels

  !  Var non-ACTIVE variables
  TYPE (ElementHeader_type) :: vis        ! DPI for visibility
  TYPE (ElementHeader_type) :: WaveHeight ! DPI for wave height
  TYPE (ElementHeader_type) :: SeaHeight  ! DPI for height of sea surface
  TYPE (ElementHeader_type) :: SST        ! DPI for Sea Surface Temperature
  TYPE (ElementHeader_type) :: CHL        ! DPI for chlorophyll content
  TYPE (ElementHeader_type) :: KD490      ! DPI for chlorophyll optical kd490
  TYPE (ElementHeader_type) :: LCHL       ! DPI for log chlorophyll content
  TYPE (ElementHeader_type) :: LKD490     ! DPI for log chlorophyll optical kd490
  TYPE (ElementHeader_type) :: TskinSea   ! interpolated using land/sea points
  TYPE (ElementHeader_type) :: TropPres   ! DPI for TropPres
  TYPE (ElementHeader_type) :: pmsl       ! DPI for pmsl
  TYPE (ElementHeader_type) :: SeaIce     ! DPI for sea ice fraction
  TYPE (ElementHeader_type) :: SnowAmount ! DPI for surface snow amount
  TYPE (ElementHeader_type) :: WAVE_HGHT  ! DPI for wave height
  TYPE (ElementHeader_type) :: WIND_SPED  ! DPI for surface windspeed
  TYPE (ElementHeader_type) :: aerosol    ! DPI for total aerosol
  TYPE (ElementHeader_type) :: aerosol_p  ! DPI for total aerosol vertical profile
  TYPE (ElementHeader_type) :: psurfparama! DPI for pstar
  TYPE (ElementHeader_type) :: psurfparamb! DPI for pstar
  TYPE (ElementHeader_type) :: LapseRate  ! DPI for (near surface) lapse rate
  TYPE (ElementHeader_type) :: CloudAmount  ! DPI for Total Cloud Amount
  TYPE (ElementHeader_type) :: CDNC       ! DPI for Cloud Droplet Number Concentration
  TYPE (ElementHeader_type) :: ConvCloudAmount ! DPI for Convective cloud amnt
  TYPE (ElementHeader_type) :: ConvCloudBaseLevel ! DPI for Conv cloud base
  TYPE (ElementHeader_type) :: ConvCloudTopLevel ! DPI for Conv cloud top
  TYPE (ElementHeader_type) :: SurfRainRate_conv ! DPI for surf conv rain rate
  TYPE (ElementHeader_type) :: SurfSnowRate_conv ! DPI for surf conv snow rate
  TYPE (ElementHeader_type) :: SurfRainRate_LS   ! DPI for surf LS rain rate
  TYPE (ElementHeader_type) :: SurfSnowRate_LS   ! DPI for surf LS snow rate
  TYPE (ElementHeader_type) :: cloud      ! DPI for cloud
  TYPE (ElementHeader_type) :: Salt       ! DPI for salinity
  TYPE (ElementHeader_type) :: ql_layer   ! DPI for layer cloud liquid water
  TYPE (ElementHeader_type) :: qf_layer   ! DPI for layer cloud frozen water
  TYPE (ElementHeader_type) :: RainRate_layer  ! DPI for layer cloud rain rate
  TYPE (ElementHeader_type) :: cloud_layer! DPI for layer cloud fraction
  TYPE (ElementHeader_type) :: cloud_conv ! DPI for conv cloud fraction
  TYPE (ElementHeader_type) :: qc_conv    ! DPI for conv cloud qc (=ql + qf)
  TYPE (ElementHeader_type) :: qcf        ! DPI for qcf (LS) after timestep
  TYPE (ElementHeader_type) :: qcl        ! DPI for qcl (LS) after timestep
  TYPE (ElementHeader_type) :: qrain      ! DPI for qrain (rain amount) after timestep
  TYPE (ElementHeader_type) :: Cf         ! DPI for frozen cloud fraction after timestep
  TYPE (ElementHeader_type) :: Cl         ! DPI for liquid cloud fraction after timestep
  TYPE (ElementHeader_type) :: cloud_bulk ! DPI for bulk coud on layers after timestep
  TYPE (ElementHeader_type) :: RH_AMC     ! DPI for relative humidity after main cloud
  TYPE (ElementHeader_type) :: SeaSrfcHeight  ! DPI for altimeter SSH
  TYPE (ElementHeader_type) :: MeanSeaHeight  ! DPI for mean SSH
  TYPE (ElementHeader_type) :: TempMes_Var     ! DPI for mesoscale temp variance
  TYPE (ElementHeader_type) :: TempMes_HCor    ! DPI for mesoscale temp horiz.correlation
  TYPE (ElementHeader_type) :: TempSyn_Var     ! DPI for synoptic temp. variance
  TYPE (ElementHeader_type) :: TempSyn_HCor    ! DPI for synoptic temp. horiz. correlation
  TYPE (ElementHeader_type) :: TempOb_Err      ! DPI for observation errors
  TYPE (ElementHeader_type) :: SalMes_Var      ! DPI for mesoscale salinity variance
  TYPE (ElementHeader_type) :: SalMes_HCor     ! DPI for mesoscale salinity horiz.correlation
  TYPE (ElementHeader_type) :: SalSyn_Var      ! DPI for synoptic salinity variance
  TYPE (ElementHeader_type) :: SalSyn_HCor     ! DPI for synoptic salinity horiz. correlation
  TYPE (ElementHeader_type) :: SalOb_Err       ! DPI for observation errors
  TYPE (ElementHeader_type) :: SSTMes_Var      ! DPI for mesoscale SST variance
  TYPE (ElementHeader_type) :: SSTMes_HCor     ! DPI for mesoscale SST horiz.correlation
  TYPE (ElementHeader_type) :: SSTSyn_Var      ! DPI for synoptic SST variance
  TYPE (ElementHeader_type) :: SSTSyn_HCor     ! DPI for synoptic SST horiz. correlation
  TYPE (ElementHeader_type) :: SSTOb_Err       ! DPI for observation errors
  TYPE (ElementHeader_type) :: SSHMes_Var      ! DPI for mesoscale SSH variance
  TYPE (ElementHeader_type) :: SSHMes_HCor     ! DPI for mesoscale SSH horiz.correlation
  TYPE (ElementHeader_type) :: SSHSyn_Var      ! DPI for synoptic SSH variance
  TYPE (ElementHeader_type) :: SSHSyn_HCor     ! DPI for synoptic SSH horiz. correlation
  TYPE (ElementHeader_type) :: SSHOb_Err       ! DPI for observation errors
  TYPE (ElementHeader_type) :: BLHeight        ! DPI for boundary layer height
  TYPE (ElementHeader_type) :: ObukhovLength   ! DPI for Obukhov length
  TYPE (ElementHeader_type) :: FrictionVel     ! DPI for friction velocity
  TYPE (ElementHeader_type) :: SWradiation     ! DPI for downward SW sfc radiation
  TYPE (ElementHeader_type) :: PrecipAcc6hr    ! DPI for 6 hrly precip accumulation
  TYPE (ElementHeader_type) :: LowCloudAmount  ! DPI for low cloud amount
  TYPE (ElementHeader_type) :: MedCloudAmount  ! DPI for medium cloud amount
  TYPE (ElementHeader_type) :: LowCloudBase    ! DPI for low cloud base
  TYPE (ElementHeader_type) :: RichNumber      ! DPI for Richardson number
  TYPE (ElementHeader_type) :: SoilMoisture    ! DPI for soil moisture
  TYPE (ElementHeader_type) :: SoilTemp        ! DPI for soil temperature
  TYPE (ElementHeader_type) :: SeaIceMes_Var   ! DPI for mesoscale sea ice variance
  TYPE (ElementHeader_type) :: SeaIceMes_HCor  ! DPI for mesoscale sea ice horiz.correlation
  TYPE (ElementHeader_type) :: SeaIceSyn_Var   ! DPI for synoptic sea ice variance
  TYPE (ElementHeader_type) :: SeaIceSyn_HCor  ! DPI for synoptic sea ice horiz. correlation
  TYPE (ElementHeader_type) :: SeaIceOb_Err    ! DPI for sea ice observation errors
  TYPE (ElementHeader_type) :: CHLOb_Err       ! DPI for chlorophyll observation errors
  TYPE (ElementHeader_type) :: KD490Ob_Err     ! DPI for optical kd490 observation errors
  TYPE (ElementHeader_type) :: LCHLOb_Err      ! DPI for log chlorophyll observation errors
  TYPE (ElementHeader_type) :: LKD490Ob_Err    ! DPI for log optical kd490 observation errors
  TYPE (ElementHeader_type) :: T_theta       ! DPI for temperature at theta levels
  TYPE (ElementHeader_type) :: SO2_AQ        ! DPI for sulphur dioxide mixing ratio
  TYPE (ElementHeader_type) :: PM10_AQ       ! DPI for particulate matter (size 10) mixing ratio
  TYPE (ElementHeader_type) :: PM2p5_AQ      ! DPI for particulate matter (size 2.5) mixing ratio
  TYPE (ElementHeader_type) :: O3_AQ         ! DPI for ozone mixing ratio
  TYPE (ElementHeader_type) :: NO2_AQ        ! DPI for nitrogen dioxide mixing ratio
  TYPE (ElementHeader_type) :: CO_AQ         ! DPI for carbon monoxide mixing ratio
  TYPE (ElementHeader_type) :: BLType        ! DPI for boundary layer type
  TYPE (ElementHeader_type) :: cha        ! DPI for cha
  TYPE (ElementHeader_type) :: mss        ! DPI for mss

END TYPE CXheader_type

! CXmulti_type used for multiple fields of the same type, eg dust in different size bins
TYPE CXmulti_type
  REAL, POINTER :: field(:,:) => NULL ()
END TYPE CXmulti_type

!-------------------------------------------------------------------
! CX type
!--------
! There is a one to one correspondence between the items
! in CXheader and CX
!
TYPE CX_type
  TYPE (CXheader_type)   :: header
  INTEGER, POINTER       :: Id(:) => NULL()    ! Unique id for each ob
  REAL, POINTER          :: PlevelsA(:,:) => NULL()
  REAL, POINTER          :: PlevelsA_unslanted(:,:) => NULL()
  REAL, POINTER          :: PlevelsB(:,:) => NULL()
  REAL, POINTER          :: ZA(:,:) => NULL()
  REAL, POINTER          :: ZB(:,:) => NULL()
  REAL, POINTER          :: ExnerA(:,:) => NULL()
  REAL, POINTER          :: ExnerB(:,:) => NULL()
  REAL, POINTER          :: orog(:) => NULL()

  !   Var ACTIVE variables
  REAL, POINTER          :: pstar(:) => NULL()
  REAL, POINTER          :: t2(:) => NULL()
  REAL, POINTER          :: rh2(:) => NULL()
  REAL, POINTER          :: u10(:) => NULL()
  REAL, POINTER          :: v10(:) => NULL()
  REAL, POINTER          :: u10_orig(:) => NULL()
  REAL, POINTER          :: v10_orig(:) => NULL()
  REAL, POINTER          :: qt2(:) => NULL()
  REAL, POINTER          :: logm(:) => NULL()
  REAL, POINTER          :: u(:,:) => NULL()
  REAL, POINTER          :: u_unslanted(:,:) => NULL()
  REAL, POINTER          :: v(:,:) => NULL()
  REAL, POINTER          :: v_unslanted(:,:) => NULL()
  REAL, POINTER          :: u_orig(:,:) => NULL()
  REAL, POINTER          :: v_orig(:,:) => NULL()
  REAL, POINTER          :: theta(:,:) => NULL()
  REAL, POINTER          :: theta_unslanted(:,:) => NULL()
  REAL, POINTER          :: t(:,:) => NULL()
  REAL, POINTER          :: q(:,:) => NULL()
  REAL, POINTER          :: q_unslanted(:,:) => NULL()
  REAL, POINTER          :: qc(:,:) => NULL()
  REAL, POINTER          :: rh(:,:) => NULL()
  REAL, POINTER          :: rhtotal(:,:) => NULL()
  REAL, POINTER          :: ozone(:,:) => NULL()
  REAL, POINTER          :: p(:,:) => NULL()
  REAL, POINTER          :: p_bar(:,:) => NULL()
  TYPE (CXmulti_type), POINTER :: dustp(:) => NULL()
  REAL, POINTER          :: AOD(:,:) => NULL()

  !   Var non-ACTIVE variables
  REAL, POINTER          :: w(:,:) => NULL()
  REAL, POINTER          :: vis(:) => NULL()
  REAL, POINTER          :: WaveHeight(:) => NULL()
  REAL, POINTER          :: SeaHeight(:) => NULL()
  REAL, POINTER          :: SST(:) => NULL()
  REAL, POINTER          :: CHL(:) => NULL()
  REAL, POINTER          :: KD490(:) => NULL()
  REAL, POINTER          :: LCHL(:) => NULL()
  REAL, POINTER          :: LKD490(:) => NULL()
  REAL, POINTER          :: TskinSea(:) => NULL()
  REAL, POINTER          :: TropPres(:) => NULL()
  REAL, POINTER          :: pmsl(:) => NULL()
  REAL, POINTER          :: SeaIce(:) => NULL()
  REAL, POINTER          :: SnowAmount(:) => NULL()
  REAL, POINTER          :: WAVE_HGHT(:) => NULL()
  REAL, POINTER          :: WIND_SPED(:) => NULL()
  REAL, POINTER          :: aerosol(:) => NULL()
  REAL, POINTER          :: aerosol_p(:,:) => NULL()
  REAL, POINTER          :: PSurfParamA(:) => NULL()
  REAL, POINTER          :: PSurfParamB(:) => NULL()
  REAL, POINTER          :: LapseRate(:) => NULL()
  REAL, POINTER          :: CloudAmount(:) => NULL()
  REAL, POINTER          :: CDNC(:,:) => NULL()
  REAL, POINTER          :: ConvCloudAmount(:) => NULL()
  REAL, POINTER          :: ConvCloudBaseLevel(:) => NULL()! INTEGER values held as real
  REAL, POINTER          :: ConvCloudTopLevel(:) => NULL() ! INTEGER values held as real
  REAL, POINTER          :: SurfRainRate_conv(:) => NULL()
  REAL, POINTER          :: SurfSnowRate_conv(:) => NULL()
  REAL, POINTER          :: SurfRainRate_LS(:) => NULL()
  REAL, POINTER          :: SurfSnowRate_LS(:) => NULL()
  REAL, POINTER          :: cloud(:,:) => NULL()
  REAL, POINTER          :: Salt(:,:) => NULL()
  REAL, POINTER          :: ql_layer(:,:) => NULL()
  REAL, POINTER          :: qf_layer(:,:) => NULL()
  REAL, POINTER          :: RainRate_layer(:,:) => NULL()
  REAL, POINTER          :: cloud_layer(:,:) => NULL()
  REAL, POINTER          :: cloud_conv(:,:) => NULL()
  REAL, POINTER          :: qc_conv(:,:) => NULL()
  REAL, POINTER          :: qcf(:,:) => NULL()
  REAL, POINTER          :: qcl(:,:) => NULL()
  REAL, POINTER          :: qrain(:,:) => NULL()
  REAL, POINTER          :: Cf(:,:) => NULL()
  REAL, POINTER          :: Cl(:,:) => NULL()
  REAL, POINTER          :: cloud_bulk(:,:) => NULL()
  REAL, POINTER          :: RH_AMC(:,:) => NULL()
  REAL, POINTER          :: SeaSrfcHeight(:) => NULL()
  REAL, POINTER          :: MeanSeaHeight(:) => NULL()
  REAL, POINTER          :: TempMes_Var(:,:) => NULL()
  REAL, POINTER          :: TempMes_HCor(:,:) => NULL()
  REAL, POINTER          :: TempSyn_Var(:,:) => NULL()
  REAL, POINTER          :: TempSyn_HCor(:,:) => NULL()
  REAL, POINTER          :: TempOb_Err(:,:) => NULL()
  REAL, POINTER          :: SalMes_Var(:,:) => NULL()
  REAL, POINTER          :: SalMes_HCor(:,:) => NULL()
  REAL, POINTER          :: SalSyn_Var(:,:) => NULL()
  REAL, POINTER          :: SalSyn_HCor(:,:) => NULL()
  REAL, POINTER          :: SalOb_Err(:,:) => NULL()
  REAL, POINTER          :: SSTMes_Var(:) => NULL()
  REAL, POINTER          :: SSTMes_HCor(:) => NULL()
  REAL, POINTER          :: SSTSyn_Var(:) => NULL()
  REAL, POINTER          :: SSTSyn_HCor(:) => NULL()
  REAL, POINTER          :: SSTOb_Err(:) => NULL()
  REAL, POINTER          :: SSHMes_Var(:) => NULL()
  REAL, POINTER          :: SSHMes_HCor(:) => NULL()
  REAL, POINTER          :: SSHSyn_Var(:) => NULL()
  REAL, POINTER          :: SSHSyn_HCor(:) => NULL()
  REAL, POINTER          :: SSHOb_Err(:) => NULL()
  REAL, POINTER          :: BLHeight(:) => NULL()
  REAL, POINTER          :: ObukhovLength(:) => NULL()
  REAL, POINTER          :: FrictionVel(:) => NULL()
  REAL, POINTER          :: SWradiation(:) => NULL()
  REAL, POINTER          :: PrecipAcc6hr(:) => NULL()
  REAL, POINTER          :: LowCloudAmount(:) => NULL()
  REAL, POINTER          :: MedCloudAmount(:) => NULL()
  REAL, POINTER          :: LowCloudBase(:) => NULL()
  REAL, POINTER          :: RichNumber(:,:) => NULL()
  REAL, POINTER          :: SoilMoisture(:,:) => NULL()
  REAL, POINTER          :: SoilTemp(:,:) => NULL()
  REAL, POINTER          :: SeaIceMes_Var(:) => NULL()
  REAL, POINTER          :: SeaIceMes_HCor(:) => NULL()
  REAL, POINTER          :: SeaIceSyn_Var(:) => NULL()
  REAL, POINTER          :: SeaIceSyn_HCor(:) => NULL()
  REAL, POINTER          :: SeaIceOb_Err(:) => NULL()
  REAL, POINTER          :: CHLOb_Err(:) => NULL()
  REAL, POINTER          :: KD490Ob_Err(:) => NULL()
  REAL, POINTER          :: LCHLOb_Err(:) => NULL()
  REAL, POINTER          :: LKD490Ob_Err(:) => NULL()
  REAL, POINTER          :: T_theta(:,:) => NULL()
  REAL, POINTER          :: SO2_AQ(:) => NULL()
  REAL, POINTER          :: PM10_AQ(:) => NULL()
  REAL, POINTER          :: PM2p5_AQ(:) => NULL()
  REAL, POINTER          :: O3_AQ(:) => NULL()
  REAL, POINTER          :: NO2_AQ(:) => NULL()
  REAL, POINTER          :: CO_AQ(:) => NULL()
  REAL, POINTER          :: BLType(:) => NULL()
  REAL, POINTER          :: cha(:) => NULL()
  REAL, POINTER          :: mss(:) => NULL()

CONTAINS

  PROCEDURE              :: init => Ops_CXInit
  PROCEDURE              :: deallocate => Ops_CXDeAllocate
  PROCEDURE              :: print => Ops_CXPrint

END TYPE CX_type

CONTAINS

INCLUDE 'Ops_CXDeAllocate.inc'
INCLUDE 'Ops_CXExnerB.inc'
INCLUDE 'Ops_CXGlobalAction.inc'
INCLUDE 'Ops_CXInit.inc'
INCLUDE 'Ops_CXP.inc'
INCLUDE 'Ops_CXPrint.inc'
INCLUDE 'Ops_CXQ.inc'
INCLUDE 'Ops_CXRH.inc'
INCLUDE 'Ops_CXT.inc'
INCLUDE 'Ops_CXZND.inc'

END MODULE OpsMod_CXInfo
