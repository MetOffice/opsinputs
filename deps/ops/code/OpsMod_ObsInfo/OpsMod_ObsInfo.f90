!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! This module contains:
!
!   a) The ob_type definition
!
!   b) Some basic generic subroutines
!
! Notes on updating this structure:
!
!   1) Make sure that for every body element there is a corresponding header element
!   2) Check the following routines to see if they need updating:
!   Ops_ObsGlobalAction
!   Ops_ModelObToOb
!-------------------------------------------------------------------------------

MODULE OpsMod_ObsInfo

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE OpsMod_DateTime, ONLY: &
  DateTime_type

USE OpsMod_MiscTypes, ONLY: &
  ElementHeader_type,       &
  coord_type,               &
  Element_Type,             &
  ActionPrint1,             &
  ActionPrint2,             &
  ActionInit,               &
  ActionDeAlloc,            &
  ObPrintIndex

USE OpsMod_ODBTypes, ONLY: &
  QueryInfo_type

IMPLICIT NONE

SAVE

! Public declarations:

TYPE OBheader_type
  LOGICAL                        :: ModelLevel = .FALSE.   ! Data averaged over model layers
  TYPE (DateTime_type)           :: ValidityTime           ! Validity time of this file
  INTEGER                        :: ValidityUTC = IMDI
  INTEGER                        :: NumObsTotal = 0  ! Number of Obs overall
  INTEGER                        :: NumObsLocal = 0  ! Number of Obs on this PE
  INTEGER                        :: NumCxBatches = 0 ! Number of batches
  INTEGER, POINTER               :: ObsPerBatchPerPE(:,:) => NULL () ! Number of obs per PE per batch
  INTEGER, POINTER               :: ObsPerPE(:) => NULL () ! Number of obs per PE per batch
  INTEGER                        :: ObsGroup = IMDI     ! Code for group of Obs TYPEs
  CHARACTER(len=16)              :: ObsGroupName = "" ! Name of group of Obs TYPEs
  INTEGER                        :: ObsRelease = 1   ! Version Number
  INTEGER                        :: ModelForecastLength = IMDI  ! Length of forecast (mins)
  INTEGER                        :: Cutoff = IMDI
  INTEGER                        :: CutoffBefore = IMDI
  LOGICAL                        :: varobs_biascorrected = .FALSE.
  INTEGER                        :: MarineWindAdjusted = 0
  INTEGER, ALLOCATABLE           :: Obsources(:)
  TYPE (QueryInfo_type), POINTER :: odb_query_info(:) => NULL()
  TYPE (ElementHeader_type)      :: Latitude     ! DPI for Latitude
  TYPE (ElementHeader_type)      :: Longitude    ! DPI for Longitude
  TYPE (ElementHeader_type)      :: Family      ! DPI for Family
  TYPE (ElementHeader_type)      :: Time         ! DPI for Time
  TYPE (ElementHeader_type)      :: ReceiptTime  ! DPI for ReceiptTime
  TYPE (ElementHeader_type)      :: GTScentre    ! DPI for GTScentre
  TYPE (ElementHeader_type)      :: SubTYPE      ! DPI for ObsType
  TYPE (ElementHeader_type)      :: ObsType      ! DPI for ObsType
  TYPE (ElementHeader_type)      :: Id           ! DPI for Ob no.
  TYPE (ElementHeader_type)      :: CallSign     ! DPI for CallSign
  TYPE (ElementHeader_type)      :: SatId        ! DPI for Satellite id
  TYPE (ElementHeader_type)      :: ReportPGE    ! DPI for ReportPGE
  TYPE (ElementHeader_type)      :: ReportFlags  ! DPI for ReportFlags
  TYPE (ElementHeader_type)      :: AssimFlags   ! DPI for AssimFlags
  TYPE (ElementHeader_type)      :: ModelOrog    ! DPI for Model Orography
  TYPE (ElementHeader_type)      :: ModelSurface ! DPI for Model Surface type
  TYPE (ElementHeader_type)      :: ModelSeaIce  ! DPI for Model Sea Ice
  TYPE (ElementHeader_type)      :: ModelCloudAmount ! DPI for model cloud amount
  TYPE (ElementHeader_type)      :: SatobCompMethod ! DPI for Satob Channel
  TYPE (ElementHeader_type)      :: SatInst      ! DPI for Satellite instr
  TYPE (ElementHeader_type)      :: ThinningScore  ! DPI for Thinning Score
  TYPE (ElementHeader_type)      :: ThinningRounds ! DPI for Thinning Rounds
  TYPE (ElementHeader_type)      :: ThinningCall   ! DPI for Thinning Call
  TYPE (ElementHeader_type)      :: MWEmissAtlas   ! DPI for MW Emissivity Atlas
  TYPE (ElementHeader_type)      :: MWEmErrAtlas   ! DPI for MW Em. Error Atlas
  TYPE (ElementHeader_type)      :: ObNumber     ! DPI for ObNumber (altimeter)
  TYPE (ElementHeader_type)      :: SSTRejFlag    ! DPI for SST rejection flags
  TYPE (ElementHeader_type)      :: SSTBias       ! DPI for SST bias
  TYPE (ElementHeader_type)      :: SSTStdev      ! DPI for SST standard deviation
  TYPE (ElementHeader_type)      :: SSTQuality    ! DPI for SST Quality code
  TYPE (ElementHeader_type)      :: SSTAlgrType   ! DPI for SST Algorithm type - only for SLSTR SST
  TYPE (ElementHeader_type)      :: ObPractice     ! DPI for ObPractice
  TYPE (ElementHeader_type)      :: VesselInfo     ! DPI for VesselInfo
  TYPE (ElementHeader_type)      :: Instruments    ! DPI for Instruments (SST only?)
  TYPE (ElementHeader_type)      :: Zstation       ! DPI for Zstation
  TYPE (ElementHeader_type)      :: Zstandard      ! DPI for Zstandard
  TYPE (ElementHeader_type)      :: AnHeight       ! DPI for AnHeight
  TYPE (ElementHeader_type)      :: CoastDist      ! DPI for CoastDist
  TYPE (ElementHeader_type)      :: LapseRate      ! DPI for LapseRate
  TYPE (ElementHeader_type)      :: HeightRange    ! DPI for HeightRange
  TYPE (ElementHeader_type)      :: zstncorrected  ! DPI for corrected station height
  TYPE (ElementHeader_type)      :: SondeTYPE       ! DPI for Sonde instrument type
  TYPE (ElementHeader_type)      :: SondeReportType ! DPI for Sonde report type
  TYPE (ElementHeader_type)      :: HawsonCor       ! DPI for HawsonCor
  TYPE (ElementHeader_type)      :: Hawson00Cor     ! DPI for Hawson00Cor
  TYPE (ElementHeader_type)      :: Hawson12Cor     ! DPI for Hawson12Cor
  TYPE (ElementHeader_type)      :: Nlevels         ! DPI for Nlevels
  TYPE (ElementHeader_type)      :: LevelType       ! DPI for level type
  TYPE (ElementHeader_type)      :: WinProQCFlags
  TYPE (ElementHeader_type)      :: TailNumber     ! DPI for TailNumber
  TYPE (ElementHeader_type)      :: FlightPhase    ! DPI for FlightPhase
  TYPE (ElementHeader_type)      :: FlightLevel    ! DPI for FlightLevel
  TYPE (ElementHeader_type)      :: AcarsStatus    ! DPI for AcarsStatus
  TYPE (ElementHeader_type)      :: NoDualPixels  ! No. of dual pixels (aatsr)
  TYPE (ElementHeader_type)      :: SSTCnfd       ! SST confidence (aatsr)
  TYPE (ElementHeader_type)      :: RcptStn        ! DPI for ground station
  TYPE (ElementHeader_type)      :: ScanLine       ! DPI for scanline number
  TYPE (ElementHeader_type)      :: ScanPosition   ! DPI for scan position
  TYPE (ElementHeader_type)      :: surface        ! DPI for surface
  TYPE (ElementHeader_type)      :: elevation      ! DPI for elevation
  TYPE (ElementHeader_type)      :: LocalZenith    ! DPI for local zenith angle
  TYPE (ElementHeader_type)      :: LocalAzimuth   ! DPI for local azimuth angle
  TYPE (ElementHeader_type)      :: SatZenithAngle ! DPI for sat zenith angle
  TYPE (ElementHeader_type)      :: SatView        ! DPI for SatView
  TYPE (ElementHeader_type)      :: SatAzimth      ! DPI for SatAzimth
  TYPE (ElementHeader_type)      :: SolarZenith    ! DPI for SolarZenith
  TYPE (ElementHeader_type)      :: SolarAzimth    ! DPI for SolarAzimth
  TYPE (ElementHeader_type)      :: ChnlQlty       ! DPI for channel quality flags
  TYPE (ElementHeader_type)      :: NIter          ! DPI for NIter (satellite 1dvar)
  TYPE (ElementHeader_type)      :: CldCost        ! DPI for CldCost (satellite 1dvar)
  TYPE (ElementHeader_type)      :: CirCost        ! DPI for CirCost (satellite 1dvar)
  TYPE (ElementHeader_type)      :: SI150          ! DPI for SI150 (satellite 1dvar)
  TYPE (ElementHeader_type)      :: SI183          ! DPI for SI183 (satellite 1dvar)
  TYPE (ElementHeader_type)      :: QCflags        ! DPI for QCflags (satellite 1dvar)
  TYPE (ElementHeader_type)      :: QCscan         ! DPI for qual indicator bits (satellite 1dvar)
  TYPE (ElementHeader_type)      :: QCprepro       ! DPI for pre-processing flags (satellite 1dvar)
  TYPE (ElementHeader_type)      :: QCinfo         ! DPI for QCinfo (satellite 1dvar)
  TYPE (ElementHeader_type)      :: QCarea         ! DPI for QCarea (satellite 1dvar)
  TYPE (ElementHeader_type)      :: TCozone_init   ! DPI for initial total column ozone
  TYPE (ElementHeader_type)      :: TCozone        ! DPI for total column ozone
  TYPE (ElementHeader_type)      :: IREmiss        ! DPI for Mono. IR surf emiss
  TYPE (ElementHeader_type)      :: Emissivity     ! DPI for surface emissivity
  TYPE (ElementHeader_type)      :: MwEmiss        ! DPI for mwave surf emissivities
  TYPE (ElementHeader_type)      :: MwEmErr        ! DPI for mwave emissivity error
  TYPE (ElementHeader_type)      :: CalcEmiss      ! DPI for Emiss fixed or not?
  TYPE (ElementHeader_type)      :: EmissPC        ! DPI for surf emiss PCs
  TYPE (ElementHeader_type)      :: CloudFrac      ! DPI for CloudFrac
  TYPE (ElementHeader_type)      :: CloudFrac_init ! DPI for CloudFrac_init
  TYPE (ElementHeader_type)      :: CloudTopTemp   ! DPI for CloudTopTemp
  TYPE (ElementHeader_type)      :: CloudTopP      ! DPI for CloudTopP
  TYPE (ElementHeader_type)      :: CloudTopPErr   ! DPI for CloudTopPErr
  TYPE (ElementHeader_type)      :: CloudTopP_init ! DPI for CloudTopP_init
  TYPE (ElementHeader_type)      :: NumClrPriChans ! DPI for NumClrPriChans
  TYPE (ElementHeader_type)      :: ObBriTemp      ! DPI for ObBriTemp
  TYPE (ElementHeader_type)      :: CorBriTemp     ! DPI for CorBriTemp
  TYPE (ElementHeader_type)      :: InitBriTemp    ! DPI for InitBriTemp
  TYPE (ElementHeader_type)      :: ClearBriTemp   ! DPI for ClearBriTemp
  TYPE (ElementHeader_type)      :: JCost          ! DPI for JCost
  TYPE (ElementHeader_type)      :: BiasPredictors ! DPI for BiasPredictors
  TYPE (ElementHeader_type)      :: BiasBriTemp    ! DPI for BiasBriTemp
  TYPE (ElementHeader_type)      :: TauSurf        ! DPI for Analysed Surface Transmittance
  TYPE (ElementHeader_type)      :: BriTempVarError ! DPI for Variable Obs Error
  TYPE (ElementHeader_type)      :: CloudRTError   ! DPI for IR cloud RT error
  TYPE (ElementHeader_type)      :: CloudRTBias    ! DPI for IR cloud RT bias
  TYPE (ElementHeader_type)      :: CloudEffectAvg ! DPI for average IR cloud effect
  TYPE (ElementHeader_type)      :: VnATOVPP       ! DPI for pre-pro version no. (atovs)
  TYPE (ElementHeader_type)      :: AMSUsurface    ! DPI for AMSU surf type (atovs)
  TYPE (ElementHeader_type)      :: AMSUcldcost    ! DPI for AMSU cloud cost (atovs)
  TYPE (ElementHeader_type)      :: AMSUscatindx   ! DPI for AMSU scattering index (atovs)
  TYPE (ElementHeader_type)      :: ATOVSProcOption !DPI for ATOVSProcOption (atovs)
  TYPE (ElementHeader_type)      :: QCHIRSfov      ! DPI for ATOVPP HIRS flags (atovs)
  TYPE (ElementHeader_type)      :: QCGIIRSfov     ! DPI for GIIRS flag
  TYPE (ElementHeader_type)      :: HIRS_Temp      ! DPI for HIRS instrument temp (atovs)
  TYPE (ElementHeader_type)      :: AMSUa1_Temp    ! DPI for AMSU-A1 instrument temp (atovs)
  TYPE (ElementHeader_type)      :: AMSUa2_Temp    ! DPI for AMSU-A2 instrument temp (atovs)
  TYPE (ElementHeader_type)      :: AMSUb_Temp     ! DPI for AMSU-B instrument temp (atovs)
  TYPE (ElementHeader_type)      :: QCIASIfov      ! DPI for  IASI fov QC from AAPP (iasi)
  TYPE (ElementHeader_type)      :: QCIASIGQis     ! DPI for General Quality Flag (iasi)
  TYPE (ElementHeader_type)      :: ScaleFacStartChan ! DPI for scale fac start chan (iasi)
  TYPE (ElementHeader_type)      :: ScaleFacEndChan   ! DPI for scale fac end chan (iasi)
  TYPE (ElementHeader_type)      :: ScaleFac       ! DPI for scale fac (iasi)
  TYPE (ElementHeader_type)      :: ScaledRadiance ! DPI for Scaled-down radiance (iasi)
  TYPE (ElementHeader_type)      :: Radiance       ! DPI for sclaed-up radiance (iasi)
  TYPE (ElementHeader_type)      :: ChannelVersion ! DPI for Chan Selection version number (iasi)
  TYPE (ElementHeader_type)      :: PCScore        ! DPI for Principal Component scores (iasi)
  TYPE (ElementHeader_type)      :: PCVersion      ! DPI for Principal Components version (iasi)
  TYPE (ElementHeader_type)      :: QCIASIPC       ! DPI for QC for PC recons. radiance (iasi)
  TYPE (ElementHeader_type)      :: PixelNumber    ! DPI for pixel number (iasi)
  TYPE (ElementHeader_type)      :: CloudFree      ! DPI for CloudFrac (seviri)
  TYPE (ElementHeader_type)      :: CloudFrac_arr  ! DPI for CloudFrac (mviri)
  TYPE (ElementHeader_type)      :: SSMISScanType   ! DPI for SSMI/S scan data type (ssmis)
  TYPE (ElementHeader_type)      :: SrfcFlag        ! DPI for SSMI/S surface flag (ssmis)
  TYPE (ElementHeader_type)      :: SurfType        ! DPI for redefined SSMI/S surface type indicator (ssmis)
  TYPE (ElementHeader_type)      :: RainFlag        ! DPI for SSMI/S rain flag (ssmis)
  TYPE (ElementHeader_type)      :: RainType        ! DPI for redefined SSMI/S rain indicator (ssmis)
  TYPE (ElementHeader_type)      :: EphemLat        ! DPI for SSMI/S Ephemeris data latitude (ssmis)
  TYPE (ElementHeader_type)      :: EphemLon        ! DPI for SSMI/S Ephemeris data longitude (ssmis)
  TYPE (ElementHeader_type)      :: EphemHeight     ! DPI for SSMI/S Ephemeris data height (ssmis)
  TYPE (ElementHeader_type)      :: BasePontLat     ! DPI for SSMI/S Base point latitude (ssmis)
  TYPE (ElementHeader_type)      :: BasePontLon     ! DPI for SSMI/S Base point longitude (ssmis)
  TYPE (ElementHeader_type)      :: BasePontIncAng  ! DPI for SSMI/S Base point incidence angle (ssmis)
  TYPE (ElementHeader_type)      :: BasePontAzhAng  ! DPI for SSMI/S Base point azimuthal angle (ssmis)
  TYPE (ElementHeader_type)      :: MagneticFieldBe ! DPI for SSMI/S Geomagnetic field, Be (ssmis)
  TYPE (ElementHeader_type)      :: ViewingVectorK  ! DPI for SSMI/S viewing vector, K (ssmis)
  TYPE (ElementHeader_type)      :: BdotK           ! DPI for SSMI/S B.K, magnetic field along radiometer view (ssmis)
  TYPE (ElementHeader_type)      :: thetaBK         ! DPI for SSMI/S angle between magnetic field and radiometer view (ssmis)
  TYPE (ElementHeader_type)      :: SatOrbitAngle   ! DPI for SSMI/S SatOrbitAngle (ssmis)
  TYPE (ElementHeader_type)      :: AMSUZenAng        ! DPI for AMSU zenith angle (airs)
  TYPE (ElementHeader_type)      :: SatCloudTestsUsed ! DPI for satellite cloud tests used (airs)
  TYPE (ElementHeader_type)      :: SatCloudFlags     ! DPI for satellite cloud flags (airs)
  TYPE (ElementHeader_type)      :: SurfWaterFraction ! DPI for surface water fraction (airs)
  TYPE (ElementHeader_type)      :: MainSurfaceType   ! DPI for main surface type (airs)
  TYPE (ElementHeader_type)      :: MainSurfFraction  ! DPI for fraction of main surface type (airs)
  TYPE (ElementHeader_type)      :: AIRSVisMeanAlb   ! DPI for mean albedo of AIRS visible channels
  TYPE (ElementHeader_type)      :: AIRSVisStdDevAlb ! DPI for std. dev. of albedo of AIRS visible channels.
  TYPE (ElementHeader_type)      :: CloudyChannel    ! DPI for flag indicating cloud signal in channel (airs)
  TYPE (ElementHeader_type)      :: SITE_NAME               ! DPI for GPS site name
  TYPE (ElementHeader_type)      :: GPSTZDelay              ! DPI for GPS total zenith delay
  TYPE (ElementHeader_type)      :: GPSTZDelayError         ! DPI for GPS total zenith delay error
  TYPE (ElementHeader_type)      :: GPS_MaxHeightDifference ! DPI for max permitted diff betw GPS rec./orography
  TYPE (ElementHeader_type)      :: GPS_QC_Threshold        ! DPI for max permitted ZTD O-B (gps)
  TYPE (ElementHeader_type)      :: GPS_MinNumObs           ! DPI for min num obs/month before assim permitted (gps)
  TYPE (ElementHeader_type)      :: GPS_TempThinList        ! DPI for Ground GPS Temporal thinning
  TYPE (ElementHeader_type)      :: GPSRO_Zmin             ! DPI for GPSRO min height threshold
  TYPE (ElementHeader_type)      :: GPSRO_Zmax             ! DPI for GPSRO min height threshold
  TYPE (ElementHeader_type)      :: SoftwareIdentity       ! DPI for GPSRO proc. software used
  TYPE (ElementHeader_type)      :: RO_Quality             ! DPI for GPSRO QC info
  TYPE (ElementHeader_type)      :: RO_Confidence          ! DPI for GPSRO confidence
  TYPE (ElementHeader_type)      :: RO_geoid_und           ! DPI for GPSRO Geoid undulation (above WGS-84 ellipsoid, m)
  TYPE (ElementHeader_type)      :: RO_Rad_Curv            ! DPI for GPSRO Earth's local radius of curvature   m
  TYPE (ElementHeader_type)      :: level_time             ! DPI for level dependent time
  TYPE (ElementHeader_type)      :: level_lat              ! DPI for level dependent latitude
  TYPE (ElementHeader_type)      :: level_lon              ! DPI for level dependent longitude
  TYPE (ElementHeader_type)      :: model_level_time       ! DPI for level dependent time
  TYPE (ElementHeader_type)      :: model_level_lat        ! DPI for level dependent latitude
  TYPE (ElementHeader_type)      :: model_level_lon        ! DPI for level dependent longitude
  TYPE (ElementHeader_type)      :: level_azimuth          ! DPI for level dependent azimuth
  TYPE (ElementHeader_type)      :: ro_occ_lat             ! DPI for lat of occultation point
  TYPE (ElementHeader_type)      :: ro_occ_lon             ! DPI for lon of occultation point
  TYPE (ElementHeader_type)      :: GPSRO_heightcorrection ! DPI for GPSRO height cor.
  TYPE (ElementHeader_type)      :: GPSRO_cost_funct_test  ! Threshold value for the cost function convergence test
  TYPE (ElementHeader_type)      :: GPSRO_y_test           ! Threshold value for the yobs-ysol tes
  TYPE (ElementHeader_type)      :: GPSRO_n_iteration_test ! Maximum number of iterations
  TYPE (ElementHeader_type)      :: GPSRO_Delta_factor     ! Delta
  TYPE (ElementHeader_type)      :: GPSRO_Delta_ct2        ! Delta ct2
  TYPE (ElementHeader_type)      :: GPSRO_OB_test          ! Threshold value for the O-B test
  TYPE (ElementHeader_type)      :: GPSRO_qc_mask          ! Integer mask used to control quality flags
  TYPE (ElementHeader_type)      :: GPSRO_max_grad         ! Max back vertical N gradient
  TYPE (ElementHeader_type)      :: GPSRO_hgt_shift        ! Shift in lower impact above max N gradient
  TYPE (ElementHeader_type)      :: GnssClass              ! GNSS classification (i.e. GPS, Beidou, etc.)
  TYPE (ElementHeader_type)      :: GnssId                 ! GNSS ID (satellite ID for GPS, Beidou, etc.)
  TYPE (ElementHeader_type)      :: ESAPCD         ! DPI for ESA Product Confidence Word (scatwind)
  TYPE (ElementHeader_type)      :: WindRetPCD     ! DPI for wind retrieval Confidence Word (scatwind)
  TYPE (ElementHeader_type)      :: BeamIncAngle   ! DPI for beam inc angle (scatwind)
  TYPE (ElementHeader_type)      :: BeamLookAngle  ! DPI for beam look angle (scatwind)
  TYPE (ElementHeader_type)      :: BackScatter    ! DPI for beam back scatter (scatwind)
  TYPE (ElementHeader_type)      :: BeamNoise      ! DPI for beam noise (scatwind)
  TYPE (ElementHeader_type)      :: MissShot       ! DPI for beam missing shot (scatwind)
  TYPE (ElementHeader_type)      :: DistConeRank1  ! DPI for distance to cone of rank1 wind (scatwind)
  TYPE (ElementHeader_type)      :: DistConeRank2  ! DPI for distance to cone of rank2 wind (scatwind)
  TYPE (ElementHeader_type)      :: WindRetSkill   ! DPI for skill in wind retrieval (scatwind)
  TYPE (ElementHeader_type)      :: ScatModel      ! DPI to indicate backscatter model (scatwind)
  TYPE (ElementHeader_type)      :: ddd10AmbWind    ! DPI for Ambiguous 10m ddd wind (scatwind)
  TYPE (ElementHeader_type)      :: fff10AmbWind    ! DPI for Ambiguous 10m fff wind (scatwind)
  TYPE (ElementHeader_type)      :: DistCone        ! DPI for distance to cone (each wind) (scatwind)
  TYPE (ElementHeader_type)      :: ChosenWind      ! DPI for which wind has been chosen (scatwind)
  TYPE (ElementHeader_type)      :: ScatRainProb    ! DPI for likelihood of rain contamination (scatwind)
  TYPE (ElementHeader_type)      :: NumBeams        ! DPI for number of beams used in retrieval (scatwind)
  TYPE (ElementHeader_type)      :: KuSolnLkhd      ! DPI for sol likelihood (KuBand scatwind)
  TYPE (ElementHeader_type)      :: ASCATL2WindSoftwareID  ! DPI for Software id
  TYPE (ElementHeader_type)      :: ASCATIceProb           ! DPI for Ice prob
  TYPE (ElementHeader_type)      :: ASCATIceAgeA           ! DPI for Ice age Aparam
  TYPE (ElementHeader_type)      :: ASCATQCflag            ! DPI for QC flag
  TYPE (ElementHeader_type)      :: ASCATNumberVectorAmby  ! DPI for no of amb
  TYPE (ElementHeader_type)      :: SatDirectionOfMotion   ! DPI for dir of sat
  TYPE (ElementHeader_type)      :: ASCATWindRepCount      ! DPI for wind rep count
  TYPE (ElementHeader_type)      :: ASCATBackDstc          ! DPI for dist to cone
  TYPE (ElementHeader_type)      :: ASCATSolnLkhd          ! DPI for sol likelihood
  TYPE (ElementHeader_type)      :: WINDSATSurfType           ! DPI for surface type
  TYPE (ElementHeader_type)      :: WINDSATWaterVapourContent ! DPI for water vapour
  TYPE (ElementHeader_type)      :: WINDSATLiquidWaterContent ! DPI for liquid water
  TYPE (ElementHeader_type)      :: WINDSATPrecipRate         ! DPI for rain rate
  TYPE (ElementHeader_type)      :: WINDSATSeaSurfTempError   ! DPI for SST error
  TYPE (ElementHeader_type)      :: WINDSATWindSpeedError     ! DPI for speed error
  TYPE (ElementHeader_type)      :: WINDSATWaterVapourError   ! DPI for vapour error
  TYPE (ElementHeader_type)      :: WINDSATLiquidWaterError   ! DPI for water error
  TYPE (ElementHeader_type)      :: WINDSATCHI2WindRetrieval  ! DPI for CHI squared
  TYPE (ElementHeader_type)      :: WINDSATWindDirectionError ! DPI for direction error
  TYPE (ElementHeader_type)      :: WINDSATQCflagA            ! DPI for quality flag
  TYPE (ElementHeader_type)      :: WINDSATQCflagB            ! DPI for quality flag
  TYPE (ElementHeader_type)      :: SuperobNused  ! DPI for SuperobNused
  TYPE (ElementHeader_type)      :: SuperobStart  ! DPI for SuperobStart
  TYPE (ElementHeader_type)      :: PlevelsA     ! DPI for PlevelsA
  TYPE (ElementHeader_type)      :: PlevelsB     ! DPI for PlevelsB
  TYPE (ElementHeader_type)      :: CloudClearMthd ! DPI for cloud clearing method (ukmosst)
  TYPE (ElementHeader_type)      :: StdDevClearAVHRR4 ! DPI for std dev clear AVHRR4 (ukmosst)
  TYPE (ElementHeader_type)      :: AVHRRSSTQC     ! DPI for AVHRR QC flags (ukmosst)
  TYPE (ElementHeader_type)      :: ClearAVHRRPxls ! DPI for no. clear pixles (ukmosst)
  TYPE (ElementHeader_type)      :: OrigCtr         ! DPI for originating centre (satwind/gpsro)
  TYPE (ElementHeader_type)      :: ChanCtralFreq   ! DPI for freq of central channel (satwind)
  TYPE (ElementHeader_type)      :: StereoFreq      ! DPI for freq of central channel (satwind-STEREOMV)
  TYPE (ElementHeader_type)      :: HeightAssMethod ! DPI for height ass'ment method (satwind)
  TYPE (ElementHeader_type)      :: CmptVecWindDirn ! DPI for wind direction of vector components (satwind)
  TYPE (ElementHeader_type)      :: CmptVecWindSpeed! DPI for speed of vector components (satwind)
  TYPE (ElementHeader_type)      :: CmptVecWindU    ! DPI for vector u component (satwind)
  TYPE (ElementHeader_type)      :: CmptVecWindV    ! DPI for vector v component (satwind)
  TYPE (ElementHeader_type)      :: AltHeightAss    ! DPI for extra info on height assignment  (satwind)
  TYPE (ElementHeader_type)      :: AltPressLevel   ! DPI for extra info (satwind)
  TYPE (ElementHeader_type)      :: AltAirTempLevel ! DPI for extra info (satwind)
  TYPE (ElementHeader_type)      :: MPEFQI          ! DPI for Quality Indicator (satwind)
  TYPE (ElementHeader_type)      :: MPEFQI2         ! DPIs for QI info as specified above (satwind)
  TYPE (ElementHeader_type)      :: RFFQI           ! DPI for Quality Indicator (satwind)
  TYPE (ElementHeader_type)      :: SuperobRounds   ! DPI for satw superob rnd cntrl (satwind)
  TYPE (ElementHeader_type)      :: uSuperob        ! DPI for satw superob u,v (satwind)
  TYPE (ElementHeader_type)      :: vSuperob        ! DPI for satw superob u,v (satwind)
  TYPE (ElementHeader_type)      :: SatwindQI       ! DPI for quality indicator (QI) (satwind)
  TYPE (ElementHeader_type)      :: SatwindHeightError ! DPI for height error (satwind)
  TYPE (ElementHeader_type)      :: SatwindBestFitU ! DPI for best fit U (satwind)
  TYPE (ElementHeader_type)      :: SatwindBestFitV ! DPI for best fit V (satwind)
  TYPE (ElementHeader_type)      :: SatwindBestFitPress ! DPI for best fit Press (satwind)
  TYPE (ElementHeader_type)      :: SatwindMaxSpeed ! DPI for max speed (satwind)
  TYPE (ElementHeader_type)      :: SatwindMinSpeed ! DPI for min speed (satwind)
  TYPE (ElementHeader_type)      :: CloudTopHeight  ! DPI for cloud top height
  TYPE (ElementHeader_type)      :: SBUVozone       ! DPI for SBUV ozone
  TYPE (ElementHeader_type)      :: SBUVStDevOzone  ! DPI for SBUV obs error
  TYPE (ElementHeader_type)      :: SBUVUppLayerP   ! DPI for SBUV upp. layer press.
  TYPE (ElementHeader_type)      :: SBUVLowLayerP   ! DPI for SBUV low  layer press.
  TYPE (ElementHeader_type)      :: SBUVSolarElev   ! DPI for SBUV solar elevation
  TYPE (ElementHeader_type)      :: SBUVQualityInf  ! DPI for SBUV quality info
  TYPE (ElementHeader_type)      :: Pstandard    ! DPI for Pstandard
  TYPE (ElementHeader_type)      :: Pstation     ! DPI for Pstation
  TYPE (ElementHeader_type)      :: Pmsl         ! DPI for Pmsl
  TYPE (ElementHeader_type)      :: pstar        ! DPI for pstar
  TYPE (ElementHeader_type)      :: t2           ! DPI for t2
  TYPE (ElementHeader_type)      :: td2          ! DPI for td2
  TYPE (ElementHeader_type)      :: rh2          ! DPI for rh2
  TYPE (ElementHeader_type)      :: ddd10        ! DPI for ddd 10
  TYPE (ElementHeader_type)      :: fff10        ! DPI for fff 10
  TYPE (ElementHeader_type)      :: u10          ! DPI for u & v 10
  TYPE (ElementHeader_type)      :: v10          ! DPI for u & v 10
  TYPE (ElementHeader_type)      :: vis          ! DPI for vis
  TYPE (ElementHeader_type)      :: logvis       ! DPI for logvis
  TYPE (ElementHeader_type)      :: WindSpeed    ! DPI for WindSpeed
  TYPE (ElementHeader_type)      :: WaveHeight   ! DPI for WaveHeight
  TYPE (ElementHeader_type)      :: WAVE_HGHT    ! DPI for WaveHeight
  TYPE (ElementHeader_type)      :: WIND_SPED    ! DPI for Surface windspeed
  TYPE (ElementHeader_type)      :: SOLR_RDTN_FLUX  ! DPI Solar flux         W/m
  TYPE (ElementHeader_type)      :: TIME_PERD_SOLR_RDTN_FLUX  ! DPI Solar flux time delay         W/m
  TYPE (ElementHeader_type)      :: SeaHeight    ! DPI for SeaHeight
  TYPE (ElementHeader_type)      :: SST          ! DPI for Sea Surface Temperature
  TYPE (ElementHeader_type)      :: Tskin        ! DPI for Surface radiative temp
  TYPE (ElementHeader_type)      :: TCWV         ! DPI for Total Column Water Vapour
  TYPE (ElementHeader_type)      :: LWP          ! DPI for Liquid Water Path
  TYPE (ElementHeader_type)      :: IWP          ! DPI for Ice Water Path
  TYPE (ElementHeader_type)      :: RetLWP       ! DPI for retrieved Liquid Water Path
  TYPE (ElementHeader_type)      :: Rainrate     ! DPI for rainrate
  TYPE (ElementHeader_type)      :: Snowrate     ! DPI for snowrate
  TYPE (ElementHeader_type)      :: u10AmbWind   ! DPI for scat Ambiguous 10m uv
  TYPE (ElementHeader_type)      :: v10AmbWind   ! DPI for scat Ambiguous 10m uv
  TYPE (ElementHeader_type)      :: Rainamount   ! DPI for rainamount wind
  TYPE (ElementHeader_type)      :: AWPriorPcorrect ! DPI for scat prior pcorrect
  TYPE (ElementHeader_type)      :: TotalCloudAmount  ! DPI for Total Cloud Amount
  TYPE (ElementHeader_type)      :: SeaSrfcHeight ! DPI for Altimeter SSH
  TYPE (ElementHeader_type)      :: DyncAtmrCortn ! DPI for dynamic atmosphere correction for Altimeter SSH
  TYPE (ElementHeader_type)      :: LongWaveErrr  ! DPI for long wave error fro Altimeter SSH
  TYPE (ElementHeader_type)      :: OcnTide       ! DPI for Ocean Tide for Altimeter SSH
  TYPE (ElementHeader_type)      :: MeanSeaHeight ! DPI for mean for Altimeter SSH
  TYPE (ElementHeader_type)      :: SeaIce        ! DPI for sea ice
  TYPE (ElementHeader_type)      :: CHL           ! DPI for ocean colour
  TYPE (ElementHeader_type)      :: KD490         ! DPI for ocean colour
  TYPE (ElementHeader_type)      :: Precip1hr     ! DPI for SREW obs
  TYPE (ElementHeader_type)      :: PrWx          ! DPI for Present Weather
  TYPE (ElementHeader_type)      :: LwstClod      ! DPI for Low cloud amount
  TYPE (ElementHeader_type)      :: LwstClodType  ! DPI for Low cloud type
  TYPE (ElementHeader_type)      :: WaveMeasureMethod ! DPI for wave recording method
  TYPE (ElementHeader_type)      :: GlobalRad1hr  ! DPI for 1hr solar radiation
  TYPE (ElementHeader_type)      :: WindGust      ! DPI for Wind gust
  TYPE (ElementHeader_type)      :: SGNT_WAVE_HGHT  ! DPI for sig wave height
  TYPE (ElementHeader_type)      :: AVGNG_PERD           ! DPI for av period
  TYPE (ElementHeader_type)      :: CSNT_TYP             ! DPI for chemical type
  TYPE (ElementHeader_type)      :: DCML_SCL_MASS_DNSTY  ! DPI for decimal scale mass densty
  TYPE (ElementHeader_type)      :: MASS_DNSTY           ! DPI for scaled mass dnsty
  TYPE (ElementHeader_type)      :: SO2_DNSTY            ! DPI for scaled mass dnsty
  TYPE (ElementHeader_type)      :: PM10_DNSTY           ! DPI for scaled mass dnsty
  TYPE (ElementHeader_type)      :: PM2p5_DNSTY          ! DPI for scaled mass dnsty
  TYPE (ElementHeader_type)      :: O3_DNSTY             ! DPI for scaled mass dnsty
  TYPE (ElementHeader_type)      :: NO2_DNSTY            ! DPI for scaled mass dnsty
  TYPE (ElementHeader_type)      :: CO_DNSTY             ! DPI for scaled mass dnsty
  TYPE (ElementHeader_type)      :: LNG_STTN_NAME        ! DPI for long station name
  TYPE (ElementHeader_type)      :: GEMS_AQTY_OBSG_STTN  ! DPI for GEMS station name
  TYPE (ElementHeader_type)      :: SITE_QAL             ! AQCOPS station quality
  TYPE (ElementHeader_type)      :: RadarRainWeight  ! DPI for radar rainfall weight
  TYPE (ElementHeader_type)      :: z            ! DPI for height
  TYPE (ElementHeader_type)      :: t            ! DPI for temperature
  TYPE (ElementHeader_type)      :: td           ! DPI for dew point temp
  TYPE (ElementHeader_type)      :: theta        ! DPI for theta
  TYPE (ElementHeader_type)      :: rh           ! DPI for rh
  TYPE (ElementHeader_type)      :: MixingRatio  ! DPI for mixing ratio
  TYPE (ElementHeader_type)      :: q            ! DPI for specific humidity
  TYPE (ElementHeader_type)      :: ddd          ! DPI for ddd & fff
  TYPE (ElementHeader_type)      :: fff          ! DPI for ddd & fff
  TYPE (ElementHeader_type)      :: u            ! DPI for u
  TYPE (ElementHeader_type)      :: v            ! DPI for v
  TYPE (ElementHeader_type)      :: w            ! DPI for w
  TYPE (ElementHeader_type)      :: Salt         ! DPI for salinity
  TYPE (ElementHeader_type)      :: BriTemp      ! DPI for brightness temps
  TYPE (ElementHeader_type)      :: NumChans     ! DPI for number of channels
  TYPE (ElementHeader_type)      :: ChanNum      ! DPI for channel numbers
  TYPE (ElementHeader_type)      :: ChannelWavenumber ! DPI for channel wavenumber
  TYPE (ElementHeader_type)      :: CLW          ! DPI for cloud liquid water
  TYPE (ElementHeader_type)      :: CIW          ! DPI for cloud ice water
  TYPE (ElementHeader_type)      :: Cloud        ! DPI for cloud
  TYPE (ElementHeader_type)      :: ozone        ! DPI for ozone
  TYPE (ElementHeader_type)      :: Refrac       ! DPI for GPSRO OBSERVED refractivity
  TYPE (ElementHeader_type)      :: SolutRefrac  ! DPI for GPSRO refractivity (1DVAR retrieval)
  TYPE (ElementHeader_type)      :: SolutBendingAngle ! DPI for GPSRO refractivity (1DVAR retrieval)
  TYPE (ElementHeader_type)      :: BendingAngleAll ! DPI for GPSRO bending angle (L1,L2,LC etc)
  TYPE (ElementHeader_type)      :: BendingAngleOrig ! DPI for GPSRO bending angle (L1,L2,LC etc)
  TYPE (ElementHeader_type)      :: BendingAngle    ! DPI for GPSRO bending angle just LC
  TYPE (ElementHeader_type)      :: RO_mean_freq    ! DPI for GPSRO mean frequency Hz
  TYPE (ElementHeader_type)      :: ImpactParamAll ! DPI for GPSRO impact parameter (L1,L2,LC etc)
  TYPE (ElementHeader_type)      :: ImpactParamOrig ! DPI for GPSRO impact parameter (L1,L2,LC etc)
  TYPE (ElementHeader_type)      :: ImpactParam  ! DPI for GPSRO impact parameter just LC
  TYPE (ElementHeader_type)      :: gpsro_orig_ob  ! DPI for GPSRO impact parameter just LC
  TYPE (ElementHeader_type)      :: gpsro_orig_level  ! DPI for GPSRO impact parameter just LC
  TYPE (ElementHeader_type)      :: p            ! DPI for GPSRO for pressure
  TYPE (ElementHeader_type)      :: Altitude     ! DPI for altitude
  TYPE (ElementHeader_type)      :: RadarObRange  ! DPI for Range of radar wind ob
  TYPE (ElementHeader_type)      :: RadarObAzim   ! DPI for Azimuth of radar wind ob
  TYPE (ElementHeader_type)      :: RadarBeamElev ! DPI for Elevation of radar beam
  TYPE (ElementHeader_type)      :: RadialVelocity! DPI for Radial velocity ob
  TYPE (ElementHeader_type)      :: Reflectivity  ! DPI for Radar Reflectivity ob
  TYPE (ElementHeader_type)      :: ReflectivityR  ! DPI for Radar Reflectivity from rain ob
  TYPE (ElementHeader_type)      :: ReflectivityI  ! DPI for Radar Reflectivity from ice ob
  TYPE (ElementHeader_type)      :: RadarObRangSO ! DPI for Range of radwind superob
  TYPE (ElementHeader_type)      :: RadarObAzimSO ! DPI for Azimuth of radwind superob
  TYPE (ElementHeader_type)      :: RadialVelocSO ! DPI for Radial velocity superob
  TYPE (ElementHeader_type)      :: ReflectivitySO ! DPI for Radar Reflectivity superob
  TYPE (ElementHeader_type)      :: RadIdent      ! DPI for Radar Identifier
  TYPE (ElementHeader_type)      :: RadSiteLat       ! DPI for Radar Latitude
  TYPE (ElementHeader_type)      :: RadSiteLong      ! DPI for Radar Longitude
  TYPE (ElementHeader_type)      :: RadAltAboveMSL   ! DPI for Radar Identifier
  TYPE (ElementHeader_type)      :: RadNoiseLvl      ! DPI for Radar Identifier
  TYPE (ElementHeader_type)      :: RadSW            ! DPI for spectral width of radar ob
  TYPE (ElementHeader_type)      :: RadNbGate       ! DPI munber of gate with a ray
  TYPE (ElementHeader_type)      :: RadGateRes     ! DPI Scan Range resolution [m]
  TYPE (ElementHeader_type)      :: RadRayRes      ! DPI Ray resolution [degree]
  TYPE (ElementHeader_type)      :: RadNbRay        ! DPI number of ray within the scan
  TYPE (ElementHeader_type)      :: RadFlag         ! DPI Flag for valid data within a scan
  TYPE (ElementHeader_type)      :: RadUnfoldVelocity ! DPI Unfolding velocity for doppler wind
  TYPE (ElementHeader_type)      :: RadRangeStart   ! DPI Start of the recording along the Ray [m]
  TYPE (ElementHeader_type)      :: RadPhase        ! DPI for phase measurements
  TYPE (ElementHeader_type)      :: RadNcoFreq      ! DPI for numerical oscilator freq
  TYPE (ElementHeader_type)      :: RadPhaseSDP     !
  TYPE (ElementHeader_type)      :: RadRefractivity ! DPI for refractivity (derived)
  TYPE (ElementHeader_type)      :: AUTOSATSpCTest  ! Spatial coherence flag
  TYPE (ElementHeader_type)      :: AUTOSATCTP      ! DPI for Cloud top pressure
  TYPE (ElementHeader_type)      :: AUTOSATECA      ! DPI for Effective cloud amount
  TYPE (ElementHeader_type)      :: Collection_Centre ! DPI for collection centre
  TYPE (ElementHeader_type)      :: NEDTCold         ! DPI for ATMS cold nedt
  TYPE (ElementHeader_type)      :: NEDTWarm         ! DPI for ATMS warm nedt
  TYPE (ElementHeader_type)      :: AAPPCirrus       ! DPI for AAPP cirrus test
  TYPE (ElementHeader_type)      :: AAPPBennartz     ! DPI for AAPP bennartz test
  TYPE (ElementHeader_type)      :: AOD               ! Aerosol Optical Depth
  TYPE (ElementHeader_type)      :: AODError          ! Aerosol Optical Depth Error
  TYPE (ElementHeader_type)      :: AngstromExponent  ! Aerosol Angstrom Exponennt
  TYPE (ElementHeader_type)      :: MassConcentration ! Aerosol Mass Concentration
  TYPE (ElementHeader_type)      :: SmallModeFraction ! Aerosol small-mode fraction
  TYPE (ElementHeader_type)      :: EffectiveRadius   ! Aerosol Effective Radius
  TYPE (ElementHeader_type)      :: AerosolType       ! Aerosol Types
  TYPE (ElementHeader_type)      :: AerosolConf       ! Aerosol Retrieval confidence flag
  TYPE (ElementHeader_type)      :: AtmrNmbr         ! DPI for sferic (lightning flash) intensity indicator
  TYPE (ElementHeader_type)      :: LowestCloudBaseHeight ! Lowest cloud base height
  TYPE (ElementHeader_type)      :: MediumCloudType       ! Medium cloud type
  TYPE (ElementHeader_type)      :: HighCloudType         ! High cloud type
  TYPE (ElementHeader_type)      :: CloudAmount8Group     ! Cloud amount in 8-groups
  TYPE (ElementHeader_type)      :: CloudType8Group       ! Cloud type in 8-groups
  TYPE (ElementHeader_type)      :: CloudBaseHeight8Group ! Cloud base height in 8-groups
  TYPE (ElementHeader_type)      :: LghnStrk         ! DPI for ATDnet lightning flash/stroke indicator
  TYPE (ElementHeader_type)      :: TAMDAR_qc_t      ! TAMDAR qc flag for temperature
  TYPE (ElementHeader_type)      :: TAMDAR_qc_ddd    ! TAMDAR qc flag for wind direction
  TYPE (ElementHeader_type)      :: TAMDAR_qc_fff    ! TAMDAR qc flag for wind speed
  TYPE (ElementHeader_type)      :: TAMDAR_qc_rh     ! TAMDAR uncertainty estimate for rh
  TYPE (ElementHeader_type)      :: station_report_type
  TYPE (ElementHeader_type)      :: TransponderAddress
  TYPE (ElementHeader_type)      :: AircraftType
  TYPE (ElementHeader_type)      :: AircraftGNSSAltitude
  TYPE (ElementHeader_type)      :: AircraftIndicatedAirSpeed
  TYPE (ElementHeader_type)      :: AircraftTrueAirSpeed
  TYPE (ElementHeader_type)      :: AircraftGroundSpeed
  TYPE (ElementHeader_type)      :: AircraftTrueHeading
  TYPE (ElementHeader_type)      :: AircraftGroundHeading
  TYPE (ElementHeader_type)      :: AircraftRadarGroundSpeed
  TYPE (ElementHeader_type)      :: AircraftRadarGroundHeading
  TYPE (ElementHeader_type)      :: MachNumber
  TYPE (ElementHeader_type)      :: AircraftRadarT
  TYPE (ElementHeader_type)      :: AircraftGeomagneticDeviation
  TYPE (ElementHeader_type)      :: AircraftMagneticHeadingBias
  TYPE (ElementHeader_type)      :: AircraftRollAngle
  TYPE (ElementHeader_type)      :: AircraftRadarDDD
  TYPE (ElementHeader_type)      :: AircraftRadarU
  TYPE (ElementHeader_type)      :: AircraftRadarV
  TYPE (ElementHeader_type)      :: RainAccum
  TYPE (ElementHeader_type)      :: SiteType
  TYPE (ElementHeader_type)      :: CeilBackscatter
  TYPE (ElementHeader_type)      :: CeilCBH
  TYPE (ElementHeader_type)      :: CeilRange
  TYPE (ElementHeader_type)      :: OpenRoadWindSpeed
  TYPE (ElementHeader_type)      :: OpenRoadWindDirection
  TYPE (ElementHeader_type)      :: OpenRoadTemp
  TYPE (ElementHeader_type)      :: OpenRoadDewPointTemp
  TYPE (ElementHeader_type)      :: OpenRoadVisibility
  TYPE (ElementHeader_type)      :: CeilSiteID
  TYPE (ElementHeader_type)      :: CeilScanIdent
  TYPE (ElementHeader_type)      :: CeilLPE
  TYPE (ElementHeader_type)      :: CeilWindowTau
  TYPE (ElementHeader_type)      :: ArgoPositionQCFlag
  TYPE (ElementHeader_type)      :: ArgoTemperatureQCFlag
  TYPE (ElementHeader_type)      :: ArgoPressureQCFlag
  TYPE (ElementHeader_type)      :: ArgoSalinityQCFlag
  TYPE (ElementHeader_type)      :: HLOSwind
  TYPE (ElementHeader_type)      :: ProfileNo
  TYPE (ElementHeader_type)      :: dWinddT
  TYPE (ElementHeader_type)      :: dWinddP
  TYPE (ElementHeader_type)      :: AzimuthCOG
  TYPE (ElementHeader_type)      :: HeightCOG
  TYPE (ElementHeader_type)      :: HeightTop
  TYPE (ElementHeader_type)      :: HeightBottom
  TYPE (ElementHeader_type)      :: WindErr
  TYPE (ElementHeader_type)      :: ChannelNumber
  TYPE (ElementHeader_type)      :: LidarClass
  TYPE (ElementHeader_type)      :: AladinConfFlag
  TYPE (Elementheader_type)      :: WindErrRejLimit
  TYPE (ElementHeader_type)      :: buoy_report_type
  TYPE (ElementHeader_type)      :: time_difference
  TYPE (ElementHeader_type)      :: xbt_depth_factor
  TYPE (ElementHeader_type)      :: background_temperature
  TYPE (ElementHeader_type)      :: background_salt
END TYPE OBheader_type

!-------------------------------------------------------------------
! OB type
!-----------------------------------
! There is a one to one correspondence between the items
! in OBheader and OB

INTEGER, PARAMETER :: LenCallSign = 16
INTEGER, PARAMETER :: LenVesselInfo = 8
INTEGER, PARAMETER :: LenTailNumber = 8     ! } Used in Ob_type
INTEGER, PARAMETER :: LenSiteName = 32
INTEGER, PARAMETER :: LenStnName = 32
INTEGER, PARAMETER :: LenGEMSName = 8
INTEGER, PARAMETER :: LenColCentre = 8
INTEGER, PARAMETER :: LenTransponderAddress = 8
INTEGER, PARAMETER :: LenAircraftType = 8

TYPE OB_type
  TYPE (OBheader_type)                   :: header
  REAL, POINTER                          :: Latitude(:) => NULL()     ! Degrees (-90 to 90)
  REAL, POINTER                          :: Longitude(:) => NULL()    ! Degrees (-180 to 180)
  INTEGER, POINTER                       :: Family(:) => NULL()
  REAL, POINTER                          :: Time(:) => NULL()         ! Seconds after ValidityTime
  INTEGER, POINTER                       :: ReceiptTime(:) => NULL()  ! Seconds after ValidityTime
  INTEGER, POINTER                       :: GTScentre(:) => NULL()
  INTEGER, POINTER                       :: ObsType(:) => NULL()
  INTEGER, POINTER                       :: SubTYPE(:) => NULL()
  INTEGER, POINTER                       :: Id(:) => NULL()           ! Unique id for each ob
  CHARACTER(len=LenCallSign), POINTER    :: CallSign(:) => NULL()
  CHARACTER(len=LenStnName),  POINTER    :: LNG_STTN_NAME(:) => NULL()
  CHARACTER(len=LenGEMSName), POINTER    :: GEMS_AQTY_OBSG_STTN(:) => NULL()
  INTEGER, POINTER                       :: SatId(:) => NULL()        ! Satellite id
  REAL, POINTER                          :: ReportPGE(:) => NULL()
  INTEGER, POINTER                       :: ReportFlags(:) => NULL()
  INTEGER, POINTER                       :: AssimFlags(:) => NULL()
  REAL, POINTER                          :: ModelOrog(:) => NULL()
  REAL, POINTER                          :: ModelSurface(:) => NULL()
  REAL, POINTER                          :: ModelSeaIce(:) => NULL()
  REAL, POINTER                          :: ModelCloudAmount(:) => NULL()
  INTEGER, POINTER                       :: SatobCompMethod(:) => NULL()
  INTEGER, POINTER                       :: ThinningScore(:) => NULL()
  INTEGER, POINTER                       :: ThinningRounds(:) => NULL()
  INTEGER, POINTER                       :: ThinningCall(:) => NULL() ! stores in which call to ops_thinning ob is rej
  INTEGER, POINTER                       :: SatInst(:) => NULL()      ! satellite instrument
  REAL, POINTER                          :: MWEmissAtlas(:,:) => NULL()
  REAL, POINTER                          :: MWEmErrAtlas(:,:) => NULL()
  INTEGER, POINTER                       :: ObNumber(:) => NULL()     ! Altimeter
  REAL, POINTER                          :: SSTRejFlag(:) => NULL()   ! SST Rejection Flag
  REAL, POINTER                          :: SSTBias(:) => NULL()      ! SST bias
  REAL, POINTER                          :: SSTStdev(:) => NULL()     ! SST standard deviation
  INTEGER, POINTER                       :: SSTQuality(:) => NULL()   ! SST Quality code
  INTEGER, POINTER                       :: SSTAlgrType(:) => NULL()  ! SST Algorithm Type -  only for SLSTR SST
  REAL, POINTER                          :: SOLR_RDTN_FLUX (:) => NULL()          ! Solar flux         W/m
  REAL, POINTER                          :: TIME_PERD_SOLR_RDTN_FLUX(:) => NULL() !Solar flux time delay hrs
  CHARACTER(len=LenVesselInfo), POINTER  :: VesselInfo(:) => NULL()
  INTEGER, POINTER                       :: ObPractice(:) => NULL()  !
  INTEGER, POINTER                       :: Instruments(:) => NULL() ! SST only?
  REAL, POINTER                          :: Zstation(:) => NULL()    ! station elevation (m)
  REAL, POINTER                          :: Zstandard(:) => NULL()   ! elevation (m) of Pstandard =0 if Pstandard = Pmsl
  REAL, POINTER                          :: AnHeight(:) => NULL()    ! anemometer height (m)
  REAL, POINTER                          :: LapseRate(:) => NULL()   ! used for height adjust (deg/m)
  REAL, POINTER                          :: CoastDist(:) => NULL()   ! distance from coast (km, sea>0)
  REAL, POINTER                          :: HeightRange(:) => NULL() ! ht range in model template (m)
  REAL, POINTER                          :: zstncorrected(:) => NULL()   ! corrected station height
  INTEGER, POINTER                       :: SondeTYPE(:) => NULL()       ! Sonde instrument type
  INTEGER, POINTER                       :: SondeReportType(:) => NULL() ! Sonde report type
  REAL, POINTER                          :: HawsonCor(:) => NULL()       ! Hawson correction
  REAL, POINTER                          :: Hawson00Cor(:) => NULL()     ! Hawson 00z correction
  REAL, POINTER                          :: Hawson12Cor(:) => NULL()     ! Hawson 12z correction
  INTEGER, POINTER                       :: Nlevels(:) => NULL()         ! Number of levels
  INTEGER, POINTER                       :: LevelType(:,:) => NULL()     ! Level type (significant, standard etc.)
  INTEGER, POINTER                       :: WinProQCFlags(:,:) => NULL() ! reported wind profiler qc info
  CHARACTER(len=LenTailNumber),  POINTER :: TailNumber(:) => NULL()      ! aircraft
  INTEGER, POINTER                       :: FlightPhase(:) => NULL()     ! aircraft
  REAL, POINTER                          :: FlightLevel(:) => NULL()     ! aircraft
  INTEGER, POINTER                       :: AcarsStatus(:) => NULL()     ! aircraft
  INTEGER, POINTER                       :: RcptStn(:) => NULL()         ! ground station id
  INTEGER, POINTER                       :: ScanLine(:) => NULL()        ! scanline number
  INTEGER, POINTER                       :: ScanPosition(:) => NULL()    ! scan position
  INTEGER, POINTER                       :: surface(:) => NULL()         ! type from the report
  INTEGER, POINTER                       :: ChnlQlty(:,:) => NULL()      ! Channel quality flags
  REAL, POINTER                          :: elevation(:) => NULL()       ! elevation (m) from report
  REAL, POINTER                          :: LocalZenith(:) => NULL()     ! local zenith angle
  REAL, POINTER                          :: LocalAzimuth(:) => NULL()    ! local azimuth angle
  REAL, POINTER                          :: SatView(:) => NULL()         ! satellite viewing angle
  REAL, POINTER                          :: SatZenithAngle(:) => NULL()  ! sat zenith angle
  REAL, POINTER                          :: SatAzimth(:) => NULL()       ! satellite azimuth angle
  REAL, POINTER                          :: SolarZenith(:) => NULL()     ! solar zenith angle
  REAL, POINTER                          :: SolarAzimth(:) => NULL()     ! solar azimuth angle
  INTEGER, POINTER                       :: NIter(:) => NULL()      ! number of iterations (satellite 1dvar)
  REAL, POINTER                          :: CldCost(:) => NULL()    ! 1DVAR cloud detection (satellite 1dvar)
  REAL, POINTER                          :: CirCost(:) => NULL()    ! 1DVAR cirrus detection (satellite 1dvar)
  REAL, POINTER                          :: SI150(:) => NULL()      ! 1DVAR cirrus detection (satellite 1dvar)
  REAL, POINTER                          :: SI183(:) => NULL()      ! 1DVAR cirrus detection (satellite 1dvar)
  INTEGER, POINTER                       :: QCflags(:) => NULL()    ! 1DVAR QC flags (satellite 1dvar)
  INTEGER, POINTER                       :: QCscan(:) => NULL()     ! qual indicator bit field (satellite 1dvar)
  INTEGER, POINTER                       :: QCprepro(:) => NULL()   ! flags from the pre-processing (satellite 1dvar)
  INTEGER, POINTER                       :: QCinfo(:) => NULL()     ! 1DVAR auxiliary QC variable (satellite 1dvar)
  INTEGER, POINTER                       :: QCarea(:) => NULL()     ! 1DVAR auxiliary QC variable (satellite 1dvar)
  REAL, POINTER                          :: TCozone_init(:) => NULL() ! Initial total column ozone (Du) (satellite 1dvar)
  REAL, POINTER                          :: TCozone(:) => NULL()    ! total column ozone (Du) (satellite 1dvar)
  REAL, POINTER                          :: IREmiss(:) => NULL()    ! Monochromatic IR surf. emissivity (satellite 1dvar)
  REAL, POINTER                          :: Emissivity(:,:) => NULL() ! surface emissivity (satellite 1dvar)
  REAL, POINTER                          :: MwEmiss(:,:) => NULL()  ! microwave surface emissivities (satellite 1dvar)
  REAL, POINTER                          :: MwEmErr(:,:) => NULL()  ! microwave emissivity error from atlas
  INTEGER, POINTER                       :: CalcEmiss(:,:) => NULL() ! whether emiss is fixed or not (satellite 1dvar)
  REAL, POINTER                          :: EmissPC(:,:) => NULL()      ! surf emissivity PCs (satellite 1dvar)
  REAL, POINTER                          :: CloudFrac(:) => NULL()      ! fractional cloud cover (satellite 1dvar)
  REAL, POINTER                          :: CloudFrac_init(:) => NULL() ! initial fractional cloud cover (satellite 1dvar)
  REAL, POINTER                          :: CloudTopTemp(:) => NULL()   ! Cloud top temperature (satellite 1dvar)
  REAL, POINTER                          :: CloudTopP(:) => NULL()      ! cloud top pressure (satellite 1dvar)
  REAL, POINTER                          :: CloudTopPErr(:) => NULL()   ! cloud top pressure obs uncertainty (satellite 1dvar)
  REAL, POINTER                          :: CloudTopP_init(:) => NULL() ! initial cloud top pressure (satellite 1dvar)
  INTEGER, POINTER                       :: NumClrPriChans(:) => NULL() ! Number of channels passing prior cloud test
  REAL, POINTER                          :: ObBriTemp(:,:) => NULL()    ! observed brightness (satellite 1dvar)
  REAL, POINTER                          :: CorBriTemp(:,:) => NULL()   ! bias-corrected brightness (satellite 1dvar)
  REAL, POINTER                          :: InitBriTemp(:,:) => NULL()  ! Intial background britemp (satellite 1dvar)
  REAL, POINTER                          :: ClearBriTemp(:,:) => NULL() ! Clear background britemp (satellite 1dvar)
  REAL, POINTER                          :: JCost(:) => NULL()          ! JCost (satellite 1dvar)
  REAL, POINTER                          :: BiasPredictors(:,:) => NULL() ! Bias predict (satellite 1dvar)ors
  REAL, POINTER                          :: BiasBriTemp(:,:) => NULL()  ! bias-correction brightness (satellite 1dvar)
  REAL, POINTER                          :: TauSurf(:,:) => NULL()      ! analysed surface transmittance (satellite 1dvar)
  REAL, POINTER                          :: BriTempVarError(:,:) => NULL() ! Variable Obs Error (satellite 1dvar)
  REAL, POINTER                          :: CloudRTError(:,:) => NULL()  ! IR cloud RT error (satellite 1dvar)
  REAL, POINTER                          :: CloudRTBias(:,:) => NULL()   ! IR cloud RT bias (satellite 1dvar)
  REAL, POINTER                          :: CloudEffectAvg(:,:) => NULL()! IR cloud RT bias (satellite 1dvar)
  INTEGER, POINTER                       :: VnATOVPP(:) => NULL()        ! version no. of the pre-processing (atovs)
  INTEGER, POINTER                       :: AMSUsurface(:) => NULL()     ! estimated AMSU surface type (atovs)
  REAL, POINTER                          :: AMSUcldcost(:) => NULL()     ! AMSU cloud cost result (atovs)
  REAL, POINTER                          :: AMSUscatindx(:) => NULL()    ! AMSU scattering index (atovs)
  INTEGER, POINTER                       :: ATOVSProcOption(:) => NULL() ! processing options from 1DVAR (atovs)
  INTEGER, POINTER                       :: QCHIRSfov(:) => NULL()       ! HIRS flags from ATOVPP (atovs)
  INTEGER, POINTER                       :: QCGIIRSfov(:) => NULL()      ! GIIRSflags 
  REAL, POINTER                          :: HIRS_Temp(:) => NULL()       ! HIRS instrument temp (K) (atovs)
  REAL, POINTER                          :: AMSUa1_Temp(:) => NULL()     ! AMSU-A1 instrument temp (K) (atovs)
  REAL, POINTER                          :: AMSUa2_Temp(:) => NULL()     ! AMSU-A2 instrument temp (K) (atovs)
  REAL, POINTER                          :: AMSUb_Temp(:) => NULL()      ! AMSU-B instrument temp (K) (atovs)
  INTEGER, POINTER                       :: QCIASIfov(:) => NULL()       ! QC for each IASI fov from AAPP (iasi)
  INTEGER, POINTER                       :: QCIASIGQis(:) => NULL()      ! General Quality Flag =0 if data OK (iasi)
  INTEGER, POINTER                       :: ScaleFacStartChan(:,:) => NULL() ! IASI data comes as scaled rads (iasi)
  INTEGER, POINTER                       :: ScaleFacEndChan(:,:) => NULL() ! where radiance = scaled * 10**fac (iasi)
  INTEGER, POINTER                       :: ScaleFac(:,:) => NULL()      ! The factor varies across channels (iasi)
  REAL, POINTER                          :: ScaledRadiance(:,:) => NULL() ! Scaled-down radiance (iasi)
  REAL, POINTER                          :: Radiance(:,:) => NULL()      ! Radiance after scaling back up (iasi)
  INTEGER, POINTER                       :: ChannelVersion(:) => NULL()  ! Channel Selection version no. (iasi)
  REAL, POINTER                          :: PCScore(:,:) => NULL()       ! Principal Component scores (iasi)
  INTEGER, POINTER                       :: PCVersion(:) => NULL()       ! Principal Components version (iasi)
  REAL, POINTER                          :: QCIASIPC(:,:) => NULL()      ! QC for recon radiance Bands1-3 (iasi)
  INTEGER, POINTER                       :: PixelNumber(:) => NULL()     ! Pixel number (1 to 4) (iasi)
  REAL, POINTER                          :: CloudFree(:,:) => NULL()     ! Channel cloud free fraction (seviri)
  REAL, POINTER                          :: CloudFrac_arr(:,:) => NULL() ! Channel cloud fraction (mviri)
  INTEGER, POINTER                       :: SSMISScanType(:) => NULL()   ! SSMI/S scan data type (ssmis)
  INTEGER, POINTER                       :: SrfcFlag(:) => NULL()        ! SSMI/S surface flag (ssmis)
  INTEGER, POINTER                       :: SurfType(:) => NULL()        ! redefined SSMI/S surface type indicator (ssmis)
  INTEGER, POINTER                       :: RainFlag(:) => NULL()        ! SSMI/S rain flag (ssmis)
  INTEGER, POINTER                       :: RainType(:) => NULL()        ! redefined SSMI/S rain indicator (ssmis)
  REAL, POINTER                          :: EphemLat(:,:) => NULL()      ! SSMI/S Ephemeris data latitude (ssmis)
  REAL, POINTER                          :: EphemLon(:,:) => NULL()      ! SSMI/S Ephemeris data longitude (ssmis)
  REAL, POINTER                          :: EphemHeight(:,:) => NULL()   ! SSMI/S Ephemeris data height (ssmis)
  REAL, POINTER                          :: BasePontLat(:,:) => NULL()   ! SSMI/S Base point latitude (ssmis)
  REAL, POINTER                          :: BasePontLon(:,:) => NULL()   ! SSMI/S Base point longitude (ssmis)
  REAL, POINTER                          :: BasePontIncAng(:,:) => NULL() ! SSMI/S Base point incidence angle (ssmis)
  REAL, POINTER                          :: BasePontAzhAng(:,:) => NULL() ! SSMI/S Base point azimuthal angle (ssmis)
  REAL, POINTER                          :: MagneticFieldBe(:,:) => NULL() ! SSMI/S Geomagnetic field (ssmis)
  REAL, POINTER                          :: ViewingVectorK(:,:) => NULL()  ! SSMI/S radiometer vieiwing vector (ssmis)
  REAL, POINTER                          :: BdotK(:) => NULL()             ! SSMI/S Magnitude B.K  (ssmis)
  REAL, POINTER                          :: thetaBK(:) => NULL()           ! SSMI/S Angle of B.K  (ssmis)
  REAL, POINTER                          :: SatOrbitAngle(:) => NULL()    ! Satellite orbit/along-track angle (ssmis)
  REAL, POINTER                          :: AMSUZenAng(:) => NULL()      ! AMSU zenith angles (airs)
  INTEGER, POINTER                       :: SatCloudTestsUsed(:) => NULL() ! Satellite cloud tests used (airs)
  INTEGER, POINTER                       :: SatCloudFlags(:) => NULL()     ! Satellite cloud flags (airs)
  REAL, POINTER                          :: SurfWaterFraction(:) => NULL() ! Surface water fraction (airs)
  INTEGER, POINTER                       :: MainSurfaceType(:) => NULL()   ! Main surface type (airs)
  REAL, POINTER                          :: MainSurfFraction(:) => NULL() ! Fraction of main surface type (airs)
  REAL, POINTER                          :: AIRSVisMeanAlb(:,:) => NULL() ! Mean albedo of AIRS visible channels.
  REAL, POINTER                          :: AIRSVisStdDevAlb(:,:) => NULL()! Std. dev. of albedo of AIRS visible channels.
  INTEGER, POINTER                       :: CloudyChannel(:,:) => NULL() ! Flag indicating significant cloud signal (airs)
  REAL, POINTER                          :: NoDualPixels(:) => NULL()    ! No. of dual pixels (aatsr)
  INTEGER, POINTER                       :: SSTCnfd(:) => NULL()         ! SST confidence (aatsr)
  CHARACTER(len=LenSiteName), POINTER    :: SITE_NAME(:) => NULL()               ! GPS site name
  TYPE (Element_type), POINTER           :: GPSTZDelay(:) => NULL()              ! GPS total zenith delay
  REAL, POINTER                          :: GPSTZDelayError(:) => NULL()         ! GPS total zenith delay error
  REAL, POINTER                          :: GPS_MaxHeightDifference(:) => NULL() ! GPS max height difference threshold
  REAL, POINTER                          :: GPS_QC_Threshold(:) => NULL()        ! GPS O-B QC threshold
  INTEGER, POINTER                       :: GPS_MinNumObs(:) => NULL()           ! GPS min num obs/station/month
  REAL, POINTER                          :: GPS_TempThinList(:,:) => NULL()      ! GPS temporal thinning info
  REAL, POINTER                          :: GPSRO_Zmin(:) => NULL()       ! GPSRO min assimilation height threshold
  REAL, POINTER                          :: GPSRO_Zmax(:) => NULL()       ! GPSRO max assimilation height threshold
  INTEGER, POINTER                       :: SoftwareIdentity(:) => NULL() ! GPSRO proc software ID
  INTEGER, POINTER                       :: RO_Quality(:) => NULL()       ! GPSRO quality indicator
  INTEGER, POINTER                       :: RO_Confidence(:) => NULL()    ! GPSRO confidence
  TYPE (Element_type), POINTER           :: RO_geoid_und(:) => NULL() ! GPSRO Geoid undulation (above WGS-84 ellipsoid, m)
  TYPE (Element_type), POINTER           :: RO_Rad_Curv(:) => NULL()  ! GPSRO Earth's local radius of curvature   m
  REAL, POINTER                          :: level_time(:,:) => NULL()   ! level dependent time
  REAL, POINTER                          :: level_lat(:,:) => NULL()    ! level dependent latitude
  REAL, POINTER                          :: level_lon(:,:) => NULL()    ! level dependent longitude
  REAL, POINTER                          :: model_level_time(:,:) => NULL()   ! level dependent time
  REAL, POINTER                          :: model_level_lat(:,:) => NULL()    ! level dependent latitude
  REAL, POINTER                          :: model_level_lon(:,:) => NULL()    ! level dependent longitude
  REAL, POINTER                          :: level_azimuth(:,:) => NULL()    ! level dependent azimuth
  REAL, POINTER                          :: ro_occ_lat(:) => NULL()  ! Original lat of occultation point
  REAL, POINTER                          :: ro_occ_lon(:) => NULL()  ! Original lon of occultation point
  INTEGER, POINTER                       :: GPSRO_heightcorrection(:) => NULL() ! GPSRO height cor. switch
  REAL, POINTER                          :: GPSRO_cost_funct_test(:) => NULL()  ! Threshold value for the cost function convergence
  REAL, POINTER                          :: GPSRO_y_test(:) => NULL()           ! Threshold value for the yobs-ysol tes
  INTEGER, POINTER                       :: GPSRO_n_iteration_test(:) => NULL() ! Maximum number of iterations
  REAL, POINTER                          :: GPSRO_Delta_factor(:) => NULL()     ! Delta
  REAL, POINTER                          :: GPSRO_Delta_ct2(:) => NULL()     ! Delta for ct2 convergence criteria
  REAL, POINTER                          :: GPSRO_OB_test(:) => NULL()       ! Delta for ct2 convergence criteria
  INTEGER, POINTER                       :: GPSRO_qc_mask(:) => NULL()       ! Integer mask used to control quality flags
  REAL, POINTER                          :: GPSRO_max_grad(:) => NULL()      ! Max back vertical N gradient
  REAL, POINTER                          :: GPSRO_hgt_shift(:) => NULL()     ! Shift in lower impact above max N gradient
  INTEGER, POINTER                       :: GnssClass(:) => NULL()           ! GNSS classification (i.e. GPS, Beidou, etc.)
  INTEGER, POINTER                       :: GnssId(:) => NULL()              ! GNSS ID (satellite ID for GPS, Beidou, etc.)
  INTEGER, POINTER                       :: ESAPCD(:) => NULL()         ! ESA Product Confidence Word (scatwind)
  INTEGER, POINTER                       :: WindRetPCD(:) => NULL()     ! Wind Retrieval Confidence Word (scatwind)
  REAL, POINTER                          :: BeamIncAngle(:,:) => NULL() ! beam incidence angles (degrees) (scatwind)
  REAL, POINTER                          :: BeamLookAngle(:,:) => NULL() ! beam look angles (degrees) (scatwind)
  REAL, POINTER                          :: BackScatter(:,:) => NULL()  ! beam back scatter (dB) (scatwind)
  REAL, POINTER                          :: BeamNoise(:,:) => NULL()    ! beam noise (%) (scatwind)
  INTEGER, POINTER                       :: MissShot(:,:) => NULL()     ! beam missing shot (scatwind)
  REAL, POINTER                          :: DistConeRank1(:) => NULL()  ! residual for the rank1 wind (scatwind)
  REAL, POINTER                          :: DistConeRank2(:) => NULL()  ! residual for the rank2 wind (scatwind)
  REAL, POINTER                          :: WindRetSkill(:) => NULL()   ! skill in the wind rerieval (scatwind)
  INTEGER, POINTER                       :: ScatModel(:) => NULL() ! indicates which back scatter model (scatwind)
  REAL, POINTER                          :: fff10AmbWind(:,:) => NULL() ! ambiguous wind speed (m/s) (scatwind)
  REAL, POINTER                          :: ddd10AmbWind(:,:) => NULL() ! ambiguous wind direction (deg) (scatwind)
  REAL, POINTER                          :: DistCone(:,:) => NULL()     ! retrieval residual for each wind (scatwind)
  INTEGER, POINTER                       :: ChosenWind(:) => NULL()     ! indicates which wind is the correct one (1->4) (scatwind)
  REAL, POINTER                          :: ScatRainProb(:) => NULL()   ! probability that rain has contaminated the retrieval
  INTEGER, POINTER                       :: NumBeams(:) => NULL()       ! number of beams used in retrieval (scatwind)
  REAL, POINTER                          :: KuSolnLkhd(:,:) => NULL()   ! Solution likelihood (KuBand scatwind)
  REAL, POINTER                          :: ASCATL2WindSoftwareID(:) => NULL() ! Software id
  REAL, POINTER                          :: ASCATIceProb(:) => NULL() ! Ice probability
  REAL, POINTER                          :: ASCATIceAgeA(:) => NULL() ! Ice age A-parameter
  INTEGER, POINTER                       :: ASCATQCflag(:) => NULL()  ! QC flag for ASCAT
  INTEGER, POINTER                       :: ASCATNumberVectorAmby(:) => NULL() ! Number of ambiguities
  REAL, POINTER                          :: SatDirectionOfMotion(:) => NULL()  ! Direction of satellite
  INTEGER, POINTER                       :: ASCATWindRepCount(:) => NULL()     ! Wind replication count
  REAL, POINTER                          :: ASCATBackDstc(:,:) => NULL()       ! Distance to cone
  REAL, POINTER                          :: ASCATSolnLkhd(:,:) => NULL()       ! Solution likelihood
  INTEGER, POINTER                       :: WINDSATSurfType(:) => NULL()       ! surface type
  REAL, POINTER                          :: WINDSATWaterVapourContent(:) => NULL()  ! water vapour
  REAL, POINTER                          :: WINDSATLiquidWaterContent(:) => NULL()  ! liquid water
  REAL, POINTER                          :: WINDSATPrecipRate(:) => NULL()     ! rain rate
  REAL, POINTER                          :: WINDSATSeaSurfTempError(:) => NULL()  ! SST error
  REAL, POINTER                          :: WINDSATWindSpeedError(:) => NULL()    ! speed error
  REAL, POINTER                          :: WINDSATWaterVapourError(:) => NULL()  ! vapour error
  REAL, POINTER                          :: WINDSATLiquidWaterError(:) => NULL()  ! water error
  REAL, POINTER                          :: WINDSATCHI2WindRetrieval(:,:) => NULL() ! CHI squared
  REAL, POINTER                          :: WINDSATWindDirectionError(:,:) => NULL() ! direction error
  INTEGER, POINTER                       :: WINDSATQCflagA(:) => NULL()  ! quality flag
  INTEGER, POINTER                       :: WINDSATQCflagB(:) => NULL()  ! quality flag
  INTEGER, POINTER                       :: SuperobNused(:) => NULL() ! number of obs in superob
  INTEGER, POINTER                       :: SuperobStart(:) => NULL() ! first ObNumber in superob
  TYPE (coord_type), POINTER             :: PlevelsA(:,:) => NULL()
  TYPE (coord_type), POINTER             :: PlevelsB(:,:) => NULL()
  INTEGER, POINTER                       :: CloudClearMthd(:) => NULL() ! cloud clearing method (ukmosst)
  REAL, POINTER                          :: StdDevClearAVHRR4(:) => NULL() ! std dev clear chan 4 (K) (ukmosst)
  INTEGER, POINTER                       :: AVHRRSSTQC(:) => NULL()     ! DPI for AVHRR QC flags (ukmosst)
  REAL, POINTER                          :: ClearAVHRRPxls(:) => NULL() ! DPI for no. clear pixels (ukmosst)
  INTEGER, POINTER                       :: OrigCtr(:) => NULL()        ! originating centre (satwind)
  REAL, POINTER                          :: ChanCtralFreq(:) => NULL()  ! freq of central channel (satwind)
  REAL, POINTER                          :: StereoFreq(:) => NULL() ! freq of central channel (satwind-STEREOMV)
  REAL, POINTER                          :: HeightAssMethod(:) => NULL() ! height ass'ment method (satwind)
  REAL, POINTER                          :: CmptVecWindDirn(:,:) => NULL()  ! wind direction of vector components (satwind)
  REAL, POINTER                          :: CmptVecWindSpeed(:,:) => NULL() ! speed of vector components (satwind)
  REAL, POINTER                          :: CmptVecWindU(:,:) => NULL()     ! vector u components (satwind)
  REAL, POINTER                          :: CmptVecWindV(:,:) => NULL()     ! vector v components (satwind)
  REAL, POINTER                          :: AltHeightAss(:,:) => NULL()     ! extra info on height assignment (satwind)
  REAL, POINTER                          :: AltPressLevel(:,:) => NULL()    ! extra info (satwind)
  REAL, POINTER                          :: AltAirTempLevel(:,:) => NULL()  ! extra info (satwind)
  REAL, POINTER                          :: MPEFQI(:,:) => NULL()           ! Quality Indicator (satwind)
  REAL, POINTER                          :: MPEFQI2(:,:) => NULL()          ! Quality Indicator w/o FG info (satwind)
  REAL, POINTER                          :: RFFQI(:,:) => NULL()            ! NESDIS quality indicator (satwind)
  INTEGER, POINTER                       :: SuperobRounds(:) => NULL()      ! Satwind superob rounds (satwind)
  TYPE (Element_type), POINTER           :: uSuperob(:,:) => NULL()         ! Wstrly. superob satwind(m/s) (satwind)
  TYPE (Element_type), POINTER           :: vSuperob(:,:) => NULL()         ! Sthrly. superob satwind(m/s) (satwind)
  INTEGER, POINTER                       :: SatwindQI(:) => NULL()          ! Selected quality indicator
  REAL, POINTER                          :: SatwindHeightError(:) => NULL() ! Height error
  REAL, POINTER                          :: SatwindBestFitU(:) => NULL()    ! Best Fit U
  REAL, POINTER                          :: SatwindBestFitV(:) => NULL()    ! Best Fit V
  REAL, POINTER                          :: SatwindBestFitPress(:) => NULL() ! Best Fit Press
  REAL, POINTER                          :: SatwindMinSpeed(:) => NULL()    ! Max Speed
  REAL, POINTER                          :: SatwindMaxSpeed(:) => NULL()    ! Min Speed
  REAL, POINTER                          :: CloudTopHeight(:) => NULL()     ! Cloud top height (for stereo motion vectors)
  TYPE (Element_type), POINTER           :: SBUVozone(:,:) => NULL()        ! int. ozone density (kg/m**2)
  REAL, POINTER                          :: SBUVStDevOzone(:,:) => NULL()   ! ozone density obs error (kg/m**2)
  REAL, POINTER                          :: SBUVUppLayerP(:,:) => NULL()    ! upper layer pressure (Pa)
  REAL, POINTER                          :: SBUVLowLayerP(:,:) => NULL()    ! lower layer pressure (Pa)
  REAL, POINTER                          :: SBUVSolarElev(:) => NULL()      ! solar elevation (?)
  REAL, POINTER                          :: SBUVQualityInf(:) => NULL()     ! quality information
  TYPE (Element_type), POINTER           :: Pstandard(:) => NULL() ! =Pmsl if Zstandard=0    Pa
  TYPE (Element_type), POINTER           :: Pstation(:) => NULL()  ! station level pressure  Pa
  TYPE (Element_type), POINTER           :: Pmsl(:) => NULL()      ! Pmsl                    Pa
  TYPE (Element_type), POINTER           :: pstar(:) => NULL()     ! pressure at model surf  Pa
  TYPE (Element_type), POINTER           :: t2(:) => NULL()        ! 2m temperature          K
  TYPE (Element_type), POINTER           :: td2(:) => NULL()       ! 2m  dewpoint temp       K
  TYPE (Element_type), POINTER           :: rh2(:) => NULL()       ! 2m relative humidity    %
  TYPE (Element_type), POINTER           :: ddd10(:) => NULL()     ! 10m wind direction      deg
  TYPE (Element_type), POINTER           :: fff10(:) => NULL()     ! 10m wind speed          m/s
  TYPE (Element_type), POINTER           :: u10(:) => NULL()       ! 10m westerly wind       m/s
  TYPE (Element_type), POINTER           :: v10(:) => NULL()       ! 10m southerly wind      m/s
  TYPE (Element_type), POINTER           :: vis(:) => NULL()       ! (visibility),      (vis m)
  TYPE (Element_type), POINTER           :: logvis(:) => NULL()    ! log10(visibility), (vis m)
  TYPE (Element_type), POINTER           :: WindSpeed(:) => NULL() ! WindSpeed               m/s
  TYPE (Element_type), POINTER           :: WaveHeight(:) => NULL() ! WaveHeight              m
  TYPE (Element_type), POINTER           :: WAVE_HGHT(:) => NULL() ! WaveHeight              m
  TYPE (Element_type), POINTER           :: WIND_SPED(:) => NULL() ! Surface windspeed       m/s
  TYPE (Element_type), POINTER           :: SeaHeight(:) => NULL() ! Height of Sea surface   m
  TYPE (Element_type), POINTER           :: SeaIce(:) => NULL()    ! Sea ice
  TYPE (Element_type), POINTER           :: CHL(:) => NULL()       ! mass concentration of chlorophyll a 
                                                                   ! in sea water milligram     m-3
  TYPE (Element_type), POINTER           :: KD490(:) => NULL()     ! volume attenuation coefficient of downwelling
                                                                   ! radiative flux in sea water at 490 nm\n  m-1
  TYPE (Element_type), POINTER           :: SST(:) => NULL()       ! Sea Surface Temperature K
  TYPE (Element_type), POINTER           :: Tskin(:) => NULL()     ! Surface radiative temp. K
  TYPE (Element_type), POINTER           :: TCWV(:) => NULL()      ! Total Column Water Vapour kg/m^2
  TYPE (Element_type), POINTER           :: LWP(:) => NULL()       ! Liquid Water Path  kg/m^2
  TYPE (Element_type), POINTER           :: IWP(:) => NULL()       ! Ice Water Path  kg/m^2 
  TYPE (Element_type), POINTER           :: RetLWP(:) => NULL()    ! retrieved Liquid Water Path  kg/m^2
  TYPE (Element_type), POINTER           :: Rainrate(:,:) => NULL()  ! Rain rate     kg/m^2/s
  TYPE (Element_type), POINTER           :: Snowrate(:,:) => NULL()  ! Snow rate     kg/m^2/s
  REAL, POINTER                          :: Rainamount(:,:) => NULL() ! Rain amount     kg/kg
  TYPE (Element_type), POINTER           :: u10Ambwind(:,:) => NULL() ! 10m westerly scat ambiguous winds
  TYPE (Element_type), POINTER           :: v10AmbWind(:,:) => NULL() ! 10m southerly scat ambiguous winds
  TYPE (Element_type), POINTER           :: AWPriorPcorrect(:,:) => NULL() ! prob of scat winds
  TYPE (Element_type), POINTER           :: TotalCloudAmount(:) => NULL()   ! Total Cloud Amount  oktas
  TYPE (Element_type), POINTER           :: SeaSrfcHeight(:) => NULL() ! Altimeter SSH       cm
  REAL, POINTER                          :: DyncAtmrCortn(:) => NULL() ! dynamic atmosphere correction for Altimeter SSH
  REAL, POINTER                          :: LongWaveErrr(:) => NULL()  ! long wave error fro Altimeter SSH
  REAL, POINTER                          :: OcnTide(:) => NULL()       ! Ocean Tide for Altimeter SSH
  REAL, POINTER                          :: MeanSeaHeight(:) => NULL() ! Mean SSH for altimeter cm
  REAL, POINTER                          :: Precip1hr(:) => NULL() ! Srew obs            mm
  REAL, POINTER                          :: PrWx(:) => NULL()      ! Present Weather
  REAL, POINTER                          :: LwstClod(:) => NULL()  ! Low cloud amount    frac
  REAL, POINTER                          :: LwstClodType(:) => NULL() ! Low cloud type
  INTEGER, POINTER                       :: WaveMeasureMethod(:) => NULL() ! Wave recording method
  REAL, POINTER                          :: GlobalRad1hr(:) => NULL() ! 1hr solar radiation J/m2
  REAL, POINTER                          :: WindGust(:) => NULL()     ! Wind gust m/s
  REAL, POINTER                          :: SGNT_WAVE_HGHT(:) => NULL() ! Sig wave height  m
  REAL, POINTER                          :: AVGNG_PERD(:) => NULL()     ! DPI for av period
  INTEGER, POINTER                       :: CSNT_TYP(:) => NULL()       ! DPI for chemical type
  INTEGER, POINTER                       :: DCML_SCL_MASS_DNSTY(:) => NULL() ! DPI for decimal scale mass densty
  INTEGER, POINTER                       :: SITE_QAL(:) => NULL()       ! AQCOPS site quality
  TYPE (Element_type), POINTER           :: MASS_DNSTY(:) => NULL()    ! DPI for scaled mass dnsty
  TYPE (Element_type), POINTER           :: SO2_DNSTY(:) => NULL()    ! DPI for scaled mass dnsty
  TYPE (Element_type), POINTER           :: PM10_DNSTY(:) => NULL()    ! DPI for scaled mass dnsty
  TYPE (Element_type), POINTER           :: PM2p5_DNSTY(:) => NULL()    ! DPI for scaled mass dnsty
  TYPE (Element_type), POINTER           :: O3_DNSTY(:) => NULL()    ! DPI for scaled mass dnsty
  TYPE (Element_type), POINTER           :: NO2_DNSTY(:) => NULL()    ! DPI for scaled mass dnsty
  TYPE (Element_type), POINTER           :: CO_DNSTY(:) => NULL()    ! DPI for scaled mass dnsty
  TYPE (Element_type), POINTER           :: z(:,:) => NULL()       ! height                    m
  TYPE (Element_type), POINTER           :: t(:,:) => NULL()       ! temperature               K
  TYPE (Element_type), POINTER           :: td(:,:) => NULL()      ! dewpoint temperature      K
  TYPE (Element_type), POINTER           :: theta(:,:) => NULL()   ! potential temperature     K
  TYPE (Element_type), POINTER           :: rh(:,:) => NULL()      ! relative humidity         %
  TYPE (Element_type), POINTER           :: MixingRatio(:,:) => NULL() ! mixing ratio       Kg/Kg
  TYPE (Element_type), POINTER           :: q(:,:) => NULL()       ! specific humidity         g/Kg
  TYPE (Element_type), POINTER           :: u(:,:) => NULL()       ! westerly wind component   m/s
  TYPE (Element_type), POINTER           :: v(:,:) => NULL()       ! southerly wind component  m/s
  TYPE (Element_type), POINTER           :: ddd(:,:) => NULL()     ! wind direction            deg
  TYPE (Element_type), POINTER           :: fff(:,:) => NULL()     ! wind speed                m/s
  TYPE (Element_type), POINTER           :: Salt(:,:) => NULL()    ! salinity                  %o
  REAL, POINTER                          :: BriTemp(:,:) => NULL() ! brightness temps          K
  INTEGER, POINTER                       :: NumChans(:) => NULL()  ! number of channels
  INTEGER, POINTER                       :: ChanNum(:,:) => NULL() ! channel numbers
  REAL, POINTER                          :: ChannelWavenumber(:,:) => NULL() ! channel wavenumbers
  TYPE (Element_type), POINTER           :: CLW(:,:) => NULL()     ! Cloud liquid water
  TYPE (Element_type), POINTER           :: CIW(:,:) => NULL()     ! Cloud ice water
  TYPE (Element_type), POINTER           :: Cloud(:,:) => NULL()   ! Layer cloud amount
  TYPE (Element_type), POINTER           :: ozone(:,:) => NULL()         ! ozone
  TYPE (Element_type), POINTER           :: Refrac(:,:) => NULL()        ! GPSRO OBSERVED refractivity
  REAL, POINTER                          :: SolutRefrac(:,:) => NULL()   ! GPSRO refractivity (1DVAR retrieval)
  TYPE (Element_type), POINTER           :: BendingAngleAll(:,:) => NULL()   ! GPSRO bending angle (L1,L2,LC etc)
  TYPE (Element_type), POINTER           :: BendingAngleOrig(:,:) => NULL()   ! GPSRO bending angle (L1,L2,LC etc)
  TYPE (Element_type), POINTER           :: BendingAngle(:,:) => NULL()      ! GPSRO bending angle just LC
  REAL, POINTER                          :: SolutBendingAngle(:,:) => NULL() ! GPSRO bending angle (1DVAR retrieval)
  REAL, POINTER                          :: RO_mean_freq(:,:) => NULL()      ! GPSRO mean frequency, Hz
  TYPE (Element_type), POINTER           :: ImpactParamAll(:,:) => NULL()    ! GPSRO impact parameter(L1,L2,LC etc)
  REAL, POINTER                          :: ImpactParamOrig(:,:) => NULL()    ! GPSRO impact parameter(L1,L2,LC etc)
  TYPE (Element_type), POINTER           :: ImpactParam(:,:) => NULL()      ! GPSRO impact parameter just LC
  INTEGER, POINTER                       :: gpsro_orig_ob(:) => NULL()      ! GPSRO impact parameter just LC
  INTEGER, POINTER                       :: gpsro_orig_level(:) => NULL()      ! GPSRO impact parameter just LC
  TYPE (Element_type), POINTER           :: p(:,:) => NULL()             ! pressure on model levels
  REAL, POINTER                          :: Altitude(:,:) => NULL()      ! altitude               m
  REAL, POINTER                          :: RadarObRange(:,:) => NULL()  ! Range of radar ob
  REAL, POINTER                          :: RadarObAzim(:,:) => NULL()   ! Azimut of radar ob
  REAL, POINTER                          :: RadarBeamElev(:,:) => NULL() ! Elevation of radar beam
  TYPE (Element_type), POINTER           :: RadialVelocity(:,:) => NULL()! Radial velocity ob
  TYPE (Element_type), POINTER           :: Reflectivity(:,:) => NULL()  ! Reflectivity ob
  TYPE (Element_type), POINTER           :: ReflectivityR(:,:) => NULL() ! Reflectivity from rain ob
  TYPE (Element_type), POINTER           :: ReflectivityI(:,:) => NULL() ! Reflectivity from ice ob
  REAL, POINTER                          :: RadarObRangSO(:,:) => NULL() ! Range rad wind superob
  REAL, POINTER                          :: RadarObAzimSO(:,:) => NULL() ! Azim rad wind superob
  TYPE (Element_type), POINTER           :: RadialVelocSO(:,:) => NULL() ! Radial velocity superob
  TYPE (Element_type), POINTER           :: ReflectivitySO(:,:) => NULL() ! Radar reflectivity superob
  INTEGER, POINTER                       :: RadIdent(:,:) => NULL()      ! Radar Identifier
  REAL, POINTER                          :: RadSiteLat(:,:) => NULL()    ! Radar Latitude
  REAL, POINTER                          :: RadSiteLong(:,:) => NULL()   ! Radar Longitude
  REAL, POINTER                          :: RadAltAboveMSL(:,:) => NULL()! Radar Atl Above MSL
  REAL, POINTER                          :: RadarRainWeight(:) => NULL() ! Radar rainfall weight
  REAL, POINTER                          :: RadNoiseLvl(:,:) => NULL()   ! Radar noise level
  REAL, POINTER                          :: RadSW(:,:) => NULL()   ! Radar noise level
  REAL, POINTER                          :: RadNbGate(:) => NULL()   ! Radar - munber of gate with a ray
  REAL, POINTER                          :: RadGateRes(:) => NULL()   ! Radar - Gate resolution [m]
  REAL, POINTER                          :: RadRayRes(:) => NULL()   ! Radar - Ray resolution [degree]
  REAL, POINTER                          :: RadNbRay(:) => NULL()   ! Radar - number of ray within the scan
  INTEGER, POINTER                       :: RadFlag(:,:) => NULL()   ! Radar - Flag for valid data within a scan
  REAL, POINTER                          :: RadUnfoldVelocity(:,:) => NULL()   ! Radar -  Unfolding velocity for doppler wind
  REAL, POINTER                          :: RadRangeStart(:,:) => NULL()       ! Radar -   Start of the recording along the Ray [m]
  REAL, POINTER                          :: RadPhase(:,:) => NULL()       ! Radar -   phase measurements
  REAL, POINTER                          :: RadRefractivity(:,:) => NULL()   ! Radar -   derived refractivity
  REAL, POINTER                          :: RadNcoFreq(:,:) => NULL()   !
  REAL, POINTER                          :: RadPhaseSDP(:,:) => NULL()   !
  INTEGER, POINTER                       :: AUTOSATSpCTest(:) => NULL()  ! Spatial coherence flag
  REAL, POINTER                          :: AUTOSATCTP(:,:) => NULL()    ! Cloud top pressure
  REAL, POINTER                          :: AUTOSATECA(:,:) => NULL()    ! Effective cloud amount
  CHARACTER(len=LenColCentre), POINTER   :: Collection_Centre(:) => NULL() ! Collection Centre
  REAL, POINTER                          :: NEDTCold (:,:) => NULL()     ! ATMS cold nedt
  REAL, POINTER                          :: NEDTWarm(:,:) => NULL()      ! ATMS warm nedt
  REAL, POINTER                          :: AAPPCirrus(:) => NULL()      ! AAPP cirrus test
  REAL, POINTER                          :: AAPPBennartz(:) => NULL()    ! AAPP bennartz test
  TYPE (Element_type), POINTER           :: AOD(:,:) => NULL()              ! Spectral AOD
  REAL, POINTER                          :: AODError(:,:) => NULL()         ! AODError
  REAL, POINTER                          :: AngstromExponent(:) => NULL()   ! Aerosol AngstromExponent
  REAL, POINTER                          :: MassConcentration(:) => NULL()  ! Aerosol MassConcentration
  REAL, POINTER                          :: SmallModeFraction(:) => NULL()  ! Aerosol SmallModeFraction
  REAL, POINTER                          :: EffectiveRadius(:) => NULL()    ! Aerosol EffectiveRadius
  INTEGER, POINTER                       :: AerosolType(:) => NULL()        ! Aerosol Types
  INTEGER, POINTER                       :: AerosolConf(:) => NULL()        ! AOD retrieval confidence
  REAL, POINTER                          :: AtmrNmbr(:) => NULL()        ! sferic (lightning flash) density indicator
  REAL, POINTER                          :: LowestCloudBaseHeight(:) => NULL()   ! Lowest cloud base height
  INTEGER, POINTER                       :: MediumCloudType(:) => NULL()         ! Medium cloud type
  INTEGER, POINTER                       :: HighCloudType(:) => NULL()           ! High cloud type
  REAL, POINTER                          :: CloudAmount8Group(:,:) => NULL()     ! Cloud amount in 8-groups
  INTEGER, POINTER                       :: CloudType8Group(:,:) => NULL()       ! Cloud type in 8-groups
  REAL, POINTER                          :: CloudBaseHeight8Group(:,:) => NULL() ! Cloud base height in 8-groups
  REAL, POINTER                          :: LghnStrk(:) => NULL()        ! ATDnet lightning flash/stroke indicator
  INTEGER, POINTER                       :: TAMDAR_qc_t(:) => NULL()   ! TAMDAR qc flag for temperature
  INTEGER, POINTER                       :: TAMDAR_qc_ddd(:) => NULL() ! TAMDAR qc flag for wind direction
  INTEGER, POINTER                       :: TAMDAR_qc_fff(:) => NULL() ! TAMDAR qc flag for wind speed
  REAL, POINTER                          :: TAMDAR_qc_rh(:) => NULL()  ! TAMDAR uncertainty estimate for rh
  INTEGER, POINTER                       :: station_report_type(:) => NULL()
  CHARACTER(len=LenTransponderAddress), POINTER :: TransponderAddress(:) => NULL()
  CHARACTER(len=LenAircraftType), POINTER :: AircraftType(:) => NULL()
  REAL, POINTER                          :: AircraftGNSSAltitude(:) => NULL()
  REAL, POINTER                          :: AircraftIndicatedAirSpeed(:) => NULL()
  REAL, POINTER                          :: AircraftTrueAirSpeed(:) => NULL()
  REAL, POINTER                          :: AircraftGroundSpeed(:) => NULL()
  REAL, POINTER                          :: AircraftTrueHeading(:) => NULL()
  REAL, POINTER                          :: AircraftGroundHeading(:) => NULL()
  REAL, POINTER                          :: AircraftRadarGroundSpeed(:) => NULL()
  REAL, POINTER                          :: AircraftRadarGroundHeading(:) => NULL()
  REAL, POINTER                          :: MachNumber(:) => NULL()
  REAL, POINTER                          :: AircraftRadarT(:) => NULL()
  REAL, POINTER                          :: AircraftGeomagneticDeviation(:) => NULL()
  REAL, POINTER                          :: AircraftMagneticHeadingBias(:) => NULL()
  REAL, POINTER                          :: AircraftRollAngle(:) => NULL()
  REAL, POINTER                          :: AircraftRadarDDD(:) => NULL()
  REAL, POINTER                          :: AircraftRadarU(:) => NULL()
  REAL, POINTER                          :: AircraftRadarV(:) => NULL()
  TYPE (Element_type), POINTER           :: RainAccum(:) => NULL()
  INTEGER, POINTER                       :: SiteType(:) => NULL()
  TYPE (Element_type), POINTER           :: CeilBackscatter(:,:) => NULL()   ! Ceilometer Backscatter
  TYPE (Element_type), POINTER           :: CeilCBH(:,:) => NULL()    ! Ceilometer Cloud Base Height reports
  REAL, POINTER                          :: CeilRange(:,:) => NULL()
  REAL, POINTER                          :: OpenRoadWindSpeed(:,:) => NULL()
  REAL, POINTER                          :: OpenRoadWindDirection(:,:) => NULL()
  REAL, POINTER                          :: OpenRoadTemp(:,:) => NULL()
  REAL, POINTER                          :: OpenRoadDewPointTemp(:,:) => NULL()
  REAL, POINTER                          :: OpenRoadVisibility(:,:) => NULL()
  INTEGER, POINTER                       :: CeilSiteID(:,:) => NULL()
  INTEGER, POINTER                       :: CeilScanIdent(:,:) => NULL()
  REAL, POINTER                          :: CeilLPE(:,:) => NULL()
  REAL, POINTER                          :: CeilWindowTau(:,:) => NULL()
  INTEGER, POINTER                       :: ARGOPositionQCFlag(:) => NULL()
  INTEGER, POINTER                       :: ARGOTemperatureQCFlag(:,:) => NULL()
  INTEGER, POINTER                       :: ARGOPressureQCFlag(:,:) => NULL()
  INTEGER, POINTER                       :: ARGOSalinityQCFlag(:,:) => NULL()
  TYPE (Element_type), POINTER           :: HLOSwind(:) => NULL() ! Horizontal Line Of Sight Wind
  INTEGER, POINTER                       :: ProfileNo(:) => NULL()
  REAL, POINTER                          :: dWinddT(:) => NULL()
  REAL, POINTER                          :: dWinddP(:) => NULL()
  REAL, POINTER                          :: AzimuthCOG(:) => NULL()
  REAL, POINTER                          :: HeightCOG(:) => NULL()
  REAL, POINTER                          :: HeightTop(:) => NULL()
  REAL, POINTER                          :: HeightBottom(:) => NULL()
  REAL, POINTER                          :: WindErr(:) => NULL()
  INTEGER, POINTER                       :: ChannelNumber(:) => NULL()
  REAL, POINTER                          :: LidarClass(:) => NULL()
  INTEGER, POINTER                       :: AladinConfFlag(:) => NULL()
  REAL, POINTER                          :: WindErrRejLimit(:) => NULL()
  INTEGER, POINTER                       :: buoy_report_type(:) => NULL()
  INTEGER, POINTER                       :: time_difference(:) => NULL()
  REAL, POINTER                          :: xbt_depth_factor(:) => NULL()
  REAL, POINTER                          :: background_temperature(:,:) => NULL()
  REAL, POINTER                          :: background_salt(:,:) => NULL()
CONTAINS
  PROCEDURE                              :: init => Ops_InitObs
  PROCEDURE                              :: deallocate => Ops_OBDeAllocate
  PROCEDURE                              :: print => Ops_OBPrint
END TYPE OB_type

! PARAMETERs:

!-------------------------------------------------------------------------------
!   Possible settings of ReportFlags in OB_type
! Flags on whole reports - bit numbers 0-31 (use in IBSET and BTEST)
INTEGER, PARAMETER :: NumValidReportFlags   = 14   ! Number of defined flags (0-13).
                                                   ! If you wish to add to these
                                                   ! please contact OPS admin.
INTEGER, PARAMETER :: FinalRejectReport     =  0   ! => one of flags 1-6 set
INTEGER, PARAMETER :: PermRejectReport      =  1   ! => blacklisted data
INTEGER, PARAMETER :: SurplusReport         =  2   ! => (near) duplicate data
INTEGER, PARAMETER :: OutOfAreaReport       =  3   ! => outside analysis area/time
INTEGER, PARAMETER :: LandRejectReport      =  4   ! => marine ob over land
INTEGER, PARAMETER :: UsedInSuperObReport   =  5   ! => combined with other obs
INTEGER, PARAMETER :: TrackRejectReport     =  6   ! => failed track check
INTEGER, PARAMETER :: SuperObReport         =  8   ! => this is a SuperOb
! Sonde data
INTEGER, PARAMETER :: NoPressureSensor      = 13   ! => no PILOT pressure sensor
! Satellite wind data
INTEGER, PARAMETER :: MissingDataReport     = 14   ! => Missing data
INTEGER, PARAMETER :: SatwindAltReport      = 15   ! => Satwind alternative p/uv
INTEGER, PARAMETER :: SatwindGoodConstraint = 16   ! => Satwind alternative p/uv
! Other miscellaneous flags
INTEGER, PARAMETER :: Thin4DFlag            = 17   ! => Satwind alternative p/uv
INTEGER, PARAMETER :: StationListThinFlag   = 18   ! => Satwind alternative p/uv

!   Possible settings of AssimFlags in OB_type
INTEGER, PARAMETER :: NewReport          = 0    ! => ob not yet assimilated
INTEGER, PARAMETER :: AssimilatedReport  = 1    ! => ob already assimilated

!Notes
! PermRejectReport   = used for station list rejections
! SurplusReport      = used for ship/airep data judged to be (almost) duplicate
!                      used for thinned buoy reports
! OutOfAreaReport    = used for data outside model area
!                      (outside ocean mask for ocean/sst/wave analysis)
!                      used for data outside time window of analysis

!-------------------------------------------------------------------------------
!   Possible settings of Flags in Element_type
! Flags on individual elements - bit numbers 0-31 (use in IBSET and BTEST)
INTEGER, PARAMETER :: NoAssimFlag        = 23   ! => do not use in analysis
INTEGER, PARAMETER :: FinalRejectFlag    =  0   ! => Final QC flag
INTEGER, PARAMETER :: BuddyRejectFlag    =  1   ! => PGE>0.5 after buddy check
INTEGER, PARAMETER :: BackRejectFlag     =  2   ! => PGE>0.5 after backgr check
INTEGER, PARAMETER :: PermRejectFlag     =  3   ! => blacklisted data
INTEGER, PARAMETER :: ClimRejectFlag     =  4   ! => PGE>0.5 after climat check
INTEGER, PARAMETER :: BuddyPerfFlag      =  5   ! => Buddy check performed
INTEGER, PARAMETER :: BackPerfFlag       =  6   ! => Background check performed
INTEGER, PARAMETER :: ClimPerfFlag       =  7   ! => Climatological check perf
INTEGER, PARAMETER :: PermCorrectFlag    =  8   ! => fixed correction
INTEGER, PARAMETER :: DataCorrectFlag    =  9   ! => Eg sign correction
INTEGER, PARAMETER :: ConsistencyFlag    = 10   ! => Internal consistency check
INTEGER, PARAMETER :: ExtremeValueFlag   = 11   ! => extreme value check
! Surface data
INTEGER, PARAMETER :: TendencyFlag       = 12   ! => Pressure tendency check.
INTEGER, PARAMETER :: PstdRepFlag        = 13   ! => Pstd reported not Pmsl.
INTEGER, PARAMETER :: PstnPrefFlag       = 14   ! => Use Pstn if reported.
INTEGER, PARAMETER :: PmslUsedFlag       = 15   ! => Pmsl used in P* calc.
INTEGER, PARAMETER :: PstdUsedFlag       = 16   ! => Pstd used in P* calc.
INTEGER, PARAMETER :: PstnUsedFlag       = 17   ! => Pstn used in P* calc.
INTEGER, PARAMETER :: QNHinHgFlag        = 16   ! => QNH in 0.01 inches Hg
INTEGER, PARAMETER :: QNHhPaFlag         = 17   ! => QNH in whole hPa - Metars
INTEGER, PARAMETER :: RHreportFlag       = 18   ! => RH was reported
INTEGER, PARAMETER :: SiteQualityFlag    = 20   ! => AIRQAL site quality reject flag
INTEGER, PARAMETER :: VisRejFlag         = 22   ! => Reject Visibility Ob
INTEGER, PARAMETER :: notRoundedFlag     = 24   ! => Metar QNH not rounded to whole hPa
! Sonde data
INTEGER, PARAMETER :: HydrostaticFlag    = 12   ! => Hydrostatic check flag
INTEGER, PARAMETER :: InterpolationFlag  = 13   ! => Interpolation check flag
INTEGER, PARAMETER :: SuperadiabatFlag   = 14   ! => Superadiabatic check flag
INTEGER, PARAMETER :: SurfaceLevelFlag   = 15   ! => Surface Level
INTEGER, PARAMETER :: StandardLevelFlag  = 16   ! => Standard Level
INTEGER, PARAMETER :: SigTempLevelFlag   = 17   ! => Significant Temperature
INTEGER, PARAMETER :: SigWindLevelFlag   = 18   ! => Significant Wind Level
INTEGER, PARAMETER :: MaxWindLevelFlag   = 19   ! => Maximum Wind Level
INTEGER, PARAMETER :: TropopauseFlag     = 20   ! => Tropopause Level
INTEGER, PARAMETER :: PartialLayerFlag   = 21   ! => Partial Layer Vert Average

! Satellite wind data
INTEGER, PARAMETER :: SatwindConfFlag       = 12 ! => Satwind product confidence
INTEGER, PARAMETER :: SatwindInversionFlag  = 13 ! => Inversion height corrected
INTEGER, PARAMETER :: SatwindDryLayerFlag   = 14 ! => Model dry layer QC
INTEGER, PARAMETER :: SatwindWrongLayerFlag = 15 ! => Wrong moist layer QC

! Scatterometer wind data
INTEGER, PARAMETER :: ScatConfidenceFlag  = 12  ! => ERS wind product confidence
INTEGER, PARAMETER :: ScatAmbigRemov1Flag = 13  ! => ERS wind ambiguity removal
INTEGER, PARAMETER :: ScatAmbigRemov2Flag = 14  ! => ERS wind ambiguity removal
INTEGER, PARAMETER :: ScatIncAngle1Flag   = 15  ! => ERS wind angle of incidence
INTEGER, PARAMETER :: ScatIncAngle2Flag   = 16  ! => ERS wind angle of incidence
! Aircraft flags
INTEGER, PARAMETER :: DerivedFromMixRatioFlag = 12 ! Relative humidity derived from mixing ratio
INTEGER, PARAMETER :: DerivedFromFlightLevelFlag = 13 ! Pressure derived from flight level
INTEGER, PARAMETER :: TamdarQualityFlag = 14 ! Flag set based on reported qc flag
! SatSST data
INTEGER, PARAMETER :: DaytimeFlag = 12      ! => Indicates a likely diurnal warming component in signal
INTEGER, PARAMETER :: DiurnalWarmFlag = 13      ! => Indicates a likely diurnal warming component in signal

! Interface blocks:
INTERFACE Ops_Alloc
  MODULE PROCEDURE Ops_Alloc_Char
  MODULE PROCEDURE Ops_Alloc_ElementType
  MODULE PROCEDURE Ops_Alloc_ElementType2d
  MODULE PROCEDURE Ops_Alloc_CoordType
  MODULE PROCEDURE Ops_Alloc_CoordType2d
  MODULE PROCEDURE Ops_Alloc_Int
  MODULE PROCEDURE Ops_Alloc_Int2d
  MODULE PROCEDURE Ops_Alloc_Real
  MODULE PROCEDURE Ops_Alloc_Real2d
END INTERFACE Ops_Alloc

INTERFACE Ops_DeAlloc
  MODULE PROCEDURE Ops_DeAlloc_Char
  MODULE PROCEDURE Ops_DeAlloc_ElementType
  MODULE PROCEDURE Ops_DeAlloc_ElementType2d
  MODULE PROCEDURE Ops_DeAlloc_CoordType
  MODULE PROCEDURE Ops_DeAlloc_CoordType2d
  MODULE PROCEDURE Ops_DeAlloc_Int
  MODULE PROCEDURE Ops_DeAlloc_Int2d
  MODULE PROCEDURE Ops_DeAlloc_Real
  MODULE PROCEDURE Ops_DeAlloc_Real2d
END INTERFACE Ops_DeAlloc

INTERFACE Ops_ReAlloc
  MODULE PROCEDURE Ops_ReAlloc_ElementType2d
  MODULE PROCEDURE Ops_ReAlloc_CoordType2d
  MODULE PROCEDURE Ops_ReAlloc_Int2d
  MODULE PROCEDURE Ops_ReAlloc_Real2d
END INTERFACE Ops_ReAlloc

CONTAINS

INCLUDE 'Ops_Alloc_Char.inc'
INCLUDE 'Ops_Alloc_ElementType.inc'
INCLUDE 'Ops_Alloc_ElementType2d.inc'
INCLUDE 'Ops_Alloc_CoordType.inc'
INCLUDE 'Ops_Alloc_CoordType2d.inc'
INCLUDE 'Ops_Alloc_Int.inc'
INCLUDE 'Ops_Alloc_Int2d.inc'
INCLUDE 'Ops_Alloc_Real.inc'
INCLUDE 'Ops_Alloc_Real2d.inc'

INCLUDE 'Ops_DeAlloc_Char.inc'
INCLUDE 'Ops_DeAlloc_ElementType.inc'
INCLUDE 'Ops_DeAlloc_ElementType2d.inc'
INCLUDE 'Ops_DeAlloc_CoordType.inc'
INCLUDE 'Ops_DeAlloc_CoordType2d.inc'
INCLUDE 'Ops_DeAlloc_Int.inc'
INCLUDE 'Ops_DeAlloc_Int2d.inc'
INCLUDE 'Ops_DeAlloc_Real.inc'
INCLUDE 'Ops_DeAlloc_Real2d.inc'

INCLUDE 'Ops_ReAlloc_ElementType2d.inc'
INCLUDE 'Ops_ReAlloc_CoordType2d.inc'
INCLUDE 'Ops_ReAlloc_Int2d.inc'
INCLUDE 'Ops_ReAlloc_Real2d.inc'

INCLUDE 'Ops_ObsAction.inc'
INCLUDE 'Ops_ObsActionDealloc.inc'
INCLUDE 'Ops_ObsActionInit.inc'
INCLUDE 'Ops_ObsActionPrint1.inc'
INCLUDE 'Ops_ObsActionPrint2.inc'
INCLUDE 'Ops_ObsGlobalAction.inc'

INCLUDE 'Ops_CopyObTypeHeader.inc'
INCLUDE 'Ops_InitObs.inc'
INCLUDE 'Ops_OBDeAllocate.inc'
INCLUDE 'Ops_OBPrint.inc'
INCLUDE 'Ops_SetupObType.inc'

END MODULE OpsMod_ObsInfo
