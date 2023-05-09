!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Performs action defined by sub on all array elements of Ob.
!
! Method: Calls Ops_ObsAction for every array element
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ObsGlobalAction (Ob,  &
                                sub)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_type)                         :: Ob

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
CHARACTER(len=*), PARAMETER            :: RoutineName = "Ops_ObsGlobalAction"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL Ops_ObsAction (Ob % header % Latitude, &
                    'Ob % Latitude',        &
                    Real1 = Ob % Latitude,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % header % Longitude, &
                    'Ob % Longitude',        &
                    Real1 = Ob % Longitude,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % header % Family, &
                    'Ob % Family',        &
                    Int1 = Ob % Family,   &
                    sub = sub)

CALL Ops_ObsAction (Ob % header % Time, &
                    'Ob % Time',        &
                    Real1 = Ob % Time,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % ReceiptTime, &
                    'Ob % ReceiptTime',        &
                    Int1 = Ob % ReceiptTime,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % GTScentre, &
                    'Ob % GTScentre',        &
                    Int1 = Ob % GTScentre,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ObsType, &
                    'Ob % ObsType',        &
                    Int1 = Ob % ObsType,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SubType, &
                    'Ob % SubType',        &
                    Int1 = Ob % SubType,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % Id, &
                    'Ob % Id',        &
                    Int1 = Ob % Id,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CallSign, &
                    'Ob % CallSign',        &
                    Char = Ob % CallSign,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SatId, &
                    'Ob % SatId',        &
                    Int1 = Ob % SatId,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ReportPGE, &
                    'Ob % ReportPGE',        &
                    Real1 = Ob % ReportPGE,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ReportFlags, &
                    'Ob % ReportFlags',        &
                    Int1 = Ob % ReportFlags,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AssimFlags, &
                    'Ob % AssimFlags',        &
                    Int1 = Ob % AssimFlags,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ModelOrog, &
                    'Ob % ModelOrog',        &
                    Real1 = Ob % ModelOrog,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ModelSurface, &
                    'Ob % ModelSurface',        &
                    Real1 = Ob % ModelSurface,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ModelSeaIce, &
                    'Ob % ModelSeaIce',        &
                    Real1 = Ob % ModelSeaIce,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ModelCloudAmount, &
                    'Ob % ModelCloudAmount',        &
                    Real1 = Ob % ModelCloudAmount,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SatobCompMethod, &
                    'Ob % SatobCompMethod',        &
                    Int1 = Ob % SatobCompMethod,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SatInst, &
                    'Ob % SatInst',        &
                    Int1 = Ob % SatInst,   &
                    sub = sub)

! Thinning info
CALL Ops_ObsAction (Ob % Header % ThinningScore, &
                    'Ob % ThinningScore',        &
                    Int1 = Ob % ThinningScore,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ThinningRounds, &
                    'Ob % ThinningRounds',        &
                    Int1 = Ob % ThinningRounds,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ThinningCall, &
                    'Ob % ThinningCall',        &
                    Int1 = Ob % ThinningCall,   &
                    sub = sub)

! Altimeter observation meta-data
CALL Ops_ObsAction (Ob % Header % ObNumber, &
                    'Ob % ObNumber',        &
                    Int1 = Ob % ObNumber,   &
                    sub = sub)

! Surface observation meta-data

CALL Ops_ObsAction (Ob % Header % VesselInfo, &
                    'Ob % VesselInfo',        &
                    Char = Ob % VesselInfo,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ObPractice, &
                    'Ob % ObPractice',        &
                    Int1 = Ob % ObPractice,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % Instruments, &
                    'Ob % Instruments',        &
                    Int1 = Ob % Instruments,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % Zstation, &
                    'Ob % Zstation',        &
                    Real1 = Ob % Zstation,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % Zstandard, &
                    'Ob % Zstandard',        &
                    Real1 = Ob % Zstandard,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AnHeight, &
                    'Ob % AnHeight',        &
                    Real1 = Ob % AnHeight,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CoastDist, &
                    'Ob % CoastDist',        &
                    Real1 = Ob % CoastDist,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % LapseRate, &
                    'Ob % LapseRate',        &
                    Real1 = Ob % LapseRate,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % HeightRange, &
                    'Ob % HeightRange',        &
                    Real1 = Ob % HeightRange,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ZStnCorrected, &
                    'Ob % ZStnCorrected',        &
                    Real1 = Ob % ZStnCorrected,  &
                    sub = sub)

! SST observation Metadata.
CALL Ops_ObsAction (Ob % Header % SSTRejFlag, &
                    'Ob % SSTRejFlag',        &
                    Real1 = Ob % SSTRejFlag,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SSTBias, &
                    'Ob % SSTBias',        &
                    Real1 = Ob % SSTBias,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SSTStdev, &
                    'Ob % SSTStdev',        &
                    Real1 = Ob % SSTStdev,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SSTQuality, &
                    'Ob % SSTQuality',        &
                    Int1 = Ob % SSTQuality,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SOLR_RDTN_FLUX, &
                    'Ob % SOLR_RDTN_FLUX',        &
                    Real1 = Ob % SOLR_RDTN_FLUX,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % TIME_PERD_SOLR_RDTN_FLUX, &
                    'Ob % TIME_PERD_SOLR_RDTN_FLUX',        &
                    Real1 = Ob % TIME_PERD_SOLR_RDTN_FLUX,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SSTAlgrType, &
                    'Ob % SSTAlgrType',        &
                    Int1 = Ob % SSTAlgrType,   &
                    sub = sub)

! Radiosonde observation meta-data
CALL Ops_ObsAction (Ob % Header % SondeType, &
                    'Ob % SondeType',        &
                    Int1 = Ob % SondeType,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SondeReportType, &
                    'Ob % SondeReportType',        &
                    Int1 = Ob % SondeReportType,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % Hawson00Cor, &
                    'Ob % Hawson00Cor',        &
                    Real1 = Ob % Hawson00Cor,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % Hawson12Cor, &
                    'Ob % Hawson12Cor',        &
                    Real1 = Ob % Hawson12Cor,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % HawsonCor, &
                    'Ob % HawsonCor',        &
                    Real1 = Ob % HawsonCor,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % Nlevels, &
                    'Ob % Nlevels',        &
                    Int1 = Ob % Nlevels,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % LevelType, &
                    'Ob % LevelType',        &
                    Int2 = Ob % LevelType,   &
                    sub = sub)

! Wind profiler observation meta-data
CALL Ops_ObsAction (Ob % Header % WinProQCFlags, &
                    'Ob % WinProQCFlags',        &
                    Int2 = Ob % WinProQCFlags,   &
                    sub = sub)

! Aircraft observation meta-data
CALL Ops_ObsAction (Ob % Header % TailNumber, &
                    'Ob % TailNumber',        &
                    Char = Ob % TailNumber,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % FlightPhase, &
                    'Ob % FlightPhase',        &
                    Int1 = Ob % FlightPhase,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % FlightLevel, &
                    'Ob % FlightLevel',        &
                    Real1 = Ob % FlightLevel,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AcarsStatus, &
                    'Ob % AcarsStatus',        &
                    Int1 = Ob % AcarsStatus,   &
                    sub = sub)

! General satellite observation meta-data
CALL Ops_ObsAction (Ob % Header % RcptStn, &
                    'Ob % RcptStn',        &
                    Int1 = Ob % RcptStn,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ScanLine, &
                    'Ob % ScanLine',        &
                    Int1 = Ob % ScanLine,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ScanPosition, &
                    'Ob % ScanPosition',        &
                    Int1 = Ob % ScanPosition,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % surface, &
                    'Ob % surface',        &
                    Int1 = Ob % surface,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % elevation, &
                    'Ob % elevation',        &
                    Real1 = Ob % elevation,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % LocalZenith, &
                    'Ob % LocalZenith',        &
                    Real1 = Ob % LocalZenith,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % LocalAzimuth, &
                    'Ob % LocalAzimuth',        &
                    Real1 = Ob % LocalAzimuth,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SatView, &
                    'Ob % SatView',        &
                    Real1 = Ob % SatView,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SatZenithAngle, &
                    'Ob % SatZenithAngle',        &
                    Real1 = Ob % SatZenithAngle,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SatAzimth, &
                    'Ob % SatAzimth',        &
                    Real1 = Ob % SatAzimth,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SolarZenith, &
                    'Ob % SolarZenith',        &
                    Real1 = Ob % SolarZenith,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SolarAzimth, &
                    'Ob % SolarAzimth',        &
                    Real1 = Ob % SolarAzimth,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ChnlQlty, &
                    'Ob % ChnlQlty',        &
                    Int2 = Ob % ChnlQlty,   &
                    sub = sub)

! Satellite 1D-Var MetaData
CALL Ops_ObsAction (Ob % Header % Niter, &
                    'Ob % Niter',        &
                    Int1 = Ob % Niter,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CldCost, &
                    'Ob % CldCost',        &
                    Real1 = Ob % CldCost,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CirCost, &
                    'Ob % CirCost',        &
                    Real1 = Ob % CirCost,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SI150, &
                    'Ob % SI150',        &
                    Real1 = Ob % SI150,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SI183, &
                    'Ob % SI183',        &
                    Real1 = Ob % SI183,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % QCflags, &
                    'Ob % QCflags',        &
                    Int1 = Ob % QCflags,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % QCscan, &
                    'Ob % QCscan',        &
                    Int1 = Ob % QCscan,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % QCprepro, &
                    'Ob % QCprepro',        &
                    Int1 = Ob % QCprepro,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % QCinfo, &
                    'Ob % QCinfo',        &
                    Int1 = Ob % QCinfo,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % QCarea, &
                    'Ob % QCarea',        &
                    Int1 = Ob % QCarea,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % TCozone_init, &
                    'Ob % TCozone_init',        &
                    Real1 = Ob % TCozone_init,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % TCozone, &
                    'Ob % TCozone',        &
                    Real1 = Ob % TCozone,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % IREmiss, &
                    'Ob % IREmiss',        &
                    Real1 = Ob % IREmiss,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % Emissivity, &
                    'Ob % Emissivity',        &
                    Real2 = Ob % Emissivity,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % MwEmiss, &
                    'Ob % MwEmiss',        &
                    Real2 = Ob % MwEmiss,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % MwEmErr, &
                    'Ob % MwEmErr',        &
                    Real2 = Ob % MwEmErr,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % MwEmissAtlas, &
                    'Ob % MwEmissAtlas',        &
                    Real2 = Ob % MwEmissAtlas,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % MwEmErrAtlas, &
                    'Ob % MwEmErrAtlas',        &
                    Real2 = Ob % MwEmErrAtlas,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CalcEmiss, &
                    'Ob % CalcEmiss',        &
                    Int2 = Ob % CalcEmiss,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % EmissPC, &
                    'Ob % EmissPC',        &
                    Real2 = Ob % EmissPC,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CloudFrac, &
                    'Ob % CloudFrac',        &
                    Real1 = Ob % CloudFrac,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CloudFrac_init, &
                    'Ob % CloudFrac_init',        &
                    Real1 = Ob % CloudFrac_init,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CloudFree, &
                    'Ob % CloudFree',        &
                    Real2 = Ob % CloudFree,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CloudFrac_arr, &
                    'Ob % CloudFrac_arr',        &
                    Real2 = Ob % CloudFrac_arr,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CloudTopTemp, &
                    'Ob % CloudTopTemp',        &
                    Real1 = Ob % CloudTopTemp,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CloudTopP, &
                    'Ob % CloudTopP',        &
                    Real1 = Ob % CloudTopP,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CloudTopPErr, &
                    'Ob % CloudTopPErr',        &
                    Real1 = Ob % CloudTopPErr,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CloudTopP_init, &
                    'Ob % CloudTopP_init',        &
                    Real1 = Ob % CloudTopP_init,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ObBriTemp, &
                    'Ob % ObBriTemp',        &
                    Real2 = Ob % ObBriTemp,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CorBriTemp, &
                    'Ob % CorBriTemp',        &
                    Real2 = Ob % CorBriTemp,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % InitBriTemp, &
                    'Ob % InitBriTemp',        &
                    Real2 = Ob % InitBriTemp,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ClearBriTemp, &
                    'Ob % ClearBriTemp',        &
                    Real2 = Ob % ClearBriTemp,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % Jcost, &
                    'Ob % Jcost',        &
                    Real1 = Ob % Jcost,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % BiasPredictors, &
                    'Ob % BiasPredictors',        &
                    Real2 = Ob % BiasPredictors,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % TauSurf, &
                    'Ob % TauSurf',        &
                    Real2 = Ob % TauSurf,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % BriTempVarError, &
                    'Ob % BriTempVarError',        &
                    Real2 = Ob % BriTempVarError,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CloudRTError, &
                    'Ob % CloudRTError',        &
                    Real2 = Ob % CloudRTError,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CloudRTBias, &
                    'Ob % CloudRTBias',        &
                    Real2 = Ob % CloudRTBias,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CloudEffectAvg, &
                    'Ob % CloudEffectAvg',        &
                    Real2 = Ob % CloudEffectAvg,  &
                    sub = sub)

! ATOVS observation meta-data (processed through 1DVAR)
! - some are used by IASI too
CALL Ops_ObsAction (Ob % Header % VnATOVPP, &
                    'Ob % VnATOVPP',        &
                    Int1 = Ob % VnATOVPP,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AMSUsurface, &
                    'Ob % AMSUsurface',        &
                    Int1 = Ob % AMSUsurface,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AMSUcldcost, &
                    'Ob % AMSUcldcost',        &
                    Real1 = Ob % AMSUcldcost,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AMSUscatindx, &
                    'Ob % AMSUscatindx',        &
                    Real1 = Ob % AMSUscatindx,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ATOVSProcOption, &
                    'Ob % ATOVSProcOption',        &
                    Int1 = Ob % ATOVSProcOption,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % QCHIRSfov, &
                    'Ob % QCHIRSfov',        &
                    Int1 = Ob % QCHIRSfov,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % QCGIIRSfov, &
                    'Ob % QCGIIRSfov',        &
                    Int1 = Ob % QCGIIRSfov,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % StdDevClearAVHRR4, &
                    'Ob % StdDevClearAVHRR4',        &
                    Real1 = Ob % StdDevClearAVHRR4,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % HIRS_temp, &
                    'Ob % HIRS_temp',        &
                    Real1 = Ob % HIRS_temp,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AMSUa1_temp, &
                    'Ob % AMSUa1_temp',        &
                    Real1 = Ob % AMSUa1_temp,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AMSUa2_temp, &
                    'Ob % AMSUa2_temp',        &
                    Real1 = Ob % AMSUa2_temp,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AMSUb_temp, &
                    'Ob % AMSUb_temp',        &
                    Real1 = Ob % AMSUb_temp,  &
                    sub = sub)

! IASI-specific MetaData
CALL Ops_ObsAction (Ob % Header % QCIASIfov, &
                    'Ob % QCIASIfov',        &
                    Int1 = Ob % QCIASIfov,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % QCIASIGQis, &
                    'Ob % QCIASIGQis',        &
                    Int1 = Ob % QCIASIGQis,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ScaleFacStartChan, &
                    'Ob % ScaleFacStartChan',        &
                    Int2 = Ob % ScaleFacStartChan,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ScaleFacEndChan, &
                    'Ob % ScaleFacEndChan',        &
                    Int2 = Ob % ScaleFacEndChan,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ScaleFac, &
                    'Ob % ScaleFac',        &
                    Int2 = Ob % ScaleFac,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ScaledRadiance, &
                    'Ob % ScaledRadiance',        &
                    Real2 = Ob % ScaledRadiance,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % Radiance, &
                    'Ob % Radiance',        &
                    Real2 = Ob % Radiance,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ChannelVersion, &
                    'Ob % ChannelVersion',        &
                    Int1 = Ob % ChannelVersion,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % PCScore, &
                    'Ob % PCScore',        &
                    Real2 = Ob % PCScore,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % PCVersion, &
                    'Ob % PCVersion',        &
                    Int1 = Ob % PCVersion,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % QCIASIPC, &
                    'Ob % QCIASIPC',        &
                    Real2 = Ob % QCIASIPC,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % PixelNumber, &
                    'Ob % PixelNumber',        &
                    Int1 = Ob % PixelNumber,   &
                    sub = sub)

! SSMIS observation data
CALL Ops_ObsAction (Ob % Header % SSMISScanType, &
                    'Ob % SSMISScanType',        &
                    Int1 = Ob % SSMISScanType,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SrfcFlag, &
                    'Ob % SrfcFlag',        &
                    Int1 = Ob % SrfcFlag,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SurfType, &
                    'Ob % SurfType',        &
                    Int1 = Ob % SurfType,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RainFlag, &
                    'Ob % RainFlag',        &
                    Int1 = Ob % RainFlag,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RainType, &
                    'Ob % Raintype',        &
                    Int1 = Ob % RainType,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % EphemLat, &
                    'Ob % EphemLat',        &
                    Real2 = Ob % EphemLat,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % EphemLon, &
                    'Ob % EphemLon',        &
                    Real2 = Ob % EphemLon,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % EphemHeight, &
                    'Ob % EphemHeight',        &
                    Real2 = Ob % EphemHeight,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % BasePontLat, &
                    'Ob % BasePontLat',        &
                    Real2 = Ob % BasePontLat,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % BasePontLon, &
                    'Ob % BasePontLon',        &
                    Real2 = Ob % BasePontLon,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % BasePontIncAng, &
                    'Ob % BasePontIncAng',        &
                    Real2 = Ob % BasePontIncAng,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % BasePontAzhAng, &
                    'Ob % BasePontAzhAng',        &
                    Real2 = Ob % BasePontAzhAng,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % MagneticFieldBe, &
                    'Ob % MagneticFieldBe',        &
                    Real2 = Ob % MagneticFieldBe,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ViewingVectorK, &
                    'Ob % ViewingVectorK',        &
                    Real2 = Ob % ViewingVectorK,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % BdotK, &
                    'Ob % BdotK',        &
                    Real1 = Ob % BdotK,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % thetaBK, &
                    'Ob % thetaBK',        &
                    Real1 = Ob % thetaBK,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SatOrbitAngle, &
                    'Ob % SatOrbitAngle',        &
                    Real1 = Ob % SatOrbitAngle,  &
                    sub = sub)

! AIRS Specific information - some are used for IASI also
CALL Ops_ObsAction (Ob % Header % AMSUZenAng, &
                    'Ob % AMSUZenAng',        &
                    Real1 = Ob % AMSUZenAng,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SatCloudTestsUsed, &
                    'Ob % SatCloudTestsUsed',        &
                    Int1 = Ob % SatCloudTestsUsed,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SatCloudFlags, &
                    'Ob % SatCloudFlags',        &
                    Int1 = Ob % SatCloudFlags,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SurfWaterFraction, &
                    'Ob % SurfWaterFraction',        &
                    Real1 = Ob % SurfWaterFraction,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % MainSurfaceType, &
                    'Ob % MainSurfaceType',        &
                    Int1 = Ob % MainSurfaceType,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % MainSurfFraction, &
                    'Ob % MainSurfFraction',        &
                    Real1 = Ob % MainSurfFraction,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AIRSVisMeanAlb, &
                    'Ob % AIRSVisMeanAlb',        &
                    Real2 = Ob % AIRSVisMeanAlb,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AIRSVisStdDevAlb, &
                    'Ob % AIRSVisStdDevAlb',        &
                    Real2 = Ob % AIRSVisStdDevAlb,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CloudyChannel, &
                    'Ob % CloudyChannel',        &
                    Int2 = Ob % CloudyChannel,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % NumClrPriChans, &
                    'Ob % NumClrPriChans',        &
                    Int1 = Ob % NumClrPriChans,   &
                    sub = sub)

! GPS total zenith delay observation meta-data

CALL Ops_ObsAction (Ob % Header % GPSTZDelayError, &
                    'Ob % GPSTZDelayError',        &
                    Real1 = Ob % GPSTZDelayError,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SITE_NAME, &
                    'Ob % SITE_NAME',        &
                    Char = Ob % SITE_NAME,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % GPS_MaxHeightDifference, &
                    'Ob % GPS_MaxHeightDifference',        &
                    Real1 = Ob % GPS_MaxHeightDifference,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % GPS_QC_Threshold, &
                    'Ob % GPS_QC_Threshold',        &
                    Real1 = Ob % GPS_QC_Threshold,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % GPS_MinNumObs, &
                    'Ob % GPS_MinNumObs',        &
                    Int1 = Ob % GPS_MinNumObs,   &
                    sub = sub)

! GPSRO observation meta-data

CALL Ops_ObsAction (Ob % Header % GPSRO_Zmin, &
                    'Ob % GPSRO_Zmin',        &
                    Real1 = Ob % GPSRO_Zmin,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % GPSRO_Zmax, &
                    'Ob % GPSRO_Zmax',        &
                    Real1 = Ob % GPSRO_Zmax,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SoftwareIdentity, &
                    'Ob % SoftwareIdentity',        &
                    Int1 = Ob % SoftwareIdentity,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RO_Quality, &
                    'Ob % RO_Quality',        &
                    Int1 = Ob % RO_Quality,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RO_Confidence, &
                    'Ob % RO_Confidence',        &
                    Int1 = Ob % RO_Confidence,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RO_geoid_und, &
                    'Ob % RO_geoid_und',        &
                    El1 = Ob % RO_geoid_und,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RO_Rad_Curv, &
                    'Ob % RO_Rad_Curv',        &
                    El1 = Ob % RO_Rad_Curv,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % GPSRO_heightcorrection, &
                    'Ob % GPSRO_heightcorrection',        &
                    Int1 = Ob % GPSRO_heightcorrection,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % GPSRO_cost_funct_test, &
                    'Ob % GPSRO_cost_funct_test',        &
                    Real1 = Ob % GPSRO_cost_funct_test,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % GPSRO_y_test, &
                    'Ob % GPSRO_y_test',        &
                    Real1 = Ob % GPSRO_y_test,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % GPSRO_n_iteration_test, &
                    'Ob % GPSRO_n_iteration_test',        &
                    Int1 = Ob % GPSRO_n_iteration_test,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % GPSRO_Delta_factor, &
                    'Ob % GPSRO_Delta_factor',        &
                    Real1 = Ob % GPSRO_Delta_factor,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % GPSRO_Delta_ct2, &
                    'Ob % GPSRO_Delta_ct2',        &
                    Real1 = Ob % GPSRO_Delta_ct2,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % GPSRO_OB_test, &
                    'Ob % GPSRO_OB_test',        &
                    Real1 = Ob % GPSRO_OB_test,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % GPSRO_qc_mask, &
                    'Ob % GPSRO_qc_mask',        &
                    Int1 = Ob % GPSRO_qc_mask,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % GPSRO_max_grad, &
                    'Ob % GPSRO_max_grad',        &
                    Real1 = Ob % GPSRO_max_grad,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % GPSRO_hgt_shift, &
                    'Ob % GPSRO_hgt_shift',        &
                    Real1 = Ob % GPSRO_hgt_shift,  &
                    sub = sub)

! Scatwind observation meta-data
CALL Ops_ObsAction (Ob % Header % ESAPCD, &
                    'Ob % ESAPCD',        &
                    Int1 = Ob % ESAPCD,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % WindRetPCD, &
                    'Ob % WindRetPCD',        &
                    Int1 = Ob % WindRetPCD,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % BeamIncAngle, &
                    'Ob % BeamIncAngle',        &
                    Real2 = Ob % BeamIncAngle,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % BeamLookAngle, &
                    'Ob % BeamLookAngle',        &
                    Real2 = Ob % BeamLookAngle,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % BackScatter, &
                    'Ob % BackScatter',        &
                    Real2 = Ob % BackScatter,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % BeamNoise, &
                    'Ob % BeamNoise',        &
                    Real2 = Ob % BeamNoise,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % MissShot, &
                    'Ob % MissShot',        &
                    Int2 = Ob % MissShot,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % DistConeRank1, &
                    'Ob % DistConeRank1',        &
                    Real1 = Ob % DistConeRank1,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % DistConeRank2, &
                    'Ob % DistConeRank2',        &
                    Real1 = Ob % DistConeRank2,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % WindRetSkill, &
                    'Ob % WindRetSkill',        &
                    Real1 = Ob % WindRetSkill,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ScatModel, &
                    'Ob % ScatModel',        &
                    Int1 = Ob % ScatModel,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % fff10AmbWind, &
                    'Ob % fff10AmbWind',        &
                    Real2 = Ob % fff10AmbWind,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ddd10AmbWind, &
                    'Ob % ddd10AmbWind',        &
                    Real2 = Ob % ddd10AmbWind,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % Distcone, &
                    'Ob % DistCone',        &
                    Real2 = Ob % DistCone,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ScatRainProb, &
                    'Ob % ScatRainProb',        &
                    Real1 = Ob % ScatRainProb,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ChosenWind, &
                    'Ob % ChosenWind',        &
                    Int1 = Ob % ChosenWind,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % NumBeams, &
                    'Ob % NumBeams',        &
                    Int1 = Ob % NumBeams,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % KuSolnLkhd, &
                    'Ob % KuSolnLkhd',        &
                    Real2 = Ob % KuSolnLkhd,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % ASCATL2WindSoftwareID, &
                    'Ob % ASCATL2WindSoftwareID',        &
                    Real1 = Ob % ASCATL2WindSoftwareID,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ASCATIceProb, &
                    'Ob % ASCATIceProb',        &
                    Real1 = Ob % ASCATIceProb,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ASCATIceAgeA, &
                    'Ob % ASCATIceAgeA',        &
                    Real1 = Ob % ASCATIceAgeA,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ASCATQCflag, &
                    'Ob % ASCATQCflag',        &
                    Int1 = Ob % ASCATQCflag,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ASCATNumberVectorAmby, &
                    'Ob % ASCATNumberVectorAmby',        &
                    Int1 = Ob % ASCATNumberVectorAmby,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SatDirectionOfMotion, &
                    'Ob % SatDirectionOfMotion',        &
                    Real1 = Ob % SatDirectionOfMotion,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ASCATWindRepCount, &
                    'Ob % ASCATWindRepCount',        &
                    Int1 = Ob % ASCATWindRepCount,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ASCATBackDstc, &
                    'Ob % ASCATBackDstc',        &
                    Real2 = Ob % ASCATBackDstc,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ASCATSolnLkhd, &
                    'Ob % ASCATSolnLkhd',        &
                    Real2 = Ob % ASCATSolnLkhd,  &
                    sub = sub)

! WINDSAT
CALL Ops_ObsAction (Ob % Header % WINDSATSurfType, &
                    'Ob % WINDSATSurfType',        &
                    Int1 = Ob % WINDSATSurfType,   &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % WINDSATWaterVapourContent, &
                    'Ob % WINDSATWaterVapourContent',        &
                    Real1 = Ob % WINDSATWaterVapourContent,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % WINDSATLiquidWaterContent, &
                    'Ob % WINDSATLiquidWaterContent',        &
                    Real1 = Ob % WINDSATLiquidWaterContent,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % WINDSATPrecipRate, &
                    'Ob % WINDSATPrecipRate',        &
                    Real1 = Ob % WINDSATPrecipRate,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % WINDSATSeaSurfTempError, &
                    'Ob % WINDSATSeaSurfTempError',        &
                    Real1 = Ob % WINDSATSeaSurfTempError,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % WINDSATWindSpeedError, &
                    'Ob % WINDSATWindSpeedError',        &
                    Real1 = Ob % WINDSATWindSpeedError,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % WINDSATWaterVapourError, &
                    'Ob % WINDSATWaterVapourError',        &
                    Real1 = Ob % WINDSATWaterVapourError,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % WINDSATLiquidWaterError, &
                    'Ob % WINDSATLiquidWaterError',        &
                    Real1 = Ob % WINDSATLiquidWaterError,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % WINDSATCHI2WindRetrieval, &
                    'Ob % WINDSATCHI2WindRetrieval',        &
                    Real2 = Ob % WINDSATCHI2WindRetrieval,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % WINDSATWindDirectionError, &
                    'Ob % WINDSATWindDirectionError',        &
                    Real2 = Ob % WINDSATWindDirectionError,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % WINDSATQCflagA, &
                    'Ob % WINDSATQCflagA',        &
                    Int1 = Ob % WINDSATQCflagA,   &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % WINDSATQCflagB, &
                    'Ob % WINDSATQCflagB',        &
                    Int1 = Ob % WINDSATQCflagB,   &
                    sub = sub)

! SuperOb meta-data
CALL Ops_ObsAction (Ob % Header % SuperobNused, &
                    'Ob % SuperobNused',        &
                    Int1 = Ob % SuperobNused,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SuperobStart, &
                    'Ob % SuperobStart',        &
                    Int1 = Ob % SuperobStart,   &
                    sub = sub)

! 2D Pressure level information
CALL Ops_ObsAction (Ob % Header % PlevelsA, &
                    'Ob % PlevelsA',        &
                    Coord2 = Ob % PlevelsA, &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % PlevelsB, &
                    'Ob % PlevelsB',        &
                    Coord2 = Ob % PlevelsB, &
                    sub = sub)

! UKMOSST observation meta-data
CALL Ops_ObsAction (Ob % Header % CloudClearMthd, &
                    'Ob % CloudClearMthd',        &
                    Int1 = Ob % CloudClearMthd,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AVHRRSSTQC, &
                    'Ob % AVHRRSSTQC',        &
                    Int1 = Ob % AVHRRSSTQC,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ClearAVHRRPxls, &
                    'Ob % ClearAVHRRPxls',        &
                    Real1 = Ob % ClearAVHRRPxls,  &
                    sub = sub)

! AATSR meta-data
CALL Ops_ObsAction (Ob % Header % NoDualPixels, &
                    'Ob % NoDualPixels',        &
                    Real1 = Ob % NoDualPixels,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SSTCnfd, &
                    'Ob % SSTCnfd',        &
                    Int1 = Ob % SSTCnfd,   &
                    sub = sub)

! Satwind BUFR observation meta-data
CALL Ops_ObsAction (Ob % Header % OrigCtr, &
                    'Ob % OrigCtr',        &
                    Int1 = Ob % OrigCtr,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ChanCtralFreq, &
                    'Ob % ChanCtralFreq',        &
                    Real1 = Ob % ChanCtralFreq,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % HeightAssMethod, &
                    'Ob % HeightAssMethod',        &
                    Real1 = Ob % HeightAssMethod,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CmptVecWindDirn, &
                    'Ob % CmptVecWindDirn',        &
                    Real2 = Ob % CmptVecWindDirn,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CmptVecWindSpeed, &
                    'Ob % CmptVecWindSpeed',        &
                    Real2 = Ob % CmptVecWindSpeed,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CmptVecWindU, &
                    'Ob % CmptVecWindU',        &
                    Real2 = Ob % CmptVecWindU,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CmptVecWindV, &
                    'Ob % CmptVecWindV',        &
                    Real2 = Ob % CmptVecWindV,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AltHeightAss, &
                    'Ob % AltHeightAss',        &
                    Real2 = Ob % AltHeightAss,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AltPressLevel, &
                    'Ob % AltPressLevel',        &
                    Real2 = Ob % AltPressLevel,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AltAirTempLevel, &
                    'Ob % AltAirTempLevel',        &
                    Real2 = Ob % AltAirTempLevel,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % MPEFQI, &
                    'Ob % MPEFQI',        &
                    Real2 = Ob % MPEFQI,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % MPEFQI2, &
                    'Ob % MPEFQI2',        &
                    Real2 = Ob % MPEFQI2,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RFFQI, &
                    'Ob % RFFQI',        &
                    Real2 = Ob % RFFQI,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SatwindQI, &
                    'Obs % SatwindQI',       &
                    Int1 = Ob % SatwindQI,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SatwindHeightError, &
                    'Obs % SatwindHeightError',       &
                    Real1 = Ob % SatwindHeightError,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SatwindBestFitU, &
                    'Obs % SatwindBestFitU',       &
                    Real1 = Ob % SatwindBestFitU,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SatwindBestFitV, &
                    'Obs % SatwindBestFitV',       &
                    Real1 = Ob % SatwindBestFitV,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SatwindBestFitPress, &
                    'Obs % SatwindBestFitPress',       &
                    Real1 = Ob % SatwindBestFitPress,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SatwindMaxSpeed, &
                    'Obs % SatwindMaxSpeed',       &
                    Real1 = Ob % SatwindMaxSpeed,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SatwindMinSpeed, &
                    'Obs % SatwindMinSpeed',       &
                    Real1 = Ob % SatwindMinSpeed,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CloudTopHeight, &
                    'Obs % CloudTopHeight',       &
                    Real1 = Ob % CloudTopHeight,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % StereoFreq, &
                    'Obs % StereoFreq',       &
                    Real1 = Ob % StereoFreq,  &
                    sub = sub)

! AOD observation data
CALL Ops_ObsAction (Ob % Header % AOD, &
                    'Ob % AOD',        &
                    El2 = Ob % AOD,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AODError, &
                    'Ob % AODError',        &
                    Real2 = Ob % AODError,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AngstromExponent, &
                    'Ob % AngstromExponent',        &
                    Real1 = Ob % AngstromExponent,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % MassConcentration, &
                    'Ob % MassConcentration',        &
                    Real1 = Ob % MassConcentration,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SmallModeFraction, &
                    'Ob % SmallModeFraction',        &
                    Real1 = Ob % SmallModeFraction,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % EffectiveRadius, &
                    'Ob % EffectiveRadius',        &
                    Real1 = Ob % EffectiveRadius,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AerosolType, &
                    'Ob % AerosolType',        &
                    Int1 = Ob % AerosolType,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AerosolConf, &
                    'Ob % AerosolConf',        &
                    Int1 = Ob % AerosolConf,   &
                    sub = sub)

!SBUV ozone observation meta-data
CALL Ops_ObsAction (Ob % Header % SBUVozone, &
                    'Ob % SBUVozone',        &
                    El2 = Ob % SBUVozone,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SBUVStDevOzone, &
                    'Ob % SBUVStDevOzone',        &
                    Real2 = Ob % SBUVStDevOzone,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SBUVUppLayerP, &
                    'Ob % SBUVUppLayerP',        &
                    Real2 = Ob % SBUVUppLayerP,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SBUVLowLayerP, &
                    'Ob % SBUVLowLayerP',        &
                    Real2 = Ob % SBUVLowLayerP,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SBUVSolarElev, &
                    'Ob % SBUVSolarElev',        &
                    Real1 = Ob % SBUVSolarElev,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SBUVQualityInf, &
                    'Ob % SBUVQualityInf',        &
                    Real1 = Ob % SBUVQualityInf,  &
                    sub = sub)

!Satwind Superob code
CALL Ops_ObsAction (Ob % Header % uSuperob, &
                    'Ob % uSuperob',        &
                    El2 = Ob % uSuperob,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % vSuperob, &
                    'Ob % vSuperob',        &
                    El2 = Ob % vSuperob,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SuperobRounds, &
                    'Ob % SuperobRounds',        &
                    Int1 = Ob % SuperobRounds,   &
                    sub = sub)

! TAMDAR qc flags
CALL Ops_ObsAction (Ob % Header % TAMDAR_qc_t, &
                    'Ob % TAMDAR_qc_t',        &
                    Int1 = Ob % TAMDAR_qc_t,   &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % TAMDAR_qc_ddd, &
                    'Ob % TAMDAR_qc_ddd',        &
                    Int1 = Ob % TAMDAR_qc_ddd,   &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % TAMDAR_qc_fff, &
                    'Ob % TAMDAR_qc_fff',        &
                    Int1 = Ob % TAMDAR_qc_fff,   &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % TAMDAR_qc_rh, &
                    'Ob % TAMDAR_qc_rh',        &
                    Real1 = Ob % TAMDAR_qc_rh,  &
                    sub = sub)

! MODE-S metadata
CALL Ops_ObsAction (Ob % Header % TransponderAddress, &
                    'Ob % TransponderAddress',        &
                    Char = Ob % TransponderAddress,   &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % AircraftType, &
                    'Ob % AircraftType',        &
                    Char = Ob % AircraftType,   &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % AircraftGNSSAltitude, &
                    'Ob % AircraftGNSSAltitude',        &
                    Real1 = Ob % AircraftGNSSAltitude,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % AircraftIndicatedAirSpeed, &
                    'Ob % AircraftIndicatedAirSpeed',        &
                    Real1 = Ob % AircraftIndicatedAirSpeed,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % AircraftTrueAirSpeed, &
                    'Ob % AircraftTrueAirSpeed',        &
                    Real1 = Ob % AircraftTrueAirSpeed,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % AircraftGroundSpeed, &
                    'Ob % AircraftGroundSpeed',        &
                    Real1 = Ob % AircraftGroundSpeed,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % AircraftTrueHeading, &
                    'Ob % AircraftTrueHeading',        &
                    Real1 = Ob % AircraftTrueHeading,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % AircraftGroundHeading, &
                    'Ob % AircraftGroundHeading',        &
                    Real1 = Ob % AircraftGroundHeading,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % AircraftRadarGroundSpeed, &
                    'Ob % AircraftRadarGroundSpeed',        &
                    Real1 = Ob % AircraftRadarGroundSpeed,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % AircraftRadarGroundHeading, &
                    'Ob % AircraftRadarGroundHeading',        &
                    Real1 = Ob % AircraftRadarGroundHeading,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % MachNumber, &
                    'Ob % MachNumber',        &
                    Real1 = Ob % MachNumber,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % AircraftRadarT, &
                    'Ob % AircraftRadarT',        &
                    Real1 = Ob % AircraftRadarT,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % AircraftGeomagneticDeviation, &
                    'Ob % AircraftGeomagneticDeviation',        &
                    Real1 = Ob % AircraftGeomagneticDeviation,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % AircraftMagneticHeadingBias, &
                    'Ob % AircraftMagneticHeadingBias',        &
                    Real1 = Ob % AircraftMagneticHeadingBias,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % AircraftRollAngle, &
                    'Ob % AircraftRollAngle',        &
                    Real1 = Ob % AircraftRollAngle,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % AircraftRadarDDD, &
                    'Ob % AircraftRadarDDD',        &
                    Real1 = Ob % AircraftRadarDDD,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % AircraftRadarU, &
                    'Ob % AircraftRadarU',        &
                    Real1 = Ob % AircraftRadarU,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % AircraftRadarV, &
                    'Ob % AircraftRadarV',        &
                    Real1 = Ob % AircraftRadarV,  &
                    sub = sub)

! Surface variables

CALL Ops_ObsAction (Ob % Header % Pstandard, &
                    'Ob % Pstandard',        &
                    El1 = Ob % Pstandard,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % Pstation, &
                    'Ob % Pstation',        &
                    El1 = Ob % Pstation,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % Pmsl, &
                    'Ob % Pmsl',        &
                    El1 = Ob % Pmsl,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % pstar, &
                    'Ob % pstar',        &
                    El1 = Ob % pstar,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % t2, &
                    'Ob % t2',        &
                    El1 = Ob % t2,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % td2, &
                    'Ob % td2',        &
                    El1 = Ob % td2,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % rh2, &
                    'Ob % rh2',        &
                    El1 = Ob % rh2,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % header % ddd10, &
                    'Ob % ddd10',        &
                    El1 = Ob % ddd10,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % fff10, &
                    'Ob % fff10',        &
                    El1 = Ob % fff10,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % u10, &
                    'Ob % u10',        &
                    El1 = Ob % u10,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % v10, &
                    'Ob % v10',        &
                    El1 = Ob % v10,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % vis, &
                    'Ob % vis',        &
                    El1 = Ob % vis,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % logvis, &
                    'Ob % logvis',        &
                    El1 = Ob % logvis,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % WindSpeed, &
                    'Ob % WindSpeed',        &
                    EL1 = Ob % WindSpeed,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % WaveHeight, &
                    'Ob % WaveHeight',        &
                    El1 = Ob % WaveHeight,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % WAVE_HGHT, &
                    'Ob % WAVE_HGHT',        &
                    El1 = Ob % WAVE_HGHT,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % WIND_SPED, &
                    'Ob % WIND_SPED',        &
                    El1 = Ob % WIND_SPED,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SeaHeight, &
                    'Ob % SeaHeight',        &
                    El1 = Ob % SeaHeight,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SeaIce, &
                    'Ob % SeaIce',        &
                    El1 = Ob % SeaIce,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CHL, &
                    'Ob % CHL',        &
                    El1 = Ob % CHL,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % KD490, &
                    'Ob % KD490',        &
                    El1 = Ob % KD490,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SST, &
                    'Ob % SST',        &
                    El1 = Ob % SST,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % Tskin, &
                    'Ob % Tskin',        &
                    El1 = Ob % Tskin,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % TCWV, &
                    'Ob % TCWV',        &
                    El1 = Ob % TCWV,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % LWP, &
                    'Ob % LWP',        &
                    El1 = Ob % LWP,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RetLWP, &
                    'Ob % RetLWP',        &
                    El1 = Ob % RetLWP,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % IWP, &
                    'Ob % IWP',        &
                    El1 = Ob % IWP,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % Rainrate, &
                    'Ob % Rainrate',        &
                    El2 = Ob % Rainrate,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % Snowrate, &
                    'Ob % Snowrate',        &
                    El2 = Ob % Snowrate,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % Rainamount, &
                    'Ob % Rainamount',        &
                    Real2 = Ob % Rainamount,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % u10AmbWind, &
                    'Ob % u10AmbWind',        &
                    El2 = Ob % u10AmbWind,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % v10AmbWind, &
                    'Ob % v10AmbWind',        &
                    El2 = Ob % v10AmbWind,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AWPriorPcorrect, &
                    'Ob % AWPriorPCorrect',        &
                    El2 = Ob % AWPriorPCorrect,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % GPSTZDelay, &
                    'Ob % GPSTZDelay',        &
                    El1 = Ob % GPSTZDelay,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % TotalCloudAmount, &
                    'Ob % TotalCloudAmount',        &
                    El1 = Ob % TotalCloudAmount,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SeaSrfcHeight, &
                    'Ob % SeaSrfcHeight',        &
                    El1 = Ob % SeaSrfcHeight,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % DyncAtmrCortn, &
                    'Ob % DyncAtmrCortn',        &
                    Real1 = Ob % DyncAtmrCortn,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % LongWaveErrr, &
                    'Ob % LongWaveErrr',        &
                    Real1 = Ob % LongWaveErrr,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % OcnTide, &
                    'Ob % OcnTide',        &
                    Real1 = Ob % OcnTide,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % MeanSeaHeight, &
                    'Ob % MeanSeaHeight',        &
                    Real1 = Ob % MeanSeaHeight,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % PrWx, &
                    'Ob % PrWx',        &
                    Real1 = Ob % PrWx,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % LwstClod, &
                    'Ob % LwstClod',        &
                    Real1 = Ob % LwstClod,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % LwstClodType, &
                    'Ob % LwstClodType',        &
                    Real1 = Ob % LwstClodType,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % Precip1hr, &
                    'Ob % Precip1hr',        &
                    Real1 = Ob % Precip1hr,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % WaveMeasureMethod, &
                    'Ob % WaveMeasureMethod',        &
                    Int1 = Ob % WaveMeasureMethod,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % GlobalRad1hr, &
                    'Ob % GlobalRad1hr',        &
                    Real1 = Ob % GlobalRad1hr,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % WindGust, &
                    'Ob % WindGust',        &
                    Real1 = Ob % WindGust,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SGNT_WAVE_HGHT, &
                    'Ob % SGNT_WAVE_HGHT',        &
                    Real1 = Ob % SGNT_WAVE_HGHT,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AVGNG_PERD, &
                    'Ob % AVGNG_PERD',        &
                    Real1 = Ob % AVGNG_PERD,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CSNT_TYP, &
                    'Ob % CSNT_TYP',        &
                    Int1 = Ob % CSNT_TYP,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SITE_QAL, &
                    'Ob % SITE_QAL',        &
                    Int1 = Ob % SITE_QAL,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % DCML_SCL_MASS_DNSTY, &
                    'Ob % DCML_SCL_MASS_DNSTY',        &
                    Int1 = Ob % DCML_SCL_MASS_DNSTY,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % MASS_DNSTY, &
                    'Ob % MASS_DNSTY',        &
                    El1 = Ob % MASS_DNSTY,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SO2_DNSTY, &
                    'Ob % SO2_DNSTY',        &
                    El1 = Ob % SO2_DNSTY,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % PM10_DNSTY, &
                    'Ob % PM10_DNSTY',        &
                    El1 = Ob % PM10_DNSTY,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % PM2p5_DNSTY, &
                    'Ob % PM2p5_DNSTY',        &
                    El1 = Ob % PM2p5_DNSTY,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % O3_DNSTY, &
                    'Ob % O3_DNSTY',        &
                    El1 = Ob % O3_DNSTY,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % NO2_DNSTY, &
                    'Ob % NO2_DNSTY',        &
                    El1 = Ob % NO2_DNSTY,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CO_DNSTY, &
                    'Ob % CO_DNSTY',        &
                    El1 = Ob % CO_DNSTY,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AtmrNmbr, &
                    'Ob % AtmrNmbr',        &
                    Real1 = Ob % AtmrNmbr,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % LghnStrk, &
                    'Ob % LghnStrk',        &
                    Real1 = Ob % LghnStrk,  &
                    sub = sub)

! Radar radial velocity
CALL Ops_ObsAction (Ob % Header % Reflectivity, &
                    'Ob % Reflectivity',        &
                    El2 = Ob % Reflectivity,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ReflectivitySO, &
                    'Ob % ReflectivitySO',        &
                    El2 = Ob % ReflectivitySO,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ReflectivityR, &
                    'Ob % ReflectivityR',        &
                    El2 = Ob % ReflectivityR,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ReflectivityI, &
                    'Ob % ReflectivityI',        &
                    El2 = Ob % ReflectivityI,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadialVelocity, &
                    'Ob % RadialVelocity',        &
                    El2 = Ob % RadialVelocity,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadarObRange, &
                    'Ob % RadarObRange',        &
                    Real2 = Ob % RadarObRange,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadarObAzim, &
                    'Ob % RadarObAzim',        &
                    Real2 = Ob % RadarObAzim,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadarBeamElev, &
                    'Ob % RadarBeamElev',        &
                    Real2 = Ob % RadarBeamElev,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadialVelocSO, &
                    'Ob % RadialVelocSO',        &
                    El2 = Ob % RadialVelocSO,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadarObRangSO, &
                    'Ob % RadarObRangSO',        &
                    Real2 = Ob % RadarObRangSO,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadarObAzimSO, &
                    'Ob % RadarObAzimSO',        &
                    Real2 = Ob % RadarObAzimSO,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadIdent, &
                    'Ob % RadIdent',        &
                    Int2 = Ob % RadIdent,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadSiteLat, &
                    'Ob % RadSiteLat',        &
                    Real2 = Ob % RadSiteLat,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadSiteLong, &
                    'Ob % RadSiteLong',        &
                    Real2 = Ob % RadSiteLong,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadAltAboveMSL, &
                    'Ob % RadAltAboveMSL',        &
                    Real2 = Ob % RadAltAboveMSL,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadRangeStart, &
                    'Ob % RadRangeStart',        &
                    Real2 = Ob % RadRangeStart,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadUnfoldVelocity, &
                    'Ob % RadUnfoldVelocity',        &
                    Real2 = Ob % RadUnfoldVelocity,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadFlag, &
                    'Ob % RadFlag',        &
                    Int2 = Ob % RadFlag,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadNbRay, &
                    'Ob % RadNbRay',        &
                    Real1 = Ob % RadNbRay,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadRayRes, &
                    'Ob % RadRayRes',        &
                    Real1 = Ob % RadRayRes,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadGateRes, &
                    'Ob % RadGateRes',        &
                    Real1 = Ob % RadGateRes,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadNbGate, &
                    'Ob % RadNbGate',        &
                    Real1 = Ob % RadNbGate,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadSW, &
                    'Ob % RadSW',        &
                    Real2 = Ob % RadSW,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadSW, &
                    'Ob % RadSW',        &
                    Real2 = Ob % RadSW,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadNoiseLvl, &
                    'Ob % RadNoiseLvl',        &
                    Real2 = Ob % RadNoiseLvl,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadPhase, &
                    'Ob % RadPhase',        &
                    Real2 = Ob % RadPhase,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadPhaseSDP, &
                    'Ob % RadPhaseSDP',        &
                    Real2 = Ob % RadPhaseSDP,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadRefractivity, &
                    'Ob % RadRefractivity',        &
                    Real2 = Ob % RadRefractivity,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RadNcoFreq, &
                    'Ob % RadNcoFreq',        &
                    Real2 = Ob % RadNcoFreq,  &
                    sub = sub)
! Autosat cloud products

CALL Ops_ObsAction (Ob % Header % AUTOSATSpCTest, &
                    'Ob % AUTOSATSpCTest',        &
                    Int1 = Ob % AUTOSATSpCTest,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AUTOSATCTP, &
                    'Ob % AUTOSATCTP',        &
                    Real2 = Ob % AUTOSATCTP,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AUTOSATECA, &
                    'Ob % AUTOSATECA',        &
                    Real2 = Ob % AUTOSATECA,  &
                    sub = sub)

! Radar precipitation weight and mask data

CALL Ops_ObsAction (Ob % Header % RadarRainWeight, &
                    'Ob % RadarRainWeight',        &
                    Real1 = Ob % RadarRainWeight,  &
                    sub = sub)

! upper air variables

CALL Ops_ObsAction (Ob % Header % z, &
                    'Ob % z',        &
                    El2 = Ob % z,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % t, &
                    'Ob % t',        &
                    El2 = Ob % t,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % td, &
                    'Ob % td',        &
                    El2 = Ob % td,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % theta, &
                    'Ob % theta',        &
                    El2 = Ob % theta,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % rh, &
                    'Ob % rh',        &
                    El2 = Ob % rh,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % MixingRatio, &
                    'Ob % mixing_ratio',       &
                    El2 = Ob % MixingRatio,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % q, &
                    'Ob % q',        &
                    El2 = Ob % q,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % u, &
                    'Ob % u',        &
                    El2 = Ob % u,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % v, &
                    'Ob % v',        &
                    El2 = Ob % v,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ddd, &
                    'Ob % ddd',        &
                    El2 = Ob % ddd,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % fff, &
                    'Ob % fff',        &
                    El2 = Ob % fff,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % Salt, &
                    'Ob % Salt',        &
                    El2 = Ob % Salt,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % BriTemp, &
                    'Ob % BriTemp',        &
                    Real2 = Ob % BriTemp,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % NumChans, &
                    'Ob % NumChans',        &
                    Int1 = Ob % NumChans,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ChanNum, &
                    'Ob % ChanNum',        &
                    Int2 = Ob % ChanNum,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ChannelWavenumber, &
                    'Ob % ChannelWavenumber',        &
                    Real2 = Ob % ChannelWavenumber,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CLW, &
                    'Ob % CLW',        &
                    El2 = Ob % CLW,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CIW, &
                    'Ob % CIW',        &
                    El2 = Ob % CIW,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % Cloud, &
                    'Ob % Cloud',        &
                    El2 = Ob % Cloud,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ozone, &
                    'Ob % ozone',        &
                    El2 = Ob % ozone,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % Refrac, &
                    'Ob % Refrac',        &
                    El2 = Ob % Refrac,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % BendingAngleAll, &
                    'Ob % BendingAngleAll',        &
                    EL2 = Ob % BendingAngleAll,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % BendingAngleOrig, &
                    'Ob % BendingAngleOrig',        &
                    EL2 = Ob % BendingAngleOrig,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ImpactParamAll, &
                    'Ob % ImpactParamAll',        &
                    EL2 = Ob % ImpactParamAll,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ImpactParamOrig, &
                    'Ob % ImpactParamOrig',        &
                    Real2 = Ob % ImpactParamOrig,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RO_mean_freq, &
                    'Ob % RO_mean_freq',        &
                    Real2 = Ob % RO_mean_freq,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % BendingAngle, &
                    'Ob % BendingAngle',        &
                    El2 = Ob % BendingAngle,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ImpactParam, &
                    'Ob % ImpactParam',        &
                    El2 = Ob % ImpactParam,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % gpsro_orig_ob, &
                    'Ob % gpsro_orig_ob',        &
                    Int1 = Ob % gpsro_orig_ob,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % gpsro_orig_level, &
                    'Ob % gpsro_orig_level',        &
                    Int1 = Ob % gpsro_orig_level,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % p, &
                    'Ob % p',        &
                    El2 = Ob % p,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % level_time, &
                    'Ob % level_time',        &
                    Real2 = Ob % level_time,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % level_lat, &
                    'Ob % level_lat',        &
                    Real2 = Ob % level_lat,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % level_lon, &
                    'Ob % level_lon',        &
                    Real2 = Ob % level_lon,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % model_level_time, &
                    'Ob % model_level_time',        &
                    Real2 = Ob % model_level_time,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % model_level_lat, &
                    'Ob % model_level_lat',        &
                    Real2 = Ob % model_level_lat,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % model_level_lon, &
                    'Ob % model_level_lon',        &
                    Real2 = Ob % model_level_lon,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % level_azimuth, &
                    'Ob % level_azimuth',        &
                    Real2 = Ob % level_azimuth,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % Altitude, &
                    'Ob % Altitude',        &
                    Real2 = Ob % Altitude,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % Collection_Centre, &
                    'Ob % Collection_Centre',        &
                    Char = Ob % Collection_Centre,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % LNG_STTN_NAME, &
                    'Ob % LNG_STTN_NAME',        &
                    Char = Ob % LNG_STTN_NAME,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % GEMS_AQTY_OBSG_STTN, &
                    'Ob % GEMS_AQTY_OBSG_STTN',        &
                    Char = Ob % GEMS_AQTY_OBSG_STTN,   &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % NEDTCold, &
                    'Ob % NEDTCold',        &
                    Real2 = Ob % NEDTCold,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % NEDTWarm, &
                    'Ob % NEDTWarm',        &
                    Real2 = Ob % NEDTWarm,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AAPPCirrus, &
                    'Ob % AAPPCirrus',        &
                    Real1 = Ob % AAPPCirrus,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AAPPBennartz, &
                    'Ob % AAPPBennartz',        &
                    Real1 = Ob % AAPPBennartz,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % LowestCloudBaseHeight, &
                    'Ob % LowestCloudBaseHeight',        &
                    Real1 = Ob % LowestCloudBaseHeight,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % MediumCloudType, &
                    'Ob % MediumCloudType',        &
                    Int1 = Ob % MediumCloudType,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % HighCloudType, &
                    'Ob % HighCloudType',        &
                    Int1 = Ob % HighCloudType,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CloudAmount8Group, &
                    'Ob % CloudAmount8Group',        &
                    Real2 = Ob % CloudAmount8Group,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CloudType8Group, &
                    'Ob % CloudType8Group',        &
                    Int2 = Ob % CloudType8Group,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CloudBaseHeight8Group, &
                    'Ob % CloudBaseHeight8Group',        &
                    Real2 = Ob % CloudBaseHeight8Group,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % station_report_type, &
                    'Ob % station_report_type',        &
                    Int1 = Ob % station_report_type,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % RainAccum, &
                    'Ob % RainAccum',        &
                    El1 = Ob % RainAccum,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % SiteType, &
                    'Ob % SiteType',        &
                    Int1 = Ob % SiteType,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % BiasBriTemp, &
                    'Ob % BiasBriTemp',        &
                    Real2 = Ob % BiasBriTemp,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CeilBackscatter, &
                    'Ob % CeilBackscatter',        &
                    El2 = Ob % CeilBackscatter,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CeilCBH, &
                    'Ob % CeilCBH',        &
                    El2 = Ob % CeilCBH,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CeilRange, &
                    'Ob % CeilRange',        &
                    Real2 = Ob % CeilRange,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % OpenRoadWindSpeed, &
                    'Ob % OpenRoadWindSpeed',        &
                    Real2 = Ob % OpenRoadWindSpeed,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % OpenRoadWindDirection, &
                    'Ob % OpenRoadWindDirection',        &
                    Real2 = Ob % OpenRoadWindDirection,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % OpenRoadTemp, &
                    'Ob % OpenRoadTemp',        &
                    Real2 = Ob % OpenRoadTemp,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % OpenRoadDewPointTemp, &
                    'Ob % OpenRoadDewPointTemp',        &
                    Real2 = Ob % OpenRoadDewPointTemp,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % OpenRoadVisibility, &
                    'Ob % OpenRoadVisibility',        &
                    Real2 = Ob % OpenRoadVisibility,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CeilSiteID, &
                    'Ob % CeilSiteID',        &
                    Int2 = Ob % CeilSiteID,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CeilScanIdent, &
                    'Ob % CeilScanIdent',        &
                    Int2 = Ob % CeilScanIdent,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CeilWindowTau, &
                    'Ob % CeilWindowTau',        &
                    Real2 = Ob % CeilWindowTau,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % CeilLPE, &
                    'Ob % CeilLPE',        &
                    Real2 = Ob % CeilLPE,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % HLOSwind, &
                    'Ob % HLOSwind',        &
                    El1 = Ob % HLOSwind,    &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ProfileNo, &
                    'Ob % ProfileNo',        &
                    Int1 = Ob % ProfileNo,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % dWinddT, &
                    'Ob % dWinddT',        &
                    Real1 = Ob % dWinddT,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % dWinddP, &
                    'Ob % dWinddP',        &
                    Real1 = Ob % dWinddP,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AzimuthCOG, &
                    'Ob % AzimuthCOG',        &
                    Real1 = Ob % AzimuthCOG,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % HeightCOG, &
                    'Ob % HeightCOG',        &
                    Real1 = Ob % HeightCOG,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % HeightTop, &
                    'Ob % HeightTop',        &
                    Real1 = Ob % HeightTop,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % HeightBottom, &
                    'Ob % HeightBottom',        &
                    Real1 = Ob % HeightBottom,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % WindErr, &
                    'Ob % WindErr',        &
                    Real1 = Ob % WindErr,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % LidarClass, &
                    'LidarClass',             &
                    Real1 = Ob % LidarClass,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % ChannelNumber, &
                    'Ob % ChannelNumber',        &
                    Int1 = Ob % ChannelNumber,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % AladinConfFlag, &
                    'Ob % AladinConfFlag',        &
                    Int1 = Ob % AladinConfFlag,   &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % WindErrRejLimit, &
                    'Ob % WindErrRejLimit',        &
                    Real1 = Ob % WindErrRejLimit,  &
                    sub = sub)

! ARGO report qc flags
CALL Ops_ObsAction (Ob % Header % ArgoPositionQCFlag, &
                    'Ob % ArgoPositionQCFlag',        &
                    Int1 = Ob % ArgoPositionQCFlag,   &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % ArgoPressureQCFlag, &
                    'Ob % ArgoPressureQCFlag',        &
                    Int2 = Ob % ArgoPressureQCFlag,   &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % ArgoTemperatureQCFlag, &
                    'Ob % ArgoTemperatureQCFlag',        &
                    Int2 = Ob % ArgoTemperatureQCFlag,   &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % ArgoSalinityQCFlag, &
                    'Ob % ArgoSalinityQCFlag',        &
                    Int2 = Ob % ArgoSalinityQCFlag,   &
                    sub = sub)

! BUOY report type
CALL Ops_ObsAction (Ob % Header % buoy_report_type, &
                    'Ob % buoy_report_type',        &
                    Int1 = Ob % buoy_report_type,   &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % time_difference, &
                    'Ob % time_difference',        &
                    Int1 = Ob % time_difference,   &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % xbt_depth_factor, &
                    'Ob % xbt_depth_factor',        &
                    Real1 = Ob % xbt_depth_factor,  &
                    sub = sub)

CALL Ops_ObsAction (Ob % Header % background_temperature, &
                    'Ob % background_temperature',        &
                    Real2 = Ob % background_temperature,  &
                    sub = sub)
CALL Ops_ObsAction (Ob % Header % background_salt, &
                    'Ob % background_salt',        &
                    Real2 = Ob % background_salt,  &
                    sub = sub)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ObsGlobalAction
