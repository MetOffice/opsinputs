!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Sets up the ElemDesp2ODBDesp data structure.
!
! ElemDesp2ODBDesp is an array that serves as a bridge between OPS Element
! files and ODB Table-Columns.  For each element index i, ElementDescrip (i,1)
! defines a parameter in one of element files
!    ElemDesp2ODBDesp(i)%TableName:     The table name in which
!                                       this parameter is defined
!    ElemDesp2ODBDesp(i)%ColName  :     The column name of this parameter
!                                       in the Table
!    ElemDesp2ODBDesp(i)%Varno(:) :     The varno for this parameter
!       Depending on ObsGroup, sometimes one element can have multiple
!       varnos (up to 4).
!
! As a new element is added to OPS or a new parameter is added to ODB
! The mapping has to be added to this subroutine.
!-------------------------------------------------------------------------------

SUBROUTINE OpsODB_InitODBElemDesp (h,        &
                                   ObsGroup)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE OpsMod_Index

USE OpsMod_ObsGroupInfo, ONLY: &
    ObsGroupAircraft,          &
    ObsGroupATMS,              &
    ObsGroupCrIS,              &
    ObsGroupHIRAS,             &
    ObsGroupGIIRSLW,           &    
    ObsGroupGIIRSMW,           &
    ObsGroupOcean,             &
    ObsGroupSatwind,           &
    ObsGroupSurface

USE OpsMod_ODBOPSInterface, ONLY: &
    ElemDesp2ODBDesp

USE OpsMod_ODBTypes, ONLY: &
    ODBSession_type

USE OpsMod_ODB2, ONLY: &
    odb_integer,       &
    odb_real,          &
    odb_string,        &
    c_false

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(IN) :: h         ! ODB handle
INTEGER, INTENT(IN)                :: ObsGroup  ! Obs group Name

! Local declarations:
CHARACTER(len=*),PARAMETER         :: RoutineName = "OpsODB_InitODBElemDesp"

!-------------------------------------------------------------------------------
! 0. Tracing
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!----------------------------------------------------------------------------
! 1. Mapping for each Element
!----------------------------------------------------------------------------

ElemDesp2ODBDesp(IndexLatitude) % TableName                      = 'hdr'
ElemDesp2ODBDesp(IndexLatitude) % ColName                        = 'lat'
ElemDesp2ODBDesp(IndexLatitude) % data_type                      = odb_real
ElemDesp2ODBDesp(IndexLatitude) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexLongitude) % TableName                     = 'hdr'
ElemDesp2ODBDesp(IndexLongitude) % ColName                       = 'lon'
ElemDesp2ODBDesp(IndexLongitude) % data_type                     = odb_real
ElemDesp2ODBDesp(IndexLongitude) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexYear) % TableName                          = 'hdr'
ElemDesp2ODBDesp(IndexYear) % ColName                            = 'date'
ElemDesp2ODBDesp(IndexYear) % data_type                          = odb_integer
ElemDesp2ODBDesp(IndexYear) % has_missing                        = c_false

ElemDesp2ODBDesp(IndexMonth) % TableName                         = 'hdr'
ElemDesp2ODBDesp(IndexMonth) % ColName                           = 'date'
ElemDesp2ODBDesp(IndexMonth) % data_type                         = odb_integer
ElemDesp2ODBDesp(IndexMonth) % has_missing                       = c_false

ElemDesp2ODBDesp(IndexDay) % TableName                           = 'hdr'
ElemDesp2ODBDesp(IndexDay) % ColName                             = 'date'
ElemDesp2ODBDesp(IndexDay) % data_type                           = odb_integer
ElemDesp2ODBDesp(IndexDay) % has_missing                         = c_false

ElemDesp2ODBDesp(IndexHour) % TableName                          = 'hdr'
ElemDesp2ODBDesp(IndexHour) % ColName                            = 'time'
ElemDesp2ODBDesp(IndexHour) % data_type                          = odb_integer
ElemDesp2ODBDesp(IndexHour) % has_missing                        = c_false

ElemDesp2ODBDesp(IndexMinute) % TableName                        = 'hdr'
ElemDesp2ODBDesp(IndexMinute) % ColName                          = 'time'
ElemDesp2ODBDesp(IndexMinute) % data_type                        = odb_integer
ElemDesp2ODBDesp(IndexMinute) % has_missing                      = c_false

ElemDesp2ODBDesp(IndexSecond) % TableName                        = 'hdr'
ElemDesp2ODBDesp(IndexSecond) % ColName                          = 'time'
ElemDesp2ODBDesp(IndexSecond) % data_type                        = odb_integer
ElemDesp2ODBDesp(IndexSecond) % has_missing                      = c_false

ElemDesp2ODBDesp(IndexReceiptYear) % TableName                   = 'hdr'
ElemDesp2ODBDesp(IndexReceiptYear) % ColName                     = 'receipt_date'
ElemDesp2ODBDesp(IndexReceiptYear) % data_type                   = odb_integer
ElemDesp2ODBDesp(IndexReceiptYear) % has_missing                 = c_false

ElemDesp2ODBDesp(IndexReceiptMonth) % TableName                  = 'hdr'
ElemDesp2ODBDesp(IndexReceiptMonth) % ColName                    = 'receipt_date'
ElemDesp2ODBDesp(IndexReceiptMonth) % data_type                  = odb_integer
ElemDesp2ODBDesp(IndexReceiptMonth) % has_missing                = c_false

ElemDesp2ODBDesp(IndexReceiptDay) % TableName                    = 'hdr'
ElemDesp2ODBDesp(IndexReceiptDay) % ColName                      = 'receipt_date'
ElemDesp2ODBDesp(IndexReceiptDay) % data_type                    = odb_integer
ElemDesp2ODBDesp(IndexReceiptDay) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexReceiptHour) % TableName                   = 'hdr'
ElemDesp2ODBDesp(IndexReceiptHour) % ColName                     = 'receipt_time'
ElemDesp2ODBDesp(IndexReceiptHour) % data_type                   = odb_integer
ElemDesp2ODBDesp(IndexReceiptHour) % has_missing                 = c_false

ElemDesp2ODBDesp(IndexReceiptMinute) % TableName                 = 'hdr'
ElemDesp2ODBDesp(IndexReceiptMinute) % ColName                   = 'receipt_time'
ElemDesp2ODBDesp(IndexReceiptMinute) % data_type                 = odb_integer
ElemDesp2ODBDesp(IndexReceiptMinute) % has_missing               = c_false

ElemDesp2ODBDesp(IndexRADI_SNDE_TYPE) % TableName                = 'conv'
ElemDesp2ODBDesp(IndexRADI_SNDE_TYPE) % ColName                  = 'sonde_type'
ElemDesp2ODBDesp(IndexRADI_SNDE_TYPE) % data_type                = odb_integer
ElemDesp2ODBDesp(IndexRADI_SNDE_TYPE) % has_missing              = c_false

ElemDesp2ODBDesp(IndexRPRT_IDNY) % TableName                     = 'conv'
ElemDesp2ODBDesp(IndexRPRT_IDNY) % ColName                       = 'sonde_report_identifier'
ElemDesp2ODBDesp(IndexRPRT_IDNY) % data_type                     = odb_integer
ElemDesp2ODBDesp(IndexRPRT_IDNY) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexTRCKG_SYTM) % TableName                    = 'conv'
ElemDesp2ODBDesp(IndexTRCKG_SYTM) % ColName                      = 'sonde_tracking_system'
ElemDesp2ODBDesp(IndexTRCKG_SYTM) % data_type                    = odb_integer
ElemDesp2ODBDesp(IndexTRCKG_SYTM) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexRADTN_CORTN) % TableName                   = 'conv'
ElemDesp2ODBDesp(IndexRADTN_CORTN) % ColName                     = 'sonde_rad_corr'
ElemDesp2ODBDesp(IndexRADTN_CORTN) % data_type                   = odb_real
ElemDesp2ODBDesp(IndexRADTN_CORTN) % has_missing                 = c_false

ElemDesp2ODBDesp(IndexLEVL_RPLTN_CONT) % TableName               = 'hdr'
ElemDesp2ODBDesp(IndexLEVL_RPLTN_CONT) % ColName                 = 'numlev'
ElemDesp2ODBDesp(IndexLEVL_RPLTN_CONT) % data_type               = odb_integer
ElemDesp2ODBDesp(IndexLEVL_RPLTN_CONT) % has_missing             = c_false

ElemDesp2ODBDesp(IndexLEVL_CDTN_CODE) % TableName                = 'conv'
ElemDesp2ODBDesp(IndexLEVL_CDTN_CODE) % ColName                  = 'level_condition_code'
ElemDesp2ODBDesp(IndexLEVL_CDTN_CODE) % data_type                = odb_integer
ElemDesp2ODBDesp(IndexLEVL_CDTN_CODE) % has_missing              = c_false

ElemDesp2ODBDesp(IndexSatID) % TableName                         = 'sat'
ElemDesp2ODBDesp(IndexSatID) % ColName                           = 'satellite_identifier'
ElemDesp2ODBDesp(IndexSatID) % data_type                         = odb_integer
ElemDesp2ODBDesp(IndexSatID) % has_missing                       = c_false

ElemDesp2ODBDesp(IndexWMOBlockNo) % TableName                    = 'conv'
ElemDesp2ODBDesp(IndexWMOBlockNo) % ColName                      = 'wmo_block_number'
ElemDesp2ODBDesp(IndexWMOBlockNo) % data_type                    = odb_integer
ElemDesp2ODBDesp(IndexWMOBlockNo) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexWMORegNo) % TableName                      = 'conv'
ElemDesp2ODBDesp(IndexWMORegNo) % ColName                        = 'wmo_region_number'
ElemDesp2ODBDesp(IndexWMORegNo) % data_type                      = odb_integer
ElemDesp2ODBDesp(IndexWMORegNo) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexWMOStnNo) % TableName                      = 'conv'
ElemDesp2ODBDesp(IndexWMOStnNo) % ColName                        = 'wmo_station_number'
ElemDesp2ODBDesp(IndexWMOStnNo) % data_type                      = odb_integer
ElemDesp2ODBDesp(IndexWMOStnNo) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexWMOStnIdxNo) % TableName                   = 'conv'
ElemDesp2ODBDesp(IndexWMOStnIdxNo) % ColName                     = 'wmo_station_number'

ElemDesp2ODBDesp(IndexBuoyID) % TableName                        = 'conv'
ElemDesp2ODBDesp(IndexBuoyID) % ColName                          = 'buoy_identifier'
ElemDesp2ODBDesp(IndexBuoyID) % data_type                        = odb_integer
ElemDesp2ODBDesp(IndexBuoyID) % has_missing                      = c_false

ElemDesp2ODBDesp(IndexCallSign) % TableName                      = 'hdr'
ElemDesp2ODBDesp(IndexCallSign) % ColName                        = 'statid'
ElemDesp2ODBDesp(IndexCallSign) % data_type                      = odb_string
ElemDesp2ODBDesp(IndexCallSign) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexBECN_RPRT_NAME) % TableName                = 'conv'
ElemDesp2ODBDesp(IndexBECN_RPRT_NAME) % ColName                  = 'beacon_report_name'
ElemDesp2ODBDesp(IndexBECN_RPRT_NAME) % data_type                = odb_string
ElemDesp2ODBDesp(IndexBECN_RPRT_NAME) % has_missing              = c_false

ElemDesp2ODBDesp(IndexCOLTN_CNTR) % TableName                    = 'conv'
ElemDesp2ODBDesp(IndexCOLTN_CNTR) % ColName                      = 'collection_centre'
ElemDesp2ODBDesp(IndexCOLTN_CNTR) % data_type                    = odb_string
ElemDesp2ODBDesp(IndexCOLTN_CNTR) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexGTSCentre) % TableName                     = 'conv'
ElemDesp2ODBDesp(IndexGTSCentre) % ColName                       = 'collection_identifier'
ElemDesp2ODBDesp(IndexGTSCentre) % data_type                     = odb_integer
ElemDesp2ODBDesp(IndexGTSCentre) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexTailNumber) % TableName                    = 'conv'
ElemDesp2ODBDesp(IndexTailNumber) % ColName                      = 'aircraft_tail_number'
ElemDesp2ODBDesp(IndexTailNumber) % data_type                    = odb_string
ElemDesp2ODBDesp(IndexTailNumber) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexAcarsStatus) % TableName                   = 'conv'
ElemDesp2ODBDesp(IndexAcarsStatus) % ColName                     = 'aircraft_data_relay_system'
ElemDesp2ODBDesp(IndexAcarsStatus) % data_type                   = odb_integer
ElemDesp2ODBDesp(IndexAcarsStatus) % has_missing                 = c_false

ElemDesp2ODBDesp(IndexBogusID) % TableName                       = 'conv'
ElemDesp2ODBDesp(IndexBogusID) % ColName                         = 'bogus_identity'
ElemDesp2ODBDesp(IndexBogusID) % data_type                       = odb_string
ElemDesp2ODBDesp(IndexBogusID) % has_missing                     = c_false

ElemDesp2ODBDesp(IndexPositionAccuracyFlag) % TableName          = 'conv'
ElemDesp2ODBDesp(IndexPositionAccuracyFlag) % ColName            = 'position_accuracy_flag'
ElemDesp2ODBDesp(IndexPositionAccuracyFlag) % data_type          = odb_integer
ElemDesp2ODBDesp(IndexPositionAccuracyFlag) % has_missing        = c_false

ElemDesp2ODBDesp(IndexBuoySpeed) % TableName                     = 'conv'
ElemDesp2ODBDesp(IndexBuoySpeed) % ColName                       = 'buoy_speed'
ElemDesp2ODBDesp(IndexBuoySpeed) % data_type                     = odb_real
ElemDesp2ODBDesp(IndexBuoySpeed) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexShipSpeed) % TableName                     = 'conv'
ElemDesp2ODBDesp(IndexShipSpeed) % ColName                       = 'ship_speed'
ElemDesp2ODBDesp(IndexShipSpeed) % data_type                     = odb_real
ElemDesp2ODBDesp(IndexShipSpeed) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexObPractice) % TableName                    = 'conv'
ElemDesp2ODBDesp(IndexObPractice) % ColName                      = 'ob_practice'
ElemDesp2ODBDesp(IndexObPractice) % data_type                    = odb_integer
ElemDesp2ODBDesp(IndexObPractice) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexBuoyDirection) % TableName                 = 'conv'
ElemDesp2ODBDesp(IndexBuoyDirection) % ColName                   = 'buoy_direction'
ElemDesp2ODBDesp(IndexBuoyDirection) % data_type                 = odb_real
ElemDesp2ODBDesp(IndexBuoyDirection) % has_missing               = c_false

ElemDesp2ODBDesp(IndexShipDirec) % TableName                     = 'conv'
ElemDesp2ODBDesp(IndexShipDirec) % ColName                       = 'ship_direction'
ElemDesp2ODBDesp(IndexShipDirec) % data_type                     = odb_real
ElemDesp2ODBDesp(IndexShipDirec) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexPrWx) % TableName                          = 'conv'
ElemDesp2ODBDesp(IndexPrWx) % ColName                            = 'present_weather_type'
ElemDesp2ODBDesp(IndexPrWx) % data_type                          = odb_integer
ElemDesp2ODBDesp(IndexPrWx) % has_missing                        = c_false

ElemDesp2ODBDesp(IndexPrecipPeriod1) % TableName                 = 'conv'
ElemDesp2ODBDesp(IndexPrecipPeriod1) % ColName                   = 'rainfall_period_1'
ElemDesp2ODBDesp(IndexPrecipPeriod1) % data_type                 = odb_integer
ElemDesp2ODBDesp(IndexPrecipPeriod1) % has_missing               = c_false

ElemDesp2ODBDesp(IndexPrecipPeriod2) % TableName                 = 'conv'
ElemDesp2ODBDesp(IndexPrecipPeriod2) % ColName                   = 'rainfall_period_3'
ElemDesp2ODBDesp(IndexPrecipPeriod2) % data_type                 = odb_integer
ElemDesp2ODBDesp(IndexPrecipPeriod2) % has_missing               = c_false

ElemDesp2ODBDesp(IndexPrecipAmount1) % TableName                 = 'conv'
ElemDesp2ODBDesp(IndexPrecipAmount1) % ColName                   = 'rainfall_amount_1'
ElemDesp2ODBDesp(IndexPrecipAmount1) % data_type                 = odb_real
ElemDesp2ODBDesp(IndexPrecipAmount1) % has_missing               = c_false

ElemDesp2ODBDesp(IndexPrecipAmount2) % TableName                 = 'conv'
ElemDesp2ODBDesp(IndexPrecipAmount2) % ColName                   = 'rainfall_amount_3'
ElemDesp2ODBDesp(IndexPrecipAmount2) % data_type                 = odb_real
ElemDesp2ODBDesp(IndexPrecipAmount2) % has_missing               = c_false

IF (ObsGroup == ObsGroupAircraft) THEN
  ElemDesp2ODBDesp(IndexAltitude) % TableName                    = 'hdr'
  ElemDesp2ODBDesp(IndexAltitude) % ColName                      = 'stalt'
  ElemDesp2ODBDesp(IndexAltitude) % data_type                    = odb_real
  ElemDesp2ODBDesp(IndexAltitude) % has_missing                  = c_false
ELSE
  ElemDesp2ODBDesp(IndexZstation) % TableName                    = 'hdr'
  ElemDesp2ODBDesp(IndexZstation) % ColName                      = 'stalt'
  ElemDesp2ODBDesp(IndexZstation) % data_type                    = odb_real
  ElemDesp2ODBDesp(IndexZstation) % has_missing                  = c_false
END IF

ElemDesp2ODBDesp(IndexIce_Degr) % TableName                      = 'conv'
ElemDesp2ODBDesp(IndexIce_Degr) % ColName                        = 'aircraft_ice_degree'
ElemDesp2ODBDesp(IndexIce_Degr) % data_type                      = odb_real
ElemDesp2ODBDesp(IndexIce_Degr) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexTurbulenceDegree) % TableName              = 'conv'
ElemDesp2ODBDesp(IndexTurbulenceDegree) % ColName                = 'aircraft_turbulence'
ElemDesp2ODBDesp(IndexTurbulenceDegree) % data_type              = odb_real
ElemDesp2ODBDesp(IndexTurbulenceDegree) % has_missing            = c_false

ElemDesp2ODBDesp(IndexPESR_SNSR_HGHT) % TableName                = 'conv'
ElemDesp2ODBDesp(IndexPESR_SNSR_HGHT) % ColName                  = 'pressure_sensor_alt'
ElemDesp2ODBDesp(IndexPESR_SNSR_HGHT) % data_type                = odb_real
ElemDesp2ODBDesp(IndexPESR_SNSR_HGHT) % has_missing              = c_false

ElemDesp2ODBDesp(IndexPESR_SNSR_FLAG) % TableName                = 'conv'
ElemDesp2ODBDesp(IndexPESR_SNSR_FLAG) % ColName                  = 'pressure_sensor_flag'
ElemDesp2ODBDesp(IndexPESR_SNSR_FLAG) % data_type                = odb_integer
ElemDesp2ODBDesp(IndexPESR_SNSR_FLAG) % has_missing              = c_false

ElemDesp2ODBDesp(IndexStationReportType) % TableName             = 'conv'
ElemDesp2ODBDesp(IndexStationReportType) % ColName               = 'station_type'
ElemDesp2ODBDesp(IndexStationReportType) % data_type             = odb_integer
ElemDesp2ODBDesp(IndexStationReportType) % has_missing           = c_false

ElemDesp2ODBDesp(IndexHoursSinceLastPos) % TableName             = 'conv'
ElemDesp2ODBDesp(IndexHoursSinceLastPos) % ColName               = 'hours_since_last_position'

ElemDesp2ODBDesp(IndexSeaSurfaceTempRecMethod) % TableName       = 'conv'
ElemDesp2ODBDesp(IndexSeaSurfaceTempRecMethod) % ColName         = 'sst_measurement_method'

ElemDesp2ODBDesp(IndexModelSurfaceType) % TableName              = 'modsurf'
ElemDesp2ODBDesp(IndexModelSurfaceType) % ColName                = 'lsm'
ElemDesp2ODBDesp(IndexModelSurfaceType) % data_type              = odb_integer
ElemDesp2ODBDesp(IndexModelSurfaceType) % has_missing            = c_false

ElemDesp2ODBDesp(IndexModelSurfaceHeight) % TableName            = 'modsurf'
ElemDesp2ODBDesp(IndexModelSurfaceHeight) % ColName              = 'orography'
ElemDesp2ODBDesp(IndexModelSurfaceHeight) % data_type            = odb_real
ElemDesp2ODBDesp(IndexModelSurfaceHeight) % has_missing          = c_false

ElemDesp2ODBDesp(IndexFlightPhase) % TableName                   = 'conv'
ElemDesp2ODBDesp(IndexFlightPhase) % ColName                     = 'flight_phase'
ElemDesp2ODBDesp(IndexFlightPhase) % data_type                   = odb_integer
ElemDesp2ODBDesp(IndexFlightPhase) % has_missing                 = c_false

ElemDesp2ODBDesp(IndexOpenRoadID) % TableName                    = 'conv'
ElemDesp2ODBDesp(IndexOpenRoadID) % ColName                      = 'road_site_identifier'
ElemDesp2ODBDesp(IndexOpenRoadID) % data_type                    = odb_string
ElemDesp2ODBDesp(IndexOpenRoadID) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexSurfaceHeight) % TableName                 = 'radiance'
ElemDesp2ODBDesp(IndexSurfaceHeight) % ColName                   = 'surface_height'
ElemDesp2ODBDesp(IndexSurfaceHeight) % data_type                 = odb_real
ElemDesp2ODBDesp(IndexSurfaceHeight) % has_missing               = c_false

ElemDesp2ODBDesp(IndexEMSY_PMTR) % TableName                     = 'radiance'
ElemDesp2ODBDesp(IndexEMSY_PMTR) % ColName                       = 'emissivity_param'
ElemDesp2ODBDesp(IndexEMSY_PMTR) % colname_array_length          = 15
ElemDesp2ODBDesp(IndexEMSY_PMTR) % data_type                     = odb_real
ElemDesp2ODBDesp(IndexEMSY_PMTR) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexLandSeaInd) % TableName                    = 'sat'
ElemDesp2ODBDesp(IndexLandSeaInd) % ColName                      = 'land_sea'
ElemDesp2ODBDesp(IndexLandSeaInd) % data_type                    = odb_integer
ElemDesp2ODBDesp(IndexLandSeaInd) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexSrfcFlag) % TableName                      = 'ssmis'
ElemDesp2ODBDesp(IndexSrfcFlag) % ColName                        = 'surface_flag'
ElemDesp2ODBDesp(IndexSrfcFlag) % data_type                      = odb_integer
ElemDesp2ODBDesp(IndexSrfcFlag) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexRainFlag) % TableName                      = 'ssmis'
ElemDesp2ODBDesp(IndexRainFlag) % ColName                        = 'rain_flag'
ElemDesp2ODBDesp(IndexRainFlag) % data_type                      = odb_integer
ElemDesp2ODBDesp(IndexRainFlag) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexBasePontLttd) % TableName                  = 'ssmis'
ElemDesp2ODBDesp(IndexBasePontLttd) % ColName                    = 'base_point_lat'
ElemDesp2ODBDesp(IndexBasePontLttd) % colname_array_length       = 28
ElemDesp2ODBDesp(IndexBasePontLttd) % data_type                  = odb_real
ElemDesp2ODBDesp(IndexBasePontLttd) % has_missing                = c_false

ElemDesp2ODBDesp(IndexBasePontLngd) % TableName                  = 'ssmis'
ElemDesp2ODBDesp(IndexBasePontLngd) % ColName                    = 'base_point_lon'
ElemDesp2ODBDesp(IndexBasePontLngd) % colname_array_length       = 28
ElemDesp2ODBDesp(IndexBasePontLngd) % data_type                  = odb_real
ElemDesp2ODBDesp(IndexBasePontLngd) % has_missing                = c_false

ElemDesp2ODBDesp(IndexBasePontIncdAngle) % TableName             = 'ssmis'
ElemDesp2ODBDesp(IndexBasePontIncdAngle) % ColName               = 'base_point_inc'
ElemDesp2ODBDesp(IndexBasePontIncdAngle) % colname_array_length  = 28
ElemDesp2ODBDesp(IndexBasePontIncdAngle) % data_type             = odb_real
ElemDesp2ODBDesp(IndexBasePontIncdAngle) % has_missing           = c_false

ElemDesp2ODBDesp(IndexBasePontAzmhAngle) % TableName             = 'ssmis'
ElemDesp2ODBDesp(IndexBasePontAzmhAngle) % ColName               = 'base_point_azi'
ElemDesp2ODBDesp(IndexBasePontAzmhAngle) % colname_array_length  = 28
ElemDesp2ODBDesp(IndexBasePontAzmhAngle) % data_type             = odb_real
ElemDesp2ODBDesp(IndexBasePontAzmhAngle) % has_missing           = c_false

ElemDesp2ODBDesp(IndexQCscan) % TableName                        = 'radiance'
ElemDesp2ODBDesp(IndexQCscan) % ColName                          = 'scanline_status'
ElemDesp2ODBDesp(IndexQCscan) % data_type                        = odb_integer
ElemDesp2ODBDesp(IndexQCscan) % has_missing                      = c_false

ElemDesp2ODBDesp(IndexScanLine) % TableName                      = 'radiance'
ElemDesp2ODBDesp(IndexScanLine) % ColName                        = 'scanline'
ElemDesp2ODBDesp(IndexScanLine) % data_type                      = odb_integer
ElemDesp2ODBDesp(IndexScanLine) % has_missing                    = c_false

IF (ObsGroup == ObsGroupCrIS .or. ObsGroup == ObsGroupHIRAS .or. &
    ObsGroup == ObsGroupGIIRSLW .or. ObsGroup == ObsGroupGIIRSMW) THEN
  ElemDesp2ODBDesp(IndexPixelNumber) % TableName                 = 'radiance'
  ElemDesp2ODBDesp(IndexPixelNumber) % ColName                   = 'field_of_regard'
  ElemDesp2ODBDesp(IndexPixelNumber) % data_type                 = odb_real
  ElemDesp2ODBDesp(IndexPixelNumber) % has_missing               = c_false

  ElemDesp2ODBDesp(IndexScanPosition) % TableName                = 'radiance'
  ElemDesp2ODBDesp(IndexScanPosition) % ColName                  = 'scanpos'
  ElemDesp2ODBDesp(IndexScanPosition) % data_type                = odb_real
  ElemDesp2ODBDesp(IndexScanPosition) % has_missing              = c_false
ELSE
  ElemDesp2ODBDesp(IndexScanPosition) % TableName                = 'radiance'
  ElemDesp2ODBDesp(IndexScanPosition) % ColName                  = 'scanpos'
  ElemDesp2ODBDesp(IndexScanPosition) % data_type                = odb_real
  ElemDesp2ODBDesp(IndexScanPosition) % has_missing              = c_false
END IF

ElemDesp2ODBDesp(IndexQCGIIRSfov) % TableName                    = 'radiance'
ElemDesp2ODBDesp(IndexQCGIIRSfov) % ColName                      = 'qcgiirsfov'
ElemDesp2ODBDesp(IndexQCGIIRSfov) % data_type                    = odb_integer
ElemDesp2ODBDesp(IndexQCGIIRSfov) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexSatZenithAngle) % TableName                = 'sat'
ElemDesp2ODBDesp(IndexSatZenithAngle) % ColName                  = 'zenith'
ElemDesp2ODBDesp(IndexSatZenithAngle) % data_type                = odb_real
ElemDesp2ODBDesp(IndexSatZenithAngle) % has_missing              = c_false

ElemDesp2ODBDesp(IndexAMSUZenithAngle) % TableName               = 'sat'
ElemDesp2ODBDesp(IndexAMSUZenithAngle) % ColName                 = 'amsua_zenith'
ElemDesp2ODBDesp(IndexAMSUZenithAngle) % data_type               = odb_real
ElemDesp2ODBDesp(IndexAMSUZenithAngle) % has_missing             = c_false

ElemDesp2ODBDesp(IndexSatAzimth) % TableName                     = 'sat'
ElemDesp2ODBDesp(IndexSatAzimth) % ColName                       = 'azimuth'
ElemDesp2ODBDesp(IndexSatAzimth) % data_type                     = odb_real
ElemDesp2ODBDesp(IndexSatAzimth) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexSolZen) % TableName                        = 'hdr'
ElemDesp2ODBDesp(IndexSolZen) % ColName                          = 'solar_zenith'
ElemDesp2ODBDesp(IndexSolZen) % data_type                        = odb_real
ElemDesp2ODBDesp(IndexSolZen) % has_missing                      = c_false

ElemDesp2ODBDesp(IndexSolAzimth) % TableName                     = 'sat'
ElemDesp2ODBDesp(IndexSolAzimth) % ColName                       = 'solar_azimuth'
ElemDesp2ODBDesp(IndexSolAzimth) % data_type                     = odb_real
ElemDesp2ODBDesp(IndexSolAzimth) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexCloudTopPressure) % TableName              = 'radiance'
ElemDesp2ODBDesp(IndexCloudTopPressure) % ColName                = 'cldptop'
ElemDesp2ODBDesp(IndexCloudTopPressure) % colname_array_length   = 3
ElemDesp2ODBDesp(IndexCloudTopPressure) % data_type              = odb_real
ElemDesp2ODBDesp(IndexCloudTopPressure) % has_missing            = c_false

ElemDesp2ODBDesp(IndexVnATOVPP) % TableName                      = 'radiance'
ElemDesp2ODBDesp(IndexVnATOVPP) % ColName                        = 'vnatovspp'
ElemDesp2ODBDesp(IndexVnATOVPP) % data_type                      = odb_integer
ElemDesp2ODBDesp(IndexVnATOVPP) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexQCprepro) % TableName                      = 'radiance'
ElemDesp2ODBDesp(IndexQCprepro) % ColName                        = 'qcprepro'
ElemDesp2ODBDesp(IndexQCprepro) % data_type                      = odb_integer
ElemDesp2ODBDesp(IndexQCprepro) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexQCIASIGQis) % TableName                    = 'radiance'
ElemDesp2ODBDesp(IndexQCIASIGQis) % ColName                      = 'general_quality_flag'
ElemDesp2ODBDesp(IndexQCIASIGQis) % data_type                    = odb_integer
ElemDesp2ODBDesp(IndexQCIASIGQis) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexAMSUscatindx) % TableName                  = 'radiance'
ElemDesp2ODBDesp(IndexAMSUscatindx) % ColName                    = 'amsuscatindex'
ElemDesp2ODBDesp(IndexAMSUscatindx) % data_type                  = odb_integer
ElemDesp2ODBDesp(IndexAMSUscatindx) % has_missing                = c_false

ElemDesp2ODBDesp(IndexAAPPCirrus) % TableName                    = 'radiance'
ElemDesp2ODBDesp(IndexAAPPCirrus) % ColName                      = 'aapp_cirrus_index'
ElemDesp2ODBDesp(IndexAAPPCirrus) % data_type                    = odb_real
ElemDesp2ODBDesp(IndexAAPPCirrus) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexAAPPBennartz) % TableName                  = 'radiance'
ElemDesp2ODBDesp(IndexAAPPBennartz) % ColName                    = 'aapp_bennartz_index'
ElemDesp2ODBDesp(IndexAAPPBennartz) % data_type                  = odb_real
ElemDesp2ODBDesp(IndexAAPPBennartz) % has_missing                = c_false

ElemDesp2ODBDesp(IndexQCHIRSfov) % TableName                     = 'radiance'
ElemDesp2ODBDesp(IndexQCHIRSfov) % ColName                       = 'qchirsfov'
ElemDesp2ODBDesp(IndexQCHIRSfov) % data_type                     = odb_integer
ElemDesp2ODBDesp(IndexQCHIRSfov) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexQCIASIFov) % TableName                     = 'radiance'
ElemDesp2ODBDesp(IndexQCIASIFov) % ColName                       = 'qchirsfov'
ElemDesp2ODBDesp(IndexQCIASIFov) % data_type                     = odb_integer
ElemDesp2ODBDesp(IndexQCIASIFov) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexIASIScaleStart) % TableName                = 'radiance'
ElemDesp2ODBDesp(IndexIASIScaleStart) % ColName                  = 'start_chan_scale'
ElemDesp2ODBDesp(IndexIASIScaleStart) % colname_array_length     = 10
ElemDesp2ODBDesp(IndexIASIScaleStart) % data_type                = odb_integer
ElemDesp2ODBDesp(IndexIASIScaleStart) % has_missing              = c_false

ElemDesp2ODBDesp(IndexIASIScaleEnd) % TableName                  = 'radiance'
ElemDesp2ODBDesp(IndexIASIScaleEnd) % ColName                    = 'end_chan_scale'
ElemDesp2ODBDesp(IndexIASIScaleEnd) % colname_array_length       = 10
ElemDesp2ODBDesp(IndexIASIScaleEnd) % data_type                  = odb_integer
ElemDesp2ODBDesp(IndexIASIScaleEnd) % has_missing                = c_false

ElemDesp2ODBDesp(IndexIASIScaleFactor) % TableName               = 'radiance'
ElemDesp2ODBDesp(IndexIASIScaleFactor) % ColName                 = 'chan_scale_factor'
ElemDesp2ODBDesp(IndexIASIScaleFactor) % colname_array_length    = 10
ElemDesp2ODBDesp(IndexIASIScaleFactor) % data_type               = odb_real
ElemDesp2ODBDesp(IndexIASIScaleFactor) % has_missing             = c_false

ElemDesp2ODBDesp(IndexChannelVersion) % TableName                = 'radiance'
ElemDesp2ODBDesp(IndexChannelVersion) % ColName                  = 'chan_sel_version'
ElemDesp2ODBDesp(IndexChannelVersion) % data_type                = odb_integer
ElemDesp2ODBDesp(IndexChannelVersion) % has_missing              = c_false

ElemDesp2ODBDesp(IndexIASIPCVersion) % TableName                 = 'radiance'
ElemDesp2ODBDesp(IndexIASIPCVersion) % ColName                   = 'princ_comp_ver'
ElemDesp2ODBDesp(IndexIASIPCVersion) % data_type                 = odb_integer
ElemDesp2ODBDesp(IndexIASIPCVersion) % has_missing               = c_false

ElemDesp2ODBDesp(IndexQCIASIPC) % TableName                      = 'radiance'
ElemDesp2ODBDesp(IndexQCIASIPC) % ColName                        = 'princ_comp_qc'
ElemDesp2ODBDesp(IndexQCIASIPC) % colname_array_length           = 3
ElemDesp2ODBDesp(IndexQCIASIPC) % data_type                      = odb_integer
ElemDesp2ODBDesp(IndexQCIASIPC) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexSatCloudTests) % TableName                 = 'radiance'
ElemDesp2ODBDesp(IndexSatCloudTests) % ColName                   = 'meto_cloud_tests'
ElemDesp2ODBDesp(IndexSatCloudTests) % data_type                 = odb_integer
ElemDesp2ODBDesp(IndexSatCloudTests) % has_missing               = c_false

ElemDesp2ODBDesp(IndexSatCloudFlags) % TableName                 = 'radiance'
ElemDesp2ODBDesp(IndexSatCloudFlags) % ColName                   = 'meto_cloud_flags'
ElemDesp2ODBDesp(IndexSatCloudFlags) % data_type                 = odb_integer
ElemDesp2ODBDesp(IndexSatCloudFlags) % has_missing               = c_false

ElemDesp2ODBDesp(IndexHIRS_Temp) % TableName                     = 'radiance'
ElemDesp2ODBDesp(IndexHIRS_Temp) % ColName                       = 'hirs_temp'
ElemDesp2ODBDesp(IndexHIRS_Temp) % data_type                     = odb_real
ElemDesp2ODBDesp(IndexHIRS_Temp) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexAMSUA1_Temp) % TableName                   = 'radiance'
ElemDesp2ODBDesp(IndexAMSUA1_Temp) % ColName                     = 'amsua1_temp'
ElemDesp2ODBDesp(IndexAMSUA1_Temp) % data_type                   = odb_real
ElemDesp2ODBDesp(IndexAMSUA1_Temp) % has_missing                 = c_false

ElemDesp2ODBDesp(IndexAMSUA2_Temp) % TableName                   = 'radiance'
ElemDesp2ODBDesp(IndexAMSUA2_Temp) % ColName                     = 'amsua2_temp'
ElemDesp2ODBDesp(IndexAMSUA2_Temp) % data_type                   = odb_real
ElemDesp2ODBDesp(IndexAMSUA2_Temp) % has_missing                 = c_false

ElemDesp2ODBDesp(IndexAMSUB_Temp) % TableName                    = 'radiance'
ElemDesp2ODBDesp(IndexAMSUB_Temp) % ColName                      = 'amsub_temp'
ElemDesp2ODBDesp(IndexAMSUB_Temp) % data_type                    = odb_real
ElemDesp2ODBDesp(IndexAMSUB_Temp) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexCrossTrackCell) % TableName                = 'scatt'
ElemDesp2ODBDesp(IndexCrossTrackCell) % ColName                  = 'cellno'
ElemDesp2ODBDesp(IndexCrossTrackCell) % data_type                = odb_integer
ElemDesp2ODBDesp(IndexCrossTrackCell) % has_missing              = c_false

ElemDesp2ODBDesp(IndexSeaWindsQCFlag) % TableName                = 'scatt'
ElemDesp2ODBDesp(IndexSeaWindsQCFlag) % ColName                  = 'wvc_qf'
ElemDesp2ODBDesp(IndexSeaWindsQCFlag) % data_type                = odb_integer
ElemDesp2ODBDesp(IndexSeaWindsQCFlag) % has_missing              = c_false

ElemDesp2ODBDesp(IndexASCATQCFlag) % TableName                   = 'scatt'
ElemDesp2ODBDesp(IndexASCATQCFlag) % ColName                     = 'wvc_qf'
ElemDesp2ODBDesp(IndexASCATQCFlag) % data_type                   = odb_integer
ElemDesp2ODBDesp(IndexASCATQCFlag) % has_missing                 = c_false

ElemDesp2ODBDesp(IndexASCATNumberVectorAmby) % TableName         = 'scatt'
ElemDesp2ODBDesp(IndexASCATNumberVectorAmby) % ColName           = 'nretr_amb'
ElemDesp2ODBDesp(IndexASCATNumberVectorAmby) % data_type         = odb_integer
ElemDesp2ODBDesp(IndexASCATNumberVectorAmby) % has_missing       = c_false

ElemDesp2ODBDesp(IndexASCATWindRepCount) % TableName             = 'scatt'
ElemDesp2ODBDesp(IndexASCATWindRepCount) % ColName               = 'max_amb'
ElemDesp2ODBDesp(IndexASCATWindRepCount) % data_type             = odb_integer
ElemDesp2ODBDesp(IndexASCATWindRepCount) % has_missing           = c_false

ElemDesp2ODBDesp(IndexASCATBackDstc) % TableName                 = 'scatt'
ElemDesp2ODBDesp(IndexASCATBackDstc) % ColName                   = 'invresid'
ElemDesp2ODBDesp(IndexASCATBackDstc) % colname_array_length      = 4
ElemDesp2ODBDesp(IndexASCATBackDstc) % data_type                 = odb_real
ElemDesp2ODBDesp(IndexASCATBackDstc) % has_missing               = c_false

ElemDesp2ODBDesp(IndexDistCone) % TableName                      = 'scatt'
ElemDesp2ODBDesp(IndexDistCone) % ColName                        = 'invresid'
ElemDesp2ODBDesp(IndexDistCone) % colname_array_length           = 4
ElemDesp2ODBDesp(IndexDistCone) % data_type                      = odb_real
ElemDesp2ODBDesp(IndexDistCone) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexASCATSolnLkhd) % TableName                 = 'scatt'
ElemDesp2ODBDesp(IndexASCATSolnLkhd) % ColName                   = 'solution_liklihood'
ElemDesp2ODBDesp(IndexASCATSolnLkhd) % colname_array_length      = 4
ElemDesp2ODBDesp(IndexASCATSolnLkhd) % data_type                 = odb_real
ElemDesp2ODBDesp(IndexASCATSolnLkhd) % has_missing               = c_false

ElemDesp2ODBDesp(IndexKuSolnLkhd) % TableName                    = 'scatt'
ElemDesp2ODBDesp(IndexKuSolnLkhd) % ColName                      = 'solution_liklihood'
ElemDesp2ODBDesp(IndexKuSolnLkhd) % colname_array_length         = 4
ElemDesp2ODBDesp(IndexKuSolnLkhd) % data_type                    = odb_real
ElemDesp2ODBDesp(IndexKuSolnLkhd) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexWINDSATCHI2WindRetrieval) % TableName            = 'scatt'
ElemDesp2ODBDesp(IndexWINDSATCHI2WindRetrieval) % ColName              = 'chi_2_probability'
ElemDesp2ODBDesp(IndexWINDSATCHI2WindRetrieval) % colname_array_length = 4
ElemDesp2ODBDesp(IndexWINDSATCHI2WindRetrieval) % data_type            = odb_real
ElemDesp2ODBDesp(IndexWINDSATCHI2WindRetrieval) % has_missing          = c_false

ElemDesp2ODBDesp(IndexChosenWind) % TableName                    = 'scatt'
ElemDesp2ODBDesp(IndexChosenWind) % ColName                      = 'sel_wnd_indx'
ElemDesp2ODBDesp(IndexChosenWind) % data_type                    = odb_integer
ElemDesp2ODBDesp(IndexChosenWind) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexRainProb) % TableName                      = 'scatt'
ElemDesp2ODBDesp(IndexRainProb) % ColName                        = 'prcp_prob'
ElemDesp2ODBDesp(IndexRainProb) % data_type                      = odb_real
ElemDesp2ODBDesp(IndexRainProb) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexNumBeams) % TableName                      = 'scatt'
ElemDesp2ODBDesp(IndexNumBeams) % ColName                        = 'num_beams'
ElemDesp2ODBDesp(IndexNumBeams) % data_type                      = odb_integer
ElemDesp2ODBDesp(IndexNumBeams) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexScatIncAngle1) % TableName                 = 'scatt'
ElemDesp2ODBDesp(IndexScatIncAngle1) % ColName                   = 'radr_fore_beam_incd_angl'
ElemDesp2ODBDesp(IndexScatIncAngle1) % data_type                 = odb_real
ElemDesp2ODBDesp(IndexScatIncAngle1) % has_missing               = c_false

ElemDesp2ODBDesp(IndexScatIncAngle2) % TableName                 = 'scatt'
ElemDesp2ODBDesp(IndexScatIncAngle2) % ColName                   = 'radr_mid_beam_incd_angl'
ElemDesp2ODBDesp(IndexScatIncAngle2) % data_type                 = odb_real
ElemDesp2ODBDesp(IndexScatIncAngle2) % has_missing               = c_false

ElemDesp2ODBDesp(IndexScatIncAngle3) % TableName                 = 'scatt'
ElemDesp2ODBDesp(IndexScatIncAngle3) % ColName                   = 'radr_aft_beam_incd_angl'
ElemDesp2ODBDesp(IndexScatIncAngle3) % data_type                 = odb_real
ElemDesp2ODBDesp(IndexScatIncAngle3) % has_missing               = c_false

ElemDesp2ODBDesp(IndexScatLookAngle1) % TableName                = 'scatt'
ElemDesp2ODBDesp(IndexScatLookAngle1) % ColName                  = 'radr_fore_beam_look_angl'
ElemDesp2ODBDesp(IndexScatLookAngle1) % data_type                = odb_real
ElemDesp2ODBDesp(IndexScatLookAngle1) % has_missing              = c_false

ElemDesp2ODBDesp(IndexScatLookAngle2) % TableName                = 'scatt'
ElemDesp2ODBDesp(IndexScatLookAngle2) % ColName                  = 'radr_mid_beam_look_angl'
ElemDesp2ODBDesp(IndexScatLookAngle2) % data_type                = odb_real
ElemDesp2ODBDesp(IndexScatLookAngle2) % has_missing              = c_false

ElemDesp2ODBDesp(IndexScatLookAngle3) % TableName                = 'scatt'
ElemDesp2ODBDesp(IndexScatLookAngle3) % ColName                  = 'radr_aft_beam_look_angl'
ElemDesp2ODBDesp(IndexScatLookAngle3) % data_type                = odb_real
ElemDesp2ODBDesp(IndexScatLookAngle3) % has_missing              = c_false

ElemDesp2ODBDesp(IndexScatBackScatter1) % TableName              = 'scatt'
ElemDesp2ODBDesp(IndexScatBackScatter1) % ColName                = 'radr_fore_beam_back_sctr'
ElemDesp2ODBDesp(IndexScatBackScatter1) % data_type              = odb_real
ElemDesp2ODBDesp(IndexScatBackScatter1) % has_missing            = c_false

ElemDesp2ODBDesp(IndexScatBackScatter2) % TableName              = 'scatt'
ElemDesp2ODBDesp(IndexScatBackScatter2) % ColName                = 'radr_mid_beam_back_sctr'
ElemDesp2ODBDesp(IndexScatBackScatter2) % data_type              = odb_real
ElemDesp2ODBDesp(IndexScatBackScatter2) % has_missing            = c_false

ElemDesp2ODBDesp(IndexScatBackScatter3) % TableName              = 'scatt'
ElemDesp2ODBDesp(IndexScatBackScatter3) % ColName                = 'radr_aft_beam_back_sctr'
ElemDesp2ODBDesp(IndexScatBackScatter3) % data_type              = odb_real
ElemDesp2ODBDesp(IndexScatBackScatter3) % has_missing            = c_false

ElemDesp2ODBDesp(IndexScatBeamNoise1) % TableName                = 'scatt'
ElemDesp2ODBDesp(IndexScatBeamNoise1) % ColName                  = 'radr_fore_beam_nois'
ElemDesp2ODBDesp(IndexScatBeamNoise1) % data_type                = odb_real
ElemDesp2ODBDesp(IndexScatBeamNoise1) % has_missing              = c_false

ElemDesp2ODBDesp(IndexScatBeamNoise2) % TableName                = 'scatt'
ElemDesp2ODBDesp(IndexScatBeamNoise2) % ColName                  = 'radr_mid_beam_nois'
ElemDesp2ODBDesp(IndexScatBeamNoise2) % data_type                = odb_real
ElemDesp2ODBDesp(IndexScatBeamNoise2) % has_missing              = c_false

ElemDesp2ODBDesp(IndexScatBeamNoise3) % TableName                = 'scatt'
ElemDesp2ODBDesp(IndexScatBeamNoise3) % ColName                  = 'radr_aft_beam_nois'
ElemDesp2ODBDesp(IndexScatBeamNoise3) % data_type                = odb_real
ElemDesp2ODBDesp(IndexScatBeamNoise3) % has_missing              = c_false

ElemDesp2ODBDesp(IndexScatMissShot1) % TableName                 = 'scatt'
ElemDesp2ODBDesp(IndexScatMissShot1) % ColName                   = 'radr_fore_beam_miss_pckt_cont'
ElemDesp2ODBDesp(IndexScatMissShot1) % data_type                 = odb_real
ElemDesp2ODBDesp(IndexScatMissShot1) % has_missing               = c_false

ElemDesp2ODBDesp(IndexScatMissShot2) % TableName                 = 'scatt'
ElemDesp2ODBDesp(IndexScatMissShot2) % ColName                   = 'radr_mid_beam_miss_pckt_cont'
ElemDesp2ODBDesp(IndexScatMissShot2) % data_type                 = odb_real
ElemDesp2ODBDesp(IndexScatMissShot2) % has_missing               = c_false

ElemDesp2ODBDesp(IndexScatMissShot3) % TableName                 = 'scatt'
ElemDesp2ODBDesp(IndexScatMissShot3) % ColName                   = 'radr_aft_beam_miss_pckt_cont'
ElemDesp2ODBDesp(IndexScatMissShot3) % data_type                 = odb_real
ElemDesp2ODBDesp(IndexScatMissShot3) % has_missing               = c_false

ElemDesp2ODBDesp(IndexEsaProdConf) % TableName                   = 'scatt'
ElemDesp2ODBDesp(IndexEsaProdConf) % ColName                     = 'esa_prdt_conf'
ElemDesp2ODBDesp(IndexEsaProdConf) % data_type                   = odb_real
ElemDesp2ODBDesp(IndexEsaProdConf) % has_missing                 = c_false

ElemDesp2ODBDesp(IndexWINDSATWindDirectionError) % TableName     = 'scatt'
ElemDesp2ODBDesp(IndexWINDSATWindDirectionError) % ColName       = 'wind_dir_est_error'
ElemDesp2ODBDesp(IndexWINDSATWindDirectionError) % data_type     = odb_real
ElemDesp2ODBDesp(IndexWINDSATWindDirectionError) % has_missing   = c_false

ElemDesp2ODBDesp(IndexASCATL2WindSoftwareID) % TableName         = 'sat'
ElemDesp2ODBDesp(IndexASCATL2WindSoftwareID) % ColName           = 'software_identifier'
ElemDesp2ODBDesp(IndexASCATL2WindSoftwareID) % data_type         = odb_integer
ElemDesp2ODBDesp(IndexASCATL2WindSoftwareID) % has_missing       = c_false

ElemDesp2ODBDesp(IndexASCATIceProb) % TableName                  = 'scatt'
ElemDesp2ODBDesp(IndexASCATIceProb) % ColName                    = 'ice_prob'
ElemDesp2ODBDesp(IndexASCATIceProb) % data_type                  = odb_real
ElemDesp2ODBDesp(IndexASCATIceProb) % has_missing                = c_false

ElemDesp2ODBDesp(IndexASCATIceAgeA) % TableName                  = 'scatt'
ElemDesp2ODBDesp(IndexASCATIceAgeA) % ColName                    = 'ice_age'
ElemDesp2ODBDesp(IndexASCATIceAgeA) % data_type                  = odb_real
ElemDesp2ODBDesp(IndexASCATIceAgeA) % has_missing                = c_false

ElemDesp2ODBDesp(IndexSatDirectionOfMotion) % TableName          = 'scatt'
ElemDesp2ODBDesp(IndexSatDirectionOfMotion) % ColName            = 'sat_direction'
ElemDesp2ODBDesp(IndexSatDirectionOfMotion) % data_type          = odb_real
ElemDesp2ODBDesp(IndexSatDirectionOfMotion) % has_missing        = c_false

ElemDesp2ODBDesp(IndexWINDSATSurfType) % TableName               = 'modsurf'
ElemDesp2ODBDesp(IndexWINDSATSurfType) % ColName                 = 'surface_class'
ElemDesp2ODBDesp(IndexWINDSATSurfType) % data_type               = odb_integer
ElemDesp2ODBDesp(IndexWINDSATSurfType) % has_missing             = c_false

ElemDesp2ODBDesp(IndexWINDSATWaterVapourContent) % TableName     = 'scatt'
ElemDesp2ODBDesp(IndexWINDSATWaterVapourContent) % ColName       = 'water_vapour'
ElemDesp2ODBDesp(IndexWINDSATWaterVapourContent) % data_type     = odb_real
ElemDesp2ODBDesp(IndexWINDSATWaterVapourContent) % has_missing   = c_false

ElemDesp2ODBDesp(IndexWINDSATLiquidWaterContent) % TableName     = 'scatt'
ElemDesp2ODBDesp(IndexWINDSATLiquidWaterContent) % ColName       = 'liquid_water'
ElemDesp2ODBDesp(IndexWINDSATLiquidWaterContent) % data_type     = odb_real
ElemDesp2ODBDesp(IndexWINDSATLiquidWaterContent) % has_missing   = c_false

ElemDesp2ODBDesp(IndexWINDSATPrecipRate) % TableName             = 'scatt'
ElemDesp2ODBDesp(IndexWINDSATPrecipRate) % ColName               = 'precip_rate'
ElemDesp2ODBDesp(IndexWINDSATPrecipRate) % data_type             = odb_real
ElemDesp2ODBDesp(IndexWINDSATPrecipRate) % has_missing           = c_false

ElemDesp2ODBDesp(IndexWINDSATWindSpeedError) % TableName         = 'scatt'
ElemDesp2ODBDesp(IndexWINDSATWindSpeedError) % ColName           = 'wind_speed_error'
ElemDesp2ODBDesp(IndexWINDSATWindSpeedError) % data_type         = odb_real
ElemDesp2ODBDesp(IndexWINDSATWindSpeedError) % has_missing       = c_false

ElemDesp2ODBDesp(IndexWINDSATWaterVapourError) % TableName       = 'scatt'
ElemDesp2ODBDesp(IndexWINDSATWaterVapourError) % ColName         = 'water_vapour_error'
ElemDesp2ODBDesp(IndexWINDSATWaterVapourError) % data_type       = odb_real
ElemDesp2ODBDesp(IndexWINDSATWaterVapourError) % has_missing     = c_false

ElemDesp2ODBDesp(IndexWINDSATLiquidWaterError) % TableName       = 'scatt'
ElemDesp2ODBDesp(IndexWINDSATLiquidWaterError) % ColName         = 'liquid_water_error'
ElemDesp2ODBDesp(IndexWINDSATLiquidWaterError) % data_type       = odb_real
ElemDesp2ODBDesp(IndexWINDSATLiquidWaterError) % has_missing     = c_false

ElemDesp2ODBDesp(IndexWINDSATQCflagA) % TableName                = 'scatt'
ElemDesp2ODBDesp(IndexWINDSATQCflagA) % ColName                  = 'windsat_qc_flag_a'
ElemDesp2ODBDesp(IndexWINDSATQCflagA) % data_type                = odb_integer
ElemDesp2ODBDesp(IndexWINDSATQCflagA) % has_missing              = c_false

ElemDesp2ODBDesp(IndexWINDSATQCflagB) % TableName                = 'scatt'
ElemDesp2ODBDesp(IndexWINDSATQCflagB) % ColName                  = 'windsat_qc_flag_b'
ElemDesp2ODBDesp(IndexWINDSATQCflagB) % data_type                = odb_integer
ElemDesp2ODBDesp(IndexWINDSATQCflagB) % has_missing              = c_false

IF (ObsGroup /= ObsGroupATMS) THEN
  ElemDesp2ODBDesp(IndexOrigCtr) % TableName                     = 'sat'
  ElemDesp2ODBDesp(IndexOrigCtr) % ColName                       = 'gen_centre'
  ElemDesp2ODBDesp(IndexOrigCtr) % data_type                     = odb_integer
  ElemDesp2ODBDesp(IndexOrigCtr) % has_missing                   = c_false
ELSE
  ElemDesp2ODBDesp(IndexOrigCtr) % TableName                     = 'sat'
  ElemDesp2ODBDesp(IndexOrigCtr) % ColName                       = 'level_1b_creation_subcentre'
  ElemDesp2ODBDesp(IndexOrigCtr) % data_type                     = odb_integer
  ElemDesp2ODBDesp(IndexOrigCtr) % has_missing                   = c_false
END IF

ElemDesp2ODBDesp(IndexCloudMotionMethod) % TableName             = 'satob'
ElemDesp2ODBDesp(IndexCloudMotionMethod) % ColName               = 'comp_method'
ElemDesp2ODBDesp(IndexCloudMotionMethod) % data_type             = odb_integer
ElemDesp2ODBDesp(IndexCloudMotionMethod) % has_missing           = c_false

ElemDesp2ODBDesp(IndexSatobSectionNumber) % TableName            = 'satob'
ElemDesp2ODBDesp(IndexSatobSectionNumber) % ColName              = 'section_num'
ElemDesp2ODBDesp(IndexSatobSectionNumber) % data_type            = odb_integer
ElemDesp2ODBDesp(IndexSatobSectionNumber) % has_missing          = c_false

ElemDesp2ODBDesp(IndexOrgngSubCntr) % TableName                  = 'sat'
ElemDesp2ODBDesp(IndexOrgngSubCntr) % ColName                    = 'gen_subcentre'
ElemDesp2ODBDesp(IndexOrgngSubCntr) % data_type                  = odb_integer
ElemDesp2ODBDesp(IndexOrgngSubCntr) % has_missing                = c_false

ElemDesp2ODBDesp(IndexHeightAssMethod) % TableName               = 'satob'
ElemDesp2ODBDesp(IndexHeightAssMethod) % ColName                 = 'height_assignment_method'
ElemDesp2ODBDesp(IndexHeightAssMethod) % data_type               = odb_integer
ElemDesp2ODBDesp(IndexHeightAssMethod) % has_missing             = c_false

ElemDesp2ODBDesp(IndexChanCtralFreq) % TableName                 = 'satob'
ElemDesp2ODBDesp(IndexChanCtralFreq) % ColName                   = 'chan_freq'
ElemDesp2ODBDesp(IndexChanCtralFreq) % data_type                 = odb_real
ElemDesp2ODBDesp(IndexChanCtralFreq) % has_missing               = c_false

ElemDesp2ODBDesp(IndexNumberOfAIRSChannels) % TableName          = 'hdr'
ElemDesp2ODBDesp(IndexNumberOfAIRSChannels) % ColName            = 'numlev'
ElemDesp2ODBDesp(IndexNumberOfAIRSChannels) % data_type          = odb_integer
ElemDesp2ODBDesp(IndexNumberOfAIRSChannels) % has_missing        = c_false

ElemDesp2ODBDesp(IndexWATR_FRCN) % TableName                     = 'radiance'
ElemDesp2ODBDesp(IndexWATR_FRCN) % ColName                       = 'water_fraction'
ElemDesp2ODBDesp(IndexWATR_FRCN) % data_type                     = odb_real
ElemDesp2ODBDesp(IndexWATR_FRCN) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexMAIN_LAND_TYPE) % TableName                = 'radiance'
ElemDesp2ODBDesp(IndexMAIN_LAND_TYPE) % ColName                  = 'surface_land_indicator'
ElemDesp2ODBDesp(IndexMAIN_LAND_TYPE) % data_type                = odb_integer
ElemDesp2ODBDesp(IndexMAIN_LAND_TYPE) % has_missing              = c_false

ElemDesp2ODBDesp(IndexMAIN_LAND_TYPE_FRCN) % TableName           = 'radiance'
ElemDesp2ODBDesp(IndexMAIN_LAND_TYPE_FRCN) % ColName             = 'surface_land_fraction'
ElemDesp2ODBDesp(IndexMAIN_LAND_TYPE_FRCN) % data_type           = odb_real
ElemDesp2ODBDesp(IndexMAIN_LAND_TYPE_FRCN) % has_missing         = c_false

IF (ObsGroup == ObsGroupOcean) THEN
  ElemDesp2ODBDesp(IndexBathyTempInst) % TableName               = 'hdr'
  ElemDesp2ODBDesp(IndexBathyTempInst) % ColName                 = 'instrument_type'
  ElemDesp2ODBDesp(IndexBathyTempInst) % data_type               = odb_integer
  ElemDesp2ODBDesp(IndexBathyTempInst) % has_missing             = c_false

  ElemDesp2ODBDesp(IndexSeaTempInst) % TableName                 = 'hdr'
  ElemDesp2ODBDesp(IndexSeaTempInst) % ColName                   = 'instrument_type'
  ElemDesp2ODBDesp(IndexSeaTempInst) % data_type                 = odb_integer
  ElemDesp2ODBDesp(IndexSeaTempInst) % has_missing               = c_false
END IF

ElemDesp2ODBDesp(IndexEphemLat) % TableName                      = 'radiance'
ElemDesp2ODBDesp(IndexEphemLat) % ColName                        = 'ephemeris_latitude'
ElemDesp2ODBDesp(IndexEphemLat) % colname_array_length           = 3
ElemDesp2ODBDesp(IndexEphemLat) % data_type                      = odb_real
ElemDesp2ODBDesp(IndexEphemLat) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexEphemLon) % TableName                      = 'radiance'
ElemDesp2ODBDesp(IndexEphemLon) % ColName                        = 'ephemeris_longitude'
ElemDesp2ODBDesp(IndexEphemLon) % colname_array_length           = 3
ElemDesp2ODBDesp(IndexEphemLon) % data_type                      = odb_real
ElemDesp2ODBDesp(IndexEphemLon) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexEphemHeight) % TableName                   = 'radiance'
ElemDesp2ODBDesp(IndexEphemHeight) % ColName                     = 'ephemeris_height'
ElemDesp2ODBDesp(IndexEphemHeight) % colname_array_length        = 3
ElemDesp2ODBDesp(IndexEphemHeight) % data_type                   = odb_real
ElemDesp2ODBDesp(IndexEphemHeight) % has_missing                 = c_false

ElemDesp2ODBDesp(IndexSeaDepth) % TableName                      = 'hdr'
ElemDesp2ODBDesp(IndexSeaDepth) % ColName                        = 'sea_depth'
ElemDesp2ODBDesp(IndexSeaDepth) % data_type                      = odb_real
ElemDesp2ODBDesp(IndexSeaDepth) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexSatInst) % TableName                       = 'sat'
ElemDesp2ODBDesp(IndexSatInst) % ColName                         = 'satellite_instrument'
ElemDesp2ODBDesp(IndexSatInst) % data_type                       = odb_integer
ElemDesp2ODBDesp(IndexSatInst) % has_missing                     = c_false

ElemDesp2ODBDesp(IndexAUTOSATSpatialTest) % TableName            = 'geocloud'
ElemDesp2ODBDesp(IndexAUTOSATSpatialTest) % ColName              = 'autosat_spatial_test'
ElemDesp2ODBDesp(IndexAUTOSATSpatialTest) % data_type            = odb_integer
ElemDesp2ODBDesp(IndexAUTOSATSpatialTest) % has_missing          = c_false

ElemDesp2ODBDesp(IndexRcptStn) % TableName                       = 'hdr'
ElemDesp2ODBDesp(IndexRcptStn) % ColName                         = 'instrument_type'
ElemDesp2ODBDesp(IndexRcptStn) % data_type                       = odb_integer
ElemDesp2ODBDesp(IndexRcptStn) % has_missing                     = c_false

ElemDesp2ODBDesp(IndexStdvClearAVHRR4) % TableName               = 'radiance'
ElemDesp2ODBDesp(IndexStdvClearAVHRR4) % ColName                 = 'avhrr4_sdev_clear_bt'
ElemDesp2ODBDesp(IndexStdvClearAVHRR4) % data_type               = odb_real
ElemDesp2ODBDesp(IndexStdvClearAVHRR4) % has_missing             = c_false

ElemDesp2ODBDesp(IndexAMSUsurface) % TableName                   = 'modsurf'
ElemDesp2ODBDesp(IndexAMSUsurface) % ColName                     = 'surface_class'
ElemDesp2ODBDesp(IndexAMSUsurface) % data_type                   = odb_integer
ElemDesp2ODBDesp(IndexAMSUsurface) % has_missing                 = c_false

ElemDesp2ODBDesp(IndexAMSUcldcost) % TableName                   = 'radiance'
ElemDesp2ODBDesp(IndexAMSUcldcost) % ColName                     = 'surface_cost_fn'
ElemDesp2ODBDesp(IndexAMSUcldcost) % data_type                   = odb_integer
ElemDesp2ODBDesp(IndexAMSUcldcost) % has_missing                 = c_false

ElemDesp2ODBDesp(IndexSITE_NAME) % TableName                     = 'hdr'
ElemDesp2ODBDesp(IndexSITE_NAME) % ColName                       = 'site_name'
IF (ObsGroup == ObsGroupSurface) THEN ! Assume WOW
  ElemDesp2ODBDesp(IndexSITE_NAME) % colname_array_length        = 4
ELSE ! Assume GPSIWV
  ElemDesp2ODBDesp(IndexSITE_NAME) % colname_array_length        = 3
END IF
ElemDesp2ODBDesp(IndexSITE_NAME) % data_type                     = odb_string
ElemDesp2ODBDesp(IndexSITE_NAME) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexSoftwareIdentity) % TableName              = 'sat'
ElemDesp2ODBDesp(IndexSoftwareIdentity) % ColName                = 'software_identifier'
ElemDesp2ODBDesp(IndexSoftwareIdentity) % data_type              = odb_integer
ElemDesp2ODBDesp(IndexSoftwareIdentity) % has_missing            = c_false

ElemDesp2ODBDesp(IndexRO_Quality) % TableName                    = 'gnssro'
ElemDesp2ODBDesp(IndexRO_Quality) % ColName                      = 'qc_flags'
ElemDesp2ODBDesp(IndexRO_Quality) % data_type                    = odb_real
ElemDesp2ODBDesp(IndexRO_Quality) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexRO_Confidence) % TableName                 = 'gnssro'
ElemDesp2ODBDesp(IndexRO_Confidence) % ColName                   = 'confidence'
ElemDesp2ODBDesp(IndexRO_Confidence) % data_type                 = odb_real
ElemDesp2ODBDesp(IndexRO_Confidence) % has_missing               = c_false

ElemDesp2ODBDesp(IndexRO_GeoidUnd) % TableName                   = 'gnssro'
ElemDesp2ODBDesp(IndexRO_GeoidUnd) % ColName                     = 'undulation'
ElemDesp2ODBDesp(IndexRO_GeoidUnd) % data_type                   = odb_real
ElemDesp2ODBDesp(IndexRO_GeoidUnd) % has_missing                 = c_false

ElemDesp2ODBDesp(IndexRO_RadCurv) % TableName                    = 'gnssro'
ElemDesp2ODBDesp(IndexRO_RadCurv) % ColName                      = 'radcurv'
ElemDesp2ODBDesp(IndexRO_RadCurv) % data_type                    = odb_real
ElemDesp2ODBDesp(IndexRO_RadCurv) % has_missing                  = c_false

IF (ObsGroup == ObsGroupSatwind) THEN
  ElemDesp2ODBDesp(IndexCmptVecWindDirn) % TableName             = 'satob'
  ElemDesp2ODBDesp(IndexCmptVecWindDirn) % ColName               = 'dd_comp'
  ElemDesp2ODBDesp(IndexCmptVecWindDirn) % colname_array_length  = 4
  ElemDesp2ODBDesp(IndexCmptVecWindDirn) % data_type             = odb_real
  ElemDesp2ODBDesp(IndexCmptVecWindDirn) % has_missing           = c_false

  ElemDesp2ODBDesp(IndexCmptVecWindSpeed) % TableName            = 'satob'
  ElemDesp2ODBDesp(IndexCmptVecWindSpeed) % ColName              = 'ff_comp'
  ElemDesp2ODBDesp(IndexCmptVecWindSpeed) % colname_array_length = 4
  ElemDesp2ODBDesp(IndexCmptVecWindSpeed) % data_type            = odb_real
  ElemDesp2ODBDesp(IndexCmptVecWindSpeed) % has_missing          = c_false

  ElemDesp2ODBDesp(IndexCmptVecWindU) % TableName                = 'satob'
  ElemDesp2ODBDesp(IndexCmptVecWindU) % ColName                  = 'u_comp'
  ElemDesp2ODBDesp(IndexCmptVecWindU) % colname_array_length     = 4
  ElemDesp2ODBDesp(IndexCmptVecWindU) % data_type                = odb_real
  ElemDesp2ODBDesp(IndexCmptVecWindU) % has_missing              = c_false

  ElemDesp2ODBDesp(IndexCmptVecWindV) % TableName                = 'satob'
  ElemDesp2ODBDesp(IndexCmptVecWindV) % ColName                  = 'v_comp'
  ElemDesp2ODBDesp(IndexCmptVecWindV) % colname_array_length     = 4
  ElemDesp2ODBDesp(IndexCmptVecWindV) % data_type                = odb_real
  ElemDesp2ODBDesp(IndexCmptVecWindV) % has_missing              = c_false
END IF

ElemDesp2ODBDesp(IndexAltHeightAss) % TableName                  = 'satob'
ElemDesp2ODBDesp(IndexAltHeightAss) % ColName                    = 'alt_height_assignment_method'
ElemDesp2ODBDesp(IndexAltHeightAss) % colname_array_length       = 10
ElemDesp2ODBDesp(IndexAltHeightAss) % data_type                  = odb_integer
ElemDesp2ODBDesp(IndexAltHeightAss) % has_missing                = c_false

ElemDesp2ODBDesp(IndexAltPressLevel) % TableName                 = 'satob'
ElemDesp2ODBDesp(IndexAltPressLevel) % ColName                   = 'alt_press'
ElemDesp2ODBDesp(IndexAltPressLevel) % colname_array_length      = 10
ElemDesp2ODBDesp(IndexAltPressLevel) % data_type                 = odb_real
ElemDesp2ODBDesp(IndexAltPressLevel) % has_missing               = c_false

ElemDesp2ODBDesp(IndexAltAirTempLevel) % TableName               = 'satob'
ElemDesp2ODBDesp(IndexAltAirTempLevel) % ColName                 = 'alt_temp'
ElemDesp2ODBDesp(IndexAltAirTempLevel) % colname_array_length    = 10
ElemDesp2ODBDesp(IndexAltAirTempLevel) % data_type               = odb_real
ElemDesp2ODBDesp(IndexAltAirTempLevel) % has_missing             = c_false

ElemDesp2ODBDesp(IndexNesdisQI1) % TableName                     = 'satob'
ElemDesp2ODBDesp(IndexNesdisQI1) % ColName                       = 'qi2'
ElemDesp2ODBDesp(IndexNesdisQI1) % colname_array_length          = 10
ElemDesp2ODBDesp(IndexNesdisQI1) % data_type                     = odb_real
ElemDesp2ODBDesp(IndexNesdisQI1) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexNesdisQI2) % TableName                     = 'satob'
ElemDesp2ODBDesp(IndexNesdisQI2) % ColName                       = 'rff'
ElemDesp2ODBDesp(IndexNesdisQI2) % colname_array_length          = 10
ElemDesp2ODBDesp(IndexNesdisQI2) % data_type                     = odb_real
ElemDesp2ODBDesp(IndexNesdisQI2) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexNesdisQI3) % TableName                     = 'satob'
ElemDesp2ODBDesp(IndexNesdisQI3) % ColName                       = 'qi1'
ElemDesp2ODBDesp(IndexNesdisQI3) % colname_array_length          = 10
ElemDesp2ODBDesp(IndexNesdisQI3) % data_type                     = odb_real
ElemDesp2ODBDesp(IndexNesdisQI3) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexJmaQI1) % TableName                        = 'satob'
ElemDesp2ODBDesp(IndexJmaQI1) % ColName                          = 'qi1'
ElemDesp2ODBDesp(IndexJmaQI1) % colname_array_length             = 4
ElemDesp2ODBDesp(IndexJmaQI1) % data_type                        = odb_real
ElemDesp2ODBDesp(IndexJmaQI1) % has_missing                      = c_false

ElemDesp2ODBDesp(IndexJmaQI2) % TableName                        = 'satob'
ElemDesp2ODBDesp(IndexJmaQI2) % ColName                          = 'qi2'
ElemDesp2ODBDesp(IndexJmaQI2) % colname_array_length             = 4
ElemDesp2ODBDesp(IndexJmaQI2) % data_type                        = odb_real
ElemDesp2ODBDesp(IndexJmaQI2) % has_missing                      = c_false

ElemDesp2ODBDesp(IndexJmaQI3) % TableName                        = 'satob'
ElemDesp2ODBDesp(IndexJmaQI3) % ColName                          = 'rff'
ElemDesp2ODBDesp(IndexJmaQI3) % colname_array_length             = 4
ElemDesp2ODBDesp(IndexJmaQI3) % data_type                        = odb_real
ElemDesp2ODBDesp(IndexJmaQI3) % has_missing                      = c_false

ElemDesp2ODBDesp(IndexMPEFQI) % TableName                        = 'satob'
ElemDesp2ODBDesp(IndexMPEFQI) % ColName                          = 'qi1'
ElemDesp2ODBDesp(IndexMPEFQI) % colname_array_length             = 4
ElemDesp2ODBDesp(IndexMPEFQI) % data_type                        = odb_real
ElemDesp2ODBDesp(IndexMPEFQI) % has_missing                      = c_false

ElemDesp2ODBDesp(IndexMPEFQI2) % TableName                       = 'satob'
ElemDesp2ODBDesp(IndexMPEFQI2) % ColName                         = 'qi2'
ElemDesp2ODBDesp(IndexMPEFQI2) % colname_array_length            = 4
ElemDesp2ODBDesp(IndexMPEFQI2) % data_type                       = odb_real
ElemDesp2ODBDesp(IndexMPEFQI2) % has_missing                     = c_false

ElemDesp2ODBDesp(IndexCmaQI1) % TableName                        = 'satob'
ElemDesp2ODBDesp(IndexCmaQI1) % ColName                          = 'qi1'
ElemDesp2ODBDesp(IndexCmaQI1) % colname_array_length             = 4
ElemDesp2ODBDesp(IndexCmaQI1) % data_type                        = odb_real
ElemDesp2ODBDesp(IndexCmaQI1) % has_missing                      = c_false

ElemDesp2ODBDesp(IndexCmaQI2) % TableName                        = 'satob'
ElemDesp2ODBDesp(IndexCmaQI2) % ColName                          = 'qi2'
ElemDesp2ODBDesp(IndexCmaQI2) % colname_array_length             = 4
ElemDesp2ODBDesp(IndexCmaQI2) % data_type                        = odb_real
ElemDesp2ODBDesp(IndexCmaQI2) % has_missing                      = c_false

ElemDesp2ODBDesp(IndexKmaQI1) % TableName                        = 'satob'
ElemDesp2ODBDesp(IndexKmaQI1) % ColName                          = 'qi1'
ElemDesp2ODBDesp(IndexKmaQI1) % colname_array_length             = 4
ElemDesp2ODBDesp(IndexKmaQI1) % data_type                        = odb_real
ElemDesp2ODBDesp(IndexKmaQI1) % has_missing                      = c_false

ElemDesp2ODBDesp(IndexKmaQI2) % TableName                        = 'satob'
ElemDesp2ODBDesp(IndexKmaQI2) % ColName                          = 'qi2'
ElemDesp2ODBDesp(IndexKmaQI2) % colname_array_length             = 4
ElemDesp2ODBDesp(IndexKmaQI2) % data_type                        = odb_real
ElemDesp2ODBDesp(IndexKmaQI2) % has_missing                      = c_false

ElemDesp2ODBDesp(IndexKmaQI3) % TableName                        = 'satob'
ElemDesp2ODBDesp(IndexKmaQI3) % ColName                          = 'rff'
ElemDesp2ODBDesp(IndexKmaQI3) % colname_array_length             = 4
ElemDesp2ODBDesp(IndexKmaQI3) % data_type                        = odb_real
ElemDesp2ODBDesp(IndexKmaQI3) % has_missing                      = c_false

ElemDesp2ODBDesp(IndexUkwQI1) % TableName                        = 'satob'
ElemDesp2ODBDesp(IndexUkwQI1) % ColName                          = 'qi1@satob'
ElemDesp2ODBDesp(IndexUkwQI1) % colname_array_length             = 4
ElemDesp2ODBDesp(IndexUkwQI1) % data_type                        = odb_real
ElemDesp2ODBDesp(IndexUkwQI1) % has_missing                      = c_false

ElemDesp2ODBDesp(IndexUkwQI2) % TableName                        = 'satob'
ElemDesp2ODBDesp(IndexUkwQI2) % ColName                          = 'qi2@satob'
ElemDesp2ODBDesp(IndexUkwQI2) % colname_array_length             = 4
ElemDesp2ODBDesp(IndexUkwQI2) % data_type                        = odb_real
ElemDesp2ODBDesp(IndexUkwQI2) % has_missing                      = c_false

ElemDesp2ODBDesp(IndexUkwQI3) % TableName                        = 'satob'
ElemDesp2ODBDesp(IndexUkwQI3) % ColName                          = 'qi3@satob'
ElemDesp2ODBDesp(IndexUkwQI3) % colname_array_length             = 4
ElemDesp2ODBDesp(IndexUkwQI3) % data_type                        = odb_real
ElemDesp2ODBDesp(IndexUkwQI3) % has_missing                      = c_false

ElemDesp2ODBDesp(IndexMisrQI1) % TableName                       = 'satob'
ElemDesp2ODBDesp(IndexMisrQI1) % ColName                         = 'qi1'
ElemDesp2ODBDesp(IndexMisrQI1) % colname_array_length            = 4
ElemDesp2ODBDesp(IndexMisrQI1) % data_type                       = odb_real
ElemDesp2ODBDesp(IndexMisrQI1) % has_missing                     = c_false

ElemDesp2ODBDesp(IndexSBUVSondgType) % TableName                 = 'sbuv'
ElemDesp2ODBDesp(IndexSBUVSondgType) % ColName                   = 'product_type'
ElemDesp2ODBDesp(IndexSBUVSondgType) % data_type                 = odb_integer
ElemDesp2ODBDesp(IndexSBUVSondgType) % has_missing               = c_false

ElemDesp2ODBDesp(IndexSBUVFOVLatitude) % TableName               = 'sbuv'
ElemDesp2ODBDesp(IndexSBUVFOVLatitude) % ColName                 = 'fov_latitude'
ElemDesp2ODBDesp(IndexSBUVFOVLatitude) % colname_array_length    = 4
ElemDesp2ODBDesp(IndexSBUVFOVLatitude) % data_type               = odb_real
ElemDesp2ODBDesp(IndexSBUVFOVLatitude) % has_missing             = c_false

ElemDesp2ODBDesp(IndexSBUVFOVLongitude) % TableName              = 'sbuv'
ElemDesp2ODBDesp(IndexSBUVFOVLongitude) % ColName                = 'fov_longitude'
ElemDesp2ODBDesp(IndexSBUVFOVLongitude) % colname_array_length   = 4
ElemDesp2ODBDesp(IndexSBUVFOVLongitude) % data_type              = odb_real
ElemDesp2ODBDesp(IndexSBUVFOVLongitude) % has_missing            = c_false

ElemDesp2ODBDesp(IndexSBUVSolarElev) % TableName                 = 'sbuv'
ElemDesp2ODBDesp(IndexSBUVSolarElev) % ColName                   = 'solar_elevation'
ElemDesp2ODBDesp(IndexSBUVSolarElev) % data_type                 = odb_real
ElemDesp2ODBDesp(IndexSBUVSolarElev) % has_missing               = c_false

ElemDesp2ODBDesp(IndexSBUVQualityInf) % TableName                = 'sbuv'
ElemDesp2ODBDesp(IndexSBUVQualityInf) % ColName                  = 'sbuv_quality_info'
ElemDesp2ODBDesp(IndexSBUVQualityInf) % data_type                = odb_integer
ElemDesp2ODBDesp(IndexSBUVQualityInf) % has_missing              = c_false

ElemDesp2ODBDesp(IndexSBUVLayerNum) % TableName                  = 'sbuv'
ElemDesp2ODBDesp(IndexSBUVLayerNum) % ColName                    = 'number_of_layers'
ElemDesp2ODBDesp(IndexSBUVLayerNum) % data_type                  = odb_integer
ElemDesp2ODBDesp(IndexSBUVLayerNum) % has_missing                = c_false

ElemDesp2ODBDesp(IndexSBUVReptiles) % TableName                  = 'sbuv'
ElemDesp2ODBDesp(IndexSBUVReptiles) % ColName                    = 'layer_replication_count'
ElemDesp2ODBDesp(IndexSBUVReptiles) % data_type                  = odb_integer
ElemDesp2ODBDesp(IndexSBUVReptiles) % has_missing                = c_false

ElemDesp2ODBDesp(IndexSBUVStDevReptiles) % TableName             = 'sbuv'
ElemDesp2ODBDesp(IndexSBUVStDevReptiles) % ColName               = 'layer_std_dev_replication_count'
ElemDesp2ODBDesp(IndexSBUVStDevReptiles) % data_type             = odb_integer
ElemDesp2ODBDesp(IndexSBUVStDevReptiles) % has_missing           = c_false

ElemDesp2ODBDesp(IndexSatView) % TableName                       = 'sat'
ElemDesp2ODBDesp(IndexSatView) % ColName                         = 'viewing_zenith_angle'
ElemDesp2ODBDesp(IndexSatView) % data_type                       = odb_real
ElemDesp2ODBDesp(IndexSatView) % has_missing                     = c_false

ElemDesp2ODBDesp(IndexAMSRLTTD_GHZ89) % TableName                = 'amsr'
ElemDesp2ODBDesp(IndexAMSRLTTD_GHZ89) % ColName                  = 'amsr_89ghz_latitude'
ElemDesp2ODBDesp(IndexAMSRLTTD_GHZ89) % colname_array_length     = 2
ElemDesp2ODBDesp(IndexAMSRLTTD_GHZ89) % data_type                = odb_real
ElemDesp2ODBDesp(IndexAMSRLTTD_GHZ89) % has_missing              = c_false

ElemDesp2ODBDesp(IndexAMSRLNGD_GHZ89) % TableName                = 'amsr'
ElemDesp2ODBDesp(IndexAMSRLNGD_GHZ89) % ColName                  = 'amsr_89ghz_longitude'
ElemDesp2ODBDesp(IndexAMSRLNGD_GHZ89) % colname_array_length     = 2
ElemDesp2ODBDesp(IndexAMSRLNGD_GHZ89) % data_type                = odb_real
ElemDesp2ODBDesp(IndexAMSRLNGD_GHZ89) % has_missing              = c_false

ElemDesp2ODBDesp(IndexAMSRFOV_NMBR_GHZ89) % TableName            = 'amsr'
ElemDesp2ODBDesp(IndexAMSRFOV_NMBR_GHZ89) % ColName              = 'amsr_89ghz_fov_number'
ElemDesp2ODBDesp(IndexAMSRFOV_NMBR_GHZ89) % colname_array_length = 2
ElemDesp2ODBDesp(IndexAMSRFOV_NMBR_GHZ89) % data_type            = odb_real
ElemDesp2ODBDesp(IndexAMSRFOV_NMBR_GHZ89) % has_missing          = c_false

ElemDesp2ODBDesp(IndexNoDualPixels) % TableName                  = 'aatsr'
ElemDesp2ODBDesp(IndexNoDualPixels) % ColName                    = 'number_of_dual_pixels'
ElemDesp2ODBDesp(IndexNoDualPixels) % data_type                  = odb_integer
ElemDesp2ODBDesp(IndexNoDualPixels) % has_missing                = c_false

ElemDesp2ODBDesp(IndexSSTCnfd) % TableName                       = 'aatsr'
ElemDesp2ODBDesp(IndexSSTCnfd) % ColName                         = 'sst_conf'
ElemDesp2ODBDesp(IndexSSTCnfd) % data_type                       = odb_real
ElemDesp2ODBDesp(IndexSSTCnfd) % has_missing                     = c_false

ElemDesp2ODBDesp(IndexSSTBias) % TableName                       = 'aatsr'
ElemDesp2ODBDesp(IndexSSTBias) % ColName                         = 'sst_bias'
ElemDesp2ODBDesp(IndexSSTBias) % data_type                       = odb_real
ElemDesp2ODBDesp(IndexSSTBias) % has_missing                     = c_false

ElemDesp2ODBDesp(IndexSSTQuality) % TableName                    = 'aatsr'
ElemDesp2ODBDesp(IndexSSTQuality) % ColName                      = 'sst_quality'
ElemDesp2ODBDesp(IndexSSTQuality) % data_type                    = odb_real
ElemDesp2ODBDesp(IndexSSTQuality) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexSSTAlgrType) % TableName                   = 'aatsr'
ElemDesp2ODBDesp(IndexSSTAlgrType) % ColName                     = 'sst_algorithm_type'
ElemDesp2ODBDesp(IndexSSTAlgrType) % data_type                   = odb_integer
ElemDesp2ODBDesp(IndexSSTAlgrType) % has_missing                 = c_false

ElemDesp2ODBDesp(IndexAngstromExponent) % TableName              = 'aod'
ElemDesp2ODBDesp(IndexAngstromExponent) % ColName                = 'angstrom_exponent'
ElemDesp2ODBDesp(IndexAngstromExponent) % data_type              = odb_real
ElemDesp2ODBDesp(IndexAngstromExponent) % has_missing            = c_false

ElemDesp2ODBDesp(IndexMassConcentration) % TableName             = 'aod'
ElemDesp2ODBDesp(IndexMassConcentration) % ColName               = 'aerosol_mass_concentration'
ElemDesp2ODBDesp(IndexMassConcentration) % data_type             = odb_real
ElemDesp2ODBDesp(IndexMassConcentration) % has_missing           = c_false

ElemDesp2ODBDesp(IndexSmallModeFraction) % TableName             = 'aod'
ElemDesp2ODBDesp(IndexSmallModeFraction) % ColName               = 'aerosol_small_mode_fraction'
ElemDesp2ODBDesp(IndexSmallModeFraction) % data_type             = odb_real
ElemDesp2ODBDesp(IndexSmallModeFraction) % has_missing           = c_false

ElemDesp2ODBDesp(IndexEffectiveRadius) % TableName               = 'aod'
ElemDesp2ODBDesp(IndexEffectiveRadius) % ColName                 = 'aerosol_effective_radius'
ElemDesp2ODBDesp(IndexEffectiveRadius) % data_type               = odb_real
ElemDesp2ODBDesp(IndexEffectiveRadius) % has_missing             = c_false

ElemDesp2ODBDesp(IndexAerosolType) % TableName                   = 'aod'
ElemDesp2ODBDesp(IndexAerosolType) % ColName                     = 'aerosol_type'
ElemDesp2ODBDesp(IndexAerosolType) % data_type                   = odb_integer
ElemDesp2ODBDesp(IndexAerosolType) % has_missing                 = c_false

ElemDesp2ODBDesp(IndexAerosolConf) % TableName                   = 'aod'
ElemDesp2ODBDesp(IndexAerosolConf) % ColName                     = 'aerosol_confidence_flag'
ElemDesp2ODBDesp(IndexAerosolConf) % data_type                   = odb_integer
ElemDesp2ODBDesp(IndexAerosolConf) % has_missing                 = c_false

ElemDesp2ODBDesp(IndexATMS_LEVL1B_CNTR) % TableName              = 'sat'
ElemDesp2ODBDesp(IndexATMS_LEVL1B_CNTR) % ColName                = 'level_1b_creation_centre'
ElemDesp2ODBDesp(IndexATMS_LEVL1B_CNTR) % data_type              = odb_integer
ElemDesp2ODBDesp(IndexATMS_LEVL1B_CNTR) % has_missing            = c_false

ElemDesp2ODBDesp(IndexLevel1CCentre1) % TableName                = 'sat'
ElemDesp2ODBDesp(IndexLevel1CCentre1) % ColName                  = 'level_1c_creation_centre'
ElemDesp2ODBDesp(IndexLevel1CCentre1) % data_type                = odb_integer
ElemDesp2ODBDesp(IndexLevel1CCentre1) % has_missing              = c_false

ElemDesp2ODBDesp(IndexLevel1CCentre2) % TableName                = 'sat'
ElemDesp2ODBDesp(IndexLevel1CCentre2) % ColName                  = 'level_1c_creation_subcentre'
ElemDesp2ODBDesp(IndexLevel1CCentre2) % data_type                = odb_integer
ElemDesp2ODBDesp(IndexLevel1CCentre2) % has_missing              = c_false

ElemDesp2ODBDesp(IndexOrbitNumber) % TableName                   = 'radiance'
ElemDesp2ODBDesp(IndexOrbitNumber) % ColName                     = 'orbit'
ElemDesp2ODBDesp(IndexOrbitNumber) % data_type                   = odb_integer
ElemDesp2ODBDesp(IndexOrbitNumber) % has_missing                 = c_false

ElemDesp2ODBDesp(IndexSatAntCorVer) % TableName                  = 'radiance'
ElemDesp2ODBDesp(IndexSatAntCorVer) % ColName                    = 'satellite_antenna_correction_version'
ElemDesp2ODBDesp(IndexSatAntCorVer) % data_type                  = odb_integer
ElemDesp2ODBDesp(IndexSatAntCorVer) % has_missing                = c_false

ElemDesp2ODBDesp(IndexMajorFrameCount) % TableName               = 'radiance'
ElemDesp2ODBDesp(IndexMajorFrameCount) % ColName                 = 'major_frame_count'
ElemDesp2ODBDesp(IndexMajorFrameCount) % data_type               = odb_integer
ElemDesp2ODBDesp(IndexMajorFrameCount) % has_missing             = c_false

ElemDesp2ODBDesp(IndexPayloadMode) % TableName                   = 'radiance'
ElemDesp2ODBDesp(IndexPayloadMode) % ColName                     = 'payload_mode'
ElemDesp2ODBDesp(IndexPayloadMode) % data_type                   = odb_integer
ElemDesp2ODBDesp(IndexPayloadMode) % has_missing                 = c_false

ElemDesp2ODBDesp(IndexSatelliteHeight) % TableName               = 'sat'
ElemDesp2ODBDesp(IndexSatelliteHeight) % ColName                 = 'satellite_height'
ElemDesp2ODBDesp(IndexSatelliteHeight) % data_type               = odb_real
ElemDesp2ODBDesp(IndexSatelliteHeight) % has_missing             = c_false

ElemDesp2ODBDesp(IndexIncidenceAngle) % TableName                = 'sat'
ElemDesp2ODBDesp(IndexIncidenceAngle) % ColName                  = 'incidence_angle'
ElemDesp2ODBDesp(IndexIncidenceAngle) % data_type                = odb_real
ElemDesp2ODBDesp(IndexIncidenceAngle) % has_missing              = c_false

ElemDesp2ODBDesp(IndexSatelliteMode) % TableName                 = 'sat'
ElemDesp2ODBDesp(IndexSatelliteMode) % ColName                   = 'satellite_mode'
ElemDesp2ODBDesp(IndexSatelliteMode) % data_type                 = odb_integer
ElemDesp2ODBDesp(IndexSatelliteMode) % has_missing               = c_false

ElemDesp2ODBDesp(IndexSaphirInstTemp) % TableName                = 'radiance'
ElemDesp2ODBDesp(IndexSaphirInstTemp) % ColName                  = 'saphir_temp'
ElemDesp2ODBDesp(IndexSaphirInstTemp) % data_type                = odb_real
ElemDesp2ODBDesp(IndexSaphirInstTemp) % has_missing              = c_false

ElemDesp2ODBDesp(IndexMHSInstTemp) % TableName                   = 'radiance'
ElemDesp2ODBDesp(IndexMHSInstTemp) % ColName                     = 'mhs_temp'
ElemDesp2ODBDesp(IndexMHSInstTemp) % data_type                   = odb_real
ElemDesp2ODBDesp(IndexMHSInstTemp) % has_missing                 = c_false

ElemDesp2ODBDesp(IndexLowestCloudBaseHeight) % TableName         = 'conv'
ElemDesp2ODBDesp(IndexLowestCloudBaseHeight) % ColName           = 'lowest_cloud_base_height'
ElemDesp2ODBDesp(IndexLowestCloudBaseHeight) % data_type         = odb_real
ElemDesp2ODBDesp(IndexLowestCloudBaseHeight) % has_missing       = c_false

ElemDesp2ODBDesp(IndexRadarBeamElev) % TableName                 = 'radar'
ElemDesp2ODBDesp(IndexRadarBeamElev) % ColName                   = 'radar_beam_elevation'
ElemDesp2ODBDesp(IndexRadarBeamElev) % data_type                 = odb_real
ElemDesp2ODBDesp(IndexRadarBeamElev) % has_missing               = c_false

ElemDesp2ODBDesp(IndexRadObRange) % TableName                    = 'radar'
ElemDesp2ODBDesp(IndexRadObRange) % ColName                      = 'radar_ob_range'
ElemDesp2ODBDesp(IndexRadObRange) % data_type                    = odb_real
ElemDesp2ODBDesp(IndexRadObRange) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexRadObAzim) % TableName                     = 'radar'
ElemDesp2ODBDesp(IndexRadObAzim) % ColName                       = 'radar_ob_azimuth'
ElemDesp2ODBDesp(IndexRadObAzim) % data_type                     = odb_real
ElemDesp2ODBDesp(IndexRadObAzim) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexRadRangeStart) % TableName                 = 'radar'
ElemDesp2ODBDesp(IndexRadRangeStart) % ColName                   = 'radar_range_start'
ElemDesp2ODBDesp(IndexRadRangeStart) % data_type                = odb_real
ElemDesp2ODBDesp(IndexRadRangeStart) % has_missing              = c_false

ElemDesp2ODBDesp(IndexRadialVelErr) % TableName                  = 'radar'
ElemDesp2ODBDesp(IndexRadialVelErr) % ColName                    = 'radial_velocity_error'
ElemDesp2ODBDesp(IndexRadialVelErr) % data_type                  = odb_real
ElemDesp2ODBDesp(IndexRadialVelErr) % has_missing                = c_false

ElemDesp2ODBDesp(IndexReflErr) % TableName                       = 'radar'
ElemDesp2ODBDesp(IndexReflErr) % ColName                         = 'reflectivity_error'
ElemDesp2ODBDesp(IndexReflErr) % data_type                       = odb_real
ElemDesp2ODBDesp(IndexReflErr) % has_missing                     = c_false

ElemDesp2ODBDesp(IndexReflFlags) % TableName                     = 'radar'
ElemDesp2ODBDesp(IndexReflFlags) % ColName                       = 'reflectivity_flags'
ElemDesp2ODBDesp(IndexReflFlags) % data_type                     = odb_integer
ElemDesp2ODBDesp(IndexReflFlags) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexRadIdent) % TableName                      = 'radar'
ElemDesp2ODBDesp(IndexRadIdent) % ColName                        = 'radar_identifier'
ElemDesp2ODBDesp(IndexRadIdent) % data_type                      = odb_integer
ElemDesp2ODBDesp(IndexRadIdent) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexRadSiteLat) % TableName                    = 'radar'
ElemDesp2ODBDesp(IndexRadSiteLat) % ColName                      = 'radar_site_latitude'
ElemDesp2ODBDesp(IndexRadSiteLat) % data_type                    = odb_real
ElemDesp2ODBDesp(IndexRadSiteLat) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexRadSiteLong) % TableName                   = 'radar'
ElemDesp2ODBDesp(IndexRadSiteLong) % ColName                     = 'radar_site_longitude'
ElemDesp2ODBDesp(IndexRadSiteLong) % data_type                   = odb_real
ElemDesp2ODBDesp(IndexRadSiteLong) % has_missing                 = c_false

ElemDesp2ODBDesp(IndexRadAltAboveMSL) % TableName                = 'radar'
ElemDesp2ODBDesp(IndexRadAltAboveMSL) % ColName                  = 'radar_altitude_above_mean_sea_level'
ElemDesp2ODBDesp(IndexRadAltAboveMSL) % data_type                = odb_real
ElemDesp2ODBDesp(IndexRadAltAboveMSL) % has_missing              = c_false

ElemDesp2ODBDesp(IndexRadRadialSW) % TableName                   = 'radar'
ElemDesp2ODBDesp(IndexRadRadialSW) % ColName                     = 'radar_SW'
ElemDesp2ODBDesp(IndexRadRadialSW) % data_type                   = odb_real
ElemDesp2ODBDesp(IndexRadRadialSW) % has_missing                 = c_false

ElemDesp2ODBDesp(IndexRadNbGate) % TableName                     = 'radar'
ElemDesp2ODBDesp(IndexRadNbGate) % ColName                       = 'radar_nb_gate'
ElemDesp2ODBDesp(IndexRadNbGate) % data_type                     = odb_integer
ElemDesp2ODBDesp(IndexRadNbGate) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexRadGateRes) % TableName                    = 'radar'
ElemDesp2ODBDesp(IndexRadGateRes) % ColName                      = 'radar_gate_res'
ElemDesp2ODBDesp(IndexRadGateRes) % data_type                    = odb_real
ElemDesp2ODBDesp(IndexRadGateRes) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexRadNbRay) % TableName                      = 'radar'
ElemDesp2ODBDesp(IndexRadNbRay) % ColName                        = 'radar_nb_ray'
ElemDesp2ODBDesp(IndexRadNbRay) % data_type                      = odb_integer
ElemDesp2ODBDesp(IndexRadNbRay) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexRadRayRes) % TableName                     = 'radar'
ElemDesp2ODBDesp(IndexRadRayRes) % ColName                       = 'radar_ray_res'
ElemDesp2ODBDesp(IndexRadRayRes) % data_type                     = odb_real
ElemDesp2ODBDesp(IndexRadRayRes) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexRadPhase) % TableName                      = 'radar'
ElemDesp2ODBDesp(IndexRadPhase) % ColName                        = 'radar_phase'
ElemDesp2ODBDesp(IndexRadPhase) % data_type                      = odb_real
ElemDesp2ODBDesp(IndexRadPhase) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexRadUnfoldVelocity) % TableName             = 'radar'
ElemDesp2ODBDesp(IndexRadUnfoldVelocity) % ColName               = 'radar_unf_vel'
ElemDesp2ODBDesp(IndexRadUnfoldVelocity) % data_type             = odb_real
ElemDesp2ODBDesp(IndexRadUnfoldVelocity) % has_missing           = c_false

ElemDesp2ODBDesp(IndexRadFlag) % TableName                       = 'radar'
ElemDesp2ODBDesp(IndexRadFlag) % ColName                         = 'radar_flag'
ElemDesp2ODBDesp(IndexRadFlag) % data_type                       = odb_integer
ElemDesp2ODBDesp(IndexRadFlag) % has_missing                     = c_false

ElemDesp2ODBDesp(IndexRadFrequency) % TableName                  = 'radar'
ElemDesp2ODBDesp(IndexRadFrequency) % ColName                    = 'radar_nco_freq'
ElemDesp2ODBDesp(IndexRadFrequency) % data_type                  = odb_integer
ElemDesp2ODBDesp(IndexRadFrequency) % has_missing                = c_false

ElemDesp2ODBDesp(IndexLNG_STTN_NAME) % TableName                 = 'conv'
ElemDesp2ODBDesp(IndexLNG_STTN_NAME) % ColName                   = 'air_quality_station_name'
ElemDesp2ODBDesp(IndexLNG_STTN_NAME) % colname_array_length      = 4
ElemDesp2ODBDesp(IndexLNG_STTN_NAME) % data_type                 = odb_string
ElemDesp2ODBDesp(IndexLNG_STTN_NAME) % has_missing               = c_false

ElemDesp2ODBDesp(IndexGEMS_AQTY_OBSG_STTN) % TableName           = 'conv'
ElemDesp2ODBDesp(IndexGEMS_AQTY_OBSG_STTN) % ColName             = 'gems_station_code'
ElemDesp2ODBDesp(IndexGEMS_AQTY_OBSG_STTN) % data_type           = odb_string
ElemDesp2ODBDesp(IndexGEMS_AQTY_OBSG_STTN) % has_missing         = c_false

ElemDesp2ODBDesp(IndexAQTY_OBSG_STTN_LCL) % TableName            = 'conv'
ElemDesp2ODBDesp(IndexAQTY_OBSG_STTN_LCL) % ColName              = 'local_station_code'
ElemDesp2ODBDesp(IndexAQTY_OBSG_STTN_LCL) % data_type            = odb_string
ElemDesp2ODBDesp(IndexAQTY_OBSG_STTN_LCL) % has_missing          = c_false

ElemDesp2ODBDesp(IndexAVGNG_PERD) % TableName                    = 'conv'
ElemDesp2ODBDesp(IndexAVGNG_PERD) % ColName                      = 'averaging_period'
ElemDesp2ODBDesp(IndexAVGNG_PERD) % data_type                    = odb_integer
ElemDesp2ODBDesp(IndexAVGNG_PERD) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexCSNT_TYP) % TableName                      = 'conv'
ElemDesp2ODBDesp(IndexCSNT_TYP) % ColName                        = 'constituent_type'
ElemDesp2ODBDesp(IndexCSNT_TYP) % data_type                      = odb_integer
ElemDesp2ODBDesp(IndexCSNT_TYP) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexDCML_SCL_MASS_DNSTY) % TableName           = 'conv'
ElemDesp2ODBDesp(IndexDCML_SCL_MASS_DNSTY) % ColName             = 'scale_mass_density'
ElemDesp2ODBDesp(IndexDCML_SCL_MASS_DNSTY) % data_type           = odb_integer
ElemDesp2ODBDesp(IndexDCML_SCL_MASS_DNSTY) % has_missing         = c_false

ElemDesp2ODBDesp(IndexSITE_QAL) % TableName                      = 'conv'
ElemDesp2ODBDesp(IndexSITE_QAL) % ColName                        = 'site_quality'
ElemDesp2ODBDesp(IndexSITE_QAL) % data_type                      = odb_integer
ElemDesp2ODBDesp(IndexSITE_QAL) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexObType) % TableName                        = 'hdr'
ElemDesp2ODBDesp(IndexObType) % ColName                          = 'ops_obstype'
ElemDesp2ODBDesp(IndexObType) % data_type                        = odb_integer
ElemDesp2ODBDesp(IndexObType) % has_missing                      = c_false

ElemDesp2ODBDesp(IndexIASIPCScore) % TableName                   = 'iasi_principle_component'
ElemDesp2ODBDesp(IndexIASIPCScore) % ColName                     = 'principle_component_score'
ElemDesp2ODBDesp(IndexIASIPCScore) % colname_array_length        = 300

ElemDesp2ODBDesp(IndexIASIPCScoreCount) % TableName              = 'iasi_principle_component'
ElemDesp2ODBDesp(IndexIASIPCScoreCount) % ColName                = 'principle_component_count'

ElemDesp2ODBDesp(IndexODBObsType) % TableName                    = 'hdr'
ElemDesp2ODBDesp(IndexODBObsType) % ColName                      = 'obstype'

ElemDesp2ODBDesp(IndexODBCodeType) % TableName                   = 'hdr'
ElemDesp2ODBDesp(IndexODBCodeType) % ColName                     = 'codetype'

ElemDesp2ODBDesp(IndexTransponderAddress) % TableName            = 'conv'
ElemDesp2ODBDesp(IndexTransponderAddress) % ColName              = 'aircraft_transponder_address'
ElemDesp2ODBDesp(IndexTransponderAddress) % data_type            = odb_string
ElemDesp2ODBDesp(IndexTransponderAddress) % has_missing          = c_false

ElemDesp2ODBDesp(IndexAircraftType) % TableName                  = 'conv'
ElemDesp2ODBDesp(IndexAircraftType) % ColName                    = 'aircraft_airframe_code'
ElemDesp2ODBDesp(IndexAircraftType) % data_type                  = odb_string
ElemDesp2ODBDesp(IndexAircraftType) % has_missing                = c_false

ElemDesp2ODBDesp(IndexAircraftGNSSAltitude) % TableName          = 'conv'
ElemDesp2ODBDesp(IndexAircraftGNSSAltitude) % ColName            = 'aircraft_gnss_alt'
ElemDesp2ODBDesp(IndexAircraftGNSSAltitude) % data_type          = odb_real
ElemDesp2ODBDesp(IndexAircraftGNSSAltitude) % has_missing        = c_false

ElemDesp2ODBDesp(IndexAircraftIndicatedAirSpeed) % TableName     = 'conv'
ElemDesp2ODBDesp(IndexAircraftIndicatedAirSpeed) % ColName       = 'aircraft_ind_air_speed'
ElemDesp2ODBDesp(IndexAircraftIndicatedAirSpeed) % data_type     = odb_real
ElemDesp2ODBDesp(IndexAircraftIndicatedAirSpeed) % has_missing   = c_false

ElemDesp2ODBDesp(IndexAircraftTrueAirSpeed) % TableName          = 'conv'
ElemDesp2ODBDesp(IndexAircraftTrueAirSpeed) % ColName            = 'aircraft_true_air_speed'
ElemDesp2ODBDesp(IndexAircraftTrueAirSpeed) % data_type          = odb_real
ElemDesp2ODBDesp(IndexAircraftTrueAirSpeed) % has_missing        = c_false

ElemDesp2ODBDesp(IndexAircraftGroundSpeed) % TableName           = 'conv'
ElemDesp2ODBDesp(IndexAircraftGroundSpeed) % ColName             = 'aircraft_ground_speed'
ElemDesp2ODBDesp(IndexAircraftGroundSpeed) % data_type           = odb_real
ElemDesp2ODBDesp(IndexAircraftGroundSpeed) % has_missing         = c_false

ElemDesp2ODBDesp(IndexAircraftTrueHeading) % TableName           = 'conv'
ElemDesp2ODBDesp(IndexAircraftTrueHeading) % ColName             = 'aircraft_true_heading'
ElemDesp2ODBDesp(IndexAircraftTrueHeading) % data_type           = odb_real
ElemDesp2ODBDesp(IndexAircraftTrueHeading) % has_missing         = c_false

ElemDesp2ODBDesp(IndexAircraftGroundHeading) % TableName         = 'conv'
ElemDesp2ODBDesp(IndexAircraftGroundHeading) % ColName           = 'aircraft_ground_heading'
ElemDesp2ODBDesp(IndexAircraftGroundHeading) % data_type         = odb_real
ElemDesp2ODBDesp(IndexAircraftGroundHeading) % has_missing       = c_false

ElemDesp2ODBDesp(IndexAircraftRadarGroundSpeed) % TableName      = 'conv'
ElemDesp2ODBDesp(IndexAircraftRadarGroundSpeed) % ColName        = 'aircraft_radar_ground_speed'
ElemDesp2ODBDesp(IndexAircraftRadarGroundSpeed) % data_type      = odb_real
ElemDesp2ODBDesp(IndexAircraftRadarGroundSpeed) % has_missing    = c_false

ElemDesp2ODBDesp(IndexAircraftRadarGroundHeading) % TableName    = 'conv'
ElemDesp2ODBDesp(IndexAircraftRadarGroundHeading) % ColName      = 'aircraft_radar_ground_heading'
ElemDesp2ODBDesp(IndexAircraftRadarGroundHeading) % data_type    = odb_real
ElemDesp2ODBDesp(IndexAircraftRadarGroundHeading) % has_missing  = c_false

ElemDesp2ODBDesp(IndexMachNumber) % TableName                    = 'conv'
ElemDesp2ODBDesp(IndexMachNumber) % ColName                      = 'aircraft_mach_number'
ElemDesp2ODBDesp(IndexMachNumber) % data_type                    = odb_real
ElemDesp2ODBDesp(IndexMachNumber) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexAircraftRadarT) % TableName                = 'conv'
ElemDesp2ODBDesp(IndexAircraftRadarT) % ColName                  = 'aircraft_radar_temp'
ElemDesp2ODBDesp(IndexAircraftRadarT) % data_type                = odb_real
ElemDesp2ODBDesp(IndexAircraftRadarT) % has_missing              = c_false

ElemDesp2ODBDesp(IndexAircraftGeomagneticDeviation) % TableName  = 'conv'
ElemDesp2ODBDesp(IndexAircraftGeomagneticDeviation) % ColName    = 'aircraft_geomagnetic_deviation'
ElemDesp2ODBDesp(IndexAircraftGeomagneticDeviation) % data_type  = odb_real
ElemDesp2ODBDesp(IndexAircraftGeomagneticDeviation) % has_missing= c_false

ElemDesp2ODBDesp(IndexAircraftMagneticHeadingBias) % TableName   = 'conv'
ElemDesp2ODBDesp(IndexAircraftMagneticHeadingBias) % ColName     = 'aircraft_heading_bias'
ElemDesp2ODBDesp(IndexAircraftMagneticHeadingBias) % data_type   = odb_real
ElemDesp2ODBDesp(IndexAircraftMagneticHeadingBias) % has_missing = c_false

ElemDesp2ODBDesp(IndexAircraftRollAngle) % TableName             = 'conv'
ElemDesp2ODBDesp(IndexAircraftRollAngle) % ColName               = 'aircraft_roll_angle'
ElemDesp2ODBDesp(IndexAircraftRollAngle) % data_type             = odb_real
ElemDesp2ODBDesp(IndexAircraftRollAngle) % has_missing           = c_false

ElemDesp2ODBDesp(IndexAircraftRadarDDD) % TableName              = 'conv'
ElemDesp2ODBDesp(IndexAircraftRadarDDD) % ColName                = 'aircraft_radar_dd'
ElemDesp2ODBDesp(IndexAircraftRadarDDD) % data_type              = odb_real
ElemDesp2ODBDesp(IndexAircraftRadarDDD) % has_missing            = c_false

ElemDesp2ODBDesp(IndexAircraftRadarU) % TableName                = 'conv'
ElemDesp2ODBDesp(IndexAircraftRadarU) % ColName                  = 'aircraft_radar_u'
ElemDesp2ODBDesp(IndexAircraftRadarU) % data_type                = odb_real
ElemDesp2ODBDesp(IndexAircraftRadarU) % has_missing              = c_false

ElemDesp2ODBDesp(IndexAircraftRadarV) % TableName                = 'conv'
ElemDesp2ODBDesp(IndexAircraftRadarV) % ColName                  = 'aircraft_radar_v'
ElemDesp2ODBDesp(IndexAircraftRadarV) % data_type                = odb_real
ElemDesp2ODBDesp(IndexAircraftRadarV) % has_missing              = c_false

ElemDesp2ODBDesp(IndexSITE_TYPE) % TableName                     = 'conv'
ElemDesp2ODBDesp(IndexSITE_TYPE) % ColName                       = 'site_type'
ElemDesp2ODBDesp(IndexSITE_TYPE) % data_type                     = odb_integer
ElemDesp2ODBDesp(IndexSITE_TYPE) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexGnssClass) % TableName                     = 'gnssro'
ElemDesp2ODBDesp(IndexGnssClass) % ColName                       = 'satellite_classification'
ElemDesp2ODBDesp(IndexGnssClass) % data_type                     = odb_integer
ElemDesp2ODBDesp(IndexGnssClass) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexGnssId) % TableName                        = 'gnssro'
ElemDesp2ODBDesp(IndexGnssId) % ColName                          = 'platform_id'
ElemDesp2ODBDesp(IndexGnssId) % data_type                        = odb_integer
ElemDesp2ODBDesp(IndexGnssId) % has_missing                      = c_false

ElemDesp2ODBDesp(IndexRotAngle) % TableName                      = 'radiance'
ElemDesp2ODBDesp(IndexRotAngle) % ColName                        = 'geometric_rotational_angle'
ElemDesp2ODBDesp(IndexRotAngle) % data_type                      = odb_real
ElemDesp2ODBDesp(IndexRotAngle) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexRadFrequency) % TableName                  = 'radar'
ElemDesp2ODBDesp(IndexRadFrequency) % ColName                    = 'radar_nco_frequency'
ElemDesp2ODBDesp(IndexRadFrequency) % data_type                  = odb_real
ElemDesp2ODBDesp(IndexRadFrequency) % has_missing                = c_false

ElemDesp2ODBDesp(IndexRadPhase) % TableName                      = 'radar'
ElemDesp2ODBDesp(IndexRadPhase) % ColName                        = 'radar_phase'
ElemDesp2ODBDesp(IndexRadPhase) % data_type                      = odb_real
ElemDesp2ODBDesp(IndexRadPhase) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexRadPhaseSDP) % TableName                   = 'radar'
ElemDesp2ODBDesp(IndexRadPhaseSDP) % ColName                     = 'radar_phase_sdp'
ElemDesp2ODBDesp(IndexRadPhaseSDP) % data_type                   = odb_real
ElemDesp2ODBDesp(IndexRadPhaseSDP) % has_missing                 = c_false

ElemDesp2ODBDesp(IndexRadClutter) % TableName                    = 'radar'
ElemDesp2ODBDesp(IndexRadClutter) % ColName                      = 'radar_clutter'
ElemDesp2ODBDesp(IndexRadClutter) % data_type                    = odb_real
ElemDesp2ODBDesp(IndexRadClutter) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexStereoFreq) % TableName                    = 'satob'
ElemDesp2ODBDesp(IndexStereoFreq) % ColName                      = 'stereo_frequency'
ElemDesp2ODBDesp(IndexStereoFreq) % data_type                    = odb_real
ElemDesp2ODBDesp(IndexStereoFreq) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexObsSiteId) % TableName                     = 'lidar'
ElemDesp2ODBDesp(IndexObsSiteId) % ColName                       = 'ceilometer_site_identifier'
ElemDesp2ODBDesp(IndexObsSiteId) % data_type                     = odb_integer
ElemDesp2ODBDesp(IndexObsSiteId) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexScan_Ident) % TableName                    = 'lidar'
ElemDesp2ODBDesp(IndexScan_Ident) % ColName                      = 'ceilometer_scan_identifier'
ElemDesp2ODBDesp(IndexScan_Ident) % data_type                    = odb_integer
ElemDesp2ODBDesp(IndexScan_Ident) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexCeil_Firmware) % TableName                 = 'lidar'
ElemDesp2ODBDesp(IndexCeil_Firmware) % ColName                   = 'ceilometer_firmware_identifier'
ElemDesp2ODBDesp(IndexCeil_Firmware) % data_type                 = odb_integer
ElemDesp2ODBDesp(IndexCeil_Firmware) % has_missing               = c_false

ElemDesp2ODBDesp(IndexCeil_WindowTau) % TableName                = 'lidar'
ElemDesp2ODBDesp(IndexCeil_WindowTau) % ColName                  = 'ceilometer_window_transmission'
ElemDesp2ODBDesp(IndexCeil_WindowTau) % data_type                = odb_real
ElemDesp2ODBDesp(IndexCeil_WindowTau) % has_missing              = c_false

ElemDesp2ODBDesp(IndexCeil_LPE) % TableName                      = 'lidar'
ElemDesp2ODBDesp(IndexCeil_LPE) % ColName                        = 'ceilometer_laser_pulse_energy'
ElemDesp2ODBDesp(IndexCeil_LPE) % data_type                      = odb_real
ElemDesp2ODBDesp(IndexCeil_LPE) % has_missing                    = c_false

ElemDesp2ODBDesp(IndexArgoID) % TableName                        = 'conv'
ElemDesp2ODBDesp(IndexArgoID) % ColName                          = 'argo_identifier'
ElemDesp2ODBDesp(IndexArgoID) % data_type                        = odb_integer
ElemDesp2ODBDesp(IndexArgoID) % has_missing                      = c_false

ElemDesp2ODBDesp(IndexAzimuthCOG) % TableName                    = 'hloswind'
ElemDesp2ODBDesp(IndexAzimuthCOG) % ColName                      = 'azimuth_cog'
ElemDesp2ODBDesp(IndexAzimuthCOG) % data_type                    = odb_real
ElemDesp2ODBDesp(IndexAzimuthCOG) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexHeightCOG) % TableName                     = 'hloswind'
ElemDesp2ODBDesp(IndexHeightCOG) % ColName                       = 'height_cog'
ElemDesp2ODBDesp(IndexHeightCOG) % data_type                     = odb_real
ElemDesp2ODBDesp(IndexHeightCOG) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexWindErr) % TableName                       = 'hloswind'
ElemDesp2ODBDesp(IndexWindErr) % ColName                         = 'wind_error'
ElemDesp2ODBDesp(IndexWindErr) % data_type                       = odb_real
ElemDesp2ODBDesp(IndexWindErr) % has_missing                     = c_false

ElemDesp2ODBDesp(IndexProfileNo) % TableName                     = 'hloswind'
ElemDesp2ODBDesp(IndexProfileNo) % ColName                       = 'profile_number'
ElemDesp2ODBDesp(IndexProfileNo) % data_type                     = odb_integer
ElemDesp2ODBDesp(IndexProfileNo) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexdWinddT) % TableName                       = 'hloswind'
ElemDesp2ODBDesp(IndexdWinddT) % ColName                         = 'dwind_dt'
ElemDesp2ODBDesp(IndexdWinddT) % data_type                       = odb_real
ElemDesp2ODBDesp(IndexdWinddT) % has_missing                     = c_false

ElemDesp2ODBDesp(IndexdWinddP) % TableName                       = 'hloswind'
ElemDesp2ODBDesp(IndexdWinddP) % ColName                         = 'dwind_dp'
ElemDesp2ODBDesp(IndexdWinddP) % data_type                       = odb_real
ElemDesp2ODBDesp(IndexdWinddP) % has_missing                     = c_false

ElemDesp2ODBDesp(IndexLidarClass) % TableName                    = 'hloswind'
ElemDesp2ODBDesp(IndexLidarClass) % ColName                      = 'lidar_class'
ElemDesp2ODBDesp(IndexLidarClass) % data_type                    = odb_real
ElemDesp2ODBDesp(IndexLidarClass) % has_missing                  = c_false

ElemDesp2ODBDesp(IndexAladinConfFlag) % TableName                = 'hloswind'
ElemDesp2ODBDesp(IndexAladinConfFlag) % ColName                  = 'aladin_confidence_flag'
ElemDesp2ODBDesp(IndexAladinConfFlag) % data_type                = odb_integer
ElemDesp2ODBDesp(IndexAladinConfFlag) % has_missing              = c_false

ElemDesp2ODBDesp(IndexBLTN_TYPE) % TableName                     = 'conv'
ElemDesp2ODBDesp(IndexBLTN_TYPE) % ColName                       = 'buoy_type'
ElemDesp2ODBDesp(IndexBLTN_TYPE) % data_type                     = odb_integer
ElemDesp2ODBDesp(IndexBLTN_TYPE) % has_missing                   = c_false

ElemDesp2ODBDesp(IndexDyncAtmrCortn) % TableName                 = 'altimeter'
ElemDesp2ODBDesp(IndexDyncAtmrCortn) % ColName                   = 'dynamic_atmosphere_correction'
ElemDesp2ODBDesp(IndexDyncAtmrCortn) % data_type                 = odb_real
ElemDesp2ODBDesp(IndexDyncAtmrCortn) % has_missing               = c_false

ElemDesp2ODBDesp(IndexLongWaveErrr) % TableName                  = 'altimeter'
ElemDesp2ODBDesp(IndexLongWaveErrr) % ColName                    = 'long_wave_error'
ElemDesp2ODBDesp(IndexLongWaveErrr) % data_type                  = odb_real
ElemDesp2ODBDesp(IndexLongWaveErrr) % has_missing                = c_false

ElemDesp2ODBDesp(IndexOcnTide) % TableName                       = 'altimeter'
ElemDesp2ODBDesp(IndexOcnTide) % ColName                         = 'ocean_tide'
ElemDesp2ODBDesp(IndexOcnTide) % data_type                       = odb_real
ElemDesp2ODBDesp(IndexOcnTide) % has_missing                     = c_false

ElemDesp2ODBDesp(IndexArgoPositionFlag) % TableName              = 'hdr'
ElemDesp2ODBDesp(IndexArgoPositionFlag) % ColName                = 'argo_position_qc'
ElemDesp2ODBDesp(IndexArgoPositionFlag) % data_type              = odb_integer
ElemDesp2ODBDesp(IndexArgoPositionFlag) % has_missing            = c_false

ElemDesp2ODBDesp(IndexTimeDifference) % TableName                = 'gnssro'
ElemDesp2ODBDesp(IndexTimeDifference) % ColName                  = 'time_difference'
ElemDesp2ODBDesp(IndexTimeDifference) % data_type                = odb_integer
ElemDesp2ODBDesp(IndexTimeDifference) % has_missing              = c_false

ElemDesp2ODBDesp(IndexModelSurfaceType) % TableName              = 'modsurf'
ElemDesp2ODBDesp(IndexModelSurfaceType) % ColName                = 'totalcloud'
ElemDesp2ODBDesp(IndexModelSurfaceType) % data_type              = odb_real
ElemDesp2ODBDesp(IndexModelSurfaceType) % has_missing            = c_false

CALL OpsODB_InitODBElemDesp_Body (h,        &
                                  obsgroup)

!-------------------------------------------------------------------------------
! 2. Tracing and end of routine
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE OpsODB_InitODBElemDesp
