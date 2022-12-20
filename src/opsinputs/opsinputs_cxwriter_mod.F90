! (C) Crown Copyright 2020, the Met Office. All rights reserved.
!
! Refer to COPYRIGHT.txt of this distribution for details.

!> Fortran part of CxWriter. It collects data from JEDI and uses OPS functions to output these data
!> to a Cx file.

module opsinputs_cxwriter_mod

use datetime_mod, only:        &
    datetime,                  &
    datetime_create,           &
    datetime_delete,           &
    datetime_to_YYYYMMDDhhmmss
use fckit_configuration_module, only: fckit_configuration
use, intrinsic :: iso_c_binding, only: &
    c_bool,                            &
    c_double,                          &
    c_float,                           &
    c_int,                             &
    c_int64_t,                         &
    c_ptr
use obsspace_mod, only: &
    obsspace_get_db,    &
    obsspace_get_nlocs
use oops_variables_mod, only: oops_variables
use opsinputs_cxfields_mod
use opsinputs_fill_mod, only:                 &
    opsinputs_fill_fillreal,                  &
    opsinputs_fill_fillrealfromgeoval,        &
    opsinputs_fill_fillreal2dfromgeoval
use opsinputs_mpl_mod, only: opsinputs_mpl_allgather_integer
use opsinputs_obsspace_mod, only: opsinputs_obsspace_get_db_datetime_offset_in_seconds
use opsinputs_utils_mod, only: &
    max_varname_length,        &
    opsinputs_utils_fillreportflags
use opsinputs_jeditoopslayoutmapping_mod, only:              &
    opsinputs_jeditoopslayoutmapping,                        &
    opsinputs_jeditoopslayoutmapping_create,                 &
    opsinputs_jeditoopslayoutmapping_clear_rejected_records, &
    opsinputs_jeditoopslayoutmapping_delete

use ufo_geovals_mod, only: &
    ufo_geovals

use GenMod_Control, only:   &
    OperationalMode,        &
    QuietMode,              &
    ProductionMode,         &
    NormalMode,             &
    DiagnosticMode,         &
    DebugMode,              &
    VerboseMode,            &
    GeneralMode,            &
    mype,                   &
    nproc
use GenMod_Core, only: &
    gen_warn,          &
    gen_fail
use GenMod_MiscUMScienceConstants, only: &
    IMDI,                                &
    RMDI
use GenMod_ModelIO, only: LenFixHd, UM_header_type
use GenMod_Setup, only: Gen_SetupControl
use GenMod_UMHeaderConstants
use GenMod_Utilities, only: Gen_LatLon_to_Eq
use GenMod_CLookAdd, only: &
    LBYR,                  &
    LBMON,                 &
    LBDAT,                 &
    LBHR,                  &
    LBMIN,                 &
    LBDAY,                 &
    LBSEC,                 &
    LBYRD,                 &
    LBMOND,                &
    LBDATD,                &
    LBHRD,                 &
    LBMIND,                &
    LBDAYD,                &
    LBSECD,                &
    LBTIM,                 &
    LBFT
use OpsMod_Ancil
use OpsMod_AODGeneral, only: NDustBins
use OpsMod_CharUtils, only: ops_to_lower_case
use OpsMod_Constants, only: PPF ! PGE packing factor
use OpsMod_Control, only:   &
    DefaultDocURL,          &
    ModelType,              &
    ModelType_Atmos,        &
    ModelType_Ocean,        &
    ModelType_SST,          &
    mpi_group,              &
    Ops_InitMPI
use OpsMod_CXInfo, only: &
    CXheader_type, &
    CX_type
use opsinputs_cxgenerate_mod, only: &
    MaxModelCodes,                  &
    Ops_ReadCXControlNL
use OpsMod_Kinds, only: &
    integer64,          &
    logical64,          &
    real64
use OpsMod_MiscTypes, only: ElementHeader_Type
use OpsMod_ModelColumnIO, only: Ops_WriteOutVarCx
use OpsMod_ObsGroupInfo, only: &
    OpsFn_ObsGroupNameToNum,   &
    ObsGroupAircraft,          &
    ObsGroupGPSRO,             &
    ObsGroupGroundLidar,       &
    ObsGroupSurface,           &
    ObsGroupSatwind,           &
    ObsGroupScatwind,          &
    ObsGroupSonde
use OpsMod_ObsInfo, only: &
    FinalRejectFlag,      &
    OB_type,              &
    Ops_Alloc,            &
    Ops_SetupObType
use OpsMod_Stash
use OpsMod_Utilities, only: &
     Ops_WCoeff, &
     Ops_WEq_to_ll

implicit none
public :: opsinputs_cxwriter_create, opsinputs_cxwriter_delete, &
          opsinputs_cxwriter_prior, opsinputs_cxwriter_post
private

! ------------------------------------------------------------------------------
type, public :: opsinputs_cxwriter
private
  integer(integer64)                     :: ObsGroup
  type(datetime)                         :: ValidityTime  ! Corresponds to OPS validity time

  logical                                :: RejectObsWithAnyVariableFailingQC
  logical                                :: RejectObsWithAllVariablesFailingQC
  logical                                :: GeoVaLsAreTopToBottom

  character(len=100)                     :: latitudeName
  character(len=100)                     :: longitudeName

  integer(integer64)                     :: FH_VertCoord
  integer(integer64)                     :: FH_HorizGrid
  integer(integer64)                     :: FH_GridStagger
  integer(integer64)                     :: FH_ObsFileType
  integer(integer64)                     :: FH_ModelVersion
  integer(integer64)                     :: FH_SubModel

  integer(integer64)                     :: IC_XLen
  integer(integer64)                     :: IC_YLen
  integer(integer64)                     :: IC_PLevels
  integer(integer64)                     :: IC_WetLevels
  integer(integer64)                     :: IC_FirstConstantRhoLevel

  real(real64)                           :: RC_LongSpacing
  real(real64)                           :: RC_LatSpacing
  real(real64)                           :: RC_FirstLat
  real(real64)                           :: RC_FirstLong
  real(real64)                           :: RC_PoleLat
  real(real64)                           :: RC_PoleLong
  real(real64)                           :: RC_z_ModelTop

  integer(integer64)                     :: TimeIndicator
  integer(integer64)                     :: ForecastPeriod

  real(real64), allocatable              :: EtaTheta(:)
  real(real64), allocatable              :: EtaRho(  :)

  type(ufo_geovals), pointer             :: GeoVals
  type(opsinputs_jeditoopslayoutmapping) :: JediToOpsLayoutMapping
  type(oops_variables)                   :: varnames
  real(c_double), pointer                :: hofx(:, :)
end type opsinputs_cxwriter

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------

!> Set up an instance of opsinputs_cxwriter. Returns .true. on success and .false. on failure.
function opsinputs_cxwriter_create(self, f_conf, geovars)
implicit none

! Subroutine arguments:
type(opsinputs_cxwriter), intent(inout) :: self
type(fckit_configuration), intent(in)   :: f_conf  ! Configuration
type(oops_variables), intent(inout)     :: geovars ! GeoVaLs required by the CxWriter.
logical                                 :: opsinputs_cxwriter_create

! Local declarations:

! Values loaded from the f_conf
character(len=:), allocatable           :: StringValue
integer                                 :: IntValue
real(kind=c_double)                     :: DoubleValue

character(len=*), parameter             :: RoutineName = "opsinputs_cxwriter_create"
character(len=200)                      :: ErrorMessage

! Body:

opsinputs_cxwriter_create = .true.

! Setup OPS

call f_conf % get_or_die("general_mode", StringValue)
select case (ops_to_lower_case(StringValue))
case ("operational")
  GeneralMode = OperationalMode
case ("quiet")
  GeneralMode = QuietMode
case ("production")
  GeneralMode = ProductionMode
case ("normal")
  GeneralMode = NormalMode
case ("diagnostic")
  GeneralMode = DiagnosticMode
case ("debug")
  GeneralMode = DebugMode
case ("verbose")
  GeneralMode = VerboseMode
case default
  write (ErrorMessage, '("GeneralMode code not recognised: ",A)') StringValue
  call gen_warn(RoutineName, ErrorMessage)
  opsinputs_cxwriter_create = .false.
  return
end select

call Gen_SetupControl(DefaultDocURL)
call Ops_InitMPI

! Retrieve parameter values from the input configuration object
! and store them in member variables

if (.not. f_conf % get("obs_group", StringValue)) then
  call gen_warn(RoutineName, "Mandatory obs_group option not found")
  opsinputs_cxwriter_create = .false.
  return
end if
self % ObsGroup = OpsFn_ObsGroupNameToNum(StringValue)

if (.not. f_conf % get("validity_time", StringValue)) then
  call gen_warn(RoutineName, "Mandatory validity_time option not found")
  opsinputs_cxwriter_create = .false.
  return
end if
call datetime_create(StringValue, self % validitytime)

call f_conf % get_or_die("reject_obs_with_any_variable_failing_qc", &
                         self % RejectObsWithAnyVariableFailingQC)

call f_conf % get_or_die("reject_obs_with_all_variables_failing_qc", &
                         self % RejectObsWithAllVariablesFailingQC)

call f_conf % get_or_die("geovals_are_top_to_bottom", &
                         self % GeoVaLsAreTopToBottom)

call f_conf % get_or_die ("latitude_name", StringValue)
self % latitudeName = StringValue

call f_conf % get_or_die ("longitude_name", StringValue)
self % longitudeName = StringValue

call f_conf % get_or_die("FH_VertCoord", StringValue)
select case (ops_to_lower_case(StringValue))
case ("hybrid")
  self % FH_VertCoord = FH_VertCoord_Hybrid
case ("sigma")
  self % FH_VertCoord = FH_VertCoord_Sigma
case ("pressure")
  self % FH_VertCoord = FH_VertCoord_Pressure
case ("depth")
  self % FH_VertCoord = FH_VertCoord_Depth
case ("cp")
  self % FH_VertCoord = FH_VertCoord_CP
case ("wave")
  self % FH_VertCoord = FH_VertCoord_Wave
case default
  write (ErrorMessage, '("FH_VertCoord code not recognised: ",A)') StringValue
  call gen_warn(RoutineName, ErrorMessage)
  opsinputs_cxwriter_create = .false.
  return
end select

call f_conf % get_or_die("FH_HorizGrid", StringValue)
select case (ops_to_lower_case(StringValue))
case ("global")
  self % FH_HorizGrid = FH_HorizGrid_Global
case ("nh")
  self % FH_HorizGrid = FH_HorizGrid_NH
case ("sh")
  self % FH_HorizGrid = FH_HorizGrid_SH
case ("lamnowrap")
  self % FH_HorizGrid = FH_HorizGrid_LamNoWrap
case ("lamwrap")
  self % FH_HorizGrid = FH_HorizGrid_LamWrap
case ("eq")
  self % FH_HorizGrid = FH_HorizGrid_Eq
case ("lamnowrapeq")
  self % FH_HorizGrid = FH_HorizGrid_LamNoWrapEq
case ("lamwrapeq")
  self % FH_HorizGrid = FH_HorizGrid_LamWrapEq
case default
  write (ErrorMessage, '("FH_HorizGrid code not recognised: ",A)') StringValue
  call gen_warn(RoutineName, ErrorMessage)
  opsinputs_cxwriter_create = .false.
  return
end select

call f_conf % get_or_die("FH_GridStagger", StringValue)
select case (ops_to_lower_case(StringValue))
case ("arakawab")
  self % FH_GridStagger = FH_GridStagger_ArakawaB
case ("arakawac")
  self % FH_GridStagger = FH_GridStagger_ArakawaC
case ("endgame")
  self % FH_GridStagger = FH_GridStagger_EndGame
case default
  write (ErrorMessage, '("FH_GridStagger code not recognised: ",A)') StringValue
  call gen_warn(RoutineName, ErrorMessage)
  opsinputs_cxwriter_create = .false.
  return
end select

call f_conf % get_or_die("FH_ObsFileType", StringValue)
select case (ops_to_lower_case(StringValue))
case ("atmos")
  self % FH_ObsFileType = FH_ObsFileType_Atmos
case ("ocean")
  self % FH_ObsFileType = FH_ObsFileType_Ocean
case ("sst")
  self % FH_ObsFileType = FH_ObsFileType_SST
case ("wave")
  self % FH_ObsFileType = FH_ObsFileType_Wave
case default
  write (ErrorMessage, '("FH_ObsFileType code not recognised: ",A)') StringValue
  call gen_warn(RoutineName, ErrorMessage)
  opsinputs_cxwriter_create = .false.
  return
end select

call f_conf % get_or_die("FH_ModelVersion", IntValue)
self % FH_ModelVersion = IntValue

call f_conf % get_or_die("FH_SubModel", StringValue)
select case (ops_to_lower_case(StringValue))
case ("atmos")
  self % FH_SubModel = FH_SubModel_Atmos
case default
  write (ErrorMessage, '("FH_SubModel code not recognised: ",A)') StringValue
  call gen_warn(RoutineName, ErrorMessage)
  opsinputs_cxwriter_create = .false.
  return
end select

call f_conf % get_or_die("IC_XLen", IntValue)
self % IC_XLen = IntValue

call f_conf % get_or_die("IC_YLen", IntValue)
self % IC_YLen = IntValue

call f_conf % get_or_die("IC_PLevels", IntValue)
self % IC_PLevels = IntValue

call f_conf % get_or_die("IC_WetLevels", IntValue)
self % IC_WetLevels = IntValue

call f_conf % get_or_die("IC_FirstConstantRhoLevel", IntValue)
self % IC_FirstConstantRhoLevel = IntValue

call f_conf % get_or_die("RC_LongSpacing", DoubleValue)
self % RC_LongSpacing = DoubleValue

call f_conf % get_or_die("RC_LatSpacing", DoubleValue)
self % RC_LatSpacing = DoubleValue

call f_conf % get_or_die("RC_FirstLat", DoubleValue)
self % RC_FirstLat = DoubleValue

call f_conf % get_or_die("RC_FirstLong", DoubleValue)
self % RC_FirstLong = DoubleValue

call f_conf % get_or_die("RC_PoleLat", DoubleValue)
self % RC_PoleLat = DoubleValue

call f_conf % get_or_die("RC_PoleLong", DoubleValue)
self % RC_PoleLong = DoubleValue

call f_conf % get_or_die("RC_z_ModelTop", DoubleValue)
self % RC_z_ModelTop = DoubleValue

if (f_conf % get("eta_theta_levels", self % EtaTheta)) then
  if (size(self % EtaTheta) /= self % IC_PLevels + 1) then
    call gen_warn(RoutineName, "eta_theta_levels should be a vector of length (IC_PLevels + 1)")
    opsinputs_cxwriter_create = .false.
    return
  end if
else
  allocate(self % EtaTheta(self % IC_PLevels + 1))
  self % EtaTheta = RMDI
end if

if (f_conf % get("eta_rho_levels", self % EtaRho)) then
  if (size(self % EtaRho) /= self % IC_PLevels) then
    call gen_warn(RoutineName, "eta_rho_levels should be a vector of length (IC_PLevels + 1)")
    opsinputs_cxwriter_create = .false.
    return
  end if
else
  allocate(self % EtaRho(self % IC_PLevels))
  self % EtaRho = RMDI
end if

call f_conf % get_or_die("time_indicator", IntValue)
self % TimeIndicator = IntValue

call f_conf % get_or_die("forecast_period", IntValue)
self % ForecastPeriod = IntValue

call f_conf % get_or_die("model_type", StringValue)
select case (ops_to_lower_case(StringValue))
case ("atmos")
  ModelType = ModelType_Atmos
case ("ocean")
  ModelType = ModelType_Ocean
case ("sst")
  ModelType = ModelType_SST
case default
  write (ErrorMessage, '("model_type code not recognised: ",A)') StringValue
  call gen_warn(RoutineName, ErrorMessage)
  opsinputs_cxwriter_create = .false.
  return
end select

call f_conf % get_or_die("num_dust_bins", IntValue)
if (IntValue /= 2 .and. IntValue /= 6) then
  write (ErrorMessage, '("num_dust_bins is ",I0," but must be either 2 or 6")') IntValue
  call gen_warn(RoutineName, ErrorMessage)
  opsinputs_cxwriter_create = .false.
  return
end if
NDustBins = IntValue

! Fill in the list of GeoVaLs that will be needed to populate the requested varfields.
call opsinputs_cxwriter_addrequiredgeovars(self, geovars)

end function opsinputs_cxwriter_create

! ------------------------------------------------------------------------------

!> Destroy an instance of opsinputs_cxwriter.
subroutine opsinputs_cxwriter_delete(self)
implicit none

! Subroutine arguments:
type(opsinputs_cxwriter), intent(inout) :: self

! Body:
call datetime_delete(self % validitytime)

end subroutine opsinputs_cxwriter_delete

! ------------------------------------------------------------------------------

!> Called by the priorFilter() method of the C++ CxWriter object.
!>
!> Set the GeoVals pointer.
subroutine opsinputs_cxwriter_prior(self, ObsSpace, GeoVals)
implicit none

! Subroutine arguments:
type(opsinputs_cxwriter), intent(inout) :: self
type(c_ptr), value, intent(in)          :: ObsSpace
type(ufo_geovals), intent(in), pointer  :: GeoVals

! Body:
self % GeoVals => GeoVals

end subroutine opsinputs_cxwriter_prior

! ------------------------------------------------------------------------------

!> Called by the postFilter() method of the C++ CxWriter object.
!>
!> Write out a Cx file containing varfields derived from JEDI variables.
subroutine opsinputs_cxwriter_post(self, ObsSpace, GeoVals, Flags, varnames, hofx)
implicit none

! Subroutine arguments:
type(opsinputs_cxwriter), intent(inout) :: self
type(c_ptr), value, intent(in)          :: ObsSpace
type(ufo_geovals), intent(in), pointer  :: GeoVals
type(c_ptr), value, intent(in)          :: Flags
type(oops_variables), intent(in)        :: varnames
real(c_double), intent(in), target      :: hofx(:, :)

! Local declarations:
logical                                :: ConvertRecordsToMultilevelObs
type(opsinputs_jeditoopslayoutmapping) :: JediToOpsLayoutMapping

! Body:

self % GeoVals => GeoVals
self % varnames = varnames
self % hofx => hofx

! For sondes, each profile is stored in a separate record of the JEDI ObsSpace, but
! it should be treated as a single (multi-level) ob in the OPS data structures.
! There may be other obs groups requiring similar treatment -- if so, edit the line below.
ConvertRecordsToMultilevelObs = (self % ObsGroup == ObsGroupSonde)

JediToOpsLayoutMapping = opsinputs_jeditoopslayoutmapping_create( &
  ObsSpace, ConvertRecordsToMultilevelObs)

self % JediToOpsLayoutMapping = JediToOpsLayoutMapping

call opsinputs_cxwriter_post_internal(self, ObsSpace, Flags)

call opsinputs_jeditoopslayoutmapping_delete(self % JediToOpsLayoutMapping)

self % hofx => null()

end subroutine opsinputs_cxwriter_post

! ------------------------------------------------------------------------------

!> Core implementation of opsinputs_cxwriter_post.
!>
!> This code has been extracted to a separate subroutine to make it possible to declare Retained as
!> an automatic array (since the number of observations is now known).
subroutine opsinputs_cxwriter_post_internal(self, ObsSpace, Flags)
USE mpl, ONLY: &
          gc_int_kind, mpl_integer
implicit none

! Subroutine arguments:
type(opsinputs_cxwriter), intent(inout)               :: self
type(c_ptr), value, intent(in)                        :: ObsSpace
type(c_ptr), value, intent(in)                        :: Flags

! Local declarations:
type(OB_type)                                         :: Ob
type(CX_type)                                         :: Cx
type(UM_header_type)                                  :: UMHeader
integer(integer64)                                    :: NumObsOnEachRank(nproc)
logical(logical64)                                    :: Retained(self % JediToOpsLayoutMapping % NumOpsObs)
integer(kind=gc_int_kind)                             :: istat
integer(integer64)                                    :: ReportFlags(self % JediToOpsLayoutMapping % NumOpsObs)

! Body:

call opsinputs_cxwriter_initialiseobservations(self, Ob, ObsSpace)
call opsinputs_cxwriter_initialisecx(self, Ob, Cx)
call opsinputs_utils_fillreportflags(self % JediToOpsLayoutMapping, ObsSpace, Flags, &
                                     self % RejectObsWithAnyVariableFailingQC, &
                                     self % RejectObsWithAllVariablesFailingQC, ReportFlags)
call opsinputs_cxwriter_populatecx(self, ReportFlags, Cx)
call opsinputs_cxwriter_unrotatewinds(self, Ob, Cx)
call opsinputs_cxwriter_populateumheader(self, UMHeader)

Retained = opsinputs_cxwriter_retainflag(self, ReportFlags)
call opsinputs_mpl_allgather_integer([self % JediToOpsLayoutMapping % NumOpsObs], 1_gc_int_kind, mpl_integer, &
                                     NumObsOnEachRank, 1_gc_int_kind, mpl_integer, &
                                     mpi_group, istat)
call Ops_WriteOutVarCx(Ob, Cx, NumObsOnEachRank, UMheader % IntC(IC_Plevels), UMheader, Retained)

call UMheader % dealloc()
call Cx % deallocate()
call Ob % deallocate()

end subroutine opsinputs_cxwriter_post_internal

! ------------------------------------------------------------------------------

!> Populate the list of GeoVaLs needed to fill in any requested cxfields.
subroutine opsinputs_cxwriter_addrequiredgeovars(self, geovars)
implicit none

! Subroutine arguments:
type(opsinputs_cxwriter), intent(in) :: self
type(oops_variables), intent(inout)  :: geovars

! Local declarations:
integer(integer64)                   :: CxFields(MaxModelCodes), CxField
integer                              :: i
character(len=max_varname_length)    :: GeoVarName
integer                              :: DustBinIndex
character                            :: DustBinIndexStr

! Body:
call Ops_ReadCXControlNL(self % obsgroup, CxFields, BGECall = .false._8, ops_call = .false._8)

do i = 1, size(CxFields)
  CxField = CxFields(i)
  GeoVarName = opsinputs_cxfields_unknown

  select case (CxField)

    ! Surface variables

    case (StashItem_Orog) ! IndexCxorog
      GeoVarName = opsinputs_cxfields_Orog
    case (StashItem_Pstar, StashItem_P_surface) ! IndexCxpstar
      GeoVarName = opsinputs_cxfields_pstar
    case (StashCode_t2) ! IndexCxt2
      GeoVarName = opsinputs_cxfields_t2
    case (StashCode_rh2) ! IndexCxrh2
      GeoVarName = opsinputs_cxfields_rh2
    case (StashCode_u10, StashCode_U10_B_grid) ! IndexCxu10
      GeoVarName = opsinputs_cxfields_u10
    case (StashCode_v10, StashCode_V10_B_grid) ! IndexCxv10
      GeoVarName = opsinputs_cxfields_v10
    case (StashCode_vis) ! IndexCxvis
      GeoVarName = opsinputs_cxfields_vis
    case (StashCode_WAVE_HGHT) ! IndexCxWAVE_HGHT
      GeoVarName = opsinputs_cxfields_WAVE_HGHT
    case (StashCode_WIND_SPED) ! IndexCxWIND_SPED
      GeoVarName = opsinputs_cxfields_WIND_SPED
    case (AncilCode_SeaHeight) ! IndexCxSeaHeight
      GeoVarName = opsinputs_cxfields_SeaHeight
    case (StashItem_SST) ! IndexCxTskinSea
      GeoVarName = opsinputs_cxfields_TskinSea
    ! wsmigaj: I haven't been able to identify the stash code associated with this field
    !  GeoVarName = opsinputs_cxfields_TropPres
    case (StashCode_pmsl) ! IndexCxpmsl
      GeoVarName = opsinputs_cxfields_pmsl
    case (StashItem_SeaIce) ! IndexCxSeaIce
      GeoVarName = opsinputs_cxfields_SeaIce
    case (StashItem_SnowAmount) ! IndexCxSnowAmount
      GeoVarName = opsinputs_cxfields_SnowAmount
    case (StashCode_qt2) ! IndexCxqt2
      GeoVarName = opsinputs_cxfields_qt2
    ! wsmigaj: I haven't been able to identify the stash code associated with this field
    !  GeoVarName = opsinputs_cxfields_aerosol
    case (StashCode_PsurfParamA) ! IndexCxPsurfParamA
      GeoVarName = opsinputs_cxfields_PSurfParamA
    case (StashCode_PSurfParamB) ! IndexCxPSurfParamB
      GeoVarName = opsinputs_cxfields_PSurfParamB
    case (StashCode_LapseRate) ! IndexCxLapseRate
      GeoVarName = opsinputs_cxfields_LapseRate
    case (StashCode_CloudAmount) ! IndexCxCloudAmount
      GeoVarName = opsinputs_cxfields_CloudAmount
    case (StashItem_ConvCloudAmount) ! IndexCxConvCloudAmount
      GeoVarName = opsinputs_cxfields_ConvCloudAmount
    case (StashItem_ConvCloudBase) ! IndexCxConvCloudBaseLevel
      GeoVarName = opsinputs_cxfields_ConvCloudBaseLevel
    case (StashItem_ConvCloudTop) ! IndexCxConvCloudTopLevel
      GeoVarName = opsinputs_cxfields_ConvCloudTopLevel
    case (StashCode_SurfRainRate_conv) ! IndexCxSurfRainRate_conv
      GeoVarName = opsinputs_cxfields_SurfRainRate_conv
    case (StashCode_SurfSnowRate_conv) ! IndexCxSurfSnowRate_conv
      GeoVarName = opsinputs_cxfields_SurfSnowRate_conv
    case (AncilCode_SeaSrfcHeight) ! IndexCxSeaSrfcHeight
      GeoVarName = opsinputs_cxfields_SeaSrfcHeight
    case (AncilCode_MeanSeaHeight) ! IndexCxMeanSeaHeight
      GeoVarName = opsinputs_cxfields_MeanSeaHeight
    case (StashCode_SurfRainRate_LS) ! IndexCxSurfRainRate_LS
      GeoVarName = opsinputs_cxfields_SurfRainRate_LS
    case (StashCode_SurfSnowRate_LS) ! IndexCxSurfSnowRate_LS
      GeoVarName = opsinputs_cxfields_SurfSnowRate_LS
    case (StashCode_SWradiation) ! IndexCxSWradiation
      GeoVarName = opsinputs_cxfields_SWradiation
    case (StashItem_BLheight) ! IndexCxBLheight
      GeoVarName = opsinputs_cxfields_BLheight
    case (StashCode_ObukhovLength) ! IndexCxObukhovLength
      GeoVarName = opsinputs_cxfields_ObukhovLength
    case (StashCode_FrictionVel) ! IndexCxFrictionVel
      GeoVarName = opsinputs_cxfields_FrictionVel
    case (StashCode_PrecipAcc6hr) ! IndexCxPrecipAcc6hr
      GeoVarName = opsinputs_cxfields_PrecipAcc6hr
    case (StashCode_LowCloudAmount) ! IndexCxLowCloudAmount
      GeoVarName = opsinputs_cxfields_LowCloudAmount
    case (StashCode_MedCloudAmount) ! IndexCxMedCloudAmount
      GeoVarName = opsinputs_cxfields_MedCloudAmount
    case (StashCode_LowCloudBase, StashCode_2p5CloudBase) ! IndexCxLowCloudBase
      GeoVarName = opsinputs_cxfields_LowCloudBase
    case (StashCode_SO2_AQ) ! IndexCxSO2_AQ
      GeoVarName = opsinputs_cxfields_SO2_AQ
    case (StashCode_PM10_AQ) ! IndexCxPM10_AQ
      GeoVarName = opsinputs_cxfields_PM10_AQ
    case (StashCode_PM2p5_AQ) ! IndexCxPM2p5_AQ
      GeoVarName = opsinputs_cxfields_PM2p5_AQ
    case (StashCode_O3_AQ) ! IndexCxO3_AQ
      GeoVarName = opsinputs_cxfields_O3_AQ
    case (StashCode_NO2_AQ) ! IndexCxNO2_AQ
      GeoVarName = opsinputs_cxfields_NO2_AQ
    case (StashCode_CO_AQ) ! IndexCxCO_AQ
      GeoVarName = opsinputs_cxfields_CO_AQ
    case (StashCode_BLtype) ! IndexCxBLtype
      GeoVarName = opsinputs_cxfields_BLtype

    ! Upper-air variables
    case (StashItem_theta) ! IndexCxtheta
      GeoVarName = opsinputs_cxfields_theta
    case (StashCode_rh, StashCode_rh_p) ! IndexCxrh
      GeoVarName = opsinputs_cxfields_rh
    case (StashItem_u, StashCode_u_p_B_grid) ! IndexCxu
      GeoVarName = opsinputs_cxfields_u
    case (StashItem_v) ! IndexCxv
      GeoVarName = opsinputs_cxfields_v
    case (StashItem_w) ! IndexCxw
      GeoVarName = opsinputs_cxfields_w
    case (StashItem_q) ! IndexCxq
      GeoVarName = opsinputs_cxfields_q
    case (StashItem_qc) ! IndexCxqc
      GeoVarName = opsinputs_cxfields_qc
    case (StashItem_p_bar) ! IndexCxp_bar
      GeoVarName = opsinputs_cxfields_p_bar
    ! wsmigaj: I haven't been able to identify the stash code associated with this field
    !  GeoVarName = opsinputs_cxfields_cloud
    case (StashCode_ql_layer) ! IndexCxql_layer
      GeoVarName = opsinputs_cxfields_ql_layer
    case (StashItem_p) ! IndexCxP
      GeoVarName = opsinputs_cxfields_PLevelsA
    case (AncilCode_salt) ! IndexCxSalt
      GeoVarName = opsinputs_cxfields_Salt
    case (StashCode_t_p) ! IndexCxt
      GeoVarName = opsinputs_cxfields_t
    case (StashCode_qf_layer) ! IndexCxqf_layer
      GeoVarName = opsinputs_cxfields_qf_layer
    case (StashCode_RainRate_layer) ! IndexCxRainRate_layer
      GeoVarName = opsinputs_cxfields_RainRate_layer
    case (StashCode_cloud_conv) ! IndexCxcloud_conv
      GeoVarName = opsinputs_cxfields_cloud_conv
    case (StashCode_qc_conv) ! IndexCxqc_conv
      GeoVarName = opsinputs_cxfields_qc_conv
    case (StashCode_cloud_layer) ! IndexCxcloud_layer
      GeoVarName = opsinputs_cxfields_cloud_layer
    case (StashItem_ozone_new) ! IndexCxOzone
      GeoVarName = opsinputs_cxfields_ozone
    case (StashItem_qcf) ! IndexCxqcf
      GeoVarName = opsinputs_cxfields_qcf
    case (StashItem_qcl) ! IndexCxqcl
      GeoVarName = opsinputs_cxfields_qcl
    case (StashItem_cloud_bulk) ! IndexCxcloud_bulk
      GeoVarName = opsinputs_cxfields_cloud_bulk
    ! wsmigaj: I haven't been able to identify the stash code associated with this field
    !  GeoVarName = opsinputs_cxfields_aerosol_p
    case (StashCode_CDNC) ! IndexCxCDNC
      GeoVarName = opsinputs_cxfields_CDNC
    case (StashCode_RH_AfterMainCloud) ! IndexCxRH_AMC
      GeoVarName = opsinputs_cxfields_RH_AMC
    case (StashItem_Cl) ! IndexCxCl
      GeoVarName = opsinputs_cxfields_Cl
    case (StashItem_Cf) ! IndexCxCf
      GeoVarName = opsinputs_cxfields_Cf
    case (StashItem_qrain) ! IndexCxqrain
      GeoVarName = opsinputs_cxfields_qrain
    case (StashItem_Exner) ! IndexCxExnerA
      GeoVarName = opsinputs_cxfields_ExnerA
    case (StashCode_RichNumber) ! IndexCxRichNumber
      GeoVarName = opsinputs_cxfields_RichNumber
    case (StashCode_SoilMoisture) ! IndexCxSoilMoisture
      GeoVarName = opsinputs_cxfields_SoilMoisture
    case (StashCode_SoilTemp) ! IndexCxSoilTemp
      GeoVarName = opsinputs_cxfields_SoilTemp
    case (StashItem_dustMin:StashItem_dustMax) ! IndexCxDust1:6
      DustBinIndex = CxField - StashItem_dustMin + 1
      if (DustBinIndex <= NDustBins) then
        write (DustBinIndexStr, '(i1)') DustBinIndex
        GeoVarName = opsinputs_cxfields_dustp_start // DustBinIndexStr // opsinputs_cxfields_dustp_end
      end if

  end select
  
  if (GeoVarName /= opsinputs_cxfields_unknown) then
    call geovars % push_back(GeoVarName)
  end if
end do

end subroutine opsinputs_cxwriter_addrequiredgeovars

! ------------------------------------------------------------------------------

!> Fill components of the Ob structure required by the OPS subroutine writing a Cx file.
!>
!> This includes the number of observations and the validity time.
subroutine opsinputs_cxwriter_initialiseobservations(self, Ob, ObsSpace)
use mpl, ONLY: gc_int_kind
implicit none

! Subroutine arguments:
type(opsinputs_cxwriter), intent(in) :: self
type(OB_type), intent(inout)         :: Ob
type(c_ptr), value, intent(in)       :: ObsSpace

! Local declarations:
integer(kind=gc_int_kind)            :: istat
integer(c_int)                       :: year, month, day, hour, minute, second

! Body:
Ob % Header % obsgroup = self % obsgroup

call Ops_SetupObType(Ob)

Ob % Header % NumObsLocal = self % JediToOpsLayoutMapping % NumOpsObs
Ob % Header % NumObsTotal = Ob % Header % NumObsLocal
! Sum the number of local observations on each process into NumObsTotal
call gcg_isum (1_gc_int_kind, mpi_group, istat, Ob % Header % NumObsTotal)

Ob % Header % NumCXBatches = 1
allocate(Ob % Header % ObsPerBatchPerPE(Ob % Header % NumCXBatches, 0:nproc - 1))
Ob % Header % ObsPerBatchPerPE(1,mype) = Ob % Header % NumObsLocal

call datetime_to_YYYYMMDDhhmmss(self % ValidityTime, year, month, day, hour, minute, second)
Ob % Header % ValidityTime % year = year
Ob % Header % ValidityTime % month = month
Ob % Header % ValidityTime % day = day
Ob % Header % ValidityTime % hour = hour
Ob % Header % ValidityTime % minute = minute
Ob % Header % ValidityTime % second = second
! util::DateTime is represented internally as UTC quantized to the nearest second.
Ob % Header % ValidityTime % diff_from_utc = 0

! Retrieve observation latitude and longitude
call opsinputs_fill_fillreal(Ob % Header % Latitude, "Latitude", self % JediToOpsLayoutMapping, &
  Ob % Latitude, ObsSpace, trim(self % latitudeName), "MetaData")
call opsinputs_fill_fillreal(Ob % Header % Longitude, "Longitude", self % JediToOpsLayoutMapping, &
  Ob % Longitude, ObsSpace, trim(self % longitudeName), "MetaData")

end subroutine opsinputs_cxwriter_initialiseobservations

! ------------------------------------------------------------------------------

!> Prepare Cx to hold the required number of model columns and set properties unrelated to any
!> specific cxfield.
subroutine opsinputs_cxwriter_initialisecx(self, Ob, Cx)
implicit none

! Subroutine arguments:
type(opsinputs_cxwriter), intent(in)    :: self
type(OB_type), intent(in)               :: Ob
type(CX_type), intent(inout)            :: Cx

! Body:

Cx % Header % obsgroup = self % obsgroup
call Cx % init

! The following code is inspired by Ops_CXSetup.

Cx % Header % NumLocal = Ob % Header % NumObsLocal
Cx % Header % NumTotal = Ob % Header % NumObsTotal
Cx % Header % ModelVersion = self % FH_ModelVersion
Cx % Header % SubModel = self % FH_SubModel
Cx % Header % NewDynamics = self % FH_ModelVersion >= 500 .and. ModelType /= ModelType_Ocean

if (Cx % Header % NewDynamics .and. ModelType /= ModelType_SST) then
  Cx % Header % FirstConstantRhoLevel = self % IC_FirstConstantRhoLevel
else
  Cx % Header % FirstConstantRhoLevel = IMDI
end if

! The following code is inspired by Ops_CXComplete

Cx % Header % Rotated = self % FH_HorizGrid > FH_HorizGrid_Eq

end subroutine opsinputs_cxwriter_initialisecx

! ------------------------------------------------------------------------------

!> Populate Cx fields required by the OPS routine writing a Cx file.
subroutine opsinputs_cxwriter_populatecx(self, ReportFlags, Cx)
implicit none

! Subroutine arguments:
type(opsinputs_cxwriter), intent(inout)               :: self
integer(integer64), intent(in)                        :: ReportFlags(self % JediToOpsLayoutMapping % NumOpsObs)
type(CX_type), intent(inout)                          :: Cx

! Local declarations:
character(len=*), parameter             :: RoutineName = "opsinputs_cxwriter_populatecx"
character(len=80)                       :: ErrorMessage

integer(integer64)                      :: CxFields(MaxModelCodes), CxField
integer                                 :: iCxField
integer                                 :: DustBinIndex
character                               :: DustBinIndexStr
integer                                 :: irh, ivar



! Body:
call Ops_ReadCXControlNL(self % obsgroup, CxFields, BGECall = .false._8, ops_call = .false._8)

! Reject any observations that have been flagged.
if (self % JediToOpsLayoutMapping % ConvertRecordsToMultilevelObs) then
  call opsinputs_jeditoopslayoutmapping_clear_rejected_records( &
    ReportFlags, self % JediToOpsLayoutMapping)
end if

do iCxField = 1, size(CxFields)
  CxField = CxFields(iCxField)
  select case (CxField)

    ! Surface variables

    case (StashItem_Orog) ! IndexCxorog
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % Orog, "Orog", Cx % Header % NumLocal, Cx % Orog, &
        self % GeoVals, opsinputs_cxfields_Orog, self % JediToOpsLayoutMapping)
    case (StashItem_Pstar, StashItem_P_surface) ! IndexCxpstar
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % pstar, "pstar", Cx % Header % NumLocal, Cx % pstar, &
        self % GeoVals, opsinputs_cxfields_pstar, self % JediToOpsLayoutMapping)
    case (StashCode_t2) ! IndexCxt2
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % t2, "t2", Cx % Header % NumLocal, Cx % t2, &
        self % GeoVals, opsinputs_cxfields_t2, self % JediToOpsLayoutMapping)
    case (StashCode_rh2) ! IndexCxrh2
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % rh2, "rh2", Cx % Header % NumLocal, Cx % rh2, &
        self % GeoVals, opsinputs_cxfields_rh2, self % JediToOpsLayoutMapping)
    case (StashCode_u10, StashCode_U10_B_grid) ! IndexCxu10
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % u10, "u10", Cx % Header % NumLocal, Cx % u10, &
        self % GeoVals, opsinputs_cxfields_u10, self % JediToOpsLayoutMapping)
    case (StashCode_v10, StashCode_V10_B_grid) ! IndexCxv10
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % v10, "v10", Cx % Header % NumLocal, Cx % v10, &
        self % GeoVals, opsinputs_cxfields_v10, self % JediToOpsLayoutMapping)
    case (StashCode_vis) ! IndexCxvis
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % vis, "vis", Cx % Header % NumLocal, Cx % vis, &
        self % GeoVals, opsinputs_cxfields_vis, self % JediToOpsLayoutMapping)
    case (StashCode_WAVE_HGHT) ! IndexCxWAVE_HGHT
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % WAVE_HGHT, "WAVE_HGHT", Cx % Header % NumLocal, Cx % WAVE_HGHT, &
        self % GeoVals, opsinputs_cxfields_WAVE_HGHT, self % JediToOpsLayoutMapping)
    case (StashCode_WIND_SPED) ! IndexCxWIND_SPED
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % WIND_SPED, "WIND_SPED", Cx % Header % NumLocal, Cx % WIND_SPED, &
        self % GeoVals, opsinputs_cxfields_WIND_SPED, self % JediToOpsLayoutMapping)
    case (AncilCode_SeaHeight) ! IndexCxSeaHeight
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % SeaHeight, "SeaHeight", Cx % Header % NumLocal, Cx % SeaHeight, &
        self % GeoVals, opsinputs_cxfields_SeaHeight, self % JediToOpsLayoutMapping)
    case (StashItem_SST) ! IndexCxTskinSea
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % TskinSea, "TskinSea", Cx % Header % NumLocal, Cx % TskinSea, &
        self % GeoVals, opsinputs_cxfields_TskinSea, self % JediToOpsLayoutMapping)
    ! wsmigaj: I haven't been able to identify the stash code associated with this field
    ! case (?) ! IndexCxTropPres
      !  call opsinputs_fill_fillrealfromgeoval( &
      !    Cx % Header % TropPres, "TropPres", Cx % Header % NumLocal, Cx % TropPres, &
      !    self % GeoVals, opsinputs_cxfields_TropPres, self % JediToOpsLayoutMapping)
    case (StashCode_pmsl) ! IndexCxpmsl
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % pmsl, "pmsl", Cx % Header % NumLocal, Cx % pmsl, &
        self % GeoVals, opsinputs_cxfields_pmsl, self % JediToOpsLayoutMapping)
    case (StashItem_SeaIce) ! IndexCxSeaIce
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % SeaIce, "SeaIce", Cx % Header % NumLocal, Cx % SeaIce, &
        self % GeoVals, opsinputs_cxfields_SeaIce, self % JediToOpsLayoutMapping)
    case (StashItem_SnowAmount) ! IndexCxSnowAmount
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % SnowAmount, "SnowAmount", Cx % Header % NumLocal, Cx % SnowAmount, &
        self % GeoVals, opsinputs_cxfields_SnowAmount, self % JediToOpsLayoutMapping)
    case (StashCode_qt2) ! IndexCxqt2
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % qt2, "qt2", Cx % Header % NumLocal, Cx % qt2, &
        self % GeoVals, opsinputs_cxfields_qt2, self % JediToOpsLayoutMapping)
    ! wsmigaj: I haven't been able to identify the stash code associated with this field
    ! case (?) ! IndexCxaerosol
      !  if (Cx % Header % ObsGroup == ObsGroupSurface) then
      !    call opsinputs_fill_fillrealfromgeoval( &
      !      Cx % Header % aerosol, "aerosol", Cx % Header % NumLocal, Cx % aerosol, &
      !      self % GeoVals, opsinputs_cxfields_aerosol, self % JediToOpsLayoutMapping)
      !  end if
    case (StashCode_PsurfParamA) ! IndexCxPsurfParamA
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % PSurfParamA, "PSurfParamA", Cx % Header % NumLocal, Cx % PSurfParamA, &
        self % GeoVals, opsinputs_cxfields_PSurfParamA, self % JediToOpsLayoutMapping)
    case (StashCode_PSurfParamB) ! IndexCxPSurfParamB
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % PSurfParamB, "PSurfParamB", Cx % Header % NumLocal, Cx % PSurfParamB, &
        self % GeoVals, opsinputs_cxfields_PSurfParamB, self % JediToOpsLayoutMapping)
    case (StashCode_LapseRate) ! IndexCxLapseRate
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % LapseRate, "LapseRate", Cx % Header % NumLocal, Cx % LapseRate, &
        self % GeoVals, opsinputs_cxfields_LapseRate, self % JediToOpsLayoutMapping)
    case (StashCode_CloudAmount) ! IndexCxCloudAmount
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % CloudAmount, "CloudAmount", Cx % Header % NumLocal, Cx % CloudAmount, &
        self % GeoVals, opsinputs_cxfields_CloudAmount, self % JediToOpsLayoutMapping)
    case (StashItem_ConvCloudAmount) ! IndexCxConvCloudAmount
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % ConvCloudAmount, "ConvCloudAmount", Cx % Header % NumLocal, Cx % ConvCloudAmount, &
        self % GeoVals, opsinputs_cxfields_ConvCloudAmount, self % JediToOpsLayoutMapping)
    case (StashItem_ConvCloudBase) ! IndexCxConvCloudBaseLevel
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % ConvCloudBaseLevel, "ConvCloudBaseLevel", Cx % Header % NumLocal, Cx % ConvCloudBaseLevel, &
        self % GeoVals, opsinputs_cxfields_ConvCloudBaseLevel, self % JediToOpsLayoutMapping)
    case (StashItem_ConvCloudTop) ! IndexCxConvCloudTopLevel
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % ConvCloudTopLevel, "ConvCloudTopLevel", Cx % Header % NumLocal, Cx % ConvCloudTopLevel, &
        self % GeoVals, opsinputs_cxfields_ConvCloudTopLevel, self % JediToOpsLayoutMapping)
    case (StashCode_SurfRainRate_conv) ! IndexCxSurfRainRate_conv
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % SurfRainRate_conv, "SurfRainRate_conv", Cx % Header % NumLocal, Cx % SurfRainRate_conv, &
        self % GeoVals, opsinputs_cxfields_SurfRainRate_conv, self % JediToOpsLayoutMapping)
    case (StashCode_SurfSnowRate_conv) ! IndexCxSurfSnowRate_conv
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % SurfSnowRate_conv, "SurfSnowRate_conv", Cx % Header % NumLocal, Cx % SurfSnowRate_conv, &
        self % GeoVals, opsinputs_cxfields_SurfSnowRate_conv, self % JediToOpsLayoutMapping)
    case (AncilCode_SeaSrfcHeight) ! IndexCxSeaSrfcHeight
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % SeaSrfcHeight, "SeaSrfcHeight", Cx % Header % NumLocal, Cx % SeaSrfcHeight, &
        self % GeoVals, opsinputs_cxfields_SeaSrfcHeight, self % JediToOpsLayoutMapping)
    case (AncilCode_MeanSeaHeight) ! IndexCxMeanSeaHeight
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % MeanSeaHeight, "MeanSeaHeight", Cx % Header % NumLocal, Cx % MeanSeaHeight, &
        self % GeoVals, opsinputs_cxfields_MeanSeaHeight, self % JediToOpsLayoutMapping)
    case (StashCode_SurfRainRate_LS) ! IndexCxSurfRainRate_LS
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % SurfRainRate_LS, "SurfRainRate_LS", Cx % Header % NumLocal, Cx % SurfRainRate_LS, &
        self % GeoVals, opsinputs_cxfields_SurfRainRate_LS, self % JediToOpsLayoutMapping)
    case (StashCode_SurfSnowRate_LS) ! IndexCxSurfSnowRate_LS
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % SurfSnowRate_LS, "SurfSnowRate_LS", Cx % Header % NumLocal, Cx % SurfSnowRate_LS, &
        self % GeoVals, opsinputs_cxfields_SurfSnowRate_LS, self % JediToOpsLayoutMapping)
    case (StashCode_SWradiation) ! IndexCxSWradiation
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % SWradiation, "SWradiation", Cx % Header % NumLocal, Cx % SWradiation, &
        self % GeoVals, opsinputs_cxfields_SWradiation, self % JediToOpsLayoutMapping)
    case (StashItem_BLheight) ! IndexCxBLheight
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % BLheight, "BLheight", Cx % Header % NumLocal, Cx % BLheight, &
        self % GeoVals, opsinputs_cxfields_BLheight, self % JediToOpsLayoutMapping)
    case (StashCode_ObukhovLength) ! IndexCxObukhovLength
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % ObukhovLength, "ObukhovLength", Cx % Header % NumLocal, Cx % ObukhovLength, &
        self % GeoVals, opsinputs_cxfields_ObukhovLength, self % JediToOpsLayoutMapping)
    case (StashCode_FrictionVel) ! IndexCxFrictionVel
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % FrictionVel, "FrictionVel", Cx % Header % NumLocal, Cx % FrictionVel, &
        self % GeoVals, opsinputs_cxfields_FrictionVel, self % JediToOpsLayoutMapping)
    case (StashCode_PrecipAcc6hr) ! IndexCxPrecipAcc6hr
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % PrecipAcc6hr, "PrecipAcc6hr", Cx % Header % NumLocal, Cx % PrecipAcc6hr, &
        self % GeoVals, opsinputs_cxfields_PrecipAcc6hr, self % JediToOpsLayoutMapping)
    case (StashCode_LowCloudAmount) ! IndexCxLowCloudAmount
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % LowCloudAmount, "LowCloudAmount", Cx % Header % NumLocal, Cx % LowCloudAmount, &
        self % GeoVals, opsinputs_cxfields_LowCloudAmount, self % JediToOpsLayoutMapping)
    case (StashCode_MedCloudAmount) ! IndexCxMedCloudAmount
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % MedCloudAmount, "MedCloudAmount", Cx % Header % NumLocal, Cx % MedCloudAmount, &
        self % GeoVals, opsinputs_cxfields_MedCloudAmount, self % JediToOpsLayoutMapping)
    case (StashCode_LowCloudBase, StashCode_2p5CloudBase) ! IndexCxLowCloudBase
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % LowCloudBase, "LowCloudBase", Cx % Header % NumLocal, Cx % LowCloudBase, &
        self % GeoVals, opsinputs_cxfields_LowCloudBase, self % JediToOpsLayoutMapping)
    case (StashCode_SO2_AQ) ! IndexCxSO2_AQ
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % SO2_AQ, "SO2_AQ", Cx % Header % NumLocal, Cx % SO2_AQ, &
        self % GeoVals, opsinputs_cxfields_SO2_AQ, self % JediToOpsLayoutMapping)
    case (StashCode_PM10_AQ) ! IndexCxPM10_AQ
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % PM10_AQ, "PM10_AQ", Cx % Header % NumLocal, Cx % PM10_AQ, &
        self % GeoVals, opsinputs_cxfields_PM10_AQ, self % JediToOpsLayoutMapping)
    case (StashCode_PM2p5_AQ) ! IndexCxPM2p5_AQ
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % PM2p5_AQ, "PM2p5_AQ", Cx % Header % NumLocal, Cx % PM2p5_AQ, &
        self % GeoVals, opsinputs_cxfields_PM2p5_AQ, self % JediToOpsLayoutMapping)
    case (StashCode_O3_AQ) ! IndexCxO3_AQ
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % O3_AQ, "O3_AQ", Cx % Header % NumLocal, Cx % O3_AQ, &
        self % GeoVals, opsinputs_cxfields_O3_AQ, self % JediToOpsLayoutMapping)
    case (StashCode_NO2_AQ) ! IndexCxNO2_AQ
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % NO2_AQ, "NO2_AQ", Cx % Header % NumLocal, Cx % NO2_AQ, &
        self % GeoVals, opsinputs_cxfields_NO2_AQ, self % JediToOpsLayoutMapping)
    case (StashCode_CO_AQ) ! IndexCxCO_AQ
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % CO_AQ, "CO_AQ", Cx % Header % NumLocal, Cx % CO_AQ, &
        self % GeoVals, opsinputs_cxfields_CO_AQ, self % JediToOpsLayoutMapping)
    case (StashCode_BLtype) ! IndexCxBLtype
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % BLtype, "BLtype", Cx % Header % NumLocal, Cx % BLtype, &
        self % GeoVals, opsinputs_cxfields_BLtype, self % JediToOpsLayoutMapping)

    ! Upper-air variables
    case (StashItem_theta) ! IndexCxtheta
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % theta, "theta", Cx % theta, &
        self % GeoVals, self % GeoVaLsAreTopToBottom, opsinputs_cxfields_theta, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashCode_rh, StashCode_rh_p) ! IndexCxrh
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % rh, "rh", Cx % rh, &
        self % GeoVaLs, self % GeoVaLsAreTopToBottom, opsinputs_cxfields_rh, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashItem_u, StashCode_u_p_B_grid) ! IndexCxu
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % u, "u", Cx % u, &
        self % GeoVals, self % GeoVaLsAreTopToBottom, opsinputs_cxfields_u, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashItem_v) ! IndexCxv
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % v, "v", Cx % v, &
        self % GeoVals, self % GeoVaLsAreTopToBottom, opsinputs_cxfields_v, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashItem_w) ! IndexCxw
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % w, "w", Cx % w, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_w, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashItem_q) ! IndexCxq
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % q, "q", Cx % q, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_q, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashItem_qc) ! IndexCxqc
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % qc, "qc", Cx % qc, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_qc, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashItem_p_bar) ! IndexCxp_bar
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % p_bar, "p_bar", Cx % p_bar, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_p_bar, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    ! wsmigaj: I haven't been able to identify the stash code associated with this field
    ! case (?) ! IndexCxcloud
      !  call opsinputs_fill_fillreal2dfromgeoval( &
      !    Cx % Header % cloud, "cloud", Cx % cloud, &
      !    self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_cloud, &
      !    self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashCode_ql_layer) ! IndexCxql_layer
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % ql_layer, "ql_layer", Cx % ql_layer, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_ql_layer, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashItem_p) ! IndexCxP
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % PLevelsA, "PLevelsA", Cx % PLevelsA, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_PLevelsA, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (AncilCode_salt) ! IndexCxSalt
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % Salt, "Salt", Cx % Salt, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_Salt, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashCode_t_p) ! IndexCxt
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % t, "t", Cx % t, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_t, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashCode_qf_layer) ! IndexCxqf_layer
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % qf_layer, "qf_layer", Cx % qf_layer, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_qf_layer, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashCode_RainRate_layer) ! IndexCxRainRate_layer
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % RainRate_layer, "RainRate_layer", Cx % RainRate_layer, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_RainRate_layer, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashCode_cloud_conv) ! IndexCxcloud_conv
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % cloud_conv, "cloud_conv", Cx % cloud_conv, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_cloud_conv, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashCode_qc_conv) ! IndexCxqc_conv
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % qc_conv, "qc_conv", Cx % qc_conv, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_qc_conv, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashCode_cloud_layer) ! IndexCxcloud_layer
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % cloud_layer, "cloud_layer", Cx % cloud_layer, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_cloud_layer, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashItem_ozone_new) ! IndexCxOzone
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % ozone, "ozone", Cx % ozone, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_ozone, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashItem_qcf) ! IndexCxqcf
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % qcf, "qcf", Cx % qcf, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_qcf, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashItem_qcl) ! IndexCxqcl
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % qcl, "qcl", Cx % qcl, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_qcl, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashItem_cloud_bulk) ! IndexCxcloud_bulk
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % cloud_bulk, "cloud_bulk", Cx % cloud_bulk, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_cloud_bulk, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    ! wsmigaj: I haven't been able to identify the stash code associated with this field
    ! case (?)
      ! if (Cx % Header % ObsGroup == ObsGroupGroundLidar) then
      !   call opsinputs_fill_fillreal2dfromgeoval( &
      !     Cx % Header % aerosol_p, "aerosol_p", Cx % aerosol_p, &
      !     self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_aerosol_p, &
      !     self % JediToOpsLayoutMapping, self % hofx, self % varnames)
      ! end if
    case (StashCode_CDNC) ! IndexCxCDNC
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % CDNC, "CDNC", Cx % CDNC, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_CDNC, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashCode_RH_AfterMainCloud) ! IndexCxRH_AMC
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % RH_AMC, "RH_AMC", Cx % RH_AMC, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_RH_AMC, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashItem_Cl) ! IndexCxCl
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % Cl, "Cl", Cx % Cl, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_Cl, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashItem_Cf) ! IndexCxCf
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % Cf, "Cf", Cx % Cf, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_Cf, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashItem_qrain) ! IndexCxqrain
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % qrain, "qrain", Cx % qrain, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_qrain, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashItem_Exner) ! IndexCxExnerA
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % ExnerA, "ExnerA", Cx % ExnerA, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_ExnerA, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashCode_RichNumber) ! IndexCxRichNumber
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % RichNumber, "RichNumber", Cx % RichNumber, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_RichNumber, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashCode_SoilMoisture) ! IndexCxSoilMoisture
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % SoilMoisture, "SoilMoisture", Cx % SoilMoisture, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_SoilMoisture, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashCode_SoilTemp) ! IndexCxSoilTemp
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % SoilTemp, "SoilTemp", Cx % SoilTemp, &
        self % GeoVals,  self % GeoVaLsAreTopToBottom, opsinputs_cxfields_SoilTemp, &
        self % JediToOpsLayoutMapping, self % hofx, self % varnames)
    case (StashItem_dustMin:StashItem_dustMax) ! IndexCxDust1:IndexCxDust6
      DustBinIndex = CxField - StashItem_dustMin + 1
      if (DustBinIndex <= NDustBins) then
        write (DustBinIndexStr, '(i1)') DustBinIndex
        call opsinputs_fill_fillreal2dfromgeoval( &
          Cx % Header % dustp, "dustp", Cx % dustp(DustBinIndex) % field, &
          self % GeoVals,  self % GeoVaLsAreTopToBottom, &
          opsinputs_cxfields_dustp_start // DustBinIndexStr // opsinputs_cxfields_dustp_end, &
          self % JediToOpsLayoutMapping, self % hofx, self % varnames)
      end if
  end select
end do
end subroutine opsinputs_cxwriter_populatecx

! ------------------------------------------------------------------------------

!> Rotate winds back to their original orientation.
!> Can be used for limited-area domains such as the UKV.
subroutine opsinputs_cxwriter_unrotatewinds(self, Ob, Cx)
implicit none

! Subroutine arguments:
type(opsinputs_cxwriter), intent(inout) :: self
type(OB_type), intent(in)               :: Ob
type(CX_type), intent(inout)            :: Cx

! Local declarations:
character(len=*), parameter             :: RoutineName = "opsinputs_cxwriter_unrotatewinds"
character(len=80)                       :: ErrorMessage
integer(integer64)                      :: CxFields(MaxModelCodes)

integer                   :: Ilev         ! Loop variable
real(real64), allocatable :: EqLat(:)     ! Latitudes on equatorial grid
real(real64), allocatable :: EqLon(:)     ! Longitudes on equatorial grid
real(real64), allocatable :: Coeff1(:)    ! Coefficients for rotation
real(real64), allocatable :: Coeff2(:)    ! Coefficients for rotation
real(real64), allocatable :: Uunrot(:)    ! Array for unrotated wind u component
real(real64), allocatable :: Vunrot(:)    ! Array for unrotated wind v component

! Body:
call Ops_ReadCXControlNL(self % obsgroup, CxFields, BGECall = .false._8, ops_call = .false._8)

! Do not do anything if the winds were not rotated in the first place.
if (.not. Cx % header % rotated) then
   return
end if

! Require both wind components to be present.
if (all(CxFields /= StashItem_u) .and. all(CxFields /= StashItem_v)) then
   return
end if

! Code initially taken from Ops_RotateWinds.
! The rotation matrix has been modified to perform an un-rotation of the winds.
if (Cx % header % NumLocal > 0) then
  allocate (Coeff1(Cx % header % NumLocal))
  allocate (Coeff2(Cx % header % NumLocal))
  allocate (EqLon(Cx % header % NumLocal))
  allocate (EqLat(Cx % header % NumLocal))
  allocate (Uunrot(Cx % header % NumLocal))
  allocate (Vunrot(Cx % header % NumLocal))

  ! Calculate longitudes on equatorial grid
  call Gen_LatLon_to_Eq (Ob % latitude,      & ! in
                         Ob % longitude,     & ! in
                         EqLat(:),           & ! out
                         EqLon(:),           & ! out
                         self % RC_PoleLat,  & ! in
                         self % RC_PoleLong)   ! in

  deallocate (EqLat) ! Don't need latitudes

  ! Get rotation coefficients for winds
  call Ops_WCoeff (Coeff1(:),              & ! out
                   Coeff2(:),              & ! out
                   Ob % longitude,         & ! in
                   EqLon(:),               & ! in
                   self % RC_PoleLat,      & ! in
                   self % RC_PoleLong,     & ! in
                   Cx % header % NumLocal)   ! in

  deallocate (EqLon)

  ! Unrotate model level wind components.
  ! Note minus sign in front of Coeff2, which reverses the previous rotation.
  do Ilev = 1, Cx % Header % u % NumLev
     call Ops_WEq_to_ll (Coeff1(:), & ! in
          -Coeff2(:),               & ! in
          Cx % u(:,Ilev),           & ! in
          Cx % v(:,Ilev),           & ! in
          Uunrot(:),                & ! out
          Vunrot(:),                & ! out
          Cx % header % NumLocal,   & ! in
          Cx % header % NumLocal)     ! in
     ! Put unrotated values into Cx structure
     Cx % u(:,ILev) = Uunrot(:)
     Cx % v(:,Ilev) = Vunrot(:)
  end do

  deallocate (Vunrot)
  deallocate (Uunrot)
  deallocate (Coeff2)
  deallocate (Coeff1)
end if

end subroutine opsinputs_cxwriter_unrotatewinds


! ------------------------------------------------------------------------------

!> Prepare a UM header object to be given to the OPS function writing a Cx file.
subroutine opsinputs_cxwriter_populateumheader(self, UMHeader)
implicit none
! Subroutine arguments:
type(opsinputs_cxwriter), intent(in) :: self
type(UM_header_type), intent(inout)  :: UmHeader

! Local declarations:
integer                              :: NumLevels
integer(c_int)                       :: year, month, day, hour, minute, second

! Body:

NumLevels = size(self % EtaRho)

UmHeader % FixHd = IMDI
UmHeader % FixHd(FH_IntCStart) = LenFixHd + 1
UmHeader % FixHd(FH_IntCSize) = 49
UmHeader % FixHd(FH_RealCStart) = UmHeader % FixHd(FH_IntCStart) + UmHeader % FixHd(FH_IntCSize)
UmHeader % FixHd(FH_RealCSize) = 34
UmHeader % FixHd(FH_LevDepCStart) = UmHeader % FixHd(FH_RealCStart) + UmHeader % FixHd(FH_RealCSize)
UmHeader % FixHd(FH_LevDepCSize1) = NumLevels + 1  ! EtaTheta has an extra ground level
UmHeader % FixHd(FH_LevDepCSize2) = 2              ! Two variables: EtaTheta and EtaRho
UmHeader % FixHd(FH_LookupStart) = UmHeader % FixHd(FH_LevDepCStart) + &
                                   UmHeader % FixHd(FH_LevDepCSize1) * UmHeader % FixHd(FH_LevDepCSize2)
UmHeader % FixHd(FH_LookupSize1) = LBFT
UmHeader % FixHd(FH_LookupSize2) = 1

call UmHeader % alloc

UmHeader % FixHd(FH_SubModel) = self % FH_SubModel
UmHeader % FixHd(FH_VertCoord) = self % FH_VertCoord
UmHeader % FixHd(FH_HorizGrid) = self % FH_HorizGrid
UmHeader % FixHd(FH_GridStagger) = self % FH_GridStagger
UmHeader % FixHd(FH_Dataset) = FH_Dataset_InstDump
UmHeader % FixHd(FH_ModelVersion) = self % FH_ModelVersion
UmHeader % FixHd(FH_ObsFileType) = self % FH_ObsFileType

call datetime_to_YYYYMMDDhhmmss(self % ValidityTime, year, month, day, hour, minute, second)

UmHeader % FixHd(FH_DTYear) = year
UmHeader % FixHd(FH_DTMonth) = month
UmHeader % FixHd(FH_DTDay) = day
UmHeader % FixHd(FH_DTHour) = hour
UmHeader % FixHd(FH_DTMinute) = minute
UmHeader % FixHd(FH_DTSecond) = second
UmHeader % FixHd(FH_DTDayNo) = IMDI

UmHeader % FixHd(FH_VTYear) = year
UmHeader % FixHd(FH_VTMonth) = month
UmHeader % FixHd(FH_VTDay) = day
UmHeader % FixHd(FH_VTHour) = hour
UmHeader % FixHd(FH_VTMinute) = minute
UmHeader % FixHd(FH_VTSecond) = second
UmHeader % FixHd(FH_VTDayNo) = IMDI

UmHeader % IntC = IMDI
UmHeader % IntC(IC_XLen) = self % IC_XLen
UmHeader % IntC(IC_YLen) = self % IC_Ylen
UmHeader % IntC(IC_PLevels) = self % IC_PLevels
UmHeader % IntC(IC_WetLevels) = self % IC_WetLevels
UmHeader % IntC(IC_FirstConstantRhoLevel) = self % IC_FirstConstantRhoLevel

UmHeader % RealC = RMDI
UmHeader % RealC(RC_LongSpacing) = self % RC_LongSpacing
UmHeader % RealC(RC_LatSpacing) = self % RC_LatSpacing
UmHeader % RealC(RC_FirstLat) = self % RC_FirstLat
UmHeader % RealC(RC_FirstLong) = self % RC_FirstLong
UmHeader % RealC(RC_PoleLat) = self % RC_PoleLat
UmHeader % RealC(RC_PoleLong) = self % RC_PoleLong
UmHeader % RealC(RC_z_ModelTop) = self % RC_z_ModelTop

UmHeader % LevDepC = RMDI
UmHeader % LevDepC(1 : NumLevels+1) = self % EtaTheta
UmHeader % LevDepC(NumLevels+2 : 2*NumLevels+1) = self % EtaRho

UmHeader % Lookup = IMDI
! Validity time
UmHeader % Lookup(LBYR:LBMIN,1) = UmHeader % FixHd(FH_VTYear:FH_VTMinute)
UmHeader % Lookup(LBDAY,1) = UmHeader % FixHd(FH_VTDayNo)
UmHeader % Lookup(LBSEC,1) = UmHeader % FixHd(FH_VTSecond)
! Data time
UmHeader % Lookup(LBYRD:LBMIND,1) = UmHeader % FixHd(FH_DTYear:FH_DTMinute)
UmHeader % Lookup(LBDAYD,1) = UmHeader % FixHd(FH_DTDayNo)
UmHeader % Lookup(LBSECD,1) = UmHeader % FixHd(FH_DTSecond)
! Others
UmHeader % Lookup(LBTIM,1) = self % TimeIndicator
UmHeader % Lookup(LBFT,1) = self % ForecastPeriod

end subroutine opsinputs_cxwriter_populateumheader

! ------------------------------------------------------------------------------

!> Return an array with elements corresponding to retained observations set to .true. and the
!> remaining ones set to .false.
function opsinputs_cxwriter_retainflag(self, ReportFlags)
implicit none

! Function arguments:

type(opsinputs_cxwriter), intent(in) :: self
integer(integer64), intent(in)       :: ReportFlags(:)

! Return value:
logical(logical64)             :: opsinputs_cxwriter_retainflag(self % JediToOpsLayoutMapping % NumOpsObs)

! Body

opsinputs_cxwriter_retainflag = .not. btest(ReportFlags, FinalRejectFlag)

end function opsinputs_cxwriter_retainflag

end module opsinputs_cxwriter_mod
