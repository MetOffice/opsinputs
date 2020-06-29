! (C) Copyright 2020 Met Office UK
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

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
use obsspace_mod
use oops_variables_mod
use opsinputs_cxfields_mod
use opsinputs_fill_mod, only:            &
    opsinputs_fill_fillrealfromgeoval,   &
    opsinputs_fill_fillreal2dfromgeoval
use opsinputs_mpl_mod, only: opsinputs_mpl_allgather_integer
use opsinputs_obsspace_mod, only: opsinputs_obsspace_get_db_datetime_offset_in_seconds
use opsinputs_utils_mod
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
use OpsMod_DateTime, only: &
    DateTime_type,         &
    OpsFn_DateTime_now
use OpsMod_Kinds, only: &
    integer64,          &
    logical64,          &
    real64
use OpsMod_MiscTypes, only: ElementHeader_Type
use OpsMod_ModelColumnIO, only: &
    Ops_WriteOutVarCx,          &
    Ops_WriteOutVarCx1pe
use OpsMod_ObsGroupInfo, only: &
    OpsFn_ObsGroupNameToNum,   &
    ObsGroupAircraft,          &
    ObsGroupGPSRO,             &
    ObsGroupGroundLidar,       &
    ObsGroupSurface,           &
    ObsGroupSatwind,           &
    ObsGroupScatwind
use OpsMod_ObsInfo, only: &
    FinalRejectFlag,      &
    OB_type,              &
    Ops_Alloc,            &
    Ops_SetupObType
use OpsMod_Stash

implicit none
public :: opsinputs_cxwriter_create, opsinputs_cxwriter_delete, &
          opsinputs_cxwriter_prior, opsinputs_cxwriter_post
private

! ------------------------------------------------------------------------------
type, public :: opsinputs_cxwriter
private
  integer(integer64)         :: ObsGroup
  type(datetime)             :: ValidityTime  ! Corresponds to OPS validity time

  logical                    :: RejectObsWithAnyVariableFailingQC
  logical                    :: RejectObsWithAllVariablesFailingQC

  integer(integer64)         :: FH_VertCoord
  integer(integer64)         :: FH_HorizGrid
  integer(integer64)         :: FH_GridStagger
  integer(integer64)         :: FH_ObsFileType
  integer(integer64)         :: FH_ModelVersion

  integer(integer64)         :: IC_XLen
  integer(integer64)         :: IC_YLen
  integer(integer64)         :: IC_PLevels
  integer(integer64)         :: IC_WetLevels
  integer(integer64)         :: IC_FirstConstantRhoLevel

  real(real64)               :: RC_LongSpacing
  real(real64)               :: RC_LatSpacing
  real(real64)               :: RC_FirstLat
  real(real64)               :: RC_FirstLong
  real(real64)               :: RC_PoleLat
  real(real64)               :: RC_PoleLong
  real(real64)               :: RC_z_ModelTop

  integer(integer64)         :: TimeIndicator
  integer(integer64)         :: ForecastPeriod

  real(real64), allocatable  :: EtaTheta(:)
  real(real64), allocatable  :: EtaRho(:)

  type(ufo_geovals), pointer :: GeoVals
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
character(len=:), allocatable           :: string
integer                                 :: int
logical                                 :: bool
real(kind=c_double)                     :: double

character(len=*), parameter             :: RoutineName = "opsinputs_cxwriter_create"
character(len=200)                      :: ErrorMessage

! Body:

opsinputs_cxwriter_create = .true.

! Setup OPS

if (.not. f_conf % get("general_mode", string)) then
  ! fall back to the default value
  string = "normal"
end if
select case (ops_to_lower_case(string))
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
  write (ErrorMessage, '("GeneralMode code not recognised: ",A)') string
  call gen_warn(RoutineName, ErrorMessage)
  opsinputs_cxwriter_create = .false.
  return
end select

call Gen_SetupControl(DefaultDocURL)
call Ops_InitMPI

! Retrieve parameter values from the input configuration object
! and store them in member variables

if (.not. f_conf % get("obs_group", string)) then
  call gen_warn(RoutineName, "Mandatory obs_group option not found")
  opsinputs_cxwriter_create = .false.
  return
end if
self % ObsGroup = OpsFn_ObsGroupNameToNum(string)

if (.not. f_conf % get("validity_time", string)) then
  call gen_warn(RoutineName, "Mandatory validity_time option not found")
  opsinputs_cxwriter_create = .false.
  return
end if
call datetime_create(string, self % validitytime)

if (.not. f_conf % get("reject_obs_with_any_variable_failing_qc", &
                       self % RejectObsWithAnyVariableFailingQC)) then
  ! fall back to the default value
  self % RejectObsWithAnyVariableFailingQC = .false.
end if

if (.not. f_conf % get("reject_obs_with_all_variables_failing_qc", &
                     self % RejectObsWithAllVariablesFailingQC)) then
  ! fall back to the default value
  self % RejectObsWithAllVariablesFailingQC = .false.
end if

if (.not. f_conf % get("FH_VertCoord", string)) then
  ! fall back to the default value
  string = "hybrid"  ! TODO(wsmigaj): is this a good default?
end if
select case (ops_to_lower_case(string))
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
  write (ErrorMessage, '("FH_VertCoord code not recognised: ",A)') string
  call gen_warn(RoutineName, ErrorMessage)
  opsinputs_cxwriter_create = .false.
  return
end select

if (.not. f_conf % get("FH_HorizGrid", string)) then
  ! fall back to the default value
  string = "global"
end if
select case (ops_to_lower_case(string))
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
  write (ErrorMessage, '("FH_HorizGrid code not recognised: ",A)') string
  call gen_warn(RoutineName, ErrorMessage)
  opsinputs_cxwriter_create = .false.
  return
end select

if (.not. f_conf % get("FH_GridStagger", string)) then
  ! fall back to the default value
  string = "endgame"
end if
select case (ops_to_lower_case(string))
case ("arakawab")
  self % FH_GridStagger = FH_GridStagger_ArakawaB
case ("arakawac")
  self % FH_GridStagger = FH_GridStagger_ArakawaC
case ("endgame")
  self % FH_GridStagger = FH_GridStagger_EndGame
case default
  write (ErrorMessage, '("FH_GridStagger code not recognised: ",A)') string
  call gen_warn(RoutineName, ErrorMessage)
  opsinputs_cxwriter_create = .false.
  return
end select

if (.not. f_conf % get("FH_ObsFileType", string)) then
  ! fall back to the default value
  string = "atmos"
end if
select case (ops_to_lower_case(string))
case ("atmos")
  self % FH_ObsFileType = FH_ObsFileType_Atmos
case ("ocean")
  self % FH_ObsFileType = FH_ObsFileType_Ocean
case ("sst")
  self % FH_ObsFileType = FH_ObsFileType_SST
case ("wave")
  self % FH_ObsFileType = FH_ObsFileType_Wave
case default
  write (ErrorMessage, '("FH_ObsFileType code not recognised: ",A)') string
  call gen_warn(RoutineName, ErrorMessage)
  opsinputs_cxwriter_create = .false.
  return
end select

if (.not. f_conf % get("FH_ModelVersion", int)) then
  ! fall back to the default value
  int = 0
end if
self % FH_ModelVersion = int

if (.not. f_conf % get("IC_XLen", int)) then
  ! fall back to the default value
  int = 0
end if
self % IC_XLen = int

if (.not. f_conf % get("IC_YLen", int)) then
  ! fall back to the default value
  int = 0
end if
self % IC_YLen = int

if (.not. f_conf % get("IC_PLevels", int)) then
  ! fall back to the default value
  int = 0
end if
self % IC_PLevels = int

if (.not. f_conf % get("IC_WetLevels", int)) then
  ! fall back to the default value
  int = 0
end if
self % IC_WetLevels = int

if (.not. f_conf % get("IC_FirstConstantRhoLevel", int)) then
  ! fall back to the default value
  int = 0
end if
self % IC_FirstConstantRhoLevel = int

if (.not. f_conf % get("RC_LongSpacing", double)) then
  ! fall back to the default value
  double = 0.0
end if
self % RC_LongSpacing = double

if (.not. f_conf % get("RC_LatSpacing", double)) then
  ! fall back to the default value
  double = 0.0
end if
self % RC_LatSpacing = double

if (.not. f_conf % get("RC_FirstLat", double)) then
  ! fall back to the default value
  double = 0.0
end if
self % RC_FirstLat = double

if (.not. f_conf % get("RC_FirstLong", double)) then
  ! fall back to the default value
  double = 0.0
end if
self % RC_FirstLong = double

if (.not. f_conf % get("RC_PoleLat", double)) then
  ! fall back to the default value
  double = 0.0
end if
self % RC_PoleLat = double

if (.not. f_conf % get("RC_PoleLong", double)) then
  ! fall back to the default value
  double = 0.0
end if
self % RC_PoleLong = double

if (.not. f_conf % get("RC_z_ModelTop", double)) then
  ! fall back to the default value
  double = 0.0
end if
self % RC_z_ModelTop = double

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
    call gen_warn(RoutineName, "eta_theta_levels should be a vector of length (IC_PLevels + 1)")
    opsinputs_cxwriter_create = .false.
    return
  end if
else
  allocate(self % EtaRho(self % IC_PLevels))
  self % EtaRho = RMDI
end if

if (.not. f_conf % get("time_indicator", int)) then
  ! fall back to the default value
  int = 0
end if
self % TimeIndicator = int

if (.not. f_conf % get("forecast_period", int)) then
  ! fall back to the default value
  int = 0
end if
self % ForecastPeriod = int

if (.not. f_conf % get("model_type", string)) then
  ! fall back to the default value
  string = "atmos"
end if
select case (ops_to_lower_case(string))
case ("atmos")
  ModelType = ModelType_Atmos
case ("ocean")
  ModelType = ModelType_Ocean
case ("sst")
  ModelType = ModelType_SST
case default
  write (ErrorMessage, '("model_type code not recognised: ",A)') string
  call gen_warn(RoutineName, ErrorMessage)
  opsinputs_cxwriter_create = .false.
  return
end select

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
type(c_ptr), value, intent(in)             :: ObsSpace
type(ufo_geovals), intent(in), pointer     :: GeoVals

! Body:
self % GeoVals => GeoVals

end subroutine opsinputs_cxwriter_prior

! ------------------------------------------------------------------------------

!> Called by the postFilter() method of the C++ CxWriter object.
!>
!> Write out a Cx file containing varfields derived from JEDI variables.
subroutine opsinputs_cxwriter_post(self, ObsSpace, Flags)
implicit none

! Subroutine arguments:
type(opsinputs_cxwriter), intent(in) :: self
type(c_ptr), value, intent(in)       :: ObsSpace
type(c_ptr), value, intent(in)       :: Flags

! Local declarations:
integer(integer64)                   :: NumObsLocal

! Body:

NumObsLocal = obsspace_get_nlocs(ObsSpace)
call opsinputs_cxwriter_post_internal(self, NumObsLocal, ObsSpace, Flags)

end subroutine opsinputs_cxwriter_post

! ------------------------------------------------------------------------------

!> Core implementation of opsinputs_cxwriter_post.
!>
!> This code has been extracted to a separate subroutine to make it possible to declare Retained as
!> an automatic array (since the number of observations is now known).
subroutine opsinputs_cxwriter_post_internal(self, NumObsLocal, ObsSpace, Flags)
USE mpl, ONLY: &
          gc_int_kind, mpl_integer
implicit none

! Subroutine arguments:
type(opsinputs_cxwriter), intent(in) :: self
integer(integer64)                   :: NumObsLocal
type(c_ptr), value, intent(in)       :: ObsSpace
type(c_ptr), value, intent(in)       :: Flags

! Local declarations:
type(OB_type)                        :: Ob
type(CX_type)                        :: Cx
type(UM_header_type)                 :: UMHeader
integer(integer64)                   :: NumObsOnEachRank(nproc)
logical(logical64)                   :: Retained(NumObsLocal)
integer(kind=gc_int_kind)            :: istat

! Body:

call opsinputs_cxwriter_allocateobservations(self, NumObsLocal, Ob)
call opsinputs_cxwriter_populateobservations(self, ObsSpace, Flags, Ob)
call opsinputs_cxwriter_allocatecx(self, Ob, Cx)
call opsinputs_cxwriter_populatecx(self, ObsSpace, Flags, Cx)
call opsinputs_cxwriter_populateumheader(self, UMHeader)

Retained = opsinputs_cxwriter_retainflag(NumObsLocal, ObsSpace, Flags, &
                                         self % RejectObsWithAnyVariableFailingQC, &
                                         self % RejectObsWithAllVariablesFailingQC)
call opsinputs_mpl_allgather_integer([NumObsLocal], 1_gc_int_kind, mpl_integer, &
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
integer(integer64)                   :: CxFields(MaxModelCodes)
integer                              :: i
character(len=max_varname_length)    :: GeoVarName

! Body:
call Ops_ReadCXControlNL(self % obsgroup, CxFields, BGECall = .false._8, ops_call = .false._8)

do i = 1, size(CxFields)
  GeoVarName = opsinputs_cxfields_unknown

  select case (CxFields(i))

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
    case (StashItem_modelsurface) ! IndexCxmodelsurface
      GeoVarName = "land_type_index"
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
      GeoVarName = "air_potential_temperature"
    case (StashCode_rh, StashCode_rh_p) ! IndexCxrh
      GeoVarName = opsinputs_cxfields_rh
    case (StashItem_u, StashCode_u_p_B_grid) ! IndexCxu
      GeoVarName = "eastward_wind"
    case (StashItem_v) ! IndexCxv
      GeoVarName = "northward_wind"
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
    ! TODO(someone): add support for these cxfields
    ! case (IndexCxDust1, IndexCxDust2, &
    !       IndexCxDust3, IndexCxDust4, &
    !       IndexCxDust5, IndexCxDust6)
    !   GeoVarName = opsinputs_cxfields_dustp
  end select
  
  if (GeoVarName /= opsinputs_cxfields_unknown) then
    call geovars % push_back(GeoVarName)
  end if
end do

end subroutine opsinputs_cxwriter_addrequiredgeovars

! ------------------------------------------------------------------------------

!> Prepare Ob to hold the required number of observations.
subroutine opsinputs_cxwriter_allocateobservations(self, NumObsLocal, Ob)
use mpl, ONLY: gc_int_kind
implicit none

! Subroutine arguments:
type(opsinputs_cxwriter), intent(in) :: self
integer(integer64), intent(in)       :: NumObsLocal
type(OB_type), intent(inout)         :: Ob

! Local declarations:
integer(kind=gc_int_kind)            :: istat

! Body:
Ob % Header % obsgroup = self % obsgroup

call Ops_SetupObType(Ob)

Ob % Header % NumObsLocal = NumObsLocal
Ob % Header % NumObsTotal = Ob % Header % NumObsLocal
! Sum the number of local observations on each process into NumObsTotal
call gcg_isum (1_gc_int_kind, mpi_group, istat, Ob % Header % NumObsTotal)

Ob % Header % NumCXBatches = 1
allocate(Ob % Header % ObsPerBatchPerPE(Ob % Header % NumCXBatches, 0:nproc - 1))
Ob % Header % ObsPerBatchPerPE(1,mype) = Ob % Header % NumObsLocal

end subroutine opsinputs_cxwriter_allocateobservations

! ------------------------------------------------------------------------------

!> Populate Ob fields required by the OPS routine writing a Cx file.
subroutine opsinputs_cxwriter_populateobservations(self, ObsSpace, Flags, Ob)
implicit none

! Subroutine arguments:
type(opsinputs_cxwriter), intent(in)    :: self
type(c_ptr), value, intent(in)          :: ObsSpace
type(c_ptr), value, intent(in)          :: Flags
type(OB_type), intent(inout)            :: Ob

! Local declarations:
integer(c_int)                          :: year, month, day, hour, minute, second
real(c_double)                          :: Reals(Ob % Header % NumObsLocal)
integer(c_int64_t)                      :: TimeOffsetsInSeconds(Ob % Header % NumObsLocal)

! Body:

call datetime_to_YYYYMMDDhhmmss(self % ValidityTime, year, month, day, hour, minute, second)
Ob % Header % ValidityTime % year = year
Ob % Header % ValidityTime % month = month
Ob % Header % ValidityTime % day = day
Ob % Header % ValidityTime % hour = hour
Ob % Header % ValidityTime % minute = minute
Ob % Header % ValidityTime % second = second
Ob % Header % ValidityTime % diff_from_utc = 0 ! TODO(wsmigaj): Is it OK to assume this?

call Ops_Alloc(Ob % Header % Latitude, "Latitude", Ob % Header % NumObsLocal, Ob % Latitude)
call obsspace_get_db(ObsSpace, "MetaData", "latitude", Reals)

call Ops_Alloc(Ob % Header % Longitude, "Longitude", Ob % Header % NumObsLocal, Ob % Longitude)
call obsspace_get_db(ObsSpace, "MetaData", "longitude", Reals)

call Ops_Alloc(Ob % Header % Time, "Time", Ob % Header % NumObsLocal, Ob % Time)
call opsinputs_obsspace_get_db_datetime_offset_in_seconds( &
  ObsSpace, "MetaData", "datetime", self % validitytime, TimeOffsetsInSeconds)
Reals = TimeOffsetsInSeconds

end subroutine opsinputs_cxwriter_populateobservations

! ------------------------------------------------------------------------------

!> Prepare Cx to hold the required number of model columns.
subroutine opsinputs_cxwriter_allocatecx(self, Ob, Cx)
implicit none

! Subroutine arguments:
type(opsinputs_cxwriter), intent(in)    :: self
type(OB_type), intent(in)               :: Ob
type(CX_type), intent(inout)            :: Cx

! Body:

call CX % init

! The following code is inspired by Ops_CXSetup.

Cx % Header % NumLocal = Ob % Header % NumObsLocal
Cx % Header % NumTotal = Ob % Header % NumObsTotal
Cx % Header % ModelVersion = self % FH_ModelVersion
Cx % Header % SubModel = FH_SubModel_Atmos
Cx % Header % NewDynamics = self % FH_ModelVersion >= 500 .and. ModelType /= ModelType_Ocean

if (Cx % Header % NewDynamics .and. ModelType /= ModelType_SST) then
  Cx % Header % FirstConstantRhoLevel = self % IC_FirstConstantRhoLevel
else
  Cx % Header % FirstConstantRhoLevel = IMDI
end if

end subroutine opsinputs_cxwriter_allocatecx

! ------------------------------------------------------------------------------

!> Populate Cx fields required by the OPS routine writing a Cx file.
subroutine opsinputs_cxwriter_populatecx(self, ObsSpace, Flags, Cx)
implicit none

! Subroutine arguments:
type(opsinputs_cxwriter), intent(in)    :: self
type(c_ptr), value, intent(in)          :: ObsSpace
type(c_ptr), value, intent(in)          :: Flags
type(CX_type), intent(inout)            :: Cx

! Local declarations:
character(len=*), parameter             :: RoutineName = "opsinputs_cxwriter_populatecx"
character(len=80)                       :: ErrorMessage

integer(integer64)                      :: CxFields(MaxModelCodes)
integer                                 :: nCxFields
integer                                 :: iCxField

! Body:
call Ops_ReadCXControlNL(self % obsgroup, CxFields, BGECall = .false._8, ops_call = .false._8)

do iCxField = 1, size(CxFields)
  select case (CxFields(iCxField))

    ! Surface variables

    case (StashItem_Orog) ! IndexCxorog
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % Orog, "Orog", Cx % Header % NumLocal, Cx % Orog, &
        self % GeoVals, opsinputs_cxfields_Orog)
    case (StashItem_Pstar, StashItem_P_surface) ! IndexCxpstar
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % pstar, "pstar", Cx % Header % NumLocal, Cx % pstar, &
        self % GeoVals, opsinputs_cxfields_pstar)
    case (StashCode_t2) ! IndexCxt2
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % t2, "t2", Cx % Header % NumLocal, Cx % t2, &
        self % GeoVals, opsinputs_cxfields_t2)
    case (StashCode_rh2) ! IndexCxrh2
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % rh2, "rh2", Cx % Header % NumLocal, Cx % rh2, &
        self % GeoVals, opsinputs_cxfields_rh2)
    case (StashCode_u10, StashCode_U10_B_grid) ! IndexCxu10
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % u10, "u10", Cx % Header % NumLocal, Cx % u10, &
        self % GeoVals, opsinputs_cxfields_u10)
    case (StashCode_v10, StashCode_V10_B_grid) ! IndexCxv10
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % v10, "v10", Cx % Header % NumLocal, Cx % v10, &
        self % GeoVals, opsinputs_cxfields_v10)
    case (StashItem_modelsurface) ! IndexCxmodelsurface
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % ModelSurface, "ModelSurface", Cx % Header % NumLocal, Cx % ModelSurface, &
        self % GeoVals, "land_type_index")
    case (StashCode_vis) ! IndexCxvis
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % vis, "vis", Cx % Header % NumLocal, Cx % vis, &
        self % GeoVals, opsinputs_cxfields_vis)
    case (StashCode_WAVE_HGHT) ! IndexCxWAVE_HGHT
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % WAVE_HGHT, "WAVE_HGHT", Cx % Header % NumLocal, Cx % WAVE_HGHT, &
        self % GeoVals, opsinputs_cxfields_WAVE_HGHT)
    case (StashCode_WIND_SPED) ! IndexCxWIND_SPED
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % WIND_SPED, "WIND_SPED", Cx % Header % NumLocal, Cx % WIND_SPED, &
        self % GeoVals, opsinputs_cxfields_WIND_SPED)
    case (AncilCode_SeaHeight) ! IndexCxSeaHeight
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % SeaHeight, "SeaHeight", Cx % Header % NumLocal, Cx % SeaHeight, &
        self % GeoVals, opsinputs_cxfields_SeaHeight)
    case (StashItem_SST) ! IndexCxTskinSea
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % TskinSea, "TskinSea", Cx % Header % NumLocal, Cx % TskinSea, &
        self % GeoVals, opsinputs_cxfields_TskinSea)
    ! wsmigaj: I haven't been able to identify the stash code associated with this field
    ! case (?) ! IndexCxTropPres
      !  call opsinputs_fill_fillrealfromgeoval( &
      !    Cx % Header % TropPres, "TropPres", Cx % Header % NumLocal, Cx % TropPres, &
      !    self % GeoVals, "PLACEHOLDER")
    case (StashCode_pmsl) ! IndexCxpmsl
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % pmsl, "pmsl", Cx % Header % NumLocal, Cx % pmsl, &
        self % GeoVals, opsinputs_cxfields_pmsl)
    case (StashItem_SeaIce) ! IndexCxSeaIce
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % SeaIce, "SeaIce", Cx % Header % NumLocal, Cx % SeaIce, &
        self % GeoVals, opsinputs_cxfields_SeaIce)
    case (StashItem_SnowAmount) ! IndexCxSnowAmount
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % SnowAmount, "SnowAmount", Cx % Header % NumLocal, Cx % SnowAmount, &
        self % GeoVals, opsinputs_cxfields_SnowAmount)
    case (StashCode_qt2) ! IndexCxqt2
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % qt2, "qt2", Cx % Header % NumLocal, Cx % qt2, &
        self % GeoVals, opsinputs_cxfields_qt2)
    ! wsmigaj: I haven't been able to identify the stash code associated with this field
    ! case (?) ! IndexCxaerosol
      !  if (Cx % Header % ObsGroup == ObsGroupSurface) then
      !    call opsinputs_fill_fillrealfromgeoval( &
      !      Cx % Header % aerosol, "aerosol", Cx % Header % NumLocal, Cx % aerosol, &
      !      self % GeoVals, "PLACEHOLDER")
      !  end if
    case (StashCode_PsurfParamA) ! IndexCxPsurfParamA
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % PSurfParamA, "PSurfParamA", Cx % Header % NumLocal, Cx % PSurfParamA, &
        self % GeoVals, opsinputs_cxfields_PSurfParamA)
    case (StashCode_PSurfParamB) ! IndexCxPSurfParamB
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % PSurfParamB, "PSurfParamB", Cx % Header % NumLocal, Cx % PSurfParamB, &
        self % GeoVals, opsinputs_cxfields_PSurfParamB)
    case (StashCode_LapseRate) ! IndexCxLapseRate
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % LapseRate, "LapseRate", Cx % Header % NumLocal, Cx % LapseRate, &
        self % GeoVals, opsinputs_cxfields_LapseRate)
    case (StashCode_CloudAmount) ! IndexCxCloudAmount
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % CloudAmount, "CloudAmount", Cx % Header % NumLocal, Cx % CloudAmount, &
        self % GeoVals, opsinputs_cxfields_CloudAmount)
    case (StashItem_ConvCloudAmount) ! IndexCxConvCloudAmount
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % ConvCloudAmount, "ConvCloudAmount", Cx % Header % NumLocal, Cx % ConvCloudAmount, &
        self % GeoVals, opsinputs_cxfields_ConvCloudAmount)
    case (StashItem_ConvCloudBase) ! IndexCxConvCloudBaseLevel
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % ConvCloudBaseLevel, "ConvCloudBaseLevel", Cx % Header % NumLocal, Cx % ConvCloudBaseLevel, &
        self % GeoVals, opsinputs_cxfields_ConvCloudBaseLevel)
    case (StashItem_ConvCloudTop) ! IndexCxConvCloudTopLevel
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % ConvCloudTopLevel, "ConvCloudTopLevel", Cx % Header % NumLocal, Cx % ConvCloudTopLevel, &
        self % GeoVals, opsinputs_cxfields_ConvCloudTopLevel)
    case (StashCode_SurfRainRate_conv) ! IndexCxSurfRainRate_conv
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % SurfRainRate_conv, "SurfRainRate_conv", Cx % Header % NumLocal, Cx % SurfRainRate_conv, &
        self % GeoVals, opsinputs_cxfields_SurfRainRate_conv)
    case (StashCode_SurfSnowRate_conv) ! IndexCxSurfSnowRate_conv
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % SurfSnowRate_conv, "SurfSnowRate_conv", Cx % Header % NumLocal, Cx % SurfSnowRate_conv, &
        self % GeoVals, opsinputs_cxfields_SurfSnowRate_conv)
    case (AncilCode_SeaSrfcHeight) ! IndexCxSeaSrfcHeight
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % SeaSrfcHeight, "SeaSrfcHeight", Cx % Header % NumLocal, Cx % SeaSrfcHeight, &
        self % GeoVals, opsinputs_cxfields_SeaSrfcHeight)
    case (AncilCode_MeanSeaHeight) ! IndexCxMeanSeaHeight
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % MeanSeaHeight, "MeanSeaHeight", Cx % Header % NumLocal, Cx % MeanSeaHeight, &
        self % GeoVals, opsinputs_cxfields_MeanSeaHeight)
    case (StashCode_SurfRainRate_LS) ! IndexCxSurfRainRate_LS
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % SurfRainRate_LS, "SurfRainRate_LS", Cx % Header % NumLocal, Cx % SurfRainRate_LS, &
        self % GeoVals, opsinputs_cxfields_SurfRainRate_LS)
    case (StashCode_SurfSnowRate_LS) ! IndexCxSurfSnowRate_LS
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % SurfSnowRate_LS, "SurfSnowRate_LS", Cx % Header % NumLocal, Cx % SurfSnowRate_LS, &
        self % GeoVals, opsinputs_cxfields_SurfSnowRate_LS)
    case (StashCode_SWradiation) ! IndexCxSWradiation
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % SWradiation, "SWradiation", Cx % Header % NumLocal, Cx % SWradiation, &
        self % GeoVals, opsinputs_cxfields_SWradiation)
    case (StashItem_BLheight) ! IndexCxBLheight
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % BLheight, "BLheight", Cx % Header % NumLocal, Cx % BLheight, &
        self % GeoVals, opsinputs_cxfields_BLheight)
    case (StashCode_ObukhovLength) ! IndexCxObukhovLength
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % ObukhovLength, "ObukhovLength", Cx % Header % NumLocal, Cx % ObukhovLength, &
        self % GeoVals, opsinputs_cxfields_ObukhovLength)
    case (StashCode_FrictionVel) ! IndexCxFrictionVel
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % FrictionVel, "FrictionVel", Cx % Header % NumLocal, Cx % FrictionVel, &
        self % GeoVals, opsinputs_cxfields_FrictionVel)
    case (StashCode_PrecipAcc6hr) ! IndexCxPrecipAcc6hr
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % PrecipAcc6hr, "PrecipAcc6hr", Cx % Header % NumLocal, Cx % PrecipAcc6hr, &
        self % GeoVals, opsinputs_cxfields_PrecipAcc6hr)
    case (StashCode_LowCloudAmount) ! IndexCxLowCloudAmount
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % LowCloudAmount, "LowCloudAmount", Cx % Header % NumLocal, Cx % LowCloudAmount, &
        self % GeoVals, opsinputs_cxfields_LowCloudAmount)
    case (StashCode_MedCloudAmount) ! IndexCxMedCloudAmount
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % MedCloudAmount, "MedCloudAmount", Cx % Header % NumLocal, Cx % MedCloudAmount, &
        self % GeoVals, opsinputs_cxfields_MedCloudAmount)
    case (StashCode_LowCloudBase, StashCode_2p5CloudBase) ! IndexCxLowCloudBase
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % LowCloudBase, "LowCloudBase", Cx % Header % NumLocal, Cx % LowCloudBase, &
        self % GeoVals, opsinputs_cxfields_LowCloudBase)
    case (StashCode_SO2_AQ) ! IndexCxSO2_AQ
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % SO2_AQ, "SO2_AQ", Cx % Header % NumLocal, Cx % SO2_AQ, &
        self % GeoVals, opsinputs_cxfields_SO2_AQ)
    case (StashCode_PM10_AQ) ! IndexCxPM10_AQ
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % PM10_AQ, "PM10_AQ", Cx % Header % NumLocal, Cx % PM10_AQ, &
        self % GeoVals, opsinputs_cxfields_PM10_AQ)
    case (StashCode_PM2p5_AQ) ! IndexCxPM2p5_AQ
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % PM2p5_AQ, "PM2p5_AQ", Cx % Header % NumLocal, Cx % PM2p5_AQ, &
        self % GeoVals, opsinputs_cxfields_PM2p5_AQ)
    case (StashCode_O3_AQ) ! IndexCxO3_AQ
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % O3_AQ, "O3_AQ", Cx % Header % NumLocal, Cx % O3_AQ, &
        self % GeoVals, opsinputs_cxfields_O3_AQ)
    case (StashCode_NO2_AQ) ! IndexCxNO2_AQ
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % NO2_AQ, "NO2_AQ", Cx % Header % NumLocal, Cx % NO2_AQ, &
        self % GeoVals, opsinputs_cxfields_NO2_AQ)
    case (StashCode_CO_AQ) ! IndexCxCO_AQ
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % CO_AQ, "CO_AQ", Cx % Header % NumLocal, Cx % CO_AQ, &
        self % GeoVals, opsinputs_cxfields_CO_AQ)
    case (StashCode_BLtype) ! IndexCxBLtype
      call opsinputs_fill_fillrealfromgeoval( &
        Cx % Header % BLtype, "BLtype", Cx % Header % NumLocal, Cx % BLtype, &
        self % GeoVals, opsinputs_cxfields_BLtype)

    ! Upper-air variables
    case (StashItem_theta) ! IndexCxtheta
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % theta, "theta", Cx % Header % NumLocal, Cx % theta, &
        self % GeoVals, "air_potential_temperature")
    case (StashCode_rh, StashCode_rh_p) ! IndexCxrh
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % rh, "rh", Cx % Header % NumLocal, Cx % rh, &
        self % GeoVals, opsinputs_cxfields_rh)
    case (StashItem_u, StashCode_u_p_B_grid) ! IndexCxu
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % u, "u", Cx % Header % NumLocal, Cx % u, &
        self % GeoVals, "eastward_wind")
    case (StashItem_v) ! IndexCxv
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % v, "v", Cx % Header % NumLocal, Cx % v, &
        self % GeoVals, "northward_wind")
    case (StashItem_w) ! IndexCxw
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % w, "w", Cx % Header % NumLocal, Cx % w, &
        self % GeoVals, opsinputs_cxfields_w)
    case (StashItem_q) ! IndexCxq
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % q, "q", Cx % Header % NumLocal, Cx % q, &
        self % GeoVals, opsinputs_cxfields_q)
    case (StashItem_qc) ! IndexCxqc
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % qc, "qc", Cx % Header % NumLocal, Cx % qc, &
        self % GeoVals, opsinputs_cxfields_qc)
    case (StashItem_p_bar) ! IndexCxp_bar
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % p_bar, "p_bar", Cx % Header % NumLocal, Cx % p_bar, &
        self % GeoVals, opsinputs_cxfields_p_bar)
    ! wsmigaj: I haven't been able to identify the stash code associated with this field
    ! case (?) ! IndexCxcloud
      !  call opsinputs_fill_fillreal2dfromgeoval( &
      !    Cx % Header % cloud, "cloud", Cx % Header % NumLocal, Cx % cloud, &
      !    self % GeoVals, "PLACEHOLDER")
    case (StashCode_ql_layer) ! IndexCxql_layer
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % ql_layer, "ql_layer", Cx % Header % NumLocal, Cx % ql_layer, &
        self % GeoVals, opsinputs_cxfields_ql_layer)
    case (StashItem_p) ! IndexCxP
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % PLevelsA, "PLevelsA", Cx % Header % NumLocal, Cx % PLevelsA, &
        self % GeoVals, opsinputs_cxfields_PLevelsA)
    case (AncilCode_salt) ! IndexCxSalt
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % Salt, "Salt", Cx % Header % NumLocal, Cx % Salt, &
        self % GeoVals, opsinputs_cxfields_Salt)
    case (StashCode_t_p) ! IndexCxt
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % t, "t", Cx % Header % NumLocal, Cx % t, &
        self % GeoVals, opsinputs_cxfields_t)
    case (StashCode_qf_layer) ! IndexCxqf_layer
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % qf_layer, "qf_layer", Cx % Header % NumLocal, Cx % qf_layer, &
        self % GeoVals, opsinputs_cxfields_qf_layer)
    case (StashCode_RainRate_layer) ! IndexCxRainRate_layer
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % RainRate_layer, "RainRate_layer", Cx % Header % NumLocal, Cx % RainRate_layer, &
        self % GeoVals, opsinputs_cxfields_RainRate_layer)
    case (StashCode_cloud_conv) ! IndexCxcloud_conv
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % cloud_conv, "cloud_conv", Cx % Header % NumLocal, Cx % cloud_conv, &
        self % GeoVals, opsinputs_cxfields_cloud_conv)
    case (StashCode_qc_conv) ! IndexCxqc_conv
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % qc_conv, "qc_conv", Cx % Header % NumLocal, Cx % qc_conv, &
        self % GeoVals, opsinputs_cxfields_qc_conv)
    case (StashCode_cloud_layer) ! IndexCxcloud_layer
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % cloud_layer, "cloud_layer", Cx % Header % NumLocal, Cx % cloud_layer, &
        self % GeoVals, opsinputs_cxfields_cloud_layer)
    case (StashItem_ozone_new) ! IndexCxOzone
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % ozone, "ozone", Cx % Header % NumLocal, Cx % ozone, &
        self % GeoVals, opsinputs_cxfields_ozone)
    case (StashItem_qcf) ! IndexCxqcf
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % qcf, "qcf", Cx % Header % NumLocal, Cx % qcf, &
        self % GeoVals, opsinputs_cxfields_qcf)
    case (StashItem_qcl) ! IndexCxqcl
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % qcl, "qcl", Cx % Header % NumLocal, Cx % qcl, &
        self % GeoVals, opsinputs_cxfields_qcl)
    case (StashItem_cloud_bulk) ! IndexCxcloud_bulk
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % cloud_bulk, "cloud_bulk", Cx % Header % NumLocal, Cx % cloud_bulk, &
        self % GeoVals, opsinputs_cxfields_cloud_bulk)
    ! wsmigaj: I haven't been able to identify the stash code associated with this field
    ! case (?)
      ! if (Cx % Header % ObsGroup == ObsGroupGroundLidar) then
      !   call opsinputs_fill_fillreal2dfromgeoval( &
      !     Cx % Header % aerosol_p, "aerosol_p", Cx % Header % NumLocal, Cx % aerosol_p, &
      !     self % GeoVals, "PLACEHOLDER")
      ! end if
    case (StashCode_CDNC) ! IndexCxCDNC
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % CDNC, "CDNC", Cx % Header % NumLocal, Cx % CDNC, &
        self % GeoVals, opsinputs_cxfields_CDNC)
    case (StashCode_RH_AfterMainCloud) ! IndexCxRH_AMC
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % RH_AMC, "RH_AMC", Cx % Header % NumLocal, Cx % RH_AMC, &
        self % GeoVals, opsinputs_cxfields_RH_AMC)
    case (StashItem_Cl) ! IndexCxCl
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % Cl, "Cl", Cx % Header % NumLocal, Cx % Cl, &
        self % GeoVals, opsinputs_cxfields_Cl)
    case (StashItem_Cf) ! IndexCxCf
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % Cf, "Cf", Cx % Header % NumLocal, Cx % Cf, &
        self % GeoVals, opsinputs_cxfields_Cf)
    case (StashItem_qrain) ! IndexCxqrain
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % qrain, "qrain", Cx % Header % NumLocal, Cx % qrain, &
        self % GeoVals, opsinputs_cxfields_qrain)
    case (StashItem_Exner) ! IndexCxExnerA
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % ExnerA, "ExnerA", Cx % Header % NumLocal, Cx % ExnerA, &
        self % GeoVals, opsinputs_cxfields_ExnerA)
    case (StashCode_RichNumber) ! IndexCxRichNumber
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % RichNumber, "RichNumber", Cx % Header % NumLocal, Cx % RichNumber, &
        self % GeoVals, opsinputs_cxfields_RichNumber)
    case (StashCode_SoilMoisture) ! IndexCxSoilMoisture
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % SoilMoisture, "SoilMoisture", Cx % Header % NumLocal, Cx % SoilMoisture, &
        self % GeoVals, opsinputs_cxfields_SoilMoisture)
    case (StashCode_SoilTemp) ! IndexCxSoilTemp
      call opsinputs_fill_fillreal2dfromgeoval( &
        Cx % Header % SoilTemp, "SoilTemp", Cx % Header % NumLocal, Cx % SoilTemp, &
        self % GeoVals, opsinputs_cxfields_SoilTemp)
    ! TODO(someone): support dust bins and set NDustBins correctly
    ! CASE (IndexCxDust1, IndexCxDust2, &
      !      IndexCxDust3, IndexCxDust4, &
      !      IndexCxDust5, IndexCxDust6)
      !  CxHdrVrbl = Cx % Header % dustp
      !  IF (Ivar > IndexCxDustMax .OR. .NOT. CxHdrVrbl % Present) THEN
      !    CxHdrVrbl % Present = .FALSE.
      !    CYCLE UairVrbl
      !  ENDIF
      !  DustInd = Ivar - IndexCxDustMin + 1
      !  CxVrblUair => Cx % dustp(DustInd) % field(:,:)
  end select
end do
end subroutine opsinputs_cxwriter_populatecx

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
TYPE (DateTime_type)                 :: now

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

UmHeader % FixHd(FH_SubModel) = FH_SubModel_Atmos
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

now = OpsFn_DateTime_now()
UmHeader % FixHd(FH_CTYear) = now % year
UmHeader % FixHd(FH_CTMonth) = now % month
UmHeader % FixHd(FH_CTDay) = now % day
UmHeader % FixHd(FH_CTHour) = now % hour
UmHeader % FixHd(FH_CTMinute) = now % minute
UmHeader % FixHd(FH_CTSecond) = now % second
UmHeader % FixHd(FH_CTDayNo) = IMDI

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
function opsinputs_cxwriter_retainflag( &
  NumObsLocal, ObsSpace, Flags, &
  RejectObsWithAnyVariableFailingQC, RejectObsWithAllVariablesFailingQC)
implicit none

! Function arguments:
integer(integer64), intent(in) :: NumObsLocal
type(c_ptr), value, intent(in) :: ObsSpace
type(c_ptr), value, intent(in) :: Flags
logical                        :: RejectObsWithAnyVariableFailingQC
logical                        :: RejectObsWithAllVariablesFailingQC

! Return value:
logical(logical64)             :: opsinputs_cxwriter_retainflag(NumObsLocal)

! Local declarations:
integer(integer64)             :: ReportFlags(NumObsLocal)

! Body

call opsinputs_utils_fillreportflags(ObsSpace, Flags, RejectObsWithAnyVariableFailingQC, &
                                     RejectObsWithAllVariablesFailingQC, ReportFlags)

opsinputs_cxwriter_retainflag = .not. btest(ReportFlags, FinalRejectFlag)

end function opsinputs_cxwriter_retainflag

end module opsinputs_cxwriter_mod
