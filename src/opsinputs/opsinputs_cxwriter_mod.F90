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
use OpsMod_CharUtils, only: ops_to_lower_case
use OpsMod_Constants, only: PPF ! PGE packing factor
use OpsMod_Control, only:   &
    DefaultDocURL,          &
    ModelType,              &
    ModelType_Ocean,        &
    ModelType_SST,          &
    mpi_group,              &
    Ops_InitMPI
use OpsMod_CXInfo, only: &
    CXheader_type, &
    CX_type
use opsinputs_cxgenerate_mod, only: &
    MaxModelCodes,                 &
    Ops_ReadCXControlNL
use OpsMod_DateTime, only: &
    DateTime_type,         &
    OpsFn_DateTime_now
use OpsMod_MiscTypes, only: ElementHeader_Type
use OpsMod_ModelColumnIO, only: &
    Ops_WriteOutVarCx,      &
    Ops_WriteOutVarCx1pe
use OpsMod_ObsGroupInfo, only: &
    OpsFn_ObsGroupNameToNum,   &
    ObsGroupAircraft,          &
    ObsGroupGPSRO,             &
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
  integer(kind=8) :: ObsGroup
  type(datetime)  :: ValidityTime  ! Corresponds to OPS validity time

  logical         :: RejectObsWithAnyVariableFailingQC
  logical         :: RejectObsWithAllVariablesFailingQC

  integer(kind=8) :: FH_VertCoord
  integer(kind=8) :: FH_HorizGrid
  integer(kind=8) :: FH_GridStagger
  integer(kind=8) :: FH_ObsFileType
  integer(kind=8) :: FH_ModelVersion

  integer(kind=8) :: IC_XLen
  integer(kind=8) :: IC_YLen
  integer(kind=8) :: IC_PLevels
  integer(kind=8) :: IC_WetLevels

  real(kind=8)    :: RC_LongSpacing
  real(kind=8)    :: RC_LatSpacing
  real(kind=8)    :: RC_FirstLat
  real(kind=8)    :: RC_FirstLong
  real(kind=8)    :: RC_PoleLat
  real(kind=8)    :: RC_PoleLong

  integer(kind=8) :: TimeIndicator
  integer(kind=8) :: ForecastPeriod

  real(kind=8), allocatable :: EtaTheta(:)
  real(kind=8), allocatable :: EtaRho(:)

  type(ufo_geovals), pointer :: GeoVals
end type opsinputs_cxwriter

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------

!> Set up an instance of opsinputs_cxwriter. Returns .true. on success and .false. on failure.
function opsinputs_cxwriter_create(self, f_conf, geovars)
implicit none

! Subroutine arguments:
type(opsinputs_cxwriter), intent(inout)    :: self
type(fckit_configuration), intent(in)      :: f_conf  ! Configuration
type(oops_variables), intent(inout)        :: geovars ! GeoVaLs required by the CxWriter.
logical                                    :: opsinputs_cxwriter_create

! Local declarations:
character(len=:), allocatable              :: string
integer                                    :: int
logical                                    :: bool
real(kind=c_double)                        :: double
logical                                    :: found

integer(kind=8), parameter                 :: zero = 0

integer                                    :: i

character(len=*), parameter :: RoutineName = "opsinputs_cxwriter_create"
character(len=200)          :: ErrorMessage

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
  goto 9999
end select

call Gen_SetupControl(DefaultDocURL)
call Ops_InitMPI

! Retrieve parameter values from the input configuration object
! and store them in member variables

if (.not. f_conf % get("obs_group", string)) then
  call gen_warn(RoutineName, "Mandatory obs_group option not found")
  opsinputs_cxwriter_create = .false.
  goto 9999
end if
self % ObsGroup = OpsFn_ObsGroupNameToNum(string)

if (.not. f_conf % get("validity_time", string)) then
  call gen_warn(RoutineName, "Mandatory validity_time option not found")
  opsinputs_cxwriter_create = .false.
  goto 9999
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
  goto 9999
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
  goto 9999
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
  goto 9999
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
  goto 9999
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

if (f_conf % get("eta_theta_levels", self % EtaTheta)) then
  if (size(self % EtaTheta) /= self % IC_PLevels + 1) then
    call gen_warn(RoutineName, "eta_theta_levels should be a vector of length (IC_PLevels + 1)")
    opsinputs_cxwriter_create = .false.
    goto 9999
  end if
else
  allocate(self % EtaTheta(self % IC_PLevels + 1))
  self % EtaTheta = RMDI
end if

if (f_conf % get("eta_rho_levels", self % EtaRho)) then
  if (size(self % EtaRho) /= self % IC_PLevels) then
    call gen_warn(RoutineName, "eta_theta_levels should be a vector of length (IC_PLevels + 1)")
    opsinputs_cxwriter_create = .false.
    goto 9999
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

! Fill in the list of GeoVaLs that will be needed to populate the requested varfields.
call opsinputs_cxwriter_addrequiredgeovars(self, geovars)

9999 if (allocated(string)) deallocate(string)

end function opsinputs_cxwriter_create

! ------------------------------------------------------------------------------

!> Destroy an instance of opsinputs_cxwriter.
subroutine opsinputs_cxwriter_delete(self)
implicit none

! Subroutine arguments:
type(opsinputs_cxwriter), intent(inout) :: self

! Body:
call datetime_delete(self % validitytime)

if (allocated(self % EtaRho)) deallocate(self % EtaRho)
if (allocated(self % EtaTheta)) deallocate(self % EtaTheta)

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
integer(kind=8)                      :: NumObsLocal

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
integer(kind=8)                :: NumObsLocal
type(c_ptr), value, intent(in) :: ObsSpace
type(c_ptr), value, intent(in) :: Flags

! Local declarations:
type(OB_type)                  :: Ob
type(CX_type)                  :: Cx
type(UM_header_type)           :: UMHeader
integer(kind=8)                :: NumObsOnEachRank(nproc)
logical(kind=8)                :: Retained(NumObsLocal)
integer(kind=gc_int_kind)      :: istat

! Body:

call opsinputs_cxwriter_allocateobservations(self, ObsSpace, NumObsLocal, Ob)
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
type(oops_variables), intent(inout) :: geovars

! Local declarations:
integer(kind=8)                     :: CxFields(MaxModelCodes) ! MaxModelCodes)
integer                             :: i

! Body:
call Ops_ReadCXControlNL(self % obsgroup, CxFields, BGECall = .false._8, ops_call = .false._8)

do i = 1, size(CxFields)
  select case (CxFields(i))
  case (StashItem_u)
    call geovars % push_back("eastward_wind")
  case (StashItem_v)
    call geovars % push_back("northward_wind")
  case (StashItem_theta)
    call geovars % push_back("air_potential_temperature")
  case (StashItem_modelsurface)
    ! TODO(someone): "land_type_index" may not be the right geoval to use. If it isn't, change it
    ! here and in opsinputs_cxwriter_populatecx.
    call geovars % push_back("land_type_index")
  end select
end do

end subroutine opsinputs_cxwriter_addrequiredgeovars

! ------------------------------------------------------------------------------

!> Prepare Ob to hold the required number of observations.
subroutine opsinputs_cxwriter_allocateobservations(self, ObsSpace, NumObsLocal, Ob)
use mpl, ONLY: gc_int_kind
implicit none

! Subroutine arguments:
type(opsinputs_cxwriter), intent(in) :: self
type(c_ptr), value, intent(in)       :: ObsSpace
integer(kind=8), intent(in)          :: NumObsLocal
type(OB_type), intent(inout)         :: Ob

! Local declarations:
integer(kind=gc_int_kind)            :: istat

! Body:
Ob % Header % obsgroup = self % obsgroup

call Ops_SetupObType(Ob)

Ob % Header % NumObsLocal = NumObsLocal
Ob % Header % NumObsTotal = Ob % Header % NumObsLocal
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
type(opsinputs_cxwriter), intent(in)     :: self
type(OB_type), intent(in)               :: Ob
type(CX_type), intent(inout)            :: Cx

! Body:

call CX % init

! TODO(wsmigaj): calculate the number of non-rejected observations

! From Ops_CXSetup

Cx % Header % NumLocal = Ob % Header % NumObsLocal
Cx % Header % NumTotal = Ob % Header % NumObsTotal
Cx % Header % ModelVersion = self % FH_ModelVersion
Cx % Header % SubModel = FH_SubModel_Atmos
! TODO(wsmigaj): to use this criterion, ModelType would need to be set appropriately.
Cx % Header % NewDynamics = self % FH_ModelVersion >= 500 .and. ModelType /= ModelType_Ocean

! TODO(wsmigaj)
!IF (Cx % Header % NewDynamics) THEN
!  IF (.NOT. BGECall .AND. ModelType /= ModelType_SST) THEN
!    Cx % Header % FirstConstantRhoLevel = UMheader % IntC(IC_FirstConstantRhoLevel)
!  END IF
!ELSE
!  Cx % Header % FirstConstantRhoLevel = IMDI
!END IF

end subroutine opsinputs_cxwriter_allocatecx

! ------------------------------------------------------------------------------

!> Populate Cx fields required by the OPS routine writing a Cx file.
subroutine opsinputs_cxwriter_populatecx(self, ObsSpace, Flags, Cx)
implicit none

! Subroutine arguments:
type(opsinputs_cxwriter), intent(in)     :: self
type(c_ptr), value, intent(in)          :: ObsSpace
type(c_ptr), value, intent(in)          :: Flags
type(CX_type), intent(inout)            :: Cx

! Local declarations:
character(len=*), parameter             :: RoutineName = "opsinputs_cxwriter_populatecx"
character(len=80)                       :: ErrorMessage

integer(kind=8)                         :: CxFields(MaxModelCodes)
integer                                 :: nCxFields
integer                                 :: iCxField

! Body:
call Ops_ReadCXControlNL(self % obsgroup, CxFields, BGECall = .false._8, ops_call = .false._8)

do iCxField = 1, size(CxFields)
  select case (CxFields(iCxField))
  case (StashItem_u)
    call opsinputs_fill_fillreal2dfromgeoval( &
      Cx % Header % u, "u", Cx % Header % NumLocal, Cx % u, &
      self % GeoVals, "eastward_wind")
  case (StashItem_v)
    call opsinputs_fill_fillreal2dfromgeoval( &
      Cx % Header % v, "v", Cx % Header % NumLocal, Cx % v, &
      self % GeoVals, "northward_wind")
  case (StashItem_theta)
    call opsinputs_fill_fillreal2dfromgeoval( &
      Cx % Header % theta, "theta", Cx % Header % NumLocal, Cx % theta, &
      self % GeoVals, "air_potential_temperature")
  case (StashItem_modelsurface)
    ! TODO(someone): "land_type_index" may not be the right geoval to use. If it isn't, change it
    ! here and in opsinputs_cxwriter_addrequiredgeovars.
    call opsinputs_fill_fillrealfromgeoval( &
      Cx % Header % ModelSurface, "ModelSurface", Cx % Header % NumLocal, Cx % ModelSurface, &
      self % GeoVals, "land_type_index")
  end select
end do
end subroutine opsinputs_cxwriter_populatecx

! ------------------------------------------------------------------------------

!> Prepare a UM header object to be given to the OPS function writing a Cx file.
subroutine opsinputs_cxwriter_populateumheader(self, UMHeader)
implicit none
! Subroutine arguments:
type(opsinputs_cxwriter), intent(in) :: self
type(UM_header_type), intent(inout) :: UmHeader

! Local declarations:
integer                             :: NumLevels
integer(c_int)                      :: year, month, day, hour, minute, second
TYPE (DateTime_type)                :: now

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

UmHeader % RealC = RMDI
UmHeader % RealC(RC_LongSpacing) = self % RC_LongSpacing
UmHeader % RealC(RC_LatSpacing) = self % RC_LatSpacing
UmHeader % RealC(RC_FirstLat) = self % RC_FirstLat
UmHeader % RealC(RC_FirstLong) = self % RC_FirstLong
UmHeader % RealC(RC_PoleLat) = self % RC_PoleLat
UmHeader % RealC(RC_PoleLong) = self % RC_PoleLong

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
integer(kind=8), intent(in)         :: NumObsLocal
type(c_ptr), value, intent(in)      :: ObsSpace
type(c_ptr), value, intent(in)      :: Flags
logical                             :: RejectObsWithAnyVariableFailingQC
logical                             :: RejectObsWithAllVariablesFailingQC

! Return value:
logical(kind=8)                     :: opsinputs_cxwriter_retainflag(NumObsLocal)

! Local declarations:
integer(kind=8)                     :: ReportFlags(NumObsLocal)

! Body

call opsinputs_utils_fillreportflags(ObsSpace, Flags, RejectObsWithAnyVariableFailingQC, &
                                     RejectObsWithAllVariablesFailingQC, ReportFlags)

opsinputs_cxwriter_retainflag = .not. btest(ReportFlags, FinalRejectFlag)

end function opsinputs_cxwriter_retainflag

end module opsinputs_cxwriter_mod
