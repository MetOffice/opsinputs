! (C) Copyright 2020 Met Office UK
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

!> Fortran part of CxWriter. It collects data from JEDI and uses OPS functions to output these data
!> to a Cx file.

module cxvarobs_cxwriter_mod

use fckit_configuration_module, only: fckit_configuration
use, intrinsic :: iso_c_binding
use datetime_mod
use kinds
use missing_values_mod
use oops_variables_mod
use obsspace_mod
use ufo_geovals_mod
use ufo_vars_mod
use cxvarobs_obsdatavector_mod
use cxvarobs_obsspace_mod
use cxvarobs_utils_mod

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
use cxvarobs_cxgenerate_mod, only: &
    MaxModelCodes,                 &
    Ops_ReadCXControlNL
use OpsMod_DateTime
use OpsMod_MiscTypes
use OpsMod_ModelColumnIO, only: &
    Ops_WriteOutVarCx1pe
use OpsMod_ObsGroupInfo, only: &
    OpsFn_ObsGroupNameToNum,   &
    ObsGroupAircraft,          &
    ObsGroupGPSRO,             &
    ObsGroupSurface,           &
    ObsGroupSatwind,           &
    ObsGroupScatwind
use OpsMod_ObsInfo
use OpsMod_Stash

implicit none
public :: cxvarobs_cxwriter_create, cxvarobs_cxwriter_delete, &
          cxvarobs_cxwriter_prior, cxvarobs_cxwriter_post
private

! ------------------------------------------------------------------------------
type, public :: cxvarobs_cxwriter
private
  integer(kind=8) :: ObsGroup
  type(datetime)  :: ValidityTime  ! Corresponds to OPS validity time

  logical         :: RejectObsWithAnyVariableFailingQC
  logical         :: RejectObsWithAllVariablesFailingQC

  logical         :: AccountForGPSROTangentPointDrift
  logical         :: UseRadarFamily

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
end type cxvarobs_cxwriter

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------

!> Set up an instance of cxvarobs_cxwriter. Returns .true. on success and .false. on failure.
function cxvarobs_cxwriter_create(self, f_conf, geovars)
implicit none

! Subroutine arguments:
type(cxvarobs_cxwriter), intent(inout) :: self
type(fckit_configuration), intent(in)      :: f_conf  ! Configuration
type(oops_variables), intent(inout)        :: geovars ! GeoVaLs required by the CxWriter.
logical(c_bool)                            :: cxvarobs_cxwriter_create

! Local declarations:
character(len=:), allocatable              :: string
integer(kind=c_int)                        :: int
logical                                    :: bool
real(kind=c_double)                        :: double
logical                                    :: found

integer(kind=8), parameter                 :: zero = 0

integer                                    :: NumLevels
integer                                    :: i

character(len=*), parameter :: RoutineName = "cxvarobs_cxwriter_create"
character(len=200)          :: ErrorMessage

! Body:

cxvarobs_cxwriter_create = .true.

! Setup OPS

string = "normal"
found = f_conf % get("general_mode", string)
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
  cxvarobs_cxwriter_create = .false.
  goto 9999
end select

call Gen_SetupControl(DefaultDocURL)
call Ops_InitMPI

! Retrieve parameter values from the input configuration object
! and store them in member variables

if (.not. f_conf % get("obs_group", string)) then
  call gen_warn(RoutineName, "Mandatory obs_group option not found")
  cxvarobs_cxwriter_create = .false.
  goto 9999
end if
self % ObsGroup = OpsFn_ObsGroupNameToNum(string)

if (.not. f_conf % get("validity_time", string)) then
  call gen_warn(RoutineName, "Mandatory validity_time option not found")
  cxvarobs_cxwriter_create = .false.
  goto 9999
end if
call datetime_create(string, self % validitytime)

self % RejectObsWithAnyVariableFailingQC = .false.
found = f_conf % get("reject_obs_with_any_variable_failing_qc", &
                     self % RejectObsWithAnyVariableFailingQC)

self % RejectObsWithAllVariablesFailingQC = .false.
found = f_conf % get("reject_obs_with_all_variables_failing_qc", &
                     self % RejectObsWithAllVariablesFailingQC)

self % AccountForGPSROTangentPointDrift = .false.
found = f_conf % get("account_for_gpsro_tangent_point_drift", &
                     self % AccountForGPSROTangentPointDrift)

self % UseRadarFamily = .false.
found = f_conf % get("use_radar_family", self % UseRadarFamily)

string = "hybrid"  ! TODO(wsmigaj): is this a good default?
found = f_conf % get("FH_VertCoord", string)
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
  cxvarobs_cxwriter_create = .false.
  goto 9999
end select

string = "global"
found = f_conf % get("FH_HorizGrid", string)
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
  cxvarobs_cxwriter_create = .false.
  goto 9999
end select

string = "endgame"
found = f_conf % get("FH_GridStagger", string)
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
  cxvarobs_cxwriter_create = .false.
  goto 9999
end select

string = "atmos"
found = f_conf % get("FH_ObsFileType", string)
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
  cxvarobs_cxwriter_create = .false.
  goto 9999
end select

int = 0
found = f_conf % get("FH_ModelVersion", int)
self % FH_ModelVersion = int

int = 0
found = f_conf % get("IC_XLen", int)
self % IC_XLen = int

int = 0
found = f_conf % get("IC_YLen", int)
self % IC_YLen = int

int = 0
found = f_conf % get("IC_PLevels", int)
self % IC_PLevels = int

int = 0
found = f_conf % get("IC_WetLevels", int)
self % IC_WetLevels = int

double = 0.0
found = f_conf % get("RC_LongSpacing", double)
self % RC_LongSpacing = double

double = 0.0
found = f_conf % get("RC_LatSpacing", double)
self % RC_LatSpacing = double

double = 0.0
found = f_conf % get("RC_FirstLat", double)
self % RC_FirstLat = double

double = 0.0
found = f_conf % get("RC_FirstLong", double)
self % RC_FirstLong = double

double = 0.0
found = f_conf % get("RC_PoleLat", double)
self % RC_PoleLat = double

double = 0.0
found = f_conf % get("RC_PoleLong", double)
self % RC_PoleLong = double

! TODO(wsmigaj): Retrieve these vectors from the configuration
NumLevels = self % IC_PLevels
allocate(self % EtaTheta(NumLevels + 1))
self % EtaTheta = (/(i, i = 0, NumLevels)/)
allocate(self % EtaRho(NumLevels))
self % EtaRho = (/(i, i = 100 + 1, 100 + NumLevels)/)

int = 0
found = f_conf % get("time_indicator", int)
self % TimeIndicator = int

int = 0
found = f_conf % get("forecast_period", int)
self % ForecastPeriod = int

! Fill in the list of GeoVaLs that will be needed to populate the requested varfields.
call cxvarobs_cxwriter_addrequiredgeovars(self, geovars)

9999 if (allocated(string)) deallocate(string)

end function cxvarobs_cxwriter_create

! ------------------------------------------------------------------------------

!> Destroy an instance of cxvarobs_cxwriter.
subroutine cxvarobs_cxwriter_delete(self)
implicit none

! Subroutine arguments:
type(cxvarobs_cxwriter), intent(inout) :: self

! Body:
call datetime_delete(self % validitytime)

if (allocated(self % EtaRho)) deallocate(self % EtaRho)
if (allocated(self % EtaTheta)) deallocate(self % EtaTheta)

end subroutine cxvarobs_cxwriter_delete

! ------------------------------------------------------------------------------

!> Called by the priorFilter() method of the C++ CxWriter object.
!>
!> Set the GeoVals pointer.
subroutine cxvarobs_cxwriter_prior(self, ObsSpace, GeoVals)
implicit none

! Subroutine arguments:
type(cxvarobs_cxwriter), intent(inout) :: self
type(c_ptr), value, intent(in)             :: ObsSpace
type(ufo_geovals), intent(in), pointer     :: GeoVals

! Body:
self % GeoVals => GeoVals

end subroutine cxvarobs_cxwriter_prior

! ------------------------------------------------------------------------------

!> Called by the postFilter() method of the C++ CxWriter object.
!>
!> Write out a Cx file containing varfields derived from JEDI variables.
subroutine cxvarobs_cxwriter_post( &
  self, ObsSpace, nchannels, Channels, Flags, ObsErrors, nvars, nlocs, hofx)
implicit none

! Subroutine arguments:
type(cxvarobs_cxwriter), intent(in) :: self
type(c_ptr), value, intent(in) :: ObsSpace
integer,            intent(in) :: nchannels
integer,            intent(in) :: Channels(nchannels)
type(c_ptr), value, intent(in) :: Flags, ObsErrors
integer,            intent(in) :: nvars, nlocs
real(c_double),     intent(in) :: hofx(nvars, nlocs)

! Local declarations:
integer                        :: NumObsLocal
integer, allocatable           :: RetainedObsIndices(:)
type(OB_type)                  :: Ob(1)
type(CX_type)                  :: Cx(1)
type(UM_header_type)           :: UMHeader

! Body:

NumObsLocal = obsspace_get_nlocs(ObsSpace)
RetainedObsIndices = cxvarobs_cxwriter_retainedobsindices( &
  NumObsLocal, ObsSpace, Flags, &
  self % RejectObsWithAnyVariableFailingQC, self % RejectObsWithAllVariablesFailingQC)

call cxvarobs_cxwriter_allocateobservations(self, ObsSpace, RetainedObsIndices, Ob(1))
call cxvarobs_cxwriter_populateobservations(self, ObsSpace, Channels, Flags, ObsErrors, &
                                            NumObsLocal, RetainedObsIndices, Ob(1))
call cxvarobs_cxwriter_allocatecx(self, Ob(1), Cx(1))
call cxvarobs_cxwriter_populatecx(self, ObsSpace, Channels, Flags, ObsErrors, RetainedObsIndices, &
                                  Cx(1))
call cxvarobs_cxwriter_populateumheader(self, UMHeader)

call Ops_WriteOutVarCx1pe (Ob, Cx, [UMheader % IntC(IC_Plevels)], UMheader)

call UMheader % dealloc()
call Cx(1) % deallocate()
call Ob(1) % deallocate()
deallocate(RetainedObsIndices)

end subroutine cxvarobs_cxwriter_post

! ------------------------------------------------------------------------------

!> Populate the list of GeoVaLs needed to fill in any requested cxfields.
subroutine cxvarobs_cxwriter_addrequiredgeovars(self, geovars)
implicit none

! Subroutine arguments:
type(cxvarobs_cxwriter), intent(in) :: self
type(oops_variables), intent(inout) :: geovars

! Local declarations:
integer(kind=8)                     :: CxFields(MaxModelCodes) ! MaxModelCodes)
integer                             :: i

! Body:
call Ops_ReadCXControlNL(self % obsgroup, CxFields, BGECall = .false._8, ops_call = .false._8)

do i = 1, size(CxFields)
  select case (CxFields(i))
  case (StashItem_theta)
    call geovars % push_back("air_potential_temperature")
  case (StashItem_modelsurface)
    ! TODO(someone): "land_type_index" may not be the right geoval to use. If it isn't, change it
    ! here and in cxvarobs_cxwriter_populatecx.
    call geovars % push_back("land_type_index")
  end select
end do

end subroutine cxvarobs_cxwriter_addrequiredgeovars

! ------------------------------------------------------------------------------

!> Prepare Ob to hold the required number of observations.
subroutine cxvarobs_cxwriter_allocateobservations(self, ObsSpace, RetainedObsIndices, Ob)
implicit none

! Subroutine arguments:
type(cxvarobs_cxwriter), intent(in) :: self
type(c_ptr), value, intent(in)      :: ObsSpace
integer, intent(in)                 :: RetainedObsIndices(:)
type(OB_type), intent(inout)        :: Ob

! Local declarations:
integer(kind=8)                     :: istat

! Body:
Ob % Header % obsgroup = self % obsgroup

call Ops_SetupObType(Ob)

Ob % Header % NumObsLocal = size(RetainedObsIndices)
Ob % Header % NumObsTotal = Ob % Header % NumObsLocal
call gcg_isum (1, mpi_group, istat, Ob % Header % NumObsTotal)

Ob % Header % NumCXBatches = 1
allocate(Ob % Header % ObsPerBatchPerPE(Ob % Header % NumCXBatches, 0:nproc - 1))
Ob % Header % ObsPerBatchPerPE(1,mype) = Ob % Header % NumObsLocal

end subroutine cxvarobs_cxwriter_allocateobservations

! ------------------------------------------------------------------------------

!> Populate Ob fields required by the OPS routine writing a Cx file.
subroutine cxvarobs_cxwriter_populateobservations( &
  self, ObsSpace, Channels, Flags, ObsErrors, NumObsLocal, RetainedObsIndices, Ob)
implicit none

! Subroutine arguments:
type(cxvarobs_cxwriter), intent(in)     :: self
type(c_ptr), value, intent(in)          :: ObsSpace
integer(c_int), intent(in)              :: Channels(:)
type(c_ptr), value, intent(in)          :: Flags, ObsErrors
! Number of observations held in ObsSpace by this process (including rejected ones)
integer, intent(in)                     :: NumObsLocal
! Indices of retained observations (held by this process)
integer, intent(in)                     :: RetainedObsIndices(:)
type(OB_type), intent(inout)            :: Ob

! Local declarations:
integer(c_int)                          :: year, month, day, hour, minute, second
real(c_double)                          :: Reals(NumObsLocal)
integer(c_int64_t)                      :: TimeOffsetsInSeconds(NumObsLocal)

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
call cxvarobs_cxwriter_selectretainedobservations(Reals, RetainedObsIndices, Ob % Latitude)

call Ops_Alloc(Ob % Header % Longitude, "Longitude", Ob % Header % NumObsLocal, Ob % Longitude)
call obsspace_get_db(ObsSpace, "MetaData", "longitude", Reals)
call cxvarobs_cxwriter_selectretainedobservations(Reals, RetainedObsIndices, Ob % Longitude)

call Ops_Alloc(Ob % Header % Time, "Time", Ob % Header % NumObsLocal, Ob % Time)
call cxvarobs_obsspace_get_db_datetime_offset_in_seconds( &
  ObsSpace, "MetaData", "datetime", self % validitytime, TimeOffsetsInSeconds)
Reals = TimeOffsetsInSeconds
call cxvarobs_cxwriter_selectretainedobservations(Reals, RetainedObsIndices, Ob % Time)

end subroutine cxvarobs_cxwriter_populateobservations

! ------------------------------------------------------------------------------

!> Prepare Cx to hold the required number of model columns.
subroutine cxvarobs_cxwriter_allocatecx(self, Ob, Cx)
implicit none

! Subroutine arguments:
type(cxvarobs_cxwriter), intent(in)     :: self
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

end subroutine cxvarobs_cxwriter_allocatecx

! ------------------------------------------------------------------------------

!> Populate Cx fields required by the OPS routine writing a Cx file.
subroutine cxvarobs_cxwriter_populatecx( &
  self, ObsSpace, Channels, Flags, ObsErrors, RetainedObsIndices, Cx)
implicit none

! Subroutine arguments:
type(cxvarobs_cxwriter), intent(in)     :: self
type(c_ptr), value, intent(in)          :: ObsSpace
integer(c_int), intent(in)              :: Channels(:)
type(c_ptr), value, intent(in)          :: Flags, ObsErrors
! Indices of retained observations (held by this process)
integer, intent(in)                     :: RetainedObsIndices(:)
type(CX_type), intent(inout)            :: Cx

! Local declarations:
character(len=*), parameter             :: RoutineName = "cxvarobs_cxwriter_populatecx"
character(len=80)                       :: ErrorMessage

integer(kind=8)                         :: CxFields(MaxModelCodes)
integer                                 :: nCxFields
integer                                 :: iCxField

! Body:
call Ops_ReadCXControlNL(self % obsgroup, CxFields, BGECall = .false._8, ops_call = .false._8)

do iCxField = 1, size(CxFields)
  select case (CxFields(iCxField))
  case (StashItem_theta)
    call cxvarobs_cxwriter_fillreal2dfromgeoval( &
      Cx % Header % theta, "theta", Cx % theta, &
      self % GeoVals, RetainedObsIndices, "air_potential_temperature")
  case (StashItem_modelsurface)
    ! TODO(someone): "land_type_index" may not be the right geoval to use. If it isn't, change it
    ! here and in cxvarobs_cxwriter_addrequiredgeovars.
    call cxvarobs_cxwriter_fillrealfromgeoval( &
      Cx % Header % ModelSurface, "ModelSurface", Cx % ModelSurface, &
      self % GeoVals, RetainedObsIndices, "land_type_index")
  end select
end do
end subroutine cxvarobs_cxwriter_populatecx

! ------------------------------------------------------------------------------

!> Prepare a UM header object to be given to the OPS function writing a Cx file.
subroutine cxvarobs_cxwriter_populateumheader(self, UMHeader)
implicit none
! Subroutine arguments:
type(cxvarobs_cxwriter), intent(in) :: self
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
UmHeader % FixHd(FH_DTDayNo) = 0  ! TODO(wsmigaj): What should this be set to?

UmHeader % FixHd(FH_VTYear) = year
UmHeader % FixHd(FH_VTMonth) = month
UmHeader % FixHd(FH_VTDay) = day
UmHeader % FixHd(FH_VTHour) = hour
UmHeader % FixHd(FH_VTMinute) = minute
UmHeader % FixHd(FH_VTSecond) = second
UmHeader % FixHd(FH_VTDayNo) = 0  ! TODO(wsmigaj): What should this be set to?

now = OpsFn_DateTime_now()
UmHeader % FixHd(FH_CTYear) = now % year
UmHeader % FixHd(FH_CTMonth) = now % month
UmHeader % FixHd(FH_CTDay) = now % day
UmHeader % FixHd(FH_CTHour) = now % hour
UmHeader % FixHd(FH_CTMinute) = now % minute
UmHeader % FixHd(FH_CTSecond) = now % second
UmHeader % FixHd(FH_CTDayNo) = 0  ! TODO(wsmigaj): What should this be set to?

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

end subroutine cxvarobs_cxwriter_populateumheader

! ------------------------------------------------------------------------------

!> Populate a 1D array of real numbers and its header from a JEDI variable.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Real1 corresponds.
!> \param[in] NumObs
!>   Number of observations held by this process.
!> \param[inout] Real1
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace object containing the specified JEDI variable.
!> \param[in] JediVarName
!>   Name of the JEDI variable used to populate \p Real1.
!> \param[in] JediGroup
!>   Group of the JEDI variable used to populate \p Real1.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine cxvarobs_cxwriter_fillreal( &
  Hdr, OpsVarName, NumObs, Real1, ObsSpace, JediVarName, JediVarGroup)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(kind=8), intent(in)                     :: NumObs
real(kind=8), pointer                           :: Real1(:)
type(c_ptr), value, intent(in)                  :: ObsSpace
character(len=*), intent(in)                    :: JediVarName
character(len=*), intent(in)                    :: JediVarGroup

! Local declarations:
real(kind=c_double)                             :: VarValue(NumObs)
real(kind=c_double)                             :: MissingDouble
integer                                         :: i

! Body:

MissingDouble = missing_value(0.0_c_double)

if (obsspace_has(ObsSpace, JediVarGroup, JediVarName)) then
  ! Retrieve data from JEDI
  call obsspace_get_db(ObsSpace, JediVarGroup, JediVarName, VarValue)

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, Real1)
  do i = 1, NumObs
    if (VarValue(i) /= MissingDouble) Real1(i) = VarValue(i)
  end do
end if
end subroutine cxvarobs_cxwriter_fillreal

! ------------------------------------------------------------------------------

!> Populate a 1D array of real numbers and its header from a GeoVaL.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Real1 corresponds.
!> \param[inout] Real1
!>   Pointer to the array to be populated.
!> \param[in] GeoVals
!>   A container holding the specified GeoVaL.
!> \param[in] RetainedObsIndices
!>   Indices of retained observations held by this process.
!> \param[in] JediVarName
!>   Name of the GeoVaL used to populate \p Real1.
!>
!> \note If you're calling this function from cxvarobs_cxwriter_populateobservations, be sure
!> to update cxvarobs_cxwriter_addrequiredgeovars by adding \p JediVarName to the list of
!> GeoVaLs required by the CxWriter.
!>
!> \note This function returns early (without a warning) if the specified GeoVaL is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine cxvarobs_cxwriter_fillrealfromgeoval( &
  Hdr, OpsVarName, Real1, GeoVals, RetainedObsIndices, JediVarName)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
real(kind=8), pointer                           :: Real1(:)
type(ufo_geovals), intent(in)                   :: GeoVals
integer, intent(in)                             :: RetainedObsIndices(:)
character(len=*), intent(in)                    :: JediVarName

! Local declarations:
integer(kind=8)                                 :: NumRetainedObs
real(kind_real)                                 :: MissingReal
type(ufo_geoval), pointer                       :: GeoVal
integer                                         :: i, iIn, iOut

character(len=*), parameter                     :: &
  RoutineName = "cxvarobs_cxwriter_fillrealfromgeoval"
character(len=256)                              :: ErrorMessage

! Body:

NumRetainedObs = size(RetainedObsIndices)
MissingReal = missing_value(0.0_c_float)

if (ufo_vars_getindex(GeoVals % variables, JediVarName) > 0) then
  ! Retrieve GeoVal
  call ufo_geovals_get_var(GeoVals, JediVarName, GeoVal)
  if (GeoVal % nval /= 1) then
    write (ErrorMessage, '("GeoVal ",A," contains more than one value per location. &
      &Only the first of these values will be written to the VarObs file")') JediVarName
    call gen_warn(RoutineName, ErrorMessage)
  end if

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumRetainedObs, Real1)
  iOut = 1
  do i = 1, NumRetainedObs
    iIn = RetainedObsIndices(i)
    if (GeoVal % vals(1,iIn) /= MissingReal) Real1(iOut) = GeoVal % vals(1,iIn)
    iOut = iOut + 1
  end do
end if
end subroutine cxvarobs_cxwriter_fillrealfromgeoval

! ------------------------------------------------------------------------------

!> Populate a 2D array of real numbers and its header from a GeoVaL.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Real1 corresponds.
!> \param[inout] Real2
!>   Pointer to the array to be populated.
!> \param[in] GeoVals
!>   A container holding the specified GeoVaL.
!> \param[in] RetainedObsIndices
!>   Indices of retained observations held by this process.
!> \param[in] JediVarName
!>   Name of the GeoVal used to populate \p Real2.
!>
!> \note This function returns early (without a warning) if the specified GeoVaL is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine cxvarobs_cxwriter_fillreal2dfromgeoval( &
  Hdr, OpsVarName, Real2, GeoVals, RetainedObsIndices, JediVarName)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
real(kind=8), pointer                           :: Real2(:,:)
type(ufo_geovals), intent(in)                   :: GeoVals
integer, intent(in)                             :: RetainedObsIndices(:)
character(len=*), intent(in)                    :: JediVarName

! Local declarations:
integer(kind=8)                                 :: NumRetainedObs
type(ufo_geoval), pointer                       :: GeoVal
real(kind_real)                                 :: MissingReal
integer                                         :: i, iIn, iOut

! Body:

NumRetainedObs = size(RetainedObsIndices)
MissingReal = missing_value(0.0_c_float)

if (ufo_vars_getindex(GeoVals % variables, JediVarName) > 0) then
  ! Retrieve GeoVal
  call ufo_geovals_get_var(GeoVals, JediVarName, GeoVal)

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumRetainedObs, Real2, num_levels = int(GeoVal % nval, kind = 8))

  iOut = 1
  do i = 1, NumRetainedObs
    iIn = RetainedObsIndices(i)
    where (GeoVal % vals(:,iIn) /= MissingReal)
      Real2(iOut,:) = GeoVal % vals(:,iIn)
    end where
    iOut = iOut + 1
  end do
end if
end subroutine cxvarobs_cxwriter_fillreal2dfromgeoval

! ------------------------------------------------------------------------------

function cxvarobs_cxwriter_retainedobsindices( &
  NumObsLocal, ObsSpace, Flags, &
  RejectObsWithAnyVariableFailingQC, RejectObsWithAllVariablesFailingQC)
implicit none

! Function arguments:
integer, intent(in)                 :: NumObsLocal
type(c_ptr), value, intent(in)      :: ObsSpace
type(c_ptr), value, intent(in)      :: Flags
logical                             :: RejectObsWithAnyVariableFailingQC
logical                             :: RejectObsWithAllVariablesFailingQC

! Return value:
integer, allocatable                :: cxvarobs_cxwriter_retainedobsindices(:)

! Local declarations:
integer(kind=8)                     :: ReportFlags(NumObsLocal)
integer                             :: NumRetainedObs
integer                             :: iObs, iRetainedObs

! Body

call cxvarobs_utils_fillreportflags(ObsSpace, Flags, RejectObsWithAnyVariableFailingQC, &
                                    RejectObsWithAllVariablesFailingQC, ReportFlags)

NumRetainedObs = NumObsLocal - count(btest(ReportFlags, FinalRejectFlag))
allocate(cxvarobs_cxwriter_retainedobsindices(NumRetainedObs))

iRetainedObs = 1
do iObs = 1, NumObsLocal
  if (.not. btest(ReportFlags(iObs), FinalRejectFlag)) then
    cxvarobs_cxwriter_retainedobsindices(iRetainedObs) = iObs
    iRetainedObs = iRetainedObs + 1
  end if
end do

end function cxvarobs_cxwriter_retainedobsindices

! ------------------------------------------------------------------------------

!> Select the elements of \p Obs with indices \p RetainedObsIndices and store them in
!> \p RetainedObs.
subroutine cxvarobs_cxwriter_selectretainedobservations(Obs, RetainedObsIndices, RetainedObs)
implicit none

! Subroutine arguments:
real(c_double) :: Obs(:)
integer        :: RetainedObsIndices(:)
real(kind=8)   :: RetainedObs(:)

! Local declarations:
integer        :: iIn, iOut

! Body:
do iOut = 1, size(RetainedObsIndices)
  iIn = RetainedObsIndices(iOut)
  RetainedObs(iOut) = Obs(iIn)
end do

end subroutine cxvarobs_cxwriter_selectretainedobservations

end module cxvarobs_cxwriter_mod
