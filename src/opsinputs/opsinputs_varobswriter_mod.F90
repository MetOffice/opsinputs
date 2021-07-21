! (C) Crown Copyright 2020, the Met Office. All rights reserved.
!
! Refer to COPYRIGHT.txt of this distribution for details.

!> Fortran part of VarObsWriter. It collects data from JEDI and uses the functions from the OPS
!> OpsMod_Varobs module to output them to a VarObs file.

module opsinputs_varobswriter_mod

use fckit_configuration_module, only: fckit_configuration
use, intrinsic :: iso_c_binding, only: &
    c_double,                          &
    c_float,                           &
    c_int,                             &
    c_int64_t,                         &
    c_ptr
use datetime_mod, only:        &
    datetime,                  &
    datetime_create,           &
    datetime_delete,           &
    datetime_to_YYYYMMDDhhmmss
use missing_values_mod, only: missing_value
use obsspace_mod, only:  &
    obsspace_get_db,     &
    obsspace_get_gnlocs, &
    obsspace_get_nlocs,  &
    obsspace_has
use oops_variables_mod, only: oops_variables
use opsinputs_fill_mod, only: &
    opsinputs_fill_fillelementtypefromsimulatedvariable, &
    opsinputs_fill_fillelementtype2dfromsimulatedvariable, &
    opsinputs_fill_fillelementtypefromnormalvariable, &
    opsinputs_fill_fillelementtype2dfromnormalvariable, &
    opsinputs_fill_fillreal, &
    opsinputs_fill_fillreal2d, &
    opsinputs_fill_fillrealfromgeoval, &
    opsinputs_fill_fillreal2dfromgeoval, &
    opsinputs_fill_fillreal2dfrom1dgeovalwithchans, &
    opsinputs_fill_fillinteger, &
    opsinputs_fill_fillcoord2d, &
    opsinputs_fill_varnames_with_channels
use opsinputs_obsdatavector_mod, only:    &
    opsinputs_obsdatavector_int_has,      &
    opsinputs_obsdatavector_int_get,      &
    opsinputs_obsdatavector_int_varnames, &
    opsinputs_obsdatavector_float_has,    &
    opsinputs_obsdatavector_float_get
use opsinputs_obsspace_mod, only:                        &
    opsinputs_obsspace_get_db_string,                    &
    opsinputs_obsspace_get_db_datetime_offset_in_seconds
use opsinputs_utils_mod, only:       &
    max_varname_length,              &
    opsinputs_utils_fillreportflags, &
    max_varname_with_channel_length
use ufo_geovals_mod, only: &
    ufo_geoval,            &
    ufo_geovals,           &
    ufo_geovals_get_var
use ufo_vars_mod, only: &
    MAXVARLEN,          &
    ufo_vars_getindex

use mpl, only: gc_int_kind, mpl_comm_world

use GenMod_Control, only: &
    OperationalMode,      &
    QuietMode,            &
    ProductionMode,       &
    NormalMode,           &
    DiagnosticMode,       &
    DebugMode,            &
    VerboseMode,          &
    GeneralMode,          &
    mype,                 &
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

use OpsMod_CharUtils, only: ops_to_lower_case
use OpsMod_Constants, only: PPF ! PGE packing factor
use OpsMod_Control, only:   &
    DefaultDocURL,          &
    Ops_InitMPI
use OpsMod_DateTime, only: &
    DateTime_type,         &
    OpsFn_DateTime_now
use OpsMod_MiscTypes, only: ElementHeader_Type
use OpsMod_ObsGroupInfo, only: &
    OpsFn_ObsGroupNameToNum,   &
    ObsGroupAircraft,          &
    ObsGroupGPSRO,             &
    ObsGroupSurface,           &
    ObsGroupSatwind,           &
    ObsGroupScatwind
use OpsMod_ObsInfo, only: &
    FinalRejectReport,    &
    LenCallsign,          &
    OB_type,              &
    Ops_Alloc,            &
    Ops_SetupObType
use OpsMod_AODGeneral, only: NAODWaves
use OpsMod_GPSRO, only: GPSRO_TPD
use OpsMod_Kinds, only: &
    integer64,          &
    real64
use OpsMod_Radar, only: RadFamily
use OpsMod_SatRad_RTmodel, only: nlevels_strat_varobs
use OpsMod_Varfields
use OpsMod_Varobs, only: &
    AssimDataFormat_VAR, &
    Ops_CreateVarobs,    &
    Ops_ReadVarobsControlNL
use OpsMod_SatRad_SetUp, only: &
    VarBC
use OpsMod_ObsTypes, only: &
    Ops_SubTypeNameToNum

implicit none
external gc_init_final

public :: opsinputs_varobswriter_create, opsinputs_varobswriter_delete, &
          opsinputs_varobswriter_prior, opsinputs_varobswriter_post
private

! ------------------------------------------------------------------------------
type, public :: opsinputs_varobswriter
private
  character(len=100) :: ObsGroupName
  integer(integer64) :: ObsGroup
  type(datetime)     :: ValidityTime  ! Corresponds to OPS validity time

  logical            :: RejectObsWithAnyVariableFailingQC
  logical            :: RejectObsWithAllVariablesFailingQC

  logical            :: AccountForGPSROTangentPointDrift
  logical            :: UseRadarFamily

  integer(integer64) :: FH_VertCoord
  integer(integer64) :: FH_HorizGrid
  integer(integer64) :: FH_GridStagger
  integer(integer64) :: FH_ModelVersion

  integer(integer64) :: IC_ShipWind
  integer(integer64) :: IC_GroundGPSOperator
  integer(integer64) :: IC_GPSRO_Operator_pseudo
  integer(integer64) :: IC_GPSRO_Operator_press

  integer(integer64) :: IC_XLen
  integer(integer64) :: IC_YLen
  integer(integer64) :: IC_PLevels
  integer(integer64) :: IC_WetLevels

  real(real64)       :: RC_LongSpacing
  real(real64)       :: RC_LatSpacing
  real(real64)       :: RC_FirstLat
  real(real64)       :: RC_FirstLong
  real(real64)       :: RC_PoleLat
  real(real64)       :: RC_PoleLong

  integer(c_int), allocatable :: channels(:)

  type(ufo_geovals), pointer :: GeoVals
  type(ufo_geovals), pointer :: ObsDiags
end type opsinputs_varobswriter

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------

!> Set up an instance of opsinputs_varobswriter. Returns .true. on success and .false. on failure.
function opsinputs_varobswriter_create(self, f_conf, comm, channels, geovars, diagvars)
implicit none

! Subroutine arguments:
type(opsinputs_varobswriter), intent(inout) :: self
type(fckit_configuration), intent(in)      :: f_conf   ! Configuration
integer(gc_int_kind)                       :: comm     ! MPI communicator standing for the processes
                                                       ! holding the data to be written to VarObs
integer(c_int)                             :: channels(:)
type(oops_variables), intent(inout)        :: geovars  ! GeoVaLs required by the VarObsWriter.
type(oops_variables), intent(inout)        :: diagvars ! HofXDiags required by the VarObsWriter.
logical                                    :: opsinputs_varobswriter_create

! Local declarations:
character(len=:), allocatable              :: StringValue
integer                                    :: IntValue
logical                                    :: BoolValue
real(kind=c_double)                        :: DoubleValue

character(len=*), parameter :: RoutineName = "opsinputs_varobswriter_create"
character(len=200)          :: ErrorMessage

! Body:

opsinputs_varobswriter_create = .true.
allocate(self % channels(size(channels)))
self % channels(:) = channels(:)

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
  opsinputs_varobswriter_create = .false.
  return
end select

if (comm /= mpl_comm_world) then
  call gc_init_final(mype, nproc, comm)
end if
call Gen_SetupControl(DefaultDocURL)
call Ops_InitMPI

! Retrieve parameter values from the input configuration object
! and store them in member variables

if (.not. f_conf % get("obs_group", StringValue)) then
  call gen_warn(RoutineName, "Mandatory obs_group option not found")
  opsinputs_varobswriter_create = .false.
  return
end if
self % ObsGroupName = StringValue
self % ObsGroup = OpsFn_ObsGroupNameToNum(StringValue)

if (.not. f_conf % get("validity_time", StringValue)) then
  call gen_warn(RoutineName, "Mandatory validity_time option not found")
  opsinputs_varobswriter_create = .false.
  return
end if
call datetime_create(StringValue, self % validitytime)

call f_conf % get_or_die("reject_obs_with_any_variable_failing_qc", &
                         self % RejectObsWithAnyVariableFailingQC)

call f_conf % get_or_die("reject_obs_with_all_variables_failing_qc", &
                         self % RejectObsWithAllVariablesFailingQC)

call f_conf % get_or_die("account_for_gpsro_tangent_point_drift", &
                         self % AccountForGPSROTangentPointDrift)

call f_conf % get_or_die("use_radar_family", self % UseRadarFamily)

! Updates the varbc flag passedaround by a module in OPS
call f_conf % get_or_die("output_varbc_predictors", BoolValue)
VarBC = BoolValue

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
  opsinputs_varobswriter_create = .false.
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
  opsinputs_varobswriter_create = .false.
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
  opsinputs_varobswriter_create = .false.
  return
end select

call f_conf % get_or_die("FH_ModelVersion", IntValue)
self % FH_ModelVersion = IntValue

call f_conf % get_or_die("IC_ShipWind", BoolValue)
if (BoolValue) then
  self % IC_ShipWind = IC_ShipWind_10m
else
  self % IC_ShipWind = 0
end if

call f_conf % get_or_die("IC_GroundGPSOperator", StringValue)
select case (ops_to_lower_case(StringValue))
case ("choice")
  self % IC_GroundGPSOperator = IC_GroundGPSOperatorChoice
case ("generic")
  self % IC_GroundGPSOperator = IC_GroundGPSOperatorGeneric
case default
  write (ErrorMessage, '("IC_GroundGPSOperator code not recognised: ",A)') StringValue
  call gen_warn(RoutineName, ErrorMessage)
  opsinputs_varobswriter_create = .false.
  return
end select

call f_conf % get_or_die("IC_GPSRO_Operator_pseudo", BoolValue)
if (BoolValue) then
  self % IC_GPSRO_Operator_pseudo = IC_GPSRO_Operator_pseudo_choice
else
  self % IC_GPSRO_Operator_pseudo = 0
end if

call f_conf % get_or_die("IC_GPSRO_Operator_press", BoolValue)
if (BoolValue) then
  self % IC_GPSRO_Operator_press = IC_GPSRO_Operator_press_choice
else
  self % IC_GPSRO_Operator_press = 0
end if

call f_conf % get_or_die("IC_XLen", IntValue)
self % IC_XLen = IntValue

call f_conf % get_or_die("IC_YLen", IntValue)
self % IC_YLen = IntValue

call f_conf % get_or_die("IC_PLevels", IntValue)
self % IC_PLevels = IntValue

call f_conf % get_or_die("IC_WetLevels", IntValue)
self % IC_WetLevels = IntValue

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

! Fill in the list of variables that will be needed to populate the requested varfields.
call opsinputs_varobswriter_addrequiredgeovars(self, geovars)
call opsinputs_varobswriter_addrequireddiagvars(self, diagvars)

end function opsinputs_varobswriter_create

! ------------------------------------------------------------------------------

!> Destroy an instance of opsinputs_varobswriter.
subroutine opsinputs_varobswriter_delete(self)
implicit none

! Subroutine arguments:
type(opsinputs_varobswriter), intent(inout) :: self

! Body:
call datetime_delete(self % validitytime)
if (allocated(self % channels)) deallocate(self % channels)

end subroutine opsinputs_varobswriter_delete

! ------------------------------------------------------------------------------

!> Called by the priorFilter() method of the C++ VarObsWriter object.
!>
!> Set the GeoVals pointer.
subroutine opsinputs_varobswriter_prior(self, ObsSpace, GeoVals)
implicit none

! Subroutine arguments:
type(opsinputs_varobswriter), intent(inout) :: self
type(c_ptr), value, intent(in)             :: ObsSpace
type(ufo_geovals), intent(in), pointer     :: GeoVals

! Body:
self % GeoVals => GeoVals

end subroutine opsinputs_varobswriter_prior

! ------------------------------------------------------------------------------

!> Called by the postFilter() method of the C++ VarObsWriter object.
!>
!> Write out a VarObs file containing varfields derived from JEDI variables.
subroutine opsinputs_varobswriter_post( &
  self, ObsSpace, Flags, ObsErrors, nvars, nlocs, hofx, obsdiags)
implicit none

! Subroutine arguments:
type(opsinputs_varobswriter), intent(inout) :: self
type(c_ptr), value, intent(in) :: ObsSpace
type(c_ptr), value, intent(in) :: Flags, ObsErrors
integer,            intent(in) :: nvars, nlocs
real(c_double),     intent(in) :: hofx(nvars, nlocs)
type(ufo_geovals),  intent(in), pointer  :: obsdiags

! Local declarations:
type(OB_type)                  :: Ob
type(UM_header_type)           :: CxHeader
integer(integer64)             :: NumVarObsTotal

! Body:
self % ObsDiags => obsdiags
call opsinputs_varobswriter_allocateobservations(self, ObsSpace, Ob)
call opsinputs_varobswriter_populateobservations(self, ObsSpace, Flags, ObsErrors, Ob)
call opsinputs_varobswriter_populatecxheader(self, CxHeader)

call Ops_CreateVarobs (Ob,                  & ! in
                       CxHeader,            & ! in
                       AssimDataFormat_VAR, &
                       NumVarobsTotal)

call Ob % deallocate()
call CxHeader % dealloc()

end subroutine opsinputs_varobswriter_post

! ------------------------------------------------------------------------------

!> Populate the list of GeoVaLs needed to fill in any requested varfields.
subroutine opsinputs_varobswriter_addrequiredgeovars(self, geovars)
implicit none

! Subroutine arguments:
type(opsinputs_varobswriter), intent(in) :: self
type(oops_variables), intent(inout)      :: geovars

! Local declarations:
integer(integer64)                       :: VarFields(ActualMaxVarfield)
integer                                  :: i

! Body:
call Ops_ReadVarobsControlNL(self % obsgroup, VarFields)

do i = 1, size(VarFields)
  select case (VarFields(i))
  case (VarField_modelsurface)
    ! TODO(someone): "land_type_index" may not be the right geoval to use. If it isn't, change it
    ! here and in opsinputs_varobswriter_populateobservations.
    call geovars % push_back("land_type_index")
  end select
end do

end subroutine opsinputs_varobswriter_addrequiredgeovars

! ------------------------------------------------------------------------------

!> Populate the list of HofXDiags needed to fill in any requested varfields.
subroutine opsinputs_varobswriter_addrequireddiagvars(self, diagvars)
implicit none

! Subroutine arguments:
type(opsinputs_varobswriter), intent(in) :: self
type(oops_variables), intent(inout)      :: diagvars

! Local declarations:
integer(integer64)                       :: VarFields(ActualMaxVarfield)
integer                                  :: i, ichan
character(len=200)                       :: varname

! Body:
call Ops_ReadVarobsControlNL(self % obsgroup, VarFields)

! Example below for mwemiss but we will be getting this from
! surface_emissivity@Emiss to begin with
!do i = 1, size(VarFields)
!  select case (VarFields(i))
!  case (VarField_mwemiss)
!    do ichan = 1, size(self % channels)
!      write(varname,"(A19,I0)") "surface_emissivity_",self % channels(ichan)
!      call diagvars % push_back(trim(varname))
!    end do
!  end select
!end do

end subroutine opsinputs_varobswriter_addrequireddiagvars

! ------------------------------------------------------------------------------

!> Prepare Ob to hold the required number of observations.
subroutine opsinputs_varobswriter_allocateobservations(self, ObsSpace, Ob)
implicit none

! Subroutine arguments:
type(opsinputs_varobswriter), intent(in) :: self
type(c_ptr), value, intent(in)          :: ObsSpace
type(OB_type), intent(inout)            :: Ob

! Body:
Ob % Header % obsgroup = self % obsgroup

call Ops_SetupObType(Ob)

Ob % Header % numobstotal = obsspace_get_gnlocs(ObsSpace)
Ob % Header % numobslocal = obsspace_get_nlocs(ObsSpace)

Ob % Header % NumCXBatches = 1
allocate(Ob % Header % ObsPerBatchPerPE(Ob % Header % NumCXBatches, 0:nproc - 1))
Ob % Header % ObsPerBatchPerPE(1,mype) = Ob % Header % numobslocal

end subroutine opsinputs_varobswriter_allocateobservations

! ------------------------------------------------------------------------------

!> Populate Ob fields needed to output the requested varfields.
subroutine opsinputs_varobswriter_populateobservations( &
  self, ObsSpace, Flags, ObsErrors, Ob)
implicit none

! Subroutine arguments:
type(opsinputs_varobswriter), intent(in) :: self
type(c_ptr), value, intent(in)          :: ObsSpace
type(c_ptr), value, intent(in)          :: Flags, ObsErrors
type(OB_type), intent(inout)            :: Ob

! Local declarations:
character(len=*), parameter             :: RoutineName = "opsinputs_varobswriter_populateobservations"
character(len=80)                       :: ErrorMessage

integer(integer64)                      :: VarFields(ActualMaxVarfield)
integer                                 :: nVarFields
integer                                 :: iVarField
integer                                 :: iobs

integer(c_int64_t)                      :: TimeOffsetsInSeconds(Ob % Header % NumObsLocal)

logical                                 :: FillChanNum = .false.
logical                                 :: FillNumChans = .false.

integer, allocatable                    :: satellite_identifier_int(:)

! Body:

! Get the list of varfields to populate

call Ops_ReadVarobsControlNL(self % obsgroup, VarFields)
nVarFields = size(VarFields)

! Fill in the "generic" parts of the Obs object (not dependent on the list of varfields)

call Ops_Alloc(Ob % Header % Latitude, "Latitude", Ob % Header % NumObsLocal, Ob % Latitude)
call obsspace_get_db(ObsSpace, "MetaData", "latitude", Ob % Latitude)

call Ops_Alloc(Ob % Header % Longitude, "Longitude", Ob % Header % NumObsLocal, Ob % Longitude)
call obsspace_get_db(ObsSpace, "MetaData", "longitude", Ob % Longitude)

call Ops_Alloc(Ob % Header % Time, "Time", Ob % Header % NumObsLocal, Ob % Time)
call opsinputs_obsspace_get_db_datetime_offset_in_seconds( &
  ObsSpace, "MetaData", "datetime", self % validitytime, TimeOffsetsInSeconds)
Ob % Time = TimeOffsetsInSeconds

call opsinputs_varobswriter_fillreportflags(Ob, ObsSpace, Flags, &
  self % RejectObsWithAnyVariableFailingQC, self % RejectObsWithAllVariablesFailingQC)

call Ops_Alloc(Ob % Header % ObsType, "ObsType", Ob % Header % NumObsLocal, Ob % ObsType)
Ob % ObsType(:) = Ops_SubTypeNameToNum(trim(self % ObsGroupName))

if (obsspace_has(ObsSpace, "MetaData", "station_id")) then
  call Ops_Alloc(Ob % Header % Callsign, "Callsign", Ob % Header % NumObsLocal, Ob % Callsign)
  call opsinputs_obsspace_get_db_string( &
    ObsSpace, "MetaData", "station_id", int(LenCallSign, kind=4), Ob % Callsign)
else if (obsspace_has(ObsSpace, "MetaData", "satellite_identifier")) then
  call Ops_Alloc(Ob % Header % Callsign, "Callsign", Ob % Header % NumObsLocal, Ob % Callsign)
  allocate(satellite_identifier_int(Ob % Header % NumObsLocal))
  call obsspace_get_db(ObsSpace, "MetaData", "satellite_identifier", satellite_identifier_int)
  do iobs = 1, Ob % Header % NumObsLocal
    write(Ob % Callsign(iobs),"(I0.4)") satellite_identifier_int(iobs)
  end do
  deallocate(satellite_identifier_int)
end if

call opsinputs_fill_fillcoord2d( &
  Ob % Header % PlevelsA, "PlevelsA", Ob % Header % NumObsLocal, Ob % PlevelsA, &
   ObsSpace, self % channels, "air_pressure", "MetaData")

GPSRO_TPD = self % AccountForGPSROTangentPointDrift
if (Ob % header % ObsGroup == ObsGroupGPSRO .and. GPSRO_TPD) then
  call opsinputs_varobswriter_fillgpsrotpddependentfields(Ob, ObsSpace)
end if

! TODO(wsmigaj): it may be possible to derive RadFamily directly from the observation group.
RadFamily = self % UseRadarFamily
if (RadFamily) then
  call opsinputs_fill_fillinteger( &
    Ob % Header % Family, "Family", Ob % Header % NumObsLocal, Ob % Family, &
    ObsSpace, "radar_family", "MetaData")
end if

! Populate Ob members dependent on the list of varfields

do iVarField = 1, nVarFields
  select case (VarFields(iVarField))
    case (imdi)
      cycle
    case (VarField_pstar)
      call opsinputs_fill_fillelementtypefromsimulatedvariable( &
        Ob % Header % pstar, "pstar", Ob % Header % NumObsLocal, Ob % pstar, &
        ObsSpace, Flags, ObsErrors, "surface_pressure")
    case (VarField_theta)
      ! TODO(wsmigaj): check if air_potential_temperature is the correct variable name
      ! (it isn't used in JEDI, but virtual_temperature is)
      call opsinputs_fill_fillelementtype2dfromsimulatedvariable( &
        Ob % Header % theta, "theta", Ob % Header % NumObsLocal, Ob % theta, &
        ObsSpace, self % channels, Flags, ObsErrors, "air_potential_temperature")
    case (VarField_temperature)
      if (Ob % Header % ObsGroup == ObsGroupSurface) then
        call opsinputs_fill_fillelementtypefromsimulatedvariable( &
          Ob % Header % t2, "t2", Ob % Header % NumObsLocal, Ob % t2, &
          ObsSpace, Flags, ObsErrors, "air_temperature")
      else
        call opsinputs_fill_fillelementtype2dfromsimulatedvariable( &
          Ob % Header % t, "t", Ob % Header % NumObsLocal, Ob % t, &
          ObsSpace, self % channels, Flags, ObsErrors, "air_temperature")
      end if
    case (VarField_rh)
      if (Ob % Header % ObsGroup == ObsGroupSurface) then
        call opsinputs_fill_fillelementtypefromsimulatedvariable( &
          Ob % Header % rh2, "rh2", Ob % Header % NumObsLocal, Ob % rh2, &
          ObsSpace, Flags, ObsErrors, "relative_humidity")
      else
        call opsinputs_fill_fillelementtype2dfromsimulatedvariable( &
          Ob % Header % rh, "rh", Ob % Header % NumObsLocal, Ob % rh, &
          ObsSpace, self % channels, Flags, ObsErrors, "relative_humidity")
      end if
    case (VarField_u)
      if (Ob % Header % ObsGroup == ObsGroupSurface .or. &
          Ob % Header % ObsGroup == ObsGroupScatwind) then
        call opsinputs_fill_fillelementtypefromsimulatedvariable( &
          Ob % Header % u10, "u10", Ob % Header % NumObsLocal, Ob % u10, &
          ObsSpace, Flags, ObsErrors, "eastward_wind")
      else
        call opsinputs_fill_fillelementtype2dfromsimulatedvariable( &
          Ob % Header % u, "u", Ob % Header % NumObsLocal, Ob % u, &
          ObsSpace, self % channels, Flags, ObsErrors, "eastward_wind")
      end if
    case (VarField_v)
      if (Ob % Header % ObsGroup == ObsGroupSurface .or. &
          Ob % Header % ObsGroup == ObsGroupScatwind) then
        call opsinputs_fill_fillelementtypefromsimulatedvariable( &
          Ob % Header % v10, "v10", Ob % Header % NumObsLocal, Ob % v10, &
          ObsSpace, Flags, ObsErrors, "northward_wind")
      else
        call opsinputs_fill_fillelementtype2dfromsimulatedvariable( &
          Ob % Header % v, "v", Ob % Header % NumObsLocal, Ob % v, &
          ObsSpace, self % channels, Flags, ObsErrors, "northward_wind")
      end if
    case (VarField_logvis)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % logvis, "logvis", Ob % Header % NumObsLocal, Ob % logvis)
    case (VarField_tcwv)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % tcwv, "TCWV", Ob % Header % NumObsLocal, Ob % tcwv)
    case (VarField_windspeed)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % WindSpeed, "WindSpeed", Ob % Header % NumObsLocal, Ob % WindSpeed)
    case (VarField_lwp)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % lwp, "LWP", Ob % Header % NumObsLocal, Ob % lwp)
    case (VarField_britemp)
      call opsinputs_fill_fillreal2d( &
        Ob % Header % CorBriTemp, "CorBriTemp", Ob % Header % NumObsLocal, Ob % CorBriTemp, &
        ObsSpace, self % channels, "brightness_temperature", "BiasCorrObsValue")
    case (VarField_tskin)
      call opsinputs_fill_fillelementtypefromnormalvariable( &
        Ob % Header % Tskin, "Tskin", Ob % Header % NumObsLocal, Ob % Tskin, &
        ObsSpace, "skin_temperature", "OneDVar")
    case (VarField_gpstzdelay)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % GPSTZDelay, "GPSTZDelay", Ob % Header % NumObsLocal, Ob % GPSTZDelay)
    case (VarField_GPS_Station_Height)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % Zstation, "Zstation", Ob % Header % NumObsLocal, Ob % Zstation)
    case (VarField_mwemiss)
      call opsinputs_fill_fillreal2d( &
        Ob % Header % MwEmiss, "MwEmiss", Ob % Header % NumObsLocal, Ob % MwEmiss, &
        ObsSpace, self % channels, "surface_emissivity", "Emiss")
    case (VarField_TCozone)
      ! TODO(someone): This will come from an ObsFunction or a variable generated by a filter. Its
      ! name and group are not known yet. Once they are, replace the placeholders in the call below.
      call opsinputs_fill_fillreal( &
        Ob % Header % TCozone, "TCozone", Ob % Header % NumObsLocal, Ob % TCozone, &
        ObsSpace, "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP")
    case (VarField_satzenith)
      call opsinputs_fill_fillreal( &
        Ob % Header % SatZenithAngle, "SatZenithAngle", Ob % Header % NumObsLocal, Ob % SatZenithAngle, &
        ObsSpace, "sensor_zenith_angle", "MetaData")
    case (VarField_scanpos)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % ScanPosition, "ScanPosition", Ob % Header % NumObsLocal, Ob % ScanPosition)
    case (VarField_surface)
      call opsinputs_fill_fillinteger( &
        Ob % Header % surface, "surface", Ob % Header % NumObsLocal, Ob % surface, &
        ObsSpace, "surface_type", "MetaData")
    case (VarField_elevation)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % elevation, "elevation", Ob % Header % NumObsLocal, Ob % elevation)
    case (VarField_modelsurface)
      ! TODO(someone): "land_type_index" may not be the right geoval to use. If it isn't, change it
      ! here and in opsinputs_varobswriter_addrequiredgeovars.
      call opsinputs_fill_fillrealfromgeoval( &
        Ob % Header % ModelSurface, "ModelSurface", Ob % Header % NumObsLocal, Ob % ModelSurface, &
        self % GeoVals, "land_type_index")
    case (VarField_modelorog)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % ModelOrog, "ModelOrog", Ob % Header % NumObsLocal, Ob % ModelOrog)
    case (VarField_stratt)
      ! TODO(someone): This will come from a (2D) variable generated by the 1D-Var filter. Its name
      ! and group are not known yet. An extra difficulty is that the second dimension (the number of
      ! stratospheric temperature levels) is not related to the number of channels. Hopefully by the
      ! time this is filled in IODA will provide a convenient interface to 2D variables.
      ! UseLevelSubset = .true.
      ! NumLevs = nlevels_strat_varobs
      ! call Ops_Alloc(Ob % Header % t, "t", Ob % Header % NumObsLocal, Ob % t)
    case (VarField_satid)
      call opsinputs_varobswriter_fillsatid(Ob, ObsSpace)
    case (VarField_satazimth)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % SatAzimth, "SatAzimth", Ob % Header % NumObsLocal, Ob % SatAzimth)
    case (VarField_localazimuth)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % LocalAzimuth, "LocalAzimuth", Ob % Header % NumObsLocal, Ob % LocalAzimuth)
    case (VarField_solzenith)
      call opsinputs_fill_fillreal( &
        Ob % Header % SolarZenith, "SolarZenith", Ob % Header % NumObsLocal, Ob % SolarZenith, &
        ObsSpace, "solar_zenith_angle", "MetaData")
    case (VarField_solazimth)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % SolarAzimth, "SolarAzimth", Ob % Header % NumObsLocal, Ob % SolarAzimth)
    case (VarField_iremiss)
      ! TODO(someone): This will come from a variable generated by the 1D-Var filter. Its name and
      ! group are not known yet. Once they are, replace the placeholders in the call below.
      call opsinputs_fill_fillreal( &
        Ob % Header % IREmiss, "IREmiss", Ob % Header % NumObsLocal, Ob % IREmiss, &
        ObsSpace, "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP")
    case (VarField_cloudtopp)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % CloudTopP, "CloudTopP", Ob % Header % NumObsLocal, Ob % CloudTopP)
    case (VarField_cloudfrac)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % CloudFrac, "CloudFrac", Ob % Header % NumObsLocal, Ob % CloudFrac)
    case (VarField_vnatovpp)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % VnATOVPP, "VnATOVPP", Ob % Header % NumObsLocal, Ob % VnATOVPP)
    case (VarField_procoption)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % ATOVSProcOption, "ATOVSProcOption", Ob % Header % NumObsLocal, Ob % ATOVSProcOption)
    case (VarField_amsusurface)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % AMSUsurface, "AMSUsurface", Ob % Header % NumObsLocal, Ob % AMSUsurface)
    case (VarField_hirs_temp)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % HIRS_Temp, "HIRS_Temp", Ob % Header % NumObsLocal, Ob % HIRS_Temp)
    case (VarField_amsua1_temp)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % AMSUa1_Temp, "AMSUa1_Temp", Ob % Header % NumObsLocal, Ob % AMSUa1_Temp)
    case (VarField_amsua2_temp)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % AMSUa2_Temp, "AMSUa2_Temp", Ob % Header % NumObsLocal, Ob % AMSUa2_Temp)
    case (VarField_amsub_temp)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % AMSUb_Temp, "AMSUb_Temp", Ob % Header % NumObsLocal, Ob % AMSUb_Temp)
    case (VarField_cloud)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % Cloud, "Cloud", Ob % Header % NumObsLocal, Ob % Cloud)
    case (VarField_rainrate)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % Rainrate, "Rainrate", Ob % Header % NumObsLocal, Ob % Rainrate)
    case (VarField_snowrate)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % Snowrate, "Snowrate", Ob % Header % NumObsLocal, Ob % Snowrate)
    case (VarField_u10ambwind)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % u10AmbWind, "u10AmbWind", Ob % Header % NumObsLocal, Ob % u10AmbWind)
    case (VarField_v10ambwind)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % v10AmbWind, "v10AmbWind", Ob % Header % NumObsLocal, Ob % v10AmbWind)
    case (VarField_pcorrect)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % AWPriorPCorrect, "AWPriorPCorrect", Ob % Header % NumObsLocal, Ob % AWPriorPCorrect)
    case (VarField_NumChans)
      FillNumChans = .true.
    case (VarField_ChanNum)
      FillChanNum = .true.
    case (VarField_Emissivity)
      ! TODO(someone): This will come from a variable generated by the 1D-Var filter. Its name and
      ! group are not known yet. Once they are, replace the placeholders in the call below.
      call opsinputs_fill_fillreal2d( &
        Ob % Header % Emissivity, "Emissivity", Ob % Header % NumObsLocal, Ob % Emissivity, &
        ObsSpace, self % channels, "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP")
    case (VarField_QCinfo)
      ! TODO(someone): This will come from a variable generated by the 1D-Var filter. Its name and
      ! group are not known yet. Once they are, replace the placeholders in the call below.
      call opsinputs_fill_fillinteger( &
        Ob % Header % QCinfo, "QCinfo", Ob % Header % NumObsLocal, Ob % QCinfo, &
        ObsSpace, "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP")
    case (VarField_SBUVozone)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % SBUVozone, "SBUVozone", Ob % Header % NumObsLocal, Ob % SBUVozone)
    case (VarField_RadialVelocity)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % RadialVelocSO, "RadialVelocSO", Ob % Header % NumObsLocal, Ob % RadialVelocSO)
    case (VarField_Reflectivity)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % ReflectivitySO, "ReflectivitySO", Ob % Header % NumObsLocal, Ob % ReflectivitySO)
    case (VarField_ReflectivityR)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % ReflectivityR, "ReflectivityR", Ob % Header % NumObsLocal, Ob % ReflectivityR)
    case (VarField_ReflectivityI)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % ReflectivityI, "ReflectivityI", Ob % Header % NumObsLocal, Ob % ReflectivityI)
    case (VarField_RadarBeamElev)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % RadarBeamElev, "RadarBeamElev", Ob % Header % NumObsLocal, Ob % RadarBeamElev)
    case (VarField_RadarObRange)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % RadarObRange, "RadarObRange", Ob % Header % NumObsLocal, Ob % RadarObRange)
    case (VarField_RadarObAzim)
      call opsinputs_fill_fillreal2d( &
        Ob % Header % RadarObAzim, "RadarObAzim", Ob % Header % NumObsLocal, Ob % RadarObAzim, &
        ObsSpace, self % channels, "radar_azimuth", "MetaData")
    case (VarField_RadIdent)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % RadIdent, "RadIdent", Ob % Header % NumObsLocal, Ob % RadIdent)
    case (VarField_RadAltAboveMSL)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % RadAltAboveMSL, "RadAltAboveMSL", Ob % Header % NumObsLocal, Ob % RadAltAboveMSL)
    case (VarField_RadNoiseLvl)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % RadNoiseLvl, "RadNoiseLvl", Ob % Header % NumObsLocal, Ob % RadNoiseLvl)
    case (VarField_RadFlag)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % RadFlag, "RadFlag", Ob % Header % NumObsLocal, Ob % RadFlag)
    case (VarField_clw)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % clw, "clw", Ob % Header % NumObsLocal, Ob % clw)
    case (VarField_refrac)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % refrac, "refrac", Ob % Header % NumObsLocal, Ob % refrac)
    case (VarField_z)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % z, "z", Ob % Header % NumObsLocal, Ob % z)
    case (VarField_BendingAngle)
      if (GPSRO_TPD) then
        ! TODO(someone): Replace the placeholder in the call with an appropriate variable name,
        ! once it is known.
        call opsinputs_fill_fillelementtype2dfromsimulatedvariable( &
          Ob % Header % BendingAngleAll, "BendingAngleAll", Ob % Header % NumObsLocal, Ob % BendingAngleAll, &
          ObsSpace, self % channels, Flags, ObsErrors, "PLACEHOLDER_VARIABLE_NAME")
      else
        call opsinputs_fill_fillelementtype2dfromsimulatedvariable( &
          Ob % Header % BendingAngle, "BendingAngle", Ob % Header % NumObsLocal, Ob % BendingAngle, &
          ObsSpace, self % channels, Flags, ObsErrors, "bending_angle")
      end if
    case (VarField_ImpactParam)
       if (GPSRO_TPD) then
         ! TODO(someone): Replace the placeholder in the call with an appropriate variable name,
         ! once it is known.
         call opsinputs_fill_fillelementtype2dfromnormalvariable( &
           Ob % Header % ImpactParamAll, "ImpactParamAll", Ob % Header % NumObsLocal, Ob % ImpactParamAll, &
           ObsSpace, self % channels, "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP")
       else
         call opsinputs_fill_fillelementtype2dfromnormalvariable( &
           Ob % Header % ImpactParam, "ImpactParam", Ob % Header % NumObsLocal, Ob % ImpactParam, &
           ObsSpace, self % channels, "impact_parameter", "MetaData")
       end if
    case (VarField_RO_Rad_Curv)
      call opsinputs_fill_fillelementtypefromnormalvariable( &
        Ob % Header % RO_Rad_Curv, "RO_Rad_Curv", Ob % Header % NumObsLocal, Ob % RO_Rad_Curv, &
        ObsSpace, "earth_radius_of_curvature", "MetaData")
    case (VarField_RO_geoid_und)
      call opsinputs_fill_fillelementtypefromnormalvariable( &
        Ob % Header % RO_geoid_und, "RO_geoid_und", Ob % Header % NumObsLocal, Ob % RO_geoid_und, &
        ObsSpace, "geoid_height_above_reference_ellipsoid", "MetaData")
    case (VarField_AOD)
      call opsinputs_fill_fillelementtype2dfromsimulatedvariable( &
        Ob % Header % AOD, "AOD", Ob % Header % NumObsLocal, Ob % AOD, &
        ObsSpace, self % channels, Flags, ObsErrors, "aerosol_optical_depth")
        ! NAODWaves is used by the Ops_VarobPGEs subroutine.
        if (Ob % Header % AOD % Present) NAODWaves = Ob % Header % AOD % NumLev
    case (VarField_BriTempVarError)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % BriTempVarError, "BriTempVarError", Ob % Header % NumObsLocal, Ob % BriTempVarError)
    case (VarField_CloudRTError)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % CloudRTError, "CloudRTError", Ob % Header % NumObsLocal, Ob % CloudRTError)
    case (VarField_CloudRTBias)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % CloudRTBias, "CloudRTBias", Ob % Header % NumObsLocal, Ob % CloudRTBias)
    case (VarField_BiasPredictors)
      call opsinputs_varobswriter_fillpredictors( &
        Ob % Header % BiasPredictors, "BiasPredictors", Ob % Header % NumObsLocal, Ob % BiasPredictors, &
        ObsSpace, self % channels, "brightness_temperature")
    case (VarField_LevelTime)
      ! IF (PRESENT (RepObs)) THEN
      !   ObHdrVrbl = RepObs % Header % model_level_time
      ! ELSE
      !   CALL Ops_Alloc(Ob % Header % level_time, "level_time", Ob % Header % NumObsLocal, Ob % level_time)
      ! END IF
    case (VarField_LevelLat)
      ! TODO(someone): the OPS code had "if (present(RepObs))...else...end if" here. Is that needed
      ! and what is RepObs?
      ! TODO(someone): Replace the placeholder in the call with an appropriate variable name,
      ! once it is known.
      call opsinputs_fill_fillreal2dfromgeoval( &
        Ob % Header % level_lat, "level_lat", Ob % Header % NumObsLocal, Ob % level_lat, &
        self % GeoVals, "PLACEHOLDER_VARIABLE_NAME")
    case (VarField_LevelLon)
      ! IF (PRESENT (RepObs)) THEN
      !   ObHdrVrbl = RepObs % Header % model_level_lon
      ! ELSE
      !   CALL Ops_Alloc(Ob % Header % level_lon, "level_lon", Ob % Header % NumObsLocal, Ob % level_lon)
      ! END IF
    case (VarField_RainAccum)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % RainAccum, "RainAccum", Ob % Header % NumObsLocal, Ob % RainAccum)
    case (VarField_CeilBackscatter)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % CeilBackscatter, "CeilBackscatter", Ob % Header % NumObsLocal, Ob % CeilBackscatter)
    case (VarField_CeilRange)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % CeilRange, "CeilRange", Ob % Header % NumObsLocal, Ob % CeilRange)
    case (VarField_CeilSiteId)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % CeilSiteID, "CeilSiteID", Ob % Header % NumObsLocal, Ob % CeilSiteID)
    case (VarField_CeilScanIdent)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % CeilScanIdent, "CeilScanIdent", Ob % Header % NumObsLocal, Ob % CeilScanIdent)
    case (VarField_airqal_consttype)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % csnt_typ, "CSNT_TYP", Ob % Header % NumObsLocal, Ob % csnt_typ)
    case (VarField_airqal_massdensity)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % mass_dnsty, "MASS_DNSTY", Ob % Header % NumObsLocal, Ob % mass_dnsty)
    case (VarField_airqal_massdensityscale)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % dcml_scl_mass_dnsty, "DCML_SCL_MASS_DNSTY", Ob % Header % NumObsLocal, Ob % dcml_scl_mass_dnsty)
    case (VarField_HLOSwind)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % HLOSwind, "HLOSwind", Ob % Header % NumObsLocal, Ob % HLOSwind)
    case (VarField_ProfileNo)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % ProfileNo, "ProfileNo", Ob % Header % NumObsLocal, Ob % ProfileNo)
    case (VarField_dWinddT)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % dWinddT, "dWinddT", Ob % Header % NumObsLocal, Ob % dWinddT)
    case (VarField_dWinddP)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % dWinddP, "dWinddP", Ob % Header % NumObsLocal, Ob % dWinddP)
    case (VarField_AzimuthCOG)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % AzimuthCOG, "AzimuthCOG", Ob % Header % NumObsLocal, Ob % AzimuthCOG)
    case (VarField_HeightCOG)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % HeightCOG, "HeightCOG", Ob % Header % NumObsLocal, Ob % HeightCOG)
    case default
      write (ErrorMessage, '(A,I0)') "VarField code not recognised ", VarFields(iVarField)
      call gen_warn(RoutineName, ErrorMessage)
      cycle
  end select

  if (FillChanNum .or. FillNumChans) then
    call opsinputs_varobswriter_fillchannumandnumchans( &
      Ob, ObsSpace, self % channels, Flags, FillChanNum, FillNumChans)
  end if

end do

end subroutine opsinputs_varobswriter_populateobservations

! ------------------------------------------------------------------------------

!> Populate the Ob % ReportFlags field.
!>
!> Observations are marked as rejected if the JEDI QC flags of any or all (depending on the
!> specified options) simulated variables are set to anything different from "pass".
subroutine opsinputs_varobswriter_fillreportflags( &
  Ob, ObsSpace, Flags, RejectObsWithAnyVariableFailingQC, RejectObsWithAllVariablesFailingQC)
implicit none

! Subroutine arguments:
type(OB_type), intent(inout)             :: Ob
type(c_ptr), value, intent(in)           :: ObsSpace, Flags
logical, intent(in)                      :: RejectObsWithAnyVariableFailingQC
logical, intent(in)                      :: RejectObsWithAllVariablesFailingQC

! Body:
call Ops_Alloc(Ob % Header % ReportFlags, "ReportFlags", &
               Ob % Header % NumObsLocal, Ob % ReportFlags)
call opsinputs_utils_fillreportflags(ObsSpace, Flags, &
                                   RejectObsWithAnyVariableFailingQC, &
                                   RejectObsWithAllVariablesFailingQC, &
                                   Ob % ReportFlags)

end subroutine opsinputs_varobswriter_fillreportflags

! ------------------------------------------------------------------------------

!> Populate the Ob % NumChans and/or Ob % ChanNum field.
!>
!> Ob % ChanNum is filled with the indices of channels that passed QC; the number of these channels
!> is stored in Ob % NumChans.
subroutine opsinputs_varobswriter_fillchannumandnumchans( &
  Ob, ObsSpace, Channels, Flags, FillChanNum, FillNumChans)
implicit none

! Subroutine arguments:
type(OB_type), intent(inout)   :: Ob
type(c_ptr), value, intent(in) :: ObsSpace
integer(c_int), intent(in)     :: Channels(:)
type(c_ptr), value, intent(in) :: Flags
logical, intent(in)            :: FillChanNum, FillNumChans

! Local declarations:
integer(integer64)             :: NumChannels
integer(integer64)             :: ChannelIndices(Ob % Header % NumObsLocal, size(Channels))
integer(integer64)             :: ChannelCounts(Ob % Header % NumObsLocal)

! Body:
NumChannels = size(Channels)
if (NumChannels == 0) return

call opsinputs_varobswriter_findchannelspassingqc( &
  Ob % Header % NumObsLocal, ObsSpace, Channels, Flags, ChannelIndices, ChannelCounts)
if (FillChanNum) then
  call Ops_Alloc(Ob % Header % ChanNum, "ChanNum", Ob % Header % NumObsLocal, Ob % ChanNum, &
                 num_levels = NumChannels)
  Ob % ChanNum = ChannelIndices
end if

if (FillNumChans) then
  call Ops_Alloc(Ob % Header % NumChans, "NumChans", Ob % Header % NumObsLocal, Ob % NumChans)
  Ob % NumChans = ChannelCounts
end if

end subroutine opsinputs_varobswriter_fillchannumandnumchans

! ------------------------------------------------------------------------------

!> Find the indices of indices of channels that passed QC for the first (and normally only)
!> simulated variable.
subroutine opsinputs_varobswriter_findchannelspassingqc( &
  NumObs, ObsSpace, Channels, Flags, ChannelIndices, ChannelCounts)
implicit none

! Subroutine arguments:
integer(integer64), intent(in)  :: NumObs
type(c_ptr), value, intent(in)  :: ObsSpace
integer(c_int), intent(in)      :: Channels(:)
type(c_ptr), value, intent(in)  :: Flags
integer(integer64), intent(out) :: ChannelIndices(NumObs, size(Channels))
integer(integer64), intent(out) :: ChannelCounts(NumObs)

! Local declarations:
integer                         :: NumChannels
type(oops_variables)            :: Variables
character(max_varname_length)   :: VariableName
integer                         :: NumVariables, NumMultichannelVariables
integer                         :: iMultichannelVariable, iChannel, iVariable, iObs
integer(c_int)                  :: VarFlags(NumObs)
character(len=*), parameter     :: RoutineName = "opsinputs_varobswriter_findchannelspassingqc"

! Body:
NumChannels = size(Channels)
if (NumChannels == 0) return

ChannelIndices = 0
ChannelCounts = 0

! We rely on an implementation detail of oops::Variables, namely that if any channels are defined,
! oops::Variables stores all channels of the first variable, then all channels of the second
! variable and so on; all variables have the same channels.
Variables = opsinputs_obsdatavector_int_varnames(Flags)
NumVariables = Variables % nvars()
NumMultichannelVariables = NumVariables / NumChannels
if (NumMultichannelVariables * NumChannels /= NumVariables) then
  call gen_fail(RoutineName, "Unexpected number of variables")
end if
! Having single NumChans and ChanNum varfields makes sense only if only one (multi-channel)
! variable is assimilated or if all assimilated variables share the same quality flags.
if (NumMultichannelVariables > 1) then
  call gen_warn(RoutineName, "More than one multichannel simulated variable found. &
                             &Assuming all these variables have the same quality flags")
end if
if (NumMultichannelVariables > 0) then
  do iChannel = 1, NumChannels
    iVariable = iChannel
    VariableName = Variables % variable(iVariable)
    call opsinputs_obsdatavector_int_get(Flags, VariableName, VarFlags)
    do iObs = 1, NumObs
      if (VarFlags(iObs) == 0) then ! This channel has passed quality control
        ChannelIndices(iObs, 1 + ChannelCounts(iObs)) = iChannel
        ChannelCounts(iObs) = ChannelCounts(iObs) + 1
      end if
    end do
  end do
end if

end subroutine opsinputs_varobswriter_findchannelspassingqc

! ------------------------------------------------------------------------------

!> Fill the members of OB_type required to take the GPSRO point drift into account.
subroutine opsinputs_varobswriter_fillgpsrotpddependentfields(Ob, ObsSpace)
implicit none
! Subroutine arguments:
type(OB_type), intent(inout)             :: Ob
type(c_ptr), value, intent(in)           :: ObsSpace

! Body:

call opsinputs_varobswriter_fillsatid(Ob, ObsSpace)
! TODO(someone): Replace the placeholder in the call below with an appropriate variable name
! and group, once they're known.
call opsinputs_fill_fillinteger( &
  Ob % Header % RO_quality, "RO_quality", Ob % Header % NumObsLocal, Ob % RO_quality, &
  ObsSpace, "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP")
! TODO(someone): Replace "latitude" and "longitude" variable names in the two calls below with
! appropriate GPSRO-specific variable names, once they're known. The group name may need to be
! adjusted as well.
call opsinputs_fill_fillreal( &
  Ob % Header % ro_occ_lat, "ro_occ_lat", Ob % Header % NumObsLocal, Ob % ro_occ_lat, &
  ObsSpace, "latitude", "MetaData")
call opsinputs_fill_fillreal( &
  Ob % Header % ro_occ_lon, "ro_occ_lon", Ob % Header % NumObsLocal, Ob % ro_occ_lon, &
  ObsSpace, "longitude", "MetaData")

end subroutine opsinputs_varobswriter_fillgpsrotpddependentfields

! ------------------------------------------------------------------------------

!> Fill the Ob % SatId field.
!>
!> This is done in a separate routine because this field is filled from two places in the code.
subroutine opsinputs_varobswriter_fillsatid(Ob, ObsSpace)
implicit none
! Subroutine arguments:
type(OB_type), intent(inout)   :: Ob
type(c_ptr), value, intent(in) :: ObsSpace

! Local variables
character(len=MAXVARLEN)       :: satidname

! Body:
satidname = "satellite_id"
if (obsspace_has(ObsSpace, "MetaData", "satellite_identifier")) satidname = "satellite_identifier"

call opsinputs_fill_fillinteger( &
  Ob % Header % SatId, "SatId", Ob % Header % NumObsLocal, Ob % SatId, &
  ObsSpace, trim(satidname), "MetaData")

end subroutine opsinputs_varobswriter_fillsatid

! ------------------------------------------------------------------------------

!> Fill the Ob % BiasPredictor field.
!>
!> This is done in a separate routine because this field is filled from 
!> several arrays in the Obs Space and there is some data manipulation
subroutine opsinputs_varobswriter_fillpredictors( &
  Hdr, OpsVarName, NumObs, Real2, ObsSpace, Channels, JediVarName)
implicit none
! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(integer64), intent(in)                  :: NumObs
real(real64), pointer                           :: Real2(:,:)
type(c_ptr), value, intent(in)                  :: ObsSpace
integer(c_int), intent(in)                      :: Channels(:)
character(len=*), intent(in)                    :: JediVarName

! Local arguments:
character(len=max_varname_with_channel_length) :: JediVarNamesWithChannels(max(size(Channels), 1))
character(len=MAXVARLEN)        :: satidname
real(kind=c_double)             :: VarValue(NumObs)
real(kind=c_double)             :: MissingDouble
integer(kind=4)                 :: SatIdValue(NumObs)
integer(kind=4), allocatable    :: UniqueSatIds(:)
integer                         :: ii, jj
integer, parameter              :: maxpred = 31
character(len=*), parameter     :: PredictorBaseName(1:maxpred) = (/ &
              "constant                  ", &
              "thickness_850_300hPa      ", &
              "thickness_200_50hPa       ", &
              "Tskin                     ", &
              "total_column_water        ", &
              "Legendre_order_1          ", &
              "Legendre_order_2          ", &
              "Legendre_order_3          ", &
              "Legendre_order_4          ", &
              "Legendre_order_5          ", &
              "Legendre_order_6          ", &
              "orbital_angle_order_1_cos ", &
              "orbital_angle_order_1_sin ", &
              "orbital_angle_order_2_cos ", &
              "orbital_angle_order_2_sin ", &
              "orbital_angle_order_3_cos ", &
              "orbital_angle_order_3_sin ", &
              "orbital_angle_order_4_cos ", &
              "orbital_angle_order_4_sin ", &
              "orbital_angle_order_5_cos ", &
              "orbital_angle_order_5_sin ", &
              "orbital_angle_order_6_cos ", &
              "orbital_angle_order_6_sin ", &
              "orbital_angle_order_7_cos ", &
              "orbital_angle_order_7_sin ", &
              "orbital_angle_order_8_cos ", &
              "orbital_angle_order_8_sin ", &
              "orbital_angle_order_9_cos ", &
              "orbital_angle_order_9_sin ", &
              "orbital_angle_order_10_cos", &
              "orbital_angle_order_10_sin" /)
character(len=150) :: JediVarGroupWithSatId

! Body:
MissingDouble = missing_value(0.0_c_double)
if (size(Channels) == 0) write(*,*) "opsinputs_varobswriter_fillpredictors channels empty => segfault"
JediVarNamesWithChannels = opsinputs_fill_varnames_with_channels(JediVarName, Channels)

! Return unique sat ids for obs space
satidname = "satellite_id"
if (obsspace_has(ObsSpace, "MetaData", "satellite_identifier")) satidname = "satellite_identifier"
call obsspace_get_db(ObsSpace, "MetaData", trim(satidname), SatIdValue)
call unique_values(SatIdValue, UniqueSatIds, positive = .true.)

! Get data for each predictor - all the current predictors are channel
! independant so just use the first channel in the channel vector
do jj = 1, size(UniqueSatIds)
  do ii = 1, maxpred
    write(JediVarGroupWithSatId,"(2A,I0,A)") &
          trim(PredictorBaseName(ii)), "_satid_", UniqueSatIds(jj), "Predictor"
    if (obsspace_has(ObsSpace, JediVarGroupWithSatId, JediVarNamesWithChannels(1))) then

      ! Initialize output if not previously done
      if (.not. associated(Real2)) then
        call Ops_Alloc(Hdr, OpsVarName, NumObs, Real2, &
                       num_levels = int(maxpred, kind=integer64))
        Real2(:,:) = 0.0
      end if

      ! Retrieve data from JEDI
      call obsspace_get_db(ObsSpace, JediVarGroupWithSatId, JediVarNamesWithChannels(1), VarValue)

      ! Fill the OPS data structure
      where (VarValue /= MissingDouble)
        Real2(:, ii) = Real2(:, ii) + VarValue(:)
      end where
    end if
  end do
end do

if (allocated(UniqueSatIds)) deallocate(UniqueSatIds)

end subroutine opsinputs_varobswriter_fillpredictors

! ------------------------------------------------------------------------------

!> Prepare a CxHeader object to be given to the OPS function writing a VarObs file.
subroutine opsinputs_varobswriter_populatecxheader(self, CxHeader)
implicit none
! Subroutine arguments:
type(opsinputs_varobswriter), intent(in) :: self
type(UM_header_type), intent(inout)     :: CxHeader

! Local declarations:
integer(c_int)                          :: year, month, day, hour, minute, second
TYPE (DateTime_type)                    :: now

! Body:

CxHeader % FixHd = 0
CxHeader % FixHd(FH_IntCStart) = LenFixHd + 1
CxHeader % FixHd(FH_IntCSize) = 49
CxHeader % FixHd(FH_RealCStart) = CxHeader % FixHd(FH_IntCStart) + CxHeader % FixHd(FH_IntCSize)
CxHeader % FixHd(FH_RealCSize) = 34
call CxHeader % alloc

CxHeader % FixHd(FH_VertCoord) = self % FH_VertCoord
CxHeader % FixHd(FH_HorizGrid) = self % FH_HorizGrid
CxHeader % FixHd(FH_GridStagger) = self % FH_GridStagger
CxHeader % FixHd(FH_ModelVersion) = self % FH_ModelVersion

call datetime_to_YYYYMMDDhhmmss(self % ValidityTime, year, month, day, hour, minute, second)
CxHeader % FixHd(FH_DTYear) = year
CxHeader % FixHd(FH_DTMonth) = month
CxHeader % FixHd(FH_DTDay) = day
CxHeader % FixHd(FH_DTHour) = hour
CxHeader % FixHd(FH_DTMinute) = minute
CxHeader % FixHd(FH_DTSecond) = second
CxHeader % FixHd(FH_DTDayNo) = 0  ! TODO(wsmigaj): What should this be set to?

CxHeader % FixHd(FH_VTYear) = year
CxHeader % FixHd(FH_VTMonth) = month
CxHeader % FixHd(FH_VTDay) = day
CxHeader % FixHd(FH_VTHour) = hour
CxHeader % FixHd(FH_VTMinute) = minute
CxHeader % FixHd(FH_VTSecond) = second
CxHeader % FixHd(FH_VTDayNo) = 0  ! TODO(wsmigaj): What should this be set to?

now = OpsFn_DateTime_now()
CxHeader % FixHd(FH_CTYear) = now % year
CxHeader % FixHd(FH_CTMonth) = now % month
CxHeader % FixHd(FH_CTDay) = now % day
CxHeader % FixHd(FH_CTHour) = now % hour
CxHeader % FixHd(FH_CTMinute) = now % minute
CxHeader % FixHd(FH_CTSecond) = now % second
CxHeader % FixHd(FH_CTDayNo) = 0  ! TODO(wsmigaj): What should this be set to?

CxHeader % IntC(IC_ShipWind) = self % IC_ShipWind
CxHeader % IntC(IC_GroundGPSOperator) = self % IC_GroundGPSOperator
CxHeader % IntC(IC_GPSRO_Operator_pseudo) = self % IC_GPSRO_Operator_pseudo
CxHeader % IntC(IC_GPSRO_Operator_press) = self % IC_GPSRO_Operator_press

CxHeader % IntC(IC_XLen) = self % IC_XLen
CxHeader % IntC(IC_YLen) = self % IC_Ylen
CxHeader % IntC(IC_PLevels) = self % IC_PLevels
CxHeader % IntC(IC_WetLevels) = self % IC_WetLevels

CxHeader % RealC(RC_LongSpacing) = self % RC_LongSpacing
CxHeader % RealC(RC_LatSpacing) = self % RC_LatSpacing
CxHeader % RealC(RC_FirstLat) = self % RC_FirstLat
CxHeader % RealC(RC_FirstLong) = self % RC_FirstLong
CxHeader % RealC(RC_PoleLat) = self % RC_PoleLat
CxHeader % RealC(RC_PoleLong) = self % RC_PoleLong

end subroutine opsinputs_varobswriter_populatecxheader

! ------------------------------------------------------------------------------
!> Return unique value from an array
subroutine unique_values(input, output, positive)
implicit none

integer(kind=4), intent(in)               :: input(:)
integer(kind=4), allocatable, intent(out) :: output(:)
logical, optional, intent(in)             :: positive

integer(kind=4), allocatable :: unique(:)
integer :: i, j, k

allocate(unique(size(input)))
k = 1
unique(1) = input(1)
do i = 2, size(input)
  if (present(positive)) then
    if (positive .and. input(i) <= 0) cycle  ! only positive values to be output
  end if
  if (any(unique(1:k) == input(i))) cycle
  k = k + 1
  unique(k) = input(i)
end do

allocate(output(k))
output = unique(1:k)

end subroutine unique_values

! ------------------------------------------------------------------------------

end module opsinputs_varobswriter_mod
