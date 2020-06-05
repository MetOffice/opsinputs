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

use OpsMod_CharUtils, only: ops_to_lower_case
use OpsMod_Constants, only: PPF ! PGE packing factor
use OpsMod_Control, only:   &
    DefaultDocURL,          &
    Ops_InitMPI
use OpsMod_DateTime
use OpsMod_MiscTypes
use OpsMod_ObsGroupInfo, only: &
    OpsFn_ObsGroupNameToNum,   &
    ObsGroupAircraft,          &
    ObsGroupGPSRO,             &
    ObsGroupSurface,           &
    ObsGroupSatwind,           &
    ObsGroupScatwind
use OpsMod_ObsInfo
use OpsMod_AODGeneral, only: NAODWaves
use OpsMod_GPSRO, only: GPSRO_TPD
use OpsMod_Radar, only: RadFamily
use OpsMod_SatRad_RTmodel, only: nlevels_strat_varobs
use OpsMod_Varfields
use OpsMod_Varobs

implicit none
public :: cxvarobs_cxwriter_create, cxvarobs_cxwriter_delete, &
          cxvarobs_cxwriter_prior, cxvarobs_cxwriter_post
private
! Maximum length of a variable name
integer, parameter :: max_varname_length=MAXVARLEN
! Maximum length of a variable name with channel suffix
integer, parameter :: max_varname_with_channel_length=max_varname_length + 10

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
  integer(kind=8) :: FH_ModelVersion

  integer(kind=8) :: IC_ShipWind
  integer(kind=8) :: IC_GroundGPSOperator
  integer(kind=8) :: IC_GPSRO_Operator_pseudo
  integer(kind=8) :: IC_GPSRO_Operator_press

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

int = 0
found = f_conf % get("FH_ModelVersion", int)
self % FH_ModelVersion = int

bool = .false.
found = f_conf % get("IC_ShipWind", bool)
self % IC_ShipWind = merge(IC_ShipWind_10m, zero, bool)

string = "choice"
found = f_conf % get("IC_GroundGPSOperator", string)
select case (ops_to_lower_case(string))
case ("choice")
  self % IC_GroundGPSOperator = IC_GroundGPSOperatorChoice
case ("generic")
  self % IC_GroundGPSOperator = IC_GroundGPSOperatorGeneric
case default
  write (ErrorMessage, '("IC_GroundGPSOperator code not recognised: ",A)') string
  call gen_warn(RoutineName, ErrorMessage)
  cxvarobs_cxwriter_create = .false.
  goto 9999
end select

bool = .false.
found = f_conf % get("IC_GPSRO_Operator_pseudo", bool)
self % IC_GPSRO_Operator_pseudo = merge(IC_GPSRO_Operator_pseudo_choice, zero, bool)

bool = .false.
found = f_conf % get("IC_GPSRO_Operator_press", bool)
self % IC_GPSRO_Operator_press = merge(IC_GPSRO_Operator_press_choice, zero, bool)

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
!> Write out a VarObs file containing varfields derived from JEDI variables.
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
type(OB_type)                  :: Ob
type(UM_header_type)           :: CxHeader
integer(kind=8)                :: NumVarObsTotal

! Body:
call cxvarobs_cxwriter_allocateobservations(self, ObsSpace, Ob)
call cxvarobs_cxwriter_populateobservations(self, ObsSpace, Channels, Flags, ObsErrors, Ob)
call cxvarobs_cxwriter_populatecxheader(self, CxHeader)

call Ops_CreateVarobs (Ob,                  & ! in
                       CxHeader,            & ! in
                       AssimDataFormat_VAR, &
                       NumVarobsTotal)

call Ob % deallocate()

end subroutine cxvarobs_cxwriter_post

! ------------------------------------------------------------------------------

!> Populate the list of GeoVaLs needed to fill in any requested varfields.
subroutine cxvarobs_cxwriter_addrequiredgeovars(self, geovars)
implicit none

! Subroutine arguments:
type(cxvarobs_cxwriter), intent(in) :: self
type(oops_variables), intent(inout)     :: geovars

! Local declarations:
integer(kind=8)                         :: VarFields(ActualMaxVarfield)
integer                                 :: i

! Body:
call Ops_ReadVarobsControlNL(self % obsgroup, VarFields)

do i = 1, size(VarFields)
  select case (VarFields(i))
  case (VarField_modelsurface)
    ! TODO(someone): "land_type_index" may not be the right geoval to use. If it isn't, change it
    ! here and in cxvarobs_cxwriter_populateobservations.
    call geovars % push_back("land_type_index")
  end select
end do

end subroutine cxvarobs_cxwriter_addrequiredgeovars

! ------------------------------------------------------------------------------

!> Prepare Ob to hold the required number of observations.
subroutine cxvarobs_cxwriter_allocateobservations(self, ObsSpace, Ob)
implicit none

! Subroutine arguments:
type(cxvarobs_cxwriter), intent(in) :: self
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

end subroutine cxvarobs_cxwriter_allocateobservations

! ------------------------------------------------------------------------------

!> Populate Ob fields needed to output the requested varfields.
subroutine cxvarobs_cxwriter_populateobservations( &
  self, ObsSpace, Channels, Flags, ObsErrors, Ob)
implicit none

! Subroutine arguments:
type(cxvarobs_cxwriter), intent(in) :: self
type(c_ptr), value, intent(in)          :: ObsSpace
integer(c_int), intent(in)              :: Channels(:)
type(c_ptr), value, intent(in)          :: Flags, ObsErrors
type(OB_type), intent(inout)            :: Ob

! Local declarations:
character(len=*), parameter             :: RoutineName = "cxvarobs_cxwriter_populateobservations"
character(len=80)                       :: ErrorMessage

integer(kind=8)                         :: VarFields(ActualMaxVarfield)
integer                                 :: nVarFields
integer                                 :: iVarField

integer(c_int64_t)                      :: TimeOffsetsInSeconds(Ob % Header % NumObsLocal)

logical                                 :: FillChanNum = .false.
logical                                 :: FillNumChans = .false.

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
call cxvarobs_obsspace_get_db_datetime_offset_in_seconds( &
  ObsSpace, "MetaData", "datetime", self % validitytime, TimeOffsetsInSeconds)
Ob % Time = TimeOffsetsInSeconds

call cxvarobs_cxwriter_fillreportflags(Ob, ObsSpace, Flags, &
  self % RejectObsWithAnyVariableFailingQC, self % RejectObsWithAllVariablesFailingQC)

! TODO(someone): This call to Ops_Alloc() will need to be replaced by
! call cxvarobs_cxwriter_fillinteger( &
!   Ob % Header % surface, "surface", Ob % Header % NumObsLocal, Ob % surface, &
!   ObsSpace, "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP")
! with the placeholders replaced by an appropriate variable name and group.
! We call Ops_Alloc() because the Ops_CreateVarobs terminates prematurely if the ObsType array
! doesn't exist.
call Ops_Alloc(Ob % Header % ObsType, "ObsType", Ob % Header % NumObsLocal, Ob % ObsType)

if (obsspace_has(ObsSpace, "MetaData", "station_id")) then
  call Ops_Alloc(Ob % Header % Callsign, "Callsign", Ob % Header % NumObsLocal, Ob % Callsign)
  call cxvarobs_obsspace_get_db_string( &
    ObsSpace, "MetaData", "station_id", int(LenCallSign, kind=4), Ob % Callsign)
end if

call cxvarobs_cxwriter_fillcoord2d( &
  Ob % Header % PlevelsA, "PlevelsA", Ob % Header % NumObsLocal, Ob % PlevelsA, &
   ObsSpace, Channels, "air_pressure", "MetaData")

GPSRO_TPD = self % AccountForGPSROTangentPointDrift
if (Ob % header % ObsGroup == ObsGroupGPSRO .and. GPSRO_TPD) then
  call cxvarobs_cxwriter_fillgpsrotpddependentfields(Ob, ObsSpace)
end if

! TODO(wsmigaj): it may be possible to derive RadFamily directly from the observation group.
RadFamily = self % UseRadarFamily
if (RadFamily) then
  call cxvarobs_cxwriter_fillinteger( &
    Ob % Header % Family, "Family", Ob % Header % NumObsLocal, Ob % Family, &
    ObsSpace, "radar_family", "MetaData")
end if

! Populate Ob members dependent on the list of varfields

do iVarField = 1, nVarFields
  select case (VarFields(iVarField))
    case (imdi)
      cycle
    case (VarField_pstar)
      call cxvarobs_cxwriter_fillelementtypefromsimulatedvariable( &
        Ob % Header % pstar, "pstar", Ob % Header % NumObsLocal, Ob % pstar, &
        ObsSpace, Flags, ObsErrors, "surface_pressure")
    case (VarField_theta)
      ! TODO(wsmigaj): check if air_potential_temperature is the correct variable name
      ! (it isn't used in JEDI, but virtual_temperature is)
      call cxvarobs_cxwriter_fillelementtype2dfromsimulatedvariable( &
        Ob % Header % theta, "theta", Ob % Header % NumObsLocal, Ob % theta, &
        ObsSpace, Channels, Flags, ObsErrors, "air_potential_temperature")
    case (VarField_temperature)
      if (Ob % Header % ObsGroup == ObsGroupSurface) then
        call cxvarobs_cxwriter_fillelementtypefromsimulatedvariable( &
          Ob % Header % t2, "t2", Ob % Header % NumObsLocal, Ob % t2, &
          ObsSpace, Flags, ObsErrors, "air_temperature")
      else
        call cxvarobs_cxwriter_fillelementtype2dfromsimulatedvariable( &
          Ob % Header % t, "t", Ob % Header % NumObsLocal, Ob % t, &
          ObsSpace, Channels, Flags, ObsErrors, "air_temperature")
      end if
    case (VarField_rh)
      if (Ob % Header % ObsGroup == ObsGroupSurface) then
        call cxvarobs_cxwriter_fillelementtypefromsimulatedvariable( &
          Ob % Header % rh2, "rh2", Ob % Header % NumObsLocal, Ob % rh2, &
          ObsSpace, Flags, ObsErrors, "relative_humidity")
      else
        call cxvarobs_cxwriter_fillelementtype2dfromsimulatedvariable( &
          Ob % Header % rh, "rh", Ob % Header % NumObsLocal, Ob % rh, &
          ObsSpace, Channels, Flags, ObsErrors, "relative_humidity")
      end if
    case (VarField_u)
      if (Ob % Header % ObsGroup == ObsGroupSurface .or. &
          Ob % Header % ObsGroup == ObsGroupScatwind) then
        call cxvarobs_cxwriter_fillelementtypefromsimulatedvariable( &
          Ob % Header % u10, "u10", Ob % Header % NumObsLocal, Ob % u10, &
          ObsSpace, Flags, ObsErrors, "eastward_wind")
      else
        call cxvarobs_cxwriter_fillelementtype2dfromsimulatedvariable( &
          Ob % Header % u, "u", Ob % Header % NumObsLocal, Ob % u, &
          ObsSpace, Channels, Flags, ObsErrors, "eastward_wind")
      end if
    case (VarField_v)
      if (Ob % Header % ObsGroup == ObsGroupSurface .or. &
          Ob % Header % ObsGroup == ObsGroupScatwind) then
        call cxvarobs_cxwriter_fillelementtypefromsimulatedvariable( &
          Ob % Header % v10, "v10", Ob % Header % NumObsLocal, Ob % v10, &
          ObsSpace, Flags, ObsErrors, "northward_wind")
      else
        call cxvarobs_cxwriter_fillelementtype2dfromsimulatedvariable( &
          Ob % Header % v, "v", Ob % Header % NumObsLocal, Ob % v, &
          ObsSpace, Channels, Flags, ObsErrors, "northward_wind")
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
      call cxvarobs_cxwriter_fillcorbritemp(Ob, ObsSpace, Channels)
    case (VarField_tskin)
      ! TODO(someone): This will come from a variable generated by the 1D-Var filter. Its name and
      ! group are not known yet. Once they are, replace the placeholders in the call below.
      call cxvarobs_cxwriter_fillelementtypefromnormalvariable( &
        Ob % Header % Tskin, "Tskin", Ob % Header % NumObsLocal, Ob % Tskin, &
        ObsSpace, "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP")
    case (VarField_gpstzdelay)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % GPSTZDelay, "GPSTZDelay", Ob % Header % NumObsLocal, Ob % GPSTZDelay)
    case (VarField_GPS_Station_Height)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % Zstation, "Zstation", Ob % Header % NumObsLocal, Ob % Zstation)
    case (VarField_mwemiss)
      ! TODO(someone): This will come from a variable generated by the 1D-Var filter. Its name and
      ! group are not known yet. Once they are, replace the placeholders in the call below.
      call cxvarobs_cxwriter_fillreal2d( &
        Ob % Header % MwEmiss, "MwEmiss", Ob % Header % NumObsLocal, Ob % MwEmiss, &
        ObsSpace, Channels, "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP")
    case (VarField_TCozone)
      ! TODO(someone): This will come from an ObsFunction or a variable generated by a filter. Its
      ! name and group are not known yet. Once they are, replace the placeholders in the call below.
      call cxvarobs_cxwriter_fillreal( &
        Ob % Header % TCozone, "TCozone", Ob % Header % NumObsLocal, Ob % TCozone, &
        ObsSpace, "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP")
    case (VarField_satzenith)
      call cxvarobs_cxwriter_fillreal( &
        Ob % Header % SatZenithAngle, "SatZenithAngle", Ob % Header % NumObsLocal, Ob % SatZenithAngle, &
        ObsSpace, "sensor_zenith_angle", "MetaData")
    case (VarField_scanpos)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % ScanPosition, "ScanPosition", Ob % Header % NumObsLocal, Ob % ScanPosition)
    case (VarField_surface)
      ! TODO(someone): This will come from an ObsFunction or a variable generated by a filter. Its
      ! name and group are not known yet. Once they are, replace the placeholders in the call below.
      call cxvarobs_cxwriter_fillinteger( &
        Ob % Header % surface, "surface", Ob % Header % NumObsLocal, Ob % surface, &
        ObsSpace, "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP")
    case (VarField_elevation)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % elevation, "elevation", Ob % Header % NumObsLocal, Ob % elevation)
    case (VarField_modelsurface)
      ! TODO(someone): "land_type_index" may not be the right geoval to use. If it isn't, change it
      ! here and in cxvarobs_cxwriter_addrequiredgeovars.
      call cxvarobs_cxwriter_fillrealfromgeoval( &
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
      call cxvarobs_cxwriter_fillsatid(Ob, ObsSpace)
    case (VarField_satazimth)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % SatAzimth, "SatAzimth", Ob % Header % NumObsLocal, Ob % SatAzimth)
    case (VarField_localazimuth)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % LocalAzimuth, "LocalAzimuth", Ob % Header % NumObsLocal, Ob % LocalAzimuth)
    case (VarField_solzenith)
      call cxvarobs_cxwriter_fillreal( &
        Ob % Header % SolarZenith, "SolarZenith", Ob % Header % NumObsLocal, Ob % SolarZenith, &
        ObsSpace, "solar_zenith_angle", "MetaData")
    case (VarField_solazimth)
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % SolarAzimth, "SolarAzimth", Ob % Header % NumObsLocal, Ob % SolarAzimth)
    case (VarField_iremiss)
      ! TODO(someone): This will come from a variable generated by the 1D-Var filter. Its name and
      ! group are not known yet. Once they are, replace the placeholders in the call below.
      call cxvarobs_cxwriter_fillreal( &
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
      call cxvarobs_cxwriter_fillreal2d( &
        Ob % Header % Emissivity, "Emissivity", Ob % Header % NumObsLocal, Ob % Emissivity, &
        ObsSpace, Channels, "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP")
    case (VarField_QCinfo)
      ! TODO(someone): This will come from a variable generated by the 1D-Var filter. Its name and
      ! group are not known yet. Once they are, replace the placeholders in the call below.
      call cxvarobs_cxwriter_fillinteger( &
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
      call cxvarobs_cxwriter_fillreal2d( &
        Ob % Header % RadarObAzim, "RadarObAzim", Ob % Header % NumObsLocal, Ob % RadarObAzim, &
        ObsSpace, Channels, "radar_azimuth", "MetaData")
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
        call cxvarobs_cxwriter_fillelementtype2dfromsimulatedvariable( &
          Ob % Header % BendingAngleAll, "BendingAngleAll", Ob % Header % NumObsLocal, Ob % BendingAngleAll, &
          ObsSpace, Channels, Flags, ObsErrors, "PLACEHOLDER_VARIABLE_NAME")
      else
        call cxvarobs_cxwriter_fillelementtype2dfromsimulatedvariable( &
          Ob % Header % BendingAngle, "BendingAngle", Ob % Header % NumObsLocal, Ob % BendingAngle, &
          ObsSpace, Channels, Flags, ObsErrors, "bending_angle")
      end if
    case (VarField_ImpactParam)
       if (GPSRO_TPD) then
         ! TODO(someone): Replace the placeholder in the call with an appropriate variable name,
         ! once it is known.
         call cxvarobs_cxwriter_fillelementtype2dfromnormalvariable( &
           Ob % Header % ImpactParamAll, "ImpactParamAll", Ob % Header % NumObsLocal, Ob % ImpactParamAll, &
           ObsSpace, Channels, "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP")
       else
         call cxvarobs_cxwriter_fillelementtype2dfromnormalvariable( &
           Ob % Header % ImpactParam, "ImpactParam", Ob % Header % NumObsLocal, Ob % ImpactParam, &
           ObsSpace, Channels, "impact_parameter", "MetaData")
       end if
    case (VarField_RO_Rad_Curv)
      call cxvarobs_cxwriter_fillelementtypefromnormalvariable( &
        Ob % Header % RO_Rad_Curv, "RO_Rad_Curv", Ob % Header % NumObsLocal, Ob % RO_Rad_Curv, &
        ObsSpace, "earth_radius_of_curvature", "MetaData")
    case (VarField_RO_geoid_und)
      call cxvarobs_cxwriter_fillelementtypefromnormalvariable( &
        Ob % Header % RO_geoid_und, "RO_geoid_und", Ob % Header % NumObsLocal, Ob % RO_geoid_und, &
        ObsSpace, "geoid_height_above_reference_ellipsoid", "MetaData")
    case (VarField_AOD)
      call cxvarobs_cxwriter_fillelementtype2dfromsimulatedvariable( &
        Ob % Header % AOD, "AOD", Ob % Header % NumObsLocal, Ob % AOD, &
        ObsSpace, Channels, Flags, ObsErrors, "aerosol_optical_depth")
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
      ! TODO(someone): handle this varfield
      ! call Ops_Alloc(Ob % Header % BiasPredictors, "BiasPredictors", Ob % Header % NumObsLocal, Ob % BiasPredictors)
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
      call cxvarobs_cxwriter_fillreal2dfromgeoval( &
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
    call cxvarobs_cxwriter_fillchannumandnumchans( &
      Ob, ObsSpace, Channels, Flags, FillChanNum, FillNumChans)
  end if

end do

end subroutine cxvarobs_cxwriter_populateobservations

! ------------------------------------------------------------------------------

!> Populate a 1D array of Element_type objects and its header from a set of variables whose name
!> is included in the list passed to the 'simulate' option of the JEDI ObsSpace.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p El1 corresponds.
!> \param[in] NumObs
!>   Number of observations held by this process.
!> \param[inout] El1
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace containing the variables used to populate \p El1 and \p Hdr.
!> \param[in] Flags
!>   Pointer to a ioda::ObsDataVector<int> object containing QC flags.
!> \param[in] ObsErrors
!>   Pointer to a ioda::ObsDataVector<float> object containing observation errors.
!> \param[in] JediVarName
!>   Name of the JEDI variables (in the ObsValue, ObsError and GrossErrorProbability groups)
!>   used to populate \p El1 and \p Hdr.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine cxvarobs_cxwriter_fillelementtypefromsimulatedvariable( &
  Hdr, OpsVarName, NumObs, El1, ObsSpace, Flags, ObsErrors, JediVarName)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(kind=8), intent(in)                     :: NumObs
type(Element_type), pointer                     :: El1(:)
type(c_ptr), value, intent(in)                  :: ObsSpace
type(c_ptr), value, intent(in)                  :: Flags
type(c_ptr), value, intent(in)                  :: ObsErrors
character(len=*), intent(in)                    :: JediVarName

! Local declarations:
real(kind=c_double)                             :: ObsValue(NumObs)
integer(kind=c_int)                             :: Flag(NumObs)
real(kind=c_float)                              :: ObsError(NumObs)
real(kind=c_double)                             :: PGE(NumObs)
real(kind=c_double)                             :: MissingDouble
real(kind=c_float)                              :: MissingFloat
integer                                         :: i
character(len=*), parameter                     :: &
  RoutineName = "cxvarobs_cxwriter_fillelementtypefromsimulatedvariable"
character(len=256)                              :: ErrorMessage

! Body:

! The types of floating-point numbers used in this function are a bit confusing. OPS stores
! observation values as doubles, whereas JEDI stores them as floats. However, the Fortran interface
! to the IODA ObsSpace is only partially implemented: obsspace_get_db_real32 doesn't work, only
! obsspace_get_db_real64 does. So we need to retrieve observation values as doubles. Observation
! errors, though, are retrieved as floats.

MissingDouble = missing_value(0.0_c_double)
MissingFloat  = missing_value(0.0_c_float)

if (obsspace_has(ObsSpace, "ObsValue", JediVarName)) then
  ! Retrieve data from JEDI:
  ! - observation value
  call obsspace_get_db(ObsSpace, "ObsValue", JediVarName, ObsValue)
  ! - QC flag
  if (cxvarobs_obsdatavector_int_has(Flags, JediVarName)) then
    call cxvarobs_obsdatavector_int_get(Flags, JediVarName, Flag)
  else
    write (ErrorMessage, '(A,A)') "QC flags not found for variable ", JediVarName
    call gen_warn(RoutineName, ErrorMessage)
    Flag = 0 ! assume all observations passed QC
  end if
  ! - observation error
  if (cxvarobs_obsdatavector_float_has(ObsErrors, JediVarName)) then
    call cxvarobs_obsdatavector_float_get(ObsErrors, JediVarName, ObsError)
  else
    write (ErrorMessage, '(A,A,A)') "Variable ", JediVarName, "@ObsError not found"
    call gen_warn(RoutineName, ErrorMessage)
    ObsError = MissingFloat
  end if
  ! - gross error probability
  if (obsspace_has(ObsSpace, "GrossErrorProbability", JediVarName)) then
    call obsspace_get_db(ObsSpace, "GrossErrorProbability", JediVarName, PGE)
  else
    PGE = MissingDouble
  end if

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, El1)
  do i = 1, NumObs
    if (ObsValue(i) /= MissingDouble) El1(i) % Value = ObsValue(i)
    if (ObsError(i) /= MissingFloat)  El1(i) % OBErr = ObsError(i)
    if (PGE(i) /= MissingDouble)      El1(i) % PGEFinal = PGE(i) * PPF
    if (Flag(i) /= 0)                 El1(i) % Flags = ibset(0, FinalRejectFlag)
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.
end subroutine cxvarobs_cxwriter_fillelementtypefromsimulatedvariable

! ------------------------------------------------------------------------------

!> Populate a 2D array of Element_type objects and its header from a set of variables whose name
!> is included in the list passed to the 'simulate' option of the JEDI ObsSpace.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p El2 corresponds.
!> \param[in] NumObs
!>   Number of observations held by this process.
!> \param[inout] El2
!>   Pointer to the array to be populated.
!> \param[in] Channel indices returned by ioda::ObsSpace::obsvariables().channels().
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace containing the variables used to populate \p El1 and \p Hdr.
!> \param[in] Flags
!>   Pointer to a ioda::ObsDataVector<int> object containing QC flags.
!> \param[in] ObsErrors
!>   Pointer to a ioda::ObsDataVector<float> object containing observation errors.
!> \param[in] JediVarName
!>   Name of the JEDI variables (in the ObsValue, ObsError and GrossErrorProbability groups)
!>   used to populate El1 and Hdr. The variables can either have no channel suffix (in which case
!>   \p El2 will have only a single row) or have suffixes representing the indices specified in
!>   \p Channels.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine cxvarobs_cxwriter_fillelementtype2dfromsimulatedvariable( &
  Hdr, OpsVarName, NumObs, El2, ObsSpace, Channels, Flags, ObsErrors, JediVarName)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(kind=8), intent(in)                     :: NumObs
type(Element_type), pointer                     :: El2(:,:)
type(c_ptr), value, intent(in)                  :: ObsSpace
integer(c_int), intent(in)                      :: Channels(:)
type(c_ptr), value, intent(in)                  :: Flags
type(c_ptr), value, intent(in)                  :: ObsErrors
character(len=*), intent(in)                    :: JediVarName

! Local declarations:
real(kind=c_double)                             :: ObsValue(NumObs)
integer(kind=c_int)                             :: Flag(NumObs)
real(kind=c_float)                              :: ObsError(NumObs)
real(kind=c_double)                             :: PGE(NumObs)
real(kind=c_double)                             :: MissingDouble
real(kind=c_float)                              :: MissingFloat
character(len=max_varname_with_channel_length)  :: JediVarNamesWithChannels(max(size(Channels), 1))

integer                                         :: iChannel, iObs
character(len=*), parameter                     :: &
  RoutineName = "cxvarobs_cxwriter_fillelementtype2dfromsimulatedvariable"
character(len=256)                              :: ErrorMessage

! Body:

! The types of floating-point numbers used in this function are a bit confusing. OPS stores
! observation values as doubles, whereas JEDI stores them as floats. However, the Fortran interface
! to the IODA ObsSpace is only partially implemented: obsspace_get_db_real32 doesn't work, only
! obsspace_get_db_real64 does. So we need to retrieve observation values as doubles. Observation
! errors, though, are retrieved as floats.

MissingDouble = missing_value(0.0_c_double)
MissingFloat  = missing_value(0.0_c_float)

JediVarNamesWithChannels = cxvarobs_cxwriter_varnames_with_channels(JediVarName, Channels)

if (obsspace_has(ObsSpace, "ObsValue", JediVarNamesWithChannels(1))) then
  ! Allocate OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, El2, &
                 num_levels = int(size(JediVarNamesWithChannels), kind=8))

  do iChannel = 1, size(JediVarNamesWithChannels)
    ! Retrieve data from JEDI:
    ! - observation value
    call obsspace_get_db(ObsSpace, "ObsValue", JediVarNamesWithChannels(iChannel), ObsValue)
    ! - QC flag
    if (cxvarobs_obsdatavector_int_has(Flags, JediVarNamesWithChannels(iChannel))) then
      call cxvarobs_obsdatavector_int_get(Flags, JediVarNamesWithChannels(iChannel), Flag)
    else
      write (ErrorMessage, '(A,A,A)') &
        "Warning: variable ", JediVarNamesWithChannels(iChannel), "@ObsError not found"
      call gen_warn(RoutineName, ErrorMessage)
      Flag = 0 ! assume all observations passed QC
    end if
    ! - observation error
    if (cxvarobs_obsdatavector_float_has(ObsErrors, JediVarNamesWithChannels(iChannel))) then
      call cxvarobs_obsdatavector_float_get(ObsErrors, JediVarNamesWithChannels(iChannel), ObsError)
    else
      write (ErrorMessage, '(A,A,A)') &
        "Warning: variable ", JediVarNamesWithChannels(iChannel), "@ObsError not found"
      call gen_warn(RoutineName, ErrorMessage)
      ObsError = MissingFloat
    end if
    ! - gross error probability
    if (obsspace_has(ObsSpace, "GrossErrorProbability", JediVarNamesWithChannels(iChannel))) then
      call obsspace_get_db(ObsSpace, "GrossErrorProbability", JediVarNamesWithChannels(iChannel), &
                           PGE)
    else
      PGE = MissingDouble
    end if

    ! Fill the OPS data structures
    do iObs = 1, NumObs
      if (ObsValue(iObs) /= MissingDouble) El2(iObs, iChannel) % Value = ObsValue(iObs)
      if (ObsError(iObs) /= MissingFloat)  El2(iObs, iChannel) % OBErr = ObsError(iObs)
      if (PGE(iObs) /= MissingDouble)      El2(iObs, iChannel) % PGEFinal = PGE(iObs) * PPF
      if (Flag(iObs) /= 0)                 El2(iObs, iChannel) % Flags = ibset(0, FinalRejectFlag)
    end do
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.
end subroutine cxvarobs_cxwriter_fillelementtype2dfromsimulatedvariable

! ------------------------------------------------------------------------------

!> Populate a 1D array of Element_type objects and its header from an arbitrary JEDI variable
!> (not included in the list passed to the 'simulate' option of the JEDI ObsSpace) containing
!> obsevation values and optionally another variable containing observation errors.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p El1 corresponds.
!> \param[in] NumObs
!>   Number of observations held by this process.
!> \param[inout] El1
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace object containing the specified JEDI variables.
!> \param[in] JediValueVarName
!>   Name of the JEDI variable containing observation values.
!> \param[in] JediValueGroup
!>   Group of the JEDI variable containing observation values.
!> \param[in] JediErrorVarName
!>   (Optional) Name of the JEDI variable containing observation errors.
!> \param[in] JediErrorGroup
!>   (Optional) Group of the JEDI variable containing observation errors.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine cxvarobs_cxwriter_fillelementtypefromnormalvariable( &
  Hdr, OpsVarName, NumObs, El1, ObsSpace, &
  JediValueVarName, JediValueGroup, JediErrorVarName, JediErrorGroup)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(kind=8), intent(in)                     :: NumObs
type(Element_type), pointer                     :: El1(:)
type(c_ptr), value, intent(in)                  :: ObsSpace
character(len=*), intent(in)                    :: JediValueVarName
character(len=*), intent(in)                    :: JediValueGroup
character(len=*), optional, intent(in)          :: JediErrorVarName
character(len=*), optional, intent(in)          :: JediErrorGroup

! Local declarations:
real(kind=c_double)                             :: ObsValue(NumObs)
real(kind=c_float)                              :: ObsError(NumObs)
real(kind=c_double)                             :: MissingDouble
real(kind=c_float)                              :: MissingFloat
integer                                         :: i
character(len=*), parameter                     :: &
  RoutineName = "cxvarobs_cxwriter_fillelementtypefromnormalvariable"
character(len=256)                              :: ErrorMessage

! Body:

if (present(JediErrorVarName) .neqv. present(JediErrorGroup)) then
  write (ErrorMessage, '(A)') &
    "JediErrorVarName and JediErrorGroup must be either both absent or both present"
  call gen_warn(RoutineName, ErrorMessage)
end if

MissingDouble = missing_value(0.0_c_double)

if (obsspace_has(ObsSpace, JediValueGroup, JediValueVarName)) then
  ! Retrieve data from JEDI
  call obsspace_get_db(ObsSpace, JediValueGroup, JediValueVarName, ObsValue)
  if (present(JediErrorVarName) .and. present(JediErrorGroup)) then
    if (obsspace_has(ObsSpace, JediErrorGroup, JediErrorVarName)) then
      call obsspace_get_db(ObsSpace, JediErrorGroup, JediErrorVarName, ObsError)
    else
      write (ErrorMessage, '("Variable ",A,"@",A," not found")') JediErrorVarName, JediErrorGroup
      call gen_warn(RoutineName, ErrorMessage)
      ObsError = MissingDouble
    end if
  else
    ObsError = MissingDouble
  end if

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, El1)
  do i = 1, NumObs
    if (ObsValue(i) /= MissingDouble) El1(i) % Value = ObsValue(i)
    if (ObsError(i) /= MissingDouble)  El1(i) % OBErr = ObsError(i)
    ! We could also fill Flags and PGEFinal if these quantities were available in separate JEDI
    ! variables. At present, however, we don't even have a use case where there is a separate
    ! variable storing the observation error.
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.
end subroutine cxvarobs_cxwriter_fillelementtypefromnormalvariable

! ------------------------------------------------------------------------------

!> Populate a 2D array of Element_type objects and its header from an arbitrary JEDI variable
!> (not included in the list passed to the 'simulate' option of the JEDI ObsSpace) containing
!> obsevation values and optionally another variable containing observation errors.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which El1 corresponds.
!> \param[in] NumObs
!>   Number of observations held by this process.
!> \param[inout] El2
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace object containing the specified JEDI variables. The variables can
!>   have either no channel suffix (in which case \p El2 will have only a single row) or suffixes
!>   representing the indices specified in \p Channels.
!> \param[in] Channel indices returned by ioda::ObsSpace::obsvariables().channels().
!> \param[in] JediValueVarName
!>   Name of the JEDI variable containing observation values.
!> \param[in] JediValueGroup
!>   Group of the JEDI variable containing observation values.
!> \param[in] JediErrorVarName
!>   (Optional) Name of the JEDI variable containing observation errors.
!> \param[in] JediErrorGroup
!>   (Optional) Group of the JEDI variable containing observation errors.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine cxvarobs_cxwriter_fillelementtype2dfromnormalvariable( &
  Hdr, OpsVarName, NumObs, El2, ObsSpace, Channels, &
  JediValueVarName, JediValueGroup, JediErrorVarName, JediErrorGroup)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(kind=8), intent(in)                     :: NumObs
type(Element_type), pointer                     :: El2(:,:)
type(c_ptr), value, intent(in)                  :: ObsSpace
integer(c_int), intent(in)                      :: Channels(:)
character(len=*), intent(in)                    :: JediValueVarName
character(len=*), intent(in)                    :: JediValueGroup
character(len=*), optional, intent(in)          :: JediErrorVarName
character(len=*), optional, intent(in)          :: JediErrorGroup

! Local declarations:
real(kind=c_double)                             :: ObsValue(NumObs)
real(kind=c_float)                              :: ObsError(NumObs)
real(kind=c_double)                             :: MissingDouble
real(kind=c_float)                              :: MissingFloat
character(len=max_varname_with_channel_length)  :: &
  JediValueVarNamesWithChannels(max(size(Channels), 1)), &
  JediErrorVarNamesWithChannels(max(size(Channels), 1))
integer                                         :: iChannel, iObs
character(len=*), parameter                     :: &
  RoutineName = "cxvarobs_cxwriter_fillelementtype2dfromnormalvariable"
character(len=256)                              :: ErrorMessage

! Body:

if (present(JediErrorVarName) .neqv. present(JediErrorGroup)) then
  write (ErrorMessage, '(A)') &
    "JediErrorVarName and JediErrorGroup must be either both absent or both present"
  call gen_warn(RoutineName, ErrorMessage)
end if

MissingDouble = missing_value(0.0_c_double)

JediValueVarNamesWithChannels = cxvarobs_cxwriter_varnames_with_channels( &
  JediValueVarName, Channels)
if (present(JediErrorVarName)) then
  JediErrorVarNamesWithChannels = cxvarobs_cxwriter_varnames_with_channels( &
    JediErrorVarName, Channels)
end if

if (obsspace_has(ObsSpace, JediValueGroup, JediValueVarNamesWithChannels(1))) then
  ! Allocate OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, El2, &
                 num_levels = int(size(JediValueVarNamesWithChannels), kind=8))

  do iChannel = 1, size(JediValueVarNamesWithChannels)
    ! Retrieve data from JEDI:
    ! - observation value
    call obsspace_get_db(ObsSpace, JediValueGroup, JediValueVarNamesWithChannels(iChannel), &
                         ObsValue)
    ! - observation error
    if (present(JediErrorVarName) .and. present(JediErrorGroup)) then
      if (obsspace_has(ObsSpace, JediErrorGroup, JediErrorVarNamesWithChannels(iChannel))) then
        call obsspace_get_db(ObsSpace, JediErrorGroup, JediErrorVarNamesWithChannels(iChannel), &
                             ObsError)
      else
        write (ErrorMessage, '("Variable ",A,"@",A," not found")') &
          JediErrorVarNamesWithChannels(iChannel), JediErrorGroup
        call gen_warn(RoutineName, ErrorMessage)
        ObsError = MissingDouble
      end if
    else
      ObsError = MissingDouble
    end if

    ! Fill the OPS data structures
    do iObs = 1, NumObs
      if (ObsValue(iObs) /= MissingDouble) El2(iObs, iChannel) % Value = ObsValue(iObs)
      if (ObsError(iObs) /= MissingDouble) El2(iObs, iChannel) % OBErr = ObsError(iObs)
      ! We could also fill Flags and PGEFinal if these quantities were available in separate JEDI
      ! variables. At present, however, we don't even have a use case where there is a separate
      ! variable storing the observation error.
    end do
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.

end subroutine cxvarobs_cxwriter_fillelementtype2dfromnormalvariable

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
!> \param[in] NumObs
!>   Number of observations held by this process.
!> \param[inout] Real1
!>   Pointer to the array to be populated.
!> \param[in] GeoVals
!>   A container holding the specified GeoVaL.
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
  Hdr, OpsVarName, NumObs, Real1, GeoVals, JediVarName)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(kind=8), intent(in)                     :: NumObs
real(kind=8), pointer                           :: Real1(:)
type(ufo_geovals), intent(in)                   :: GeoVals
character(len=*), intent(in)                    :: JediVarName

! Local declarations:
type(ufo_geoval), pointer                       :: GeoVal
real(kind_real)                                 :: MissingReal

character(len=*), parameter                     :: &
  RoutineName = "cxvarobs_cxwriter_fillrealfromgeoval"
character(len=256)                              :: ErrorMessage

! Body:

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
  call Ops_Alloc(Hdr, OpsVarName, NumObs, Real1)
  where (GeoVal % vals(1,:) /= MissingReal)
    Real1 = GeoVal % vals(1,:)
  end where
end if
end subroutine cxvarobs_cxwriter_fillrealfromgeoval

! ------------------------------------------------------------------------------

!> Populate a 2D array of real numbers and its header from a JEDI variable.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Real1 corresponds.
!> \param[in] NumObs
!>   Number of observations held by this process.
!> \param[inout] Real2
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace object containing the specified JEDI variable. The variable can
!>   have either no channel suffix (in which case \p Real2 will have only a single row) or suffixes
!>   representing the indices specified in \p Channels.
!> \param[in] Channel indices returned by ioda::ObsSpace::obsvariables().channels().
!> \param[in] JediVarName
!>   Name of the JEDI variable used to populate \p Real2. This can represent either a single
!>   variable with no channel suffix (in which case \p Real2 will have only a single row) or a set
!>   of variables with suffixes corresponding to the indices specified in \p Channels.
!> \param[in] JediGroup
!>   Group of the JEDI variable used to populate \p Real2.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine cxvarobs_cxwriter_fillreal2d( &
  Hdr, OpsVarName, NumObs, Real2, ObsSpace, Channels, JediVarName, JediVarGroup)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(kind=8), intent(in)                     :: NumObs
real(kind=8), pointer                           :: Real2(:,:)
type(c_ptr), value, intent(in)                  :: ObsSpace
integer(c_int), intent(in)                      :: Channels(:)
character(len=*), intent(in)                    :: JediVarName
character(len=*), intent(in)                    :: JediVarGroup

! Local declarations:
real(kind=c_double)                             :: VarValue(NumObs)
real(kind=c_double)                             :: MissingDouble
character(len=max_varname_with_channel_length)  :: JediVarNamesWithChannels(max(size(Channels), 1))
integer                                         :: iChannel

! Body:

MissingDouble = missing_value(0.0_c_double)

JediVarNamesWithChannels = cxvarobs_cxwriter_varnames_with_channels(JediVarName, Channels)

if (obsspace_has(ObsSpace, JediVarGroup, JediVarNamesWithChannels(1))) then
  ! Allocate OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, Real2, &
                 num_levels = int(size(JediVarNamesWithChannels), kind=8))
  print *, "size(Real2): ", size(Real2,1), " ", size(Real2,2)
  do iChannel = 1, size(JediVarNamesWithChannels)
    ! Retrieve data from JEDI
    call obsspace_get_db(ObsSpace, JediVarGroup, JediVarNamesWithChannels(iChannel), VarValue)

    ! Fill the OPS data structures
    where (VarValue /= MissingDouble)
      Real2(:, iChannel) = VarValue
    end where
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.
end subroutine cxvarobs_cxwriter_fillreal2d

! ------------------------------------------------------------------------------

!> Populate a 2D array of real numbers and its header from a GeoVaL.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Real1 corresponds.
!> \param[in] NumObs
!>   Number of observations held by this process.
!> \param[inout] Real2
!>   Pointer to the array to be populated.
!> \param[in] GeoVals
!>   A container holding the specified GeoVaL.
!> \param[in] JediVarName
!>   Name of the GeoVal used to populate \p Real2.
!>
!> \note This function returns early (without a warning) if the specified GeoVaL is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine cxvarobs_cxwriter_fillreal2dfromgeoval( &
  Hdr, OpsVarName, NumObs, Real2, GeoVals, JediVarName)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(kind=8), intent(in)                     :: NumObs
real(kind=8), pointer                           :: Real2(:,:)
character(len=*), intent(in)                    :: JediVarName
type(ufo_geovals), intent(in)                   :: GeoVals

! Local declarations:
type(ufo_geoval), pointer                       :: GeoVal
real(kind_real)                                 :: MissingReal

! Body:

MissingReal = missing_value(0.0_c_float)

if (ufo_vars_getindex(GeoVals % variables, JediVarName) > 0) then
  ! Retrieve GeoVal
  call ufo_geovals_get_var(GeoVals, JediVarName, GeoVal)

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, Real2, num_levels = int(GeoVal % nval, kind = 8))
  where (transpose(GeoVal % vals) /= MissingReal)
    Real2 = transpose(GeoVal % vals)
  end where
end if
end subroutine cxvarobs_cxwriter_fillreal2dfromgeoval

! ------------------------------------------------------------------------------

!> Populate a 1D array of integers and its header from a JEDI variable.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Int1 corresponds.
!> \param[in] NumObs
!>   Number of observations held by this process.
!> \param[inout] Int1
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace object containing the specified JEDI variable.
!> \param[in] JediVarName
!>   Name of the JEDI variable used to populate \p Int1.
!> \param[in] JediGroup
!>   Group of the JEDI variable used to populate \p Int1.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine cxvarobs_cxwriter_fillinteger( &
  Hdr, OpsVarName, NumObs, Int1, ObsSpace, JediVarName, JediVarGroup)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(kind=8), intent(in)                     :: NumObs
integer(kind=8), pointer                        :: Int1(:)
type(c_ptr), value, intent(in)                  :: ObsSpace
character(len=*), intent(in)                    :: JediVarName
character(len=*), intent(in)                    :: JediVarGroup

! Local declarations:
integer(kind=4)                                 :: VarValue(NumObs)
integer(kind=4)                                 :: MissingInt

! Body:

MissingInt = missing_value(0_c_int32_t)

if (obsspace_has(ObsSpace, JediVarGroup, JediVarName)) then
  ! Retrieve data from JEDI
  call obsspace_get_db(ObsSpace, JediVarGroup, JediVarName, VarValue)

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, Int1)
  where (VarValue /= MissingInt)
    Int1 = VarValue
  end where
end if
end subroutine cxvarobs_cxwriter_fillinteger

! ------------------------------------------------------------------------------

!> Populate a 2D array of Coord_type objects and its header from a JEDI variable.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Coord2 corresponds.
!> \param[in] NumObs
!>   Number of observations held by this process.
!> \param[inout] Coord2
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace object containing the specified JEDI variable. The variable can
!>   have either no channel suffix (in which case \p Coord2 will have only a single row) or suffixes
!>   representing the indices specified in \p Channels.
!> \param[in] Channel indices returned by ioda::ObsSpace::obsvariables().channels().
!> \param[in] JediVarName
!>   Name of the JEDI variable used to populate \p Coord2. This can represent either a single
!>   variable with no channel suffix (in which case \p Coord2 will have only a single row) or a set
!>   of variables with suffixes corresponding to the indices specified in \p Channels.
!> \param[in] JediGroup
!>   Group of the JEDI variable used to populate \p Coord2.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine cxvarobs_cxwriter_fillcoord2d( &
  Hdr, OpsVarName, NumObs, Coord2, ObsSpace, Channels, JediVarName, JediVarGroup)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(kind=8), intent(in)                     :: NumObs
type(coord_type), pointer                       :: Coord2(:,:)
type(c_ptr), value, intent(in)                  :: ObsSpace
integer(c_int), intent(in)                      :: Channels(:)
character(len=*), intent(in)                    :: JediVarName
character(len=*), intent(in)                    :: JediVarGroup

! Local declarations:
real(kind=c_double)                             :: VarValue(NumObs)
real(kind=c_double)                             :: MissingDouble
character(len=max_varname_with_channel_length)  :: JediVarNamesWithChannels(max(size(Channels), 1))
integer                                         :: iChannel

! Body:

MissingDouble = missing_value(0.0_c_double)

JediVarNamesWithChannels = cxvarobs_cxwriter_varnames_with_channels(JediVarName, Channels)

if (obsspace_has(ObsSpace, JediVarGroup, JediVarNamesWithChannels(1))) then
  ! Allocate OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, Coord2, &
                 num_levels = int(size(JediVarNamesWithChannels), kind=8))

  do iChannel = 1, size(JediVarNamesWithChannels)
    ! Retrieve data from JEDI
    call obsspace_get_db(ObsSpace, JediVarGroup, JediVarNamesWithChannels(iChannel), VarValue)

    ! Fill the OPS data structures
    where (VarValue /= MissingDouble)
      Coord2(:, iChannel) % Value = VarValue
    end where
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.
end subroutine cxvarobs_cxwriter_fillcoord2d

! ------------------------------------------------------------------------------

!> Populate the Ob % ReportFlags field.
!>
!> Observations are marked as rejected if the JEDI QC flags of any or all (depending on the
!> specified options) simulated variables are set to anything different from "pass".
subroutine cxvarobs_cxwriter_fillreportflags( &
  Ob, ObsSpace, Flags, RejectObsWithAnyVariableFailingQC, RejectObsWithAllVariablesFailingQC)
use oops_variables_mod
implicit none

! Subroutine arguments:
type(OB_type), intent(inout)             :: Ob
type(c_ptr), value, intent(in)           :: ObsSpace, Flags
logical, intent(in)                      :: RejectObsWithAnyVariableFailingQC
logical, intent(in)                      :: RejectObsWithAllVariablesFailingQC

! Local declarations:
type(oops_variables)                     :: ObsVariables
character(max_varname_length)            :: VarName
integer                                  :: NumObsVariables, iVar
integer(c_int)                           :: VarFlags(Ob % Header % NumObsLocal)

! Body:
call Ops_Alloc(Ob % Header % ReportFlags, "ReportFlags", &
               Ob % Header % NumObsLocal, Ob % ReportFlags)
Ob % ReportFlags = 0

ObsVariables = cxvarobs_obsdatavector_int_varnames(Flags)
NumObsVariables = ObsVariables % nvars()

if (RejectObsWithAnyVariableFailingQC) then
  Ob % ReportFlags = 0

  ! Set the FinalRejectReport bit in ReportFlags for observations with a non-zero QC flag
  ! in at least one variable.
  do iVar = 1, NumObsVariables
    VarName = ObsVariables % variable(iVar)
    call cxvarobs_obsdatavector_int_get(Flags, VarName, VarFlags)
    where (VarFlags > 0)
      Ob % ReportFlags = ibset(Ob % ReportFlags, FinalRejectReport)
    end where
  end do
else if (RejectObsWithAllVariablesFailingQC) then
  Ob % ReportFlags = ibset(Ob % ReportFlags, FinalRejectReport)

  ! Clear the FinalRejectReport bit in ReportFlags for observations with a zero QC flag
  ! in at least one variable.
  do iVar = 1, NumObsVariables
    VarName = ObsVariables % variable(iVar)
    call cxvarobs_obsdatavector_int_get(Flags, VarName, VarFlags)
    where (VarFlags == 0)
      Ob % ReportFlags = ibclr(Ob % ReportFlags, FinalRejectReport)
    end where
  end do
end if

end subroutine cxvarobs_cxwriter_fillreportflags

! ------------------------------------------------------------------------------

!> Populate the Ob % NumChans and/or Ob % ChanNum field.
!>
!> Ob % ChanNum is filled with the indices of channels that passed QC; the number of these channels
!> is stored in Ob % NumChans.
subroutine cxvarobs_cxwriter_fillchannumandnumchans( &
  Ob, ObsSpace, Channels, Flags, FillChanNum, FillNumChans)
implicit none

! Subroutine arguments:
type(OB_type), intent(inout)   :: Ob
type(c_ptr), value, intent(in) :: ObsSpace
integer(c_int), intent(in)     :: Channels(:)
type(c_ptr), value, intent(in) :: Flags
logical, intent(in)            :: FillChanNum, FillNumChans

! Local declarations:
integer(kind=8)                :: NumChannels
integer(kind=8)                :: ChannelIndices(Ob % Header % NumObsLocal, size(Channels))
integer(kind=8)                :: ChannelCounts(Ob % Header % NumObsLocal)

! Body:
NumChannels = size(Channels)
if (NumChannels == 0) return

call cxvarobs_cxwriter_findchannelspassingqc( &
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

end subroutine cxvarobs_cxwriter_fillchannumandnumchans

! ------------------------------------------------------------------------------

!> Find the indices of indices of channels that passed QC for the first (and normally only)
!> simulated variable.
subroutine cxvarobs_cxwriter_findchannelspassingqc( &
  NumObs, ObsSpace, Channels, Flags, ChannelIndices, ChannelCounts)
use oops_variables_mod
implicit none

! Subroutine arguments:
integer(kind=8), intent(in)               :: NumObs
type(c_ptr), value, intent(in)            :: ObsSpace
integer(c_int), intent(in)                :: Channels(:)
type(c_ptr), value, intent(in)            :: Flags
integer(kind=8), intent(out)              :: ChannelIndices(NumObs, size(Channels))
integer(kind=8), intent(out)              :: ChannelCounts(NumObs)

! Local declarations:
integer                        :: NumChannels
type(oops_variables)           :: Variables
character(max_varname_length)  :: VariableName
integer                        :: NumVariables, NumMultichannelVariables
integer                        :: iMultichannelVariable, iChannel, iVariable, iObs
integer(c_int)                 :: VarFlags(NumObs)
character(len=*), parameter    :: RoutineName = "cxvarobs_cxwriter_findchannelspassingqc"

! Body:
NumChannels = size(Channels)
if (NumChannels == 0) return

ChannelIndices = 0
ChannelCounts = 0

! We rely on an implementation detail of oops::Variables, namely that if any channels are defined,
! oops::Variables stores all channels of the first variable, then all channels of the second
! variable and so on; all variables have the same channels.
Variables = cxvarobs_obsdatavector_int_varnames(Flags)
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
    call cxvarobs_obsdatavector_int_get(Flags, VariableName, VarFlags)
    do iObs = 1, NumObs
      if (VarFlags(iObs) == 0) then ! This channel has passed quality control
        ChannelIndices(iObs, 1 + ChannelCounts(iObs)) = iChannel
        ChannelCounts(iObs) = ChannelCounts(iObs) + 1
      end if
    end do
  end do
end if

end subroutine cxvarobs_cxwriter_findchannelspassingqc

! ------------------------------------------------------------------------------

!> Populate the Ob % CorBriTemp field.
subroutine cxvarobs_cxwriter_fillcorbritemp(Ob, ObsSpace, Channels)
implicit none
! Subroutine arguments:
type(OB_type), intent(inout)             :: Ob
type(c_ptr), value, intent(in)           :: ObsSpace
integer(c_int), intent(in)               :: Channels(:)

! Local declarations:
type(ElementHeader_type)                 :: CorBriTempBiasHeader
real(kind=8), pointer                    :: CorBriTempBias(:,:)
character(len=*), parameter              :: RoutineName = "cxvarobs_cxwriter_fillcorbritemp"
character(len=256)                       :: ErrorMessage

! Body:

CorBriTempBias => null()

! Retrieve the uncorrected brightness temperature...
call cxvarobs_cxwriter_fillreal2d( &
  Ob % Header % CorBriTemp, "CorBriTemp", Ob % Header % NumObsLocal, Ob % CorBriTemp, &
  ObsSpace, Channels, "brightness_temperature", "ObsValue")

! ... and correct it by subtracting the bias.
if (Ob % Header % CorBriTemp % Present) then
  call cxvarobs_cxwriter_fillreal2d( &
    CorBriTempBiasHeader, "CorBriTemp", Ob % Header % NumObsLocal, CorBriTempBias, &
    ObsSpace, Channels, "brightness_temperature", "ObsBias")
  if (CorBriTempBiasHeader % Present) then
    where (Ob % CorBriTemp /= RMDI .and. CorBriTempBias /= RMDI)
      Ob % CorBriTemp = Ob % CorBriTemp - CorBriTempBias
    end where
  else
    write (ErrorMessage, '(A)') "Warning: variable brightness_temperature*@ObsBias not found"
    call gen_warn(RoutineName, ErrorMessage)
  end if
end if ! brightness_temperature@ObsValue not present? OPS will produce a warning
       ! -- we don't need to duplicate it.

end subroutine cxvarobs_cxwriter_fillcorbritemp

! ------------------------------------------------------------------------------

!> Fill the members of OB_type required to take the GPSRO point drift into account.
subroutine cxvarobs_cxwriter_fillgpsrotpddependentfields(Ob, ObsSpace)
implicit none
! Subroutine arguments:
type(OB_type), intent(inout)             :: Ob
type(c_ptr), value, intent(in)           :: ObsSpace

! Body:

call cxvarobs_cxwriter_fillsatid(Ob, ObsSpace)
! TODO(someone): Replace the placeholder in the call below with an appropriate variable name
! and group, once they're known.
call cxvarobs_cxwriter_fillinteger( &
  Ob % Header % RO_quality, "RO_quality", Ob % Header % NumObsLocal, Ob % RO_quality, &
  ObsSpace, "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP")
! TODO(someone): Replace "latitude" and "longitude" variable names in the two calls below with
! appropriate GPSRO-specific variable names, once they're known. The group name may need to be
! adjusted as well.
call cxvarobs_cxwriter_fillreal( &
  Ob % Header % ro_occ_lat, "ro_occ_lat", Ob % Header % NumObsLocal, Ob % ro_occ_lat, &
  ObsSpace, "latitude", "MetaData")
call cxvarobs_cxwriter_fillreal( &
  Ob % Header % ro_occ_lon, "ro_occ_lon", Ob % Header % NumObsLocal, Ob % ro_occ_lon, &
  ObsSpace, "longitude", "MetaData")

end subroutine cxvarobs_cxwriter_fillgpsrotpddependentfields

! ------------------------------------------------------------------------------

!> Fill the Ob % SatId field.
!>
!> This is done in a separate routine because this field is filled from two places in the code.
subroutine cxvarobs_cxwriter_fillsatid(Ob, ObsSpace)
implicit none
! Subroutine arguments:
type(OB_type), intent(inout)             :: Ob
type(c_ptr), value, intent(in)           :: ObsSpace

! Body:
call cxvarobs_cxwriter_fillinteger( &
  Ob % Header % SatId, "SatId", Ob % Header % NumObsLocal, Ob % SatId, &
  ObsSpace, "satellite_id", "MetaData")

end subroutine cxvarobs_cxwriter_fillsatid

! ------------------------------------------------------------------------------

!> Return an array containing the names of JEDI variables storing individual channels of the
!> variable \p VarName. If the list of channels is empty, this means the variable in question is
!> 1D and hence the returned array contains just the single string \p VarName.
function cxvarobs_cxwriter_varnames_with_channels(VarName, Channels) result(VarNames)
implicit none
! Subroutine arguments:
character(len=*), intent(in)                   :: VarName
integer(c_int), intent(in)                     :: Channels(:)

! Local declarations:
character(len=max_varname_with_channel_length) :: VarNames(max(size(Channels), 1))
integer                                        :: i

if (size(Channels) == 0) then
  VarNames(1) = VarName
else
  do i = 1, size(Channels)
    write (VarNames(i),'(A,"_",I0)') VarName, Channels(i)
  end do
end if
end function cxvarobs_cxwriter_varnames_with_channels

! ------------------------------------------------------------------------------

!> Prepare a CxHeader object to be given to the OPS function writing a VarObs file.
subroutine cxvarobs_cxwriter_populatecxheader(self, CxHeader)
implicit none
! Subroutine arguments:
type(cxvarobs_cxwriter), intent(in) :: self
type(UM_header_type), intent(inout)     :: CxHeader

! Local declarations:
integer(c_int)                          :: year, month, day, hour, minute, second
TYPE (DateTime_type)                    :: now

! Body:

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

end subroutine cxvarobs_cxwriter_populatecxheader

end module cxvarobs_cxwriter_mod
