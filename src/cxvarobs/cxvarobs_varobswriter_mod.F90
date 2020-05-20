! (C) Copyright 2020 Met Office UK
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

!> Fortran module to implement varobswriter

module cxvarobs_varobswriter_mod

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
    DebugMode,              &
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
use OpsMod_GPSRO, only: GPSRO_TPD
use OpsMod_Radar, only: RadFamily
use OpsMod_SatRad_RTmodel, only: nlevels_strat_varobs
use OpsMod_Varfields
use OpsMod_Varobs

implicit none
public :: cxvarobs_varobswriter_create, cxvarobs_varobswriter_delete, &
          cxvarobs_varobswriter_prior, cxvarobs_varobswriter_post
private
integer, parameter :: max_string=800
! Maximum length of a variable name
integer, parameter :: max_varname_length=MAXVARLEN
! Maximum length of a variable name with channel suffix
integer, parameter :: max_varname_with_channel_length=max_varname_length + 10

! ------------------------------------------------------------------------------
type, public :: cxvarobs_varobswriter
private
  integer(kind=8) :: ObsGroup
  type(datetime)  :: ValidityTime  ! Corresponds to OPS validity time

  logical         :: AccountForGPSROTangentPointDrift
  logical         :: UseRadarFamily

  integer(kind=8) :: FH_VertCoord
  integer(kind=8) :: FH_HorizGrid
  integer(kind=8) :: FH_GridStagger
  integer(kind=8) :: FH_ModelVersion

  integer(kind=8) :: IC_TorTheta
  integer(kind=8) :: IC_ShipWind
  integer(kind=8) :: IC_GroundGPSOperator
  integer(kind=8) :: IC_GPSRO_Operator_pseudo
  integer(kind=8) :: IC_GPSRO_Operator_press

  integer(kind=8) :: IC_XLen
  integer(kind=8) :: IC_YLen
  integer(kind=8) :: IC_PLevels
  integer(kind=8) :: IC_WetLevels

  real(kind=8) RC_LongSpacing
  real(kind=8) RC_LatSpacing
  real(kind=8) RC_FirstLat
  real(kind=8) RC_FirstLong
  real(kind=8) RC_PoleLat
  real(kind=8) RC_PoleLong

  type(ufo_geovals), pointer :: GeoVals
end type cxvarobs_varobswriter

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------

! Set up a cxvarobs_varobswriter. Returns .true. on success and .false. on failure.
function cxvarobs_varobswriter_create(self, f_conf, geovars)
implicit none
type(cxvarobs_varobswriter), intent(inout) :: self
type(fckit_configuration), intent(in)      :: f_conf
type(oops_variables), intent(inout)        :: geovars
logical(c_bool)                            :: cxvarobs_varobswriter_create

character(len=:), allocatable              :: string
integer(kind=c_int)                        :: int
logical                                    :: bool
real(kind=c_double)                        :: double
logical                                    :: found

integer(kind=8), parameter                 :: zero = 0

character(len=*), parameter :: RoutineName = "cxvarobs_varobswriter_create"
character(len=200)          :: ErrorMessage

cxvarobs_varobswriter_create = .true.

call Gen_SetupControl(DefaultDocURL)
call Ops_InitMPI

GeneralMode = DebugMode

! TODO: set self%geovars (list of variables to use from GeoVaLs) if needed

if (.not. f_conf % get("obs_group", string)) then
  call gen_warn(RoutineName, "Mandatory obs_group option not found")
  cxvarobs_varobswriter_create = .false.
  goto 9999
end if
self % ObsGroup = OpsFn_ObsGroupNameToNum(string)

if (.not. f_conf % get("validity_time", string)) then
  call gen_warn(RoutineName, "Mandatory validity_time option not found")
  cxvarobs_varobswriter_create = .false.
  goto 9999
end if
call datetime_create(string, self % validitytime)

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
case ("depth")
  self % FH_VertCoord = FH_VertCoord_Depth
case ("cp")
  self % FH_VertCoord = FH_VertCoord_CP
case ("wave")
  self % FH_VertCoord = FH_VertCoord_Wave
case default
  write (ErrorMessage, '("FH_VertCoord code not recognised: ",A)') string
  call gen_warn(RoutineName, ErrorMessage)
  cxvarobs_varobswriter_create = .false.
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
  cxvarobs_varobswriter_create = .false.
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
  cxvarobs_varobswriter_create = .false.
  goto 9999
end select

int = 0
found = f_conf % get("FH_ModelVersion", int)
self % FH_ModelVersion = int

string = "t"
found = f_conf % get("IC_TorTheta", string)
select case (ops_to_lower_case(string))
case ("t")
  self % IC_TorTheta = IC_TorTheta_T
case ("theta")
  self % IC_TorTheta = IC_TorTheta_Theta
case default
  write (ErrorMessage, '("IC_TorTheta code not recognised: ",A)') string
  call gen_warn(RoutineName, ErrorMessage)
  cxvarobs_varobswriter_create = .false.
  goto 9999
end select

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
  cxvarobs_varobswriter_create = .false.
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

call cxvarobs_varobswriter_addrequiredgeovars(self, geovars)

9999 if (allocated(string)) deallocate(string)

end function cxvarobs_varobswriter_create

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_delete(self)
implicit none
type(cxvarobs_varobswriter), intent(inout) :: self

call datetime_delete(self % validitytime)

end subroutine cxvarobs_varobswriter_delete

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_prior(self, ObsSpace, GeoVals)
implicit none
type(cxvarobs_varobswriter), intent(inout) :: self
type(c_ptr), value, intent(in)             :: ObsSpace
type(ufo_geovals), intent(in), pointer     :: GeoVals

self % GeoVals => GeoVals

end subroutine cxvarobs_varobswriter_prior

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_post(self, ObsSpace, nchannels, Channels, &
                                      Flags, ObsErrors, nvars, nlocs, hofx)
implicit none
type(cxvarobs_varobswriter), intent(in) :: self
type(c_ptr), value, intent(in) :: ObsSpace
integer,            intent(in) :: nchannels
integer,            intent(in) :: Channels(nchannels)
type(c_ptr), value, intent(in) :: Flags, ObsErrors
integer,            intent(in) :: nvars, nlocs
real(c_double),     intent(in) :: hofx(nvars, nlocs)

type(OB_type)                  :: obs
type(UM_header_type)           :: CxHeader
integer(kind=8)                :: VarFields(ActualMaxVarfield)
integer(kind=8)                :: NumVarObsTotal

obs % Header % obsgroup = self % obsgroup

print *, 'Calling Ops_SetupObType in post'
call Ops_SetupObType(obs)
print *, 'Called Ops_SetupObType'

obs % Header % numobstotal = obsspace_get_gnlocs(ObsSpace)
obs % Header % numobslocal = obsspace_get_nlocs(ObsSpace)

print *, 'obsspace_get_gnlocs: ', obs % Header % numobstotal
print *, 'obsspace_get_gnlocs: ', obsspace_get_gnlocs(ObsSpace)
print *, 'obsspace_get_nlocs: ', obs % Header % numobslocal

Obs % Header % NumCXBatches = 1
allocate(Obs % Header % ObsPerBatchPerPE(Obs % Header % NumCXBatches, 0:nproc - 1))
Obs % Header % ObsPerBatchPerPE(1,mype) = obs % Header % numobslocal

call Ops_ReadVarobsControlNL(self % obsgroup, VarFields) ! TODO(wsmigaj): move to separate function?
call cxvarobs_varobswriter_populateobservations(self, VarFields, ObsSpace, Channels, &
                                                Flags, ObsErrors, obs)
call cxvarobs_varobswriter_populatecxheader(self, CxHeader)

call Ops_CreateVarobs (Obs,                 & ! in
                       CxHeader,            & ! in
                       AssimDataFormat_VAR, &
                       NumVarobsTotal)        ! TODO: PGEBd

call obs % deallocate()
! DEALLOCATE(Obs % Header % ObsPerBatchPerPE)

end subroutine cxvarobs_varobswriter_post

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_addrequiredgeovars(self, geovars)
implicit none
type(cxvarobs_varobswriter), intent(in) :: self
type(oops_variables), intent(inout)     :: geovars

integer(kind=8)                         :: VarFields(ActualMaxVarfield)
integer                                 :: i

call Ops_ReadVarobsControlNL(self % obsgroup, VarFields)

do i = 1, size(VarFields)
  select case (VarFields(i))
  case (VarField_modelsurface)
    ! TODO(someone): "land_type_index" may not be the right geoval to use. If it isn't, change it
    ! here and in cxvarobs_varobswriter_populateobservations.
    call geovars % push_back("land_type_index")
  end select
end do

end subroutine cxvarobs_varobswriter_addrequiredgeovars

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_populateobservations(self, VarFields, ObsSpace, Channels, &
                                                      Flags, ObsErrors, Ob)
implicit none
type(cxvarobs_varobswriter),  intent(in) :: self
integer(kind=8), intent(in)              :: VarFields(:)
type(c_ptr), value, intent(in)           :: ObsSpace
integer(c_int), intent(in)               :: Channels(:)
type(c_ptr), value, intent(in)           :: Flags, ObsErrors
type(OB_type), intent(inout)             :: Ob

character(len=*), parameter              :: RoutineName = "cxvarobs_varobswriter_populateobservations"
character(len=80)                        :: ErrorMessage

integer                                  :: nVarFields
integer                                  :: iVarField

integer(c_int64_t)                       :: TimeOffsetsInSeconds(Ob % Header % NumObsLocal)

integer                                  :: Zcode

logical                                  :: FillChanNum = .false.
logical                                  :: FillNumChans = .false.

nVarFields = size(VarFields)

call Ops_Alloc(Ob % Header % Latitude, "Latitude", Ob % Header % NumObsLocal, Ob % Latitude)
call obsspace_get_db(ObsSpace, "MetaData", "latitude", Ob % Latitude)

call Ops_Alloc(Ob % Header % Longitude, "Longitude", Ob % Header % NumObsLocal, Ob % Longitude)
call obsspace_get_db(ObsSpace, "MetaData", "longitude", Ob % Longitude)

call Ops_Alloc(Ob % Header % Time, "Time", Ob % Header % NumObsLocal, Ob % Time)
call cxvarobs_obsspace_get_db_datetime_offset_in_seconds( &
  ObsSpace, "MetaData", "datetime", self % validitytime, TimeOffsetsInSeconds)
Ob % Time = TimeOffsetsInSeconds

call cxvarobs_varobswriter_fillreportflags(Ob, ObsSpace, Flags)

! TODO(someone): This call to Ops_Alloc() will need to be replaced by
! call cxvarobs_varobswriter_fillinteger( &
!   Ob % Header % surface, "surface", Ob % Header % NumObsLocal, Ob % surface, &
!   "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP", ObsSpace)
! with the placeholders replaced by an appropriate variable name and group.
! We call Ops_Alloc() because the Ops_CreateVarobs terminates prematurely if the ObsType array
! doesn't exist.
call Ops_Alloc(Ob % Header % ObsType, "ObsType", Ob % Header % NumObsLocal, Ob % ObsType)

if (obsspace_has(ObsSpace, "MetaData", "station_id")) then
  call Ops_Alloc(Ob % Header % Callsign, "Callsign", Ob % Header % NumObsLocal, Ob % Callsign)
  call cxvarobs_obsspace_get_db_string( &
    ObsSpace, "MetaData", "station_id", int(LenCallSign, kind=4), Ob % Callsign)
end if

call cxvarobs_varobswriter_fillcoord2d( &
  Ob % Header % PlevelsA, "PlevelsA", Ob % Header % NumObsLocal, Ob % PlevelsA, &
  "air_pressure", "MetaData", ObsSpace, Channels)

GPSRO_TPD = self % AccountForGPSROTangentPointDrift
if (Ob % header % ObsGroup == ObsGroupGPSRO .and. GPSRO_TPD) then
  call cxvarobs_varobswriter_fillgpsrotpddependentfields(Ob, ObsSpace)
end if

RadFamily = self % UseRadarFamily
if (RadFamily) then
  call cxvarobs_varobswriter_fillinteger( &
    Ob % Header % Family, "Family", Ob % Header % NumObsLocal, Ob % Family, &
    "radar_family", "MetaData", ObsSpace)
end if

do iVarField = 1, nVarFields
  Zcode = ZcodeUnused ! TODO: fill in correctly

  select case (VarFields(iVarField))
    case (imdi)
      cycle
    case (VarField_pstar)
      call cxvarobs_varobswriter_fillelementtypefromsimulatedvariable( &
        Ob % Header % pstar, "pstar", Ob % Header % NumObsLocal, Ob % pstar, &
        "surface_pressure", ObsSpace, Flags, ObsErrors)
    case (VarField_theta)
      ! TODO(wsmigaj): check if air_potential_temperature is the correct variable name
      ! (it isn't used in JEDI, but virtual_temperature is)
      call cxvarobs_varobswriter_fillelementtype2dfromsimulatedvariable( &
        Ob % Header % theta, "theta", Ob % Header % NumObsLocal, Ob % theta, &
        "air_potential_temperature", ObsSpace, Channels, Flags, ObsErrors)
    case (VarField_temperature)
      if (Ob % Header % ObsGroup == ObsGroupSurface) then
        call cxvarobs_varobswriter_fillelementtypefromsimulatedvariable( &
          Ob % Header % t2, "t2", Ob % Header % NumObsLocal, Ob % t2, &
          "air_temperature", ObsSpace, Flags, ObsErrors)
      else
        call cxvarobs_varobswriter_fillelementtype2dfromsimulatedvariable( &
          Ob % Header % t, "t", Ob % Header % NumObsLocal, Ob % t, &
          "air_temperature", ObsSpace, Channels, Flags, ObsErrors)
      end if
    case (VarField_rh)
      if (Ob % Header % ObsGroup == ObsGroupSurface) then
        call cxvarobs_varobswriter_fillelementtypefromsimulatedvariable( &
          Ob % Header % rh2, "rh2", Ob % Header % NumObsLocal, Ob % rh2, &
          "relative_humidity", ObsSpace, Flags, ObsErrors)
      else
        call cxvarobs_varobswriter_fillelementtype2dfromsimulatedvariable( &
          Ob % Header % rh, "rh", Ob % Header % NumObsLocal, Ob % rh, &
          "relative_humidity", ObsSpace, Channels, Flags, ObsErrors)
      end if
    case (VarField_u)
      if (Ob % Header % ObsGroup == ObsGroupSurface .or. &
          Ob % Header % ObsGroup == ObsGroupScatwind) then
        call cxvarobs_varobswriter_fillelementtypefromsimulatedvariable( &
          Ob % Header % u10, "u10", Ob % Header % NumObsLocal, Ob % u10, &
          "eastward_wind", ObsSpace, Flags, ObsErrors)
      else
        call cxvarobs_varobswriter_fillelementtype2dfromsimulatedvariable( &
          Ob % Header % u, "u", Ob % Header % NumObsLocal, Ob % u, &
          "eastward_wind", ObsSpace, Channels, Flags, ObsErrors)
      end if
    case (VarField_v)
      if (Ob % Header % ObsGroup == ObsGroupSurface .or. &
          Ob % Header % ObsGroup == ObsGroupScatwind) then
        call cxvarobs_varobswriter_fillelementtypefromsimulatedvariable( &
          Ob % Header % v10, "v10", Ob % Header % NumObsLocal, Ob % v10, &
          "northward_wind", ObsSpace, Flags, ObsErrors)
      else
        call cxvarobs_varobswriter_fillelementtype2dfromsimulatedvariable( &
          Ob % Header % v, "v", Ob % Header % NumObsLocal, Ob % v, &
          "northward_wind", ObsSpace, Channels, Flags, ObsErrors)
      end if
    case (VarField_logvis)
      call Ops_Alloc(Ob % Header % logvis, "logvis", Ob % Header % NumObsLocal, Ob % logvis)
    case (VarField_tcwv)
      call Ops_Alloc(Ob % Header % tcwv, "TCWV", Ob % Header % NumObsLocal, Ob % tcwv)
    case (VarField_windspeed)
      call Ops_Alloc(Ob % Header % WindSpeed, "WindSpeed", Ob % Header % NumObsLocal, Ob % WindSpeed)
    case (VarField_lwp)
      call Ops_Alloc(Ob % Header % lwp, "LWP", Ob % Header % NumObsLocal, Ob % lwp)
    case (VarField_britemp)
      call cxvarobs_varobswriter_fillcorbritemp(Ob, ObsSpace, Channels)
    case (VarField_tskin)
      ! TODO(someone): This will come from a variable generated by the 1D-Var filter. Its name and
      ! group are not known yet. Once they are, replace the placeholders in the call below.
      call cxvarobs_varobswriter_fillelementtypefromnormalvariable( &
        Ob % Header % Tskin, "Tskin", Ob % Header % NumObsLocal, Ob % Tskin, &
        ObsSpace, "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP")
    case (VarField_gpstzdelay)
      call Ops_Alloc(Ob % Header % GPSTZDelay, "GPSTZDelay", Ob % Header % NumObsLocal, Ob % GPSTZDelay)
    case (VarField_GPS_Station_Height)
      call Ops_Alloc(Ob % Header % Zstation, "Zstation", Ob % Header % NumObsLocal, Ob % Zstation)
    case (VarField_mwemiss)
      ! TODO(someone): This will come from a variable generated by the 1D-Var filter. Its name and
      ! group are not known yet. Once they are, replace the placeholders in the call below.
      call cxvarobs_varobswriter_fillreal2d( &
        Ob % Header % MwEmiss, "MwEmiss", Ob % Header % NumObsLocal, Ob % MwEmiss, &
        "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP", ObsSpace, Channels)
    case (VarField_TCozone)
      ! TODO(someone): This will come from an ObsFunction or a variable generated by a filter. Its
      ! name and group are not known yet. Once they are, replace the placeholders in the call below.
      call cxvarobs_varobswriter_fillreal( &
        Ob % Header % TCozone, "TCozone", Ob % Header % NumObsLocal, Ob % TCozone, &
        "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP", ObsSpace)
    case (VarField_satzenith)
      call cxvarobs_varobswriter_fillreal( &
        Ob % Header % SatZenithAngle, "SatZenithAngle", Ob % Header % NumObsLocal, Ob % SatZenithAngle, &
        "sensor_zenith_angle", "MetaData", ObsSpace)
    case (VarField_scanpos)
      call Ops_Alloc(Ob % Header % ScanPosition, "ScanPosition", Ob % Header % NumObsLocal, Ob % ScanPosition)
    case (VarField_surface)
      ! TODO(someone): This will come from an ObsFunction or a variable generated by a filter. Its
      ! name and group are not known yet. Once they are, replace the placeholders in the call below.
      call cxvarobs_varobswriter_fillinteger( &
        Ob % Header % surface, "surface", Ob % Header % NumObsLocal, Ob % surface, &
        "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP", ObsSpace)
    case (VarField_elevation)
      call Ops_Alloc(Ob % Header % elevation, "elevation", Ob % Header % NumObsLocal, Ob % elevation)
    case (VarField_modelsurface)
      ! TODO(someone): "land_type_index" may not be the right geoval to use. If it isn't, change it
      ! here and in cxvarobs_varobswriter_addrequiredgeovars.
      call cxvarobs_varobswriter_fillrealfromgeoval( &
        Ob % Header % ModelSurface, "ModelSurface", Ob % Header % NumObsLocal, Ob % ModelSurface, &
        "land_type_index", self % GeoVals)
    case (VarField_modelorog)
      call Ops_Alloc(Ob % Header % ModelOrog, "ModelOrog", Ob % Header % NumObsLocal, Ob % ModelOrog)
    case (VarField_stratt)
      ! TODO(someone): This will come from a (2D) variable generated by the 1D-Var filter. Its name
      ! and group are not known yet. An extra difficulty is that the second dimension (the number of
      ! stratospheric temperature levels) is not related to the number of channels. Hopefully by the
      ! time this is filled in IODA will provide a convenient interface to 2D variables.
      ! UseLevelSubset = .true.
      ! NumLevs = nlevels_strat_varobs
      ! call Ops_Alloc(Ob % Header % t, "t", Ob % Header % NumObsLocal, Ob % t)
    case (VarField_satid)
      call cxvarobs_varobswriter_fillsatid(Ob, ObsSpace)
    case (VarField_satazimth)
      call Ops_Alloc(Ob % Header % SatAzimth, "SatAzimth", Ob % Header % NumObsLocal, Ob % SatAzimth)
    case (VarField_localazimuth)
      call Ops_Alloc(Ob % Header % LocalAzimuth, "LocalAzimuth", Ob % Header % NumObsLocal, Ob % LocalAzimuth)
    case (VarField_solzenith)
      call cxvarobs_varobswriter_fillreal( &
        Ob % Header % SolarZenith, "SolarZenith", Ob % Header % NumObsLocal, Ob % SolarZenith, &
        "solar_zenith_angle", "MetaData", ObsSpace)
    case (VarField_solazimth)
      call Ops_Alloc(Ob % Header % SolarAzimth, "SolarAzimth", Ob % Header % NumObsLocal, Ob % SolarAzimth)
    case (VarField_iremiss)
      ! TODO(someone): This will come from a variable generated by the 1D-Var filter. Its name and
      ! group are not known yet. Once they are, replace the placeholders in the call below.
      call cxvarobs_varobswriter_fillreal( &
        Ob % Header % IREmiss, "IREmiss", Ob % Header % NumObsLocal, Ob % IREmiss, &
        "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP", ObsSpace)
    case (VarField_cloudtopp)
      call Ops_Alloc(Ob % Header % CloudTopP, "CloudTopP", Ob % Header % NumObsLocal, Ob % CloudTopP)
    case (VarField_cloudfrac)
      call Ops_Alloc(Ob % Header % CloudFrac, "CloudFrac", Ob % Header % NumObsLocal, Ob % CloudFrac)
    case (VarField_vnatovpp)
      call Ops_Alloc(Ob % Header % VnATOVPP, "VnATOVPP", Ob % Header % NumObsLocal, Ob % VnATOVPP)
    case (VarField_procoption)
      call Ops_Alloc(Ob % Header % ATOVSProcOption, "ATOVSProcOption", Ob % Header % NumObsLocal, Ob % ATOVSProcOption)
    case (VarField_amsusurface)
      call Ops_Alloc(Ob % Header % AMSUsurface, "AMSUsurface", Ob % Header % NumObsLocal, Ob % AMSUsurface)
    case (VarField_hirs_temp)
      call Ops_Alloc(Ob % Header % HIRS_Temp, "HIRS_Temp", Ob % Header % NumObsLocal, Ob % HIRS_Temp)
    case (VarField_amsua1_temp)
      call Ops_Alloc(Ob % Header % AMSUa1_Temp, "AMSUa1_Temp", Ob % Header % NumObsLocal, Ob % AMSUa1_Temp)
    case (VarField_amsua2_temp)
      call Ops_Alloc(Ob % Header % AMSUa2_Temp, "AMSUa2_Temp", Ob % Header % NumObsLocal, Ob % AMSUa2_Temp)
    case (VarField_amsub_temp)
      call Ops_Alloc(Ob % Header % AMSUb_Temp, "AMSUb_Temp", Ob % Header % NumObsLocal, Ob % AMSUb_Temp)
    case (VarField_cloud)
      call Ops_Alloc(Ob % Header % Cloud, "Cloud", Ob % Header % NumObsLocal, Ob % Cloud)
    case (VarField_rainrate)
      call Ops_Alloc(Ob % Header % Rainrate, "Rainrate", Ob % Header % NumObsLocal, Ob % Rainrate)
    case (VarField_snowrate)
      call Ops_Alloc(Ob % Header % Snowrate, "Snowrate", Ob % Header % NumObsLocal, Ob % Snowrate)
    case (VarField_u10ambwind)
      call Ops_Alloc(Ob % Header % u10AmbWind, "u10AmbWind", Ob % Header % NumObsLocal, Ob % u10AmbWind)
    case (VarField_v10ambwind)
      call Ops_Alloc(Ob % Header % v10AmbWind, "v10AmbWind", Ob % Header % NumObsLocal, Ob % v10AmbWind)
    case (VarField_pcorrect)
      call Ops_Alloc(Ob % Header % AWPriorPCorrect, "AWPriorPCorrect", Ob % Header % NumObsLocal, Ob % AWPriorPCorrect)
    case (VarField_NumChans)
      FillNumChans = .true.
    case (VarField_ChanNum)
      FillChanNum = .true.
    case (VarField_Emissivity)
      ! TODO(someone): This will come from a variable generated by the 1D-Var filter. Its name and
      ! group are not known yet. Once they are, replace the placeholders in the call below.
      call cxvarobs_varobswriter_fillreal2d( &
        Ob % Header % Emissivity, "Emissivity", Ob % Header % NumObsLocal, Ob % Emissivity, &
        "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP", ObsSpace, Channels)
    case (VarField_QCinfo)
      ! TODO(someone): This will come from a variable generated by the 1D-Var filter. Its name and
      ! group are not known yet. Once they are, replace the placeholders in the call below.
      call cxvarobs_varobswriter_fillinteger( &
        Ob % Header % QCinfo, "QCinfo", Ob % Header % NumObsLocal, Ob % QCinfo, &
        "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP", ObsSpace)
    case (VarField_SBUVozone)
      call Ops_Alloc(Ob % Header % SBUVozone, "SBUVozone", Ob % Header % NumObsLocal, Ob % SBUVozone)
    case (VarField_RadialVelocity)
        call Ops_Alloc(Ob % Header % RadialVelocSO, "RadialVelocSO", Ob % Header % NumObsLocal, Ob % RadialVelocSO)
    case (VarField_Reflectivity)
      call Ops_Alloc(Ob % Header % ReflectivitySO, "ReflectivitySO", Ob % Header % NumObsLocal, Ob % ReflectivitySO)
    case (VarField_ReflectivityR)
      call Ops_Alloc(Ob % Header % ReflectivityR, "ReflectivityR", Ob % Header % NumObsLocal, Ob % ReflectivityR)
    case (VarField_ReflectivityI)
      call Ops_Alloc(Ob % Header % ReflectivityI, "ReflectivityI", Ob % Header % NumObsLocal, Ob % ReflectivityI)
    case (VarField_RadarBeamElev)
      call Ops_Alloc(Ob % Header % RadarBeamElev, "RadarBeamElev", Ob % Header % NumObsLocal, Ob % RadarBeamElev)
    case (VarField_RadarObRange)
      call Ops_Alloc(Ob % Header % RadarObRange, "RadarObRange", Ob % Header % NumObsLocal, Ob % RadarObRange)
    case (VarField_RadarObAzim)
      call Ops_Alloc(Ob % Header % RadarObAzim, "RadarObAzim", Ob % Header % NumObsLocal, Ob % RadarObAzim)
    case (VarField_RadIdent)
      call Ops_Alloc(Ob % Header % RadIdent, "RadIdent", Ob % Header % NumObsLocal, Ob % RadIdent)
    case (VarField_RadAltAboveMSL)
      call Ops_Alloc(Ob % Header % RadAltAboveMSL, "RadAltAboveMSL", Ob % Header % NumObsLocal, Ob % RadAltAboveMSL)
    case (VarField_RadNoiseLvl)
      call Ops_Alloc(Ob % Header % RadNoiseLvl, "RadNoiseLvl", Ob % Header % NumObsLocal, Ob % RadNoiseLvl)
    case (VarField_RadFlag)
      call Ops_Alloc(Ob % Header % RadFlag, "RadFlag", Ob % Header % NumObsLocal, Ob % RadFlag)
    case (VarField_clw)
      call Ops_Alloc(Ob % Header % clw, "clw", Ob % Header % NumObsLocal, Ob % clw)
    case (VarField_refrac)
      call Ops_Alloc(Ob % Header % refrac, "refrac", Ob % Header % NumObsLocal, Ob % refrac)
    case (VarField_z)
      call Ops_Alloc(Ob % Header % z, "z", Ob % Header % NumObsLocal, Ob % z)
    case (VarField_BendingAngle)
      if (GPSRO_TPD) then
        ! TODO(someone): Replace the placeholder in the call with an appropriate variable name,
        ! once it is known.
        call cxvarobs_varobswriter_fillelementtype2dfromsimulatedvariable( &
          Ob % Header % BendingAngleAll, "BendingAngleAll", Ob % Header % NumObsLocal, Ob % BendingAngleAll, &
          "PLACEHOLDER_VARIABLE_NAME", ObsSpace, Channels, Flags, ObsErrors)
      else
        call cxvarobs_varobswriter_fillelementtype2dfromsimulatedvariable( &
          Ob % Header % BendingAngle, "BendingAngle", Ob % Header % NumObsLocal, Ob % BendingAngle, &
          "bending_angle", ObsSpace, Channels, Flags, ObsErrors)
      end if
    case (VarField_ImpactParam)
       if (GPSRO_TPD) then
         ! TODO(someone): Replace the placeholder in the call with an appropriate variable name,
         ! once it is known.
         call cxvarobs_varobswriter_fillelementtype2dfromnormalvariable( &
           Ob % Header % ImpactParamAll, "ImpactParamAll", Ob % Header % NumObsLocal, Ob % ImpactParamAll, &
           ObsSpace, Channels, "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP")
       else
         call cxvarobs_varobswriter_fillelementtype2dfromnormalvariable( &
           Ob % Header % ImpactParam, "ImpactParam", Ob % Header % NumObsLocal, Ob % ImpactParam, &
           ObsSpace, Channels, "impact_parameter", "MetaData")
       end if
    case (VarField_RO_Rad_Curv)
      call cxvarobs_varobswriter_fillelementtypefromnormalvariable( &
        Ob % Header % RO_Rad_Curv, "RO_Rad_Curv", Ob % Header % NumObsLocal, Ob % RO_Rad_Curv, &
        ObsSpace, "earth_radius_of_curvature", "MetaData")
    case (VarField_RO_geoid_und)
      call cxvarobs_varobswriter_fillelementtypefromnormalvariable( &
        Ob % Header % RO_geoid_und, "RO_geoid_und", Ob % Header % NumObsLocal, Ob % RO_geoid_und, &
        ObsSpace, "geoid_height_above_reference_ellipsoid", "MetaData")
    case (VarField_AOD)
      call cxvarobs_varobswriter_fillelementtype2dfromsimulatedvariable( &
        Ob % Header % AOD, "AOD", Ob % Header % NumObsLocal, Ob % AOD, &
        "aerosol_optical_depth", ObsSpace, Channels, Flags, ObsErrors)
    case (VarField_BriTempVarError)
      call Ops_Alloc(Ob % Header % BriTempVarError, "BriTempVarError", Ob % Header % NumObsLocal, Ob % BriTempVarError)
    case (VarField_CloudRTError)
      call Ops_Alloc(Ob % Header % CloudRTError, "CloudRTError", Ob % Header % NumObsLocal, Ob % CloudRTError)
    case (VarField_CloudRTBias)
      call Ops_Alloc(Ob % Header % CloudRTBias, "CloudRTBias", Ob % Header % NumObsLocal, Ob % CloudRTBias)
    case (VarField_BiasPredictors)
      call Ops_Alloc(Ob % Header % BiasPredictors, "BiasPredictors", Ob % Header % NumObsLocal, Ob % BiasPredictors)
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
      call cxvarobs_varobswriter_fillreal2dfromgeoval( &
        Ob % Header % level_lat, "level_lat", Ob % Header % NumObsLocal, Ob % level_lat, &
        "PLACEHOLDER_VARIABLE_NAME", self % GeoVals)
    case (VarField_LevelLon)
      ! IF (PRESENT (RepObs)) THEN
      !   ObHdrVrbl = RepObs % Header % model_level_lon
      ! ELSE
      !   CALL Ops_Alloc(Ob % Header % level_lon, "level_lon", Ob % Header % NumObsLocal, Ob % level_lon)
      ! END IF
    case (VarField_RainAccum)
      call Ops_Alloc(Ob % Header % RainAccum, "RainAccum", Ob % Header % NumObsLocal, Ob % RainAccum)
    case (VarField_CeilBackscatter)
      call Ops_Alloc(Ob % Header % CeilBackscatter, "CeilBackscatter", Ob % Header % NumObsLocal, Ob % CeilBackscatter)
    case (VarField_CeilRange)
      call Ops_Alloc(Ob % Header % CeilRange, "CeilRange", Ob % Header % NumObsLocal, Ob % CeilRange)
    case (VarField_CeilSiteId)
      call Ops_Alloc(Ob % Header % CeilSiteID, "CeilSiteID", Ob % Header % NumObsLocal, Ob % CeilSiteID)
    case (VarField_CeilScanIdent)
      call Ops_Alloc(Ob % Header % CeilScanIdent, "CeilScanIdent", Ob % Header % NumObsLocal, Ob % CeilScanIdent)
    case (VarField_airqal_consttype)
      call Ops_Alloc(Ob % Header % csnt_typ, "CSNT_TYP", Ob % Header % NumObsLocal, Ob % csnt_typ)
    case (VarField_airqal_massdensity)
      call Ops_Alloc(Ob % Header % mass_dnsty, "MASS_DNSTY", Ob % Header % NumObsLocal, Ob % mass_dnsty)
    case (VarField_airqal_massdensityscale)
      call Ops_Alloc(Ob % Header % dcml_scl_mass_dnsty, "DCML_SCL_MASS_DNSTY", Ob % Header % NumObsLocal, Ob % dcml_scl_mass_dnsty)
    case (VarField_HLOSwind)
      call Ops_Alloc(Ob % Header % HLOSwind, "HLOSwind", Ob % Header % NumObsLocal, Ob % HLOSwind)
    case (VarField_ProfileNo)
      call Ops_Alloc(Ob % Header % ProfileNo, "ProfileNo", Ob % Header % NumObsLocal, Ob % ProfileNo)
    case (VarField_dWinddT)
      call Ops_Alloc(Ob % Header % dWinddT, "dWinddT", Ob % Header % NumObsLocal, Ob % dWinddT)
    case (VarField_dWinddP)
      call Ops_Alloc(Ob % Header % dWinddP, "dWinddP", Ob % Header % NumObsLocal, Ob % dWinddP)
    case (VarField_AzimuthCOG)
      call Ops_Alloc(Ob % Header % AzimuthCOG, "AzimuthCOG", Ob % Header % NumObsLocal, Ob % AzimuthCOG)
    case (VarField_HeightCOG)
      call Ops_Alloc(Ob % Header % HeightCOG, "HeightCOG", Ob % Header % NumObsLocal, Ob % HeightCOG)
    case default
      write (ErrorMessage, '(A,I0)') "VarField code not recognised ", VarFields(iVarField)
      call gen_warn(RoutineName, ErrorMessage)
      cycle
  end select

  if (FillChanNum .or. FillNumChans) then
    call cxvarobs_varobswriter_fillchannumandnumchans( &
      Ob, ObsSpace, Channels, Flags, FillChanNum, FillNumChans)
  end if

end do

end subroutine cxvarobs_varobswriter_populateobservations

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_fillelementtypefromsimulatedvariable( &
  Hdr, OpsVarName, NumObs, El1, JediVarName, ObsSpace, Flags, ObsErrors, HdrIn, initial_value)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(kind=8), intent(in)                     :: NumObs
type(Element_type), pointer                     :: El1(:)
character(len=*), intent(in)                    :: JediVarName
type(c_ptr), value, intent(in)                  :: ObsSpace
type(c_ptr), value, intent(in)                  :: Flags
type(c_ptr), value, intent(in)                  :: ObsErrors
type(ElementHeader_Type), optional, intent(in)  :: HdrIn
type(Element_Type), optional, intent(in)        :: initial_value

! Local declarations:
real(kind=c_double)                             :: ObsValue(NumObs)
integer(kind=c_int)                             :: Flag(NumObs)
real(kind=c_float)                              :: ObsError(NumObs)
real(kind=c_double)                             :: MissingDouble
real(kind=c_float)                              :: MissingFloat
integer                                         :: i
character(len=*), parameter                     :: RoutineName = "cxvarobs_varobswriter_fillelementtypefromsimulatedvariable"
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
    ObsError = RMDI
  end if

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, El1, HdrIn, initial_value)
  do i = 1, NumObs
    if (ObsValue(i) /= MissingDouble) El1(i) % Value = ObsValue(i)
    if (ObsError(i) /= MissingFloat)  El1(i) % OBErr = ObsError(i)
    if (Flag(i) /= 0)                 El1(i) % Flags = ibset(0, FinalRejectFlag)
    ! TODO: PGEFinal
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.
end subroutine cxvarobs_varobswriter_fillelementtypefromsimulatedvariable

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_fillelementtype2dfromsimulatedvariable( &
  Hdr, OpsVarName, NumObs, El2, JediVarName, ObsSpace, Channels, Flags, ObsErrors, &
  HdrIn, initial_value)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(kind=8), intent(in)                     :: NumObs
type(Element_type), pointer                     :: El2(:,:)
character(len=*), intent(in)                    :: JediVarName
type(c_ptr), value, intent(in)                  :: ObsSpace
integer(c_int), intent(in)                      :: Channels(:)
type(c_ptr), value, intent(in)                  :: Flags
type(c_ptr), value, intent(in)                  :: ObsErrors
type(ElementHeader_Type), optional, intent(in)  :: HdrIn
type(Element_Type), optional, intent(in)        :: initial_value

! Local declarations:
real(kind=c_double)                             :: ObsValue(NumObs)
integer(kind=c_int)                             :: Flag(NumObs)
real(kind=c_float)                              :: ObsError(NumObs)
real(kind=c_double)                             :: MissingDouble
real(kind=c_float)                              :: MissingFloat
character(len=max_varname_with_channel_length)  :: JediVarNamesWithChannels(max(size(Channels), 1))

integer                                         :: iChannel, iObs
character(len=*), parameter                     :: &
  RoutineName = "cxvarobs_varobswriter_fillelementtype2dfromsimulatedvariable"
character(len=256)                              :: ErrorMessage

! Body:

! The types of floating-point numbers used in this function are a bit confusing. OPS stores
! observation values as doubles, whereas JEDI stores them as floats. However, the Fortran interface
! to the IODA ObsSpace is only partially implemented: obsspace_get_db_real32 doesn't work, only
! obsspace_get_db_real64 does. So we need to retrieve observation values as doubles. Observation
! errors, though, are retrieved as floats.

MissingDouble = missing_value(0.0_c_double)
MissingFloat  = missing_value(0.0_c_float)

JediVarNamesWithChannels = cxvarobs_varobswriter_varnames_with_channels(JediVarName, Channels)

if (obsspace_has(ObsSpace, "ObsValue", JediVarNamesWithChannels(1))) then
  ! Allocate OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, El2, &
                 HdrIn = HdrIn, &
                 num_levels = int(size(JediVarNamesWithChannels), kind=8), &
                 initial_value = initial_value)

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
      ObsError = RMDI
    end if

    ! Fill the OPS data structures
    do iObs = 1, NumObs
      if (ObsValue(iObs) /= MissingDouble) El2(iObs, iChannel) % Value = ObsValue(iObs)
      if (ObsError(iObs) /= MissingFloat)  El2(iObs, iChannel) % OBErr = ObsError(iObs)
      if (Flag(iObs) /= 0)                 El2(iObs, iChannel) % Flags = ibset(0, FinalRejectFlag)
      ! TODO: Flags, PGEFinal
    end do
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.
end subroutine cxvarobs_varobswriter_fillelementtype2dfromsimulatedvariable

! ------------------------------------------------------------------------------

!> Fill a 1D field of type Element_Type with values taken from an arbitrary JEDI variable
!> (i.e. not a simulated variable) and optionally errors taken from another such variable.

subroutine cxvarobs_varobswriter_fillelementtypefromnormalvariable(Hdr,                   &
                                                                   OpsVarName,            &
                                                                   NumObs,                &
                                                                   El1,                   &
                                                                   ObsSpace,              &
                                                                   JediValueVarName,      &
                                                                   JediValueGroup,        &
                                                                   JediErrorVarName,      &
                                                                   JediErrorGroup,        &
                                                                   HdrIn,                 &
                                                                   initial_value)
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
type(ElementHeader_Type), optional, intent(in)  :: HdrIn
type(Element_Type), optional, intent(in)        :: initial_value

! Local declarations:
real(kind=c_double)                             :: ObsValue(NumObs)
real(kind=c_float)                              :: ObsError(NumObs)
real(kind=c_double)                             :: MissingDouble
real(kind=c_float)                              :: MissingFloat
integer                                         :: i
character(len=*), parameter                     :: &
  RoutineName = "cxvarobs_varobswriter_fillelementtypefromnormalvariable"
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
      ObsError = RMDI
    end if
  else
    ObsError = RMDI
  end if

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, El1, HdrIn, initial_value)
  do i = 1, NumObs
    if (ObsValue(i) /= MissingDouble) El1(i) % Value = ObsValue(i)
    if (ObsError(i) /= MissingDouble)  El1(i) % OBErr = ObsError(i)
    ! TODO(someone): Fill Flags and PGEFinal, if available.
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.
end subroutine cxvarobs_varobswriter_fillelementtypefromnormalvariable

! ------------------------------------------------------------------------------

!> Fill a 1D field of type Element_Type with values taken from an arbitrary JEDI variable
!> (i.e. not a simulated variable) and optionally errors taken from another such variable.

subroutine cxvarobs_varobswriter_fillelementtype2dfromnormalvariable( &
  Hdr, OpsVarName, NumObs, El2, ObsSpace, Channels, &
  JediValueVarName, JediValueGroup, JediErrorVarName, JediErrorGroup, HdrIn, initial_value)
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
type(ElementHeader_Type), optional, intent(in)  :: HdrIn
type(Element_Type), optional, intent(in)        :: initial_value

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
  RoutineName = "cxvarobs_varobswriter_fillelementtype2dfromnormalvariable"
character(len=256)                              :: ErrorMessage

! Body:

if (present(JediErrorVarName) .neqv. present(JediErrorGroup)) then
  write (ErrorMessage, '(A)') &
    "JediErrorVarName and JediErrorGroup must be either both absent or both present"
  call gen_warn(RoutineName, ErrorMessage)
end if

MissingDouble = missing_value(0.0_c_double)

JediValueVarNamesWithChannels = cxvarobs_varobswriter_varnames_with_channels( &
  JediValueVarName, Channels)
if (present(JediErrorVarName)) then
  JediErrorVarNamesWithChannels = cxvarobs_varobswriter_varnames_with_channels( &
    JediErrorVarName, Channels)
end if

if (obsspace_has(ObsSpace, JediValueGroup, JediValueVarNamesWithChannels(1))) then
  ! Allocate OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, El2, &
                 HdrIn = HdrIn, &
                 num_levels = int(size(JediValueVarNamesWithChannels), kind=8), &
                 initial_value = initial_value)

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
        ObsError = RMDI
      end if
    else
      ObsError = RMDI
    end if

    ! Fill the OPS data structures
    do iObs = 1, NumObs
      if (ObsValue(iObs) /= MissingDouble) El2(iObs, iChannel) % Value = ObsValue(iObs)
      if (ObsError(iObs) /= MissingDouble) El2(iObs, iChannel) % OBErr = ObsError(iObs)
      ! TODO: PGEFinal
    end do
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.

end subroutine cxvarobs_varobswriter_fillelementtype2dfromnormalvariable

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_fillreal( &
  Hdr, OpsVarName, NumObs, Real1, JediVarName, JediVarGroup, ObsSpace, HdrIn, initial_value)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(kind=8), intent(in)                     :: NumObs
real(kind=8), pointer                           :: Real1(:)
character(len=*), intent(in)                    :: JediVarName
character(len=*), intent(in)                    :: JediVarGroup
type(c_ptr), value, intent(in)                  :: ObsSpace
type(ElementHeader_Type), optional, intent(in)  :: HdrIn
real(kind=8), optional, intent(in)              :: initial_value

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
  call Ops_Alloc(Hdr, OpsVarName, NumObs, Real1, HdrIn, initial_value)
  do i = 1, NumObs
    if (VarValue(i) /= MissingDouble) Real1(i) = VarValue(i)
  end do
end if
end subroutine cxvarobs_varobswriter_fillreal

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_fillrealfromgeoval( &
  Hdr, OpsVarName, NumObs, Real1, JediVarName, GeoVals, HdrIn, initial_value)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(kind=8), intent(in)                     :: NumObs
real(kind=8), pointer                           :: Real1(:)
character(len=*), intent(in)                    :: JediVarName
type(ufo_geovals), intent(in)                   :: GeoVals
type(ElementHeader_Type), optional, intent(in)  :: HdrIn
real(kind=8), optional, intent(in)              :: initial_value

! Local declarations:
type(ufo_geoval), pointer                       :: GeoVal
real(kind_real)                                 :: MissingReal

character(len=*), parameter                     :: &
  RoutineName = "cxvarobs_varobswriter_fillrealfromgeoval"
character(len=256)                              :: ErrorMessage

! Body:

MissingReal = missing_value(0_kind_real)

if (ufo_vars_getindex(GeoVals % variables, JediVarName) > 0) then
  ! Retrieve GeoVal
  call ufo_geovals_get_var(GeoVals, JediVarName, GeoVal)
  if (GeoVal % nval /= 1) then
    write (ErrorMessage, '("GeoVal ",A," contains more than one value per location. &
      &Only the first of these values will be written to the VarObs file")') JediVarName
    call gen_warn(RoutineName, ErrorMessage)
  end if

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, Real1, HdrIn, initial_value)
  where (GeoVal % vals(1,:) /= MissingReal)
    Real1 = GeoVal % vals(1,:)
  end where
end if
end subroutine cxvarobs_varobswriter_fillrealfromgeoval

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_fillreal2d(Hdr,           &
                                                 OpsVarName,    &
                                                 NumObs,        &
                                                 Real2,         &
                                                 JediVarName,   &
                                                 JediVarGroup,  &
                                                 ObsSpace,      &
                                                 Channels,      &
                                                 HdrIn,         &
                                                 initial_value)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(kind=8), intent(in)                     :: NumObs
real(kind=8), pointer                           :: Real2(:,:)
character(len=*), intent(in)                    :: JediVarName
character(len=*), intent(in)                    :: JediVarGroup
type(c_ptr), value, intent(in)                  :: ObsSpace
integer(c_int), intent(in)                      :: Channels(:)
type(ElementHeader_Type), optional, intent(in)  :: HdrIn
real(kind=8), optional, intent(in)              :: initial_value

! Local declarations:
real(kind=c_double)                             :: VarValue(NumObs)
real(kind=c_double)                             :: MissingDouble
character(len=max_varname_with_channel_length)  :: JediVarNamesWithChannels(max(size(Channels), 1))
integer                                         :: iChannel

! Body:

MissingDouble = missing_value(0.0_c_double)

JediVarNamesWithChannels = cxvarobs_varobswriter_varnames_with_channels(JediVarName, Channels)

if (obsspace_has(ObsSpace, JediVarGroup, JediVarNamesWithChannels(1))) then
  ! Allocate OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, Real2, &
                 HdrIn = HdrIn, &
                 num_levels = int(size(JediVarNamesWithChannels), kind=8), &
                 initial_value = initial_value)

  do iChannel = 1, size(JediVarNamesWithChannels)
    ! Retrieve data from JEDI
    call obsspace_get_db(ObsSpace, JediVarGroup, JediVarNamesWithChannels(iChannel), VarValue)

    ! Fill the OPS data structures
    where (VarValue /= MissingDouble)
      Real2(:, iChannel) = VarValue
    end where
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.
end subroutine cxvarobs_varobswriter_fillreal2d

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_fillreal2dfromgeoval( &
  Hdr, OpsVarName, NumObs, Real2, JediVarName, GeoVals, HdrIn, initial_value)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(kind=8), intent(in)                     :: NumObs
real(kind=8), pointer                           :: Real2(:,:)
character(len=*), intent(in)                    :: JediVarName
type(ufo_geovals), intent(in)                   :: GeoVals
type(ElementHeader_Type), optional, intent(in)  :: HdrIn
real(kind=8), optional, intent(in)              :: initial_value

! Local declarations:
type(ufo_geoval), pointer                       :: GeoVal
real(kind_real)                                 :: MissingReal

! Body:

MissingReal = missing_value(0_kind_real)

if (ufo_vars_getindex(GeoVals % variables, JediVarName) > 0) then
  ! Retrieve GeoVal
  call ufo_geovals_get_var(GeoVals, JediVarName, GeoVal)

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, Real2, HdrIn, initial_value = initial_value, &
                 num_levels = int(GeoVal % nval, kind = 8))
  where (transpose(GeoVal % vals) /= MissingReal)
    Real2 = transpose(GeoVal % vals)
  end where
end if
end subroutine cxvarobs_varobswriter_fillreal2dfromgeoval

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_fillinteger(Hdr,           &
                                             OpsVarName,    &
                                             NumObs,        &
                                             Int1,          &
                                             JediVarName,   &
                                             JediVarGroup,  &
                                             ObsSpace,      &
                                             HdrIn,         &
                                             initial_value)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(kind=8), intent(in)                     :: NumObs
integer(kind=8), pointer                        :: Int1(:)
character(len=*), intent(in)                    :: JediVarName
character(len=*), intent(in)                    :: JediVarGroup
type(c_ptr), value, intent(in)                  :: ObsSpace
type(ElementHeader_Type), optional, intent(in)  :: HdrIn
integer(kind=8), optional, intent(in)           :: initial_value

! Local declarations:
integer(kind=4)                                 :: VarValue(NumObs)
integer(kind=4)                                 :: MissingInt

! Body:

MissingInt = missing_value(0_c_int32_t)

if (obsspace_has(ObsSpace, JediVarGroup, JediVarName)) then
  ! Retrieve data from JEDI
  call obsspace_get_db(ObsSpace, JediVarGroup, JediVarName, VarValue)

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, Int1, HdrIn, initial_value)
  where (VarValue /= MissingInt)
    Int1 = VarValue
  end where
end if
end subroutine cxvarobs_varobswriter_fillinteger

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_fillcoord2d(Hdr, OpsVarName, NumObs, Coord2, &
                                             JediVarName, JediVarGroup, ObsSpace, Channels, &
                                             HdrIn, initial_value)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(kind=8), intent(in)                     :: NumObs
type(coord_type), pointer                       :: Coord2(:,:)
character(len=*), intent(in)                    :: JediVarName
character(len=*), intent(in)                    :: JediVarGroup
type(c_ptr), value, intent(in)                  :: ObsSpace
integer(c_int), intent(in)                      :: Channels(:)
type(ElementHeader_Type), optional, intent(in)  :: HdrIn
type(coord_type), optional, intent(in)          :: initial_value

! Local declarations:
real(kind=c_double)                             :: VarValue(NumObs)
real(kind=c_double)                             :: MissingDouble
character(len=max_varname_with_channel_length)  :: JediVarNamesWithChannels(max(size(Channels), 1))
integer                                         :: iChannel

! Body:

MissingDouble = missing_value(0.0_c_double)

JediVarNamesWithChannels = cxvarobs_varobswriter_varnames_with_channels(JediVarName, Channels)

if (obsspace_has(ObsSpace, JediVarGroup, JediVarNamesWithChannels(1))) then
  ! Allocate OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, Coord2, &
                 HdrIn = HdrIn, &
                 num_levels = int(size(JediVarNamesWithChannels), kind=8), &
                 initial_value = initial_value)

  do iChannel = 1, size(JediVarNamesWithChannels)
    ! Retrieve data from JEDI
    call obsspace_get_db(ObsSpace, JediVarGroup, JediVarNamesWithChannels(iChannel), VarValue)

    ! Fill the OPS data structures
    where (VarValue /= MissingDouble)
      Coord2(:, iChannel) % Value = VarValue
    end where
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.
end subroutine cxvarobs_varobswriter_fillcoord2d

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_fillreportflags(Ob, ObsSpace, Flags)
use oops_variables_mod
implicit none

! Subroutine arguments:
type(OB_type), intent(inout)             :: Ob
type(c_ptr), value, intent(in)           :: ObsSpace, Flags

! Local declarations:
type(oops_variables)                     :: ObsVariables
character(max_varname_length)            :: VarName
integer                                  :: NumObsVariables, iVar
integer(c_int)                           :: VarFlags(Ob % Header % NumObsLocal)

! Body:
call Ops_Alloc(Ob % Header % ReportFlags, "ReportFlags", &
               Ob % Header % NumObsLocal, Ob % ReportFlags)
Ob % ReportFlags = 0

! Toggle on the FinalRejectReport bit in ReportFlags for observations with a non-zero flag
! in at least one variable.
! TODO(wsmigaj): Is this the right thing to do?
ObsVariables = cxvarobs_obsdatavector_int_varnames(Flags)
NumObsVariables = ObsVariables % nvars()
do iVar = 1, NumObsVariables
  VarName = ObsVariables % variable(iVar)
  call cxvarobs_obsdatavector_int_get(Flags, VarName, VarFlags)
  ! TODO(wsmigaj): during development, we leave ReportFlags = 0 for observations with missing data
  ! (as otherwise all observations in some of the IODA test files would be rejected).
  where (VarFlags > 1)
    Ob % ReportFlags = ibset(Ob % ReportFlags, FinalRejectReport)
  end where
end do

end subroutine cxvarobs_varobswriter_fillreportflags

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_fillchannumandnumchans(Ob, ObsSpace, Channels, Flags, &
                                                        FillChanNum, FillNumChans)
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

call cxvarobs_varobswriter_findchannelspassingqc( &
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

end subroutine cxvarobs_varobswriter_fillchannumandnumchans

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_findchannelspassingqc(NumObs, ObsSpace, Channels, Flags, &
                                                       ChannelIndices, ChannelCounts)
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
character(len=*), parameter    :: RoutineName = "cxvarobs_varobswriter_findchannelspassingqc"

! Body:
NumChannels = size(Channels)
if (NumChannels == 0) return

ChannelIndices = IMDI
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

do iMultichannelVariable = 1, NumMultichannelVariables
  do iChannel = 1, NumChannels
    iVariable = (iMultichannelVariable - 1) * NumChannels + iChannel
    VariableName = Variables % variable(iVariable)
    call cxvarobs_obsdatavector_int_get(Flags, VariableName, VarFlags)
    do iObs = 1, NumObs
      if (VarFlags(iObs) == 0) then ! This channel has passed quality control
        ChannelIndices(iObs, 1 + ChannelCounts(iObs)) = Channels(iChannel)
        ChannelCounts(iObs) = ChannelCounts(iObs) + 1
      end if
    end do
  end do
end do

end subroutine cxvarobs_varobswriter_findchannelspassingqc

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_fillcorbritemp(Ob, ObsSpace, Channels)
implicit none
! Subroutine arguments:
type(OB_type), intent(inout)             :: Ob
type(c_ptr), value, intent(in)           :: ObsSpace
integer(c_int), intent(in)               :: Channels(:)

! Local declarations:
type(ElementHeader_type)                 :: CorBriTempBiasHeader
real(kind=8), pointer                    :: CorBriTempBias(:,:)
character(len=*), parameter              :: RoutineName = "cxvarobs_varobswriter_fillcorbritemp"
character(len=256)                       :: ErrorMessage

! Body:

! Retrieve the uncorrected brightness temperature...
call cxvarobs_varobswriter_fillreal2d( &
  Ob % Header % CorBriTemp, "CorBriTemp", Ob % Header % NumObsLocal, Ob % CorBriTemp, &
  "brightness_temperature", "ObsValue", ObsSpace, Channels)

! ... and correct it by subtracting the bias.
if (Ob % Header % CorBriTemp % Present) then
  call cxvarobs_varobswriter_fillreal2d( &
    CorBriTempBiasHeader, "CorBriTemp", Ob % Header % NumObsLocal, CorBriTempBias, &
    "brightness_temperature", "ObsBias", ObsSpace, Channels)
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

end subroutine cxvarobs_varobswriter_fillcorbritemp

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_fillgpsrotpddependentfields(Ob, ObsSpace)
implicit none
! Subroutine arguments:
type(OB_type), intent(inout)             :: Ob
type(c_ptr), value, intent(in)           :: ObsSpace

! Body:

call cxvarobs_varobswriter_fillsatid(Ob, ObsSpace)
! TODO(someone): Replace the placeholder in the call below with an appropriate variable name
! and group, once they're known.
call cxvarobs_varobswriter_fillinteger( &
  Ob % Header % RO_quality, "RO_quality", Ob % Header % NumObsLocal, Ob % RO_quality, &
  "PLACEHOLDER_VARIABLE_NAME", "PLACEHOLDER_GROUP", ObsSpace)
! TODO(someone): Replace "latitude" and "longitude" variable names in the two calls below with
! appropriate GPSRO-specific variable names, once they're known. The group name may need to be
! adjusted as well.
call cxvarobs_varobswriter_fillreal( &
  Ob % Header % ro_occ_lat, "ro_occ_lat", Ob % Header % NumObsLocal, Ob % ro_occ_lat, &
  "latitude", "MetaData", ObsSpace)
call cxvarobs_varobswriter_fillreal( &
  Ob % Header % ro_occ_lon, "ro_occ_lon", Ob % Header % NumObsLocal, Ob % ro_occ_lon, &
  "longitude", "MetaData", ObsSpace)

end subroutine cxvarobs_varobswriter_fillgpsrotpddependentfields

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_fillsatid(Ob, ObsSpace)
implicit none
! Subroutine arguments:
type(OB_type), intent(inout)             :: Ob
type(c_ptr), value, intent(in)           :: ObsSpace

! Body:
call cxvarobs_varobswriter_fillinteger( &
  Ob % Header % SatId, "SatId", Ob % Header % NumObsLocal, Ob % SatId, &
  "satellite_id", "MetaData", ObsSpace)

end subroutine cxvarobs_varobswriter_fillsatid

! ------------------------------------------------------------------------------

!> Return an array containing the names of JEDI variables storing individual channels of the
!> variable VarName. If the list of channels is empty, this means the variable in question is
!> 1D and hence the returned array contains just the single string VarName.
function cxvarobs_varobswriter_varnames_with_channels(VarName, Channels) result(VarNames)
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
end function cxvarobs_varobswriter_varnames_with_channels

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_populatecxheader(self, CxHeader)
implicit none
! Subroutine arguments:
type(cxvarobs_varobswriter), intent(in) :: self
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

CxHeader % IntC(IC_TorTheta) = self % IC_TorTheta
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

end subroutine cxvarobs_varobswriter_populatecxheader

end module cxvarobs_varobswriter_mod
