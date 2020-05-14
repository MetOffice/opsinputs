! (C) Copyright 2020 Met Office UK
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

!> Fortran module to implement varobswriter

module cxvarobs_varobswriter_mod

use fckit_configuration_module, only: fckit_configuration
use, intrinsic :: iso_c_binding
use datetime_mod
use missing_values_mod
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
    gen_warn          
use GenMod_ModelIO, only: LenFixHd, UM_header_type
use GenMod_Setup, only: Gen_SetupControl
use GenMod_UMHeaderConstants

use OpsMod_Control, only:   &
    DefaultDocURL,          &
    Ops_InitMPI
use OpsMod_MiscTypes
use OpsMod_ObsGroupInfo, only: &
    OpsFn_ObsGroupNameToNum,   &
    ObsGroupAircraft,          &
    ObsGroupSurface,           &
    ObsGroupSatwind,           &
    ObsGroupScatwind
use OpsMod_ObsInfo
use OpsMod_Varfields
use OpsMod_Varobs

use OpsMod_SatRad_RTmodel, only: &
    nlevels_strat_varobs

implicit none
public :: cxvarobs_varobswriter_create, cxvarobs_varobswriter_delete, &
          cxvarobs_varobswriter_prior, cxvarobs_varobswriter_post
private
integer, parameter :: max_string=800

! ------------------------------------------------------------------------------
!> TODO: fill in this type
type, public :: cxvarobs_varobswriter
private
  character(len=max_string), public, allocatable :: geovars(:)
  integer(kind=8) :: obsgroup
  type(datetime)  :: validitytime  ! Corresponds to OPS validity time
end type cxvarobs_varobswriter

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_create(self, f_conf)
implicit none
type(cxvarobs_varobswriter), intent(inout)      :: self
type(fckit_configuration), intent(in) :: f_conf

call Gen_SetupControl(DefaultDocURL)
call Ops_InitMPI

GeneralMode = DebugMode

self % obsgroup = cxvarobs_varobswriter_getobsgroup(self, f_conf)
self % validitytime = cxvarobs_varobswriter_getvaliditytime(self, f_conf)
! TODO: set self%geovars (list of variables to use from GeoVaLs) if needed

end subroutine cxvarobs_varobswriter_create

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_delete(self)
implicit none
type(cxvarobs_varobswriter), intent(inout) :: self

call datetime_delete(self % validitytime)
if (allocated(self%geovars))   deallocate(self%geovars)

end subroutine cxvarobs_varobswriter_delete

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_prior(self, ObsSpace, GeoVals)
implicit none
type(cxvarobs_varobswriter),  intent(in) :: self
type(c_ptr), value, intent(in) :: ObsSpace
type(ufo_geovals),  intent(in) :: GeoVals

end subroutine cxvarobs_varobswriter_prior

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_post(self, ObsSpace, Flags, ObsErrors, nvars, nlocs, hofx)
implicit none
type(cxvarobs_varobswriter),  intent(in) :: self
type(c_ptr), value, intent(in) :: ObsSpace, Flags, ObsErrors
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

call Ops_ReadVarobsControlNL (self % obsgroup, VarFields) ! TODO(wsmigaj): move to separate function?
call cxvarobs_varobswriter_populateobservations(self, VarFields, ObsSpace, Flags, ObsErrors, obs)

CxHeader % FixHd(FH_IntCStart) = LenFixHd + 1
CxHeader % FixHd(FH_IntCSize) = 49
CxHeader % FixHd(FH_RealCStart) = CxHeader % FixHd(FH_IntCStart) + CxHeader % FixHd(FH_IntCSize)
CxHeader % FixHd(FH_RealCSize) = 34
call CxHeader % alloc

call Ops_CreateVarobs (Obs,                 & ! in
                       CxHeader,            & ! in
                       AssimDataFormat_VAR, &
                       NumVarobsTotal)        ! TODO: PGEBd

call obs % deallocate()
! DEALLOCATE(Obs % Header % ObsPerBatchPerPE)

end subroutine cxvarobs_varobswriter_post

! ------------------------------------------------------------------------------

integer function cxvarobs_varobswriter_getobsgroup(self, f_conf)
implicit none
type(cxvarobs_varobswriter), intent(in) :: self
type(fckit_configuration), intent(in) :: f_conf

character(len=:),allocatable :: obsgroupname

call f_conf % get_or_die("obs_group", obsgroupname)
cxvarobs_varobswriter_getobsgroup = OpsFn_ObsGroupNameToNum(obsgroupname)
end function cxvarobs_varobswriter_getobsgroup

! ------------------------------------------------------------------------------

type(datetime) function cxvarobs_varobswriter_getvaliditytime(self, f_conf)
implicit none
type(cxvarobs_varobswriter), intent(in) :: self
type(fckit_configuration), intent(in) :: f_conf

character(len=:),allocatable :: validitytimestr

call f_conf % get_or_die("validity_time", validitytimestr)
call datetime_create(validitytimestr, cxvarobs_varobswriter_getvaliditytime)
end function cxvarobs_varobswriter_getvaliditytime

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_populateobservations(self, VarFields, ObsSpace, &
                                                      Flags, ObsErrors, Ob)
implicit none
type(cxvarobs_varobswriter),  intent(in) :: self
integer(kind=8), intent(in)              :: VarFields(:)
type(c_ptr), value, intent(in)           :: ObsSpace, Flags, ObsErrors
type(OB_type), intent(inout)             :: Ob

character(len=*), parameter              :: RoutineName = "cxvarobs_varobswriter_populateobservations"
character(len=80)                        :: ErrorMessage

integer                                  :: nVarFields
integer                                  :: iVarField

integer(c_int64_t)                       :: TimeOffsetsInSeconds(Ob % Header % NumObsLocal)

integer                                  :: NumLevs
integer                                  :: Zcode
logical                                  :: UseLevelSubset

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

call Ops_Alloc(Ob % Header % ObsType, "ObsType", Ob % Header % NumObsLocal, Ob % ObsType)

if (obsspace_has(ObsSpace, "MetaData", "station_id")) then
  call Ops_Alloc(Ob % Header % Callsign, "Callsign", Ob % Header % NumObsLocal, Ob % Callsign)
  call cxvarobs_obsspace_get_db_string( &
    ObsSpace, "MetaData", "station_id", int(LenCallSign, kind=4), Ob % Callsign)
end if

! TODO: Num levels
select case (Ob % Header % ObsGroup)
case (ObsGroupAircraft, ObsGroupSatwind)
  call Ops_Alloc(Ob % Header % PlevelsA, "PlevelsA", Ob % Header % NumObsLocal, Ob % PlevelsA)
end select

do iVarField = 1, nVarFields
  NumLevs = 1         ! TODO: Extend to multilevel quantities
  Zcode = ZcodeUnused ! TODO: fill in correctly

  UseLevelSubset = .false.
  select case (VarFields(iVarField))
    case (imdi)
      cycle
    case (VarField_pstar)
      call cxvarobs_varobswriter_fillobsvalueanderror_1d( &
        Ob % Header % pstar, "pstar", Ob % Header % NumObsLocal, Ob % pstar, &
        "surface_pressure", ObsSpace, ObsErrors)
    case (VarField_theta)
      call Ops_Alloc(Ob % Header % theta, "theta", Ob % Header % NumObsLocal, Ob % theta)
    case (VarField_temperature)
      if (Ob % Header % ObsGroup == ObsGroupSurface) then
        call cxvarobs_varobswriter_fillobsvalueanderror_1d( &
          Ob % Header % t2, "t2", Ob % Header % NumObsLocal, Ob % t2, &
          "air_temperature", ObsSpace, ObsErrors)
      else
        call Ops_Alloc(Ob % Header % t, "t", Ob % Header % NumObsLocal, Ob % t)
      end if
    case (VarField_rh)
      if (Ob % Header % ObsGroup == ObsGroupSurface) then
        call cxvarobs_varobswriter_fillobsvalueanderror_1d( &
          Ob % Header % rh2, "rh2", Ob % Header % NumObsLocal, Ob % rh2, &
          "relative_humidity", ObsSpace, ObsErrors)
      else
        call Ops_Alloc(Ob % Header % rh, "rh", Ob % Header % NumObsLocal, Ob % rh)
      end if
    case (VarField_u)
      if (Ob % Header % ObsGroup == ObsGroupSurface .or. &
          Ob % Header % ObsGroup == ObsGroupScatwind) then
        call cxvarobs_varobswriter_fillobsvalueanderror_1d( &
          Ob % Header % u10, "u10", Ob % Header % NumObsLocal, Ob % u10, &
          "eastward_wind", ObsSpace, ObsErrors)
      else
        call Ops_Alloc(Ob % Header % u, "u", Ob % Header % NumObsLocal, Ob % u)
      end if
    case (VarField_v)
      if (Ob % Header % ObsGroup == ObsGroupSurface .or. &
          Ob % Header % ObsGroup == ObsGroupScatwind) then
        call cxvarobs_varobswriter_fillobsvalueanderror_1d( &
          Ob % Header % v10, "v10", Ob % Header % NumObsLocal, Ob % v10, &
          "northward_wind", ObsSpace, ObsErrors)
      else
        call Ops_Alloc(Ob % Header % v, "v", Ob % Header % NumObsLocal, Ob % v)
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
      call Ops_Alloc(Ob % Header % CorBriTemp, "CorBriTemp", Ob % Header % NumObsLocal, Ob % CorBriTemp)
    case (VarField_tskin)
      call Ops_Alloc(Ob % Header % Tskin, "Tskin", Ob % Header % NumObsLocal, Ob % Tskin)
    case (VarField_gpstzdelay)
      call Ops_Alloc(Ob % Header % GPSTZDelay, "GPSTZDelay", Ob % Header % NumObsLocal, Ob % GPSTZDelay)
    case (VarField_GPS_Station_Height)
      call Ops_Alloc(Ob % Header % Zstation, "Zstation", Ob % Header % NumObsLocal, Ob % Zstation)
    case (VarField_mwemiss)
      call Ops_Alloc(Ob % Header % MwEmiss, "MwEmiss", Ob % Header % NumObsLocal, Ob % MwEmiss)
    case (VarField_TCozone)
      call Ops_Alloc(Ob % Header % TCozone, "TCozone", Ob % Header % NumObsLocal, Ob % TCozone)
    case (VarField_satzenith)
      call cxvarobs_varobswriter_fillvariable_1d( &
        Ob % Header % SatZenithAngle, "SatZenithAngle", Ob % Header % NumObsLocal, Ob % SatZenithAngle, &
        "sensor_zenith_angle", "MetaData", ObsSpace)
    case (VarField_scanpos)
      call Ops_Alloc(Ob % Header % ScanPosition, "ScanPosition", Ob % Header % NumObsLocal, Ob % ScanPosition)
    case (VarField_surface)
      call Ops_Alloc(Ob % Header % surface, "surface", Ob % Header % NumObsLocal, Ob % surface)
    case (VarField_elevation)
      call Ops_Alloc(Ob % Header % elevation, "elevation", Ob % Header % NumObsLocal, Ob % elevation)
    case (VarField_modelsurface)
      call Ops_Alloc(Ob % Header % ModelSurface, "ModelSurface", Ob % Header % NumObsLocal, Ob % ModelSurface)
    case (VarField_modelorog)
      call Ops_Alloc(Ob % Header % ModelOrog, "ModelOrog", Ob % Header % NumObsLocal, Ob % ModelOrog)
    case (VarField_stratt)
      UseLevelSubset = .true.
      NumLevs = nlevels_strat_varobs
      call Ops_Alloc(Ob % Header % t, "t", Ob % Header % NumObsLocal, Ob % t)
    case (VarField_satid)
      call Ops_Alloc(Ob % Header % SatId, "SatId", Ob % Header % NumObsLocal, Ob % SatId)
    case (VarField_satazimth)
      call Ops_Alloc(Ob % Header % SatAzimth, "SatAzimth", Ob % Header % NumObsLocal, Ob % SatAzimth)
    case (VarField_localazimuth)
      call Ops_Alloc(Ob % Header % LocalAzimuth, "LocalAzimuth", Ob % Header % NumObsLocal, Ob % LocalAzimuth)
    case (VarField_solzenith)
      call cxvarobs_varobswriter_fillvariable_1d( &
        Ob % Header % SolarZenith, "SolarZenith", Ob % Header % NumObsLocal, Ob % SolarZenith, &
        "solar_zenith_angle", "MetaData", ObsSpace)
    case (VarField_solazimth)
      call Ops_Alloc(Ob % Header % SolarAzimth, "SolarAzimth", Ob % Header % NumObsLocal, Ob % SolarAzimth)
    case (VarField_iremiss)
      call Ops_Alloc(Ob % Header % IREmiss, "IREmiss", Ob % Header % NumObsLocal, Ob % IREmiss)
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
      call Ops_Alloc(Ob % Header % NumChans, "NumChans", Ob % Header % NumObsLocal, Ob % NumChans)
    case (VarField_ChanNum)
      call Ops_Alloc(Ob % Header % ChanNum, "ChanNum", Ob % Header % NumObsLocal, Ob % ChanNum)
    case (VarField_Emissivity)
      call Ops_Alloc(Ob % Header % Emissivity, "Emissivity", Ob % Header % NumObsLocal, Ob % Emissivity)
    case (VarField_QCinfo)
      call Ops_Alloc(Ob % Header % QCinfo, "QCinfo", Ob % Header % NumObsLocal, Ob % QCinfo)
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
      ! IF (GPSRO_TPD) THEN
      !   CALL Ops_Alloc(Ob % Header % BendingAngleAll, "BendingAngleAll", Ob % Header % NumObsLocal, Ob % BendingAngleAll)
      ! ELSE
      !   CALL Ops_Alloc(Ob % Header % BendingAngle, "BendingAngle", Ob % Header % NumObsLocal, Ob % BendingAngle)
      ! END IF
    case (VarField_ImpactParam)
      ! IF (GPSRO_TPD) THEN
      !   CALL Ops_Alloc(Ob % Header % ImpactParamAll, "ImpactParamAll", Ob % Header % NumObsLocal, Ob % ImpactParamAll)
      ! ELSE
      !   CALL Ops_Alloc(Ob % Header % ImpactParam, "ImpactParam", Ob % Header % NumObsLocal, Ob % ImpactParam)
      ! END IF
    case (VarField_RO_Rad_Curv)
      call Ops_Alloc(Ob % Header % RO_Rad_Curv, "RO_Rad_Curv", Ob % Header % NumObsLocal, Ob % RO_Rad_Curv)
    case (VarField_RO_geoid_und)
      call Ops_Alloc(Ob % Header % RO_geoid_und, "RO_geoid_und", Ob % Header % NumObsLocal, Ob % RO_geoid_und)
    case (VarField_AOD)
      call Ops_Alloc(Ob % Header % aod, "AOD", Ob % Header % NumObsLocal, Ob % aod)
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
      ! IF (PRESENT (RepObs)) THEN
      !   ObHdrVrbl = RepObs % Header % model_level_lat
      ! ELSE
      !   CALL Ops_Alloc(Ob % Header % level_lat, "level_lat", Ob % Header % NumObsLocal, Ob % level_lat)
      ! END IF
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
end do

end subroutine cxvarobs_varobswriter_populateobservations

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_fillobsvalueanderror_1d(Hdr,           &
                                                         OpsVarName,    &
                                                         NumObs,        &
                                                         El1,           &
                                                         JediVarName,   &
                                                         ObsSpace,      &
                                                         ObsErrors,     &
                                                         HdrIn,         &
                                                         initial_value)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(kind=8), intent(in)                     :: NumObs
type(Element_type), pointer                     :: El1(:)
character(len=*), intent(in)                    :: JediVarName
type(c_ptr), value, intent(in)                  :: ObsSpace
type(c_ptr), value, intent(in)                  :: ObsErrors
type(ElementHeader_Type), optional, intent(in)  :: HdrIn
type(Element_Type), optional, intent(in)        :: initial_value

! Local declarations:
real(kind=c_double)                             :: ObsValue(NumObs)
real(kind=c_float)                              :: ObsError(NumObs)
real(kind=c_double)                             :: MissingDouble
real(kind=c_float)                              :: MissingFloat
integer                                         :: i
character(len=*), parameter                     :: RoutineName = "cxvarobs_varobswriter_fillobsvalueanderror_1d"
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
  ! Retrieve data from JEDI
  call obsspace_get_db(ObsSpace, "ObsValue", JediVarName, ObsValue)
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
    ! TODO: Flags, PGEFinal
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.
end subroutine cxvarobs_varobswriter_fillobsvalueanderror_1d

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_fillvariable_1d(Hdr,           &
                                                 OpsVarName,    &
                                                 NumObs,        &
                                                 Real1,         &
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

! The types of floating-point numbers used in this function are a bit confusing. OPS stores
! observation values as doubles, whereas JEDI stores them as floats. However, the Fortran interface
! to the IODA ObsSpace is only partially implemented: obsspace_get_db_real32 doesn't work, only
! obsspace_get_db_real64 does. So we need to retrieve observation values as doubles.

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
end subroutine cxvarobs_varobswriter_fillvariable_1d

subroutine cxvarobs_varobswriter_fillreportflags(Ob, ObsSpace, Flags)
use oops_variables_mod
implicit none

! Subroutine arguments:
type(OB_type), intent(inout)             :: Ob
type(c_ptr), value, intent(in)           :: ObsSpace, Flags

! Local declarations:
type(oops_variables)                     :: ObsVariables
character(MAXVARLEN)                     :: VarName
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
  where (VarFlags /= 0)
    Ob % ReportFlags = ibset(Ob % ReportFlags, FinalRejectReport)
  end where
end do

end subroutine cxvarobs_varobswriter_fillreportflags


end module cxvarobs_varobswriter_mod
