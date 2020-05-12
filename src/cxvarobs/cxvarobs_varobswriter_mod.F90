! (C) Copyright 2020 Met Office UK
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

!> Fortran module to implement varobswriter

module cxvarobs_varobswriter_mod

use fckit_configuration_module, only: fckit_configuration
use, intrinsic :: iso_c_binding
use kinds
use missing_values_mod
use obsspace_mod
use ufo_geovals_mod
use ufo_vars_mod
use cxvarobs_obsdatavector_mod

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
public :: cxvarobs_varobswriter_create, cxvarobs_varobswriter_delete, cxvarobs_varobswriter_prior, cxvarobs_varobswriter_post
private
integer, parameter :: max_string=800

! ------------------------------------------------------------------------------
!> TODO: fill in this type
type, public :: cxvarobs_varobswriter
private
  character(len=max_string), public, allocatable :: geovars(:)
  integer(kind=8) :: obsgroup
end type cxvarobs_varobswriter

! #include "obsdatavector_interface.f90"

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

! TODO: set self%geovars (list of variables to use from GeoVaLs) if needed

end subroutine cxvarobs_varobswriter_create

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_delete(self)
implicit none
type(cxvarobs_varobswriter), intent(inout) :: self

if (allocated(self%geovars))   deallocate(self%geovars)

end subroutine cxvarobs_varobswriter_delete

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_prior(self, obspace, geovals)
implicit none
type(cxvarobs_varobswriter),  intent(in) :: self
type(c_ptr), value, intent(in) :: obspace
type(ufo_geovals),  intent(in) :: geovals

end subroutine cxvarobs_varobswriter_prior

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_post(self, obspace, ObsErrors, nvars, nlocs, hofx)
implicit none
type(cxvarobs_varobswriter),  intent(in) :: self
type(c_ptr), value, intent(in) :: obspace
type(c_ptr), value, intent(in) :: ObsErrors
integer,            intent(in) :: nvars, nlocs
real(c_double),     intent(in) :: hofx(nvars, nlocs)

type(OB_type)                  :: obs
type(UM_header_type)           :: CxHeader
integer(kind=8)                :: varfields(ActualMaxVarfield)
integer(kind=8)                :: NumVarObsTotal

obs % header % obsgroup = self % obsgroup

print *, 'Calling Ops_SetupObType in post'
call Ops_SetupObType(obs)
print *, 'Called Ops_SetupObType'

obs % header % numobstotal = obsspace_get_gnlocs(obspace)
obs % header % numobslocal = obsspace_get_nlocs(obspace)

print *, 'obsspace_get_gnlocs: ', obs % header % numobstotal 
print *, 'obsspace_get_gnlocs: ', obsspace_get_gnlocs(obspace) 
print *, 'obsspace_get_nlocs: ', obs % header % numobslocal 

Obs % Header % NumCXBatches = 1
allocate(Obs % Header % ObsPerBatchPerPE(Obs % Header % NumCXBatches, 0:nproc - 1))
Obs % Header % ObsPerBatchPerPE(1,mype) = obs % header % numobslocal

call Ops_ReadVarobsControlNL (self % obsgroup, varfields) ! TODO(wsmigaj): move to separate function?
call cxvarobs_varobswriter_populateobservations(self, varfields, obspace, ObsErrors, obs)

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
  type(cxvarobs_varobswriter),  intent(in) :: self
  type(fckit_configuration), intent(in) :: f_conf

  character(len=:),allocatable :: obsgroupname

  call f_conf % get_or_die("obs_group", obsgroupname)
  cxvarobs_varobswriter_getobsgroup = OpsFn_ObsGroupNameToNum(obsgroupname)
end function cxvarobs_varobswriter_getobsgroup

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_populateobservations(self, varfields, obspace, ObsErrors, Ob)
implicit none
type(cxvarobs_varobswriter),  intent(in) :: self
integer(kind=8), intent(in)              :: varfields(:)
type(c_ptr), value, intent(in)           :: obspace
type(c_ptr), value, intent(in)           :: ObsErrors
type(OB_type), intent(inout)             :: Ob

character(len=*), parameter          :: RoutineName = "cxvarobs_varobswriter_populateobservations"
character(len=80)                    :: ErrorMessage

integer :: nvarfields
integer :: Ivar

integer                              :: NumLevs
integer                              :: Zcode
logical                              :: UseLevelSubset

nvarfields = size(varfields)

call Ops_Alloc(Ob % header % Latitude, "Latitude", Ob % Header % NumObsLocal, Ob % Latitude)
call Ops_Alloc(Ob % header % Longitude, "Longitude", Ob % Header % NumObsLocal, Ob % Longitude)
call Ops_Alloc(Ob % header % Time, "Time", Ob % Header % NumObsLocal, Ob % Time)
call Ops_Alloc(Ob % header % ReportFlags, "ReportFlags", Ob % Header % NumObsLocal, Ob % ReportFlags)
call Ops_Alloc(Ob % header % ObsType, "ObsType", Ob % Header % NumObsLocal, Ob % ObsType)
call obsspace_get_db(obspace, "MetaData", "latitude", Ob % Latitude)
call obsspace_get_db(obspace, "MetaData", "longitude", Ob % Longitude)

! TODO: Num levels
select case (Ob % header % ObsGroup)
case (ObsGroupAircraft, ObsGroupSatwind)
  call Ops_Alloc(Ob % header % PlevelsA, "PlevelsA", Ob % Header % NumObsLocal, Ob % PlevelsA)
end select

do Ivar = 1, nvarfields
  NumLevs = 1         ! TODO: Extend to multilevel quantities
  Zcode = ZcodeUnused ! TODO: fill in correctly

  UseLevelSubset = .false.
  select case (varfields(Ivar))
    case (imdi)
      cycle
    case (VarField_pstar)
      call cxvarobs_varobswriter_fillobsvalueanderror_1d( &
        Ob % header % pstar, "pstar", Ob % Header % NumObsLocal, Ob % pstar, "surface_pressure", &
        obspace, ObsErrors)
    case (VarField_theta)
      call Ops_Alloc(Ob % header % theta, "theta", Ob % Header % NumObsLocal, Ob % theta)
    case (VarField_temperature)
      if (Ob % header % ObsGroup == ObsGroupSurface) then
        call cxvarobs_varobswriter_fillobsvalueanderror_1d( &
          Ob % header % t2, "t2", Ob % Header % NumObsLocal, Ob % t2, "air_temperature", &
          obspace, ObsErrors)
      else
        call Ops_Alloc(Ob % header % t, "t", Ob % Header % NumObsLocal, Ob % t)
!        CALL cxvarobs_varobswriter_fillobsvalueanderror_2d( &
!          Ob % header % t, "t", Ob % Header % NumObsLocal, Ob % t, "air_temperature", obspace)
      end if
    case (VarField_rh)
      if (Ob % header % ObsGroup == ObsGroupSurface) then
        call cxvarobs_varobswriter_fillobsvalueanderror_1d( &
          Ob % header % rh2, "rh2", Ob % Header % NumObsLocal, Ob % rh2, "air_temperature", &
          obspace, ObsErrors)
      else
        call Ops_Alloc(Ob % header % rh, "rh", Ob % Header % NumObsLocal, Ob % rh)
      end if
    case (VarField_u)
      if (Ob % header % ObsGroup == ObsGroupSurface .or. &
          Ob % header % ObsGroup == ObsGroupScatwind) then
        call cxvarobs_varobswriter_fillobsvalueanderror_1d( &
          Ob % header % u10, "u10", Ob % Header % NumObsLocal, Ob % u10, "eastward_wind", &
          obspace, ObsErrors)
      else
        call Ops_Alloc(Ob % header % u, "u", Ob % Header % NumObsLocal, Ob % u)
      end if
    case (VarField_v)
      if (Ob % header % ObsGroup == ObsGroupSurface .or. &
          Ob % header % ObsGroup == ObsGroupScatwind) then
        call cxvarobs_varobswriter_fillobsvalueanderror_1d( &
          Ob % header % v10, "v10", Ob % Header % NumObsLocal, Ob % v10, "northward_wind", &
          obspace, ObsErrors)
      else
        call Ops_Alloc(Ob % header % v, "v", Ob % Header % NumObsLocal, Ob % v)
      end if
    case (VarField_logvis)
      call Ops_Alloc(Ob % header % logvis, "logvis", Ob % Header % NumObsLocal, Ob % logvis)
    case (VarField_tcwv)
      call Ops_Alloc(Ob % header % tcwv, "TCWV", Ob % Header % NumObsLocal, Ob % tcwv)
    case (VarField_windspeed)
      call Ops_Alloc(Ob % header % WindSpeed, "WindSpeed", Ob % Header % NumObsLocal, Ob % WindSpeed)
    case (VarField_lwp)
      call Ops_Alloc(Ob % header % lwp, "LWP", Ob % Header % NumObsLocal, Ob % lwp)
    case (VarField_britemp)
      call Ops_Alloc(Ob % header % CorBriTemp, "CorBriTemp", Ob % Header % NumObsLocal, Ob % CorBriTemp)
    case (VarField_tskin)
      call Ops_Alloc(Ob % header % Tskin, "Tskin", Ob % Header % NumObsLocal, Ob % Tskin)
    case (VarField_gpstzdelay)
      call Ops_Alloc(Ob % header % GPSTZDelay, "GPSTZDelay", Ob % Header % NumObsLocal, Ob % GPSTZDelay)
    case (VarField_GPS_Station_Height)
      call Ops_Alloc(Ob % header % Zstation, "Zstation", Ob % Header % NumObsLocal, Ob % Zstation)
    case (VarField_mwemiss)
      call Ops_Alloc(Ob % header % MwEmiss, "MwEmiss", Ob % Header % NumObsLocal, Ob % MwEmiss)
    case (VarField_TCozone)
      call Ops_Alloc(Ob % header % TCozone, "TCozone", Ob % Header % NumObsLocal, Ob % TCozone)
    case (VarField_satzenith)
      call Ops_Alloc(Ob % header % SatZenithAngle, "SatZenithAngle", Ob % Header % NumObsLocal, Ob % SatZenithAngle)
    case (VarField_scanpos)
      call Ops_Alloc(Ob % header % ScanPosition, "ScanPosition", Ob % Header % NumObsLocal, Ob % ScanPosition)
    case (VarField_surface)
      call Ops_Alloc(Ob % header % surface, "surface", Ob % Header % NumObsLocal, Ob % surface)
    case (VarField_elevation)
      call Ops_Alloc(Ob % header % elevation, "elevation", Ob % Header % NumObsLocal, Ob % elevation)
    case (VarField_modelsurface)
      call Ops_Alloc(Ob % header % ModelSurface, "ModelSurface", Ob % Header % NumObsLocal, Ob % ModelSurface)
    case (VarField_modelorog)
      call Ops_Alloc(Ob % header % ModelOrog, "ModelOrog", Ob % Header % NumObsLocal, Ob % ModelOrog)
    case (VarField_stratt)
      UseLevelSubset = .true.
      NumLevs = nlevels_strat_varobs
      call Ops_Alloc(Ob % header % t, "t", Ob % Header % NumObsLocal, Ob % t)
    case (VarField_satid)
      call Ops_Alloc(Ob % header % SatId, "SatId", Ob % Header % NumObsLocal, Ob % SatId)
    case (VarField_satazimth)
      call Ops_Alloc(Ob % header % SatAzimth, "SatAzimth", Ob % Header % NumObsLocal, Ob % SatAzimth)
    case (VarField_localazimuth)
      call Ops_Alloc(Ob % header % LocalAzimuth, "LocalAzimuth", Ob % Header % NumObsLocal, Ob % LocalAzimuth)
    case (VarField_solzenith)
      call Ops_Alloc(Ob % header % SolarZenith, "SolarZenith", Ob % Header % NumObsLocal, Ob % SolarZenith)
    case (VarField_solazimth)
      call Ops_Alloc(Ob % header % SolarAzimth, "SolarAzimth", Ob % Header % NumObsLocal, Ob % SolarAzimth)
    case (VarField_iremiss)
      call Ops_Alloc(Ob % header % IREmiss, "IREmiss", Ob % Header % NumObsLocal, Ob % IREmiss)
    case (VarField_cloudtopp)
      call Ops_Alloc(Ob % header % CloudTopP, "CloudTopP", Ob % Header % NumObsLocal, Ob % CloudTopP)
    case (VarField_cloudfrac)
      call Ops_Alloc(Ob % header % CloudFrac, "CloudFrac", Ob % Header % NumObsLocal, Ob % CloudFrac)
    case (VarField_vnatovpp)
      call Ops_Alloc(Ob % header % VnATOVPP, "VnATOVPP", Ob % Header % NumObsLocal, Ob % VnATOVPP)
    case (VarField_procoption)
      call Ops_Alloc(Ob % header % ATOVSProcOption, "ATOVSProcOption", Ob % Header % NumObsLocal, Ob % ATOVSProcOption)
    case (VarField_amsusurface)
      call Ops_Alloc(Ob % header % AMSUsurface, "AMSUsurface", Ob % Header % NumObsLocal, Ob % AMSUsurface)
    case (VarField_hirs_temp)
      call Ops_Alloc(Ob % header % HIRS_Temp, "HIRS_Temp", Ob % Header % NumObsLocal, Ob % HIRS_Temp)
    case (VarField_amsua1_temp)
      call Ops_Alloc(Ob % header % AMSUa1_Temp, "AMSUa1_Temp", Ob % Header % NumObsLocal, Ob % AMSUa1_Temp)
    case (VarField_amsua2_temp)
      call Ops_Alloc(Ob % header % AMSUa2_Temp, "AMSUa2_Temp", Ob % Header % NumObsLocal, Ob % AMSUa2_Temp)
    case (VarField_amsub_temp)
      call Ops_Alloc(Ob % header % AMSUb_Temp, "AMSUb_Temp", Ob % Header % NumObsLocal, Ob % AMSUb_Temp)
    case (VarField_cloud)
      call Ops_Alloc(Ob % header % Cloud, "Cloud", Ob % Header % NumObsLocal, Ob % Cloud)
    case (VarField_rainrate)
      call Ops_Alloc(Ob % header % Rainrate, "Rainrate", Ob % Header % NumObsLocal, Ob % Rainrate)
    case (VarField_snowrate)
      call Ops_Alloc(Ob % header % Snowrate, "Snowrate", Ob % Header % NumObsLocal, Ob % Snowrate)
    case (VarField_u10ambwind)
      call Ops_Alloc(Ob % header % u10AmbWind, "u10AmbWind", Ob % Header % NumObsLocal, Ob % u10AmbWind)
    case (VarField_v10ambwind)
      call Ops_Alloc(Ob % header % v10AmbWind, "v10AmbWind", Ob % Header % NumObsLocal, Ob % v10AmbWind)
    case (VarField_pcorrect)
      call Ops_Alloc(Ob % header % AWPriorPCorrect, "AWPriorPCorrect", Ob % Header % NumObsLocal, Ob % AWPriorPCorrect)
    case (VarField_NumChans)
      call Ops_Alloc(Ob % header % NumChans, "NumChans", Ob % Header % NumObsLocal, Ob % NumChans)
    case (VarField_ChanNum)
      call Ops_Alloc(Ob % header % ChanNum, "ChanNum", Ob % Header % NumObsLocal, Ob % ChanNum)
    case (VarField_Emissivity)
      call Ops_Alloc(Ob % header % Emissivity, "Emissivity", Ob % Header % NumObsLocal, Ob % Emissivity)
    case (VarField_QCinfo)
      call Ops_Alloc(Ob % header % QCinfo, "QCinfo", Ob % Header % NumObsLocal, Ob % QCinfo)
    case (VarField_SBUVozone)
      call Ops_Alloc(Ob % header % SBUVozone, "SBUVozone", Ob % Header % NumObsLocal, Ob % SBUVozone)
    case (VarField_RadialVelocity)
        call Ops_Alloc(Ob % header % RadialVelocSO, "RadialVelocSO", Ob % Header % NumObsLocal, Ob % RadialVelocSO)
    case (VarField_Reflectivity)
      call Ops_Alloc(Ob % header % ReflectivitySO, "ReflectivitySO", Ob % Header % NumObsLocal, Ob % ReflectivitySO)
    case (VarField_ReflectivityR)
      call Ops_Alloc(Ob % header % ReflectivityR, "ReflectivityR", Ob % Header % NumObsLocal, Ob % ReflectivityR)
    case (VarField_ReflectivityI)
      call Ops_Alloc(Ob % header % ReflectivityI, "ReflectivityI", Ob % Header % NumObsLocal, Ob % ReflectivityI)
    case (VarField_RadarBeamElev)
      call Ops_Alloc(Ob % header % RadarBeamElev, "RadarBeamElev", Ob % Header % NumObsLocal, Ob % RadarBeamElev)
    case (VarField_RadarObRange)
      call Ops_Alloc(Ob % header % RadarObRange, "RadarObRange", Ob % Header % NumObsLocal, Ob % RadarObRange)
    case (VarField_RadarObAzim)
      call Ops_Alloc(Ob % header % RadarObAzim, "RadarObAzim", Ob % Header % NumObsLocal, Ob % RadarObAzim)
    case (VarField_RadIdent)
      call Ops_Alloc(Ob % header % RadIdent, "RadIdent", Ob % Header % NumObsLocal, Ob % RadIdent)
    case (VarField_RadAltAboveMSL)
      call Ops_Alloc(Ob % header % RadAltAboveMSL, "RadAltAboveMSL", Ob % Header % NumObsLocal, Ob % RadAltAboveMSL)
    case (VarField_RadNoiseLvl)
      call Ops_Alloc(Ob % header % RadNoiseLvl, "RadNoiseLvl", Ob % Header % NumObsLocal, Ob % RadNoiseLvl)
    case (VarField_RadFlag)
      call Ops_Alloc(Ob % header % RadFlag, "RadFlag", Ob % Header % NumObsLocal, Ob % RadFlag)
    case (VarField_clw)
      call Ops_Alloc(Ob % header % clw, "clw", Ob % Header % NumObsLocal, Ob % clw)
    case (VarField_refrac)
      call Ops_Alloc(Ob % header % refrac, "refrac", Ob % Header % NumObsLocal, Ob % refrac)
    case (VarField_z)
      call Ops_Alloc(Ob % header % z, "z", Ob % Header % NumObsLocal, Ob % z)
    case (VarField_BendingAngle)
      ! IF (GPSRO_TPD) THEN
      !   CALL Ops_Alloc(Ob % header % BendingAngleAll, "BendingAngleAll", Ob % Header % NumObsLocal, Ob % BendingAngleAll)
      ! ELSE
      !   CALL Ops_Alloc(Ob % header % BendingAngle, "BendingAngle", Ob % Header % NumObsLocal, Ob % BendingAngle)
      ! END IF
    case (VarField_ImpactParam)
      ! IF (GPSRO_TPD) THEN
      !   CALL Ops_Alloc(Ob % header % ImpactParamAll, "ImpactParamAll", Ob % Header % NumObsLocal, Ob % ImpactParamAll)
      ! ELSE
      !   CALL Ops_Alloc(Ob % header % ImpactParam, "ImpactParam", Ob % Header % NumObsLocal, Ob % ImpactParam)
      ! END IF
    case (VarField_RO_Rad_Curv)
      call Ops_Alloc(Ob % header % RO_Rad_Curv, "RO_Rad_Curv", Ob % Header % NumObsLocal, Ob % RO_Rad_Curv)
    case (VarField_RO_geoid_und)
      call Ops_Alloc(Ob % header % RO_geoid_und, "RO_geoid_und", Ob % Header % NumObsLocal, Ob % RO_geoid_und)
    case (VarField_AOD)
      call Ops_Alloc(Ob % header % aod, "AOD", Ob % Header % NumObsLocal, Ob % aod)
    case (VarField_BriTempVarError)
      call Ops_Alloc(Ob % header % BriTempVarError, "BriTempVarError", Ob % Header % NumObsLocal, Ob % BriTempVarError)
    case (VarField_CloudRTError)
      call Ops_Alloc(Ob % header % CloudRTError, "CloudRTError", Ob % Header % NumObsLocal, Ob % CloudRTError)
    case (VarField_CloudRTBias)
      call Ops_Alloc(Ob % header % CloudRTBias, "CloudRTBias", Ob % Header % NumObsLocal, Ob % CloudRTBias)
    case (VarField_BiasPredictors)
      call Ops_Alloc(Ob % header % BiasPredictors, "BiasPredictors", Ob % Header % NumObsLocal, Ob % BiasPredictors)
    case (VarField_LevelTime)
      ! IF (PRESENT (RepObs)) THEN
      !   ObHdrVrbl = RepObs % header % model_level_time
      ! ELSE
      !   CALL Ops_Alloc(Ob % header % level_time, "level_time", Ob % Header % NumObsLocal, Ob % level_time)
      ! END IF
    case (VarField_LevelLat)
      ! IF (PRESENT (RepObs)) THEN
      !   ObHdrVrbl = RepObs % header % model_level_lat
      ! ELSE
      !   CALL Ops_Alloc(Ob % header % level_lat, "level_lat", Ob % Header % NumObsLocal, Ob % level_lat)
      ! END IF
    case (VarField_LevelLon)
      ! IF (PRESENT (RepObs)) THEN
      !   ObHdrVrbl = RepObs % header % model_level_lon
      ! ELSE
      !   CALL Ops_Alloc(Ob % header % level_lon, "level_lon", Ob % Header % NumObsLocal, Ob % level_lon)
      ! END IF
    case (VarField_RainAccum)
      call Ops_Alloc(Ob % header % RainAccum, "RainAccum", Ob % Header % NumObsLocal, Ob % RainAccum)
    case (VarField_CeilBackscatter)
      call Ops_Alloc(Ob % header % CeilBackscatter, "CeilBackscatter", Ob % Header % NumObsLocal, Ob % CeilBackscatter)
    case (VarField_CeilRange)
      call Ops_Alloc(Ob % header % CeilRange, "CeilRange", Ob % Header % NumObsLocal, Ob % CeilRange)
    case (VarField_CeilSiteId)
      call Ops_Alloc(Ob % header % CeilSiteID, "CeilSiteID", Ob % Header % NumObsLocal, Ob % CeilSiteID)
    case (VarField_CeilScanIdent)
      call Ops_Alloc(Ob % header % CeilScanIdent, "CeilScanIdent", Ob % Header % NumObsLocal, Ob % CeilScanIdent)
    case (VarField_airqal_consttype)
      call Ops_Alloc(Ob % header % csnt_typ, "CSNT_TYP", Ob % Header % NumObsLocal, Ob % csnt_typ)
    case (VarField_airqal_massdensity)
      call Ops_Alloc(Ob % header % mass_dnsty, "MASS_DNSTY", Ob % Header % NumObsLocal, Ob % mass_dnsty)
    case (VarField_airqal_massdensityscale)
      call Ops_Alloc(Ob % header % dcml_scl_mass_dnsty, "DCML_SCL_MASS_DNSTY", Ob % Header % NumObsLocal, Ob % dcml_scl_mass_dnsty)
    case (VarField_HLOSwind)
      call Ops_Alloc(Ob % header % HLOSwind, "HLOSwind", Ob % Header % NumObsLocal, Ob % HLOSwind)
    case (VarField_ProfileNo)
      call Ops_Alloc(Ob % header % ProfileNo, "ProfileNo", Ob % Header % NumObsLocal, Ob % ProfileNo)
    case (VarField_dWinddT)
      call Ops_Alloc(Ob % header % dWinddT, "dWinddT", Ob % Header % NumObsLocal, Ob % dWinddT)
    case (VarField_dWinddP)
      call Ops_Alloc(Ob % header % dWinddP, "dWinddP", Ob % Header % NumObsLocal, Ob % dWinddP)
    case (VarField_AzimuthCOG)
      call Ops_Alloc(Ob % header % AzimuthCOG, "AzimuthCOG", Ob % Header % NumObsLocal, Ob % AzimuthCOG)
    case (VarField_HeightCOG)
      call Ops_Alloc(Ob % header % HeightCOG, "HeightCOG", Ob % Header % NumObsLocal, Ob % HeightCOG)
    case default
      write (ErrorMessage, '(A,I0)') "VarField code not recognised ", VarFields(Ivar)
      call gen_warn (RoutineName,  &
                      ErrorMessage)
      cycle
  end select
  
!   case (Varfield_theta)
!     self%geovars(i) = "air_temperature"
!   case (Varfield_u)
!     self%geovars(i) = "eastward_wind"

!   case (Varfield_v)
!     self%geovars(i) = "northward_wind"
!   case (Varfield_rh)
!     self%geovars(i) = "relative_humidity"
!   end select
end do

end subroutine cxvarobs_varobswriter_populateobservations

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
type (ElementHeader_Type), intent(inout)        :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(kind=8), intent(in)                     :: NumObs
type(Element_type), pointer                     :: El1(:)
character(len=*), intent(in)                    :: JediVarName
type(c_ptr), value, intent(in)                  :: ObsSpace
type(c_ptr), value, intent(in)                  :: ObsErrors
type (ElementHeader_Type), optional, intent(in) :: HdrIn
type(Element_Type), optional, intent(in)        :: initial_value

! Local declarations:
real(kind=c_double)                             :: ObsValue(NumObs)
real(kind=c_float)                              :: ObsError(NumObs)
real(kind=c_double)                             :: MissingDouble
real(kind=c_float)                              :: MissingFloat
integer                                         :: i

! Body:

! The types of floating-point numbers used in this function are a bit confusing. OPS stores
! observation values as doubles, whereas JEDI stores them as floats. However, the Fortran interface
! to the IODA ObsSpace is only partially implemented: obsspace_get_db_real32 doesn't work, only
! obsspace_get_db_real64 does. So we need to retrieve observation values as doubles. Observation
! errors, though, are retrieved as floats.

MissingDouble = missing_value(0.0_c_double)
MissingFloat  = missing_value(0.0_c_float)

call Ops_Alloc(Hdr, OpsVarName, NumObs, El1, HdrIn, initial_value)
call obsspace_get_db(ObsSpace, "ObsValue", JediVarName, ObsValue)
call cxvarobs_obsdatavector_float_get(ObsErrors, JediVarName, ObsError)
do i = 1, NumObs
  if (ObsValue(i) /= MissingDouble) El1(i) % Value = ObsValue(i)
  if (ObsError(i) /= MissingFloat)  El1(i) % OBErr = ObsError(i)
  ! TODO: Flags, PGEFinal
end do
end subroutine cxvarobs_varobswriter_fillobsvalueanderror_1d

end module cxvarobs_varobswriter_mod
