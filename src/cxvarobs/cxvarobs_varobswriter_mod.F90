! (C) Copyright 2020 Met Office UK
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

!> Fortran module to implement varobswriter

module cxvarobs_varobswriter_mod

use fckit_configuration_module, only: fckit_configuration
use iso_c_binding
use kinds
use ufo_geovals_mod
use obsspace_mod
use ufo_vars_mod

use opsmod_obsinfo

USE GenMod_Core, ONLY: &
    gen_warn          

USE GenMod_ModelIO, ONLY: LenFixHd, UM_header_type
USE GenMod_Setup, ONLY: Gen_SetupControl
USE GenMod_UMHeaderConstants

USE OpsMod_Control, ONLY:   &
    DefaultDocURL,          &
    Ops_InitMPI
USE OpsMod_MiscTypes
USE OpsMod_ObsGroupInfo, ONLY: &
    OpsFn_ObsGroupNameToNum,   &
    ObsGroupAircraft,          &
    ObsGroupSurface,           &
    ObsGroupSatwind,           &
    ObsGroupScatwind
USE OpsMod_Varfields
USE OpsMod_Varobs

USE OpsMod_SatRad_RTmodel, ONLY: &
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

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_create(self, f_conf)
implicit none
type(cxvarobs_varobswriter), intent(inout)      :: self
type(fckit_configuration), intent(in) :: f_conf

CALL Gen_SetupControl(DefaultDocURL)
CALL Ops_InitMPI

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

subroutine cxvarobs_varobswriter_post(self, obspace, nvars, nlocs, hofx)
implicit none
type(cxvarobs_varobswriter),  intent(in) :: self
type(c_ptr), value, intent(in) :: obspace
integer,            intent(in) :: nvars, nlocs
real(c_double),     intent(in) :: hofx(nvars, nlocs)

type(OB_type)                  :: obs
type(UM_header_type)           :: CxHeader
integer(kind=8)                :: varfields(ActualMaxVarfield)
integer, parameter             :: nproc = 1, rank = 0
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

! TODO: deal with MPI

Obs % Header % NumCXBatches = 1
ALLOCATE(Obs % Header % ObsPerBatchPerPE(Obs % Header % NumCXBatches, 0:nproc - 1))
Obs % Header % ObsPerBatchPerPE(1,rank) = obs % header % numobslocal

call Ops_ReadVarobsControlNL (self % obsgroup, varfields) ! TODO(wsmigaj): move to separate function?
call cxvarobs_varobswriter_populateobservations(self, varfields, obs)

CxHeader % FixHd(FH_IntCStart) = LenFixHd + 1
CxHeader % FixHd(FH_IntCSize) = 49
CxHeader % FixHd(FH_RealCStart) = CxHeader % FixHd(FH_IntCStart) + CxHeader % FixHd(FH_IntCSize)
CxHeader % FixHd(FH_RealCSize) = 34
CALL CxHeader % alloc

CALL Ops_CreateVarobs (Obs,                 & ! in
                       CxHeader,            & ! in
                       AssimDataFormat_VAR, &
                       NumVarobsTotal)        ! TODO: PGEBd

CALL obs % deallocate()
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

subroutine cxvarobs_varobswriter_populateobservations(self, varfields, Ob)
implicit none
type(cxvarobs_varobswriter),  intent(in) :: self
integer(kind=8), intent(in)              :: varfields(:)
type(OB_type), intent(inout)             :: Ob

CHARACTER(len=*), PARAMETER          :: RoutineName = "cxvarobs_varobswriter_populateobservations"
CHARACTER(len=80)                    :: ErrorMessage

integer :: nvarfields
integer :: Ivar

TYPE (ElementHeader_type)            :: ObHdrVrbl
INTEGER                              :: FieldSize
INTEGER                              :: NumLevs
INTEGER                              :: Zcode
LOGICAL                              :: UseLevelSubset

nvarfields = size(varfields)

CALL Ops_Alloc(Ob % header % Latitude, "Latitude", Ob % Header % NumObsLocal, Ob % Latitude)
CALL Ops_Alloc(Ob % header % Longitude, "Longitude", Ob % Header % NumObsLocal, Ob % Longitude)
CALL Ops_Alloc(Ob % header % Time, "Time", Ob % Header % NumObsLocal, Ob % Time)
CALL Ops_Alloc(Ob % header % ReportFlags, "ReportFlags", Ob % Header % NumObsLocal, Ob % ReportFlags)
CALL Ops_Alloc(Ob % header % ObsType, "ObsType", Ob % Header % NumObsLocal, Ob % ObsType)

! TODO: Num levels
SELECT CASE (Ob % header % ObsGroup)
CASE (ObsGroupAircraft, ObsGroupSatwind)
  CALL Ops_Alloc(Ob % header % PlevelsA, "PlevelsA", Ob % Header % NumObsLocal, Ob % PlevelsA)
END SELECT

do Ivar = 1, nvarfields
  NumLevs = 1         ! TODO: Extend to multilevel quantities
  Zcode = ZcodeUnused ! TODO: fill in correctly

  UseLevelSubset = .FALSE.
  select case (varfields(Ivar))
    CASE (IMDI)
      CYCLE
    CASE (VarField_pstar)
      CALL Ops_Alloc(Ob % header % pstar, "pstar", Ob % Header % NumObsLocal, Ob % pstar)
    CASE (VarField_theta)
      CALL Ops_Alloc(Ob % header % theta, "theta", Ob % Header % NumObsLocal, Ob % theta)
    CASE (VarField_temperature)
      IF (Ob % header % ObsGroup == ObsGroupSurface) THEN
        CALL Ops_Alloc(Ob % header % t2, "t2", Ob % Header % NumObsLocal, Ob % t2)
      ELSE
        CALL Ops_Alloc(Ob % header % t, "t", Ob % Header % NumObsLocal, Ob % t)
      END IF
    CASE (VarField_rh)
      IF (Ob % header % ObsGroup == ObsGroupSurface) THEN
        CALL Ops_Alloc(Ob % header % rh2, "rh2", Ob % Header % NumObsLocal, Ob % rh2)
      ELSE
        CALL Ops_Alloc(Ob % header % rh, "rh", Ob % Header % NumObsLocal, Ob % rh)
      END IF
    CASE (VarField_u)
      IF (Ob % header % ObsGroup == ObsGroupSurface .OR. &
          Ob % header % ObsGroup == ObsGroupScatwind) THEN
        CALL Ops_Alloc(Ob % header % u10, "u10", Ob % Header % NumObsLocal, Ob % u10)
      ELSE
        CALL Ops_Alloc(Ob % header % u, "u", Ob % Header % NumObsLocal, Ob % u)
      END IF
    CASE (VarField_v)
      IF (Ob % header % ObsGroup == ObsGroupSurface .OR. &
          Ob % header % ObsGroup == ObsGroupScatwind) THEN
        CALL Ops_Alloc(Ob % header % v10, "v10", Ob % Header % NumObsLocal, Ob % v10)
      ELSE
        CALL Ops_Alloc(Ob % header % v, "v", Ob % Header % NumObsLocal, Ob % v)
      END IF
    CASE (VarField_logvis)
      CALL Ops_Alloc(Ob % header % logvis, "logvis", Ob % Header % NumObsLocal, Ob % logvis)
    CASE (VarField_tcwv)
      CALL Ops_Alloc(Ob % header % TCWV, "TCWV", Ob % Header % NumObsLocal, Ob % TCWV)
    CASE (VarField_windspeed)
      CALL Ops_Alloc(Ob % header % WindSpeed, "WindSpeed", Ob % Header % NumObsLocal, Ob % WindSpeed)
    CASE (VarField_lwp)
      CALL Ops_Alloc(Ob % header % LWP, "LWP", Ob % Header % NumObsLocal, Ob % LWP)
    CASE (VarField_britemp)
      CALL Ops_Alloc(Ob % header % CorBriTemp, "CorBriTemp", Ob % Header % NumObsLocal, Ob % CorBriTemp)
    CASE (VarField_tskin)
      CALL Ops_Alloc(Ob % header % Tskin, "Tskin", Ob % Header % NumObsLocal, Ob % Tskin)
    CASE (VarField_gpstzdelay)
      CALL Ops_Alloc(Ob % header % GPSTZDelay, "GPSTZDelay", Ob % Header % NumObsLocal, Ob % GPSTZDelay)
    CASE (VarField_GPS_Station_Height)
      CALL Ops_Alloc(Ob % header % Zstation, "Zstation", Ob % Header % NumObsLocal, Ob % Zstation)
    CASE (VarField_mwemiss)
      CALL Ops_Alloc(Ob % header % MwEmiss, "MwEmiss", Ob % Header % NumObsLocal, Ob % MwEmiss)
    CASE (VarField_TCozone)
      CALL Ops_Alloc(Ob % header % TCozone, "TCozone", Ob % Header % NumObsLocal, Ob % TCozone)
    CASE (VarField_satzenith)
      CALL Ops_Alloc(Ob % header % SatZenithAngle, "SatZenithAngle", Ob % Header % NumObsLocal, Ob % SatZenithAngle)
    CASE (VarField_scanpos)
      CALL Ops_Alloc(Ob % header % ScanPosition, "ScanPosition", Ob % Header % NumObsLocal, Ob % ScanPosition)
    CASE (VarField_surface)
      CALL Ops_Alloc(Ob % header % surface, "surface", Ob % Header % NumObsLocal, Ob % surface)
    CASE (VarField_elevation)
      CALL Ops_Alloc(Ob % header % elevation, "elevation", Ob % Header % NumObsLocal, Ob % elevation)
    CASE (VarField_modelsurface)
      CALL Ops_Alloc(Ob % header % ModelSurface, "ModelSurface", Ob % Header % NumObsLocal, Ob % ModelSurface)
    CASE (VarField_modelorog)
      CALL Ops_Alloc(Ob % header % ModelOrog, "ModelOrog", Ob % Header % NumObsLocal, Ob % ModelOrog)
    CASE (VarField_stratt)
      UseLevelSubset = .TRUE.
      NumLevs = nlevels_strat_varobs
      CALL Ops_Alloc(Ob % header % t, "t", Ob % Header % NumObsLocal, Ob % t)
    CASE (VarField_satid)
      CALL Ops_Alloc(Ob % header % SatId, "SatId", Ob % Header % NumObsLocal, Ob % SatId)
    CASE (VarField_satazimth)
      CALL Ops_Alloc(Ob % header % SatAzimth, "SatAzimth", Ob % Header % NumObsLocal, Ob % SatAzimth)
    CASE (VarField_localazimuth)
      CALL Ops_Alloc(Ob % header % LocalAzimuth, "LocalAzimuth", Ob % Header % NumObsLocal, Ob % LocalAzimuth)
    CASE (VarField_solzenith)
      CALL Ops_Alloc(Ob % header % SolarZenith, "SolarZenith", Ob % Header % NumObsLocal, Ob % SolarZenith)
    CASE (VarField_solazimth)
      CALL Ops_Alloc(Ob % header % SolarAzimth, "SolarAzimth", Ob % Header % NumObsLocal, Ob % SolarAzimth)
    CASE (VarField_iremiss)
      CALL Ops_Alloc(Ob % header % IREmiss, "IREmiss", Ob % Header % NumObsLocal, Ob % IREmiss)
    CASE (VarField_cloudtopp)
      CALL Ops_Alloc(Ob % header % CloudTopP, "CloudTopP", Ob % Header % NumObsLocal, Ob % CloudTopP)
    CASE (VarField_cloudfrac)
      CALL Ops_Alloc(Ob % header % CloudFrac, "CloudFrac", Ob % Header % NumObsLocal, Ob % CloudFrac)
    CASE (VarField_vnatovpp)
      CALL Ops_Alloc(Ob % header % VnATOVPP, "VnATOVPP", Ob % Header % NumObsLocal, Ob % VnATOVPP)
    CASE (VarField_procoption)
      CALL Ops_Alloc(Ob % header % ATOVSProcOption, "ATOVSProcOption", Ob % Header % NumObsLocal, Ob % ATOVSProcOption)
    CASE (VarField_amsusurface)
      CALL Ops_Alloc(Ob % header % AMSUsurface, "AMSUsurface", Ob % Header % NumObsLocal, Ob % AMSUsurface)
    CASE (VarField_hirs_temp)
      CALL Ops_Alloc(Ob % header % HIRS_Temp, "HIRS_Temp", Ob % Header % NumObsLocal, Ob % HIRS_Temp)
    CASE (VarField_amsua1_temp)
      CALL Ops_Alloc(Ob % header % AMSUa1_Temp, "AMSUa1_Temp", Ob % Header % NumObsLocal, Ob % AMSUa1_Temp)
    CASE (VarField_amsua2_temp)
      CALL Ops_Alloc(Ob % header % AMSUa2_Temp, "AMSUa2_Temp", Ob % Header % NumObsLocal, Ob % AMSUa2_Temp)
    CASE (VarField_amsub_temp)
      CALL Ops_Alloc(Ob % header % AMSUb_Temp, "AMSUb_Temp", Ob % Header % NumObsLocal, Ob % AMSUb_Temp)
    CASE (VarField_cloud)
      CALL Ops_Alloc(Ob % header % Cloud, "Cloud", Ob % Header % NumObsLocal, Ob % Cloud)
    CASE (VarField_rainrate)
      CALL Ops_Alloc(Ob % header % Rainrate, "Rainrate", Ob % Header % NumObsLocal, Ob % Rainrate)
    CASE (VarField_snowrate)
      CALL Ops_Alloc(Ob % header % Snowrate, "Snowrate", Ob % Header % NumObsLocal, Ob % Snowrate)
    CASE (VarField_u10ambwind)
      CALL Ops_Alloc(Ob % header % u10AmbWind, "u10AmbWind", Ob % Header % NumObsLocal, Ob % u10AmbWind)
    CASE (VarField_v10ambwind)
      CALL Ops_Alloc(Ob % header % v10AmbWind, "v10AmbWind", Ob % Header % NumObsLocal, Ob % v10AmbWind)
    CASE (VarField_pcorrect)
      CALL Ops_Alloc(Ob % header % AWPriorPCorrect, "AWPriorPCorrect", Ob % Header % NumObsLocal, Ob % AWPriorPCorrect)
    CASE (VarField_NumChans)
      CALL Ops_Alloc(Ob % header % NumChans, "NumChans", Ob % Header % NumObsLocal, Ob % NumChans)
    CASE (VarField_ChanNum)
      CALL Ops_Alloc(Ob % header % ChanNum, "ChanNum", Ob % Header % NumObsLocal, Ob % ChanNum)
    CASE (VarField_Emissivity)
      CALL Ops_Alloc(Ob % header % Emissivity, "Emissivity", Ob % Header % NumObsLocal, Ob % Emissivity)
    CASE (VarField_QCinfo)
      CALL Ops_Alloc(Ob % header % QCinfo, "QCinfo", Ob % Header % NumObsLocal, Ob % QCinfo)
    CASE (VarField_SBUVozone)
      CALL Ops_Alloc(Ob % header % SBUVozone, "SBUVozone", Ob % Header % NumObsLocal, Ob % SBUVozone)
    CASE (VarField_RadialVelocity)
        CALL Ops_Alloc(Ob % header % RadialVelocSO, "RadialVelocSO", Ob % Header % NumObsLocal, Ob % RadialVelocSO)
    CASE (VarField_Reflectivity)
      CALL Ops_Alloc(Ob % header % ReflectivitySO, "ReflectivitySO", Ob % Header % NumObsLocal, Ob % ReflectivitySO)
    CASE (VarField_ReflectivityR)
      CALL Ops_Alloc(Ob % header % ReflectivityR, "ReflectivityR", Ob % Header % NumObsLocal, Ob % ReflectivityR)
    CASE (VarField_ReflectivityI)
      CALL Ops_Alloc(Ob % header % ReflectivityI, "ReflectivityI", Ob % Header % NumObsLocal, Ob % ReflectivityI)
    CASE (VarField_RadarBeamElev)
      CALL Ops_Alloc(Ob % header % RadarBeamElev, "RadarBeamElev", Ob % Header % NumObsLocal, Ob % RadarBeamElev)
    CASE (VarField_RadarObRange)
      CALL Ops_Alloc(Ob % header % RadarObRange, "RadarObRange", Ob % Header % NumObsLocal, Ob % RadarObRange)
    CASE (VarField_RadarObAzim)
      CALL Ops_Alloc(Ob % header % RadarObAzim, "RadarObAzim", Ob % Header % NumObsLocal, Ob % RadarObAzim)
    CASE (VarField_RadIdent)
      CALL Ops_Alloc(Ob % header % RadIdent, "RadIdent", Ob % Header % NumObsLocal, Ob % RadIdent)
    CASE (VarField_RadAltAboveMSL)
      CALL Ops_Alloc(Ob % header % RadAltAboveMSL, "RadAltAboveMSL", Ob % Header % NumObsLocal, Ob % RadAltAboveMSL)
    CASE (VarField_RadNoiseLvl)
      CALL Ops_Alloc(Ob % header % RadNoiseLvl, "RadNoiseLvl", Ob % Header % NumObsLocal, Ob % RadNoiseLvl)
    CASE (VarField_RadFlag)
      CALL Ops_Alloc(Ob % header % RadFlag, "RadFlag", Ob % Header % NumObsLocal, Ob % RadFlag)
    CASE (VarField_clw)
      CALL Ops_Alloc(Ob % header % clw, "clw", Ob % Header % NumObsLocal, Ob % clw)
    CASE (VarField_refrac)
      CALL Ops_Alloc(Ob % header % refrac, "refrac", Ob % Header % NumObsLocal, Ob % refrac)
    CASE (VarField_z)
      CALL Ops_Alloc(Ob % header % z, "z", Ob % Header % NumObsLocal, Ob % z)
    CASE (VarField_BendingAngle)
      ! IF (GPSRO_TPD) THEN
      !   CALL Ops_Alloc(Ob % header % BendingAngleAll, "BendingAngleAll", Ob % Header % NumObsLocal, Ob % BendingAngleAll)
      ! ELSE
      !   CALL Ops_Alloc(Ob % header % BendingAngle, "BendingAngle", Ob % Header % NumObsLocal, Ob % BendingAngle)
      ! END IF
    CASE (VarField_ImpactParam)
      ! IF (GPSRO_TPD) THEN
      !   CALL Ops_Alloc(Ob % header % ImpactParamAll, "ImpactParamAll", Ob % Header % NumObsLocal, Ob % ImpactParamAll)
      ! ELSE
      !   CALL Ops_Alloc(Ob % header % ImpactParam, "ImpactParam", Ob % Header % NumObsLocal, Ob % ImpactParam)
      ! END IF
    CASE (VarField_RO_Rad_Curv)
      CALL Ops_Alloc(Ob % header % RO_Rad_Curv, "RO_Rad_Curv", Ob % Header % NumObsLocal, Ob % RO_Rad_Curv)
    CASE (VarField_RO_geoid_und)
      CALL Ops_Alloc(Ob % header % RO_geoid_und, "RO_geoid_und", Ob % Header % NumObsLocal, Ob % RO_geoid_und)
    CASE (VarField_AOD)
      CALL Ops_Alloc(Ob % header % AOD, "AOD", Ob % Header % NumObsLocal, Ob % AOD)
    CASE (VarField_BriTempVarError)
      CALL Ops_Alloc(Ob % header % BriTempVarError, "BriTempVarError", Ob % Header % NumObsLocal, Ob % BriTempVarError)
    CASE (VarField_CloudRTError)
      CALL Ops_Alloc(Ob % header % CloudRTError, "CloudRTError", Ob % Header % NumObsLocal, Ob % CloudRTError)
    CASE (VarField_CloudRTBias)
      CALL Ops_Alloc(Ob % header % CloudRTBias, "CloudRTBias", Ob % Header % NumObsLocal, Ob % CloudRTBias)
    CASE (VarField_BiasPredictors)
      CALL Ops_Alloc(Ob % header % BiasPredictors, "BiasPredictors", Ob % Header % NumObsLocal, Ob % BiasPredictors)
    CASE (VarField_LevelTime)
      ! IF (PRESENT (RepObs)) THEN
      !   ObHdrVrbl = RepObs % header % model_level_time
      ! ELSE
      !   CALL Ops_Alloc(Ob % header % level_time, "level_time", Ob % Header % NumObsLocal, Ob % level_time)
      ! END IF
    CASE (VarField_LevelLat)
      ! IF (PRESENT (RepObs)) THEN
      !   ObHdrVrbl = RepObs % header % model_level_lat
      ! ELSE
      !   CALL Ops_Alloc(Ob % header % level_lat, "level_lat", Ob % Header % NumObsLocal, Ob % level_lat)
      ! END IF
    CASE (VarField_LevelLon)
      ! IF (PRESENT (RepObs)) THEN
      !   ObHdrVrbl = RepObs % header % model_level_lon
      ! ELSE
      !   CALL Ops_Alloc(Ob % header % level_lon, "level_lon", Ob % Header % NumObsLocal, Ob % level_lon)
      ! END IF
    CASE (VarField_RainAccum)
      CALL Ops_Alloc(Ob % header % RainAccum, "RainAccum", Ob % Header % NumObsLocal, Ob % RainAccum)
    CASE (VarField_CeilBackscatter)
      CALL Ops_Alloc(Ob % header % CeilBackscatter, "CeilBackscatter", Ob % Header % NumObsLocal, Ob % CeilBackscatter)
    CASE (VarField_CeilRange)
      CALL Ops_Alloc(Ob % header % CeilRange, "CeilRange", Ob % Header % NumObsLocal, Ob % CeilRange)
    CASE (VarField_CeilSiteId)
      CALL Ops_Alloc(Ob % header % CeilSiteID, "CeilSiteID", Ob % Header % NumObsLocal, Ob % CeilSiteID)
    CASE (VarField_CeilScanIdent)
      CALL Ops_Alloc(Ob % header % CeilScanIdent, "CeilScanIdent", Ob % Header % NumObsLocal, Ob % CeilScanIdent)
    CASE (VarField_airqal_consttype)
      CALL Ops_Alloc(Ob % header % CSNT_TYP, "CSNT_TYP", Ob % Header % NumObsLocal, Ob % CSNT_TYP)
    CASE (VarField_airqal_massdensity)
      CALL Ops_Alloc(Ob % header % MASS_DNSTY, "MASS_DNSTY", Ob % Header % NumObsLocal, Ob % MASS_DNSTY)
    CASE (VarField_airqal_massdensityscale)
      CALL Ops_Alloc(Ob % header % DCML_SCL_MASS_DNSTY, "DCML_SCL_MASS_DNSTY", Ob % Header % NumObsLocal, Ob % DCML_SCL_MASS_DNSTY)
    CASE (VarField_HLOSwind)
      CALL Ops_Alloc(Ob % header % HLOSwind, "HLOSwind", Ob % Header % NumObsLocal, Ob % HLOSwind)
    CASE (VarField_ProfileNo)
      CALL Ops_Alloc(Ob % header % ProfileNo, "ProfileNo", Ob % Header % NumObsLocal, Ob % ProfileNo)
    CASE (VarField_dWinddT)
      CALL Ops_Alloc(Ob % header % dWinddT, "dWinddT", Ob % Header % NumObsLocal, Ob % dWinddT)
    CASE (VarField_dWinddP)
      CALL Ops_Alloc(Ob % header % dWinddP, "dWinddP", Ob % Header % NumObsLocal, Ob % dWinddP)
    CASE (VarField_AzimuthCOG)
      CALL Ops_Alloc(Ob % header % AzimuthCOG, "AzimuthCOG", Ob % Header % NumObsLocal, Ob % AzimuthCOG)
    CASE (VarField_HeightCOG)
      CALL Ops_Alloc(Ob % header % HeightCOG, "HeightCOG", Ob % Header % NumObsLocal, Ob % HeightCOG)
    CASE DEFAULT
      WRITE (ErrorMessage, '(A,I0)') "VarField code not recognised ", VarFields(Ivar)
      CALL gen_warn (RoutineName,  &
                      ErrorMessage)
      CYCLE
  END SELECT
  
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

! subroutine InitElementHeader(NumLevels, ElementHeader)
!   implicit none
!   integer, intent(in) :: NumLevels
!   type(ElementHeader_type), intent(inout) :: ElementHeader
    
!   Header % Present = .TRUE.
!   Header % NumLev = NumLevels
!   ! Header % Zcode = Zcode ! TODO: Is this needed?
! end subroutine InitHeader

! subroutine InitSingleLevelElements(Ob % Header % NumObsLocal, Elements)
! implicit none
! integer, intent(in) :: Ob % Header % NumObsLocal
! type(Element_type), pointer, intent(inout) :: Elements(:)
  
! ALLOCATE(Elements(Ob % Header % NumObsLocal))
! Elements = RMDI

! end subroutine InitSingleLevelElements

end module cxvarobs_varobswriter_mod
