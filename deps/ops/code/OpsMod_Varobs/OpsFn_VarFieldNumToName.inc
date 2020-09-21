!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Converts varfield number to string.
!-------------------------------------------------------------------------------

ELEMENTAL FUNCTION OpsFn_VarFieldNumToName (varfield)

! Function arguments:
INTEGER, INTENT(IN) :: varfield

! Function result:
CHARACTER(len=50)   :: OpsFn_VarFieldNumToName

OpsFn_VarFieldNumToName = "unknown"

SELECT CASE (varfield)
  CASE (VarField_pstar)
    OpsFn_VarFieldNumToName = "pstar"
  CASE (VarField_temperature)
    OpsFn_VarFieldNumToName = "temperature"
  CASE (VarField_rh)
    OpsFn_VarFieldNumToName = "rh"
  CASE (VarField_u)
    OpsFn_VarFieldNumToName = "u"
  CASE (VarField_v)
    OpsFn_VarFieldNumToName = "v"
  CASE (VarField_logvis)
    OpsFn_VarFieldNumToName = "logvis"
  CASE (VarField_tcwv)
    OpsFn_VarFieldNumToName = "tcwv"
  CASE (VarField_windspeed)
    OpsFn_VarFieldNumToName = "windspeed"
  CASE (VarField_lwp)
    OpsFn_VarFieldNumToName = "lwp"
  CASE (VarField_britemp)
    OpsFn_VarFieldNumToName = "britemp"
  CASE (VarField_tskin)
    OpsFn_VarFieldNumToName = "tskin"
  CASE (VarField_gpstzdelay)
    OpsFn_VarFieldNumToName = "gpstzdelay"
  CASE (VarField_cloud)
    OpsFn_VarFieldNumToName = "cloud"
  CASE (VarField_rainrate)
    OpsFn_VarFieldNumToName = "rainrate"
  CASE (VarField_mwemiss)
    OpsFn_VarFieldNumToName = "mwemiss"
  CASE (VarField_tcozone)
    OpsFn_VarFieldNumToName = "tcozone"
  CASE (VarField_satzenith)
    OpsFn_VarFieldNumToName = "satzenith"
  CASE (VarField_scanpos)
    OpsFn_VarFieldNumToName = "scanpos"
  CASE (VarField_surface)
    OpsFn_VarFieldNumToName = "surface"
  CASE (VarField_elevation)
    OpsFn_VarFieldNumToName = "elevation"
  CASE (VarField_modelsurface)
    OpsFn_VarFieldNumToName = "model surface"
  CASE (VarField_modelorog)
    OpsFn_VarFieldNumToName = "model orog"
  CASE (VarField_chanword_ir)
    OpsFn_VarFieldNumToName = "channel word ir"
  CASE (VarField_chanword_mw)
    OpsFn_VarFieldNumToName = "channel word mw"
  CASE (VarField_stratt)
    OpsFn_VarFieldNumToName = "stratt"
  CASE (VarField_satid)
    OpsFn_VarFieldNumToName = "sat id"
  CASE (VarField_satazimth)
    OpsFn_VarFieldNumToName = "sat azimuth"
  CASE (VarField_localazimuth)
    OpsFn_VarFieldNumToName = "local azimuth"
  CASE (VarField_solzenith)
    OpsFn_VarFieldNumToName = "solar zenith"
  CASE (VarField_solazimth)
    OpsFn_VarFieldNumToName = "solar azimuth"
  CASE (VarField_iremiss)
    OpsFn_VarFieldNumToName = "IR emiss param"
  CASE (VarField_cloudtopp)
    OpsFn_VarFieldNumToName = "cloud top p"
  CASE (VarField_cloudfrac)
    OpsFn_VarFieldNumToName = "cloud frac"
  CASE (VarField_vnatovpp)
    OpsFn_VarFieldNumToName = "VN atov PP"
  CASE (VarField_procoption)
    OpsFn_VarFieldNumToName = "proc option"
  CASE (VarField_amsusurface)
    OpsFn_VarFieldNumToName = "AMSU surface"
  CASE (VarField_hirs_temp)
    OpsFn_VarFieldNumToName = "HIRS temp"
  CASE (VarField_amsua1_temp)
    OpsFn_VarFieldNumToName = "AMSUA 1 temp"
  CASE (VarField_amsua2_temp)
    OpsFn_VarFieldNumToName = "AMSUA 2 temp"
  CASE (VarField_amsub_temp)
    OpsFn_VarFieldNumToName = "AMSUB temp"
  CASE (VarField_u10ambwind)
    OpsFn_VarFieldNumToName = "u10 amb wind"
  CASE (VarField_v10ambwind)
    OpsFn_VarFieldNumToName = "v10 amb wind"
  CASE (VarField_pcorrect)
    OpsFn_VarFieldNumToName = "p correct"
  CASE (VarField_NumChans)
    OpsFn_VarFieldNumToName = "num chans"
  CASE (VarField_ChanNum)
    OpsFn_VarFieldNumToName = "chan num"
  CASE (VarField_Emissivity)
    OpsFn_VarFieldNumToName = "emissivity"
  CASE (VarField_QCinfo)
    OpsFn_VarFieldNumToName = "qc info"
  CASE (VarField_refrac)
    OpsFn_VarFieldNumToName = "refrac"
  CASE (VarField_z)
    OpsFn_VarFieldNumToName = "z"
  CASE (VarField_SBUVozone )
    OpsFn_VarFieldNumToName = "SBUV ozone"
  CASE (VarField_GeoBriTemp)
    OpsFn_VarFieldNumToName = "geo bri temp"
  CASE (VarField_RadialVelocity)
    OpsFn_VarFieldNumToName = "radial velocity"
  CASE (VarField_RadarBeamElev)
    OpsFn_VarFieldNumToName = "radar beam elev"
  CASE (VarField_RadarObRange)
    OpsFn_VarFieldNumToName = "radar ob range"
  CASE (VarField_RadarObAzim)
    OpsFn_VarFieldNumToName = "radar ob azim"
  CASE (VarField_GPS_Station_Height)
    OpsFn_VarFieldNumToName = "gps station height"
  CASE (VarField_clw)
    OpsFn_VarFieldNumToName = "clw"
  CASE (VarField_RadIdent)
    OpsFn_VarFieldNumToName = "rad ident"
  CASE (VarField_Reflectivity)
    OpsFn_VarFieldNumToName = "radar reflectivity"
  CASE (VarField_BendingAngle)
    OpsFn_VarFieldNumToName = "bending angle"
  CASE (VarField_ImpactParam)
    OpsFn_VarFieldNumToName = "impact param"
  CASE (VarField_RO_Rad_Curv)
    OpsFn_VarFieldNumToName = "ro rad curv"
  CASE (VarField_RO_geoid_und)
    OpsFn_VarFieldNumToName = "ro geoid und"
  CASE (VarField_RadAltAboveMSL)
    OpsFn_VarFieldNumToName = "rad alt above msl"
  CASE (VarField_BriTempVarError)
    OpsFn_VarFieldNumToName = "bri temp var error"
  CASE (VarField_AOD)
    OpsFn_VarFieldNumToName = "aod"
  CASE (VarField_Theta)
    OpsFn_VarFieldNumToName = "theta"
  CASE (VarField_RadNoiseLvl)
    OpsFn_VarFieldNumToName = "rad noise level"
  CASE (VarField_BiasPredictors)
    OpsFn_VarFieldNumToName = "bias predictors"
  CASE (VarField_LevelTime)
    OpsFn_VarFieldNumToName = "level time"
  CASE (VarField_LevelLat)
    OpsFn_VarFieldNumToName = "level lat"
  CASE (VarField_LevelLon)
    OpsFn_VarFieldNumToName = "level lon"
  CASE (VarField_RainAccum)
    OpsFn_VarFieldNumToName = "rain accum"
  CASE (VarField_CloudRTError)
    OpsFn_VarFieldNumToName = "cloud rt error"
  CASE (VarField_CloudRTBias)
    OpsFn_VarFieldNumToName = "cloud rt bias"
  CASE (VarField_Refractivity)
    OpsFn_VarFieldNumToName = "refractivity"
  CASE (VarField_ReflectivityR)
    OpsFn_VarFieldNumToName = "radar reflectivity from rain"
  CASE (VarField_ReflectivityI)
    OpsFn_VarFieldNumToName = "radar reflectivity from ice"
  CASE (VarField_CeilBackscatter)
    OpsFn_VarFieldNumToName = "ceilometer backscatter profile"
  CASE (VarField_CeilRange)
    OpsFn_VarFieldNumToName = "ceilometer backscatter range"
  CASE (VarField_CeilSiteID)
    OpsFn_VarFieldNumToName = "ceilometer wmo site id"
  CASE (VarField_CeilScanIdent)
    OpsFn_VarFieldNumToName = "ceilometer scan ident"
  CASE (VarField_airqal_consttype)
    OpsFn_VarFieldNumToName = "air quality constituent type"
  CASE (VarField_airqal_massdensity)
    OpsFn_VarFieldNumToName = "air quality mass density"
  CASE (VarField_airqal_massdensityscale)
    OpsFn_VarFieldNumToName = "air quality mass density scale"
  CASE (VarField_HLOSwind)
    OpsFn_VarFieldNumtoName = "hlos wind"
  CASE (VarField_ProfileNo)
    OpsFn_VarFieldNumtoName = "profile number"
  CASE (VarField_dWinddT)
    OpsFn_VarFieldNumtoName = "HLOS wind derivative wrt t"
  CASE (VarField_dWinddP)
    OpsFn_VarFieldNumtoName = "HLOS wind derivative wrt p"
  CASE (VarField_AzimuthCOG)
    OpsFn_VarFieldNumtoName = "azimuth at centre of gravity of range bin"
  CASE (VarField_HeightCOG)
    OpsFn_VarFieldNumtoName = "height at centre of gravity of range bin"
  CASE (VarField_RadFlag)
    OpsFn_VarFieldNumToName = "radar qc flag"
END SELECT

END FUNCTION OpsFn_VarFieldNumToName
