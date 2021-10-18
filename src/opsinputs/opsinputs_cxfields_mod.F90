! (C) Crown Copyright 2020, the Met Office. All rights reserved.
!
! Refer to COPYRIGHT.txt of this distribution for details.

!> Maps names of fields defined in the Cx_Info type to names of JEDI geovals used to fill
!> these fields.

module opsinputs_cxfields_mod

use ufo_vars_mod

implicit none
private

character(len=*), parameter, public :: opsinputs_cxfields_unknown = "UNKNOWN"

! TODO(someone): Replace instances of opsinputs_cxfields_unknown with appropriate geoval names.
character(len=*), parameter, public :: opsinputs_cxfields_Orog = var_sfc_geomz
character(len=*), parameter, public :: opsinputs_cxfields_pstar = var_sfc_p2m
character(len=*), parameter, public :: opsinputs_cxfields_t2 = var_sfc_t2m
character(len=*), parameter, public :: opsinputs_cxfields_rh2 = "relative_humidity_2m"
character(len=*), parameter, public :: opsinputs_cxfields_u10 = var_sfc_u10
character(len=*), parameter, public :: opsinputs_cxfields_v10 = var_sfc_v10
character(len=*), parameter, public :: opsinputs_cxfields_vis = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_WAVE_HGHT = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_WIND_SPED = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_SeaHeight = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_TskinSea = var_sfc_tskin
character(len=*), parameter, public :: opsinputs_cxfields_TropPres = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_pmsl = var_ps
character(len=*), parameter, public :: opsinputs_cxfields_SeaIce = var_sfc_ifrac
character(len=*), parameter, public :: opsinputs_cxfields_SnowAmount = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_qt2 = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_aerosol = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_PSurfParamA = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_PSurfParamB = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_LapseRate = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_CloudAmount = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_ConvCloudAmount = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_ConvCloudBaseLevel = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_ConvCloudTopLevel = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_SurfRainRate_conv = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_SurfSnowRate_conv = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_SeaSrfcHeight = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_MeanSeaHeight = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_SurfRainRate_LS = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_SurfSnowRate_LS = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_SWradiation = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_BLheight = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_ObukhovLength = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_FrictionVel = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_PrecipAcc6hr = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_LowCloudAmount = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_MedCloudAmount = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_LowCloudBase = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_SO2_AQ = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_PM10_AQ = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_PM2p5_AQ = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_O3_AQ = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_NO2_AQ = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_CO_AQ = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_BLtype = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_theta = "theta"
character(len=*), parameter, public :: opsinputs_cxfields_rh = "relative_humidity"
character(len=*), parameter, public :: opsinputs_cxfields_u = var_u
character(len=*), parameter, public :: opsinputs_cxfields_v = var_v
character(len=*), parameter, public :: opsinputs_cxfields_w = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_q = var_q
character(len=*), parameter, public :: opsinputs_cxfields_qc = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_p_bar = var_prs
character(len=*), parameter, public :: opsinputs_cxfields_cloud = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_ql_layer = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_PLevelsA = var_prsi
character(len=*), parameter, public :: opsinputs_cxfields_Salt = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_t = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_qf_layer = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_RainRate_layer = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_cloud_conv = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_qc_conv = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_cloud_layer = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_ozone = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_qcf = var_cli
character(len=*), parameter, public :: opsinputs_cxfields_qcl = var_clw
character(len=*), parameter, public :: opsinputs_cxfields_cloud_bulk = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_aerosol_p = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_CDNC = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_RH_AMC = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_Cl = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_Cf = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_qrain = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_ExnerA = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_RichNumber = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_SoilMoisture = opsinputs_cxfields_unknown
character(len=*), parameter, public :: opsinputs_cxfields_SoilTemp = opsinputs_cxfields_unknown
! TODO(someone): is this geoval name correct? It matches the name used in the
! aod_geoval_2018041500_s.nc4 file in UFO.
character(len=*), parameter, public :: opsinputs_cxfields_dustp = "dust"

end module opsinputs_cxfields_mod
