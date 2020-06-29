! (C) Copyright 2020 Met Office UK
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

!> Maps names of fields of the Cx_Info type to names of JEDI geovals used to fill these fields.

module opsinputs_cxfields_mod

implicit none

character(len=*), parameter :: opsinputs_cxfields_unknown = "UNKNOWN"

! TODO(someone): Replace instances of opsinputs_cxfields_unknown with appropriate geoval names.
character(len=*), parameter :: opsinputs_cxfields_Orog = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_pstar = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_t2 = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_rh2 = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_u10 = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_v10 = opsinputs_cxfields_unknown
! TODO(someone): Verify that this is the right geoval name to use
character(len=*), parameter :: opsinputs_cxfields_ModelSurface = "land_type_index"
character(len=*), parameter :: opsinputs_cxfields_vis = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_WAVE_HGHT = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_WIND_SPED = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_SeaHeight = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_TskinSea = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_TropPres = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_pmsl = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_SeaIce = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_SnowAmount = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_qt2 = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_aerosol = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_PSurfParamA = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_PSurfParamB = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_LapseRate = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_CloudAmount = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_ConvCloudAmount = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_ConvCloudBaseLevel = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_ConvCloudTopLevel = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_SurfRainRate_conv = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_SurfSnowRate_conv = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_SeaSrfcHeight = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_MeanSeaHeight = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_SurfRainRate_LS = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_SurfSnowRate_LS = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_SWradiation = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_BLheight = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_ObukhovLength = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_FrictionVel = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_PrecipAcc6hr = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_LowCloudAmount = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_MedCloudAmount = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_LowCloudBase = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_SO2_AQ = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_PM10_AQ = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_PM2p5_AQ = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_O3_AQ = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_NO2_AQ = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_CO_AQ = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_BLtype = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_theta = "air_potential_temperature"
character(len=*), parameter :: opsinputs_cxfields_rh = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_u = "eastward_wind"
character(len=*), parameter :: opsinputs_cxfields_v = "northward_wind"
character(len=*), parameter :: opsinputs_cxfields_w = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_q = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_qc = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_p_bar = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_cloud = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_ql_layer = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_PLevelsA = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_Salt = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_t = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_qf_layer = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_RainRate_layer = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_cloud_conv = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_qc_conv = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_cloud_layer = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_ozone = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_qcf = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_qcl = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_cloud_bulk = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_aerosol_p = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_CDNC = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_RH_AMC = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_Cl = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_Cf = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_qrain = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_ExnerA = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_RichNumber = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_SoilMoisture = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_SoilTemp = opsinputs_cxfields_unknown
character(len=*), parameter :: opsinputs_cxfields_dustp = opsinputs_cxfields_unknown

end module opsinputs_cxfields_mod
