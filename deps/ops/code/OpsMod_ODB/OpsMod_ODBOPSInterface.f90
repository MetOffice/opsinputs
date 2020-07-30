!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Holds variables and functions to aid OPS/ODB interfacing.
!-------------------------------------------------------------------------------

MODULE OpsMod_ODBOPSInterface

USE OpsMod_Index, ONLY: &
  MaxElementIndex

USE OpsMod_ODBTypes, ONLY: &
  ODBElemDesp_type

IMPLICIT NONE

SAVE

TYPE (ODBElemDesp_type) :: ElemDesp2ODBDesp(MaxElementIndex)

! ODB report types, Ops_SetupODBReportTypes and report_types.h
LOGICAL, PRIVATE        :: have_setup_groupids = .FALSE.
LOGICAL, PRIVATE        :: have_setup_reporttypes = .FALSE.
LOGICAL, PRIVATE        :: have_setup_vertcotypes = .FALSE.

! Vertco types referenced directly by OPS
INTEGER                 :: vertco_type_amb_wind_num = 0
INTEGER                 :: vertco_type_cha_number = 0
INTEGER                 :: vertco_type_derived_pressure = 0
INTEGER                 :: vertco_type_imp_param = 0
INTEGER                 :: vertco_type_lidar_range = 0
INTEGER                 :: vertco_type_model_pressure = 0
INTEGER                 :: vertco_type_modlevno = 0
INTEGER                 :: vertco_type_pressure = 0
INTEGER                 :: vertco_type_tangent_height = 0
INTEGER                 :: vertco_type_lane_number = 0

! Standard report type numbers
INTEGER                 :: rep_type_meteosat_7_geos_rad = 0
INTEGER                 :: rep_type_meteosat_8_geos_rad = 0
INTEGER                 :: rep_type_meteosat_9_geos_rad = 0
INTEGER                 :: rep_type_mtsat_1r_geos_rad = 0
INTEGER                 :: rep_type_goes_13_geos_rad = 0
INTEGER                 :: rep_type_mtsat_2r_geos_rad = 0
INTEGER                 :: rep_type_goes_14_geos_rad = 0
INTEGER                 :: rep_type_goes_15_geos_rad = 0
INTEGER                 :: rep_type_goes_16_geos_rad = 0
INTEGER                 :: rep_type_goes_17_geos_rad = 0
INTEGER                 :: rep_type_meteosat_10_geos_rad = 0
INTEGER                 :: rep_type_meteosat_11_geos_rad = 0
INTEGER                 :: rep_type_noaa_16_sbuv = 0
INTEGER                 :: rep_type_noaa_17_sbuv = 0
INTEGER                 :: rep_type_noaa_18_sbuv = 0
INTEGER                 :: rep_type_noaa_19_sbuv = 0
INTEGER                 :: rep_type_envisat_meris_tcwv = 0
INTEGER                 :: rep_type_meteosat_11_aod = 0
INTEGER                 :: rep_type_metop_a_gpsro = 0
INTEGER                 :: rep_type_champ_gpsro = 0
INTEGER                 :: rep_type_grace_a_gpsro = 0
INTEGER                 :: rep_type_cosmic_1_gpsro = 0
INTEGER                 :: rep_type_cosmic_2_gpsro = 0
INTEGER                 :: rep_type_cosmic_3_gpsro = 0
INTEGER                 :: rep_type_cosmic_4_gpsro = 0
INTEGER                 :: rep_type_cosmic_5_gpsro = 0
INTEGER                 :: rep_type_cosmic_6_gpsro = 0
INTEGER                 :: rep_type_cosmic2_e1_gpsro = 0
INTEGER                 :: rep_type_cosmic2_e2_gpsro = 0
INTEGER                 :: rep_type_cosmic2_e3_gpsro = 0
INTEGER                 :: rep_type_cosmic2_e4_gpsro = 0
INTEGER                 :: rep_type_cosmic2_e5_gpsro = 0
INTEGER                 :: rep_type_cosmic2_e6_gpsro = 0
INTEGER                 :: rep_type_terrasar_x_gpsro = 0
INTEGER                 :: rep_type_cnofs_gpsro = 0
INTEGER                 :: rep_type_sac_c_gpsro = 0
INTEGER                 :: rep_type_metop_b_gpsro = 0
INTEGER                 :: rep_type_metop_c_gpsro = 0
INTEGER                 :: rep_type_grace_b_gpsro = 0
INTEGER                 :: rep_type_tandem_x_gpsro = 0
INTEGER                 :: rep_type_fy3c_gpsro = 0
INTEGER                 :: rep_type_megha_tropiques_gpsro = 0
INTEGER                 :: rep_type_kompsat_5_gpsro = 0
INTEGER                 :: rep_type_paz_gpsro = 0
INTEGER                 :: rep_type_fy3d_gpsro = 0
INTEGER                 :: rep_type_meteosat_7_amv = 0
INTEGER                 :: rep_type_meteosat_8_amv = 0
INTEGER                 :: rep_type_meteosat_9_amv = 0
INTEGER                 :: rep_type_meteosat_10_amv = 0
INTEGER                 :: rep_type_meteosat_11_amv = 0
INTEGER                 :: rep_type_mtsat_1r_amv = 0
INTEGER                 :: rep_type_goes_11_amv = 0
INTEGER                 :: rep_type_goes_13_amv = 0
INTEGER                 :: rep_type_goes_14_amv = 0
INTEGER                 :: rep_type_goes_15_amv = 0
INTEGER                 :: rep_type_goes_16_amv = 0
INTEGER                 :: rep_type_noaa_15_amv = 0
INTEGER                 :: rep_type_noaa_16_amv = 0
INTEGER                 :: rep_type_noaa_18_amv = 0
INTEGER                 :: rep_type_noaa_19_amv = 0
INTEGER                 :: rep_type_terra_modis_amv = 0
INTEGER                 :: rep_type_aqua_modis_amv = 0
INTEGER                 :: rep_type_fy2c_amv = 0
INTEGER                 :: rep_type_fy2d_amv = 0
INTEGER                 :: rep_type_fy2e_amv = 0
INTEGER                 :: rep_type_fy2g_amv = 0
INTEGER                 :: rep_type_mtsat_2_amv = 0
INTEGER                 :: rep_type_metop_a_amv = 0
INTEGER                 :: rep_type_metop_b_amv = 0
INTEGER                 :: rep_type_metop_c_amv = 0
INTEGER                 :: rep_type_himawari_8_amv = 0
INTEGER                 :: rep_type_himawari_9_amv = 0
INTEGER                 :: rep_type_noaa_20_amv = 0
INTEGER                 :: rep_type_goes_17_amv = 0
INTEGER                 :: rep_type_sentinel_3a_amv = 0
INTEGER                 :: rep_type_sentinel_3b_amv = 0
INTEGER                 :: rep_type_metop_a_ascat = 0
INTEGER                 :: rep_type_metop_b_ascat = 0
INTEGER                 :: rep_type_metop_c_ascat = 0
INTEGER                 :: rep_type_oceansat_2_ascat = 0
INTEGER                 :: rep_type_iss_ascat = 0
INTEGER                 :: rep_type_scatsat1_ascat = 0
INTEGER                 :: rep_type_oceansat_3_ascat = 0
INTEGER                 :: rep_type_hy2b_ascat = 0
INTEGER                 :: rep_type_cfosat_ascat = 0
INTEGER                 :: rep_type_metop_a_iasi_rad = 0
INTEGER                 :: rep_type_metop_b_iasi_rad = 0
INTEGER                 :: rep_type_metop_c_iasi_rad = 0
INTEGER                 :: rep_type_aqua_airs_rad = 0
INTEGER                 :: rep_type_auto_land_synop = 0
INTEGER                 :: rep_type_manual_land_synop = 0
INTEGER                 :: rep_type_bufr_land_synop = 0
INTEGER                 :: rep_type_metar = 0
INTEGER                 :: rep_type_dribu = 0
INTEGER                 :: rep_type_auto_ship = 0
INTEGER                 :: rep_type_ship = 0
INTEGER                 :: rep_type_dribu_bathy = 0
INTEGER                 :: rep_type_dribu_tesac = 0
INTEGER                 :: rep_type_moored_buoy_bufr = 0
INTEGER                 :: rep_type_drifting_buoy_bufr = 0
INTEGER                 :: rep_type_ground_gps = 0
INTEGER                 :: rep_type_land_pilot = 0
INTEGER                 :: rep_type_ship_pilot = 0
INTEGER                 :: rep_type_american_wind_prof_1 = 0
INTEGER                 :: rep_type_ship_temp = 0
INTEGER                 :: rep_type_dropsond = 0
INTEGER                 :: rep_type_mobile_temp = 0
INTEGER                 :: rep_type_land_temp = 0
INTEGER                 :: rep_type_airep = 0
INTEGER                 :: rep_type_amdar = 0
INTEGER                 :: rep_type_mobile_pilot = 0
INTEGER                 :: rep_type_tamdar = 0
INTEGER                 :: rep_type_coriolis_windsat = 0
INTEGER                 :: rep_type_dmsp_16_ssmis_rad = 0
INTEGER                 :: rep_type_dmsp_17_ssmis_rad = 0
INTEGER                 :: rep_type_dmsp_18_ssmis_rad = 0
INTEGER                 :: rep_type_dmsp_19_ssmis_rad = 0
INTEGER                 :: rep_type_ground_based_radar = 0
INTEGER                 :: rep_type_fy3b = 0
INTEGER                 :: rep_type_mwts2_fy3c = 0
INTEGER                 :: rep_type_mwri_fy3c = 0
INTEGER                 :: rep_type_mwts2_fy3d = 0
INTEGER                 :: rep_type_mwri_fy3d = 0
INTEGER                 :: rep_type_snpp_atms = 0
INTEGER                 :: rep_type_noaa_20_atms = 0
INTEGER                 :: rep_type_snpp_cris = 0
INTEGER                 :: rep_type_noaa_20_cris = 0
INTEGER                 :: rep_type_envisat_wave = 0
INTEGER                 :: rep_type_ers_1_wave = 0
INTEGER                 :: rep_type_ers_2_wave = 0
INTEGER                 :: rep_type_jason_1_wave = 0
INTEGER                 :: rep_type_jason_2_wave = 0
INTEGER                 :: rep_type_jason_3_wave = 0
INTEGER                 :: rep_type_saral_wave = 0
INTEGER                 :: rep_type_sentinel_3a_wave = 0
INTEGER                 :: rep_type_cryosat_2_wave = 0
INTEGER                 :: rep_type_amsr2_rad = 0
INTEGER                 :: rep_type_saphir_rad = 0
INTEGER                 :: rep_type_noaa_15_amsub_rad = 0
INTEGER                 :: rep_type_noaa_16_amsub_rad = 0
INTEGER                 :: rep_type_noaa_17_amsub_rad = 0
INTEGER                 :: rep_type_noaa_18_amsub_rad = 0
INTEGER                 :: rep_type_mobile_land_synop = 0
INTEGER                 :: rep_type_bogus = 0
INTEGER                 :: rep_type_tcbogus = 0
INTEGER                 :: rep_type_bufr_land_temp = 0
INTEGER                 :: rep_type_bufr_ship_temp = 0
INTEGER                 :: rep_type_rainfall_accumulation_1h = 0
INTEGER                 :: rep_type_noaa_19_mhs_rad = 0
INTEGER                 :: rep_type_metop_a_mhs_rad = 0
INTEGER                 :: rep_type_metop_b_mhs_rad = 0
INTEGER                 :: rep_type_metop_c_mhs_rad = 0
INTEGER                 :: rep_type_doppler_wind_lidar = 0
INTEGER                 :: rep_type_ceilometer_lidar = 0
INTEGER                 :: rep_type_himawari_8_rad = 0
INTEGER                 :: rep_type_himawari_9_rad = 0
INTEGER                 :: rep_type_gmi_rad = 0
INTEGER                 :: rep_type_aladin_hloswind = 0
INTEGER                 :: rep_type_sentinel_3a_olci_tcwv = 0
INTEGER                 :: rep_type_sentinel_3b_olci_tcwv = 0
INTEGER                 :: rep_type_fy3_d_hiras_rad = 0
INTEGER                 :: rep_type_bufr_ship = 0

! UKMO local report types values (starting at 98000)
INTEGER                 :: rep_type_atovs_metop_a = 0
INTEGER                 :: rep_type_atovs_metop_b = 0
INTEGER                 :: rep_type_atovs_metop_c = 0
INTEGER                 :: rep_type_atovs_noaa_15 = 0
INTEGER                 :: rep_type_atovs_noaa_18 = 0
INTEGER                 :: rep_type_atovs_noaa_19 = 0
INTEGER                 :: rep_type_radar_scan = 0
INTEGER                 :: rep_type_metop_b_satsst = 0
INTEGER                 :: rep_type_metop_c_satsst = 0
INTEGER                 :: rep_type_meteosat_9_satsst = 0
INTEGER                 :: rep_type_noaa_18_satsst = 0
INTEGER                 :: rep_type_noaa_19_satsst = 0
INTEGER                 :: rep_type_goes_13_satsst = 0
INTEGER                 :: rep_type_trmm_satsst = 0
INTEGER                 :: rep_type_roadside_sensor = 0
INTEGER                 :: rep_type_meteosat_9_aod = 0
INTEGER                 :: rep_type_osseaice = 0
INTEGER                 :: rep_type_envisat_satsst = 0
INTEGER                 :: rep_type_ukpp_cloud = 0
INTEGER                 :: rep_type_kma_amv = 0
INTEGER                 :: rep_type_aqua_modis_aod = 0
INTEGER                 :: rep_type_terra_modis_aod = 0
INTEGER                 :: rep_type_imd_amv = 0
INTEGER                 :: rep_type_kma_geos_rad = 0
INTEGER                 :: rep_type_meteosat_10_aod = 0
INTEGER                 :: rep_type_msg3_satsst = 0
INTEGER                 :: rep_type_air_quality = 0
INTEGER                 :: rep_type_lightning_report = 0
INTEGER                 :: rep_type_ocean_wave = 0
INTEGER                 :: rep_type_atdnet = 0
INTEGER                 :: rep_type_wow = 0
INTEGER                 :: rep_type_envisat_radiance = 0
INTEGER                 :: rep_type_cimss_leogeo_amv = 0
INTEGER                 :: rep_type_dmspf11_satsst = 0
INTEGER                 :: rep_type_modes = 0
INTEGER                 :: rep_type_combined_metop_amv = 0
INTEGER                 :: rep_type_insat_3d_amv = 0
INTEGER                 :: rep_type_suomi_npp_amv = 0
INTEGER                 :: rep_type_insat_3d_imager = 0
INTEGER                 :: rep_type_insat_3d_sounder = 0
INTEGER                 :: rep_type_stereo_mv = 0
INTEGER                 :: rep_type_tovs = 0
INTEGER                 :: rep_type_gmi_rad_low = 0
INTEGER                 :: rep_type_gmi_rad_high = 0
INTEGER                 :: rep_type_oceancolour = 0
INTEGER                 :: rep_type_fy3d_cris = 0
INTEGER                 :: rep_type_oceanwinds = 0
INTEGER                 :: rep_type_moored_buoy = 0
INTEGER                 :: rep_type_spire_gpsro = 0
INTEGER                 :: rep_type_giirs_fy4a = 0 
INTEGER                 :: rep_type_emaddc = 0
INTEGER                 :: rep_type_goes_18_amv = 0
INTEGER                 :: rep_type_goes_19_amv = 0
INTEGER                 :: rep_type_noaa_21_amv = 0
INTEGER                 :: rep_type_fy2h_amv = 0
INTEGER                 :: rep_type_fy4a_amv = 0
INTEGER                 :: rep_type_fy4b_amv = 0
INTEGER                 :: rep_type_oceansat_3a_ascat = 0
INTEGER                 :: rep_type_hy2c_ascat = 0
INTEGER                 :: rep_type_fy3e_ascat = 0
INTEGER                 :: rep_type_insat_3dr_amv = 0
INTEGER                 :: rep_type_insat_3ds_amv = 0
INTEGER                 :: rep_type_geokompsat_2a_amv = 0

INTEGER                 :: group_id_hiras = 0
INTEGER                 :: group_id_hirs = 0
INTEGER                 :: group_id_amsua = 0
INTEGER                 :: group_id_amsub = 0
INTEGER                 :: group_id_mhs = 0
INTEGER                 :: group_id_geos = 0
INTEGER                 :: group_id_resat = 0
INTEGER                 :: group_id_meris = 0
INTEGER                 :: group_id_gpsro = 0
INTEGER                 :: group_id_satob = 0
INTEGER                 :: group_id_scatt = 0
INTEGER                 :: group_id_ssmi_all_sky = 0
INTEGER                 :: group_id_iasi = 0
INTEGER                 :: group_id_airs = 0
INTEGER                 :: group_id_ssmis_all_sky = 0
INTEGER                 :: group_id_tmi_all_sky = 0
INTEGER                 :: group_id_amsre_all_sky = 0
INTEGER                 :: group_id_conv = 0
INTEGER                 :: group_id_smos = 0
INTEGER                 :: group_id_windsat_all_sky = 0
INTEGER                 :: group_id_ssmi = 0
INTEGER                 :: group_id_amsua_all_sky = 0
INTEGER                 :: group_id_amsre = 0
INTEGER                 :: group_id_tmi = 0
INTEGER                 :: group_id_ssmis = 0
INTEGER                 :: group_id_gbrad = 0
INTEGER                 :: group_id_mwhs = 0
INTEGER                 :: group_id_mwts = 0
INTEGER                 :: group_id_mwri_all_sky = 0
INTEGER                 :: group_id_iras = 0
INTEGER                 :: group_id_msu = 0
INTEGER                 :: group_id_ssu = 0
INTEGER                 :: group_id_vtpr1 = 0
INTEGER                 :: group_id_vtpr2 = 0
INTEGER                 :: group_id_atms = 0
INTEGER                 :: group_id_resat_averaging_kernels = 0
INTEGER                 :: group_id_cris = 0
INTEGER                 :: group_id_wave_integrated_parameters = 0
INTEGER                 :: group_id_wave_spectra = 0
INTEGER                 :: group_id_raingg = 0
INTEGER                 :: group_id_surface_multisensor = 0
INTEGER                 :: group_id_amsr2_all_sky = 0
INTEGER                 :: group_id_saphir_all_sky = 0
INTEGER                 :: group_id_amsub_all_sky = 0
INTEGER                 :: group_id_mhs_all_sky = 0
INTEGER                 :: group_id_doppler_wind_lidar = 0
INTEGER                 :: group_id_iris = 0
INTEGER                 :: group_id_aatsr = 0
INTEGER                 :: group_id_atms_all_sky = 0
INTEGER                 :: group_id_gmi_all_sky = 0
INTEGER                 :: group_id_godae_sea_surface_temperature = 0
INTEGER                 :: group_id_atovs_multisensor = 0
INTEGER                 :: group_id_atmospheric_composition = 0
INTEGER                 :: group_id_non_surface_multisensor = 0
INTEGER                 :: group_id_mwts2 = 0
INTEGER                 :: group_id_ssmi_1dvar_tcwv_cloudy_sky = 0
INTEGER                 :: group_id_mwhs2 = 0
INTEGER                 :: group_id_ssmt2 = 0
INTEGER                 :: group_id_giirs = 0
INTEGER                 :: group_id_test = 0

CONTAINS

INCLUDE 'Ops_SetupODBGroupIds.inc'
INCLUDE 'Ops_SetupODBReportTypes.inc'
INCLUDE 'Ops_SetupODBVertcoTypes.inc'
INCLUDE 'OpsFn_ObsToReportType.inc'
INCLUDE 'OpsFn_ExtractDataToGroupId.inc'
INCLUDE 'OpsFn_ExtractDataToReportType.inc'

END MODULE OpsMod_ODBOPSInterface
