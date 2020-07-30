!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Function to return a hardwired value for various ODB parameters, needed when a
! real ODB session is not available.
!-------------------------------------------------------------------------------

FUNCTION ops_odb_getval_hardwired (string)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

IMPLICIT NONE

! Function arguments:
CHARACTER(len=*), INTENT(IN) :: string

! Function result:
INTEGER                      :: ops_odb_getval_hardwired

! Local declarations:
CHARACTER(len=*), PARAMETER  :: routine_name = "ops_odb_getval_hardwired"

IF (UseTrace) CALL gen_trace_entry (routine_name)

SELECT CASE (string)
  CASE ("$aerod")
    ops_odb_getval_hardwired = 174
  CASE ("$air_pressure")
    ops_odb_getval_hardwired = 236
  CASE ("$amb_wind_num")
    ops_odb_getval_hardwired = 12
  CASE ("$atmosphere_number")
    ops_odb_getval_hardwired = 263
  CASE ("$bend_angle")
    ops_odb_getval_hardwired = 162
  CASE ("$c")
    ops_odb_getval_hardwired = 69
  CASE ("$ch")
    ops_odb_getval_hardwired = 63
  CASE ("$cha_number")
    ops_odb_getval_hardwired = 7
  CASE ("$chl")
    ops_odb_getval_hardwired = 288
  CASE ("$cl")
    ops_odb_getval_hardwired = 65
  CASE ("$cllqw")
    ops_odb_getval_hardwired = 123
  CASE ("$cloud_clear")
    ops_odb_getval_hardwired = 232
  CASE ("$cloud_cover")
    ops_odb_getval_hardwired = 271
  CASE ("$cloud_frac_clear")
    ops_odb_getval_hardwired = 247
  CASE ("$cloud_frac_covered")
    ops_odb_getval_hardwired = 257
  CASE ("$cloud_ice_water")
    ops_odb_getval_hardwired = 260
  CASE ("$cloud_top_height")
    ops_odb_getval_hardwired = 276
  CASE ("$cloud_top_press")
    ops_odb_getval_hardwired = 235
  CASE ("$cloud_top_temp")
    ops_odb_getval_hardwired = 231
  CASE ("$cm")
    ops_odb_getval_hardwired = 64
  CASE ("$cod")
    ops_odb_getval_hardwired = 175
  CASE ("$cpt")
    ops_odb_getval_hardwired = 130
  CASE ("$dd")
    ops_odb_getval_hardwired = 111
  CASE ("$depth")
    ops_odb_getval_hardwired = 272
  CASE ("$derived_pressure")
    ops_odb_getval_hardwired = 11
  CASE ("$ff")
    ops_odb_getval_hardwired = 112
  CASE ("$forward_cha_number")
    ops_odb_getval_hardwired = 15
  CASE ("$hshs")
    ops_odb_getval_hardwired = 68
  CASE ("$hwhw")
    ops_odb_getval_hardwired = 84
  CASE ("$imp_param")
    ops_odb_getval_hardwired = 6
  CASE ("$impact_param")
    ops_odb_getval_hardwired = 240
  CASE ("$kd490")
    ops_odb_getval_hardwired = 289
  CASE ("$lane_number")
    ops_odb_getval_hardwired = 16
  CASE ("$level_cloud")
    ops_odb_getval_hardwired = 266
  CASE ("$level_mixing_ratio")
    ops_odb_getval_hardwired = 226
  CASE ("$libksc")
    ops_odb_getval_hardwired = 222
  CASE ("$lidar_cloud_backscatter")
    ops_odb_getval_hardwired = 237
  CASE ("$lidar_range")
    ops_odb_getval_hardwired = 16
  CASE ("$lightning")
    ops_odb_getval_hardwired = 265
  CASE ("$los")
    ops_odb_getval_hardwired = 187
  CASE ("$lower_layer_p")
    ops_odb_getval_hardwired = 269
  CASE ("$lwp")
    ops_odb_getval_hardwired = 244
  CASE ("$lwp_ret")
    ops_odb_getval_hardwired = 246
  CASE ("$mass_density")
    ops_odb_getval_hardwired = 262
  CASE ("$so2_density")
    ops_odb_getval_hardwired = 282
  CASE ("$pm10_density")
    ops_odb_getval_hardwired = 283
  CASE ("$pm2p5_density")
    ops_odb_getval_hardwired = 284
  CASE ("$o3_density")
    ops_odb_getval_hardwired = 285
  CASE ("$no2_density")
    ops_odb_getval_hardwired = 286
  CASE ("$co_density")
    ops_odb_getval_hardwired = 287
  CASE ("$max_wind_shear1")
    ops_odb_getval_hardwired = 219
  CASE ("$max_wind_shear2")
    ops_odb_getval_hardwired = 268
  CASE ("$mean_freq")
    ops_odb_getval_hardwired = 241
  CASE ("$model_pressure")
    ops_odb_getval_hardwired = 15
  CASE ("$modlevno")
    ops_odb_getval_hardwired = 5
  CASE ("$n")
    ops_odb_getval_hardwired = 91
  CASE ("$nn")
    ops_odb_getval_hardwired = 67
  CASE ("$ns")
    ops_odb_getval_hardwired = 70
  CASE ("$o3lay")
    ops_odb_getval_hardwired = 206
  CASE ("$ocean_depth")
    ops_odb_getval_hardwired = 10
  CASE ("$pmsl")
    ops_odb_getval_hardwired = 108
  CASE ("$prc")
    ops_odb_getval_hardwired = 202
  CASE ("$precipitation_amount_last_hour")
    ops_odb_getval_hardwired = 264
  CASE ("$pressure")
    ops_odb_getval_hardwired = 1
  CASE ("$ps")
    ops_odb_getval_hardwired = 110
  CASE ("$pstandard")
    ops_odb_getval_hardwired = 109
  CASE ("$pstation")
    ops_odb_getval_hardwired = 107
  CASE ("$ptend")
    ops_odb_getval_hardwired = 30
  CASE ("$q")
    ops_odb_getval_hardwired = 7
  CASE ("$radial_velocity")
    ops_odb_getval_hardwired = 259
  CASE ("$rainrate")
    ops_odb_getval_hardwired = 251
  CASE ("$rawbt")
    ops_odb_getval_hardwired = 119
  CASE ("$rawbt_airs")
    ops_odb_getval_hardwired = 234
  CASE ("$rawbt_amsr_89ghz")
    ops_odb_getval_hardwired = 267
  CASE ("$rawbt_amsu")
    ops_odb_getval_hardwired = 249
  CASE ("$rawbt_hirs")
    ops_odb_getval_hardwired = 248
  CASE ("$rawbt_hirs20")
    ops_odb_getval_hardwired = 250
  CASE ("$rawbt_mviri_1")
    ops_odb_getval_hardwired = 254
  CASE ("$rawbt_mviri_2")
    ops_odb_getval_hardwired = 255
  CASE ("$rawbt_mviri_3")
    ops_odb_getval_hardwired = 256
  CASE ("$rawbt_mwhs")
    ops_odb_getval_hardwired = 276
  CASE ("$rawbt_mwts")
    ops_odb_getval_hardwired = 275
  CASE ("$rawsca")
    ops_odb_getval_hardwired = 233
  CASE ("$refl")
    ops_odb_getval_hardwired = 192
  CASE ("$rh")
    ops_odb_getval_hardwired = 29
  CASE ("$rh2m")
    ops_odb_getval_hardwired = 58
  CASE ("$road_side_dd")
    ops_odb_getval_hardwired = 277
  CASE ("$road_side_ff")
    ops_odb_getval_hardwired = 278
  CASE ("$road_side_t2m")
    ops_odb_getval_hardwired = 279
  CASE ("$road_side_td2m")
    ops_odb_getval_hardwired = 280
  CASE ("$road_side_vv")
    ops_odb_getval_hardwired = 281
  CASE ("$rr")
    ops_odb_getval_hardwired = 80
  CASE ("$salinity")
    ops_odb_getval_hardwired = 224
  CASE ("$satcl")
    ops_odb_getval_hardwired = 121
  CASE ("$sea_ice")
    ops_odb_getval_hardwired = 253
  CASE ("$soln_lkhd")
    ops_odb_getval_hardwired = 228
  CASE ("$ssh")
    ops_odb_getval_hardwired = 273
  CASE ("$t")
    ops_odb_getval_hardwired = 2
  CASE ("$t2m")
    ops_odb_getval_hardwired = 39
  CASE ("$tangent_height")
    ops_odb_getval_hardwired = 14
  CASE ("$tcwv")
    ops_odb_getval_hardwired = 245
  CASE ("$td")
    ops_odb_getval_hardwired = 59
  CASE ("$td2m")
    ops_odb_getval_hardwired = 40
  CASE ("$theta")
    ops_odb_getval_hardwired = 246
  CASE ("$tot_zen_delay")
    ops_odb_getval_hardwired = 229
  CASE ("$tot_zen_delay_err")
    ops_odb_getval_hardwired = 230
  CASE ("$ts")
    ops_odb_getval_hardwired = 11
  CASE ("$tsts")
    ops_odb_getval_hardwired = 12
  CASE ("$u")
    ops_odb_getval_hardwired = 3
  CASE ("$u_amb")
    ops_odb_getval_hardwired = 242
  CASE ("$u10m")
    ops_odb_getval_hardwired = 41
  CASE ("$upper_layer_p")
    ops_odb_getval_hardwired = 270
  CASE ("$v")
    ops_odb_getval_hardwired = 4
  CASE ("$v_amb")
    ops_odb_getval_hardwired = 243
  CASE ("$v10m")
    ops_odb_getval_hardwired = 42
  CASE ("$vert_vv")
    ops_odb_getval_hardwired = 218
  CASE ("$vv")
    ops_odb_getval_hardwired = 62
  CASE ("$wind_gust")
    ops_odb_getval_hardwired = 261
  CASE ("$z")
    ops_odb_getval_hardwired = 1
  CASE ("$rep_type_noaa_15_amsub_rad")
    ops_odb_getval_hardwired = 2001
  CASE ("$rep_type_noaa_16_amsub_rad")
    ops_odb_getval_hardwired = 2002
  CASE ("$rep_type_noaa_17_amsub_rad")
    ops_odb_getval_hardwired = 2003
  CASE ("$rep_type_noaa_18_amsub_rad")
    ops_odb_getval_hardwired = 2004
  CASE ("$rep_type_noaa_19_mhs_rad")
    ops_odb_getval_hardwired = 3001
  CASE ("$rep_type_metop_a_mhs_rad")
    ops_odb_getval_hardwired = 3002
  CASE ("$rep_type_metop_b_mhs_rad")
    ops_odb_getval_hardwired = 3004
  CASE ("$rep_type_metop_c_mhs_rad")
    ops_odb_getval_hardwired = 98047
  CASE ("$rep_type_meteosat_7_geos_rad")
    ops_odb_getval_hardwired = 4007
  CASE ("$rep_type_meteosat_8_geos_rad")
    ops_odb_getval_hardwired = 4008
  CASE ("$rep_type_meteosat_9_geos_rad")
    ops_odb_getval_hardwired = 4009
  CASE ("$rep_type_mtsat_1r_geos_rad")
    ops_odb_getval_hardwired = 4010
  CASE ("$rep_type_goes_13_geos_rad")
    ops_odb_getval_hardwired = 4011
  CASE ("$rep_type_mtsat_2r_geos_rad")
    ops_odb_getval_hardwired = 4012
  CASE ("$rep_type_goes_14_geos_rad")
    ops_odb_getval_hardwired = 4014
  CASE ("$rep_type_goes_15_geos_rad")
    ops_odb_getval_hardwired = 4015
  CASE ("$rep_type_goes_16_geos_rad")
    ops_odb_getval_hardwired = 4025
  CASE ("$rep_type_meteosat_10_geos_rad")
    ops_odb_getval_hardwired = 4016
  CASE ("$rep_type_kma_geos_rad")
    ops_odb_getval_hardwired = 4019
  CASE ("$rep_type_himawari_8_rad")
    ops_odb_getval_hardwired = 4022
  CASE ("$rep_type_meteosat_11_geos_rad")
    ops_odb_getval_hardwired = 4023
  CASE ("$rep_type_himawari_9_rad")
    ops_odb_getval_hardwired = 4024
  CASE ("$rep_type_noaa_16_sbuv")
    ops_odb_getval_hardwired = 5009
  CASE ("$rep_type_noaa_17_sbuv")
    ops_odb_getval_hardwired = 5010
  CASE ("$rep_type_noaa_18_sbuv")
    ops_odb_getval_hardwired = 5011
  CASE ("$rep_type_noaa_19_sbuv")
    ops_odb_getval_hardwired = 5012
  CASE ("$rep_type_meteosat_9_aod")
    ops_odb_getval_hardwired = 5063
  CASE ("$rep_type_meteosat_10_aod")
    ops_odb_getval_hardwired = 5064
  CASE ("$rep_type_aqua_modis_aod")
    ops_odb_getval_hardwired = 5019
  CASE ("$rep_type_terra_modis_aod")
    ops_odb_getval_hardwired = 5020
  CASE ("$rep_type_meteosat_11_aod")
    ops_odb_getval_hardwired = 5080
  CASE ("$rep_type_envisat_meris_tcwv")
    ops_odb_getval_hardwired = 6001
  CASE ("$rep_type_sentinel_3a_olci_tcwv")
    ops_odb_getval_hardwired = 6002
  CASE ("$rep_type_sentinel_3b_olci_tcwv")
    ops_odb_getval_hardwired = 6003
  CASE ("$rep_type_metop_a_gpsro")
    ops_odb_getval_hardwired = 7001
  CASE ("$rep_type_champ_gpsro")
    ops_odb_getval_hardwired = 7002
  CASE ("$rep_type_grace_a_gpsro")
    ops_odb_getval_hardwired = 7003
  CASE ("$rep_type_cosmic_1_gpsro")
    ops_odb_getval_hardwired = 7004
  CASE ("$rep_type_cosmic_2_gpsro")
    ops_odb_getval_hardwired = 7005
  CASE ("$rep_type_cosmic_3_gpsro")
    ops_odb_getval_hardwired = 7006
  CASE ("$rep_type_cosmic_4_gpsro")
    ops_odb_getval_hardwired = 7007
  CASE ("$rep_type_cosmic_5_gpsro")
    ops_odb_getval_hardwired = 7008
  CASE ("$rep_type_cosmic_6_gpsro")
    ops_odb_getval_hardwired = 7009
  CASE ("$rep_type_terrasar_x_gpsro")
    ops_odb_getval_hardwired = 7010
  CASE ("$rep_type_cnofs_gpsro")
    ops_odb_getval_hardwired = 7011
  CASE ("$rep_type_sac_c_gpsro")
    ops_odb_getval_hardwired = 7012
  CASE ("$rep_type_metop_b_gpsro")
    ops_odb_getval_hardwired = 7013
  CASE ("$rep_type_grace_b_gpsro")
    ops_odb_getval_hardwired = 7014
  CASE ("$rep_type_tandem_x_gpsro")
    ops_odb_getval_hardwired = 7015
  CASE ("$rep_type_fy3c_gpsro")
    ops_odb_getval_hardwired = 7016
  CASE ("$rep_type_megha_tropiques_gpsro")
    ops_odb_getval_hardwired = 7017
  CASE ("$rep_type_kompsat_5_gpsro")
    ops_odb_getval_hardwired = 7018
  CASE ("$rep_type_fy3d_gpsro")
    ops_odb_getval_hardwired = 7022
  CASE ("$rep_type_paz_gpsro")
    ops_odb_getval_hardwired = 7023
  CASE ("$rep_type_cosmic2_e1_gpsro")
    ops_odb_getval_hardwired = 7024
  CASE ("$rep_type_cosmic2_e2_gpsro")
    ops_odb_getval_hardwired = 7025
  CASE ("$rep_type_cosmic2_e3_gpsro")
    ops_odb_getval_hardwired = 7026
  CASE ("$rep_type_cosmic2_e4_gpsro")
    ops_odb_getval_hardwired = 7027
  CASE ("$rep_type_cosmic2_e5_gpsro")
    ops_odb_getval_hardwired = 7028
  CASE ("$rep_type_cosmic2_e6_gpsro")
    ops_odb_getval_hardwired = 7029
  CASE ("$rep_type_spire_gpsro")
    ops_odb_getval_hardwired = 7030
  CASE ("$rep_type_metop_c_gpsro")
    ops_odb_getval_hardwired = 98043
  CASE ("$rep_type_meteosat_7_amv")
    ops_odb_getval_hardwired = 8006
  CASE ("$rep_type_meteosat_8_amv")
    ops_odb_getval_hardwired = 8007
  CASE ("$rep_type_meteosat_9_amv")
    ops_odb_getval_hardwired = 8008
  CASE ("$rep_type_mtsat_1r_amv")
    ops_odb_getval_hardwired = 8010
  CASE ("$rep_type_goes_11_amv")
    ops_odb_getval_hardwired = 8013
  CASE ("$rep_type_noaa_15_amv")
    ops_odb_getval_hardwired = 8015
  CASE ("$rep_type_noaa_16_amv")
    ops_odb_getval_hardwired = 8016
  CASE ("$rep_type_noaa_18_amv")
    ops_odb_getval_hardwired = 8018
  CASE ("$rep_type_noaa_19_amv")
    ops_odb_getval_hardwired = 8019
  CASE ("$rep_type_terra_modis_amv")
    ops_odb_getval_hardwired = 8020
  CASE ("$rep_type_aqua_modis_amv")
    ops_odb_getval_hardwired = 8021
  CASE ("$rep_type_fy2c_amv")
    ops_odb_getval_hardwired = 8022
  CASE ("$rep_type_fy2d_amv")
    ops_odb_getval_hardwired = 8023
  CASE ("$rep_type_fy2e_amv")
    ops_odb_getval_hardwired = 8024
  CASE ("$rep_type_goes_13_amv")
    ops_odb_getval_hardwired = 8025
  CASE ("$rep_type_mtsat_2_amv")
    ops_odb_getval_hardwired = 8026
  CASE ("$rep_type_metop_a_amv")
    ops_odb_getval_hardwired = 8027
  CASE ("$rep_type_goes_14_amv")
    ops_odb_getval_hardwired = 8028
  CASE ("$rep_type_goes_15_amv")
    ops_odb_getval_hardwired = 8029
  CASE ("$rep_type_metop_b_amv")
    ops_odb_getval_hardwired = 8030
  CASE ("$rep_type_metop_c_amv")
    ops_odb_getval_hardwired = 98044
  CASE ("$rep_type_meteosat_10_amv")
    ops_odb_getval_hardwired = 8031
  CASE ("$rep_type_kma_amv")
    ops_odb_getval_hardwired = 8045
  CASE ("$rep_type_imd_amv")
    ops_odb_getval_hardwired = 8046
  CASE ("$rep_type_cimss_leogeo_amv")
    ops_odb_getval_hardwired = 8047
  CASE ("$rep_type_combined_metop_amv")
    ops_odb_getval_hardwired = 8048
  CASE ("$rep_type_insat_3d_amv")
    ops_odb_getval_hardwired = 8049
  CASE ("$rep_type_insat_3dr_amv")
    ops_odb_getval_hardwired = 98064
  CASE ("$rep_type_insat_3ds_amv")
    ops_odb_getval_hardwired = 98065
  CASE ("$rep_type_suomi_npp_amv")
    ops_odb_getval_hardwired = 8050
  CASE ("$rep_type_himawari_8_amv")
    ops_odb_getval_hardwired = 8053
  CASE ("$rep_type_fy2g_amv")
    ops_odb_getval_hardwired = 8054
  CASE ("$rep_type_fy2h_amv")
    ops_odb_getval_hardwired = 98058
  CASE ("$rep_type_fy4a_amv")
    ops_odb_getval_hardwired = 98059
  CASE ("$rep_type_fy4b_amv")
    ops_odb_getval_hardwired = 98060
  CASE ("$rep_type_stereo_mv")
    ops_odb_getval_hardwired = 8055
  CASE ("$rep_type_meteosat_11_amv")
    ops_odb_getval_hardwired = 8056
  CASE ("$rep_type_goes_16_amv")
    ops_odb_getval_hardwired = 8057
  CASE ("$rep_type_himawari_9_amv")
    ops_odb_getval_hardwired = 8058
  CASE ("$rep_type_noaa_20_amv")
    ops_odb_getval_hardwired = 8060
  CASE ("$rep_type_noaa_21_amv")
    ops_odb_getval_hardwired = 98057
  CASE ("$rep_type_goes_17_amv")
    ops_odb_getval_hardwired = 8061
  CASE ("$rep_type_goes_18_amv")
    ops_odb_getval_hardwired = 98055
  CASE ("$rep_type_goes_19_amv")
    ops_odb_getval_hardwired = 98056
  CASE ("$rep_type_sentinel_3a_amv")
    ops_odb_getval_hardwired = 8062
  CASE ("$rep_type_sentinel_3b_amv")
    ops_odb_getval_hardwired = 8063
  CASE ("$rep_type_geokompsat_2a_amv")
    ops_odb_getval_hardwired = 98066
  CASE ("$rep_type_metop_a_ascat")
    ops_odb_getval_hardwired = 9005
  CASE ("$rep_type_oceansat_2_ascat")
    ops_odb_getval_hardwired = 9006
  CASE ("$rep_type_metop_b_ascat")
    ops_odb_getval_hardwired = 9007
  CASE ("$rep_type_iss_ascat")
    ops_odb_getval_hardwired = 9008
  CASE ("$rep_type_scatsat1_ascat")
    ops_odb_getval_hardwired = 9010
  CASE ("$rep_type_oceansat_3_ascat")
    ops_odb_getval_hardwired = 9012
  CASE ("$rep_type_oceansat_3a_ascat")
    ops_odb_getval_hardwired = 98061
  CASE ("$rep_type_fy3e_ascat")
    ops_odb_getval_hardwired = 98063
  CASE ("$rep_type_hy2b_ascat")
    ops_odb_getval_hardwired = 9013
  CASE ("$rep_type_hy2c_ascat")
    ops_odb_getval_hardwired = 98062
  CASE ("$rep_type_cfosat_ascat")
    ops_odb_getval_hardwired = 9014
  CASE ("$rep_type_metop_c_ascat")
    ops_odb_getval_hardwired = 98045
  CASE ("$rep_type_metop_a_iasi_rad")
    ops_odb_getval_hardwired = 11001
  CASE ("$rep_type_metop_b_iasi_rad")
    ops_odb_getval_hardwired = 11002
  CASE ("$rep_type_metop_c_iasi_rad")
    ops_odb_getval_hardwired = 98046
  CASE ("$rep_type_aqua_airs_rad")
    ops_odb_getval_hardwired = 12001
  CASE ("$rep_type_auto_land_synop")
    ops_odb_getval_hardwired = 16001
  CASE ("$rep_type_manual_land_synop")
    ops_odb_getval_hardwired = 16002
  CASE ("$rep_type_metar")
    ops_odb_getval_hardwired = 16004
  CASE ("$rep_type_dribu")
    ops_odb_getval_hardwired = 16005
  CASE ("$rep_type_auto_ship")
    ops_odb_getval_hardwired = 16006
  CASE ("$rep_type_ship")
    ops_odb_getval_hardwired = 16008
  CASE ("$rep_type_dribu_bathy")
    ops_odb_getval_hardwired = 16010
  CASE ("$rep_type_dribu_tesac")
    ops_odb_getval_hardwired = 16011
  CASE ("$rep_type_ground_gps")
    ops_odb_getval_hardwired = 16012
  CASE ("$rep_type_land_pilot")
    ops_odb_getval_hardwired = 16013
  CASE ("$rep_type_ship_pilot")
    ops_odb_getval_hardwired = 16014
  CASE ("$rep_type_american_wind_prof_1")
    ops_odb_getval_hardwired = 16015
  CASE ("$rep_type_ship_temp")
    ops_odb_getval_hardwired = 16019
  CASE ("$rep_type_dropsond")
    ops_odb_getval_hardwired = 16020
  CASE ("$rep_type_mobile_temp")
    ops_odb_getval_hardwired = 16021
  CASE ("$rep_type_land_temp")
    ops_odb_getval_hardwired = 16022
  CASE ("$rep_type_airep")
    ops_odb_getval_hardwired = 16026
  CASE ("$rep_type_amdar")
    ops_odb_getval_hardwired = 16029
  CASE ("$rep_type_bufr_land_temp")
    ops_odb_getval_hardwired = 16045
  CASE ("$rep_type_bufr_ship_temp")
    ops_odb_getval_hardwired = 16046
  CASE ("$rep_type_mobile_land_synop")
    ops_odb_getval_hardwired = 16058
  CASE ("$rep_type_bogus")
    ops_odb_getval_hardwired = 16059
  CASE ("$rep_type_tcbogus")
    ops_odb_getval_hardwired = 16060
  CASE ("$rep_type_tamdar")
    ops_odb_getval_hardwired = 16066
  CASE ("$rep_type_bufr_land_pilot")
    ops_odb_getval_hardwired = 16068
  CASE ("$rep_type_bufr_ship_pilot")
    ops_odb_getval_hardwired = 16069
  CASE ("$rep_type_bufr_land_synop_old")
    ops_odb_getval_hardwired = 16070
  CASE ("$rep_type_bufr_land_synop_2")
    ops_odb_getval_hardwired = 16071
  CASE ("$rep_type_bufr_land_synop_region_6")
    ops_odb_getval_hardwired = 16072
  CASE ("$rep_type_bufr_land_synop_1_hourly")
    ops_odb_getval_hardwired = 16073
  CASE ("$rep_type_bufr_ship")
    ops_odb_getval_hardwired = 16074
  CASE ("$rep_type_mobile_pilot")
    ops_odb_getval_hardwired = 16075
  CASE ("$rep_type_bufr_land_synop")
    ops_odb_getval_hardwired = 16076
  CASE ("$rep_type_roadside_sensor")
    ops_odb_getval_hardwired = 16077
  CASE ("$rep_type_wow")
    ops_odb_getval_hardwired = 16078
  CASE ("$rep_type_lightning_report")
    ops_odb_getval_hardwired = 16079
  CASE ("$rep_type_atdnet")
    ops_odb_getval_hardwired = 16080
  CASE ("$rep_type_moored_buoy_bufr")
    ops_odb_getval_hardwired = 16083
  CASE ("$rep_type_drifting_buoy_bufr")
    ops_odb_getval_hardwired = 16084
  CASE ("$rep_type_ceilometer_lidar")
    ops_odb_getval_hardwired = 16085
  CASE ("$rep_type_coriolis_windsat")
    ops_odb_getval_hardwired = 19001
  CASE ("$rep_type_dmsp_16_ssmis_rad")
    ops_odb_getval_hardwired = 24001
  CASE ("$rep_type_dmsp_17_ssmis_rad")
    ops_odb_getval_hardwired = 24002
  CASE ("$rep_type_dmsp_18_ssmis_rad")
    ops_odb_getval_hardwired = 24003
  CASE ("$rep_type_dmsp_19_ssmis_rad")
    ops_odb_getval_hardwired = 24004
  CASE ("$rep_type_ground_based_radar")
    ops_odb_getval_hardwired = 25001
  CASE ("$rep_type_fy3b")
    ops_odb_getval_hardwired = 26002
  CASE ("$rep_type_mwts2_fy3c")
    ops_odb_getval_hardwired = 55001
  CASE ("$rep_type_mwts2_fy3d")
    ops_odb_getval_hardwired = 55002
  CASE ("$rep_type_mwri_fy3c")
    ops_odb_getval_hardwired = 28003
  CASE ("$rep_type_mwri_fy3d")
    ops_odb_getval_hardwired = 28004
  CASE ("$rep_type_snpp_atms")
    ops_odb_getval_hardwired = 34001
  CASE ("$rep_type_noaa_20_atms")
    ops_odb_getval_hardwired = 34002
  CASE ("$rep_type_snpp_cris")
    ops_odb_getval_hardwired = 36001
  CASE ("$rep_type_noaa_20_cris")
    ops_odb_getval_hardwired = 36002
  CASE ("$rep_type_envisat_wave")
    ops_odb_getval_hardwired = 37001
  CASE ("$rep_type_jason_1_wave")
    ops_odb_getval_hardwired = 37002
  CASE ("$rep_type_jason_2_wave")
    ops_odb_getval_hardwired = 37003
  CASE ("$rep_type_ers_1_wave")
    ops_odb_getval_hardwired = 37004
  CASE ("$rep_type_ers_2_wave")
    ops_odb_getval_hardwired = 37005
  CASE ("$rep_type_cryosat_2_wave")
    ops_odb_getval_hardwired = 37006
  CASE ("$rep_type_saral_wave")
    ops_odb_getval_hardwired = 37007
  CASE ("$rep_type_jason_3_wave")
    ops_odb_getval_hardwired = 37008
  CASE ("$rep_type_sentinel_3a_wave")
    ops_odb_getval_hardwired = 37009
  CASE ("$rep_type_rainfall_accumulation_1h")
    ops_odb_getval_hardwired = 39001
  CASE ("$rep_type_amsr2_rad")
    ops_odb_getval_hardwired = 41001
  CASE ("$rep_type_saphir_rad")
    ops_odb_getval_hardwired = 42001
  CASE ("$rep_type_doppler_wind_lidar")
    ops_odb_getval_hardwired = 45001
  CASE ("$rep_type_envisat_radiance")
    ops_odb_getval_hardwired = 48001
  CASE ("$rep_type_gmi_rad")
    ops_odb_getval_hardwired = 50001 
  CASE ("$rep_type_metop_b_satsst")
    ops_odb_getval_hardwired = 51001
  CASE ("$rep_type_meteosat_9_satsst")
    ops_odb_getval_hardwired = 51002
  CASE ("$rep_type_noaa_18_satsst")
    ops_odb_getval_hardwired = 51003
  CASE ("$rep_type_noaa_19_satsst")
    ops_odb_getval_hardwired = 51004
  CASE ("$rep_type_goes_13_satsst")
    ops_odb_getval_hardwired = 51005
  CASE ("$rep_type_trmm_satsst")
    ops_odb_getval_hardwired = 51006
  CASE ("$rep_type_envisat_satsst")
    ops_odb_getval_hardwired = 51007
  CASE ("$rep_type_msg3_satsst")
    ops_odb_getval_hardwired = 51008
  CASE ("$rep_type_dmspf11_satsst")
    ops_odb_getval_hardwired = 51009
  CASE ("$rep_type_metop_c_satsst")
    ops_odb_getval_hardwired = 98042
  CASE ("$rep_type_atovs_metop_a")
    ops_odb_getval_hardwired = 52002
  CASE ("$rep_type_atovs_metop_b")
    ops_odb_getval_hardwired = 52001
  CASE ("$rep_type_atovs_metop_c")
    ops_odb_getval_hardwired = 98048
  CASE ("$rep_type_atovs_noaa_15")
    ops_odb_getval_hardwired = 52003
  CASE ("$rep_type_atovs_noaa_18")
    ops_odb_getval_hardwired = 52004
  CASE ("$rep_type_atovs_noaa_19")
    ops_odb_getval_hardwired = 52005
  CASE ("$rep_type_air_quality")
    ops_odb_getval_hardwired = 53001
  CASE ("$rep_type_ukpp_cloud")
    ops_odb_getval_hardwired = 54001
  CASE ("$rep_type_tovs")
    ops_odb_getval_hardwired = 60001
  CASE ("$rep_type_modes")
    ops_odb_getval_hardwired = 98001
  CASE ("$rep_type_radar_scan")
    ops_odb_getval_hardwired = 98002
  CASE ("$rep_type_osseaice")
    ops_odb_getval_hardwired = 98016
  CASE ("$rep_type_oceancolour")
    ops_odb_getval_hardwired = 98031
  CASE ("$rep_type_ocean_wave")
    ops_odb_getval_hardwired = 98032
  CASE ("$rep_type_insat_3d_imager")
    ops_odb_getval_hardwired = 98036
  CASE ("$rep_type_insat_3d_sounder")
    ops_odb_getval_hardwired = 98037
  CASE ("$rep_type_oceanwinds")
    ops_odb_getval_hardwired = 98038
  CASE ("$rep_type_gmi_rad_low")
    ops_odb_getval_hardwired = 98039
  CASE ("$rep_type_gmi_rad_high")
    ops_odb_getval_hardwired = 98040
  CASE ("$rep_type_aladin_hloswind")
    ops_odb_getval_hardwired = 98041
  CASE ("$rep_type_goes_17_geos_rad")
    ops_odb_getval_hardwired = 98049
  CASE ("$rep_type_fy3d_cris")
    ops_odb_getval_hardwired = 98050
  CASE ("$rep_type_fy3_d_hiras_rad")
    ops_odb_getval_hardwired = 98051
  CASE ("$rep_type_moored_buoy")
    ops_odb_getval_hardwired = 98052
  CASE ("$rep_type_emaddc")
    ops_odb_getval_hardwired = 98053
  CASE DEFAULT
    CALL gen_fail (routine_name,                        &
                   "Unknown string: " // TRIM (string))
END SELECT

IF (UseTrace) CALL gen_trace_exit (routine_name)

END FUNCTION ops_odb_getval_hardwired
