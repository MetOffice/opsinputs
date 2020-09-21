!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! STASH codes used in OPS and VAR.
!
! These are based on the central UM STASHmaster files. See:
!     URL http://www-nwp/umdoc/pages/stashtech.html
!
! If adding a new item here ensure that it is reserved in the central UM
! Stashmaster where appropriate:
!     URL http://fcm2/projects/UM/wiki/ReservedCodes
!
! Note: This procedure has not always been followed and has led to some
!       conflicts between the items here and the official UM items.
!-------------------------------------------------------------------------------

MODULE OpsMod_Stash

IMPLICIT NONE

SAVE

! Public declarations:

! Codes matching UM STASH section 0

INTEGER, PARAMETER :: StashItem_pstar = 1            ! pstar (old dynamics)
INTEGER, PARAMETER :: StashItem_u = 2                ! u wind
INTEGER, PARAMETER :: StashItem_v = 3                ! v wind
INTEGER, PARAMETER :: StashItem_theta = 4            ! Potential temperature
INTEGER, PARAMETER :: StashItem_q = 10               ! Specific humidity
INTEGER, PARAMETER :: StashItem_qcf = 12             ! Cloud ice fraction
INTEGER, PARAMETER :: StashItem_ConvCloudAmount = 13 ! Convective cloud amount
INTEGER, PARAMETER :: StashItem_ConvCloudBase = 14   ! Convective cloud base level
INTEGER, PARAMETER :: StashItem_ConvCloudTop = 15    ! Convective cloud top level
INTEGER, PARAMETER :: StashItem_SnowAmount = 23      ! Snow amount kg/m2
INTEGER, PARAMETER :: StashItem_SST = 24             ! Surface temperature
INTEGER, PARAMETER :: StashCode_Tstar = 24           ! Surface temperature (land and ice)
INTEGER, PARAMETER :: StashItem_BLHeight = 25        ! Boundary Layer Height
INTEGER, PARAMETER :: StashItem_ModelSurface = 30    ! Land Mask
INTEGER, PARAMETER :: StashItem_SeaIce = 31          ! Sea Ice fraction
INTEGER, PARAMETER :: StashItem_orog = 33            ! Orography
INTEGER, PARAMETER :: StashCode_SMCsat = 43          ! Volumetric soil moisture content at saturation
INTEGER, PARAMETER :: StashCode_SnowDensity = 377    ! Snowpack bulk density
INTEGER, PARAMETER :: StashItem_ozone_old = 61       ! Ozone (tracer) (up toUM6.0)
INTEGER, PARAMETER :: StashItem_ozone_new = 480      ! Ozone (tracer) (UM6.1+)
INTEGER            :: StashItem_ozone

INTEGER, PARAMETER :: StashItem_dust1 = 431          ! Dust MMR - size bin 1
INTEGER, PARAMETER :: StashItem_dust2 = 432          ! Dust MMR - size bin 2
INTEGER, PARAMETER :: StashItem_dust3 = 433          ! Dust MMR - size bin 3
INTEGER, PARAMETER :: StashItem_dust4 = 434          ! Dust MMR - size bin 4
INTEGER, PARAMETER :: StashItem_dust5 = 435          ! Dust MMR - size bin 5
INTEGER, PARAMETER :: StashItem_dust6 = 436          ! Dust MMR - size bin 6
INTEGER, PARAMETER :: StashItem_dustt = 17257        ! Total dust MMR
INTEGER, PARAMETER :: StashItem_dustMin = 431
INTEGER, PARAMETER :: StashItem_dustMax = 436

INTEGER, PARAMETER :: StashItem_aerosol = 90         ! Total Aerosol (for Vis)
INTEGER, PARAMETER :: StashCode_SO2_AQ = 101       ! Sulphur dioxide mixing
                                                     ! ratio for AQCOPS model

INTEGER, PARAMETER :: StashItem_w = 150              ! w wind
INTEGER, PARAMETER :: StashItem_p_OD = 151           ! Pressure
INTEGER, PARAMETER :: StashItem_rho_OD = 152         ! Density*r_p*r_p (old dynamics)
INTEGER, PARAMETER :: StashItem_u_OD = 153           ! u wind  (OD but on)
INTEGER, PARAMETER :: StashItem_v_OD = 154           ! v wind  (ND grid)
INTEGER, PARAMETER :: StashItem_logm_OD = 155        ! log aerosol (
INTEGER, PARAMETER :: StashItem_rho = 253            ! Density*r_p*r_p, rho levels
INTEGER, PARAMETER :: StashItem_qcl = 254            ! Cloud liquid water
INTEGER, PARAMETER :: StashItem_exner = 255          ! Exner pressure
INTEGER, PARAMETER :: StashItem_Cl = 267             ! Liquid cloud fraction
INTEGER, PARAMETER :: StashItem_Cf = 268             ! Frozen cloud fraction
INTEGER, PARAMETER :: StashItem_cloud_bulk = 266     ! Bulk cloud fraction
INTEGER, PARAMETER :: StashItem_qrain = 272          ! Rain amount symbol`qR', code variable `qrain', units kg kg-1.
INTEGER, PARAMETER :: StashItem_mflx_A = 5246        ! convective mass flux part
INTEGER, PARAMETER :: StashItem_mflx_I = 5247        ! convective mass flux part
INTEGER, PARAMETER :: StashItem_mflx = 5248          ! midlevel convective mass flux as appropr. for subsidence calc.
INTEGER, PARAMETER :: StashItem_mflx_sub = 5295      ! convective mass flux part
INTEGER, PARAMETER :: StashItem_th_conv = 5187       ! LS convection increments of theta
INTEGER, PARAMETER :: StashItem_q_conv = 5188        ! LS convection increments of q
!
INTEGER, PARAMETER :: StashItem_u_conv = 5185        ! LS convection increments of u
INTEGER, PARAMETER :: StashItem_v_conv = 5186        ! LS convection increments of v
INTEGER, PARAMETER :: StashItem_ntml = 259           ! Top of mixed layer(used for conv.param.)
INTEGER, PARAMETER :: StashItem_shallowc = 5270      ! Shallow convection indicator
INTEGER, PARAMETER :: StashItem_exner_bar = 406      ! Exner P on theta levels
INTEGER, PARAMETER :: StashItem_p = 407              ! Pressure on exner levels
INTEGER, PARAMETER :: StashItem_p_bar = 408          ! Pressure at theta levels
INTEGER, PARAMETER :: StashItem_p_surface = 409      ! Surface pressure

! VAR specific codes
INTEGER, PARAMETER :: StashItem_Gp = 450             ! The geostrophic P (PF only)
INTEGER, PARAMETER :: StashItem_rhoY = 451           ! r2 * dry density
INTEGER, PARAMETER :: StashItem_rhoY_bar = 452       ! rhoY at theta levels
INTEGER, PARAMETER :: StashItem_rho_bar = 453        ! rho at theta levels
INTEGER, PARAMETER :: StashItem_Hexner = 454         ! Hydrostatic exner pressure
INTEGER, PARAMETER :: StashItem_q_bar = 455          ! Total cloud water at P-lev
INTEGER, PARAMETER :: StashItem_qc = 456             ! Cloud water content
INTEGER, PARAMETER :: StashItem_rh = 457             ! Relative humidity
INTEGER, PARAMETER :: StashItem_Iw = 458             ! Incompressible vert velocity
INTEGER, PARAMETER :: StashItem_Vtheta = 459         ! Virtual potential temp.
INTEGER, PARAMETER :: StashItem_Vtheta_bar = 460     ! Virtual potential temp. at pressure levels.
INTEGER, PARAMETER :: StashItem_logm = 461
INTEGER, PARAMETER :: StashItem_r_p = 462            ! Ht of p points at p-levs.
INTEGER, PARAMETER :: StashItem_r_theta = 463        ! Ht of p points theta-levs (including the surface).
INTEGER, PARAMETER :: StashItem_r_psi = 464          ! Ht at psi points on p-levs
INTEGER, PARAMETER :: StashItem_r_u = 465            ! Ht at u points
INTEGER, PARAMETER :: StashItem_r_v = 466            ! Ht at v points
INTEGER, PARAMETER :: StashItem_r_u_theta = 467      ! Ht at u pnts on theta levels (including sfc)
INTEGER, PARAMETER :: StashItem_r_v_theta = 468      ! Ht at v pnts on theta levels (incl surface)
INTEGER, PARAMETER :: StashItem_Aw = 469             ! Compressible vert velocity
INTEGER, PARAMETER :: StashItem_BLcoeff1 = 470       ! was 3281
INTEGER, PARAMETER :: StashItem_BLcoeff2 = 471       ! was 3282
INTEGER, PARAMETER :: StashItem_TauX = 472           ! X-comp of boundary layer stress term. (PF only). (At u pnts on theta levs)
INTEGER, PARAMETER :: StashItem_TauY = 473           ! Y-comp of boundary layer stress term. (PF only)  (At v pnts on theta levs)
INTEGER, PARAMETER :: StashItem_Hp = 474             ! The hydrostatic pressure
INTEGER, PARAMETER :: StashItem_RHtotal = 475        ! Total relative humidity = q+ql+qf / qsat(Tl)
INTEGER, PARAMETER :: StashItem_pstar_tend = 476     ! pstar tendency
INTEGER, PARAMETER :: StashItem_div_tend = 477       ! divergence tendency
INTEGER, PARAMETER :: StashItem_PV = 478             ! Potential vorticity

INTEGER, PARAMETER :: StashItem_psi = 500            ! Stream function
INTEGER, PARAMETER :: StashItem_chi = 501            ! Velocity potential
INTEGER, PARAMETER :: StashItem_Ap = 502             ! Geostrophically unbalanced part of hydrostatic pressure
INTEGER, PARAMETER :: StashItem_mu = 503             ! Humidity variable
INTEGER, PARAMETER :: StashItem_alpha = 504          ! EOTD Control Variable
INTEGER, PARAMETER :: StashItem_qT_bar = 505         ! Total specific humidity at P-levels
INTEGER, PARAMETER :: StashItem_BCcoeffs = 506       ! VarBC coeffs
INTEGER, PARAMETER :: StashItem_psi_BL = 509         ! Unbalanced boundary layer Stream function
INTEGER, PARAMETER :: StashItem_chi_BL = 510         ! Unbalanced boundary layer Velocity potential
INTEGER, PARAMETER :: StashItem_Ap_BL = 511          ! Unbalanced boundary layer pressure

INTEGER, PARAMETER :: StashItem_psi_bal = 512        ! Balanced boundary layer Stream function
INTEGER, PARAMETER :: StashItem_chi_bal = 513        ! balanced boundary layer velocity potential
INTEGER, PARAMETER :: StashItem_psi_b  = 514         ! Balanced streamfunction above boundary layer
INTEGER, PARAMETER :: StashItem_psi_ub = 515         ! Unbalanced streamfunction above boundary layer

! Additional codes required by Var_PFdiags:
INTEGER, PARAMETER :: StashItem_u_div = 600          ! Divergent u
INTEGER, PARAMETER :: StashItem_v_div = 601          ! Divergent v
INTEGER, PARAMETER :: StashItem_u_nondiv = 602       ! Non-divergent u
INTEGER, PARAMETER :: StashItem_v_nondiv = 603       ! Non-divergent v
INTEGER, PARAMETER :: StashItem_vort = 604           ! Vorticity
INTEGER, PARAMETER :: StashItem_div = 605            ! Divergence
INTEGER, PARAMETER :: StashItem_PE_A = 606           ! Thermobaric energy
INTEGER, PARAMETER :: StashItem_PE_E = 607           ! Elastic energy
INTEGER, PARAMETER :: StashItem_KE_tot = 608         ! Total KE
INTEGER, PARAMETER :: StashItem_KE_div = 609         ! Divergent KE
INTEGER, PARAMETER :: StashItem_KE_nondiv = 610      ! Non-divergent KE
INTEGER, PARAMETER :: StashItem_E_tot = 611          ! Total energy
INTEGER, PARAMETER :: StashItem_WtChiSq = 612        ! Weighted chi^2

! Non-section 0 parameters are StashCodes and include the section no.
! Not all these are present in STASHmaster_A files.
INTEGER, PARAMETER :: StashCode_pmsl = 16222
INTEGER, PARAMETER :: StashCode_theta2 = 9999        ! unsure
INTEGER, PARAMETER :: StashCode_rh2 = 3245           ! relative humidity at 1.5 m
INTEGER, PARAMETER :: StashCode_u10 = 3209
INTEGER, PARAMETER :: StashCode_v10 = 3210
INTEGER, PARAMETER :: StashCode_u10_B_grid = 3225    ! \ For BGE creation
INTEGER, PARAMETER :: StashCode_v10_B_grid = 3226    ! / as well as OD
INTEGER, PARAMETER :: StashCode_vis = 3247
INTEGER, PARAMETER :: StashCode_t2 = 3236
INTEGER, PARAMETER :: StashCode_WIND_SPED = 6017
INTEGER, PARAMETER :: StashCode_WAVE_HGHT = 6001
INTEGER, PARAMETER :: StashCode_qt2 = 3255
INTEGER, PARAMETER :: StashCode_ObukhovLength = 3464 ! Obukhov length
INTEGER, PARAMETER :: StashCode_FrictionVel = 3465   ! Friction velocity
INTEGER, PARAMETER :: StashCode_BLtype = 3476   ! Boundary layer type
INTEGER, PARAMETER :: StashCode_SurfRainRate_LS = 4203
INTEGER, PARAMETER :: StashCode_SurfSnowRate_LS = 4204
INTEGER, PARAMETER :: StashCode_ql_layer = 4205      ! layer cloud ql
INTEGER, PARAMETER :: StashCode_qf_layer = 4206      ! layer cloud qf
INTEGER, PARAMETER :: StashCode_rh = 4207
INTEGER, PARAMETER :: StashCode_CDNC = 4210          ! Cloud Droplet Number Concentration
INTEGER, PARAMETER :: StashCode_RainRate_layer = 4222
INTEGER, PARAMETER :: StashCode_SurfRainRate_conv = 5205
INTEGER, PARAMETER :: StashCode_SurfSnowRate_conv = 5206
INTEGER, PARAMETER :: StashCode_cloud_conv = 5212
INTEGER, PARAMETER :: StashCode_up_mflx = 5250       ! updraft convective mass flux
INTEGER, PARAMETER :: StashCode_cloud_layer = 9201
INTEGER, PARAMETER :: StashCode_qc_conv = 5213       ! convective ql + qf
INTEGER, PARAMETER :: StashCode_CloudAmount = 9217   ! total
INTEGER, PARAMETER :: StashCode_u_p_B_grid = 15201   ! For BGE creation
INTEGER, PARAMETER :: StashCode_T = 16004            ! Temperature on theta levels, code variable `T', units K.
INTEGER, PARAMETER :: StashCode_t_p = 16203
INTEGER, PARAMETER :: StashCode_rh_p = 16204
INTEGER, PARAMETER :: StashCode_ozone_p = 16226      ! ozone (p levs)
INTEGER, PARAMETER :: StashCode_qT = 16207

INTEGER, PARAMETER :: StashCode_PM10_AQ = 17220  ! stash code for particulate
                                                   ! matter (size 10) for AQCOPS
                                                   ! model
INTEGER, PARAMETER :: StashCode_PM2p5_AQ = 17221 ! stash code for particulate
                                                   ! matter (size 2.5) for AQCOPS
                                                   ! model

INTEGER, PARAMETER :: StashCode_qT_old = 18001 ! Deprecated code
INTEGER, PARAMETER :: StashCode_PSurfParamA = 20000
INTEGER, PARAMETER :: StashCode_PSurfParamB = 20001
INTEGER, PARAMETER :: StashCode_LapseRate = 20002
INTEGER, PARAMETER :: StashCode_SWradiation = 1235
INTEGER, PARAMETER :: StashCode_RichNumber = 3208
INTEGER, PARAMETER :: StashCode_PrecipAcc6hr = 5226
INTEGER, PARAMETER :: StashCode_SoilMoisture = 8223
INTEGER, PARAMETER :: StashCode_SoilTemp = 8225
INTEGER, PARAMETER :: StashCode_LowCloudAmount = 9203
INTEGER, PARAMETER :: StashCode_MedCloudAmount = 9204
INTEGER, PARAMETER :: StashCode_LowCloudBase = 9219
INTEGER, PARAMETER :: StashCode_2p5CloudBase = 9210  ! base for cover > 2.5 okta
INTEGER, PARAMETER :: StashCode_RH_AfterMainCloud = 9229  ! relative humidity after main cloud

INTEGER, PARAMETER :: StashCode_O3_AQ  = 34001 ! stash code for ozone mixing
                                                 ! ratio in AQCOPS model
INTEGER, PARAMETER :: StashCode_NO2_AQ = 34004 ! stash code for nitrogen
                                                 ! dioxide mixing ratio in AQCOPS model
INTEGER, PARAMETER :: StashCode_CO_AQ = 34010  ! stash code for carbon monoxide
                                                 ! mixing ratio in AQCOPS model

INTEGER, PARAMETER :: StashCode_CoastDist = 44011
INTEGER, PARAMETER :: StashCode_MWEmissAtlas = 44012
INTEGER, PARAMETER :: StashCode_MWEmErrAtlas = 44013

! Model AOD
INTEGER, PARAMETER :: StashCode_AODClim = 2285       ! mineral dust AOD climat.
INTEGER, PARAMETER :: StashCode_AOD = 2422           ! Spectral dust AOD
INTEGER, PARAMETER :: StashCode_TotalAOD = 2298      ! Spectral total AOD

INTEGER, PARAMETER :: StashItem_theta_unslanted = 80000            ! Potential temperature
INTEGER, PARAMETER :: StashItem_u_unslanted = 80001            ! Potential temperature
INTEGER, PARAMETER :: StashItem_v_unslanted = 80002            ! Potential temperature
INTEGER, PARAMETER :: StashItem_q_unslanted = 80003            ! Potential temperature
INTEGER, PARAMETER :: StashItem_p_unslanted = 80004            ! Potential temperature
INTEGER, PARAMETER :: stashcode_cha = 999998
INTEGER, PARAMETER :: stashcode_mss = 999999

END MODULE OpsMod_Stash
