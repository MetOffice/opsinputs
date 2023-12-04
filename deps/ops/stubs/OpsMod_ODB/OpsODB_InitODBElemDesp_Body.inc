!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Initialises ElemDesp2ODBDesp for the body table.
!-------------------------------------------------------------------------------

SUBROUTINE OpsODB_InitODBElemDesp_Body (h,        &
                                        ObsGroup)

USE GenMod_Core, ONLY: &
    gen_trace_entry,     &
    gen_trace_exit,      &
    UseTrace

USE ops_odb_wrappers, ONLY: &
    ops_odb_getval

USE OpsMod_ObsGroupInfo, ONLY: &
    ObsGroupABIClr,            &
    ObsGroupAircraft,          &
    ObsGroupAIRS,              &
    ObsGroupAltim,             &
    ObsGroupAMSR,              &
    ObsGroupAMSUB,             &
    ObsGroupATMS,              &
    ObsGroupATOVS,             &
    ObsGroupCOMSMIClr,         &
    ObsGroupCrIS,              &
    ObsGroupHIRAS,             &
    ObsGroupMWSFY3B,           &
    ObsGroupGeoCloud,          &
    ObsGroupGIIRSLW,           &
    ObsGroupGIIRSMW,           &
    ObsGroupGMIlow,            &
    ObsGroupGMIhigh,           &
    ObsGroupGOESImClr,         &
    ObsGroupGPSRO,             &
    ObsGroupHLOSWind,          &
    ObsGroupIASI,              &
    ObsGroupIN3DIClr,          &
    ObsGroupIN3DS,             &
    ObsGroupSAPHIR,            &
    ObsGroupMTSATImClr,        &
    ObsGroupMWSFY3,            &
    ObsGroupMWRI,              &
    ObsGroupOcean,             &
    ObsGroupSatSST,            &
    ObsGroupSatwind,           &
    ObsGroupScatwind,          &
    ObsGroupSonde,             &
    ObsGroupSSMIS,             &
    ObsGroupSurface,           &
    ObsGroupSurfaceCloud,      &
    ObsGroupTOVS,              &
    OpsFn_IsAHIGroup,          &
    OpsFn_IsSEVIRIGroup

USE OpsMod_Index

USE OpsMod_ODBInfo, ONLY: &
    num_cloud_vars

USE OpsMod_ODBOPSInterface, ONLY: &
    ElemDesp2ODBDesp,             &
    Ops_SetupODBVertcoTypes,      &
    vertco_type_cha_number,       &
    vertco_type_derived_pressure, &
    vertco_type_imp_param,        &
    vertco_type_lidar_range,      &
    vertco_type_pressure,         &
    vertco_type_tangent_height

USE OpsMod_ODBTypes, ONLY: &
    ODBSession_type

USE OpsMod_Process, ONLY: &
    VerRun

USE OpsMod_SatRad_SetUp, ONLY: &
    SrcObsGroupGeoCloud

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(IN) :: h         ! ODB handle
INTEGER, INTENT(IN)                :: ObsGroup  ! Obs group Name

! Local declarations:
CHARACTER(len=*),PARAMETER         :: RoutineName = "OpsODB_InitODBElemDesp_Body"

!-------------------------------------------------------------------------------
! 0. Tracing
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL Ops_SetupODBVertcoTypes (h)

!----------------------------------------------------------------------------
! 2. Mapping for each Element
!----------------------------------------------------------------------------
! if not Satwind
IF (ObsGroup == ObsGroupSurface) THEN
  ! get varno for wind speed
  ElemDesp2ODBDesp(Indexddd10) % TableName                    = 'body'
  ElemDesp2ODBDesp(Indexddd10) % ColName                      = 'initial_obsvalue'
  ElemDesp2ODBDesp(Indexddd10) % Varno                        = ops_odb_getval (h, "$dd")
  ElemDesp2ODBDesp(Indexddd10) % also_do(1:2)                 = (/IndexUComponent10m, IndexVComponent10m/)

  ElemDesp2ODBDesp(Indexfff10) % TableName                    = 'body'
  ElemDesp2ODBDesp(Indexfff10) % ColName                      = 'initial_obsvalue'
  ElemDesp2ODBDesp(Indexfff10) % Varno                        = ops_odb_getval (h, "$ff")
  ElemDesp2ODBDesp(Indexfff10) % also_do(1:2)                 = (/IndexUComponent10m, IndexVComponent10m/)
END IF

ElemDesp2ODBDesp(Indext2) % TableName                         = 'body'
ElemDesp2ODBDesp(Indext2) % ColName                           = 'initial_obsvalue'
ElemDesp2ODBDesp(Indext2) % Varno                             = ops_odb_getval (h, "$t2m")

IF (VerRun) THEN
  ElemDesp2ODBDesp(Indexrh) % TableName                       = 'body'
  ElemDesp2ODBDesp(Indexrh) % ColName                         = 'obsvalue'
  ElemDesp2ODBDesp(Indexrh) % related_varnos(1)               = ops_odb_getval (h, "$rh")
  IF (ObsGroup == ObsGroupSurface) THEN
    ElemDesp2ODBDesp(Indexrh2) % TableName                    = 'body'
    ElemDesp2ODBDesp(Indexrh2) % ColName                      = 'obsvalue'
    ElemDesp2ODBDesp(Indexrh2) % related_varnos(1)            = ops_odb_getval (h, "$rh2m")
  ELSE
    ElemDesp2ODBDesp(Indexrh2) % TableName                    = 'body'
    ElemDesp2ODBDesp(Indexrh2) % ColName                      = 'corvalue'
    ElemDesp2ODBDesp(Indexrh2) % related_varnos(1)            = ops_odb_getval (h, "$rh2m")
  END IF
ELSE
  ElemDesp2ODBDesp(Indexrh) % TableName                       = 'body'
  ElemDesp2ODBDesp(Indexrh) % ColName                         = 'initial_obsvalue'
  ElemDesp2ODBDesp(Indexrh) % Varno                           = ops_odb_getval (h, "$rh")    ! rh
  ElemDesp2ODBDesp(Indexrh2) % TableName                      = 'body'
  ElemDesp2ODBDesp(Indexrh2) % ColName                        = 'initial_obsvalue'
  ElemDesp2ODBDesp(Indexrh2) % Varno                          = ops_odb_getval (h, "$rh2m")    ! 2m rh
END IF

ElemDesp2ODBDesp(Indextd2) % TableName                        = 'body'
ElemDesp2ODBDesp(Indextd2) % ColName                          = 'initial_obsvalue'
ElemDesp2ODBDesp(Indextd2) % Varno                            = ops_odb_getval (h, "$td2m")    ! dewpoint
IF (ObsGroup == ObsGroupSurface) THEN
  ElemDesp2ODBDesp(Indextd2) % also_do(1:1)                   = (/Indexrh2/)
END IF

ElemDesp2ODBDesp(IndexHorizVisib) % TableName                 = 'body'
ElemDesp2ODBDesp(IndexHorizVisib) % ColName                   = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexHorizVisib) % Varno                     = ops_odb_getval (h, "$vv") ! h. Visibility

ElemDesp2ODBDesp(IndexVertVisib) % TableName                  = 'body'
ElemDesp2ODBDesp(IndexVertVisib) % ColName                    = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexVertVisib) % Varno                      = ops_odb_getval (h, "$vert_vv") ! v. Visibility

ElemDesp2ODBDesp(IndexMXMM_WIND_LEVL_WIND_SHER_1) % TableName = 'body'
ElemDesp2ODBDesp(IndexMXMM_WIND_LEVL_WIND_SHER_1) % ColName   = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexMXMM_WIND_LEVL_WIND_SHER_1) % Varno     = ops_odb_getval (h, "$max_wind_shear1")

ElemDesp2ODBDesp(IndexMXMM_WIND_LEVL_WIND_SHER_2) % TableName = 'body'
ElemDesp2ODBDesp(IndexMXMM_WIND_LEVL_WIND_SHER_2) % ColName   = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexMXMM_WIND_LEVL_WIND_SHER_2) % Varno     = ops_odb_getval (h, "$max_wind_shear2")

ElemDesp2ODBDesp(IndexPmsl) % TableName                       = 'body'
ElemDesp2ODBDesp(IndexPmsl) % ColName                         = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexPmsl) % Varno                           = ops_odb_getval (h, "$pmsl")    ! mslp
IF (ObsGroup == ObsGroupSurface) THEN
  ElemDesp2ODBDesp(IndexPmsl) % also_do(1:2)                  = (/IndexPstar, IndexPStation/)
END IF

ElemDesp2ODBDesp(IndexPstandard) % TableName                  = 'body'
ElemDesp2ODBDesp(IndexPstandard) % ColName                    = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexPstandard) % Varno                      = ops_odb_getval (h, "$pstandard")    ! Pressure

ElemDesp2ODBDesp(IndexPstation) % TableName                   = 'body'
ElemDesp2ODBDesp(IndexPstation) % ColName                     = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexPstation) % Varno                       = ops_odb_getval (h, "$pstation")    ! Station level pressure

ElemDesp2ODBDesp(IndexPstar) % TableName                      = 'body'
ElemDesp2ODBDesp(IndexPstar) % ColName                        = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexPstar) % Varno                          = ops_odb_getval (h, "$ps")    ! pstar

IF (ObsGroup == ObsGroupSonde .OR. ObsGroup == ObsGroupGPSRO) THEN
  ElemDesp2ODBDesp(IndexLEVL_HGHT) % TableName                = 'body'
  ElemDesp2ODBDesp(IndexLEVL_HGHT) % ColName                  = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexLEVL_HGHT) % Varno                    = ops_odb_getval (h, "$z")
ELSE
  ElemDesp2ODBDesp(IndexZstandard) % TableName                = 'body'
  ElemDesp2ODBDesp(IndexZstandard) % ColName                  = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexZstandard) % Varno                    = ops_odb_getval (h, "$z")
END IF

ElemDesp2ODBDesp(IndexPressureTendency) % TableName           = 'body'
ElemDesp2ODBDesp(IndexPressureTendency) % ColName             = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexPressureTendency) % Varno               = ops_odb_getval (h, "$cpt")

ElemDesp2ODBDesp(IndexPressureChange) % TableName             = 'body'
ElemDesp2ODBDesp(IndexPressureChange) % ColName               = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexPressureChange) % Varno                 = ops_odb_getval (h, "$ptend")

IF (ANY ((/ObsGroupAircraft, ObsGroupSatwind, ObsGroupSonde, ObsGroupSurface, ObsGroupSSMIS/) == ObsGroup)) THEN
  ElemDesp2ODBDesp(Indexddd) % TableName                      = 'body'
  ElemDesp2ODBDesp(Indexddd) % ColName                        = 'initial_obsvalue'
  ElemDesp2ODBDesp(Indexddd) % Varno                          = ops_odb_getval (h, "$dd")  ! wind direction
  ElemDesp2ODBDesp(Indexddd) % also_do(1:2)                   = (/IndexUComponent, IndexVComponent/)

  ElemDesp2ODBDesp(Indexfff) % TableName                      = 'body'
  ElemDesp2ODBDesp(Indexfff) % ColName                        = 'initial_obsvalue'
  ElemDesp2ODBDesp(Indexfff) % Varno                          = ops_odb_getval (h, "$ff")  ! wind speed
  ElemDesp2ODBDesp(Indexfff) % also_do(1:2)                   = (/IndexUComponent, IndexVComponent/)

  IF (ObsGroup == ObsGroupSatwind .OR. ObsGroup == ObsGroupAircraft) THEN
    ElemDesp2ODBDesp(IndexWIND_SPED) % TableName              = 'body'
    ElemDesp2ODBDesp(IndexWIND_SPED) % ColName                = 'initial_obsvalue'
    ElemDesp2ODBDesp(IndexWIND_SPED) % Varno                  = ops_odb_getval (h, "$ff")  ! wind speed
    ElemDesp2ODBDesp(IndexWIND_SPED) % also_do(1:2)           = (/IndexUComponent, IndexVComponent/)
  END IF
END IF

ElemDesp2ODBDesp(IndexTheta) % TableName                      = 'body'
ElemDesp2ODBDesp(IndexTheta) % ColName                        = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexTheta) % Varno                          = ops_odb_getval (h, "$theta")  ! temperature

ElemDesp2ODBDesp(IndexTemp) % TableName                       = 'body'
ElemDesp2ODBDesp(IndexTemp) % ColName                         = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexTemp) % Varno                           = ops_odb_getval (h, "$t")  ! temperature
IF (ObsGroup == ObsGroupOcean) THEN
  ElemDesp2ODBDesp(IndexTemp) % also_do(1:1)                  = (/IndexTheta/)
END IF
IF (ObsGroup == ObsGroupSatwind) THEN
  ElemDesp2ODBDesp(IndexTemp) % also_do(1:1)                  = (/IndexPstar/)
END IF
IF (ObsGroup == ObsGroupSonde) THEN
  ElemDesp2ODBDesp(IndexTemp) % also_do(1:1)                  = (/IndexRH/)
END IF

ElemDesp2ODBDesp(IndexUComponent) % TableName                 = 'body'
ElemDesp2ODBDesp(IndexUComponent) % ColName                   = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexUComponent) % Varno                     = ops_odb_getval (h, "$u")  ! temperature

ElemDesp2ODBDesp(IndexVComponent) % TableName                 = 'body'
ElemDesp2ODBDesp(IndexVComponent) % ColName                   = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexVComponent) % Varno                     = ops_odb_getval (h, "$v")  ! temperature

ElemDesp2ODBDesp(IndexUComponent10m) % TableName              = 'body'
ElemDesp2ODBDesp(IndexUComponent10m) % ColName                = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexUComponent10m) % Varno                  = ops_odb_getval (h, "$u10m")  ! temperature

ElemDesp2ODBDesp(IndexVComponent10m) % TableName              = 'body'
ElemDesp2ODBDesp(IndexVComponent10m) % ColName                = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexVComponent10m) % Varno                  = ops_odb_getval (h, "$v10m")  ! temperature

ElemDesp2ODBDesp(IndexDewPoint) % TableName                   = 'body'
ElemDesp2ODBDesp(IndexDewPoint) % ColName                     = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexDewPoint) % Varno                       = ops_odb_getval (h, "$td")  ! temperature

ElemDesp2ODBDesp(IndexCLW) % TableName                        = 'body'
ElemDesp2ODBDesp(IndexCLW) % ColName                          = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexCLW) % Varno                            = ops_odb_getval (h, "$cllqw")  ! temperature

ElemDesp2ODBDesp(IndexCIW) % TableName                        = 'body'
ElemDesp2ODBDesp(IndexCIW) % ColName                          = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexCIW) % Varno                            = ops_odb_getval (h, "$cloud_ice_water")  ! temperature

ElemDesp2ODBDesp(IndexSkinTemperature) % TableName            = 'body'
ElemDesp2ODBDesp(IndexSkinTemperature) % ColName              = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexSkinTemperature) % Varno                = ops_odb_getval (h, "$ts")

ElemDesp2ODBDesp(IndexLWP) % TableName                        = 'body'
ElemDesp2ODBDesp(IndexLWP) % ColName                          = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexLWP) % Varno                            = ops_odb_getval (h, "$lwp")

ElemDesp2ODBDesp(IndexTCWV) % TableName                       = 'body'
ElemDesp2ODBDesp(IndexTCWV) % ColName                         = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexTCWV) % Varno                           = ops_odb_getval (h, "$tcwv")

ElemDesp2ODBDesp(IndexLWPRet) % TableName                     = 'body'
ElemDesp2ODBDesp(IndexLWPRet) % ColName                       = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexLWPRet) % Varno                         = ops_odb_getval (h, "$lwp_ret")

IF (ObsGroup == ObsGroupATOVS) THEN
  ElemDesp2ODBDesp(IndexHIRS20_BRGTS_TMPR) % TableName        = 'body'
  ElemDesp2ODBDesp(IndexHIRS20_BRGTS_TMPR) % ColName          = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexHIRS20_BRGTS_TMPR) % Varno            = ops_odb_getval (h, "$rawbt_hirs20")  ! brightness temperature

  ElemDesp2ODBDesp(IndexHIRS_BRGTS_TMPR) % TableName          = 'body'
  ElemDesp2ODBDesp(IndexHIRS_BRGTS_TMPR) % ColName            = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexHIRS_BRGTS_TMPR) % Varno              = ops_odb_getval (h, "$rawbt_hirs")  ! brightness temperature

  ElemDesp2ODBDesp(IndexAMSU_BRGTS_TMPR) % TableName          = 'body'
  ElemDesp2ODBDesp(IndexAMSU_BRGTS_TMPR) % ColName            = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexAMSU_BRGTS_TMPR) % Varno              = ops_odb_getval (h, "$rawbt_amsu")  ! brightness temperature
  ElemDesp2ODBDesp(IndexAMSU_BRGTS_TMPR) % also_do(1:8)       = (/IndexPstar,           &
                                                                  IndexSkinTemperature, &
                                                                  IndexRH,              &
                                                                  IndexTemp,            &
                                                                  IndexLWP,             &
                                                                  IndexLWPRet,          &
                                                                  IndexRH2,             &
                                                                  IndexT2/)
  ElemDesp2ODBDesp(IndexAMSU_BRGTS_TMPR) % also_do_levels(1:8) = (/1,1,43,43,1,1,1,1/)
END IF

IF (ObsGroup == ObsGroupTOVS) THEN
  ElemDesp2ODBDesp(IndexHIRS20_BRGTS_TMPR) % TableName           = 'body'
  ElemDesp2ODBDesp(IndexHIRS20_BRGTS_TMPR) % ColName             = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexHIRS20_BRGTS_TMPR) % Varno               = ops_odb_getval (h, "$rawbt_hirs20")  ! brightness temperature

  ElemDesp2ODBDesp(IndexHIRS_BRGTS_TMPR) % TableName             = 'body'
  ElemDesp2ODBDesp(IndexHIRS_BRGTS_TMPR) % ColName               = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexHIRS_BRGTS_TMPR) % Varno                 = ops_odb_getval (h, "$rawbt_hirs")  ! brightness temperature

  ElemDesp2ODBDesp(IndexMSU_SSU_BRGTS_TMPR) % TableName          = 'body'
  ElemDesp2ODBDesp(IndexMSU_SSU_BRGTS_TMPR) % ColName            = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexMSU_SSU_BRGTS_TMPR) % Varno              = ops_odb_getval (h, "$rawbt_amsu")  ! brightness temperature
END IF

IF (ObsGroup == ObsGroupMWSFY3) THEN
  ElemDesp2ODBDesp(IndexBRGTS_TMPR1) % TableName              = 'body'
  ElemDesp2ODBDesp(IndexBRGTS_TMPR1) % ColName                = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexBRGTS_TMPR1) % Varno                  = ops_odb_getval (h, "$rawbt_mwts")  ! brightness temperature

  ElemDesp2ODBDesp(IndexBRGTS_TMPR2) % TableName              = 'body'
  ElemDesp2ODBDesp(IndexBRGTS_TMPR2) % ColName                = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexBRGTS_TMPR2) % Varno                  = ops_odb_getval (h, "$rawbt_mwhs")  ! brightness temperature
  ElemDesp2ODBDesp(IndexBRGTS_TMPR2) % also_do(1:6)           = (/IndexPstar,           &
                                                                  IndexSkinTemperature, &
                                                                  IndexRH,              &
                                                                  IndexTemp,            &
                                                                  IndexRH2,             &
                                                                  IndexT2/)
  ElemDesp2ODBDesp(IndexBRGTS_TMPR2) % also_do_levels(1:6)    = (/1,1,43,43,1,1/)
END IF

IF (ObsGroup == ObsGroupIASI) THEN
  ElemDesp2ODBDesp(IndexAMSU_BRGTS_TMPR) % TableName          = 'body'
  ElemDesp2ODBDesp(IndexAMSU_BRGTS_TMPR) % ColName            = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexAMSU_BRGTS_TMPR) % Varno              = ops_odb_getval (h, "$rawbt_amsu")  ! brightness temperature
END IF

IF (ObsGroup == ObsGroupHIRAS) THEN
  ElemDesp2ODBDesp(IndexRadiance) % TableName         = 'body'
  ElemDesp2ODBDesp(IndexRadiance) % ColName           = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexRadiance) % Varno             = ops_odb_getval (h, "$rawsca")
  ElemDesp2ODBDesp(IndexRadiance) % also_do(1:7)      = (/IndexPstar,           &
                                                          IndexSkinTemperature, &
                                                          IndexRH,              &
                                                          IndexTemp,            &
                                                          IndexRH2,             &
                                                          IndexT2,              &
                                                          IndexHIRASBrightnessTemp/)
  ElemDesp2ODBDesp(IndexRadiance) % also_do_levels(1:7)       = (/1,1,43,43,1,1,399/)

  ElemDesp2ODBDesp(IndexHIRASBrightnessTemp) % TableName         = 'body'
  ElemDesp2ODBDesp(IndexHIRASBrightnessTemp) % ColName           = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexHIRASBrightnessTemp) % Varno             = ops_odb_getval (h, "$rawbt")
END IF

IF (ObsGroup == ObsGroupGIIRSLW .or. ObsGroup == ObsGroupGIIRSMW) THEN
  ElemDesp2ODBDesp(IndexRadiance) % TableName    = 'body'
  ElemDesp2ODBDesp(IndexRadiance) % ColName      = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexRadiance) % Varno        = ops_odb_getval (h, "$rawsca")
  ElemDesp2ODBDesp(IndexRadiance) % also_do(1:7) = (/IndexPstar,           &
                                                     IndexSkinTemperature, &
                                                     IndexRH,              &
                                                     IndexTemp,            &
                                                     IndexRH2,             &
                                                     IndexT2,              &
                                                     IndexGIIRSBrightnessTemp/)
  ElemDesp2ODBDesp(IndexRadiance) % also_do_levels(1:7)  = (/1,1,43,43,1,1,1650/)
  ElemDesp2ODBDesp(IndexGIIRSBrightnessTemp) % TableName = 'body'
  ElemDesp2ODBDesp(IndexGIIRSBrightnessTemp) % ColName   = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexGIIRSBrightnessTemp) % Varno     = ops_odb_getval (h, "$rawbt")
END IF

IF (ObsGroup == ObsGroupCrIS) THEN
  ElemDesp2ODBDesp(IndexATMS_BRGTS_TMPR) % TableName          = 'body'
  ElemDesp2ODBDesp(IndexATMS_BRGTS_TMPR) % ColName            = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexATMS_BRGTS_TMPR) % Varno              = ops_odb_getval (h, "$rawbt")  ! brightness temperature
  ElemDesp2ODBDesp(IndexATMS_BRGTS_TMPR) % also_do(1:6)       = (/IndexPstar,           &
                                                                  IndexSkinTemperature, &
                                                                  IndexRH,              &
                                                                  IndexTemp,            &
                                                                  IndexRH2,             &
                                                                  IndexT2/)
  ElemDesp2ODBDesp(IndexATMS_BRGTS_TMPR) % also_do_levels(1:6) = (/1,1,43,43,1,1/)
  ElemDesp2ODBDesp(IndexATMS_BRGTS_TMPR) % num_extra_levs     = 399
END IF

IF (ObsGroup == ObsGroupAMSUB) THEN
  ElemDesp2ODBDesp(IndexAMSU_BRGTS_TMPR) % TableName          = 'body'
  ElemDesp2ODBDesp(IndexAMSU_BRGTS_TMPR) % ColName            = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexAMSU_BRGTS_TMPR) % Varno              = ops_odb_getval (h, "$rawbt")  ! brightness temperature
  ElemDesp2ODBDesp(IndexAMSU_BRGTS_TMPR) % also_do(1:1)       = (/IndexLWP/)
  ElemDesp2ODBDesp(IndexAMSU_BRGTS_TMPR) % also_do_levels(1:1) = (/1/)
END IF

IF (ObsGroup == ObsGroupMWSFY3B) THEN
  ElemDesp2ODBDesp(IndexAMSU_BRGTS_TMPR) % TableName          = 'body'
  ElemDesp2ODBDesp(IndexAMSU_BRGTS_TMPR) % ColName            = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexAMSU_BRGTS_TMPR) % Varno              = ops_odb_getval (h, "$rawbt_amsu")  ! brightness temperature
  ElemDesp2ODBDesp(IndexAMSU_BRGTS_TMPR) % also_do(1:7)       = (/IndexPstar,           &
                                                                  IndexSkinTemperature, &
                                                                  IndexRH,              &
                                                                  IndexTemp,            &
                                                                  IndexRH2,             &
                                                                  IndexT2,              &
                                                                  IndexFY3BBriTemp/)
  ElemDesp2ODBDesp(IndexAMSU_BRGTS_TMPR) % also_do_levels(1:7) = (/1,1,43,43,1,1,9/)

  ElemDesp2ODBDesp(IndexHIRS_BRGTS_TMPR) % TableName          = 'body'
  ElemDesp2ODBDesp(IndexHIRS_BRGTS_TMPR) % ColName            = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexHIRS_BRGTS_TMPR) % Varno              = ops_odb_getval (h, "$rawbt_hirs")  ! brightness temperature

  ElemDesp2ODBDesp(IndexHIRS20_BRGTS_TMPR) % TableName        = 'body'
  ElemDesp2ODBDesp(IndexHIRS20_BRGTS_TMPR) % ColName          = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexHIRS20_BRGTS_TMPR) % Varno            = ops_odb_getval (h, "$rawbt_hirs20")  ! brightness temperature

  ElemDesp2ODBDesp(IndexFY3BBriTemp) % TableName              = 'body'
  ElemDesp2ODBDesp(IndexFY3BBriTemp) % ColName                = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexFY3BBriTemp) % Varno                  = ops_odb_getval (h, "$rawbt")  ! brightness temperature
END IF

IF (ObsGroup == ObsGroupAIRS) THEN
  ElemDesp2ODBDesp(IndexAMSU_BRGTS_TMPR) % TableName          = 'body'
  ElemDesp2ODBDesp(IndexAMSU_BRGTS_TMPR) % ColName            = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexAMSU_BRGTS_TMPR) % Varno              = ops_odb_getval (h, "$rawbt_amsu")  ! brightness temperature
END IF

IF (ObsGroup == ObsGroupATMS) THEN
  ElemDesp2ODBDesp(IndexATMS_BRGTS_TMPR) % TableName          = 'body'
  ElemDesp2ODBDesp(IndexATMS_BRGTS_TMPR) % ColName            = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexATMS_BRGTS_TMPR) % Varno              = ops_odb_getval (h, "$rawbt")  ! brightness temperature
  ElemDesp2ODBDesp(IndexATMS_BRGTS_TMPR) % also_do(1:10)      = (/IndexCLW,             &
                                                                  IndexSkinTemperature, &
                                                                  IndexRH,              &
                                                                  IndexTemp,            &
                                                                  IndexRH2,             &
                                                                  IndexT2,              &
                                                                  IndexLWP,             &
                                                                  IndexTCWV,            &
                                                                  IndexPstar,           &
                                                                  IndexCIW/)
  ElemDesp2ODBDesp(IndexATMS_BRGTS_TMPR) % also_do_levels(1:10) = (/43,1,43,43,1,1,1,1,1,43/)

  ElemDesp2ODBDesp(IndexChnlQlty) % TableName                 = 'sat_body'
  ElemDesp2ODBDesp(IndexChnlQlty) % ColName                   = 'channel_quality_flags'
  ElemDesp2ODBDesp(IndexChnlQlty) % related_varnos(1:1)       = (/ops_odb_getval (h, "$rawbt")/)

  ElemDesp2ODBDesp(IndexATMS_NEDT_COLD) % TableName           = 'sat_body'
  ElemDesp2ODBDesp(IndexATMS_NEDT_COLD) % ColName             = 'cold_nedt'
  ElemDesp2ODBDesp(IndexATMS_NEDT_COLD) % related_varnos(1:1) = (/ops_odb_getval (h, "$rawbt")/)

  ElemDesp2ODBDesp(IndexATMS_NEDT_WARM) % TableName           = 'sat_body'
  ElemDesp2ODBDesp(IndexATMS_NEDT_WARM) % ColName             = 'warm_nedt'
  ElemDesp2ODBDesp(IndexATMS_NEDT_WARM) % related_varnos(1:1) = (/ops_odb_getval (h, "$rawbt")/)
END IF

ElemDesp2ODBDesp(IndexTotalCloudAmount) % TableName           = 'body'
ElemDesp2ODBDesp(IndexTotalCloudAmount) % ColName             = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexTotalCloudAmount) % Varno               = ops_odb_getval (h, "$n")

IF (ObsGroup == ObsGroupScatwind .OR. &
    ObsGroup == ObsGroupSatSST .OR.   &
    ObsGroup == ObsGroupAltim) THEN
  ElemDesp2ODBDesp(IndexWIND_SPED) % TableName                = 'body'
  ElemDesp2ODBDesp(IndexWIND_SPED) % ColName                  = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexWIND_SPED) % Varno                    = ops_odb_getval (h, "$ff")
  IF (ObsGroup == ObsGroupScatwind) THEN
    ElemDesp2ODBDesp(IndexWIND_SPED) % also_do(1:2)           = (/IndexU10AmbWindComponent, IndexV10AmbWindComponent/)
  END IF
END IF

ElemDesp2ODBDesp(IndexWindSpeed) % TableName                = 'body'
ElemDesp2ODBDesp(IndexWindSpeed) % ColName                  = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexWindSpeed) % Varno                    = ops_odb_getval (h, "$ff")

IF (ObsGroup == ObsGroupScatwind) THEN
  ElemDesp2ODBDesp(Indexddd10AmbWind) % TableName             = 'body'
  ElemDesp2ODBDesp(Indexddd10AmbWind) % ColName               = 'initial_obsvalue'
  ElemDesp2ODBDesp(Indexddd10AmbWind) % Varno                 = ops_odb_getval (h, "$dd")
  ElemDesp2ODBDesp(Indexddd10AmbWind) % also_do(1:2)          = (/IndexU10AmbWindComponent, IndexV10AmbWindComponent/)
END IF

ElemDesp2ODBDesp(IndexU10AmbWindComponent) % TableName        = 'body'
ElemDesp2ODBDesp(IndexU10AmbWindComponent) % ColName          = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexU10AmbWindComponent) % Varno            = ops_odb_getval (h, "$u_amb")

ElemDesp2ODBDesp(IndexV10AmbWindComponent) % TableName        = 'body'
ElemDesp2ODBDesp(IndexV10AmbWindComponent) % ColName          = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexV10AmbWindComponent) % Varno            = ops_odb_getval (h, "$v_amb")


ElemDesp2ODBDesp(IndexCloudCover) % TableName                 = 'body'
ElemDesp2ODBDesp(IndexCloudCover) % ColName                   = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexCloudCover) % Varno                     = ops_odb_getval (h, "$cloud_cover")

ElemDesp2ODBDesp(IndexSST) % TableName                        = 'body'
ElemDesp2ODBDesp(IndexSST) % ColName                          = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexSST) % Varno                            = ops_odb_getval (h, "$tsts")

ElemDesp2ODBDesp(IndexSeaSrfcHeight) % TableName              = 'body'
ElemDesp2ODBDesp(IndexSeaSrfcHeight) % ColName                = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexSeaSrfcHeight) % Varno                  = ops_odb_getval (h, "$ssh")

ElemDesp2ODBDesp(IndexSal) % TableName                        = 'body'
ElemDesp2ODBDesp(IndexSal) % ColName                          = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexSal) % Varno                            = ops_odb_getval (h, "$salinity")

ElemDesp2ODBDesp(IndexSSTStdev) % TableName                   = 'body'
ElemDesp2ODBDesp(IndexSSTStdev) % ColName                     = 'std_dev'
ElemDesp2ODBDesp(IndexSSTStdev) % related_varnos(1:1)         = (/ops_odb_getval (h, "$tsts")/)

ElemDesp2ODBDesp(IndexGPSTZDelay) % TableName                 = 'body'
ElemDesp2ODBDesp(IndexGPSTZDelay) % ColName                   = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexGPSTZDelay) % Varno                     = ops_odb_getval (h, "$tot_zen_delay")

ElemDesp2ODBDesp(IndexGPSTZDelayError) % TableName            = 'body'
ElemDesp2ODBDesp(IndexGPSTZDelayError) % ColName              = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexGPSTZDelayError) % Varno                = ops_odb_getval (h, "$tot_zen_delay_err")

ElemDesp2ODBDesp(IndexRefrac) % TableName                     = 'body'
ElemDesp2ODBDesp(IndexRefrac) % ColName                       = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexRefrac) % Varno                         = ops_odb_getval (h, "$refl")

ElemDesp2ODBDesp(IndexSpecificHumidity) % TableName           = 'body'
ElemDesp2ODBDesp(IndexSpecificHumidity) % ColName             = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexSpecificHumidity) % Varno               = ops_odb_getval (h, "$q")

ElemDesp2ODBDesp(IndexBendingAngleAll) % TableName            = 'body'
ElemDesp2ODBDesp(IndexBendingAngleAll) % ColName              = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexBendingAngleAll) % Varno                = ops_odb_getval (h, "$bend_angle")
ElemDesp2ODBDesp(IndexBendingAngleAll) % also_do(1)           = IndexSpecificHumidity

ElemDesp2ODBDesp(IndexImpactParamAll) % TableName             = 'body'
ElemDesp2ODBDesp(IndexImpactParamAll) % ColName               = 'initial_vertco_reference'
ElemDesp2ODBDesp(IndexImpactParamAll) % Pressno               = vertco_type_imp_param
ElemDesp2ODBDesp(IndexImpactParamAll) % related_varnos(1:3)   = (/ops_odb_getval (h, "$q"),           &
                                                                  ops_odb_getval (h, "$mean_freq"),   &
                                                                  ops_odb_getval (h, "$bend_angle")/)

ElemDesp2ODBDesp(IndexRO_Mean_Freq) % TableName               = 'body'
ElemDesp2ODBDesp(IndexRO_Mean_Freq) % ColName                 = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexRO_Mean_Freq) % Varno                   = ops_odb_getval (h, "$mean_freq")

IF (ObsGroup /= ObsGroupSurfaceCloud .AND. ObsGroup /= ObsGroupSurface) THEN
  ElemDesp2ODBDesp(IndexCLOD_AMNT) % TableName                = 'body'
  ElemDesp2ODBDesp(IndexCLOD_AMNT) % ColName                  = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexCLOD_AMNT) % Varno                    = ops_odb_getval (h, "$satcl")
END IF

ElemDesp2ODBDesp(IndexSEVIRICloudTopT) % TableName            = 'body'
ElemDesp2ODBDesp(IndexSEVIRICloudTopT) % ColName              = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexSEVIRICloudTopT) % Varno                = ops_odb_getval (h, "$cloud_top_temp")

IF (OpsFn_IsSEVIRIGroup(ObsGroup) .OR. &
    (ObsGroup == ObsGroupGeoCloud .AND. OpsFn_IsSEVIRIGroup (SrcObsGroupGeoCloud))) THEN
  ElemDesp2ODBDesp(IndexSEVIRIBrightnessTemp) % TableName     = 'body'
  ElemDesp2ODBDesp(IndexSEVIRIBrightnessTemp) % ColName       = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexSEVIRIBrightnessTemp) % Varno         = ops_odb_getval (h, "$rawbt")  ! brightness temperature

  ElemDesp2ODBDesp(IndexSEVIRICloudFree) % TableName          = 'body'
  ElemDesp2ODBDesp(IndexSEVIRICloudFree) % ColName            = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexSEVIRICloudFree) % Varno              = ops_odb_getval (h, "$cloud_frac_clear")

  ElemDesp2ODBDesp(IndexSEVIRIBrightnessTemp) % also_do(1:5)  = (/IndexSkinTemperature, &
                                                                  IndexRH,              &
                                                                  IndexTemp,            &
                                                                  IndexRH2,             &
                                                                  IndexT2/)
  ElemDesp2ODBDesp(IndexSEVIRIBrightnessTemp) % also_do_levels(1:5)  = (/1,43,43,1,1/)
END IF

IF (OpsFn_IsAHIGroup(ObsGroup) .OR. &
    (ObsGroup == ObsGroupGeoCloud .AND. OpsFn_IsAHIGroup(SrcObsGroupGeoCloud))) THEN
  ElemDesp2ODBDesp(IndexAHIBrightnessTemp) % TableName        = 'body'
  ElemDesp2ODBDesp(IndexAHIBrightnessTemp) % ColName          = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexAHIBrightnessTemp) % Varno            = ops_odb_getval (h, "$rawbt")  ! brightness temperature

  ElemDesp2ODBDesp(IndexAHICloudFree) % TableName             = 'body'
  ElemDesp2ODBDesp(IndexAHICloudFree) % ColName               = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexAHICloudFree) % Varno                 = ops_odb_getval (h, "$cloud_frac_clear")

  ElemDesp2ODBDesp(IndexAHIBrightnessTemp) % also_do(1:5)     = (/IndexSkinTemperature, &
                                                                  IndexRH,              &
                                                                  IndexTemp,            &
                                                                  IndexRH2,             &
                                                                  IndexT2/)
  ElemDesp2ODBDesp(IndexAHIBrightnessTemp) % also_do_levels(1:5)  = (/1,43,43,1,1/)
END IF

IF (ObsGroup == ObsGroupMTSATImClr) THEN
  ElemDesp2ODBDesp(IndexMTSATImBrightnessTemp) % TableName     = 'body'
  ElemDesp2ODBDesp(IndexMTSATImBrightnessTemp) % ColName       = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexMTSATImBrightnessTemp) % Varno         = ops_odb_getval (h, "$rawbt")  ! brightness temperature

  ElemDesp2ODBDesp(IndexMTSATImCloudFree) % TableName          = 'body'
  ElemDesp2ODBDesp(IndexMTSATImCloudFree) % ColName            = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexMTSATImCloudFree) % Varno              = ops_odb_getval (h, "$cloud_frac_clear")

  ElemDesp2ODBDesp(IndexMTSATImBrightnessTemp) % also_do(1:5)  = (/IndexSkinTemperature, &
                                                                   IndexRH,              &
                                                                   IndexTemp,            &
                                                                   IndexRH2,             &
                                                                   IndexT2/)
  ElemDesp2ODBDesp(IndexMTSATImBrightnessTemp) % also_do_levels(1:5)  = (/1,43,43,1,1/)
END IF

IF (ObsGroup == ObsGroupIN3DS) THEN
  ElemDesp2ODBDesp(IndexIN3DSdBrightnessTemp) % TableName      = 'body'
  ElemDesp2ODBDesp(IndexIN3DSdBrightnessTemp) % ColName        = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexIN3DSdBrightnessTemp) % Varno          = ops_odb_getval (h, "$rawbt")  ! brightness temperature

  ElemDesp2ODBDesp(IndexIN3DImCloudFree) % TableName           = 'body'
  ElemDesp2ODBDesp(IndexIN3DImCloudFree) % ColName             = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexIN3DImCloudFree) % Varno               = ops_odb_getval (h, "$cloud_frac_clear")

  ElemDesp2ODBDesp(IndexIN3DSdBrightnessTemp) % also_do(1:5)   = (/IndexSkinTemperature, &
                                                                   IndexRH ,             &
                                                                   IndexTemp,            &
                                                                   IndexRH2,             &
                                                                   IndexT2/)
  ElemDesp2ODBDesp(IndexIN3DSdBrightnessTemp) % also_do_levels(1:5)  = (/1,43,43,1,1/)
END IF

IF (ObsGroup == ObsGroupGOESImClr) THEN
  ElemDesp2ODBDesp(IndexGOESImBrightnessTemp) % TableName     = 'body'
  ElemDesp2ODBDesp(IndexGOESImBrightnessTemp) % ColName       = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexGOESImBrightnessTemp) % Varno         = ops_odb_getval (h, "$rawbt")  ! brightness temperature

  ElemDesp2ODBDesp(IndexGOESImCloudFree) % TableName          = 'body'
  ElemDesp2ODBDesp(IndexGOESImCloudFree) % ColName            = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexGOESImCloudFree) % Varno              = ops_odb_getval (h, "$cloud_frac_clear")

  ElemDesp2ODBDesp(IndexGOESImBrightnessTemp) % also_do(1:5)  = (/IndexSkinTemperature, &
                                                                  IndexRH,              &
                                                                  IndexTemp,            &
                                                                  IndexRH2,             &
                                                                  IndexT2/)
  ElemDesp2ODBDesp(IndexGOESImBrightnessTemp) % also_do_levels(1:5)  = (/1,43,43,1,1/)
END IF

IF (ObsGroup == ObsGroupABIClr) THEN
  ElemDesp2ODBDesp(IndexABIBrightnessTemp) % TableName        = 'body'
  ElemDesp2ODBDesp(IndexABIBrightnessTemp) % ColName          = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexABIBrightnessTemp) % Varno            = ops_odb_getval (h, "$rawbt")  ! brightness temperature

  ElemDesp2ODBDesp(IndexABICloudFree) % TableName             = 'body'
  ElemDesp2ODBDesp(IndexABICloudFree) % ColName               = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexABICloudFree) % Varno                 = ops_odb_getval (h, "$cloud_frac_clear")

  ElemDesp2ODBDesp(IndexABIBrightnessTemp) % also_do(1:5)     = (/IndexSkinTemperature, &
                                                                  IndexRH,              &
                                                                  IndexTemp,            &
                                                                  IndexRH2,             &
                                                                  IndexT2/)
  ElemDesp2ODBDesp(IndexABIBrightnessTemp) % also_do_levels(1:5)     = (/1,43,43,1,1/)
END IF

IF (ObsGroup == ObsGroupIN3DIClr) THEN
  ElemDesp2ODBDesp(IndexIN3DImBrightnessTemp) % TableName     = 'body'
  ElemDesp2ODBDesp(IndexIN3DImBrightnessTemp) % ColName       = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexIN3DImBrightnessTemp) % Varno         = ops_odb_getval (h, "$rawbt")  ! brightness temperature

  ElemDesp2ODBDesp(IndexIN3DImCloudFree) % TableName          = 'body'
  ElemDesp2ODBDesp(IndexIN3DImCloudFree) % ColName            = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexIN3DImCloudFree) % Varno              = ops_odb_getval (h, "$cloud_frac_clear")

  ElemDesp2ODBDesp(IndexIN3DImBrightnessTemp) % also_do(1:5)  = (/IndexSkinTemperature, &
                                                                  IndexRH,              &
                                                                  IndexTemp,            &
                                                                  IndexRH2,             &
                                                                  IndexT2/)
  ElemDesp2ODBDesp(IndexIN3DImBrightnessTemp) % also_do_levels(1:5)  = (/1,43,43,1,1/)
END IF

IF (ObsGroup == ObsGroupIN3DS) THEN
  ElemDesp2ODBDesp(IndexIN3DSdBrightnessTemp) % TableName     = 'body'
  ElemDesp2ODBDesp(IndexIN3DSdBrightnessTemp) % ColName       = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexIN3DSdBrightnessTemp) % Varno         = ops_odb_getval (h, "$rawbt")  ! brightness temperature

  ElemDesp2ODBDesp(IndexIN3DSdCloudFree) % TableName          = 'body'
  ElemDesp2ODBDesp(IndexIN3DSdCloudFree) % ColName            = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexIN3DSdCloudFree) % Varno              = ops_odb_getval (h, "$cloud_frac_clear")

  ElemDesp2ODBDesp(IndexIN3DSdBrightnessTemp) % also_do(1:5)  = (/IndexSkinTemperature, &
                                                                  IndexRH,              &
                                                                  IndexTemp,            &
                                                                  IndexRH2,             &
                                                                  IndexT2/)
  ElemDesp2ODBDesp(IndexIN3DSdBrightnessTemp) % also_do_levels(1:5)  = (/1,43,43,1,1/)
END IF

IF (ObsGroup == ObsGroupCOMSMIClr) THEN
  ElemDesp2ODBDesp(IndexCOMSBrightnessTemp) % TableName     = 'body'
  ElemDesp2ODBDesp(IndexCOMSBrightnessTemp) % ColName       = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexCOMSBrightnessTemp) % Varno         = ops_odb_getval (h, "$rawbt")  ! brightness temperature

  ElemDesp2ODBDesp(IndexCOMSCloudFree) % TableName          = 'body'
  ElemDesp2ODBDesp(IndexCOMSCloudFree) % ColName            = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexCOMSCloudFree) % Varno              = ops_odb_getval (h, "$cloud_frac_clear")

  ElemDesp2ODBDesp(IndexCOMSBrightnessTemp) % also_do(1:5)  = (/IndexSkinTemperature, &
                                                                IndexRH,              &
                                                                IndexTemp,            &
                                                                IndexRH2,             &
                                                                IndexT2/)
  ElemDesp2ODBDesp(IndexCOMSBrightnessTemp) % also_do_levels(1:5)  = (/1,43,43,1,1/)
END IF

ElemDesp2ODBDesp(IndexIASIScaledRadiance) % TableName         = 'body'
ElemDesp2ODBDesp(IndexIASIScaledRadiance) % ColName           = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexIASIScaledRadiance) % Varno             = ops_odb_getval (h, "$rawsca")
ElemDesp2ODBDesp(IndexIASIScaledRadiance) % also_do(1:6)      = (/IndexSkinTemperature,     &
                                                                  IndexTemp,                &
                                                                  IndexRH,                  &
                                                                  IndexRH2,                 &
                                                                  IndexT2,                  &
                                                                  IndexIASIBrightnessTemp/)
ElemDesp2ODBDesp(IndexIASIScaledRadiance) % also_do_levels(1:6) = (/1,43,43,1,1,314/)

ElemDesp2ODBDesp(IndexIASIBrightnessTemp) % TableName         = 'body'
ElemDesp2ODBDesp(IndexIASIBrightnessTemp) % ColName           = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexIASIBrightnessTemp) % Varno             = ops_odb_getval (h, "$rawbt")

ElemDesp2ODBDesp(IndexRadiance) % TableName                   = 'body'
ElemDesp2ODBDesp(IndexRadiance) % ColName                     = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexRadiance) % Varno                       = ops_odb_getval (h, "$rawsca")

ElemDesp2ODBDesp(IndexAIRSBrightnessTemp) % TableName         = 'body'
ElemDesp2ODBDesp(IndexAIRSBrightnessTemp) % ColName           = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexAIRSBrightnessTemp) % Varno             = ops_odb_getval (h, "$rawbt")
ElemDesp2ODBDesp(IndexAIRSBrightnessTemp) % also_do(1:5)      = (/IndexRH,              &
                                                                  IndexTemp,            &
                                                                  IndexRH2,             &
                                                                  IndexT2,              &
                                                                  IndexSkinTemperature/)
ElemDesp2ODBDesp(IndexAIRSBrightnessTemp) % also_do_levels(1:5) = (/43,43,1,1,1/)

IF (ObsGroup == ObsGroupSSMIS) THEN
  ElemDesp2ODBDesp(IndexSSMISBrightnessTemp) % TableName      = 'body'
  ElemDesp2ODBDesp(IndexSSMISBrightnessTemp) % ColName        = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexSSMISBrightnessTemp) % Varno          = ops_odb_getval (h, "$rawbt")  ! brightness temperature
  ElemDesp2ODBDesp(IndexSSMISBrightnessTemp) % also_do(1:9)   = (/IndexCLW,             &
                                                                  Indexfff,             &
                                                                  IndexSkinTemperature, &
                                                                  IndexRH,              &
                                                                  IndexTemp,            &
                                                                  IndexRH2,             &
                                                                  IndexT2,              &
                                                                  IndexLWP,             &
                                                                  IndexTCWV/)
  ElemDesp2ODBDesp(IndexSSMISBrightnessTemp) % also_do_levels(1:9) = (/43,1,1,43,43,1,1,1,1/)
END IF

IF (ObsGroup == ObsGroupSatwind) THEN
  ElemDesp2ODBDesp(IndexPlevelsA) % TableName                 = 'body'
  ElemDesp2ODBDesp(IndexPlevelsA) % ColName                   = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexPlevelsA) % Varno                     = ops_odb_getval (h, "$cloud_top_press")
END IF

! Pressures...

ElemDesp2ODBDesp(IndexChannelNumber) % TableName              = 'body'
ElemDesp2ODBDesp(IndexChannelNumber) % ColName                = 'initial_vertco_reference'
ElemDesp2ODBDesp(IndexChannelNumber) % Pressno                = vertco_type_cha_number
IF (ObsGroup == ObsGroupHLOSWind) THEN
  ElemDesp2ODBDesp(IndexChannelNumber) % related_varnos(1)    = ops_odb_getval (h, "$los")
ELSE
  ElemDesp2ODBDesp(IndexChannelNumber) % related_varnos(1)    = ops_odb_getval (h, "$rawbt")
END IF

ElemDesp2ODBDesp(IndexDepth) % TableName                      = 'body'
ElemDesp2ODBDesp(IndexDepth) % ColName                        = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexDepth) % Varno                          = ops_odb_getval (h, "$pressure")

ElemDesp2ODBDesp(IndexArgoWaterPressure) % TableName          = 'body'
ElemDesp2ODBDesp(IndexArgoWaterPressure) % ColName            = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexArgoWaterPressure) % Varno              = ops_odb_getval (h, "$pressure")

! Others

IF (ObsGroup == ObsGroupAircraft .OR. ObsGroup == ObsGroupSonde) THEN
  ElemDesp2ODBDesp(IndexPLevelsA) % TableName                 = 'body'
  ElemDesp2ODBDesp(IndexPLevelsA) % ColName                   = 'initial_vertco_reference'
  ElemDesp2ODBDesp(IndexPLevelsA) % Pressno                   = vertco_type_pressure
  ElemDesp2ODBDesp(IndexPLevelsA) % related_varnos(1:9)       = (/ops_odb_getval (h, "$q"),  &
                                                                  ops_odb_getval (h, "$t"),  &
                                                                  ops_odb_getval (h, "$td"), &
                                                                  ops_odb_getval (h, "$dd"), &
                                                                  ops_odb_getval (h, "$ff"), &
                                                                  ops_odb_getval (h, "$z"),  &
                                                                  ops_odb_getval (h, "$rh"), &
                                                                  ops_odb_getval (h, "$u"),  &
                                                                  ops_odb_getval (h, "$v")/)
END IF

ElemDesp2ODBDesp(IndexLEVL_IDNY) % TableName                  = 'conv_body'
ElemDesp2ODBDesp(IndexLEVL_IDNY) % ColName                    = 'level'
ElemDesp2ODBDesp(IndexLEVL_IDNY) % related_varnos(1:5)        = (/ops_odb_getval (h, "$z"),   &
                                                                  ops_odb_getval (h, "$t"),   &
                                                                  ops_odb_getval (h, "$td"),  &
                                                                  ops_odb_getval (h, "$dd"),  &
                                                                  ops_odb_getval (h, "$ff")/)

ElemDesp2ODBDesp(IndexQC_USA_Wind) % TableName                = 'conv_body'
ElemDesp2ODBDesp(IndexQC_USA_Wind) % ColName                  = 'winpro_flags_usa'
ElemDesp2ODBDesp(IndexQC_USA_Wind) % related_varnos(1:3)      = (/ops_odb_getval (h, "$z"),   &
                                                                  ops_odb_getval (h, "$dd"),  &
                                                                  ops_odb_getval (h, "$ff")/)

ElemDesp2ODBDesp(IndexQC_ERPN_UV_Wind) % TableName            = 'conv_body'
ElemDesp2ODBDesp(IndexQC_ERPN_UV_Wind) % ColName              = 'winpro_flags_eu'
ElemDesp2ODBDesp(IndexQC_ERPN_UV_Wind) % related_varnos(1:3)  = (/ops_odb_getval (h, "$z"),   &
                                                                  ops_odb_getval (h, "$dd"),  &
                                                                  ops_odb_getval (h, "$ff")/)

IF (ObsGroup == ObsGroupAIRS) THEN
  ElemDesp2ODBDesp(IndexAMSU_EMSY_PMTR) % TableName           = 'sat_body'
  ElemDesp2ODBDesp(IndexAMSU_EMSY_PMTR) % ColName             = 'airs_emissivity_param'
  ElemDesp2ODBDesp(IndexAMSU_EMSY_PMTR) % related_varnos(1)   = ops_odb_getval (h, "$rawbt_amsu")
END IF

ElemDesp2ODBDesp(IndexLevelLat) % TableName                 = 'body'
ElemDesp2ODBDesp(IndexLevelLat) % ColName                   = 'initial_level_lat'
ElemDesp2ODBDesp(IndexLevelLat) % related_varnos(1:10)       = (/ops_odb_getval (h, "$q"),           &
                                                                 ops_odb_getval (h, "$t"),           &
                                                                 ops_odb_getval (h, "$td"),          &
                                                                 ops_odb_getval (h, "$dd"),          &
                                                                 ops_odb_getval (h, "$ff"),          &
                                                                 ops_odb_getval (h, "$u"),           &
                                                                 ops_odb_getval (h, "$v"),           &
                                                                 ops_odb_getval (h, "$z"),           &
                                                                 ops_odb_getval (h, "$mean_freq"),   &
                                                                 ops_odb_getval (h, "$bend_angle")/)

ElemDesp2ODBDesp(IndexLevelLon) % TableName                 = 'body'
ElemDesp2ODBDesp(IndexLevelLon) % ColName                   = 'initial_level_lon'
ElemDesp2ODBDesp(IndexLevelLon) % related_varnos(1:10)       = (/ops_odb_getval (h, "$q"),           &
                                                                 ops_odb_getval (h, "$t"),           &
                                                                 ops_odb_getval (h, "$td"),          &
                                                                 ops_odb_getval (h, "$dd"),          &
                                                                 ops_odb_getval (h, "$ff"),          &
                                                                 ops_odb_getval (h, "$u"),           &
                                                                 ops_odb_getval (h, "$v"),           &
                                                                 ops_odb_getval (h, "$z"),           &
                                                                 ops_odb_getval (h, "$mean_freq"),   &
                                                                 ops_odb_getval (h, "$bend_angle")/)

ElemDesp2ODBDesp(IndexLevelTime) % TableName                = 'body'
ElemDesp2ODBDesp(IndexLevelTime) % ColName                  = 'initial_level_time'
ElemDesp2ODBDesp(IndexLevelTime) % related_varnos(1:10)      = (/ops_odb_getval (h, "$q"),           &
                                                                 ops_odb_getval (h, "$t"),           &
                                                                 ops_odb_getval (h, "$td"),          &
                                                                 ops_odb_getval (h, "$dd"),          &
                                                                 ops_odb_getval (h, "$ff"),          &
                                                                 ops_odb_getval (h, "$z"),           &
                                                                 ops_odb_getval (h, "$u"),           &
                                                                 ops_odb_getval (h, "$v"),           &
                                                                 ops_odb_getval (h, "$mean_freq"),   &
                                                                 ops_odb_getval (h, "$bend_angle")/)

ElemDesp2ODBDesp(IndexLevelAzim) % TableName                = 'body'
ElemDesp2ODBDesp(IndexLevelAzim) % ColName                  = 'level_azimuth'
ElemDesp2ODBDesp(IndexLevelAzim) % related_varnos(1:3)      = (/ops_odb_getval (h, "$q"),           &
                                                                ops_odb_getval (h, "$mean_freq"),   &
                                                                ops_odb_getval (h, "$bend_angle")/)

ElemDesp2ODBDesp(IndexRainRate) % TableName                 = 'body'
ElemDesp2ODBDesp(IndexRainRate) % ColName                   = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexRainRate) % Varno                     = ops_odb_getval (h, "$prc")

ElemDesp2ODBDesp(IndexRainAccum) % TableName                = 'body'
ElemDesp2ODBDesp(IndexRainAccum) % ColName                  = 'obsvalue'
ElemDesp2ODBDesp(IndexRainAccum) % Varno                    = ops_odb_getval (h, "$rr")

ElemDesp2ODBDesp(IndexRainAccumError) % TableName           = 'body'
ElemDesp2ODBDesp(IndexRainAccumError) % ColName             = 'std_dev'
ElemDesp2ODBDesp(IndexRainAccumError) % related_varnos(1:1) = (/ops_odb_getval (h, "$rr")/)

ElemDesp2ODBDesp(IndexCloudOpticalThickness) % TableName    = 'body'
ElemDesp2ODBDesp(IndexCloudOpticalThickness) % ColName      = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexCloudOpticalThickness) % Varno        = ops_odb_getval (h, "$cod")

ElemDesp2ODBDesp(IndexTCWV) % TableName                     = 'body'
ElemDesp2ODBDesp(IndexTCWV) % ColName                       = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexTCWV) % Varno                         = ops_odb_getval (h, "$tcwv")

ElemDesp2ODBDesp(IndexRadarRainWeight) % TableName          = 'body'
ElemDesp2ODBDesp(IndexRadarRainWeight) % ColName            = 'radar_weight'
ElemDesp2ODBDesp(IndexRadarRainWeight) % related_varnos(1)  = ops_odb_getval (h, "$prc")

ElemDesp2ODBDesp(IndexRadarArcVal) % TableName              = 'body'
ElemDesp2ODBDesp(IndexRadarArcVal) % ColName                = 'radar_arcval'
ElemDesp2ODBDesp(IndexRadarArcVal) % related_varnos(1)      = ops_odb_getval (h, "$prc")

ElemDesp2ODBDesp(IndexSBUVLevelOzone) % TableName           = 'body'
ElemDesp2ODBDesp(IndexSBUVLevelOzone) % ColName             = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexSBUVLevelOzone) % varno               = ops_odb_getval (h, "$o3lay")

ElemDesp2ODBDesp(IndexSBUVUppLayerP) % TableName            = 'body'
ElemDesp2ODBDesp(IndexSBUVUppLayerP) % ColName              = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexSBUVUppLayerP) % varno                = ops_odb_getval (h, "$upper_layer_p")

ElemDesp2ODBDesp(IndexSBUVLowLayerP) % TableName            = 'body'
ElemDesp2ODBDesp(IndexSBUVLowLayerP) % ColName              = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexSBUVLowLayerP) % varno                = ops_odb_getval (h, "$lower_layer_p")

ElemDesp2ODBDesp(IndexSBUVTangentH) % TableName             = 'body'
ElemDesp2ODBDesp(IndexSBUVTangentH) % ColName               = 'initial_vertco_reference'
ElemDesp2ODBDesp(IndexSBUVTangentH) % Pressno               = vertco_type_tangent_height
ElemDesp2ODBDesp(IndexSBUVTangentH) % related_varnos(1:3)   = (/ops_odb_getval (h, "$o3lay"),          &
                                                                ops_odb_getval (h, "$upper_layer_p"),  &
                                                                ops_odb_getval (h, "$lower_layer_p")/)

ElemDesp2ODBDesp(IndexSBUVStDevOzone) % TableName           = 'body'
ElemDesp2ODBDesp(IndexSBUVStDevOzone) % ColName             = 'std_dev'
ElemDesp2ODBDesp(IndexSBUVStDevOzone) % related_varnos(1:1) = (/ops_odb_getval (h, "$o3lay")/)

ElemDesp2ODBDesp(IndexAOD) % TableName                      = 'body'
ElemDesp2ODBDesp(IndexAOD) % ColName                        = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexAOD) % varno                          = ops_odb_getval (h, "$aerod")

ElemDesp2ODBDesp(IndexAODError) % TableName                 = 'body'
ElemDesp2ODBDesp(IndexAODError) % ColName                   = 'std_dev'
ElemDesp2ODBDesp(IndexAODError) % related_varnos(1:1)       = (/ops_odb_getval (h, "$aerod")/)

IF (ObsGroup == ObsGroupAMSR) THEN
  ElemDesp2ODBDesp(IndexAMSRBrightnessTemp) % TableName     = 'body'
  ElemDesp2ODBDesp(IndexAMSRBrightnessTemp) % ColName       = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexAMSRBrightnessTemp) % Varno         = ops_odb_getval (h, "$rawbt")  ! brightness temperature

  ElemDesp2ODBDesp(IndexAMSR_BRGTS_TMPR_GHZ89) % TableName  = 'body'
  ElemDesp2ODBDesp(IndexAMSR_BRGTS_TMPR_GHZ89) % ColName    = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexAMSR_BRGTS_TMPR_GHZ89) % Varno      = ops_odb_getval (h, "$rawbt_amsr_89ghz")  ! brightness temperature

  ElemDesp2ODBDesp(IndexChnlQlty) % TableName               = 'sat_body'
  ElemDesp2ODBDesp(IndexChnlQlty) % ColName                 = 'channel_quality_flags'
  ElemDesp2ODBDesp(IndexChnlQlty) % related_varnos(1:1)     = (/ops_odb_getval (h, "$rawbt")/)

  ElemDesp2ODBDesp(IndexAMSR_ChnlQlty_GHZ89) % TableName           = 'sat_body'
  ElemDesp2ODBDesp(IndexAMSR_ChnlQlty_GHZ89) % ColName             = 'channel_quality_flags'
  ElemDesp2ODBDesp(IndexAMSR_ChnlQlty_GHZ89) % related_varnos(1:1) = (/ops_odb_getval (h, "$rawbt_amsr_89ghz")/)
END IF

IF (ObsGroup == ObsGroupSAPHIR) THEN
  ElemDesp2ODBDesp(IndexSAPHIRBrightnessTemp) % TableName = 'body'
  ElemDesp2ODBDesp(IndexSAPHIRBrightnessTemp) % ColName   = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexSAPHIRBrightnessTemp) % Varno     = ops_odb_getval (h, "$rawbt")  ! brightness temperature

  ElemDesp2ODBDesp(IndexChnlQlty) % TableName               = 'sat_body'
  ElemDesp2ODBDesp(IndexChnlQlty) % ColName                 = 'channel_quality_flags'
  ElemDesp2ODBDesp(IndexChnlQlty) % related_varnos(1:1)     = (/ops_odb_getval (h, "$rawbt")/)
END IF

ElemDesp2ODBDesp(IndexSeaIce) % TableName                   = 'body'
ElemDesp2ODBDesp(IndexSeaIce) % ColName                     = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexSeaIce) % varno                       = ops_odb_getval (h, "$sea_ice")

ElemDesp2ODBDesp(IndexSSTStdev) % TableName                 = 'body'
ElemDesp2ODBDesp(IndexSSTStdev) % ColName                   = 'std_dev'
ElemDesp2ODBDesp(IndexSSTStdev) % related_varnos(1)         = ops_odb_getval (h, "$tsts")


ElemDesp2ODBDesp(IndexMVIRIBrightnessTemp1) % TableName           = 'body'
ElemDesp2ODBDesp(IndexMVIRIBrightnessTemp1) % ColName             = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexMVIRIBrightnessTemp1) % varno               = ops_odb_getval (h, "$rawbt")
ElemDesp2ODBDesp(IndexMVIRIBrightnessTemp1) % fixed_vertco_value  = 1
ElemDesp2ODBDesp(IndexMVIRIBrightnessTemp1) % fixed_vertco_type   = ops_odb_getval (h, "$cha_number")
ElemDesp2ODBDesp(IndexMVIRIBrightnessTemp1) % also_do(1:6)        = (/IndexPstar,           &
                                                                      IndexSkinTemperature, &
                                                                      IndexRH,              &
                                                                      IndexTemp,            &
                                                                      IndexRH2,             &
                                                                      IndexT2/)
ElemDesp2ODBDesp(IndexMVIRIBrightnessTemp1) % also_do_levels(1:6) = (/1,1,43,43,1,1/)

ElemDesp2ODBDesp(IndexMVIRIBrightnessTemp2) % TableName           = 'body'
ElemDesp2ODBDesp(IndexMVIRIBrightnessTemp2) % ColName             = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexMVIRIBrightnessTemp2) % varno               = ops_odb_getval (h, "$rawbt")
ElemDesp2ODBDesp(IndexMVIRIBrightnessTemp2) % fixed_vertco_value  = 2
ElemDesp2ODBDesp(IndexMVIRIBrightnessTemp2) % fixed_vertco_type   = ops_odb_getval (h, "$cha_number")

ElemDesp2ODBDesp(IndexMVIRIBrightnessTemp3) % TableName           = 'body'
ElemDesp2ODBDesp(IndexMVIRIBrightnessTemp3) % ColName             = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexMVIRIBrightnessTemp3) % varno               = ops_odb_getval (h, "$rawbt")
ElemDesp2ODBDesp(IndexMVIRIBrightnessTemp3) % fixed_vertco_value  = 3
ElemDesp2ODBDesp(IndexMVIRIBrightnessTemp3) % fixed_vertco_type   = ops_odb_getval (h, "$cha_number")

ElemDesp2ODBDesp(IndexMVIRICloudFrac) % TableName           = 'body'
ElemDesp2ODBDesp(IndexMVIRICloudFrac) % ColName             = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexMVIRICloudFrac) % varno               = ops_odb_getval (h, "$cloud_frac_covered")

ElemDesp2ODBDesp(IndexLwstClod) % TableName                 = 'body'
ElemDesp2ODBDesp(IndexLwstClod) % ColName                   = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexLwstClod) % varno                     = ops_odb_getval (h, "$nn")
IF (ObsGroup == ObsGroupSurfaceCloud) THEN
  ElemDesp2ODBDesp(IndexLwstClod) % also_do(1)              = IndexLevelCloud
  ElemDesp2ODBDesp(IndexLwstClod) % also_do_levels(1)       = num_cloud_vars
END IF

ElemDesp2ODBDesp(IndexLevelCloud) % TableName               = 'body'
ElemDesp2ODBDesp(IndexLevelCloud) % ColName                 = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexLevelCloud) % varno                   = ops_odb_getval (h, "$level_cloud")

ElemDesp2ODBDesp(IndexLwstClodType) % TableName             = 'body'
ElemDesp2ODBDesp(IndexLwstClodType) % ColName               = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexLwstClodType) % varno                 = ops_odb_getval (h, "$cl")

ElemDesp2ODBDesp(IndexMediumCloudType) % TableName          = 'body'
ElemDesp2ODBDesp(IndexMediumCloudType) % ColName            = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexMediumCloudType) % varno              = ops_odb_getval (h, "$cm")

ElemDesp2ODBDesp(IndexHighCloudType) % TableName            = 'body'
ElemDesp2ODBDesp(IndexHighCloudType) % ColName              = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexHighCloudType) % varno                = ops_odb_getval (h, "$ch")

ElemDesp2ODBDesp(IndexCloudAmount8Group) % TableName        = 'body'
ElemDesp2ODBDesp(IndexCloudAmount8Group) % ColName          = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexCloudAmount8Group) % Varno            = ops_odb_getval (h, "$ns")

ElemDesp2ODBDesp(IndexCloudType8Group) % TableName          = 'body'
ElemDesp2ODBDesp(IndexCloudType8Group) % ColName            = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexCloudType8Group) % Varno              = ops_odb_getval (h, "$c")

ElemDesp2ODBDesp(IndexCloudBaseHeight8Group) % TableName    = 'body'
ElemDesp2ODBDesp(IndexCloudBaseHeight8Group) % ColName      = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexCloudBaseHeight8Group) % Varno        = ops_odb_getval (h, "$hshs")

ElemDesp2ODBDesp(IndexMixingRatio) % TableName              = 'body'
ElemDesp2ODBDesp(IndexMixingRatio) % ColName                = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexMixingRatio) % Varno                  = ops_odb_getval (h, "$level_mixing_ratio")

ElemDesp2ODBDesp(IndexRadialVel) % TableName                = 'body'
ElemDesp2ODBDesp(IndexRadialVel) % ColName                  = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexRadialVel) % varno                    = ops_odb_getval (h, "$radial_velocity")

ElemDesp2ODBDesp(IndexWindGust) % TableName                 = 'body'
ElemDesp2ODBDesp(IndexWindGust) % ColName                   = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexWindGust) % varno                     = ops_odb_getval (h, "$wind_gust")

ElemDesp2ODBDesp(IndexMASS_DNSTY) % TableName               = 'body'
ElemDesp2ODBDesp(IndexMASS_DNSTY) % ColName                 = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexMASS_DNSTY) % varno                   = ops_odb_getval (h, "$mass_density")
ElemDesp2ODBDesp(IndexMASS_DNSTY) % also_do(1:6)            = (/IndexSO2_DNSTY,   &
                                                                IndexPM10_DNSTY,  &
                                                                IndexPM2p5_DNSTY, &
                                                                IndexO3_DNSTY,    &
                                                                IndexNO2_DNSTY,   &
                                                                IndexCO_DNSTY/)

ElemDesp2ODBDesp(IndexSO2_DNSTY) % TableName                = 'body'
ElemDesp2ODBDesp(IndexSO2_DNSTY) % ColName                  = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexSO2_DNSTY) % varno                    = ops_odb_getval (h, "$so2_density")

ElemDesp2ODBDesp(IndexPM10_DNSTY) % TableName               = 'body'
ElemDesp2ODBDesp(IndexPM10_DNSTY) % ColName                 = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexPM10_DNSTY) % varno                   = ops_odb_getval (h, "$pm10_density")

ElemDesp2ODBDesp(IndexPM2p5_DNSTY) % TableName              = 'body'
ElemDesp2ODBDesp(IndexPM2p5_DNSTY) % ColName                = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexPM2p5_DNSTY) % varno                  = ops_odb_getval (h, "$pm2p5_density")

ElemDesp2ODBDesp(IndexO3_DNSTY) % TableName                 = 'body'
ElemDesp2ODBDesp(IndexO3_DNSTY) % ColName                   = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexO3_DNSTY) % varno                     = ops_odb_getval (h, "$o3_density")

ElemDesp2ODBDesp(IndexNO2_DNSTY) % TableName                = 'body'
ElemDesp2ODBDesp(IndexNO2_DNSTY) % ColName                  = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexNO2_DNSTY) % varno                    = ops_odb_getval (h, "$no2_density")

ElemDesp2ODBDesp(IndexCO_DNSTY) % TableName                 = 'body'
ElemDesp2ODBDesp(IndexCO_DNSTY) % ColName                   = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexCO_DNSTY) % varno                     = ops_odb_getval (h, "$co_density")

ElemDesp2ODBDesp(IndexAtmrNmbr) % TableName                 = 'body'
ElemDesp2ODBDesp(IndexAtmrNmbr) % ColName                   = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexAtmrNmbr) % varno                     = ops_odb_getval (h, "$atmosphere_number")

ElemDesp2ODBDesp(IndexPrecip1hr) % TableName                = 'body'
ElemDesp2ODBDesp(IndexPrecip1hr) % ColName                  = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexPrecip1hr) % varno                    = ops_odb_getval (h, "$rr")

ElemDesp2ODBDesp(IndexSGNT_WAVE_HGHT) % TableName           = 'body'
ElemDesp2ODBDesp(IndexSGNT_WAVE_HGHT) % ColName             = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexSGNT_WAVE_HGHT) % varno               = ops_odb_getval (h, "$hwhw")

ElemDesp2ODBDesp(IndexPLevelDer) % TableName                = 'body'
ElemDesp2ODBDesp(IndexPLevelDer) % ColName                  = 'vertco_reference_1'
ElemDesp2ODBDesp(IndexPLevelDer) % Pressno                  = vertco_type_derived_pressure

ElemDesp2ODBDesp(IndexTempQC) % TableName                   = 'body'
ElemDesp2ODBDesp(IndexTempQC) % ColName                     = 'ops_datum_flags'
ElemDesp2ODBDesp(IndexTempQC) % related_varnos(1:1)         = (/ops_odb_getval (h, "$t")/)

ElemDesp2ODBDesp(IndexRHQC) % TableName                     = 'body'
ElemDesp2ODBDesp(IndexRHQC) % ColName                       = 'ops_datum_flags'
ElemDesp2ODBDesp(IndexRHQC) % related_varnos(1:1)           = (/ops_odb_getval (h, "$rh")/)

ElemDesp2ODBDesp(IndexRH2QC) % TableName                    = 'body'
ElemDesp2ODBDesp(IndexRH2QC) % ColName                      = 'ops_datum_flags'
ElemDesp2ODBDesp(IndexRH2QC) % related_varnos(1:1)          = (/ops_odb_getval (h, "$rh2m")/)

ElemDesp2ODBDesp(IndexHeightQC) % TableName                 = 'body'
ElemDesp2ODBDesp(IndexHeightQC) % ColName                   = 'ops_datum_flags'
ElemDesp2ODBDesp(IndexHeightQC) % related_varnos(1:1)       = (/ops_odb_getval (h, "$z")/)

ElemDesp2ODBDesp(IndexPmslQC) % TableName                   = 'body'
ElemDesp2ODBDesp(IndexPmslQC) % ColName                     = 'ops_datum_flags'
ElemDesp2ODBDesp(IndexPmslQC) % related_varnos(1:1)         = (/ops_odb_getval (h, "$pmsl")/)

ElemDesp2ODBDesp(IndexT2QC) % TableName                     = 'body'
ElemDesp2ODBDesp(IndexT2QC) % ColName                       = 'ops_datum_flags'
ElemDesp2ODBDesp(IndexT2QC) % related_varnos(1:1)           = (/ops_odb_getval (h, "$t2m")/)

ElemDesp2ODBDesp(IndexT2ObErr) % TableName                  = 'errstat'
ElemDesp2ODBDesp(IndexT2ObErr) % ColName                    = 'obs_error'
ElemDesp2ODBDesp(IndexT2ObErr) % related_varnos(1:1)        = (/ops_odb_getval (h, "$t2m")/)

ElemDesp2ODBDesp(IndexRH2ObErr) % TableName                 = 'errstat'
ElemDesp2ODBDesp(IndexRH2ObErr) % ColName                   = 'obs_error'
ElemDesp2ODBDesp(IndexRH2ObErr) % related_varnos(1:1)       = (/ops_odb_getval (h, "$rh2m")/)

ElemDesp2ODBDesp(IndexPmslObErr) % TableName                = 'errstat'
ElemDesp2ODBDesp(IndexPmslObErr) % ColName                  = 'obs_error'
ElemDesp2ODBDesp(IndexPmslObErr) % related_varnos(1:1)      = (/ops_odb_getval (h, "$pmsl")/)

ElemDesp2ODBDesp(IndexTempObErr) % TableName                = 'errstat'
ElemDesp2ODBDesp(IndexTempObErr) % ColName                  = 'obs_error'
ElemDesp2ODBDesp(IndexTempObErr) % related_varnos(1:1)      = (/ops_odb_getval (h, "$t")/)

ElemDesp2ODBDesp(IndexRHObErr) % TableName                  = 'errstat'
ElemDesp2ODBDesp(IndexRHObErr) % ColName                    = 'obs_error'
ElemDesp2ODBDesp(IndexRHObErr) % related_varnos(1:1)        = (/ops_odb_getval (h, "$rh")/)

ElemDesp2ODBDesp(IndexHeightObErr) % TableName              = 'errstat'
ElemDesp2ODBDesp(IndexHeightObErr) % ColName                = 'obs_error'
ElemDesp2ODBDesp(IndexHeightObErr) % related_varnos(1:1)    = (/ops_odb_getval (h, "$z")/)

ElemDesp2ODBDesp(IndexSalQC) % TableName                    = 'body'
ElemDesp2ODBDesp(IndexSalQC) % ColName                      = 'ops_datum_flags'
ElemDesp2ODBDesp(IndexSalQC) % related_varnos(1:1)          = (/ops_odb_getval (h, "$salinity")/)

ElemDesp2ODBDesp(IndexSSTQC) % TableName                    = 'body'
ElemDesp2ODBDesp(IndexSSTQC) % ColName                      = 'ops_datum_flags'
ElemDesp2ODBDesp(IndexSSTQC) % related_varnos(1:1)          = (/ops_odb_getval (h, "$tsts")/)

IF (ObsGroup == ObsGroupSurface) THEN
  ElemDesp2ODBDesp(IndexWind10QC) % TableName               = 'body'
  ElemDesp2ODBDesp(IndexWind10QC) % ColName                 = 'ops_datum_flags'
  ElemDesp2ODBDesp(IndexWind10QC) % related_varnos(1:2)     = (/ops_odb_getval (h, "$u10m"), ops_odb_getval (h, "$v10m")/)

  ElemDesp2ODBDesp(IndexWind10ObErr) % TableName            = 'errstat'
  ElemDesp2ODBDesp(IndexWind10ObErr) % ColName              = 'obs_error'
  ElemDesp2ODBDesp(IndexWind10ObErr) % related_varnos(1:2)  = (/ops_odb_getval (h, "$u10m"), ops_odb_getval (h, "$v10m")/)

ELSE
  ElemDesp2ODBDesp(IndexWindQC) % TableName                 = 'body'
  ElemDesp2ODBDesp(IndexWindQC) % ColName                   = 'ops_datum_flags'
  ElemDesp2ODBDesp(IndexWindQC) % related_varnos(1:2)       = (/ops_odb_getval (h, "$u"), ops_odb_getval (h, "$v")/)

  ElemDesp2ODBDesp(IndexWindObErr) % TableName              = 'errstat'
  ElemDesp2ODBDesp(IndexWindObErr) % ColName                = 'obs_error'
  ElemDesp2ODBDesp(IndexWindObErr) % related_varnos(1:2)    = (/ops_odb_getval (h, "$u"), ops_odb_getval (h, "$v")/)
END IF

ElemDesp2ODBDesp(IndexTempCor) % TableName                  = 'body'
ElemDesp2ODBDesp(IndexTempCor) % ColName                    = 'obsvalue'
ElemDesp2ODBDesp(IndexTempCor) % related_varnos(1:1)        = (/ops_odb_getval (h, "$t")/)

ElemDesp2ODBDesp(IndexHeightCor) % TableName                = 'body'
ElemDesp2ODBDesp(IndexHeightCor) % ColName                  = 'obsvalue'
ElemDesp2ODBDesp(IndexHeightCor) % related_varnos(1:1)      = (/ops_odb_getval (h, "$z")/)

ElemDesp2ODBDesp(IndexT2Cor) % TableName                    = 'body'
ElemDesp2ODBDesp(IndexT2Cor) % ColName                      = 'obsvalue'
ElemDesp2ODBDesp(IndexT2Cor) % related_varnos(1:1)          = (/ops_odb_getval (h, "$t2m")/)

ElemDesp2ODBDesp(IndexLghnStrk) % TableName                 = 'body'
ElemDesp2ODBDesp(IndexLghnStrk) % ColName                   = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexLghnStrk) % varno                     = ops_odb_getval (h, "$lightning")

ElemDesp2ODBDesp(IndexTAMDAR_qc_t) % TableName              = 'conv_body'
ElemDesp2ODBDesp(IndexTAMDAR_qc_t) % ColName                = 'tamdar_flag_qc_t'
ElemDesp2ODBDesp(IndexTAMDAR_qc_t) % related_varnos(1:2)    = (/ops_odb_getval (h, "$t"), ops_odb_getval (h, "$rh")/)

ElemDesp2ODBDesp(IndexTAMDAR_qc_ddd) % TableName            = 'conv_body'
ElemDesp2ODBDesp(IndexTAMDAR_qc_ddd) % ColName              = 'tamdar_flag_qc_dd'
ElemDesp2ODBDesp(IndexTAMDAR_qc_ddd) % related_varnos(1:4)   = (/ops_odb_getval (h, "$dd"), &
                                                                 ops_odb_getval (h, "$ff"), &
                                                                 ops_odb_getval (h, "$u"),  &
                                                                 ops_odb_getval (h, "$v")/)

ElemDesp2ODBDesp(IndexTAMDAR_qc_fff) % TableName            = 'conv_body'
ElemDesp2ODBDesp(IndexTAMDAR_qc_fff) % ColName              = 'tamdar_flag_qc_ff'
ElemDesp2ODBDesp(IndexTAMDAR_qc_fff) % related_varnos(1:4)   = (/ops_odb_getval (h, "$dd"), &
                                                                 ops_odb_getval (h, "$ff"), &
                                                                 ops_odb_getval (h, "$u"),  &
                                                                 ops_odb_getval (h, "$v")/)

ElemDesp2ODBDesp(IndexTAMDAR_qc_rh) % TableName             = 'conv_body'
ElemDesp2ODBDesp(IndexTAMDAR_qc_rh) % ColName               = 'tamdar_flag_qc_rh'
ElemDesp2ODBDesp(IndexTAMDAR_qc_rh) % related_varnos(1:2)    = (/ops_odb_getval (h, "$t"), ops_odb_getval (h, "$rh")/)

ElemDesp2ODBDesp(IndexAUTOSATECA) % TableName                     = 'body'
ElemDesp2ODBDesp(IndexAUTOSATECA) % ColName                       = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexAUTOSATECA) % varno                         = ops_odb_getval (h, "$cloud_frac_covered")

ElemDesp2ODBDesp(IndexAUTOSATCTP) % TableName                     = 'body'
ElemDesp2ODBDesp(IndexAUTOSATCTP) % ColName                       = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexAUTOSATCTP) % Varno                         = ops_odb_getval (h, "$lower_layer_p")
IF (ObsGroup == ObsGroupGeoCloud) THEN
  ElemDesp2ODBDesp(IndexAUTOSATCTP) % also_do(1)                  = IndexLevelCloud
  ElemDesp2ODBDesp(IndexAUTOSATCTP) % also_do_levels(1)           = num_cloud_vars
END IF

ElemDesp2ODBDesp(IndexTemperatureRadianceLog) % TableName         = 'sat_body'
ElemDesp2ODBDesp(IndexTemperatureRadianceLog) % ColName           = 'temperature_radiance_log'
ElemDesp2ODBDesp(IndexTemperatureRadianceLog) % related_varnos(1) = ops_odb_getval (h, "$rawbt")

ElemDesp2ODBDesp(IndexBandwidthCorrectionCoeff1) % TableName         = 'sat_body'
ElemDesp2ODBDesp(IndexBandwidthCorrectionCoeff1) % ColName           = 'bandwidth_correction_coeff_1'
ElemDesp2ODBDesp(IndexBandwidthCorrectionCoeff1) % related_varnos(1) = ops_odb_getval (h, "$rawbt")

ElemDesp2ODBDesp(IndexBandwidthCorrectionCoeff2) % TableName         = 'sat_body'
ElemDesp2ODBDesp(IndexBandwidthCorrectionCoeff2) % ColName           = 'bandwidth_correction_coeff_2'
ElemDesp2ODBDesp(IndexBandwidthCorrectionCoeff2) % related_varnos(1) = ops_odb_getval (h, "$rawbt")

ElemDesp2ODBDesp(IndexCalibrationFlags) % TableName               = 'sat_body'
ElemDesp2ODBDesp(IndexCalibrationFlags) % ColName                 = 'calibration_flags'
ElemDesp2ODBDesp(IndexCalibrationFlags) % related_varnos(1)       = ops_odb_getval (h, "$rawbt")

ElemDesp2ODBDesp(IndexSunGlintIndicator) % TableName              = 'sat_body'
ElemDesp2ODBDesp(IndexSunGlintIndicator) % ColName                = 'sun_glint_indicator'
ElemDesp2ODBDesp(IndexSunGlintIndicator) % related_varnos(1)      = ops_odb_getval (h, "$rawbt")

ElemDesp2ODBDesp(IndexRefl) % TableName                           = 'body'
ElemDesp2ODBDesp(IndexRefl) % ColName                             = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexRefl) % Varno                               = ops_odb_getval (h, "$refl")

ElemDesp2ODBDesp(IndexCloudTopHeight) % TableName                 = 'body'
ElemDesp2ODBDesp(IndexCloudTopHeight) % ColName                   = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexCloudTopHeight) % Varno                     = ops_odb_getval (h, "$cloud_top_height")

ElemDesp2ODBDesp(IndexLidarnetRange) % TableName                  = 'body'
ElemDesp2ODBDesp(IndexLidarnetRange) % ColName                    = 'initial_vertco_reference'
ElemDesp2ODBDesp(IndexLidarnetRange) % Pressno                    = vertco_type_lidar_range
ElemDesp2ODBDesp(IndexLidarnetRange) % related_varnos(1:2)        = (/ops_odb_getval (h, "$libksc"),                   &
                                                                      ops_odb_getval (h, "$lidar_cloud_backscatter")/)

ElemDesp2ODBDesp(IndexRawBackscatter) % TableName                 = 'body'
ElemDesp2ODBDesp(IndexRawBackscatter) % ColName                   = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexRawBackscatter) % Varno                     = ops_odb_getval (h, "$libksc")

ElemDesp2ODBDesp(IndexBackscatter) % TableName                    = 'body'
ElemDesp2ODBDesp(IndexBackscatter) % ColName                      = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexBackscatter) % Varno                        = ops_odb_getval (h, "$lidar_cloud_backscatter")

ElemDesp2ODBDesp(IndexCBH) % TableName                            = 'body'
ElemDesp2ODBDesp(IndexCBH) % ColName                              = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexCBH) % Varno                                = ops_odb_getval (h, "$hshs")

ElemDesp2ODBDesp(IndexOpenRoadWindDirection) % TableName          = 'body'
ElemDesp2ODBDesp(IndexOpenRoadWindDirection) % ColName            = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexOpenRoadWindDirection) % varno              = ops_odb_getval (h, "$road_side_dd")
ElemDesp2ODBDesp(IndexOpenRoadWindDirection) % also_do(1:4)       = (/Indexddd10,Indexfff10,IndexUComponent10m, IndexVComponent10m/)
ElemDesp2ODBDesp(IndexOpenRoadWindDirection) % also_do_levels(1:4) = 1

ElemDesp2ODBDesp(IndexOpenRoadWindSpeed) % TableName              = 'body'
ElemDesp2ODBDesp(IndexOpenRoadWindSpeed) % ColName                = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexOpenRoadWindSpeed) % varno                  = ops_odb_getval (h, "$road_side_ff")
ElemDesp2ODBDesp(IndexOpenRoadWindSpeed) % also_do(1:4)           = (/Indexfff10,Indexddd10,IndexUComponent10m, IndexVComponent10m/)
ElemDesp2ODBDesp(IndexOpenRoadWindSpeed) % also_do_levels(1:4)    = 1

ElemDesp2ODBDesp(IndexOpenRoadTemp) % TableName                   = 'body'
ElemDesp2ODBDesp(IndexOpenRoadTemp) % ColName                     = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexOpenRoadTemp) % varno                       = ops_odb_getval (h, "$road_side_t2m")
ElemDesp2ODBDesp(IndexOpenRoadTemp) % also_do(1)                  = Indext2
ElemDesp2ODBDesp(IndexOpenRoadTemp) % also_do_levels(1)           = 1

ElemDesp2ODBDesp(IndexOpenRoadDewPointTemp) % TableName           = 'body'
ElemDesp2ODBDesp(IndexOpenRoadDewPointTemp) % ColName             = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexOpenRoadDewPointTemp) % varno               = ops_odb_getval (h, "$road_side_td2m")
ElemDesp2ODBDesp(IndexOpenRoadDewPointTemp) % also_do(1)          = Indextd2
ElemDesp2ODBDesp(IndexOpenRoadDewPointTemp) % also_do_levels(1)   = 1

ElemDesp2ODBDesp(IndexOpenRoadVisibility) % TableName             = 'body'
ElemDesp2ODBDesp(IndexOpenRoadVisibility) % ColName               = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexOpenRoadVisibility) % varno                 = ops_odb_getval (h, "$road_side_vv")
ElemDesp2ODBDesp(IndexOpenRoadVisibility) % also_do(1)            = IndexHorizVisib
ElemDesp2ODBDesp(IndexOpenRoadVisibility) % also_do_levels(1)     = 1

IF (ObsGroup == ObsGroupMWRI) THEN
  ElemDesp2ODBDesp(IndexBRGTS_TMPR1) % TableName           = 'body'
  ElemDesp2ODBDesp(IndexBRGTS_TMPR1) % ColName             = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexBRGTS_TMPR1) % Varno               = ops_odb_getval (h, "$rawbt")  ! brightness temperature

  ElemDesp2ODBDesp(IndexBRGTS_TMPR1) % also_do(1:5)        = (/IndexSkinTemperature, &
                                                                      IndexRH,              &
                                                                      IndexTemp,            &
                                                                      IndexRH2,             &
                                                                      IndexT2/)
  ElemDesp2ODBDesp(IndexBRGTS_TMPR1) % also_do_levels(1:5) = (/1,43,43,1,1/)
END IF

IF (ObsGroup == ObsGroupGMIlow .OR. ObsGroup == ObsGroupGMIhigh) THEN

  ElemDesp2ODBDesp(IndexGMIBrightnessTemp) % TableName    = 'body'
  ElemDesp2ODBDesp(IndexGMIBrightnessTemp) % ColName      = 'initial_obsvalue'
  ElemDesp2ODBDesp(IndexGMIBrightnessTemp) % Varno        = ops_odb_getval (h, "$rawbt")  ! brightness temperature
  ElemDesp2ODBDesp(IndexGMIBrightnessTemp) % also_do(1:5) = (/IndexSkinTemperature, & 
                                                              IndexRH,              &
                                                              IndexTemp,            &
                                                              IndexRH2,             &
                                                              IndexT2/)
  ElemDesp2ODBDesp(IndexGMIBrightnessTemp) % also_do_levels(1:5) = (/1,43,43,1,1/)

  ElemDesp2ODBDesp(IndexChnlQlty) % TableName                 = 'sat_body'
  ElemDesp2ODBDesp(IndexChnlQlty) % ColName                   = 'channel_quality_flags'
  ElemDesp2ODBDesp(IndexChnlQlty) % related_varnos(1:1)       = (/ops_odb_getval (h, "$rawbt")/)

END IF

ElemDesp2ODBDesp(IndexHLOSWIND) % TableName                   = 'body'
ElemDesp2ODBDesp(IndexHLOSWIND) % ColName                     = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexHLOSWIND) % Varno                       = ops_odb_getval (h, "$los")

ElemDesp2ODBDesp(IndexArgoPressureFlag) % TableName           = 'conv_body'
ElemDesp2ODBDesp(IndexArgoPressureFlag) % ColName             = 'argo_qc'
ElemDesp2ODBDesp(IndexArgoPressureFlag) % related_varnos(1:1) = (/ops_odb_getval (h, "$pressure")/)

ElemDesp2ODBDesp(IndexArgoTemperatureFlag) % TableName           = 'conv_body'
ElemDesp2ODBDesp(IndexArgoTemperatureFlag) % ColName             = 'argo_qc'
ElemDesp2ODBDesp(IndexArgoTemperatureFlag) % related_varnos(1:1) = (/ops_odb_getval (h, "$t")/)

ElemDesp2ODBDesp(IndexArgoSalinityFlag) % TableName           = 'conv_body'
ElemDesp2ODBDesp(IndexArgoSalinityFlag) % ColName             = 'argo_qc'
ElemDesp2ODBDesp(IndexArgoSalinityFlag) % related_varnos(1:1) = (/ops_odb_getval (h, "$salinity")/)

ElemDesp2ODBDesp(IndexCHL) % TableName                            = 'body'
ElemDesp2ODBDesp(IndexCHL) % ColName                              = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexCHL) % Varno                                = ops_odb_getval (h, "$chl")

ElemDesp2ODBDesp(IndexKD490) % TableName                          = 'body'
ElemDesp2ODBDesp(IndexKD490) % ColName                            = 'initial_obsvalue'
ElemDesp2ODBDesp(IndexKD490) % Varno                              = ops_odb_getval (h, "$kd490")

!-------------------------------------------------------------------------------
! 2. Tracing and end of routine
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE OpsODB_InitODBElemDesp_Body
