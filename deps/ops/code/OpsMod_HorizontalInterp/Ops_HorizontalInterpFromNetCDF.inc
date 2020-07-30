!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Horizontal interpolation.  Interpolates by linear interpolation, a 2-d field
! to a 2-d set of points.  Input data can be on a sphere or within a rectangular
! box. Requested output data points must lie within the region defined for the
! input data.
!
! i=x=lambda, j=y=phi
! assumption that nth point defined by:
!        (LambdaOrigin+(n-1)*DLambdaIn , PhiOrigin+(n-1)*DPhiIn)
! assumption that input and output in radians in range
!        (0>2pi , -pi/2>+pi/2)
! assumption that check for output positions outside input grid domain
!         is done externally
!
!-------------------------------------------------------------------------------

SUBROUTINE Ops_HorizontalInterpFromNetCDF (Obs,           &
                                           CX,            &
                                           BGECX,         &
                                           ts,            &
                                           is,            &
                                           js,            &
                                           netcdf_fields)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI

USE OpsMod_CXInfo, ONLY: &
    Cx_type

USE OpsMod_ModelIO, ONLY:   &
    OceanNetCDFFields_type

USE OpsMod_ObsGroupInfo, ONLY: &
    ObsGroupOcean,             &
    ObsGroupSurface,           &
    ObsGroupAltim,             &
    ObsGroupSeaIce,            &
    ObsGroupOceanColour,       &
    ObsGroupSatSST

USE OpsMod_ObsInfo, ONLY: &
    Ob_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (Ob_type), INTENT(INOUT)             :: Obs           ! Lambda coordinate of output data on input grid
TYPE (Cx_type), INTENT(INOUT)             :: CX            ! Data interpolated to desired locations.
TYPE (Cx_type), INTENT(INOUT)             :: BGECX         ! Data interpolated to desired locations.
INTEGER, INTENT(IN)                       :: ts(:)
INTEGER, INTENT(IN)                       :: is(:)
INTEGER, INTENT(IN)                       :: js(:)
TYPE (OceanNetCDFFields_type), INTENT(IN) :: netcdf_fields

! Local declarations:
CHARACTER(len=*), PARAMETER               :: RoutineName = "Ops_HorizontalInterpFromNetCDF"
INTEGER                                   :: i
REAL, ALLOCATABLE                         :: weights(:,:)
REAL, ALLOCATABLE                         :: t_weights(:,:)

! ----------------------------------------------------------------------
!  Section 1.   Initialize
! ----------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! Construct the horizontal weights once, we can reuse them for all fields
CALL Ops_HorizontalInterp_OceanWeights (Obs,           &
                                        is,            &
                                        js,            &
                                        netcdf_fields, &
                                        weights)

ALLOCATE (t_weights(2,obs % header % numobslocal))
CALL Ops_TimeInterp_OceanWeights (netcdf_fields % time_counter_adj, &
                                  Obs % Time,                       &
                                  ts,                               &
                                  t_weights)

IF (CX % header % ObsGroup == ObsGroupOcean) THEN
  CALL Ops_HorizontalInterpProfile (Obs,                                                &
                                    is,                                                 &
                                    js,                                                 &
                                    weights,                                            &
                                    netcdf_fields % gdept_0(:,:,:,1),                   &
                                    netcdf_fields % nav_lev(:),                         &
                                    netcdf_fields % votemper(:,:,:,:),                  &
                                    netcdf_fields % depth_in_s_levels,                  &
                                    CX % Theta,                                         &
                                    ts_in = ts,                                         &
                                    t_weights_in = t_weights,                           &
                                    reported_level_data = Obs % background_temperature)

  CALL Ops_HorizontalInterpProfile (Obs,                                         &
                                    is,                                          &
                                    js,                                          &
                                    weights,                                     &
                                    netcdf_fields % gdept_0(:,:,:,1),            &
                                    netcdf_fields % nav_lev(:),                  &
                                    netcdf_fields % vosaline(:,:,:,:),           &
                                    netcdf_fields % depth_in_s_levels,           &
                                    CX % Salt,                                   &
                                    ts_in = ts,                                  &
                                    t_weights_in = t_weights,                    &
                                    reported_level_data = Obs % background_salt)

  CALL Ops_HorizontalInterpProfile (Obs,                                &
                                    is,                                 &
                                    js,                                 &
                                    weights,                            &
                                    netcdf_fields % gdept_0(:,:,:,1),   &
                                    netcdf_fields % nav_lev(:),         &
                                    netcdf_fields % s_obs_var(:,:,:,:), &
                                    netcdf_fields % depth_in_s_levels,  &
                                    BGECX % SalOb_Err)

  CALL Ops_HorizontalInterpProfile (Obs,                                &
                                    is,                                 &
                                    js,                                 &
                                    weights,                            &
                                    netcdf_fields % gdept_0(:,:,:,1),   &
                                    netcdf_fields % nav_lev(:),         &
                                    netcdf_fields % s_mes_var(:,:,:,:), &
                                    netcdf_fields % depth_in_s_levels,  &
                                    BGECX % SalMes_Var)

  CALL Ops_HorizontalInterpProfile (Obs,                                &
                                    is,                                 &
                                    js,                                 &
                                    weights,                            &
                                    netcdf_fields % gdept_0(:,:,:,1),   &
                                    netcdf_fields % nav_lev(:),         &
                                    netcdf_fields % s_syn_var(:,:,:,:), &
                                    netcdf_fields % depth_in_s_levels,  &
                                    BGECX % SalSyn_Var)

  WHERE (BGECX % SalSyn_Var /= RMDI)
    BGECX % Salt = BGECX % SalSyn_Var + BGECX % SalMes_Var
  END WHERE

  CALL Ops_HorizontalInterpProfile (Obs,                                &
                                    is,                                 &
                                    js,                                 &
                                    weights,                            &
                                    netcdf_fields % gdept_0(:,:,:,1),   &
                                    netcdf_fields % nav_lev(:),         &
                                    netcdf_fields % t_obs_var(:,:,:,:), &
                                    netcdf_fields % depth_in_s_levels,  &
                                    BGECX % TempOb_Err)

  CALL Ops_HorizontalInterpProfile (Obs,                                &
                                    is,                                 &
                                    js,                                 &
                                    weights,                            &
                                    netcdf_fields % gdept_0(:,:,:,1),   &
                                    netcdf_fields % nav_lev(:),         &
                                    netcdf_fields % t_mes_var(:,:,:,:), &
                                    netcdf_fields % depth_in_s_levels,  &
                                    BGECX % TempMes_Var)

  CALL Ops_HorizontalInterpProfile (Obs,                                &
                                    is,                                 &
                                    js,                                 &
                                    weights,                            &
                                    netcdf_fields % gdept_0(:,:,:,1),   &
                                    netcdf_fields % nav_lev(:),         &
                                    netcdf_fields % t_syn_var(:,:,:,:), &
                                    netcdf_fields % depth_in_s_levels,  &
                                    BGECX % TempSyn_Var)

  WHERE (BGECX % TempSyn_Var /= RMDI)
    BGECX % Theta = BGECX % TempSyn_Var + BGECX % TempMes_Var
  END WHERE

ELSE IF (CX % header % ObsGroup == ObsGroupAltim) THEN

  DO i = 1, Obs % header % NumObsLocal

    IF ((is(i) < 1) .OR. (js(i) < 1)) CYCLE

    CX % SeaSrfcHeight(i) = OpsFn_ApplySurfaceOceanWeights (weights,                  &
                                                            netcdf_fields % sossheig, &
                                                            is,                       &
                                                            js,                       &
                                                            i,                        &
                                                            ts_in = ts,               &
                                                            t_weights_in = t_weights)

    BGECX % SSHOb_Err(i)  = OpsFn_ApplySurfaceOceanWeights (weights,                     &
                                                            netcdf_fields % ssh_obs_var, &
                                                            is,                          &
                                                            js,                          &
                                                            i)

    BGECX % SSHSyn_var(i) = OpsFn_ApplySurfaceOceanWeights (weights,                     &
                                                            netcdf_fields % ssh_syn_var, &
                                                            is,                          &
                                                            js,                          &
                                                            i)

    BGECX % SSHMes_var(i) = OpsFn_ApplySurfaceOceanWeights (weights,                     &
                                                            netcdf_fields % ssh_mes_var, &
                                                            is,                          &
                                                            js,                          &
                                                            i)

    BGECX % SeaSrfcHeight(i) = BGECX % SSHSyn_var(i) + BGECX % SSHMes_var(i)

    BGECX % MeanSeaHeight(i) = OpsFn_Apply2DOceanWeights (weights,                         &
                                                          netcdf_fields % mean_sea_height, &
                                                          is,                              &
                                                          js,                              &
                                                          i)
  END DO

ELSE IF (CX % header % ObsGroup == ObsGroupSeaIce) THEN

  DO i = 1, Obs % header % NumObsLocal

    IF ((is(i) < 1) .OR. (js(i) < 1)) CYCLE

    CX % SeaIce(i) = OpsFn_ApplySurfaceOceanWeights (weights,                  &
                                                     netcdf_fields % iiceconc, &
                                                     is,                       &
                                                     js,                       &
                                                     i,                        &
                                                     ts_in = ts,               &
                                                     t_weights_in = t_weights)

    BGECX % SeaIceOb_Err(i) = OpsFn_ApplySurfaceOceanWeights (weights,                        &
                                                              netcdf_fields % seaice_obs_var, &
                                                              is,                             &
                                                              js,                             &
                                                              i)

    BGECX % SeaIceSyn_var(i) = OpsFn_ApplySurfaceOceanWeights (weights,                        &
                                                               netcdf_fields % seaice_syn_var, &
                                                               is,                             &
                                                               js,                             &
                                                               i)

    BGECX % SeaIceMes_var(i) = OpsFn_ApplySurfaceOceanWeights (weights,                        &
                                                               netcdf_fields % seaice_mes_var, &
                                                               is,                             &
                                                               js,                             &
                                                               i)

  END DO

ELSE IF (CX % header % ObsGroup == ObsGroupOceanColour) THEN

  DO i = 1, Obs % header % NumObsLocal

    IF ((is(i) < 1) .OR. (js(i) < 1)) CYCLE

    IF (ALLOCATED (netcdf_fields % schltot)) THEN
      CX % CHL(i) = OpsFn_ApplySurfaceOceanWeights (weights,                  &
                                                    netcdf_fields % schltot,  &
                                                    is,                       &
                                                    js,                       &
                                                    i,                        &
                                                    ts_in = ts,               &
                                                    t_weights_in = t_weights)
    END IF

    IF (ALLOCATED (netcdf_fields % skd490tot)) THEN
      CX % KD490(i) = OpsFn_ApplySurfaceOceanWeights (weights,                   &
                                                      netcdf_fields % skd490tot, &
                                                      is,                        &
                                                      js,                        &
                                                      i,                         &
                                                      ts_in = ts,                &
                                                      t_weights_in = t_weights)
    END IF

    IF (ALLOCATED (netcdf_fields % chl_tot_var)) THEN
      BGECX % CHL(i) = OpsFn_ApplySurfaceOceanWeights (weights,                     &
                                                       netcdf_fields % chl_tot_var, &
                                                       is,                          &
                                                       js,                          &
                                                       i)
    END IF

    IF (ALLOCATED (netcdf_fields % chl_obs_var)) THEN
      BGECX % CHLOb_Err(i) = OpsFn_ApplySurfaceOceanWeights (weights,                     &
                                                             netcdf_fields % chl_obs_var, &
                                                             is,                          &
                                                             js,                          &
                                                             i)
    END IF

    IF (ALLOCATED (netcdf_fields % lchl_tot_var)) THEN
      BGECX % LCHL(i) = OpsFn_ApplySurfaceOceanWeights (weights,                      &
                                                        netcdf_fields % lchl_tot_var, &
                                                        is,                           &
                                                        js,                           &
                                                        i)
    END IF

    IF (ALLOCATED (netcdf_fields % lchl_obs_var)) THEN
      BGECX % LCHLOb_Err(i) = OpsFn_ApplySurfaceOceanWeights (weights,                      &
                                                              netcdf_fields % lchl_obs_var, &
                                                              is,                           &
                                                              js,                           &
                                                              i)
    END IF

    IF (ALLOCATED (netcdf_fields % kd490_tot_var)) THEN
      BGECX % KD490(i) = OpsFn_ApplySurfaceOceanWeights (weights,                       &
                                                         netcdf_fields % kd490_tot_var, &
                                                         is,                            &
                                                         js,                            &
                                                         i)
    END IF

    IF (ALLOCATED (netcdf_fields % kd490_obs_var)) THEN
      BGECX % KD490Ob_Err(i) = OpsFn_ApplySurfaceOceanWeights (weights,                       &
                                                               netcdf_fields % kd490_obs_var, &
                                                               is,                            &
                                                               js,                            &
                                                               i)
    END IF

    IF (ALLOCATED (netcdf_fields % lkd490_tot_var)) THEN
      BGECX % LKD490(i) = OpsFn_ApplySurfaceOceanWeights (weights,                        &
                                                          netcdf_fields % lkd490_tot_var, &
                                                          is,                             &
                                                          js,                             &
                                                          i)
    END IF

    IF (ALLOCATED (netcdf_fields % lkd490_obs_var)) THEN
      BGECX % LKD490Ob_Err(i) = OpsFn_ApplySurfaceOceanWeights (weights,                        &
                                                                netcdf_fields % lkd490_obs_var, &
                                                                is,                             &
                                                                js,                             &
                                                                i)
    END IF
  END DO

ELSE

  CALL Ops_HorizontalInterpProfile (Obs,                               &
                                    is,                                &
                                    js,                                &
                                    weights,                           &
                                    netcdf_fields % gdept_0(:,:,:,1),  &
                                    netcdf_fields % nav_lev(:),        &
                                    netcdf_fields % votemper(:,:,:,:), &
                                    netcdf_fields % depth_in_s_levels, &
                                    CX % Theta,                        &
                                    ts_in = ts,                        &
                                    t_weights_in = t_weights)

  CALL Ops_HorizontalInterpProfile (Obs,                                &
                                    is,                                 &
                                    js,                                 &
                                    weights,                            &
                                    netcdf_fields % gdept_0(:,:,:,1),   &
                                    netcdf_fields % nav_lev(:),         &
                                    netcdf_fields % t_obs_var(:,:,:,:), &
                                    netcdf_fields % depth_in_s_levels,  &
                                    BGECX % TempOb_Err)

  CALL Ops_HorizontalInterpProfile (Obs,                                &
                                    is,                                 &
                                    js,                                 &
                                    weights,                            &
                                    netcdf_fields % gdept_0(:,:,:,1),   &
                                    netcdf_fields % nav_lev(:),         &
                                    netcdf_fields % t_syn_var(:,:,:,:), &
                                    netcdf_fields % depth_in_s_levels,  &
                                    BGECX % TempSyn_var)

  CALL Ops_HorizontalInterpProfile (Obs,                                &
                                    is,                                 &
                                    js,                                 &
                                    weights,                            &
                                    netcdf_fields % gdept_0(:,:,:,1),   &
                                    netcdf_fields % nav_lev(:),         &
                                    netcdf_fields % t_mes_var(:,:,:,:), &
                                    netcdf_fields % depth_in_s_levels,  &
                                    BGECX % TempMes_var)

  WHERE (BGECX % TempSyn_Var /= RMDI)
    BGECX % Theta = BGECX % TempSyn_Var + BGECX % TempMes_Var
  END WHERE

END IF


IF (CX % header % ObsGroup == ObsGroupSatSST .OR. &
    CX % header % ObsGroup == ObsGroupSurface) THEN
  CX % header % SST % Present = .TRUE.
  CX % SST(:) = CX % Theta(:,1)

  BGECX % header % SST % Present = .TRUE.
  BGECX % SST(:) = BGECX % Theta(:,1)

  BGECX % header % SSTOb_err % Present = .TRUE.
  BGECX % SSTOb_err(:) = BGECX % TempOb_err(:,1)

  BGECX % header % SSTSyn_var % Present = .TRUE.
  BGECX % SSTSyn_var(:) = BGECX % TempSyn_Var(:,1)

  BGECX % header % SSTMes_var % Present = .TRUE.
  BGECX % SSTMes_var(:) = BGECX % TempMes_Var(:,1)

END IF

!------------------------------------------------------------------------------
! [5] Tidy up
!------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_HorizontalInterpFromNetCDF
