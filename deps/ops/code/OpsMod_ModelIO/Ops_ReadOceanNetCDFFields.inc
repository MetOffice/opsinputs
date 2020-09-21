!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Retrieve the NetCDF Background and Background error data
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ReadOceanNetCDFFields (ObsGroup)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  StatusOK,            &
  UseTrace

USE OpsMod_CharUtils, ONLY: &
  ops_to_lower_case

USE OpsMod_ObsGroupInfo, ONLY: &
  ObsGroupOcean,               &
  ObsGroupSurface,             &
  ObsGroupAltim,               &
  ObsGroupSeaIce,              &
  ObsGroupOceanColour,         &
  ObsGroupSatSST

USE ops_netcdf_wrappers, ONLY: &
  ops_netcdf_close,            &
  ops_netcdf_get_var,          &
  ops_netcdf_get_att,          &
  ops_netcdf_inq_dim_len,      &
  ops_netcdf_inq_varid,        &
  ops_netcdf_open

USE OpsMod_EnvUtils, ONLY: &
  ops_env_is_set,          &
  ops_env_is_true,         &
  ops_get_env

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)         :: ObsGroup

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = 'Ops_ReadOceanNetCDFFields'
CHARACTER(len=800)          :: messages(2)
CHARACTER(len=5000)         :: netcdf_bkg_filename
CHARACTER(len=5000)         :: netcdf_err_filename
CHARACTER(len=300)          :: bkg_time_origin
INTEGER                     :: bkg_file_handle
INTEGER                     :: err_file_handle
INTEGER                     :: var_id
INTEGER                     :: num_lats
INTEGER                     :: num_lons

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL ops_get_env ("OPS_UMBACK_NETCDF_BACKGROUND", &
                  netcdf_bkg_filename)
CALL ops_get_env ("OPS_UMBACK_NETCDF_BACKGROUND_ERROR", &
                  netcdf_err_filename)

CALL ops_netcdf_open (netcdf_bkg_filename, &
                      bkg_file_handle)
CALL ops_netcdf_inq_dim_len (bkg_file_handle,                          &
                             "time_counter",                           &
                             OceanNetCDFFields % netcdf_num_bkg_times)
ALLOCATE (OceanNetCDFFields % time_counter(OceanNetCDFFields % netcdf_num_bkg_times))
CALL ops_netcdf_get_var (bkg_file_handle,                  &
                         "time_counter",                   &
                         OceanNetCDFFields % time_counter)

! Retrieve reference time information from the variable metadata
CALL ops_netcdf_inq_varid (bkg_file_handle, &
                           "time_counter",  &
                           var_id)
CALL ops_netcdf_get_att (bkg_file_handle, &
                         "units",         &
                         bkg_time_origin, &
                         var_id)
bkg_time_origin = bkg_time_origin
IF (ops_to_lower_case (bkg_time_origin(1:13)) /= "seconds since") THEN
  messages(1) = "time units attribute not in seconds since ref time: "
  messages(2) = bkg_time_origin
  CALL gen_fail (RoutineName,   &
                 messages(1:2))
END IF
READ (bkg_time_origin(15:18), *) OceanNetCDFFields % ref_bkg_time % year  
READ (bkg_time_origin(20:21), *) OceanNetCDFFields % ref_bkg_time % month 
READ (bkg_time_origin(23:24), *) OceanNetCDFFields % ref_bkg_time % day   
READ (bkg_time_origin(26:27), *) OceanNetCDFFields % ref_bkg_time % hour  
READ (bkg_time_origin(29:30), *) OceanNetCDFFields % ref_bkg_time % minute
READ (bkg_time_origin(32:33), *) OceanNetCDFFields % ref_bkg_time % second

IF (OceanNetCDFFields % ref_bkg_time % year < 1970) THEN
  ! Temporary fix if using the CPLDA test file
  IF (OceanNetCDFFields % ref_bkg_time % year == 1900 .AND. &
      OceanNetCDFFields % ref_bkg_time % month == 1 .AND. &
      OceanNetCDFFields % ref_bkg_time % day == 1 .AND. &
      OceanNetCDFFields % ref_bkg_time % hour == 0 .AND. &
      OceanNetCDFFields % ref_bkg_time % minute == 0 .AND. &
      OceanNetCDFFields % ref_bkg_time % second == 0) THEN
    OceanNetCDFFields % time_counter(:) = OceanNetCDFFields % time_counter(:) - 2208988800
    OceanNetCDFFields % ref_bkg_time % year = 1970
  ! Temporary fix if using the ORCA12ext test file
  ELSE IF (OceanNetCDFFields % ref_bkg_time % year == 1950 .AND. &
      OceanNetCDFFields % ref_bkg_time % month == 1 .AND. &
      OceanNetCDFFields % ref_bkg_time % day == 1 .AND. &
      OceanNetCDFFields % ref_bkg_time % hour == 0 .AND. &
      OceanNetCDFFields % ref_bkg_time % minute == 0 .AND. &
      OceanNetCDFFields % ref_bkg_time % second == 0) THEN
    OceanNetCDFFields % time_counter(:) = OceanNetCDFFields % time_counter(:) - 631152000
    OceanNetCDFFields % ref_bkg_time % year = 1970
  ELSE
    messages(1) = "Ref time must be greater than 1970 for OPS DateTime routines to work!"
    messages(2) = bkg_time_origin
    CALL gen_fail (RoutineName,   &
                   messages(1:2))
  END IF
END IF

CALL ops_netcdf_inq_dim_len (bkg_file_handle, &
                             "y",             &
                             num_lats)
IF (num_lats /= OceanNetCDFFields % netcdf_num_lats) THEN
  WRITE (messages(1), '(A)') "Background latitude /= mesh file latitude: "
  WRITE (messages(2), '(I0,A,I0)') num_lats, "/=", OceanNetCDFFields % netcdf_num_lats 
  CALL gen_fail (RoutineName,   &
                 messages(1:2))
END IF
CALL ops_netcdf_inq_dim_len (bkg_file_handle, &
                             "x",             &
                             num_lons)
IF (num_lats /= OceanNetCDFFields % netcdf_num_lats) THEN
  WRITE (messages(1), '(A)') "Background longitude /= mesh file longitude: "
  WRITE (messages(2), '(I0,A,I0)') num_lons, "/=", OceanNetCDFFields % netcdf_num_lons 
  CALL gen_fail (RoutineName,   &
                 messages(1:2))
END IF

CALL ops_netcdf_open (netcdf_err_filename, &
                      err_file_handle)
CALL ops_netcdf_inq_dim_len (err_file_handle,                          &
                             "time_counter",                           &
                             OceanNetCDFFields % netcdf_num_err_times, &
                             status_if_dim_not_present = StatusOK)
IF (OceanNetCDFFields % netcdf_num_err_times <= 0) THEN
  CALL ops_netcdf_inq_dim_len (err_file_handle,                          &
                               "t",                                      &
                               OceanNetCDFFields % netcdf_num_err_times)
END IF
CALL ops_netcdf_inq_dim_len (err_file_handle, &
                             "y",             &
                             num_lats)
IF (num_lats /= OceanNetCDFFields % netcdf_num_lats) THEN
  WRITE (messages(1), '(A)') "Error latitude /= mesh file latitude: "
  WRITE (messages(2), '(I0,A,I0)') num_lats, "/=", OceanNetCDFFields % netcdf_num_lats 
  CALL gen_fail (RoutineName,   &
                 messages(1:2))
END IF
CALL ops_netcdf_inq_dim_len (err_file_handle, &
                             "x",             &
                             num_lons)
IF (num_lats /= OceanNetCDFFields % netcdf_num_lats) THEN
  WRITE (messages(1), '(A)') "Error longitude /= mesh file longitude: "
  WRITE (messages(2), '(I0,A,I0)') num_lons, "/=", OceanNetCDFFields % netcdf_num_lons 
  CALL gen_fail (RoutineName,   &
                 messages(1:2))
END IF

IF (ObsGroup == ObsGroupSatSST .OR. ObsGroup == ObsGroupOcean .OR. ObsGroup == ObsGroupSurface) THEN
  ALLOCATE (OceanNetCDFFields % votemper(OceanNetCDFFields % netcdf_num_lons,       &
                                         OceanNetCDFFields % netcdf_num_lats,       &
                                         OceanNetCDFFields % netcdf_num_depths,     &
                                         OceanNetCDFFields % netcdf_num_bkg_times))
  CALL ops_netcdf_get_var (bkg_file_handle,              &
                           "votemper",                   &
                           OceanNetCDFFields % votemper)
  ALLOCATE (OceanNetCDFFields % t_obs_var(OceanNetCDFFields % netcdf_num_lons,       &
                                          OceanNetCDFFields % netcdf_num_lats,       &
                                          OceanNetCDFFields % netcdf_num_depths,     &
                                          OceanNetCDFFields % netcdf_num_err_times))
  ALLOCATE (OceanNetCDFFields % t_syn_var(OceanNetCDFFields % netcdf_num_lons,       &
                                          OceanNetCDFFields % netcdf_num_lats,       &
                                          OceanNetCDFFields % netcdf_num_depths,     &
                                          OceanNetCDFFields % netcdf_num_err_times))
  ALLOCATE (OceanNetCDFFields % t_mes_var(OceanNetCDFFields % netcdf_num_lons,       &
                                          OceanNetCDFFields % netcdf_num_lats,       &
                                          OceanNetCDFFields % netcdf_num_depths,     &
                                          OceanNetCDFFields % netcdf_num_err_times))
  CALL ops_netcdf_get_var (err_file_handle,               &
                           "t_obs_var",                   &
                           OceanNetCDFFields % t_obs_var)
  CALL ops_netcdf_get_var (err_file_handle,               &
                           "t_syn_var",                   &
                           OceanNetCDFFields % t_syn_var)
  CALL ops_netcdf_get_var (err_file_handle,               &
                           "t_mes_var",                   &
                           OceanNetCDFFields % t_mes_var)
END IF

IF (ObsGroup == ObsGroupOcean) THEN
  ALLOCATE (OceanNetCDFFields % vosaline(OceanNetCDFFields % netcdf_num_lons,       &
                                         OceanNetCDFFields % netcdf_num_lats,       &
                                         OceanNetCDFFields % netcdf_num_depths,     &
                                         OceanNetCDFFields % netcdf_num_bkg_times))
  CALL ops_netcdf_get_var (bkg_file_handle,              &
                           "vosaline",                   &
                           OceanNetCDFFields % vosaline)
  OceanNetCDFFields % vosaline = (OceanNetCDFFields % vosaline - 35) / 1000
  ALLOCATE (OceanNetCDFFields % s_obs_var(OceanNetCDFFields % netcdf_num_lons,       &
                                          OceanNetCDFFields % netcdf_num_lats,       &
                                          OceanNetCDFFields % netcdf_num_depths,     &
                                          OceanNetCDFFields % netcdf_num_err_times))
  ALLOCATE (OceanNetCDFFields % s_syn_var(OceanNetCDFFields % netcdf_num_lons,       &
                                          OceanNetCDFFields % netcdf_num_lats,       &
                                          OceanNetCDFFields % netcdf_num_depths,     &
                                          OceanNetCDFFields % netcdf_num_err_times))
  ALLOCATE (OceanNetCDFFields % s_mes_var(OceanNetCDFFields % netcdf_num_lons,       &
                                          OceanNetCDFFields % netcdf_num_lats,       &
                                          OceanNetCDFFields % netcdf_num_depths,     &
                                          OceanNetCDFFields % netcdf_num_err_times))
  CALL ops_netcdf_get_var (err_file_handle,               &
                           "s_obs_var",                   &
                           OceanNetCDFFields % s_obs_var)
  CALL ops_netcdf_get_var (err_file_handle,               &
                           "s_syn_var",                   &
                           OceanNetCDFFields % s_syn_var)
  CALL ops_netcdf_get_var (err_file_handle,               &
                           "s_mes_var",                   &
                           OceanNetCDFFields % s_mes_var)
END IF

IF (ObsGroup == ObsGroupAltim) THEN
  ALLOCATE (OceanNetCDFFields % sossheig(OceanNetCDFFields % netcdf_num_lons,       &
                                         OceanNetCDFFields % netcdf_num_lats,       &
                                         OceanNetCDFFields % netcdf_num_bkg_times))
  CALL ops_netcdf_get_var (bkg_file_handle,              &
                           "sossheig",                   &
                           OceanNetCDFFields % sossheig)
  ALLOCATE (OceanNetCDFFields % ssh_mes_var(OceanNetCDFFields % netcdf_num_lons,       &
                                            OceanNetCDFFields % netcdf_num_lats,       &
                                            OceanNetCDFFields % netcdf_num_err_times))
  ALLOCATE (OceanNetCDFFields % ssh_obs_var(OceanNetCDFFields % netcdf_num_lons,       &
                                            OceanNetCDFFields % netcdf_num_lats,       &
                                            OceanNetCDFFields % netcdf_num_err_times))
  ALLOCATE (OceanNetCDFFields % ssh_syn_var(OceanNetCDFFields % netcdf_num_lons,       &
                                            OceanNetCDFFields % netcdf_num_lats,       &
                                            OceanNetCDFFields % netcdf_num_err_times))
  CALL ops_netcdf_get_var (err_file_handle,                 &
                           "ssh_mes_var",                   &
                           OceanNetCDFFields % ssh_mes_var)
  CALL ops_netcdf_get_var (err_file_handle,                 &
                           "ssh_obs_var",                   &
                           OceanNetCDFFields % ssh_obs_var)
  CALL ops_netcdf_get_var (err_file_handle,                 &
                           "ssh_syn_var",                   &
                           OceanNetCDFFields % ssh_syn_var)
END IF

IF (ObsGroup == ObsGroupSeaIce) THEN
  ALLOCATE (OceanNetCDFFields % iiceconc(OceanNetCDFFields % netcdf_num_lons,       &
                                         OceanNetCDFFields % netcdf_num_lats,       &
                                         OceanNetCDFFields % netcdf_num_bkg_times))
  CALL ops_netcdf_get_var (bkg_file_handle,              &
                           "iiceconc",                   &
                           OceanNetCDFFields % iiceconc)
  ALLOCATE (OceanNetCDFFields % seaice_mes_var(OceanNetCDFFields % netcdf_num_lons,       &
                                               OceanNetCDFFields % netcdf_num_lats,       &
                                               OceanNetCDFFields % netcdf_num_err_times))
  ALLOCATE (OceanNetCDFFields % seaice_obs_var(OceanNetCDFFields % netcdf_num_lons,       &
                                               OceanNetCDFFields % netcdf_num_lats,       &
                                               OceanNetCDFFields % netcdf_num_err_times))
  ALLOCATE (OceanNetCDFFields % seaice_syn_var(OceanNetCDFFields % netcdf_num_lons,       &
                                               OceanNetCDFFields % netcdf_num_lats,       &
                                               OceanNetCDFFields % netcdf_num_err_times))
  CALL ops_netcdf_get_var (err_file_handle,                    &
                           "seaice_mes_var",                   &
                           OceanNetCDFFields % seaice_mes_var)
  CALL ops_netcdf_get_var (err_file_handle,                    &
                           "seaice_obs_var",                   &
                           OceanNetCDFFields % seaice_obs_var)
  CALL ops_netcdf_get_var (err_file_handle,                    &
                           "seaice_syn_var",                   &
                           OceanNetCDFFields % seaice_syn_var)
END IF

IF (ObsGroup == ObsGroupOceanColour) THEN
  ALLOCATE (OceanNetCDFFields % chl_tot_var(OceanNetCDFFields % netcdf_num_lons,       &
                                            OceanNetCDFFields % netcdf_num_lats,       &
                                            OceanNetCDFFields % netcdf_num_err_times))
  CALL ops_netcdf_get_var (err_file_handle,                 &
                           "chl_tot_var",                   &
                           OceanNetCDFFields % chl_tot_var)
  ALLOCATE (OceanNetCDFFields % lchl_tot_var(OceanNetCDFFields % netcdf_num_lons,       &
                                             OceanNetCDFFields % netcdf_num_lats,       &
                                             OceanNetCDFFields % netcdf_num_err_times))
  CALL ops_netcdf_get_var (err_file_handle,                  &
                           "lchl_tot_var",                   &
                           OceanNetCDFFields % lchl_tot_var)

  ALLOCATE (OceanNetCDFFields % chl_obs_var(OceanNetCDFFields % netcdf_num_lons,       &
                                            OceanNetCDFFields % netcdf_num_lats,       &
                                            OceanNetCDFFields % netcdf_num_err_times))
  CALL ops_netcdf_get_var (err_file_handle,                 &
                           "chl_obs_var",                   &
                           OceanNetCDFFields % chl_obs_var)
  ALLOCATE (OceanNetCDFFields % lchl_obs_var(OceanNetCDFFields % netcdf_num_lons,       &
                                             OceanNetCDFFields % netcdf_num_lats,       &
                                             OceanNetCDFFields % netcdf_num_err_times))
  CALL ops_netcdf_get_var (err_file_handle,                  &
                           "lchl_obs_var",                   &
                           OceanNetCDFFields % lchl_obs_var)

  ALLOCATE (OceanNetCDFFields % kd490_tot_var(OceanNetCDFFields % netcdf_num_lons,       &
                                              OceanNetCDFFields % netcdf_num_lats,       &
                                              OceanNetCDFFields % netcdf_num_err_times))
  CALL ops_netcdf_get_var (err_file_handle,                   &
                           "kd490_tot_var",                   &
                           OceanNetCDFFields % kd490_tot_var)
  ALLOCATE (OceanNetCDFFields % lkd490_tot_var(OceanNetCDFFields % netcdf_num_lons,       &
                                               OceanNetCDFFields % netcdf_num_lats,       &
                                               OceanNetCDFFields % netcdf_num_err_times))
  CALL ops_netcdf_get_var (err_file_handle,                    &
                           "lkd490_tot_var",                   &
                           OceanNetCDFFields % lkd490_tot_var)

  ALLOCATE (OceanNetCDFFields % kd490_obs_var(OceanNetCDFFields % netcdf_num_lons,       &
                                              OceanNetCDFFields % netcdf_num_lats,       &
                                              OceanNetCDFFields % netcdf_num_err_times))
  CALL ops_netcdf_get_var (err_file_handle,                   &
                           "kd490_obs_var",                   &
                           OceanNetCDFFields % kd490_obs_var)

  ALLOCATE (OceanNetCDFFields % lkd490_obs_var(OceanNetCDFFields % netcdf_num_lons,       &
                                               OceanNetCDFFields % netcdf_num_lats,       &
                                               OceanNetCDFFields % netcdf_num_err_times))
  CALL ops_netcdf_get_var (err_file_handle,                    &
                           "lkd490_obs_var",                   &
                           OceanNetCDFFields % lkd490_obs_var)

  ALLOCATE (OceanNetCDFFields % schltot(OceanNetCDFFields % netcdf_num_lons,       &
                                        OceanNetCDFFields % netcdf_num_lats,       &
                                        OceanNetCDFFields % netcdf_num_bkg_times))
  CALL ops_netcdf_get_var (bkg_file_handle,             &
                           "CHL",                       &
                           OceanNetCDFFields % schltot)

  ALLOCATE (OceanNetCDFFields % skd490tot(OceanNetCDFFields % netcdf_num_lons,       &
                                          OceanNetCDFFields % netcdf_num_lats,       &
                                          OceanNetCDFFields % netcdf_num_bkg_times))
  CALL ops_netcdf_get_var (bkg_file_handle,               &
                           "Visib",                       &
                           OceanNetCDFFields % skd490tot)
END IF

CALL ops_netcdf_close (bkg_file_handle)
CALL ops_netcdf_close (err_file_handle)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ReadOceanNetCDFFields
