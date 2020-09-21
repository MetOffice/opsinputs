!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Retrieve the NetCDF coordinates and pre-search data
!-------------------------------------------------------------------------------

SUBROUTINE Ops_OceanInitNetCDFFields

USE GenMod_Core, ONLY: &
  MessageOut,          &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  StatusOK,            &
  UseTrace

USE GenMod_Control, ONLY: &
    GeneralMode,          &
    DebugMode

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

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = 'Ops_OceanInitNetCDFFields'
CHARACTER(len=5000)         :: netcdf_filename
INTEGER                     :: file_handle
INTEGER                     :: J
INTEGER                     :: netcdf_num_coord_times

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL ops_get_env ("OPS_UMBACK_NETCDF_MESH", &
                  netcdf_filename)
CALL ops_netcdf_open (netcdf_filename,                   &
                      OceanNetCDFFields % netcdf_handle)

CALL ops_netcdf_inq_dim_len (OceanNetCDFFields % netcdf_handle,    &
                             "y",                                  &
                             OceanNetCDFFields % netcdf_num_lats,  &
                             status_if_dim_not_present = StatusOK)
IF (OceanNetCDFFields % netcdf_num_lats > 0) THEN
  CALL ops_netcdf_inq_dim_len (OceanNetCDFFields % netcdf_handle,   &
                               "x",                                 &
                               OceanNetCDFFields % netcdf_num_lons)
ELSE
  CALL ops_netcdf_inq_dim_len (OceanNetCDFFields % netcdf_handle,   &
                               "lat",                               &
                               OceanNetCDFFields % netcdf_num_lats)
  CALL ops_netcdf_inq_dim_len (OceanNetCDFFields % netcdf_handle,   &
                               "lon",                               &
                               OceanNetCDFFields % netcdf_num_lons)
END IF
CALL ops_netcdf_inq_dim_len (OceanNetCDFFields % netcdf_handle,     &
                             "z",                                   &
                             OceanNetCDFFields % netcdf_num_depths)
CALL ops_netcdf_inq_dim_len (OceanNetCDFFields % netcdf_handle,    &
                             "t",                                  &
                             netcdf_num_coord_times)

ALLOCATE (OceanNetCDFFields % nav_lat(OceanNetCDFFields % netcdf_num_lons,OceanNetCDFFields % netcdf_num_lats))
ALLOCATE (OceanNetCDFFields % nav_lon(OceanNetCDFFields % netcdf_num_lons,OceanNetCDFFields % netcdf_num_lats))
CALL ops_netcdf_get_var (OceanNetCDFFields % netcdf_handle, &
                         "nav_lat",                         &
                         OceanNetCDFFields % nav_lat)
CALL ops_netcdf_get_var (OceanNetCDFFields % netcdf_handle, &
                         "nav_lon",                         &
                         OceanNetCDFFields % nav_lon)

ALLOCATE (OceanNetCDFFields % gdept_0(OceanNetCDFFields % netcdf_num_lons,OceanNetCDFFields % netcdf_num_lats, &
                                      OceanNetCDFFields % netcdf_num_depths,netcdf_num_coord_times))
ALLOCATE (OceanNetCDFFields % nav_lev(OceanNetCDFFields % netcdf_num_depths))
IF (ops_env_is_true ("OPS_UMBACK_NETCDF_S_LEVEL_DEPTHS")) THEN
  OceanNetCDFFields % depth_in_s_levels = .TRUE. 
  IF (GeneralMode >= DebugMode) THEN
    WRITE (MessageOut, '(A)') RoutineName//"S-level data, reading full 3D depth field "// &
                              "and Z depth levels separately"
  END IF
  CALL ops_netcdf_get_var (OceanNetCDFFields % netcdf_handle, &
                           "gdept_0",                         &
                           OceanNetCDFFields % gdept_0)
  CALL ops_netcdf_get_var (OceanNetCDFFields % netcdf_handle, &
                           "gdept_1d",                        &
                           OceanNetCDFFields % nav_lev)
ELSE
  OceanNetCDFFields % depth_in_s_levels = .FALSE. 
  IF (GeneralMode >= DebugMode) THEN
    WRITE (MessageOut, '(A)') RoutineName//"Z-level data, reading 1D depth field and "// &
                              "copying into 3D depth field"
  END IF
  CALL ops_netcdf_get_var (OceanNetCDFFields % netcdf_handle, &
                           "nav_lev",                         &
                           OceanNetCDFFields % nav_lev)
  DO J = 1, OceanNetCDFFields % netcdf_num_depths
    OceanNetCDFFields % gdept_0(:,:,J,1) = OceanNetCDFFields % nav_lev(J)
  END DO
END IF
CALL ops_netcdf_close (OceanNetCDFFields % netcdf_handle)

! OceanPresearchGrid
CALL ops_get_env ("OPS_UMBACK_NETCDF_PRESEARCH_GRID", &
                  netcdf_filename)
CALL ops_netcdf_open (netcdf_filename, &
                      file_handle)
CALL ops_netcdf_inq_dim_len (file_handle,                &
                             "ny",                       &
                             OceanPresearchGrid % nlats)
CALL ops_netcdf_inq_dim_len (file_handle,                &
                             "nx",                       &
                             OceanPresearchGrid % nlons)
CALL ops_netcdf_get_att (file_handle,               &
                         'latmin',                  &
                         OceanPresearchGrid%latmin)
CALL ops_netcdf_get_att (file_handle,               &
                         'lonmin',                  &
                         OceanPresearchGrid%lonmin)
CALL ops_netcdf_get_att (file_handle,               &
                         'dlon',                    &
                         OceanPresearchGrid % dlon)
CALL ops_netcdf_get_att (file_handle,               &
                         'dlat',                    &
                         OceanPresearchGrid % dlat)
CALL ops_netcdf_get_att (file_handle,                   &
                         'maxydiff',                    &
                         OceanPresearchGrid % maxydiff)
CALL ops_netcdf_get_att (file_handle,                   &
                         'maxxdiff',                    &
                         OceanPresearchGrid % maxxdiff)

ALLOCATE (OceanPresearchGrid % lons(OceanPresearchGrid % nlons,OceanPresearchGrid % nlats))
ALLOCATE (OceanPresearchGrid % lats(OceanPresearchGrid % nlons,OceanPresearchGrid % nlats))
ALLOCATE (OceanPresearchGrid % ixpos(OceanPresearchGrid % nlons,OceanPresearchGrid % nlats))
ALLOCATE (OceanPresearchGrid % iypos(OceanPresearchGrid % nlons,OceanPresearchGrid % nlats))

CALL ops_netcdf_get_var (file_handle,               &
                         "LON",                     &
                         OceanPresearchGrid % lons)
CALL ops_netcdf_get_var (file_handle,               &
                         "LAT",                     &
                         OceanPresearchGrid % lats)
CALL ops_netcdf_get_var (file_handle,                &
                         "XPOS",                     &
                         OceanPresearchGrid % ixpos)
CALL ops_netcdf_get_var (file_handle,                &
                         "YPOS",                     &
                         OceanPresearchGrid % iypos)
CALL ops_netcdf_close (file_handle)

! mean dynamic topography
CALL ops_get_env ("OPS_UMBACK_NETCDF_TOPOGRAPHY", &
                  netcdf_filename)
CALL ops_netcdf_open (netcdf_filename,                   &
                      OceanNetCDFFields % netcdf_handle)
ALLOCATE (OceanNetCDFFields % mean_sea_height(OceanNetCDFFields % netcdf_num_lons,OceanNetCDFFields % netcdf_num_lats))
CALL ops_netcdf_get_var (OceanNetCDFFields % netcdf_handle,   &
                         "sossheig",                          &
                         OceanNetCDFFields % mean_sea_height)
CALL ops_netcdf_close (OceanNetCDFFields % netcdf_handle)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_OceanInitNetCDFFields
