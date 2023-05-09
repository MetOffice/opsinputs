!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Open a fields file and read its header.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_InitNetCDFFields (ncfields)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE ops_netcdf_wrappers, ONLY: &
  ops_netcdf_get_var,          &
  ops_netcdf_inq_dim_len,      &
  ops_netcdf_open

USE OpsMod_EnvUtils, ONLY: &
  ops_env_is_set,          &
  ops_get_env

IMPLICIT NONE

! Subroutine arguments:
TYPE (NetCDFFields_type), INTENT(OUT) :: ncfields

! Local declarations:
CHARACTER(len=*), PARAMETER           :: RoutineName = 'Ops_InitNetCDFFields'
CHARACTER(len=5000)                   :: netcdf_filename

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (ops_env_is_set ("OPS_UMBACK_NETCDF")) THEN
  CALL ops_get_env ("OPS_UMBACK_NETCDF", &
                    netcdf_filename)
  CALL ops_netcdf_open (netcdf_filename,          &
                        ncfields % netcdf_handle)
  CALL ops_netcdf_inq_dim_len (ncfields % netcdf_handle,   &
                               "latitude",                 &
                               ncfields % netcdf_num_lats)
  CALL ops_netcdf_inq_dim_len (ncfields % netcdf_handle,   &
                               "longitude",                &
                               ncfields % netcdf_num_lons)
  CALL ops_netcdf_inq_dim_len (ncfields % netcdf_handle,    &
                               "time",                      &
                               ncfields % netcdf_num_times)
  ALLOCATE (ncfields % netcdf_latitudes(ncfields % netcdf_num_lats))
  ALLOCATE (ncfields % netcdf_longitudes(ncfields % netcdf_num_lons))
  CALL ops_netcdf_get_var (ncfields % netcdf_handle,    &
                           "latitude",                  &
                           ncfields % netcdf_latitudes)
  CALL ops_netcdf_get_var (ncfields % netcdf_handle,     &
                           "longitude",                  &
                           ncfields % netcdf_longitudes)
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_InitNetCDFFields
