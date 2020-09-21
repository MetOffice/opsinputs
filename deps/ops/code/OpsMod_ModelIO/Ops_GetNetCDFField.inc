!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Open a fields file and read its header.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_GetNetCDFField (ncfields, &
                               name)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE ops_netcdf_wrappers, ONLY: &
  ops_netcdf_get_var

IMPLICIT NONE

! Subroutine arguments:
TYPE (NetCDFFields_type), INTENT(INOUT) :: ncfields
CHARACTER(len=*), INTENT(IN)            :: name

! Local declarations:
CHARACTER(len=*), PARAMETER             :: RoutineName = 'Ops_GetNetCDFField'
CHARACTER(len=500)                      :: message

IF (UseTrace) CALL gen_trace_entry (RoutineName)

SELECT CASE (name)
  CASE ("mss")
    IF (.NOT. ALLOCATED (ncfields % netcdf_mss)) THEN
      ALLOCATE (ncfields % netcdf_mss(ncfields % netcdf_num_lons,ncfields % netcdf_num_lats,ncfields % netcdf_num_times))
      CALL ops_netcdf_get_var (ncfields % netcdf_handle, &
                               "mss",                    &
                               ncfields % netcdf_mss)
      WHERE (ncfields % netcdf_mss < 0) ncfields % netcdf_mss = 0
    END IF
  CASE ("cha")
    IF (.NOT. ALLOCATED (ncfields % netcdf_cha)) THEN
      ALLOCATE (ncfields % netcdf_cha(ncfields % netcdf_num_lons,ncfields % netcdf_num_lats,ncfields % netcdf_num_times))
      CALL ops_netcdf_get_var (ncfields % netcdf_handle, &
                               "cha",                    &
                               ncfields % netcdf_cha)
      WHERE (ncfields % netcdf_cha < 0) ncfields % netcdf_cha = 0
    END IF
  CASE DEFAULT
    message = "Unknown name " // name
    CALL gen_fail (RoutineName, &
                   message)
END SELECT

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_GetNetCDFField
