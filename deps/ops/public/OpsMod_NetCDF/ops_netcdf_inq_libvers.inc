!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for nf90_inq_libvers
!-------------------------------------------------------------------------------

FUNCTION ops_netcdf_inq_libvers ()

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE netcdf, ONLY:   &
  nf90_inq_libvers

IMPLICIT NONE

! Function result:
CHARACTER(len=80)           :: ops_netcdf_inq_libvers

! Local declarations:
CHARACTER(len=*), PARAMETER :: routine_name = "ops_netcdf_inq_libvers"

IF (UseTrace) CALL gen_trace_entry (routine_name)

ops_netcdf_inq_libvers = nf90_inq_libvers ()

IF (UseTrace) CALL gen_trace_exit (routine_name)

END FUNCTION ops_netcdf_inq_libvers
