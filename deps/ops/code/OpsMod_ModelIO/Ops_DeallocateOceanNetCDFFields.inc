!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Retrieve the NetCDF coordinates and pre-search data
!-------------------------------------------------------------------------------

SUBROUTINE Ops_DeallocateOceanNetCDFFields (netcdf_fields)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

IMPLICIT NONE

! Subroutine arguments:
TYPE (OceanNetCDFFields_type), INTENT(OUT) :: netcdf_fields

! Local declarations:
CHARACTER(len=*), PARAMETER                :: RoutineName = 'Ops_DeallocateOceanNetCDFFields'

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_DeallocateOceanNetCDFFields
