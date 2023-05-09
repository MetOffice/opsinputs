!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for nf90_inquire_variable
!-------------------------------------------------------------------------------

SUBROUTINE ops_netcdf_inquire_variable (file_id, &
                                        var_id,  &
                                        ndims,   &
                                        dimids,  &
                                        type,    &
                                        natts)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE netcdf, ONLY:        &
  nf90_inquire_variable, &
  nf90_noerr,            &
  nf90_strerror

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)            :: file_id
INTEGER, INTENT(IN)            :: var_id
INTEGER, OPTIONAL, INTENT(OUT) :: ndims
INTEGER, OPTIONAL, INTENT(OUT) :: dimids(:)
INTEGER, OPTIONAL, INTENT(OUT) :: type
INTEGER, OPTIONAL, INTENT(OUT) :: natts

! Local declarations:
CHARACTER(len=*), PARAMETER    :: routine_name = "ops_netcdf_inquire_variable"
INTEGER(kind=nc_int_kind)      :: rc
CHARACTER(len=300)             :: messages(4)
INTEGER(kind=nc_int_kind)      :: local_ndims
INTEGER(kind=nc_int_kind)      :: local_dimids(50)
INTEGER(kind=nc_int_kind)      :: local_type
INTEGER(kind=nc_int_kind)      :: local_natts

IF (UseTrace) CALL gen_trace_entry (routine_name)

rc = nf90_inquire_variable (INT (file_id, nc_int_kind), &
                            INT (var_id, nc_int_kind),  &
                            ndims = local_ndims,        &
                            dimids = local_dimids,      &
                            xtype = local_type,         &
                            natts = local_natts)
IF (rc /= nf90_noerr) THEN
  WRITE (messages(1), '(A,I0)') "nf90_inquire_variable failed with rc = ", rc
  WRITE (messages(2), '(A,I0)') "file_id = ", file_id
  WRITE (messages(3), '(A,I0)') "var_id = ", var_id
  messages(4) = nf90_strerror (rc)
  CALL gen_fail (routine_name,  &
                 messages(1:4))
END IF

IF (PRESENT (ndims)) ndims = local_ndims
IF (PRESENT (dimids)) dimids = local_dimids(1:SIZE (dimids))
IF (PRESENT (type)) type = local_type
IF (PRESENT (natts)) natts = local_natts

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_netcdf_inquire_variable
