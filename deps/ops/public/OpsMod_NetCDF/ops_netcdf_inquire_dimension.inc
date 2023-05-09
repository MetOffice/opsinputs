!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for nf90_inquire_dimension
!-------------------------------------------------------------------------------

SUBROUTINE ops_netcdf_inquire_dimension (file_id, &
                                         dim_id,  &
                                         len)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE netcdf, ONLY:         &
  nf90_inquire_dimension, &
  nf90_noerr,             &
  nf90_strerror

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)            :: file_id
INTEGER, INTENT(IN)            :: dim_id
INTEGER, OPTIONAL, INTENT(OUT) :: len

! Local declarations:
CHARACTER(len=*), PARAMETER    :: routine_name = "ops_netcdf_inquire_dimension"
INTEGER(kind=nc_int_kind)      :: rc
CHARACTER(len=300)             :: messages(4)
INTEGER(kind=nc_int_kind)      :: local_len

IF (UseTrace) CALL gen_trace_entry (routine_name)

rc = nf90_inquire_dimension (INT (file_id, nc_int_kind), &
                             INT (dim_id, nc_int_kind),  &
                             len = local_len)
IF (rc /= nf90_noerr) THEN
  WRITE (messages(1), '(A,I0)') "nf90_inquire_dimension failed with rc = ", rc
  WRITE (messages(2), '(A,I0)') "file_id = ", file_id
  WRITE (messages(3), '(A,I0)') "dim_id = ", dim_id
  messages(4) = nf90_strerror (rc)
  CALL gen_fail (routine_name,  &
                 messages(1:4))
END IF

IF (PRESENT (len)) len = local_len

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_netcdf_inquire_dimension 
