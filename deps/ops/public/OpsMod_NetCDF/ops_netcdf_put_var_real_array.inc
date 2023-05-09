!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for nf90_put_var (real arrays)
!-------------------------------------------------------------------------------

SUBROUTINE ops_netcdf_put_var_real_array (file_id, &
                                          var_id,  &
                                          values,  &
                                          start)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE netcdf, ONLY: &
  nf90_noerr,     &
  nf90_put_var,   &
  nf90_strerror

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)           :: file_id
INTEGER, INTENT(IN)           :: var_id
REAL, INTENT(IN)              :: values(:)
INTEGER, OPTIONAL, INTENT(IN) :: start(:)

! Local declarations:
CHARACTER(len=*), PARAMETER   :: routine_name = "ops_netcdf_put_var_real_array"
INTEGER(kind=nc_int_kind)     :: rc
CHARACTER(len=300)            :: messages(5)

IF (UseTrace) CALL gen_trace_entry (routine_name)

IF (PRESENT (start)) THEN
  rc = nf90_put_var (INT (file_id, nc_int_kind), &
                     INT (var_id, nc_int_kind),  &
                     values,                     &
                     INT (start, nc_int_kind))
ELSE
  rc = nf90_put_var (INT (file_id, nc_int_kind), &
                     INT (var_id, nc_int_kind),  &
                     values)
END IF
IF (rc /= nf90_noerr) THEN
  WRITE (messages(1), '(A,I0)') "nf90_put_var failed with rc = ", rc
  WRITE (messages(2), '(A,I0)') "file id = ", file_id
  WRITE (messages(3), '(A,I0)') "var id = ", var_id
  WRITE (messages(4), '(A,I0)') "num values = ", SIZE (values)
  messages(5) = nf90_strerror (rc)
  CALL gen_fail (routine_name,  &
                 messages(1:5))
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_netcdf_put_var_real_array
