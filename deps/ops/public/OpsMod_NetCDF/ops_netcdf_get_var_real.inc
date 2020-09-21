!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for nf90_put_var (real scalars)
!-------------------------------------------------------------------------------

SUBROUTINE ops_netcdf_get_var_real (file_id, &
                                    name,    &
                                    value)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE netcdf, ONLY: &
  nf90_get_var,   &
  nf90_noerr,     &
  nf90_strerror

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)          :: file_id
CHARACTER(len=*), INTENT(IN) :: name
REAL, INTENT(OUT)            :: value

! Local declarations:
CHARACTER(len=*), PARAMETER  :: routine_name = "ops_netcdf_get_var_real"
INTEGER(kind=nc_int_kind)    :: rc
CHARACTER(len=300)           :: messages(3)
INTEGER                      :: var_id

IF (UseTrace) CALL gen_trace_entry (routine_name)

CALL ops_netcdf_inq_varid (file_id, &
                           name,    &
                           var_id)

rc = nf90_get_var (INT (file_id, nc_int_kind), &
                   INT (var_id, nc_int_kind),  &
                   value)
IF (rc /= nf90_noerr) THEN
  WRITE (messages(1), '(A,I0)') "nf90_get_var failed with rc = ", rc
  WRITE (messages(2), '(A,I0)') TRIM (name) // " var_id = ", var_id
  messages(3) = nf90_strerror (rc)
  CALL gen_fail (routine_name,  &
                 messages(1:3))
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_netcdf_get_var_real
