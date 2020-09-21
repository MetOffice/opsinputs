!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for nf90_get_var (integer 1d arrays)
!-------------------------------------------------------------------------------

SUBROUTINE ops_netcdf_get_var_int_array1d (file_id, &
                                           name,    &
                                           values)

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
INTEGER, INTENT(OUT)         :: values(:)

! Local declarations:
CHARACTER(len=*), PARAMETER  :: routine_name = "ops_netcdf_get_var_int_array1d"
INTEGER(kind=nc_int_kind)    :: rc
CHARACTER(len=300)           :: messages(5)
INTEGER(kind=nc_int_kind)    :: count_local(1)
INTEGER(kind=nc_int_kind)    :: start_local(1)
INTEGER                      :: var_id

IF (UseTrace) CALL gen_trace_entry (routine_name)

CALL ops_netcdf_inq_varid (file_id, &
                           name,    &
                           var_id)

start_local = 1

count_local = (/SIZE (values)/)

rc = nf90_get_var (INT (file_id, nc_int_kind), &
                   INT (var_id, nc_int_kind),  &
                   values,                     &
                   start = start_local,        &
                   count = count_local)
IF (rc /= nf90_noerr) THEN
  WRITE (messages(1), '(A,I0)') "nf90_get_var failed with rc = ", rc
  WRITE (messages(2), '(A,I0)') TRIM (name) // " var_id = ", var_id
  WRITE (messages(3), '(A,I0,A,I0,A,I0)') "start = ", start_local(1)
  WRITE (messages(4), '(A,I0,A,I0,A,I0)') "count = ", count_local(1)
  messages(5) = nf90_strerror (rc)
  CALL gen_fail (routine_name,  &
                 messages(1:5))
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_netcdf_get_var_int_array1d
