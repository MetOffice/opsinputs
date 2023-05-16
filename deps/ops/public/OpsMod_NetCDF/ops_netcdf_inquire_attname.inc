!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for nf90_inquire_variable
!-------------------------------------------------------------------------------

SUBROUTINE ops_netcdf_inquire_attname (file_id, &
                                       var_id,  &
                                       attnum,  &
                                       name)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE netcdf, ONLY:   &
  nf90_inq_attname, &
  nf90_noerr,       &
  nf90_strerror

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)           :: file_id
INTEGER, INTENT(IN)           :: var_id
INTEGER, INTENT(IN)           :: attnum
CHARACTER(len=*), INTENT(OUT) :: name

! Local declarations:
CHARACTER(len=*), PARAMETER    :: routine_name = "ops_netcdf_inquie_attname"
INTEGER(kind=nc_int_kind)      :: rc
CHARACTER(len=300)             :: messages(5)

IF (UseTrace) CALL gen_trace_entry (routine_name)

rc = nf90_inq_attname (INT (file_id, nc_int_kind), &
                       INT (var_id, nc_int_kind),  &
                       INT (attnum, nc_int_kind),  &
                       name)
IF (rc /= nf90_noerr) THEN
  WRITE (messages(1), '(A,I0)') "nf90_inq_attname failed with rc = ", rc
  WRITE (messages(2), '(A,I0)') "file_id = ", file_id
  WRITE (messages(3), '(A,I0)') "var_id = ", var_id
  WRITE (messages(4), '(A,I0)') "attnum = ", attnum
  messages(5) = nf90_strerror (rc)
  CALL gen_fail (routine_name,  &
                 messages(1:5))
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_netcdf_inquire_attname
