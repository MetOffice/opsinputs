!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for nf90_def_var (arrays)
!-------------------------------------------------------------------------------

SUBROUTINE ops_netcdf_def_var_single (file_id, &
                                      name,    &
                                      type,    &
                                      dim_id,  &
                                      var_id)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE netcdf, ONLY: &
  nf90_def_var,   &
  nf90_noerr,     &
  nf90_strerror

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)          :: file_id
CHARACTER(len=*), INTENT(IN) :: name
INTEGER, INTENT(IN)          :: type
INTEGER, INTENT(IN)          :: dim_id
INTEGER, INTENT(OUT)         :: var_id

! Local declarations:
CHARACTER(len=*), PARAMETER  :: routine_name = "ops_netcdf_def_var_single"
INTEGER(kind=nc_int_kind)    :: rc
CHARACTER(len=300)           :: messages(6)
INTEGER(kind=nc_int_kind)    :: var_id_local

IF (UseTrace) CALL gen_trace_entry (routine_name)

rc = nf90_def_var (INT (file_id, nc_int_kind), &
                   name,                       &
                   INT (type, nc_int_kind),    &
                   INT (dim_id, nc_int_kind),  &
                   var_id_local)
IF (rc /= nf90_noerr) THEN
  WRITE (messages(1), '(A,I0)') "nf90_def_var failed with rc = ", rc
  WRITE (messages(2), '(A,I0)') "file id = ", file_id
  WRITE (messages(3), '(A,A)') "name = ", TRIM (name)
  WRITE (messages(4), '(A,I0)') "type = ", type
  WRITE (messages(5), '(A,I0)') "dim_id = ", dim_id
  messages(6) = nf90_strerror (rc)
  CALL gen_fail (routine_name, &
                 messages(:))
END IF

var_id = var_id_local

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_netcdf_def_var_single
