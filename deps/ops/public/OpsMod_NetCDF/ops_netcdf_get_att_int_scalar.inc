!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for nf90_get_att (integer scalars)
!-------------------------------------------------------------------------------

SUBROUTINE ops_netcdf_get_att_int_scalar (file_id, &
                                          name,    &
                                          value,   &
                                          var_id)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE netcdf, ONLY: &
  nf90_get_att,   &
  nf90_global,    &
  nf90_noerr,     &
  nf90_strerror

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)           :: file_id
CHARACTER(len=*), INTENT(IN)  :: name
INTEGER, INTENT(OUT)          :: value
INTEGER, OPTIONAL, INTENT(IN) :: var_id

! Local declarations:
CHARACTER(len=*), PARAMETER   :: routine_name = "ops_netcdf_get_att_int_scalar"
INTEGER(kind=nc_int_kind)     :: rc
CHARACTER(len=300)            :: messages(5)
INTEGER(kind=nc_int_kind)     :: var_id_local

IF (UseTrace) CALL gen_trace_entry (routine_name)

IF (PRESENT (var_id)) THEN
  var_id_local = var_id
ELSE
  var_id_local = nf90_global
END IF

rc = nf90_get_att (INT (file_id, nc_int_kind), &
                   var_id_local,               &
                   name,                       &
                   value)
IF (rc /= nf90_noerr) THEN
  WRITE (messages(1), '(A,I0)') "nf90_get_att failed with rc = ", rc
  WRITE (messages(2), '(A,I0)') "file id = ", file_id
  WRITE (messages(3), '(A,A)') "name = ", TRIM (name)
  WRITE (messages(4), '(A,I0)') "var id = ", var_id_local
  messages(5) = nf90_strerror (rc)
  CALL gen_fail (routine_name,  &
                 messages(1:5))
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_netcdf_get_att_int_scalar
