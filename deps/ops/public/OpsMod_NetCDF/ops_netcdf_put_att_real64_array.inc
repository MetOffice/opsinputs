!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for nf90_put_att (real arrays)
!-------------------------------------------------------------------------------

SUBROUTINE ops_netcdf_put_att_real64_array (file_id, &
                                            name,    &
                                            values,  &
                                            var_id)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE netcdf, ONLY: &
  nf90_global,    &
  nf90_noerr,     &
  nf90_put_att,   &
  nf90_strerror

USE OpsMod_Kinds, ONLY: &
  real64

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)             :: file_id
CHARACTER(len=*), INTENT(IN)    :: name
REAL(kind=real64), INTENT(IN)   :: values(:)
INTEGER, OPTIONAL, INTENT(IN)   :: var_id

! Local declarations:
CHARACTER(len=*), PARAMETER     :: routine_name = "ops_netcdf_put_att_real64_array"
INTEGER(kind=nc_int_kind)       :: rc
CHARACTER(len=300), ALLOCATABLE :: messages(:)
INTEGER(kind=nc_int_kind)       :: var_id_local
INTEGER                         :: i

IF (UseTrace) CALL gen_trace_entry (routine_name)

IF (PRESENT (var_id)) THEN
  var_id_local = var_id
ELSE
  var_id_local = nf90_global
END IF

rc = nf90_put_att (INT (file_id, nc_int_kind), &
                   var_id_local,               &
                   name,                       &
                   values)
IF (rc /= nf90_noerr) THEN
  ALLOCATE (messages(5 + SIZE (values)))
  WRITE (messages(1), '(A,I0)') "nf90_put_att failed with rc = ", rc
  WRITE (messages(2), '(A,I0)') "file id = ", file_id
  WRITE (messages(3), '(A,A)') "name = ", TRIM (name)
  WRITE (messages(4), '(A,I0)') "var id = ", var_id_local
  DO i = 1, SIZE (values)
    WRITE (messages(4 + i), '(A,I0,A,F22.3)') "values ", i, " = ", values(i)
  END DO
  messages(5 + SIZE (values)) = nf90_strerror (rc)
  CALL gen_fail (routine_name, &
                 messages(:))
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_netcdf_put_att_real64_array
