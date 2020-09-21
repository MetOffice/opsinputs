!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for nf90_inq_varid
!-------------------------------------------------------------------------------

SUBROUTINE ops_netcdf_inq_varid (file_id, &
                                 name,    &
                                 var_id)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE netcdf, ONLY: &
  nf90_inq_varid, &
  nf90_noerr,     &
  nf90_strerror

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)          :: file_id
CHARACTER(len=*), INTENT(IN) :: name
INTEGER, INTENT(OUT)         :: var_id

! Local declarations:
CHARACTER(len=*), PARAMETER  :: routine_name = "ops_netcdf_inq_varid"
INTEGER(kind=nc_int_kind)    :: rc
CHARACTER(len=300)           :: messages(4)
INTEGER(kind=nc_int_kind)    :: var_id_local

IF (UseTrace) CALL gen_trace_entry (routine_name)

rc = nf90_inq_varid (INT (file_id, nc_int_kind), &
                     name,                       &
                     var_id_local)
IF (rc /= nf90_noerr) THEN
  WRITE (messages(1), '(A,I0)') "nf90_inq_varid failed with rc = ", rc
  WRITE (messages(2), '(A,I0)') "file_id = ", file_id
  WRITE (messages(3), '(A,A)') "name = ", TRIM (name)
  messages(4) = nf90_strerror (rc)
  CALL gen_fail (routine_name,  &
                 messages(1:4))
END IF

var_id = var_id_local

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_netcdf_inq_varid
