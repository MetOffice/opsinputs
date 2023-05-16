!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for nf90_enddef
!-------------------------------------------------------------------------------

SUBROUTINE ops_netcdf_enddef (file_id)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE netcdf, ONLY: &
  nf90_enddef,    &
  nf90_noerr,     &
  nf90_strerror

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)         :: file_id

! Local declarations:
CHARACTER(len=*), PARAMETER :: routine_name = "ops_netcdf_enddef"
INTEGER(kind=nc_int_kind)   :: rc
CHARACTER(len=300)          :: messages(3)

IF (UseTrace) CALL gen_trace_entry (routine_name)

rc = nf90_enddef (INT (file_id, nc_int_kind))
IF (rc /= nf90_noerr) THEN
  WRITE (messages(1), '(A,I0)') "nf90_enddef failed with rc = ", rc
  WRITE (messages(2), '(A,I0)') "file id = ", file_id
  messages(3) = nf90_strerror (rc)
  CALL gen_fail (routine_name,  &
                 messages(1:3))
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_netcdf_enddef
