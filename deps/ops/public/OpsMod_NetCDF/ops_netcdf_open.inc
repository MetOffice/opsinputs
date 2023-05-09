!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for nf90_open
!-------------------------------------------------------------------------------

SUBROUTINE ops_netcdf_open (file_name, &
                            file_id)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE netcdf, ONLY: &
  nf90_noerr,     &
  nf90_nowrite,   &
  nf90_open,      &
  nf90_strerror

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN) :: file_name
INTEGER, INTENT(OUT)         :: file_id

! Local declarations:
CHARACTER(len=*), PARAMETER  :: routine_name = "ops_netcdf_open"
INTEGER(kind=nc_int_kind)    :: rc
CHARACTER(len=300)           :: messages(3)
INTEGER(kind=nc_int_kind)    :: file_id_local

IF (UseTrace) CALL gen_trace_entry (routine_name)

rc = nf90_open (file_name,     &
                nf90_nowrite,  &
                file_id_local)
IF (rc /= nf90_noerr) THEN
  WRITE (messages(1), '(A,I0)') "nf90_open failed with rc = ", rc
  WRITE (messages(2), '(A,A)') "file_name = ", TRIM (file_name)
  messages(3) = nf90_strerror (rc)
  CALL gen_fail (routine_name,  &
                 messages(1:3))
END IF

file_id = file_id_local

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_netcdf_open
