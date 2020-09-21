!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for nf90_create
!-------------------------------------------------------------------------------

SUBROUTINE ops_netcdf_create (file_name, &
                              file_id,   &
                              clobber,   &
                              offset_64)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE netcdf, ONLY:    &
  nf90_64bit_offset, &
  nf90_clobber,      &
  nf90_create,       &
  nf90_noerr,        &
  nf90_strerror

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)  :: file_name
INTEGER, INTENT(OUT)          :: file_id
LOGICAL, OPTIONAL, INTENT(IN) :: clobber
LOGICAL, OPTIONAL, INTENT(IN) :: offset_64

! Local declarations:
CHARACTER(len=*), PARAMETER   :: routine_name = "ops_netcdf_create"
INTEGER(kind=nc_int_kind)     :: rc
INTEGER(kind=nc_int_kind)     :: flags
CHARACTER(len=300)            :: messages(5)
INTEGER(kind=nc_int_kind)     :: file_id_local

IF (UseTrace) CALL gen_trace_entry (routine_name)

flags = 0

IF (PRESENT (clobber)) THEN
  IF (clobber) flags = flags + nf90_clobber
END IF

IF (PRESENT (offset_64)) THEN
  IF (offset_64) flags = flags + nf90_64bit_offset
END IF

rc = nf90_create (file_name,     &
                  flags,         &
                  file_id_local)
IF (rc /= nf90_noerr) THEN
  WRITE (messages(1), '(A,I0)') "nf90_create failed with rc = ", rc
  WRITE (messages(2), '(A,A)') "file name = ", TRIM (file_name)
  WRITE (messages(3), '(A,L7)') "clobber? ", .FALSE.
  IF (PRESENT (clobber)) THEN
    WRITE (messages(3), '(A,L7)') "clobber? ", clobber
  END IF
  WRITE (messages(4), '(A,L7)') "64bit offset? ", .FALSE.
  IF (PRESENT (offset_64)) THEN
    WRITE (messages(4), '(A,L7)') "64bit_offset? ", offset_64
  END IF
  messages(5) = nf90_strerror (rc)
  CALL gen_fail (routine_name,  &
                 messages(1:5))
END IF

file_id = file_id_local

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_netcdf_create
