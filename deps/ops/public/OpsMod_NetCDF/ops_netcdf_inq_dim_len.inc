!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for nf90_inquire_dimension/nf90_inq_dimid for length.
!-------------------------------------------------------------------------------

SUBROUTINE ops_netcdf_inq_dim_len (file_id,                   &
                                   name,                      &
                                   length,                    &
                                   status_if_dim_not_present)

USE GenMod_Control, ONLY: &
  GeneralMode,            &
  VerboseMode

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_message,         &
  gen_trace_entry,     &
  gen_trace_exit,      &
  gen_warn,            &
  StatusFatal,         &
  StatusOK,            &
  StatusWarning,       &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE netcdf, ONLY:         &
  nf90_inq_dimid,         &
  nf90_inquire_dimension, &
  nf90_noerr,             &
  nf90_strerror

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)           :: file_id
CHARACTER(len=*), INTENT(IN)  :: name
INTEGER, INTENT(OUT)          :: length
INTEGER, OPTIONAL, INTENT(IN) :: status_if_dim_not_present

! Local declarations:
CHARACTER(len=*), PARAMETER   :: routine_name = "ops_netcdf_inq_dim_len"
INTEGER(kind=nc_int_kind)     :: rc
CHARACTER(len=300)            :: messages(4)
INTEGER(kind=nc_int_kind)     :: dim_id
INTEGER(kind=nc_int_kind)     :: length_local
INTEGER                       :: status_if_dim_not_present_local

IF (UseTrace) CALL gen_trace_entry (routine_name)

IF (PRESENT (status_if_dim_not_present)) THEN
  status_if_dim_not_present_local = status_if_dim_not_present
ELSE
  status_if_dim_not_present_local = StatusFatal
END IF

rc = nf90_inq_dimid (INT (file_id, nc_int_kind), &
                     name,                       &
                     dim_id)
IF (rc /= nf90_noerr) THEN
  WRITE (messages(1), '(A,I0)') "nf90_inq_dimid failed with rc = ", rc
  WRITE (messages(2), '(A,I0)') "file_id = ", file_id
  WRITE (messages(3), '(A,A)') "name = ", TRIM (name)
  messages(4) = nf90_strerror (rc)
  SELECT CASE (status_if_dim_not_present_Local)
    CASE (StatusFatal)
      CALL gen_fail (routine_name,  &
                     messages(1:4))
    CASE (StatusWarning)
      CALL gen_warn (routine_name,  &
                     messages(1:4))
    CASE (StatusOK)
      IF (GeneralMode >= VerboseMode) THEN
        CALL gen_message (routine_name,  &
                          messages(1:4))
      END IF
    CASE DEFAULT
      CALL gen_fail (routine_name,                        &
                     "Invalid status_if_dim_not_present")
  END SELECT
  length = IMDI
  GOTO 9999
END IF

rc = nf90_inquire_dimension (INT (file_id, nc_int_kind), &
                             dim_id,                     &
                             len = length_local)
IF (rc /= nf90_noerr) THEN
  WRITE (messages(1), '(A,I0)') "nf90_inquire_dimension failed with rc = ", rc
  WRITE (messages(2), '(A,I0)') "file_id = ", file_id
  WRITE (messages(3), '(A,I0)') "dim_id = ", dim_id
  messages(4) = nf90_strerror (rc)
  CALL gen_fail (routine_name,  &
                 messages(1:4))
END IF

length = length_local

9999 CONTINUE

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_netcdf_inq_dim_len
