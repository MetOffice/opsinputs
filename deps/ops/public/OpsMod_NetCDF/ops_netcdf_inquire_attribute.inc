!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for nf90_inquire_attribute
!-------------------------------------------------------------------------------

SUBROUTINE ops_netcdf_inquire_attribute (file_id, &
                                         var_id,  &
                                         name,    &
                                         type)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE netcdf, ONLY:         &
  nf90_inquire_attribute, &
  nf90_noerr,             &
  nf90_strerror

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)          :: file_id
INTEGER, INTENT(IN)          :: var_id
CHARACTER(len=*), INTENT(IN) :: name
INTEGER, INTENT(OUT)         :: type

! Local declarations:
CHARACTER(len=*), PARAMETER  :: routine_name = "ops_netcdf_inquire_attribute"
INTEGER(kind=nc_int_kind)    :: rc
CHARACTER(len=300)           :: messages(5)
INTEGER(kind=nc_int_kind)    :: local_type
INTEGER(kind=nc_int_kind)    :: attr_len

IF (UseTrace) CALL gen_trace_entry (routine_name)

rc = nf90_inquire_attribute (INT (file_id, nc_int_kind), &
                             INT (var_id, nc_int_kind),  &
                             name,                       &
                             xtype = local_type,         &
                             len = attr_len)
IF (rc /= nf90_noerr) THEN
  WRITE (messages(1), '(A,I0)') "nf90_inquire_attribute failed with rc = ", rc
  WRITE (messages(2), '(A,I0)') "file_id = ", file_id
  WRITE (messages(3), '(A,I0)') "var_id = ", var_id
  messages(4) = "name = " // name
  messages(5) = nf90_strerror (rc)
  CALL gen_fail (routine_name,  &
                 messages(1:5))
END IF

type = local_type

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_netcdf_inquire_attribute
