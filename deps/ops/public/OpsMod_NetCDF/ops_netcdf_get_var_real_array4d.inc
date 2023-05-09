!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for nf90_get_var (real 3d arrays)
!-------------------------------------------------------------------------------

SUBROUTINE ops_netcdf_get_var_real_array4d (file_id, &
                                            name,    &
                                            values)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  RMDI

USE netcdf, ONLY: &
  nf90_get_var,   &
  nf90_noerr,     &
  nf90_short,     &
  nf90_strerror

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)          :: file_id
CHARACTER(len=*), INTENT(IN) :: name
REAL, INTENT(OUT)            :: values(:,:,:,:)

! Local declarations:
CHARACTER(len=*), PARAMETER  :: routine_name = "ops_netcdf_get_var_real_array4d"
INTEGER(kind=nc_int_kind)    :: rc
CHARACTER(len=300)           :: messages(6)
INTEGER(kind=nc_int_kind)    :: count_local(4)
INTEGER(kind=nc_int_kind)    :: start_local(4)
INTEGER                      :: type
INTEGER, ALLOCATABLE         :: local_values_int(:,:,:,:)
REAL                         :: scale
INTEGER                      :: natts
INTEGER                      :: i
CHARACTER(len=200)           :: attr_name
INTEGER                      :: var_id
REAL                         :: fill_value

IF (UseTrace) CALL gen_trace_entry (routine_name)

CALL ops_netcdf_inq_varid (file_id, &
                           name,    &
                           var_id)

start_local = 1
count_local = (/SIZE (values, DIM = 1), SIZE (values, DIM = 2), SIZE (values, DIM = 3), SIZE (values, DIM = 4)/)

scale = 1
fill_value = RMDI

CALL ops_netcdf_inquire_variable (file_id,       &
                                  var_id,        &
                                  type = type,   &
                                  natts = natts)
DO i = 1, natts
  CALL ops_netcdf_inquire_attname (file_id,   &
                                   var_id,    &
                                   i,         &
                                   attr_name)
  IF (attr_name == "scale_factor") THEN
    CALL ops_netcdf_get_att (file_id,         &
                             "scale_factor",  &
                             scale,           &
                             var_id = var_id)
  END IF
  IF (attr_name == "_FillValue") THEN
    CALL ops_netcdf_get_att (file_id,         &
                             "_FillValue",    &
                             fill_value,      &
                             var_id = var_id)
  END IF
END DO

SELECT CASE (type)
  CASE (nf90_short)
    ALLOCATE (local_values_int(SIZE (values, DIM = 1),SIZE (values, DIM = 2),SIZE (values, DIM = 3),SIZE (values, DIM = 4)))
    rc = nf90_get_var (INT (file_id, nc_int_kind), &
                       INT (var_id, nc_int_kind),  &
                       local_values_int,           &
                       start = start_local,        &
                       count = count_local)
    IF (rc == nf90_noerr) values = local_values_int
  CASE DEFAULT
    rc = nf90_get_var (INT (file_id, nc_int_kind), &
                       INT (var_id, nc_int_kind),  &
                       values,                     &
                       start = start_local,        &
                       count = count_local)
END SELECT
IF (rc /= nf90_noerr) THEN
  WRITE (messages(1), '(A,I0)') "nf90_get_var failed with rc = ", rc
  WRITE (messages(2), '(A,I0)') TRIM (name) // " var_id = ", var_id
  WRITE (messages(3), '(4(A,I0))') "starts = ", start_local(1), ", ", start_local(2), ", ", start_local(3), ", ", start_local(4)
  WRITE (messages(4), '(4(A,I0))') "counts = ", count_local(1), ", ", count_local(2), ", ", count_local(3), ", ", count_local(4)
  WRITE (messages(5), '(A,I0)') "type = ", type
  messages(6) = nf90_strerror (rc)
  CALL gen_fail (routine_name,  &
                 messages(1:6))
END IF

IF (fill_value /= RMDI) THEN
  WHERE (values == fill_value) values = RMDI
END IF
WHERE (values /= RMDI)
  values = values * scale
END WHERE

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_netcdf_get_var_real_array4d
