!
! (C) Copyright 2020 Met Office UK
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

!> Extensions to the Fortran-callable interface to ioda::ObsSpace.

interface

subroutine c_cxvarobs_obsspace_get_db_datetime_offset_in_seconds( &
    obss, group, variable, reference, length, offset) &
    bind(C,name='cxvarobs_obsspace_get_db_datetime_offset_in_seconds_f')
  use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_size_t, c_int64_t
  implicit none
  type(c_ptr), value :: obss
  character(kind=c_char, len=1), intent(in) :: group(*)
  character(kind=c_char, len=1), intent(in) :: variable(*)
  type(c_ptr), value :: reference
  integer(c_size_t), intent(in) :: length
  integer(c_int64_t), intent(inout) :: offset(length)
end subroutine c_cxvarobs_obsspace_get_db_datetime_offset_in_seconds

end interface
