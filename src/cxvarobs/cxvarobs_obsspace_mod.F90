!
! (C) Copyright 2020 Met Office UK
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

!> Extensions to the Fortran interface to ioda::ObsSpace.
module cxvarobs_obsspace_mod

use datetime_mod
use string_f_c_mod

implicit none

public cxvarobs_obsspace_get_db_datetime_offset_in_seconds

#include "cxvarobs_obsspace_interface.f90"

contains

!> Get a datetime variable from ObsSpace, representing it as an array of offsets (in seconds)
!> with respect to a reference datetime.
subroutine cxvarobs_obsspace_get_db_datetime_offset_in_seconds(obss, group, vname, reference, offset)
  use, intrinsic :: iso_c_binding
  implicit none
  type(c_ptr), value, intent(in) :: obss
  character(len=*), intent(in) :: group
  character(len=*), intent(in) :: vname
  type(datetime), intent(in) :: reference
  integer(c_int64_t), intent(inout) :: offset(:)

  character(kind=c_char,len=1), allocatable :: c_group(:), c_vname(:)
  type(c_ptr) :: c_reference
  integer(c_size_t) :: length

  !  Translate query from Fortran string to C++ char[].
  call f_c_string(group, c_group)
  call f_c_string(vname, c_vname)
  call f_c_datetime(reference, c_reference)
  length = size(offset)

  call c_cxvarobs_obsspace_get_db_datetime_offset_in_seconds(obss, c_group, c_vname, c_reference, &
                                                             length, offset)

  deallocate(c_group, c_vname)
end subroutine cxvarobs_obsspace_get_db_datetime_offset_in_seconds

end module cxvarobs_obsspace_mod
