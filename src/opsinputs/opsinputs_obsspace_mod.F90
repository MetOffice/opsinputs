!
! (C) Copyright 2020 Met Office UK
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

!> Extensions to the Fortran interface to ioda::ObsSpace.
module opsinputs_obsspace_mod

use datetime_mod, only: datetime, f_c_datetime
use string_f_c_mod, only: f_c_string

implicit none

public opsinputs_obsspace_get_db_datetime_offset_in_seconds, &
       opsinputs_obsspace_get_db_string
private

#include "opsinputs_obsspace_interface.f90"

contains

!> Get a datetime variable from ObsSpace, representing it as an array of offsets (in seconds)
!> with respect to a reference datetime.
subroutine opsinputs_obsspace_get_db_datetime_offset_in_seconds(obss, group, vname, reference, offsets)
  use, intrinsic :: iso_c_binding
  implicit none
  type(c_ptr), value, intent(in) :: obss
  character(len=*), intent(in) :: group
  character(len=*), intent(in) :: vname
  type(datetime), intent(in) :: reference
  integer(c_int64_t), intent(inout) :: offsets(:)

  character(kind=c_char,len=1), allocatable :: c_group(:), c_vname(:)
  type(c_ptr) :: c_reference
  integer(c_size_t) :: length

  !  Translate query from Fortran string to C++ char[].
  call f_c_string(group, c_group)
  call f_c_string(vname, c_vname)
  call f_c_datetime(reference, c_reference)
  length = size(offsets)

  call c_opsinputs_obsspace_get_db_datetime_offset_in_seconds(obss, c_group, c_vname, c_reference, &
                                                             length, offsets)
end subroutine opsinputs_obsspace_get_db_datetime_offset_in_seconds

!> Get a string variable from ObsSpace.
subroutine opsinputs_obsspace_get_db_string(obss, group, vname, string_length, strings)
  use, intrinsic :: iso_c_binding
  implicit none
  type(c_ptr), value, intent(in) :: obss
  character(len=*), intent(in) :: group
  character(len=*), intent(in) :: vname
  integer, intent(in) :: string_length
  character(len=string_length), intent(inout) :: strings(:)

  character(kind=c_char,len=1), allocatable :: c_group(:), c_vname(:)
  integer(c_size_t) :: num_strings

  !  Translate query from Fortran string to C++ char[].
  call f_c_string(group, c_group)
  call f_c_string(vname, c_vname)
  num_strings = size(strings)

  call c_opsinputs_obsspace_get_db_string(obss, c_group, c_vname, &
                                         int(string_length, kind=8), num_strings, strings)
end subroutine opsinputs_obsspace_get_db_string

end module opsinputs_obsspace_mod
