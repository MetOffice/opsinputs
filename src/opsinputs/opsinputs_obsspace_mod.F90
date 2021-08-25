!
! (C) Crown Copyright 2020, the Met Office. All rights reserved.
!
! Refer to COPYRIGHT.txt of this distribution for details.

!> Extensions to the Fortran interface to ioda::ObsSpace.
module opsinputs_obsspace_mod

use datetime_mod, only: datetime, f_c_datetime
use string_f_c_mod, only: f_c_string

implicit none

public opsinputs_obsspace_get_db_datetime_offset_in_seconds, &
       opsinputs_obsspace_get_db_string, &
       opsinputs_obsspace_get_locs_ordered_by_record
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
                                          int(string_length, kind=c_size_t), num_strings, strings)
end subroutine opsinputs_obsspace_get_db_string

!> Order location indices first by record index and then the sorting variable defined for the
!> ObsSpace
!>
!> \param[in] obsspace
!>   The ObsSpace.
!> \param[inout] LocationsOrderedByRecord
!>   A vector with `obss.nlocs()` elements that will be filled with ordered (1-based)
!>   location indices.
!> \param[inout] RecordStarts
!>   A vector with `obss.nrecs() + 1` elements that will be filled with (1-based) indices of the
!>   elements of `LocationsOrderedByRecord` storing the first location of each record.
subroutine opsinputs_obsspace_get_locs_ordered_by_record(obss,  &
                                                         LocationsOrderedByRecord, RecordStarts)
  use, intrinsic :: iso_c_binding
  implicit none
  type(c_ptr), value, intent(in) :: obss
  integer(c_int32_t), intent(inout) :: LocationsOrderedByRecord(:)
  integer(c_int32_t), intent(inout) :: RecordStarts(:)

  call c_opsinputs_obsspace_get_locs_ordered_by_record(obss, &
    size(LocationsOrderedByRecord), &
    LocationsOrderedByRecord, &
    size(RecordStarts), &
    RecordStarts)
end subroutine opsinputs_obsspace_get_locs_ordered_by_record

end module opsinputs_obsspace_mod
